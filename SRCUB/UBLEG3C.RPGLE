000100100203     /*MODULE
000200100203     /*PRM dbgview(*source)
000300100203     /*END
000400100203       //==============================================================
000500100203       //
000600100203       // Questo modulo reperisce famiglia (padre e figli) in tab. "3C"
000700100203       // del cliente ricevuto come parametro.
000800100203       //
000900100203       // Può essere usato solo come sottoprocedura.
001000100203       // Ogni metodo restituisce un valore 10I:
001100100203       //    · = *zero => tutto ok
001200100203       //    · < *zero => errore
001300100203       //
001400100203       // _________________________________________________
001500100203       // PARAMETRI DELLA SOTTOPROCEDURA: UBLEG3C_Rtv...
001600100203       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
001700100203       // Posizione Uso Tipo       Lunghezza Descrizione
001800100203       // --------- --- ---------- --------- --------------------------
001900100203       // 1         In  *Dec          7 / 0  Codice cliente
002000100203       // 2         Out *Flag         1      P=Padre, F=Figlio
002100100203       // 3         Out *Dec          7 / 0  Codice padre
002200100203       // 4         Out *Char      3200      800 cod.clienti (packed) in famiglia
002300100203       // 4         Out *Char       800      800 flags "EasyWEB" (*on/*off)
002400100203       // --------- --- ----------  --------- -------------------------
002500100203       //
002600100203       // __________________
002700100203       // ESEMPI DI CHIAMATA
002800100203       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002900100203       // if  UBLEG3C_Rtv ( pInKSC : pOutFLGPF : pOutPADRE
003000100203       //                   pOutSKC : pOutSKEW ) = *zero;
003100100203       //
003200100203       // ______________________________
003300100203       // ISTRUZIONI PER LA COMPILAZIONE
003400100203       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003500100203       // 1. Creare il modulo UBLEG3C (Opz 15 PDM)
003600100203       // 2. Creare/Aggiornare il programma di servizio UBLEG3C (CRTSRVPGM / UPDSRVPGM)
003700100203       // 4. Cancellare il modulo.
003800100203       //
003900100203       //==============================================================
004000100203
004100100203     h NoMain
004200100203
004300100203       //--------------------------------------------------------------
004400100203       //?Definizione archivi.                                         ?
004500100203       //--------------------------------------------------------------
004600100203
004700100203       // -?Tabella "3C"?
004800100203     fTABEL00F  if   e           k disk    usropn
004900100203
005000100203       //--------------------------------------------------------------
005100100203       //?Definizione costanti.                                        ?
005200100203       //--------------------------------------------------------------
005300100203
005400100203       // -?Codice utente/lingua di default?
005500100203     d c_KUT           c                   const(1)
005600100203
005700100203       // -?Codice tabella da elaborare?
005800100203     d c_TAB           c                   const('3C')
005900100203
006000100203       // -?Costante Supporto "EasyWEB"?
006100100203     d c_EasyWEB       c                   const('ESWEB')
006200100203
006300100203       // -?Errori gestiti?
006400100203     d c_ErrOpen...
006500100203     d                 c                   const(-1)
006600100203     d c_NotFound...
006700100203     d                 c                   const(-2)
006800100203     d c_Annull...
006900100203     d                 c                   const(-3)
007000100203     d c_ErrClose...
007100100203     d                 c                   const(-4)
007200100203
007300100203       //--------------------------------------------------------------
007400100203       //?Definizione prototipi procedure.                             ?
007500100203       //--------------------------------------------------------------
007600100203
007700100203       // -?Verifica esistenza riferimento mittente numerico x cliente?
007800100203      /copy gaitrasrc/srcProtoPR,UBLEG3C
007900100203
008000100203       //--------------------------------------------------------------
008100100203       //?Definizione variabili globali.                               ?
008200100203       //--------------------------------------------------------------
008300100203
008400100203       // -?Valori di ritorno?
008500100203     d rtnEsito        s             10i 0 inz
008600100203
008700100203       // -?Flags booleani?
008800100203     d $InitDone       s               n   inz
008900100203
009000100203       //--------------------------------------------------------------
009100100203       //?Definizione key-list globali.                                ?
009200100203       //--------------------------------------------------------------
009300100203
009400100203       // -?TABEL00F?
009500100203     d k03tabel00    e ds                  extname(TABEL00F : *key)
009600100203     d                                     prefix(k_)  inz
009700100203
009800100203       //--------------------------------------------------------------
009900100203       //?Procedure name: UBLEG3C_Open                                ?
010000100203       // Purpose:        Apertura archivi
010100100203       // Returns:        Integer (Status_Code)
010200100203       //--------------------------------------------------------------
010300100203
010400100203     p UBLEG3C_Open...
010500100203     p                 B                   export
010600100203
010700100203     d UBLEG3C_Open...
010800100203     d                 PI            10i 0
010900100203
011000100203       //--------------------------------------------------------------
011100100203
011200100203      /free
011300100203
011400100203       // -?Inizializzazione variabili?
011500100203       reset  rtnEsito;
011600100203       reset  $InitDone;
011700100203
011800100203       // -?Apertura archivi?
011900100203       if  not %open(TABEL00F);
012000100203         open(e)  TABEL00F;
012100100203       endif;
012200100203       if  %error;
012300100217         rtnEsito = c_ErrOpen;
012400100203         return  rtnEsito;
012500100203       endif;
012600100203
012700100203       // -?Valorizzazione variabili?
012800100203       $InitDone = *on;
012900100203
013000100203       return  rtnEsito;
013100100203
013200100203      /end-free
013300100203
013400100203     p UBLEG3C_Open...
013500100203     p                 E
013600100203
013700100203       //--------------------------------------------------------------
013800100203       //?Procedure name: UBLEG3C_Rtv...                             ?
013900100203       // Purpose:        Reperisce padre e figli in tab. "3C"
014000100203       //                 del cliente ricevuto come parametro.
014100100203       // Returns:        Integer (Status_Code)
014200100203       // Parameter:      pInKSC    => Codice cliente
014300100203       //                 pOutFLGPF => P=Padre, F=Figlio
014400100203       //                 pOutPADRE => Codice cliente padre
014500100203       //                 pOutSKC   => 800 codici clienti in famiglia
014600100203       //                 pOutSKEW  => 800 flags "EasyWEB"
014700100203       //--------------------------------------------------------------
014800100203
014900100203     p UBLEG3C_Rtv...
015000100203     p                 B                   export
015100100203
015200100203     d UBLEG3C_Rtv...
015300100203     d                 PI            10i 0
015400100203     d   pInKSC                       7p 0 const
015500100203     d   pOutFLGPF                    1a   options(*nopass : *omit)
015600100203     d   pOutPADRE                    7p 0 options(*nopass : *omit)
015700100203     d   pOutSKC                   3200a   options(*varsize :
015800100203     d                                              *nopass : *omit)
015900100203     d   pOutSKEW                   800a   options(*varsize :
016000100203     d                                              *nopass : *omit)
016100100203
016200100203       //--------------------------------------------------------------
016300100203       //?Definizione costanti locali.                                 ?
016400100203       //--------------------------------------------------------------
016500100203
016600100203       // -?Elementi di schiera gestiti?
016700100203     d c_MaxElem       c                   const(800)
016800100203
016900100203       //--------------------------------------------------------------
017000100203       //?Definizione schiere locali.                                  ?
017100100203       //--------------------------------------------------------------
017200100203
017300100203       // -?Codici clienti della stessa famiglia in tab. "3C"?
017400100203     d ds_SKC          ds          3200    inz
017500100203     d  sch_SKC                       7p 0 dim(800)  inz
017600100203
017700100203       // -?Relativi flags relativi all'abilitazione "EasyWEB"?
017800100203     d ds_SKEW         ds           800    inz
017900100203     d  sch_SKEW                       n   dim(800)  inz
018000100203
018100100203       //--------------------------------------------------------------
018200100203       //?Definizione strutture dati locali.                           ?
018300100203       //--------------------------------------------------------------
018400100203
018500100203       // -?Tab. "3C" = Invio dati - BOLLETTAZIONE?
018600100203     d ds3C          e ds                  qualified  inz
018700100203
018800100203       //--------------------------------------------------------------
018900100203       //?Definizione variabili locali.                                ?
019000100203       //--------------------------------------------------------------
019100100203
019200100203       // -?Indici di schiera?
019300100203     d xx              s              5  0 inz
019400100203
019500100203       //--------------------------------------------------------------
019600100203       //?M A I N - L I N E .                                          ?
019700100203       //--------------------------------------------------------------
019800100203
019900100203      /free
020000100203
020100100203       // -?Inizializzazione parametri di output?
020200100203       reset  rtnEsito;
020300100203       clear  sch_SKC;
020400100203       clear  sch_SKEW;
020500100217       if  %parms() > 1   and   %addr(pOutFLGPF) <> *null;
020600100217         clear  pOutFLGPF;
020700100217       endif;
020800100217       if  %parms() > 2   and   %addr(pOutPADRE) <> *null;
020900100217         clear  pOutPADRE;
021000100217       endif;
021100100217       if  %parms() > 3   and   %addr(pOutSKC) <> *null;
021200100217         pOutSKC  = ds_SKC;
021300100217       endif;
021400100217       if  %parms() > 4   and   %addr(pOutSKEW) <> *null;
021500100217         pOutSKEW = ds_SKEW;
021600100217       endif;
021700100203
021800100203       // -?Se non già inizializzata la procedure?
021900100203       //  ?=> si forza l'inizializzazione?
022000100203       if  not $InitDone;
022100100203         rtnEsito = UBLEG3C_Open;
022200100203       endif;
022300100203       // -?Se ricevuto errore => uscita?
022400100203       if  rtnEsito < *zero;
022500100203         return  rtnEsito;
022600100203       endif;
022700100203
022800100203       // -?Reperimento dati del codice ricevuto come parametro?
022900100203       clear  ds3C;
023000100203       k_TBLkut = c_KUT;
023100100203       k_TBLcod = c_TAB;
023200100203       k_TBLkey = %editc( pInKSC : 'X');
023300100203       chain  %kds( k03tabel00 )  TABEL;
023400100203
023500100203       // -?Cliente NON trovato in tab. "3C" => errore?
023600100203       if  NOT %found(TABEL00F);
023700100203         rtnEsito = c_NotFound;
023800100203         return  rtnEsito;
023900100203       endif;
024000100203
024100100203       // -?Cliente annullato in tab. "3C" => avviso?
024200100203       if  TBLflg <> *blank;
024300100203         rtnEsito = c_Annull;
024400100203       endif;
024500100203
024600100203
024700100203       // -?Impostazione parametri di output per il cliente ricevuto:?
024800100203       ds3C = TBLuni;
024900100203
025000100203       //   -?Flag P=Padre, F=Figlio?
025100100217       if  %parms() > 1   and   %addr(pOutFLGPF) <> *null;
025200100217         if  ds3C.§3Ccks = pInKSC;
025300100217           pOutFLGPF = 'P';
025400100217         else;
025500100217           pOutFLGPF = 'F';
025600100217         endif;
025700100217       endif;
025800100203
025900100203       //   -?Codice padre?
026000100217       if  %parms() > 2   and   %addr(pOutPADRE) <> *null;
026100100217         pOutPADRE = ds3C.§3Ccks;
026200100217       endif;
026300100203
026400100203       //   -?Elenco clienti con lo stesso padre?
026500100217       IF  %parms() > 3   and  (%addr(pOutSKC)  <> *null
026600100217                           or   %addr(pOutSKEW) <> *null);
026700100217
026800100217         clear  ds3C;
026900100217         xx = 1;
027000100217         setll  %kds( k03tabel00 : 2 )  TABEL;
027100100217         reade  %kds( k03tabel00 : 2 )  TABEL;
027200100217
027300100217         DoW  NOT %eof(TABEL00F);
027400100217           if  TBLflg = *blank;
027500100217             ds3C = TBLuni;
027600100217             if  ds3C.§3Ccks = pOutPADRE;
027700100217               if  %trimr(TBLkey) = %editc( ds3C.§3Ccks : 'X' );
027800100217                 sch_SKC(1)  = %int( %trimr( TBLkey ) );
027900100217                 sch_SKEW(1) = (ds3C.§3Ccba = c_EasyWEB);
028000100217               else;
028100100217                 xx += 1;
028200100217                 sch_SKC(xx)  = %int( %trimr( TBLkey ) );
028300100217                 sch_SKEW(xx) = (ds3C.§3Ccba = c_EasyWEB);
028400100217               endif;
028500100217             endif;
028600100217           endif;
028700100217           reade  %kds( k03tabel00 : 2 )  TABEL;
028800100217         EndDo;
028900100217
029000100217       ENDIF;
029100100203
029200100217       if  %parms() > 3   and   %addr(pOutSKC) <> *null;
029300100217         //pOutSKC  = %subst( ds_SKC  : 1 : 4 * xx );
029400100217         pOutSKC  = ds_SKC;
029500100217       endif;
029600100217       if  %parms() > 4   and   %addr(pOutSKEW) <> *null;
029700100217         //pOutSKEW = %subst( ds_SKEW : 1 : 1 * xx );
029800100217         pOutSKEW = ds_SKEW;
029900100217       endif;
030000100203
030100100203
030200100203       return  rtnEsito;
030300100203
030400100203      /end-free
030500100203
030600100203     p UBLEG3C_Rtv...
030700100203     p                 E
030800100203
030900100203       //--------------------------------------------------------------
031000100203       //?Procedure name: UBLEG3C_Close                               ?
031100100203       // Purpose:        Chiusura archivi
031200100203       // Returns:        Integer (Status_Code)
031300100203       //--------------------------------------------------------------
031400100203
031500100203     p UBLEG3C_Close...
031600100203     p                 B                   export
031700100203
031800100203     d UBLEG3C_Close...
031900100203     d                 PI            10i 0
032000100203
032100100203      /free
032200100203
032300100203       // -?Inizializzazione dei parametri di output?
032400100203       reset  rtnEsito;
032500100203
032600100203       // -?Chiusura archivio TABEL?
032700100203       if  %open(TABEL00F);
032800100203         close(E)  TABEL00F;
032900100203         if  %error   and   rtnEsito = *zero;
033000100203           rtnEsito = c_ErrClose;
033100100203         endif;
033200100203       endif;
033300100203
033400100203       // -?Valorizzazione opcode di metodo?
033500100203       $InitDone = *off;
033600100203
033700100203       return  rtnEsito;
033800100203
033900100203      /end-free
034000100203
034100100203     p UBLEG3C_Close...
034200100203     p                 E
