000100121207     /*MODULE
000200121207     /*PRM dbgview(*source)
000300121207     /*END
000400121207       //==============================================================
000500121207       //
000600121207       // Questo modulo verifica l'eventuale blocco dei clienti
000700121207       // ricevuti come parametro.
000800121207       //
000900121207       // Può essere usato solo come sottoprocedura.
001000121207       // Ogni metodo restituisce un valore 10I:
001100121207       //    · = *zero => tutto ok
001200121207       //    · < *zero => errore
001300121207       //
001400121207       // _________________________________________________
001500121207       // PARAMETRI DELLA SOTTOPROCEDURA: UBCHKABL_Chk...
001600121207       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
001700121207       // Posizione Uso Tipo       Lunghezza Descrizione
001800121207       // --------- --- ---------- --------- --------------------------
001900121207       // 1         In  *Char      3200      800 codici clienti (packed)
002000121207       // 2         Out *Char       800      800 flags "blocco" (ACOABL)
002100121207       // --------- --- ----------  --------- -------------------------
002200121207       //
002300121207       // __________________
002400121207       // ESEMPI DI CHIAMATA
002500121207       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002600121207       // if  UBLEG3C_Chk ( pInSkKSC : pOutSkABL ) = *zero;
002700121207       //
002800121207       // ______________________________
002900121207       // ISTRUZIONI PER LA COMPILAZIONE
003000121207       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003100121207       // 1. Creare il modulo UBCHKABL (Opz 15 PDM)
003200121207       // 2. Creare/Aggiornare il programma di servizio UBCHKABL (CRTSRVPGM / UPDSRVPGM)
003300121207       // 4. Cancellare il modulo.
003400121207       //
003500121207       //==============================================================
003600121207
003700121207     h NoMain
003800121207
003900121207       //--------------------------------------------------------------
004000121207       //?Definizione archivi.                                         ?
004100121207       //--------------------------------------------------------------
004200121207
004300121207       // -?Anagrafica P.d.C.?
004400121207     fCNACO00F  if   e           k disk    usropn
004500121207
004600121207       //--------------------------------------------------------------
004700121207       //?Definizione costanti.                                        ?
004800121207       //--------------------------------------------------------------
004900121207
005000121207       // -?Codice utente/lingua di default?
005100121207     d c_KUT           c                   const(1)
005200121213       // -?Codice Capoconto di default?
005300121213     d c_KCC           c                   const(0151)
005400121207
005500121207       // -?Errori gestiti?
005600121207     d c_ErrOpen...
005700121207     d                 c                   const(-1)
005800121207     d c_NotFound...
005900121207     d                 c                   const(-2)
006000121207     d c_Annull...
006100121207     d                 c                   const(-3)
006200121207     d c_ErrClose...
006300121207     d                 c                   const(-4)
006400121207
006500121207       //--------------------------------------------------------------
006600121207       //?Definizione aree dati.                                       ?
006700121207       //--------------------------------------------------------------
006800121207
006900121207       // -?Dati utente?
007000121207     d §AzUte        e ds                  extname(AZUTE00F)
007100121207     d                                     dtaara
007200121207     d §DatiUte      e ds                  extname(dDatiUte)
007300121207     d                                     dtaara
007400121207
007500121207       //--------------------------------------------------------------
007600121207       //?Definizione prototipi procedure.                             ?
007700121207       //--------------------------------------------------------------
007800121207
007900121207       // -?Verifica blocco clienti?
008000121207      /copy gaitrasrc/srcProtoPR,UBCHKABL
008100121207
008200121207       //--------------------------------------------------------------
008300121207       //?Definizione variabili globali.                               ?
008400121207       //--------------------------------------------------------------
008500121207
008600121207       // -?Valori di ritorno?
008700121207     d rtnEsito        s             10i 0 inz
008800121207
008900121207       // -?Flags booleani?
009000121207     d $InitDone       s               n   inz
009100121207
009200121207       //--------------------------------------------------------------
009300121207       //?Definizione key-list globali.                                ?
009400121207       //--------------------------------------------------------------
009500121207
009600121207       // -?CNACO00F?
009700121207     d k03cnaco00    e ds                  extname(CNACO00F : *key)
009800121207     d                                     prefix(k_)  inz
009900121207
010000121207       //--------------------------------------------------------------
010100121207       //?Procedure name: UBCHKABL_Open                               ?
010200121207       // Purpose:        Apertura archivi
010300121207       // Returns:        Integer (Status_Code)
010400121207       //--------------------------------------------------------------
010500121207
010600121207     p UBCHKABL_Open...
010700121207     p                 B                   export
010800121207
010900121207     d UBCHKABL_Open...
011000121207     d                 PI            10i 0
011100121207
011200121207       //--------------------------------------------------------------
011300121207
011400121207      /free
011500121207
011600121207       // -?Inizializzazione variabili?
011700121207       reset  rtnEsito;
011800121207       reset  $InitDone;
011900121207
012000121207       // -?Apertura archivi?
012100121207       if  not %open(CNACO00F);
012200121207         open(e)  CNACO00F;
012300121207       endif;
012400121207       if  %error;
012500121207         rtnEsito = c_ErrOpen;
012600121207         return  rtnEsito;
012700121207       endif;
012800121207
012900121207       // -?Valorizzazione variabili?
013000121207       $InitDone = *on;
013100121207
013200121207       return  rtnEsito;
013300121207
013400121207      /end-free
013500121207
013600121207     p UBCHKABL_Open...
013700121207     p                 E
013800121207
013900121207       //--------------------------------------------------------------
014000121207       //?Procedure name: UBCHKABL_Chk...                            ?
014100121207       // Purpose:        Restituisce il flag CNACO00F.ACOABL (flag
014200121207       //                 blocco) dei clienti ricevuti come parametro.
014300121207       // Returns:        Integer (Status_Code)
014400121207       // Parameter:      pInSkKSC  => 800 codici clienti
014500121207       //                 pOutSkABL => 800 flags "blocco" (ABOABL)
014600121207       //--------------------------------------------------------------
014700121207
014800121207     p UBCHKABL_Chk...
014900121207     p                 B                   export
015000121207
015100121207     d UBCHKABL_Chk...
015200121207     d                 PI            10i 0
015300121207     d   pInSkKSC                  3200a   const
015400121207     d                                     options(*varsize)
015500121207     d   pOutSkABL                  800a   options(*varsize)
015600121207
015700121207       //--------------------------------------------------------------
015800121207       //?Definizione costanti locali.                                 ?
015900121207       //--------------------------------------------------------------
016000121207
016100121207       // -?Elementi di schiera gestiti?
016200121207     d c_MaxElem       c                   const(800)
016300121207
016400121207       //--------------------------------------------------------------
016500121207       //?Definizione schiere locali.                                  ?
016600121207       //--------------------------------------------------------------
016700121207
016800121207       // -?Codici clienti di cui reperire il flag da CNACO?
016900121207     d ds_SkKSC        ds          3200    inz
017000121210     d   sk_KSC                            like(ACOksc)
017100121210     d                                     dim(800)  inz
017200121210     d*//sk_KSC                       7p 0 dim(800)  inz
017300121207
017400121207       // -?Relativi flags relativi all'abilitazione "EasyWEB"?
017500121207     d ds_SkABL        ds           800    inz
017600121207     d   sk_ABL                            like(ACOabl)
017700121207     d                                     dim(800)  inz
017800121207
017900121207       //--------------------------------------------------------------
018000121207       //?Definizione strutture dati locali.                           ?
018100121207       //--------------------------------------------------------------
018200121207
018300121207
018400121207       //--------------------------------------------------------------
018500121207       //?Definizione variabili locali.                                ?
018600121207       //--------------------------------------------------------------
018700121207
018800121207       // -?Indici di schiera?
018900121207     d xx              s              5  0 inz
019000121207
019100121207       //--------------------------------------------------------------
019200121207       //?M A I N - L I N E .                                          ?
019300121207       //--------------------------------------------------------------
019400121207
019500121207      /free
019600121210
019700121210       ds_SkKSC = pInSkKSC;
019800121207
019900121207       // -?Inizializzazione parametri di output?
020000121207       reset  rtnEsito;
020100121207       clear  sk_ABL;
020200121207
020300121207       // -?Se non già inizializzata la procedure?
020400121207       //  ?=> si forza l'inizializzazione?
020500121207       if  not $InitDone;
020600121207         rtnEsito = ubChkABL_Open;
020700121207       endif;
020800121207       // -?Se ricevuto errore => uscita?
020900121207       if  rtnEsito < *zero;
021000121207         return  rtnEsito;
021100121207       endif;
021200121207
021300121207       // -?Reperimento dati dei codici ricevuti come parametro?
021400121207       k_ACOkut = c_KUT;
021500121213       k_ACOkcc = c_KCC;
021600121207
021700121207       For  xx = 1   To  %elem(sk_KSC);
021800121207
021900121217         if  sk_KSC(xx) <= *zero;
022000121217           //iter;
022100121217           leave;
022200121207         endif;
022300121207
022400121207         k_ACOksc = sk_KSC(xx);
022500121207         chain  %kds( k03cnaco00 )  CNACO000;
022600121207
022700121207         // -?Cliente NON trovato in anagrafica => avviso?
022800121207         if  NOT %found(CNACO00F);
022900121207           sk_ABL(xx) = '?';
023000121207           //rtnEsito = c_NotFound;
023100121207           //return  rtnEsito;
023200121207           iter;
023300121207         endif;
023400121207
023500121207         // -?Cliente annullato in anagrafica => avviso?
023600121207         if  ACOflg <> *blank;
023700121207           sk_ABL(xx) = 'A';
023800121207           //rtnEsito = c_Annull;
023900121207           //return  rtnEsito;
024000121207           iter;
024100121207         endif;
024200121207
024300121207         // -?Impostazione parametri di output per il cliente ricevuto:?
024400121207         sk_ABL(xx) = ACOabl;
024500121207
024600121207       EndFor;
024700121207
024800121207       // -?Restituzione flags rilevati?
024900121207       pOutSkABL = ds_SkABL;
025000121207
025100121207       return  rtnEsito;
025200121207
025300121207      /end-free
025400121207
025500121207     p UBCHKABL_Chk...
025600121207     p                 E
025700121207
025800121207       //--------------------------------------------------------------
025900121207       //?Procedure name: UBCHLABL_Close                              ?
026000121207       // Purpose:        Chiusura archivi
026100121207       // Returns:        Integer (Status_Code)
026200121207       //--------------------------------------------------------------
026300121207
026400121207     p UBCHKABL_Close...
026500121207     p                 B                   export
026600121207
026700121207     d UBCHKABL_Close...
026800121207     d                 PI            10i 0
026900121207
027000121207      /free
027100121207
027200121207       // -?Inizializzazione dei parametri di output?
027300121207       reset  rtnEsito;
027400121207
027500121207       // -?Chiusura archivio TABEL?
027600121207       if  %open(CNACO00F);
027700121207         close(E)  CNACO00F;
027800121207         if  %error   and   rtnEsito = *zero;
027900121207           rtnEsito = c_ErrClose;
028000121207         endif;
028100121207       endif;
028200121207
028300121207       // -?Valorizzazione opcode di metodo?
028400121207       $InitDone = *off;
028500121207
028600121207       return  rtnEsito;
028700121207
028800121207      /end-free
028900121207
029000121207     p UBCHKABL_Close...
029100121207     p                 E
