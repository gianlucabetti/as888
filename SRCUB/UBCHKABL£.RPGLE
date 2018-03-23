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
005200121207
005300121207       // -?Errori gestiti?
005400121207     d c_ErrOpen...
005500121207     d                 c                   const(-1)
005600121207     d c_NotFound...
005700121207     d                 c                   const(-2)
005800121207     d c_Annull...
005900121207     d                 c                   const(-3)
006000121207     d c_ErrClose...
006100121207     d                 c                   const(-4)
006200121207
006300121207       //--------------------------------------------------------------
006400121207       //?Definizione aree dati.                                       ?
006500121207       //--------------------------------------------------------------
006600121207
006700121207       // -?Dati utente?
006800121207     d §AzUte        e ds                  extname(AZUTE00F)
006900121207     d                                     dtaara
007000121207     d §DatiUte      e ds                  extname(dDatiUte)
007100121207     d                                     dtaara
007200121207
007300121207       //--------------------------------------------------------------
007400121207       //?Definizione prototipi procedure.                             ?
007500121207       //--------------------------------------------------------------
007600121207
007700121207       // -?Reperimento dati utente?
007800121207     d TIBS34ds      e ds                  inz
007900121207      /copy gaitrasrc/srcProtoPR,TIBS34R
008000121207
008100121207       // -?Verifica blocco clienti?
008200121207      /copy gaitrasrc/srcProtoPR,UBCHKABL
008300121207
008400121207       //--------------------------------------------------------------
008500121207       //?Definizione variabili globali.                               ?
008600121207       //--------------------------------------------------------------
008700121207
008800121207       // -?Valori di ritorno?
008900121207     d rtnEsito        s             10i 0 inz
009000121207
009100121207       // -?Flags booleani?
009200121207     d $InitDone       s               n   inz
009300121207
009400121207       //--------------------------------------------------------------
009500121207       //?Definizione key-list globali.                                ?
009600121207       //--------------------------------------------------------------
009700121207
009800121207       // -?CNACO00F?
009900121207     d k03cnaco00    e ds                  extname(CNACO00F : *key)
010000121207     d                                     prefix(k_)  inz
010100121207
010200121207       //--------------------------------------------------------------
010300121207       //?Procedure name: UBCHKABL_Open                               ?
010400121207       // Purpose:        Apertura archivi
010500121207       // Returns:        Integer (Status_Code)
010600121207       //--------------------------------------------------------------
010700121207
010800121207     p UBCHKABL_Open...
010900121207     p                 B                   export
011000121207
011100121207     d UBCHKABL_Open...
011200121207     d                 PI            10i 0
011300121207
011400121207       //--------------------------------------------------------------
011500121207
011600121207      /free
011700121207
011800121207       // -?Inizializzazione variabili?
011900121207       reset  rtnEsito;
012000121207       reset  $InitDone;
012100121207
012200121207       // -?Reperimento dati utente?
012300121207       in(E) §AzUte;
012400121207       if NOT %error;
012500121207         in(E) §DatiUte;
012600121207       endif;
012700121207       if %error or RSut = *blanks;
012800121207         clear TIBS34ds;
012900121207         tibs34r(tibs34ds);
013000121207         in §AzUte;
013100121207         in §DatiUte;
013200121207       endif;
013300121207
013400121207       // -?Apertura archivi?
013500121207       if  not %open(CNACO00F);
013600121207         open(e)  CNACO00F;
013700121207       endif;
013800121207       if  %error;
013900121207         rtnEsito = c_ErrOpen;
014000121207         return  rtnEsito;
014100121207       endif;
014200121207
014300121207       // -?Valorizzazione variabili?
014400121207       $InitDone = *on;
014500121207
014600121207       return  rtnEsito;
014700121207
014800121207      /end-free
014900121207
015000121207     p UBCHKABL_Open...
015100121207     p                 E
015200121207
015300121207       //--------------------------------------------------------------
015400121207       //?Procedure name: UBCHKABL_Chk...                            ?
015500121207       // Purpose:        Restituisce il flag CNACO00F.ACOABL (flag
015600121207       //                 blocco) dei clienti ricevuti come parametro.
015700121207       // Returns:        Integer (Status_Code)
015800121207       // Parameter:      pInSkKSC  => 800 codici clienti
015900121207       //                 pOutSkABL => 800 flags "blocco" (ABOABL)
016000121207       //--------------------------------------------------------------
016100121207
016200121207     p UBCHKABL_Chk...
016300121207     p                 B                   export
016400121207
016500121207     d UBCHKABL_Chk...
016600121207     d                 PI            10i 0
016700121207     d   pInSkKSC                  3200a   const
016800121207     d                                     options(*varsize)
016900121207     d   pOutSkABL                  800a   options(*varsize)
017000121207
017100121207       //--------------------------------------------------------------
017200121207       //?Definizione costanti locali.                                 ?
017300121207       //--------------------------------------------------------------
017400121207
017500121207       // -?Elementi di schiera gestiti?
017600121207     d c_MaxElem       c                   const(800)
017700121207
017800121207       //--------------------------------------------------------------
017900121207       //?Definizione schiere locali.                                  ?
018000121207       //--------------------------------------------------------------
018100121207
018200121207       // -?Codici clienti di cui reperire il flag da CNACO?
018300121207     d ds_SkKSC        ds          3200    inz
018400121210     d   sk_KSC                            like(ACOksc)
018500121210     d                                     dim(800)  inz
018600121210     d*//sk_KSC                       7p 0 dim(800)  inz
018700121207
018800121207       // -?Relativi flags relativi all'abilitazione "EasyWEB"?
018900121207     d ds_SkABL        ds           800    inz
019000121207     d   sk_ABL                            like(ACOabl)
019100121207     d                                     dim(800)  inz
019200121207
019300121207       //--------------------------------------------------------------
019400121207       //?Definizione strutture dati locali.                           ?
019500121207       //--------------------------------------------------------------
019600121207
019700121207
019800121207       //--------------------------------------------------------------
019900121207       //?Definizione variabili locali.                                ?
020000121207       //--------------------------------------------------------------
020100121207
020200121207       // -?Indici di schiera?
020300121207     d xx              s              5  0 inz
020400121207
020500121207       //--------------------------------------------------------------
020600121207       //?M A I N - L I N E .                                          ?
020700121207       //--------------------------------------------------------------
020800121207
020900121207      /free
021000121210
021100121210       ds_SkKSC = pInSkKSC;
021200121207
021300121207       // -?Inizializzazione parametri di output?
021400121207       reset  rtnEsito;
021500121207       clear  sk_ABL;
021600121207
021700121207       // -?Se non già inizializzata la procedure?
021800121207       //  ?=> si forza l'inizializzazione?
021900121207       if  not $InitDone;
022000121207         rtnEsito = ubChkABL_Open;
022100121207       endif;
022200121207       // -?Se ricevuto errore => uscita?
022300121207       if  rtnEsito < *zero;
022400121207         return  rtnEsito;
022500121207       endif;
022600121207
022700121207       // -?Reperimento dati dei codici ricevuti come parametro?
022800121207       k_ACOkut = c_KUT;
022900121207       k_ACOkcc = DUTkci;
023000121207
023100121207       For  xx = 1   To  %elem(sk_KSC);
023200121207
023300121207         if  sk_KSC(xx) = *zero;
023400121207           iter;
023500121207         endif;
023600121207
023700121207         k_ACOksc = sk_KSC(xx);
023800121207         chain  %kds( k03cnaco00 )  CNACO000;
023900121207
024000121207         // -?Cliente NON trovato in anagrafica => avviso?
024100121207         if  NOT %found(CNACO00F);
024200121207           sk_ABL(xx) = '?';
024300121207           //rtnEsito = c_NotFound;
024400121207           //return  rtnEsito;
024500121207           iter;
024600121207         endif;
024700121207
024800121207         // -?Cliente annullato in anagrafica => avviso?
024900121207         if  ACOflg <> *blank;
025000121207           sk_ABL(xx) = 'A';
025100121207           //rtnEsito = c_Annull;
025200121207           //return  rtnEsito;
025300121207           iter;
025400121207         endif;
025500121207
025600121207         // -?Impostazione parametri di output per il cliente ricevuto:?
025700121207         sk_ABL(xx) = ACOabl;
025800121207
025900121207       EndFor;
026000121207
026100121207       // -?Restituzione flags rilevati?
026200121207       pOutSkABL = ds_SkABL;
026300121207
026400121207       return  rtnEsito;
026500121207
026600121207      /end-free
026700121207
026800121207     p UBCHKABL_Chk...
026900121207     p                 E
027000121207
027100121207       //--------------------------------------------------------------
027200121207       //?Procedure name: UBCHLABL_Close                              ?
027300121207       // Purpose:        Chiusura archivi
027400121207       // Returns:        Integer (Status_Code)
027500121207       //--------------------------------------------------------------
027600121207
027700121207     p UBCHKABL_Close...
027800121207     p                 B                   export
027900121207
028000121207     d UBCHKABL_Close...
028100121207     d                 PI            10i 0
028200121207
028300121207      /free
028400121207
028500121207       // -?Inizializzazione dei parametri di output?
028600121207       reset  rtnEsito;
028700121207
028800121207       // -?Chiusura archivio TABEL?
028900121207       if  %open(CNACO00F);
029000121207         close(E)  CNACO00F;
029100121207         if  %error   and   rtnEsito = *zero;
029200121207           rtnEsito = c_ErrClose;
029300121207         endif;
029400121207       endif;
029500121207
029600121207       // -?Valorizzazione opcode di metodo?
029700121207       $InitDone = *off;
029800121207
029900121207       return  rtnEsito;
030000121207
030100121207      /end-free
030200121207
030300121207     p UBCHKABL_Close...
030400121207     p                 E
