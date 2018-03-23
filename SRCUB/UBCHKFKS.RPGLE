000100091217       //==============================================================
000200091217       //
000300091217       // Questo modulo verifica la compatibilità tra un codice cliente
000400091217       // (pInKSC) e quello del relativo cliente di invio dati (pInFKS)
000500091217       //
000600091217       // Può essere usato solo come sottoprocedura.
000700091217       // Ogni metodo restituisce un valore 10I:
000800091217       //    · = *zero => tutto ok
000900091217       //    · < *zero => errore
001000091217       //
001100091217       // _________________________________________________
001200091217       // PARAMETRI DELLA SOTTOPROCEDURA: UBCHKFKS_Check...
001300091217       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
001400091217       // Posizione Uso Tipo       Lunghezza Descrizione
001500091217       // --------- --- ---------- --------- --------------------------
001600091217       // 1         In  *Dec          7 / 0  Codice cliente (CLPKSC)
001700091217       // 2         In  *Dec          7 / 0  Cliente intestaz. fatt. (1)
001800091217       // 3         In  *Dec          7 / 0  Cliente invio dati (§4CFKS)
001900091217       // 4         In  *Dec          7 / 0  Cliente intestaz. fatt. (3)
002000091217       // --------- --- ----------  --------- -------------------------
002100091217       //
002200091217       // __________________
002300091217       // ESEMPI DI CHIAMATA
002400091217       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002500091217       // if  UBCHKFKS_Check ( pInKSC : pInSCF : pInFKS : pInFSC ) = 0;
002600091217       //
002700091217       // ______________________________
002800091217       // ISTRUZIONI PER LA COMPILAZIONE
002900091217       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003000091217       // 1. Creare il modulo UBCHKFKS (Opz 15 PDM)
003100091217       // 2. Creare/Aggiornare il programma di servizio UBCHKFKS (CRTSRVPGM / UPDSRVPGM)
003200091217       // 4. Cancellare il modulo.
003300091217       //
003400091217       //==============================================================
003500091217
003600091217     h NoMain
003700091217
003800091217       //--------------------------------------------------------------
003900091217       //?Definizione costanti.                                        ?
004000091217       //--------------------------------------------------------------
004100091217
004200091217
004300091217       //--------------------------------------------------------------
004400091217       //?Definizione prototipi procedure.                             ?
004500091217       //--------------------------------------------------------------
004600091217
004700091217      /copy gaitrasrc/srcProtoPR,UBCHKFKS
004800091217
004900091217       //--------------------------------------------------------------
005000091217       //?Definizione variabili globali.                               ?
005100091217       //--------------------------------------------------------------
005200091217
005300091217       // - Valori di ritorno
005400091217     d retField        s             10i 0 inz
005500091217
005600091217       //--------------------------------------------------------------
005700091217       //?Procedure name: UBCHKFKS_Check...                             ?
005800091217       // Purpose:        Verifica la compatibilità tra un cod. cliente
005900091217       //                 (pInKSC) e quello del relativo cliente di
006000091217       //                 invio dati (pInFKS)
006100091217       // Returns:        Integer (Status_Code)
006200091217       // Parameter:      pInKSC  => Codice Cliente
006300091217       // Parameter:      pInSCF  => Cliente intestaz. fatt. di pInKSC
006400091217       // Parameter:      pInFKS  => Cliente invio dati (§4CFKS)
006500091217       // Parameter:      pInFSC  => Cliente intestaz. fatt. di pInFKS
006600091217       //--------------------------------------------------------------
006700091217
006800091217     p UBCHKFKS_Check...
006900091217     p                 B                   export
007000091217
007100091217     d UBCHKFKS_Check...
007200091217     d                 PI            10i 0
007300091217     d   pInKSC                       7p 0 const
007400091217     d   pInSCF                       7p 0 const
007500091217     d   pInFKS                       7p 0 const
007600091217     d   pInFSC                       7p 0 const
007700091217
007800091217      /free
007900091217
008000091217       // -?Inizializzazione dei parametri di output?
008100091217       reset  retField;
008200091217
008300091217       select;
008400091217
008500091217         // -Cliente di fatturazione NON inserito => ok?
008600091217         when  pInFKS = *zero;
008700091217
008800091217         // -Cliente in esame = Cliente di fatturazione => ok?
008900091217         when  pInKSC = pInFKS;
009000091217
009100091217         // -Codice intestazione fattura = Cliente di fatturazione?
009200091217         //  => ok?
009300091217         when  pInSCF = pInFKS;
009400091217
009500091217         // -Stesso codice intestazione fattura => ok?
009600091217         when  pInSCF = pInFSC;
009700091217
009800091217         // -Codice intestazione fattura diverso => ERRORE?
009900091217         other;
010000091217           retField = -1;
010100091217
010200091217       endsl;
010300091217
010400091217       return  retField;
010500091217
010600091217      /end-free
010700091217
010800091217     p UBCHKFKS_Check...
010900091217     p                 E
