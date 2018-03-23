000100080611     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400141126     ** Questo modulo verifica l'abilitazione ai servizi internet
000500081003     **
000600141126     ** Può essere usato sia come sottoprocedura che come pgm.
000700141126     ** Come sottoprocedura ogni metodo restituisce un valore 10I:
000800081003     **             - = 0 => tutto ok
000900081003     **             - < 0 => errore
001000141126     ** Come programma bisogna controllare il parametro di esito.
001100071128     **
001200141126     ** PARAMETRI DELLA SOTTOPROCEDURA: UBCHKPWD_Check...
001300071129     **
001400081003     ** Posizione Uso Tipo      Lunghezza Descrizione
001500081003     ** --------- --- --------- --------- ------------------------------
001600141126     ** 1         IN  CHARACTER   9       Strategi User Number
001700141126     ** 2         IN  CHARACTER   2       Tipo Servizio Internet
001800141126     ** 3         IN  CHARACTER  20       Password
001900141126     ** 4         IN  CHARACTER   2       Linguaggio
002000141126     ** 5         OUT CHARACTER   8       Cliente Unificante
002100141126     ** 6         OUT CHARACTER   2       Tipo Legame
002200141126     ** 7         OUT CHARACTER   3       Esito
002300141126     ** 8         OUT CHARACTER 100       Msg
002400081003     ** --------- --- --------- --------- ------------------------------
002500071128     **
002600071128     **
002700071128     ** ESEMPI DI CHIAMATA
002800071128     **
002900141126     ** come sottoprocedura:
003000141126     ** if UBCHKPWD_Check(pInSUN:pInISV:pinPwd:pInLANG:
003100141126     **                   pOutKSU:pOutTLE:pOutEsito:pOutMsg = 0
003200141126     ** come pgm:
003300141126     **    UBCHKPWD_Check(pInSUN:pInISV:pinPwd:pInLANG:
003400141126     **                   pOutKSU:pOutTLE:pOutEsito:pOutMsg);
003500141126     **    if pOutEsito = *blank;
003600071128     **
003700071128     ** ISTRUZIONI PER LA COMPILAZIONE
003800071128     **
003900141126     ** 1. Creare il modulo UBCHKPWD (Opz 15 PDM)
004000141126     ** 2. Creare/Aggiornare il programma di servizio UBCHKPWD (CRTSRVPGM / UPDSRVPGM)
004100141126     ** 3. Creare/Aggiornare il programma UBCHKPWD (CRTPGM / UPDPGM)
004200080610     ** 4. Cancellare il modulo.
004300071128     **
004400071128     ***********************************************************************************************
004500071128
004600141126
004700141126      //********************************************************************************************
004800141126      //
004900141126      // Definizione archivi                                       ?
005000141126      //
005100141126      //********************************************************************************************
005200141126      // - Anagrafica Clienti iscritti ai servizi Internet
005300141126     fTIABL01L  if   e           k disk    usropn
005400141126
005500141126      //********************************************************************************************
005600141126      //
005700141126      // Definizione key-list globali                              ?
005800141126      //
005900141126      //********************************************************************************************
006000141126      // TIABL01L
006100141126     d k03TIABL01    e ds                  extname(TIABL01L : *key)
006200141126     d                                     prefix(k_)  inz
006300141126
006400071128      //********************************************************************************************
006500071128      //
006600071128      // Definizione prototipi procedure.
006700071128      //
006800071128      //********************************************************************************************
006900141126     D/COPY GAITRASRC/SRCPROTOPR,UBCHKPWD
007000071128
007100071128      //********************************************************************************************
007200071128      //
007300071128      // Definizione variabili globali.
007400071128      //
007500071128      //********************************************************************************************
007600080612     D
007700071128
007800080611     P*--------------------------------------------------
007900141126     P* Procedure name: UBCHKPWD_Check...
008000141126     P* Purpose:        Controllo abilitazione servizi internet
008100141126     P* Returns:        Integer (Status_Code)
008200141126     P* Parameter:      pInSUN         => Strategi User Number
008300141126     P* Parameter:      pInISV         => Tipo Servizio Internet
008400141126     P* Parameter:      pInPWD         => Password
008500141126     P* Parameter:      pInLANG        => Linguaggio
008600141126     P* Parameter:      pOutKSU        => Cliente Unificante
008700141126     P* Parameter:      pOutTLE        => Tipo Legame
008800141126     P* Parameter:      pOutEsito      => Esito
008900141126     P* Parameter:      pOutMsg        => Msg
009000080611     P*--------------------------------------------------
009100080611     P*
009200141126     P UBCHKPWD_Check...
009300071129     P                 B                   EXPORT
009400071129     D*
009500141126     D UBCHKPWD_Check...
009600080612     D                 PI            10I 0
009700141126     D   pInSUN                       9A
009800141126     D   pInISV                       2A
009900141126     D   pInPWD                      20A
010000141126     D   pInLANG                      2A
010100141126     D   pOutKSU                      8A
010200141126     D   pOutTLE                      2A
010300141126     D   pOutEsito                    3A
010400141126     D   pOutMsg                    100A
010500080612
010600141126
010700141126
010800081020     D*
010900081020     D* varibili d wrk
011000141126     d wPwd            S             20a   inz
011100141126     d wDate           s              8  0 inz
011200141126     d wSQL            s           2048    inz  varying
011300131017
011400131017     D*
011500131017     D* Costanti
011600131017     D numeri          c                   const('1234567890')
011700080612
011800080612      //********************************************************************************************
011900080612      //
012000080612      // Definizione variabili work.
012100080612      //
012200080612      //********************************************************************************************
012300081003
012400081003
012500081003     C*
012600081003     C* Inizializzazioni
012700131017      /free
012800131017
012900141126       // Inizializzazioni
013000141126       clear pOutKSU  ;
013100141126       clear pOutTLE  ;
013200141126       clear pOutEsito;
013300141126       clear pOutMsg  ;
013400141126       clear wPwd     ;
013500141126
013600141126       // Reperimento data odierna (fmt aaaa/mm/gg)
013700141126       wDate = %dec( %date() );
013800141126
013900141126       // Ctrl parametri input
014000141126       // se il linguaggio è vuoto, vale IT
014100141126       if pInLANG = *blank;
014200141126         pInLANG='IT';
014300141126       endif;
014400141126       // il linguaggio può valere solo IT o EN, altrimenti assume IT ma viene scritto un msg
014500141126       if pInLANG <> 'IT' and pInLANG <> 'EN';
014600141126         pInLANG='IT';
014700141126         pOutMsg = 'Valid languages are IT or EN';
014800141126       endif;
014900141126       // tutti i parametri (eccetto LANG già controllato) devono essere valorizzati
015000141126       if pInSUN = *blank or pInISV = *blank or pInPWD = *blank;
015100141126         pOutEsito = '010';
015200141126         select;
015300141126           when pInLANG = 'IT';
015400141126             pOutMsg = 'Parametri di input non validi';
015500141126           when pInLANG = 'EN';
015600141126             pOutMsg = 'Input parameters not valid';
015700141126         endsl;
015800141126         return 10;
015900141126       endif;
016000141126
016100141126       // aperture file in usropn
016200141126       if not %open(TIABL01L);
016300141126         open TIABL01L;
016400141126       endif;
016500141126
016600141126       // Recupero pwd clienti iscritti ai servizi internet
016700141126       // 1° tentativo
016800141126       K_ABLSUN = pInSUN;
016900141126       K_ABLISV = pInISV;
017000141126       K_ABLTIP = 'SG1';
017100141126       chain  %kds( k03TIABL01 )  TIABL000;
017200141126       if %found();
017300141126         wPwd = ABLPWD;
017400141126       else;
017500141126         // 2° tentativo
017600141126         K_ABLTIP = 'PWD';
017700141126         chain  %kds( k03TIABL01 )  TIABL000;
017800141126         if %found();
017900141126           wPwd = ABLPWD;
018000141126         endif;
018100141126       endif;
018200131017
018300141126       // se non trovato rcd o la pwd reperita non è quella in ingresso, errore
018400141126       if wPwd = *blank or wPwd <> pInPWD;
018500141126         pOutEsito = '020';
018600141126         select;
018700141126           when pInLANG = 'IT';
018800141126             pOutMsg = 'Utente e/o password errati';
018900141126           when pInLANG = 'EN';
019000141126             pOutMsg = 'Username and/or password is wrong';
019100141126         endsl;
019200141126         return 20;
019300141126       endif;
019400141126
019500141126       // estraggo Unificante e Tipo Legame dall'abilitazione
019600141126       wSQL = 'select VSSKSU, VSSTLE +
019700141126        from TIVSS00F +
019800141127        where VSSSUN = ''' + pInSUN +
019900141127        ''' and VSSISV = ''' + pInISV +
020000141127        ''' and ' + %editc(wDate:'X') + ' between VSSDDE and VSSDSC';
020100141126
020200141126       // Dichiarazione cursore
020300141126       exec sql   prepare S0   from :wSQL;
020400141126       exec sql   declare C0   cursor   for S0;
020500141126       // Apertura del cursore
020600141126       exec sql   open C0;
020700141126       // Lettura cursore
020800141126       exec sql   fetch next   from C0   into :pOutKSU , :pOutTLE;
020900141126       // se la fetch non ha letto, errore
021000141126       if SQLCod < 0 or SQLCod = 100;
021100141126         pOutEsito = '030';
021200141126         select;
021300141126           when pInLANG = 'IT';
021400141126             pOutMsg = 'Utente non abilitato al servizio';
021500141126           when pInLANG = 'EN';
021600141127             pOutMsg = 'Service is not enabled for this user';
021700141126         endsl;
021800141126         // Chiusura del cursore
021900141126         exec sql   close C0;
022000141126         return 30;
022100141126       endif;
022200141126
022300141126       // Chiusura del cursore
022400141126       exec sql   close C0;
022500141126
022600141126       // Esco dicendo che va tutto bene
022700141126       pOutEsito = '000';
022800141126       return 0;
022900131017
023000131017      /end-free
023100071129
023200141126     P UBCHKPWD_Check...
023300071129     P                 E
