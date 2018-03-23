000100151125     H BNDDIR('UBBNDDIR':'QC2LE')
000200080611     H NOMAIN
000300071128     ***********************************************************************************************
000400071128     **
000500161017     ** Questo modulo fornisce funzionalità varie, sia standard che utente, relativamente
000600161017     ** alla gestione dei file/cartelle nel IFS
000700071128     **
000800080611     ** Può essere usato solo come sottoprocedura.
000900080612     ** Ogni metodo restituisce un valore 10I:
001000080612     **             - = 0 => tutto ok
001100080612     **             - < 0 => errore
001200080612     **
001300151126     **
001400161017     ** --------- --- --------- ------------------------------
001500161017     **
001600161019     ** PARAMETRI DELLA SOTTOPROCEDURA: UBBASE64R_Encode_Binary...
001700161017     **
001800161017     ** Posizione Uso Tipo      Descrizione
001900161017     ** --------- --- --------- ------------------------------
002000161019     ** 1         OUT CHARACTER Dati convertiti (encodati)
002100161020     ** 2         OUT INTEGER   Lunghezza dati da convertiti (encodati)
002200161020     ** 3         IN  CHARACTER Dati da convertire
002300161020     ** 4         IN  INTEGER   Lunghezza dati da convertire
002400161018     ** --------- --- --------- ------------------------------
002500161018     **
002600161018     **
002700161018     ** ESEMPI DI CHIAMATA
002800161018     **
002900161020     **  if UBBASE64R_Encode_Binary(oBASE64enc_dta : oBASE64enc_len
003000161020     **                            :iBASE64src_dta : iBASE64src_len) > 0;
003100161019     **
003200161017     **
003300080612     ** --------- --- --------- ------------------------------
003400071128     **
003500071128     ** ISTRUZIONI PER LA COMPILAZIONE
003600170516     **
003700170516     ** 1. Creare il modulo UBBASE64R (Opz 15 PDM)
003800170516     **    indicando TGTRLS(*CURRENT) e CLOSQLCSR(*ENDACTGRP)
003900170516     **              BNDDIR('UBBNDDIR':'QC2LE')
004000170516     **
004100170516     ** 2. Creare/Aggiornare il programma di servizio UBBASE64R (CRTSRVPGM / UPDSRVPGM)
004200170516     **    con ACTGRP(*caller), e BNDDIR(UBBNDDIR)
004300170516     **      CRTSRVPGM SRVPGM(GAITRAOBJ/UBBASE64R) EXPORT(*ALL)
004400170516     **                TEXT('BASE64 - Metodi vari (standard e utente)  ')
004500170516     **                BNDDIR(UBBNDDIR QC2LE) TGTRLS(*CURRENT)
004600170516     **
004700170516     ** 3. Eventualmente si può cancellare il modulo.
004800170516     **
004900071128     **
005000071128     ***********************************************************************************************
005100080612
005200080612
005300080612      //********************************************************************************************
005400080612      //
005500080612      // Definizione file (globali)
005600080612      //
005700080612      //********************************************************************************************
005800071128
005900080612
006000071128      //********************************************************************************************
006100071128      //
006200080612      // Definizione prototipi procedure (globali)
006300071128      //
006400071128      //********************************************************************************************
006500161018     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64R
006600161018     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64R
006700161018     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64
006800161018     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64
006900080612
007000080612
007100071128      //********************************************************************************************
007200071128      //
007300080612      // Definizione variabili work (globali)
007400071128      //
007500071128      //********************************************************************************************
007600151126     D InitDone        S               N   INZ(*off)
007700161017     D
007800071129
007900151126
008000160404
008100161018     P*--------------------------------------------------
008200161019     P* Procedure name: UBBASE64R_Encode_Binary
008300161018     P* Purpose:        Converte dati in BASE64
008400161018     P* Returns:        Integer (Status_Code)
008500161019     P* Parameter:      oBASE64enc_dta  => Dati convertiti (encodati)
008600161020     P* Parameter:      oBASE64enc_len  => Lunghezza dati convertiti (encodati)
008700161019     P* Parameter:      iBASE64src_dta  => Dati da convertire
008800161020     P* Parameter:      iBASE64src_len  => Lunghezza dati da convertire
008900161018     P*--------------------------------------------------
009000160404     P*
009100161019     P UBBASE64R_Encode_Binary...
009200160404     P                 B                   EXPORT
009300160404     D*
009400161019     D UBBASE64R_Encode_Binary...
009500160404     D                 PI            10I 0
009600161019     DoBASE64enc_dta              65535A   VARYING OPTIONS(*VARSIZE)
009700161020     DoBASE64enc_len                 10I 0
009800161019     DiBASE64src_dta              65535A   VARYING OPTIONS(*VARSIZE) CONST
009900161021     DiBASE64src_len                 10I 0 CONST
010000160404     D*
010100160404      //********************************************************************************************
010200160404      //
010300160404      // Definizione variabili work (locali)
010400160404      //
010500160404      //********************************************************************************************
010600160404     D locMethodCode   S             10I 0
010700160404     D
010800161018
010900160404      /FREE
011000161017        // Inizializzo opcode di metodo a errore
011100160404        locMethodCode = -1;
011200161017
011300161017
011400161017        // Monitorizzo l'intero processo
011500161017        Monitor;
011600161017
011700161017        // Inizializzo i parametri di output
011800161018        clear oBASE64enc_dta;
011900161020        clear oBASE64enc_len;
012000161018
012100161019        // Effettuo l'encoding in Base64
012200161019        B64coded_dst = oBASE64enc_dta;
012300161019        B64plain_src = iBASE64src_dta;
012400161020        B64src_len   = iBASE64src_len;
012500161019
012600161019        oBASE64enc_len = BASE64_encode_binary(B64coded_dst
012700161019                                             :B64plain_src
012800161020                                             :B64src_len);
012900161019
013000161019        // Se encodato
013100161019        if oBASE64enc_len > *zeros;
013200161019
013300161019           // Valorizzo parametro di output
013400161019           oBASE64enc_dta = B64coded_dst;
013500161019
013600161019           // Valorizzo opcode di metodo
013700161019           locMethodCode = 0;
013800161019
013900161019        endif;
014000161017
014100161017        On-Error;
014200161017
014300161017           // Esito chiamata già inizializzato a errore e parametri output già puliti
014400161017
014500161017        // Fine monitoring
014600161017        EndMon;
014700161017
014800160404
014900160404        return locMethodCode;
015000160404
015100160404      /END-FREE
015200160404
015300161019     P UBBASE64R_Encode_Binary...
015400160404     P                 E
015500161019
015600161019
015700161019
015800161019     P*--------------------------------------------------
015900161019     P* Procedure name: UBBASE64R_Encode
016000161019     P* Purpose:        Converte dati in BASE64
016100161019     P* Returns:        Integer (Status_Code)
016200161019     P* Parameter:      oBASE64enc_dta  => Dati convertiti (encodati)
016300161020     P* Parameter:      oBASE64enc_len  => Lunghezza dati convertiti (encodati)
016400161019     P* Parameter:      iBASE64src_ptr  => Puntatore dati da convertire
016500161020     P* Parameter:      iBASE64src_len  => Lunghezza dati da convertire
016600161019     P*--------------------------------------------------
016700161019     P*
016800161019     P UBBASE64R_Encode...
016900161019     P                 B                   EXPORT
017000161019     D*
017100161019     D UBBASE64R_Encode...
017200161019     D                 PI            10I 0
017300161019     DoBASE64enc_dta              65535A   VARYING OPTIONS(*VARSIZE)
017400161020     DoBASE64enc_len                 10I 0
017500161019     DiBASE64src_ptr                   *   VALUE OPTIONS(*STRING)
017600161021     DiBASE64src_len                 10I 0 CONST
017700161019     D*
017800161019      //********************************************************************************************
017900161019      //
018000161019      // Definizione variabili work (locali)
018100161019      //
018200161019      //********************************************************************************************
018300161019     D locMethodCode   S             10I 0
018400161019     D
018500161019
018600161019      /FREE
018700161019        // Inizializzo opcode di metodo a errore
018800161019        locMethodCode = -1;
018900161019
019000161019
019100161019        // Monitorizzo l'intero processo
019200161019        Monitor;
019300161019
019400161019        // Inizializzo i parametri di output
019500161019        clear oBASE64enc_dta;
019600161019        clear oBASE64enc_len;
019700161019
019800161019        // Effettuo l'encoding in Base64
019900161019        B64coded_dst = oBASE64enc_dta;
020000161019        B64src_plain = iBASE64src_ptr;
020100161020        B64src_len   = iBASE64src_len;
020200161019
020300161019        oBASE64enc_len = BASE64_encode(B64coded_dst
020400161019                                      :B64src_plain
020500161020                                      :B64src_len);
020600161019
020700161019        // Se encodato valorizzo parametro di output
020800161019        if oBASE64enc_len > *zeros;
020900161019
021000161019           // Valorizzo parametro di output
021100161019           oBASE64enc_dta = B64coded_dst;
021200161019
021300161019           // Valorizzo opcode di metodo
021400161019           locMethodCode = 0;
021500161019
021600161019        endif;
021700161019
021800161019        On-Error;
021900161019
022000161019           // Esito chiamata già inizializzato a errore e parametri output già puliti
022100161019
022200161019        // Fine monitoring
022300161019        EndMon;
022400161019
022500161019
022600161019        return locMethodCode;
022700161019
022800161019      /END-FREE
022900161019
023000161019     P UBBASE64R_Encode...
023100161019     P                 E
023200161019
023300161019
023400161019
023500161019     P*--------------------------------------------------
023600161019     P* Procedure name: UBBASE64R_Decode
023700161019     P* Purpose:        Converte dati da BASE64
023800161019     P* Returns:        Integer (Status_Code)
023900161019     P* Parameter:      oBASE64dec_dta  => Dati convertiti (decodati)
024000161020     P* Parameter:      iBASE64src_ptr  => Puntatore dati da convertire
024100161020     P* Parameter:      oBASE64dec_len  => Lunghezza dati convertiti (decodati)
024200161020     P*--------------------------------------------------
024300161019     P*
024400161019     P UBBASE64R_Decode...
024500161019     P                 B                   EXPORT
024600161019     D*
024700161019     D UBBASE64R_Decode...
024800161019     D                 PI            10I 0
024900170516     DoBASE64dec_dta              65535A   OPTIONS(*VARSIZE)
025000161019     DiBASE64src_ptr                   *   VALUE OPTIONS(*STRING)
025100161020     DoBASE64dec_len                 10I 0
025200161019     D*
025300161019      //********************************************************************************************
025400161019      //
025500161019      // Definizione variabili work (locali)
025600161019      //
025700161019      //********************************************************************************************
025800161019     D locMethodCode   S             10I 0
025900161019     D
026000161019
026100161019      /FREE
026200161019        // Inizializzo opcode di metodo a errore
026300161019        locMethodCode = -1;
026400161019
026500161019
026600161019        // Monitorizzo l'intero processo
026700161019        Monitor;
026800161019
026900161019        // Inizializzo i parametri di output
027000161019        clear oBASE64dec_dta;
027100161019        clear oBASE64dec_len;
027200161019
027300161019        // Effettuo il decoding da Base64
027400161019        B64plain_dst = oBASE64dec_dta;
027500161019        B64coded_src = iBASE64src_ptr;
027600161019
027700161019        oBASE64dec_len = BASE64_decode(B64plain_dst
027800161019                                      :B64coded_src);
027900161019
028000161019        // Se encodato valorizzo parametro di output
028100161019        if oBASE64dec_len > *zeros;
028200161019
028300161019           // Valorizzo parametro di output
028400161019           oBASE64dec_dta = B64plain_dst;
028500161019
028600161019           // Valorizzo opcode di metodo
028700161019           locMethodCode = 0;
028800161019
028900161019        endif;
029000161019
029100161019        On-Error;
029200161019
029300161019           // Esito chiamata già inizializzato a errore e parametri output già puliti
029400161019
029500161019        // Fine monitoring
029600161019        EndMon;
029700161019
029800161019
029900161019        return locMethodCode;
030000161019
030100161019      /END-FREE
030200161019
030300161019     P UBBASE64R_Decode...
030400161019     P                 E
030500151126
