000100000000     H Option( *SrcStmt )
000200170518     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300000000     **-- Parameters:  -------------------------------------------------------**
000400090610
000500161019     D/COPY GAITRASRC/SRCPROTOPI,UBIFSUTIR
000600161019     D/COPY GAITRASRC/SRCPROTOPR,UBIFSUTIR
000700161019     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64R
000800161019     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64R
000900170518     D/COPY GAITRASRC/SRCPROTOPI,UBGETIFSR
001000090521
001100170518     D wrkDati         S               A   INZ LEN(4194304)
001200170518     D wrkDatiLen      S             10I 0 INZ
001300090520
001400090520     C*
001500161026     C     *entry        plist
001600170518     C                   parm                    iGETIFSfile
001700170518     C                   parm                    iGETIFSbinTxt
001800170518     C                   parm                    iGETIFSbase64
001900170518     C                   parm                    iGETIFSmaxLen
002000170518     C                   parm                    oGETIFSesito
002100170518     C                   parm                    oGETIFScodPage
002200170518     C                   parm                    oGETIFS_CCSID
002300170518     C                   parm                    oGETIFSlen
002400170518     C                   parm                    oGETIFSdati4MB
002500090610
002600090610      /FREE
002700170518
002800170518        // Monitorizzo tutto (per necessità massima solidità stack chiamate)
002900170518        Monitor;
003000161024
003100170518        // Operazioni iniziali
003200170518        exsr RoutInz;
003300161025
003400161025        // Verifico la dimensione in Byte del file richiesto
003500161028        exsr rtvFileSize;
003600161028
003700161028        // Reperisco la CodePage ed il CCSID del file richiesto
003800161030        exsr rtvFileEncoding;
003900161026
004000170518        // Leggo il contenuto del file
004100170518        exsr rtvReadFile;
004200170518
004300170518        // Eventuale conversione Base64
004400170518        exsr cvtBase64;
004500170518
004600170518        // Se arrivato sin qui => esito OK
004700170518        oGETIFSesito = *zeros;
004800170518
004900170518
005000170518        // Su errore imprevisto
005100170518        On-Error;
005200170518           oGETIFSesito = -1;
005300170518           exsr exeUscita;
005400170518
005500170518        // Fine monitoring
005600170518        Endmon;
005700170518
005800161025
005900170518        // Uscita
006000170518        exsr exeUscita;
006100161026
006200161026
006300161026
006400161026
006500161026        //--------------------------------------------------------------
006600161026        // Sub-routine di procedura
006700161026        //--------------------------------------------------------------
006800161028
006900161028
007000170518        BEGSR RoutInz;
007100161028
007200170518          // Inizializzo esito a errore
007300170518          oGETIFSesito = -1;
007400161028
007500170518          // Inizializzazione campi di work e di output
007600170518          clear oGETIFScodPage;
007700170518          clear oGETIFS_CCSID;
007800170518          clear oGETIFSlen;
007900170518          clear oGETIFSdati4MB;
008000170518          clear wrkDati;
008100170518          clear wrkDatiLen;
008200170518
008300170518          // Controllo i parametri di input
008400170518          exsr CtrlPIn;
008500161028
008600161028        ENDSR;
008700170518
008800170518
008900170518
009000170518        BEGSR  CtrlPIn;
009100170518
009200170518          // Se non specificato nulla assumo 'BIN'
009300170518          if iGETIFSbinTxt = *blanks;
009400170518             iGETIFSbinTxt = 'BIN';
009500170518          endif;
009600170518
009700170518          // I parametri fondamentali devono essere valorizzati
009800170518          if iGETIFSfile = *blanks;
009900170518             oGETIFSesito = -2;
010000170518             exsr exeUscita;
010100170518          endif;
010200170518          if iGETIFSbinTxt <> 'BIN' and iGETIFSbinTxt <> 'TXT';
010300170518             oGETIFSesito = -2;
010400170518             exsr exeUscita;
010500170518          endif;
010600170518          if iGETIFSbase64 <> 'Y' and
010700170518             iGETIFSbase64 <> 'N' and
010800170518             iGETIFSbase64 <> *blanks;
010900170518             oGETIFSesito = -2;
011000170518             exsr exeUscita;
011100170518          endif;
011200170518
011300170518          // Se non specificato nulla assumo 4MB o 3MB
011400170518          if iGETIFSmaxLen < *zeros;
011500170518             iGETIFSmaxLen = *zeros;
011600170518          endif;
011700170518          select;
011800170518            when iGETIFSmaxLen = *zeros and iGETIFSbase64 <> 'Y';
011900170518              iGETIFSmaxLen = 1024*1024*4;
012000170518            when iGETIFSmaxLen = *zeros and iGETIFSbase64  = 'Y';
012100170518              iGETIFSmaxLen = 1024*1024*3;
012200170518          endsl;
012300170518
012400170518        ENDSR;
012500161028
012600170518
012700161028
012800161028        BEGSR rtvFileSize;
012900161028
013000161028          // Verifico la dimensione in Byte del file richiesto
013100170518          if UBIFSUTIR_GetFileSizeByte(iGETIFSfile
013200161028                                   :oIFSUTIsizeByte) < 0;
013300170518             oGETIFSesito = -1;
013400170518             exsr exeUscita;
013500170518          else;
013600170518
013700170518             // Verifico che la dimenzione in Byte del file non sia superiore ai 4MB
013800170518             // o ai 3MB se richiesto encoding Base64
013900170518             if oIFSUTIsizeByte > iGETIFSmaxLen;
014000170518
014100170518                oGETIFSesito = -3;
014200170518                exsr exeUscita;
014300170518
014400170518             endif;
014500170518
014600161028          endif;
014700161028
014800161028        ENDSR;
014900161028
015000161028
015100161028
015200161028        BEGSR rtvFileEncoding;
015300161028
015400161028          // Reperisco la CodePage ed il CCSID del file richiesto
015500170518          if UBIFSUTIR_GetFileEncoding(iGETIFSfile
015600170518                                       :oGETIFScodPage
015700170518                                       :oGETIFS_CCSID) < 0;
015800170518             oGETIFSesito = -1;
015900170518             exsr exeUscita;
016000161028          endif;
016100161028
016200161028        ENDSR;
016300161025
016400161025
016500161025
016600161025        BEGSR rtvReadFile;
016700161025
016800170518          // La dimensione del file reperita diventa il valore di confronto
016900170518          // di input per la lettura dei dati del file (deve coincidere)
017000170518          iIFSUTIsizeByte = oIFSUTIsizeByte;
017100170518
017200170518          // Leggo il file nella modalità richeista in input
017300170518          select;
017400170518
017500170518            when iGETIFSbinTxt = 'BIN';
017600170518              if UBIFSUTIR_ReadFileUpTo4MB(iGETIFSfile
017700170518                                          :iIFSUTIsizeByte
017800170518                                          :oIFSUTIsizeByte
017900170518                                          :oIFSUTIdata4MB) < *zeros;
018000170518                 oGETIFSesito = -1;
018100170518                 exsr exeUscita;
018200170518              endif;
018300170516
018400170518            when iGETIFSbinTxt = 'TXT';
018500170518              if UBIFSUTIR_ReadFileUpTo4MBtext(iGETIFSfile
018600170518                                              :iIFSUTIsizeByte
018700170518                                              :oIFSUTIsizeByte
018800170518                                              :oIFSUTIdata4MB) < *zeros;
018900170518                 oGETIFSesito = -1;
019000170518                 exsr exeUscita;
019100170518              endif;
019200161028
019300170518          endsl;
019400170518
019500170518          // Valorizzo parametri di output
019600170518          oGETIFSlen     = oIFSUTIsizeByte;
019700170518          oGETIFSdati4MB = oIFSUTIdata4MB;
019800161025
019900161025        ENDSR;
020000161028
020100170516
020200161025
020300170518        BEGSR cvtBase64;
020400170518
020500170518          // Se richiesto in input effettuo conversione in Base64
020600170518          if iGETIFSbase64 = 'Y';
020700170517
020800170517            // Encoda to Base64
020900170518            wrkDatiLen = UBBASE64R_EncodeLarge(%addr(oIFSUTIdata4MB:*DATA)
021000170517                                              :%len(oIFSUTIdata4MB)
021100170518                                              :%addr(wrkDati)
021200170518                                              :%size(wrkDati));
021300170518            if wrkDatiLen = -1;
021400170518               clear oGETIFSlen;
021500170518               clear oGETIFSdati4MB;
021600170518
021700170518               oGETIFSesito = -1;
021800170518               exsr exeUscita;
021900170518            else;
022000170518
022100170518               // Valorizzo parametri di output
022200170518               oGETIFSlen     = wrkDatiLen;
022300170518               oGETIFSdati4MB = %subst(wrkDati:1:wrkDatiLen);
022400170516            endif;
022500161025
022600170518          endif;
022700170518
022800161025        ENDSR;
022900170518
023000170518
023100170518
023200170518        BEGSR exeUscita;
023300170518
023400170518          return;
023500170518
023600170518        ENDSR;
023700161025
023800090710
023900090610
024000090610      /END-FREE
024100161025
