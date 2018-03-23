000100000000     H Option( *SrcStmt )
000200090610     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('UBTEST')
000300000000     **-- Parameters:  -------------------------------------------------------**
000400090610
000500161021     D/COPY GAITRASRC/SRCPROTOPI,UBIFS
000600161021     D/COPY GAITRASRC/SRCPROTOPR,UBIFS
000700161019     D/COPY GAITRASRC/SRCPROTOPI,UBIFSUTIR
000800161019     D/COPY GAITRASRC/SRCPROTOPR,UBIFSUTIR
000900161019     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64R
001000161019     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64R
001100090521
001200161026     D FD              S             10I 0 INZ(-1)
001300161026     D RC              S             10I 0 INZ
001400161026     D idx             S              5U 0 INZ
001500161026     D outFileName     S            255A   INZ
001600170516     D decDati         S                   INZ LIKE(oIFSUTIdata4MB)
001700161102     D decDatiLen      S             10I 0 INZ
001800170517     D encDati         S                   INZ LIKE(oIFSUTIdata4MB)
001900170516     D encDatiLen      S             10I 0 INZ
002000170517     D prefix_size     S             10I 0 INZ
002100170517     D wrkDatiOut      S               A   INZ LEN(4194304)
002200161028     D wErrore         S               N   INZ
002300161026     D wTipoEncode     S              3A   INZ('BIN')
002400161028     D wUTF8           S               N   INZ
002500161028     D wCCSID          S             10I 0 INZ
002600090520
002700090520     C*
002800161026     C     *entry        plist
002900170516     C                   parm                    pInFile          40
003000161026     C                   parm                    pInCCSID         10
003100170516     C                   parm                    pInTYP            3
003200090610
003300090610      /FREE
003400161024
003500161028        // Inizializzo i parametri di output
003600161028        exsr allInz;
003700161025
003800161025        // Verifico la dimensione in Byte del file richiesto
003900161028        exsr rtvFileSize;
004000161028
004100161028        // Reperisco la CodePage ed il CCSID del file richiesto
004200161030        exsr rtvFileEncoding;
004300161026
004400161025        // Reperisco gli elementi del percorso/nome file
004500161028        exsr rtvFileNameElements;
004600161026
004700161025        // Leggo il contenuto del file
004800170516        exsr rtvReadFile;
004900170516
005000170516        // A seconda del tipo output richiesto => gestisco BINary piuttosto che Base64
005100170516        select;
005200170516          when pInTYP = 'B64';
005300170516
005400170516            // Encodo A Base64
005500170517            exsr exeEncodeLarge;
005600170517
005700170517            decDati = encDati;
005800170517            decDatiLen = %len(encDati);
005900170516
006000170516            // Scarico il buffer ottenuto su file IFS
006100170516            outFileName = '/VAS/WS/' + %trim(oIFSUTIName) +
006200170516                          '_CONV_B64_ENC' + '.' + oIFSUTIExt;
006300170516            exsr writeFile;
006400170517
006500170516
006600170516            // Decodo DA Base64
006700170517            exsr exeDecodeLarge;
006800170516
006900170516            // Scarico il buffer ottenuto su file IFS
007000170516            outFileName = '/VAS/WS/' + %trim(oIFSUTIName) +
007100170516                          '_CONV_B64_DEC' + '.' + oIFSUTIExt;
007200170516            exsr writeFile;
007300170516
007400170516          other;
007500170516
007600170516            decDatiLen = oIFSUTIsizeByte;
007700170516            decDati    = oIFSUTIdata4MB;
007800170516
007900170516            // Scarico il buffer ottenuto su file IFS
008000170516            outFileName = '/VAS/WS/' + %trim(oIFSUTIName) +
008100170516                          '_CONV' + '.' + oIFSUTIExt;
008200170516            exsr writeFile;
008300170516
008400170516        endsl;
008500161026
008600161025
008700161025        // Uscita
008800161025        *inlr = *on;
008900161026
009000161026
009100161026
009200161026
009300161026        //--------------------------------------------------------------
009400161026        // Sub-routine di procedura
009500161026        //--------------------------------------------------------------
009600161028
009700161028
009800161028        BEGSR allInz;
009900161028
010000161028          iIFSUTIfile = pInFile;
010100161028          wCCSID      = %dec(pInCCSID:10:0);
010200161028
010300161028          wErrore     = *off;
010400161028          wTipoEncode = 'BIN';
010500161028          wUTF8       = *off;
010600161028
010700161028          clear decDati;
010800161028          clear decDatiLen;
010900170517          clear encDati;
011000170517          clear encDatiLen;
011100161028
011200161028        ENDSR;
011300161028
011400161028
011500161028
011600161028        BEGSR rtvFileSize;
011700161028
011800161028          // Verifico la dimensione in Byte del file richiesto
011900161028          if not wErrore and
012000161028             UBIFSUTIR_GetFileSizeByte(iIFSUTIfile
012100161028                                   :oIFSUTIsizeByte) < 0;
012200161028             wErrore = *on;
012300161028          endif;
012400161028
012500161028        ENDSR;
012600161028
012700161028
012800161028
012900161028        BEGSR rtvFileEncoding;
013000161028
013100161028          // Reperisco la CodePage ed il CCSID del file richiesto
013200161028          if not wErrore and
013300161028             UBIFSUTIR_GetFileEncoding(iIFSUTIfile
013400161028                                       :oIFSUTIcodePage
013500161028                                       :oIFSUTI_CCSID) < 0;
013600161028             wErrore = *on;
013700161028          endif;
013800161028
013900161028        ENDSR;
014000161028
014100161028
014200161028
014300161028        BEGSR rtvFileNameElements;
014400161028
014500161028          // Reperisco gli elementi del percorso/nome file
014600161028          if not wErrore and
014700161028             UBIFSUTIR_GetFileNameElements(iIFSUTIfile
014800161028                                          :oIFSUTIpath
014900161028                                          :oIFSUTIfullName
015000161028                                          :oIFSUTIName
015100161028                                          :oIFSUTIExt) < 0;
015200161028           wErrore = *on;
015300161028          endif;
015400161028
015500161028        ENDSR;
015600161025
015700161025
015800161025
015900161025        BEGSR rtvReadFile;
016000161025
016100161028          // Se tutto OK
016200161028          if not wErrore;
016300170516
016400170516           select;
016500170516
016600170516             when pInTYP = 'TXT';
016700170516
016800170516             // La dimensione del file reperita diventa il valore di confronto
016900170516             // di input per la lettura dei dati del file (deve coincidere)
017000170516             iIFSUTIsizeByte = oIFSUTIsizeByte;
017100170516
017200170516             if UBIFSUTIR_ReadFileUpTo4MBtext(iIFSUTIfile
017300170516                                             :iIFSUTIsizeByte
017400170516                                             :oIFSUTIsizeByte
017500170516                                             :oIFSUTIdata4MB) = *zeros;
017600170516             else;
017700170516                wErrore = *on;
017800170516             endif;
017900161028
018000170516
018100170516             other;
018200170516
018300170516             // La dimensione del file reperita diventa il valore di confronto
018400170516             // di input per la lettura dei dati del file (deve coincidere)
018500170516             iIFSUTIsizeByte = oIFSUTIsizeByte;
018600170516
018700170518             if UBIFSUTIR_ReadFileUpTo4MB(iIFSUTIfile
018800170518                                         :iIFSUTIsizeByte
018900170518                                         :oIFSUTIsizeByte
019000170518                                         :oIFSUTIdata4MB) = *zeros;
019100170516             else;
019200170516                wErrore = *on;
019300170516             endif;
019400170516
019500170516             endsl;
019600170516
019700161028          endif;
019800161025
019900161025        ENDSR;
020000161028
020100170516
020200161025
020300170517        BEGSR exeEncodeLarge;
020400170517
020500170517            prefix_size = %addr(encDati:*DATA) - %addr(encDati);
020600170517
020700170517            // Encoda to Base64
020800170517            encDatiLen = UBBASE64R_EncodeLarge(%addr(oIFSUTIdata4MB:*DATA)
020900170517                                              :%len(oIFSUTIdata4MB)
021000170517                                              :%addr(wrkDatiOut)
021100170517                                              :%size(wrkDatiOut));
021200170517
021300170517            encDati = %subst(wrkDatiOut:1:encDatiLen);
021400170517
021500170517            if encDatiLen = -1;
021600170516               wErrore = *on;
021700170517               clear encDati;
021800170517               clear encDatiLen;
021900170516            endif;
022000161025
022100161025        ENDSR;
022200170517
022300170517
022400170517
022500170517        BEGSR exeDecodeLarge;
022600170517
022700170517            // Decoda from Base64
022800170517            decDatiLen = UBBASE64R_DecodeLarge(%addr(encDati:*DATA)
022900170517                                              :%len(encDati)
023000170517                                              :%addr(wrkDatiOut)
023100170517                                              :%size(wrkDatiOut));
023200170517
023300170517            decDati = %subst(wrkDatiOut:1:decDatiLen);
023400170517
023500170517            if encDatiLen = -1;
023600170517               wErrore = *on;
023700170517               clear decDati;
023800170517               clear decDatiLen;
023900170517            endif;
024000170517
024100170517        ENDSR;
024200161025
024300161109
024400161025
024500161028        BEGSR writeFile;
024600161025
024700161028          FD = -1;
024800161028
024900161026          // Se tutto OK
025000161026          if not wErrore;
025100161026
025200161025          // Scarico il buffer ottenuto su file IFS avente medesimo nome
025300161025          // con suffisso "_CONV" in cartella superiore
025400161025          // con CCSID 1208
025500161025          if not wErrore AND FD = -1;
025600161025
025700161025             // Rimozione preventiva file
025800161025             FD = IFS_RemoveFile(%trim(outFileName));
025900170516
026000170516             // Se non richiesto uno specifico CCSID si utilizza il medesimo del file di input
026100170516             if wCCSID = *zeros;
026200170516                wCCSID = oIFSUTI_CCSID;
026300170516             endif;
026400161025
026500161025             // Creazione del file vuoto
026600161025             FD = IFS_OpenFile(%trim(outFileName)
026700161025                  :O_CREAT+O_CCSID+O_TRUNC+O_WRONLY
026800161025                  :M_RDWR
026900161025                  :wCCSID);
027000161025
027100161025             if FD < 0;
027200161025             else;
027300161028
027400161025               // Se richiesto output UTF8 aggiugo BOM
027500161025               if wUTF8;
027600161102                  decDati = x'EFBBBF' + decDati;
027700161102                  decDatiLen = decDatiLen + 3;
027800161025               endif;
027900161025
028000161025               // Eseguo write
028100161030               RC = IFS_WriteFile(FD
028200161102                                 :%addr(decDati) + 4
028300161102                                 :decDatiLen);
028400161025
028500161025               // Chiusura file IFS
028600161025               RC = IFS_CloseFile(FD);
028700161025             endif;
028800161025          endif;
028900161026
029000161026          endif;
029100161025
029200161025        ENDSR;
029300161025
029400090710
029500090610
029600090610      /END-FREE
029700161025
