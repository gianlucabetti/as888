000100170914
000200170914       //--------------------------------------------------------------
000300170914       // Dichiarazione files
000400170914       //--------------------------------------------------------------
000500170914     FTIVGD00F  O    E             DISK
000600170913
000700170913
000800170913       //--------------------------------------------------------------
000900170913       // Definizione prototipi
001000170913       //--------------------------------------------------------------
001100170911
001200170913
001300170913       //--------------------------------------------------------------
001400170913       // Definizione variabili
001500170913       //--------------------------------------------------------------
001600170919     D iKSU            s              8a
001700170919     D iTIP            s              2a
001800170919     D iTKNUM          s             10a
001900170919     D iVBELN          s             10a
002000170919     D iTRACKN         s             35a
002100170919     D iTRKSTAT        s              1a
002200170919     D iTRKSTATD       s              4a
002300170919     D iTRACKTSTMP     s             15s 0
002400170919     D iTRACKLOC       s             50a
002500170919     D iTRACKTEXT      s             80a
002600170919     D oReturnCode     s              1s 0
002700170919
002800170919     D wBufferOut      s           1024a   inz varying
002900170919     D i               s              2s 0 inz
003000170914     D TIS7VASDS     e ds
003100170914     D psds           sds
003200170914     D  procname         *PROC
003300170914
003400170914
003500170914       //--------------------------------------------------------------
003600170914       // Schiere a programma
003700170914       //--------------------------------------------------------------
003800170919     D out_rows        S            100    DIM(25) CTDATA PERRCD(1)
003900170914
004000170907
004100170911
004200170911     C     *Entry        plist
004300170919     C                   parm                    iKSU
004400170919     C                   parm                    iTIP
004500170919     C                   parm                    iTKNUM
004600170919     C                   parm                    iVBELN
004700170919     C                   parm                    iTRACKN
004800170919     C                   parm                    iTRKSTAT
004900170919     C                   parm                    iTRKSTATD
005000170919     C                   parm                    iTRACKTSTMP
005100170919     C                   parm                    iTRACKLOC
005200170919     C                   parm                    iTRACKTEXT
005300170919     C                   parm                    oReturnCode
005400170911
005500170919
005600170907
005700170907       // Monitoraggio intero flusso applicativo
005800170907       Monitor;
005900170914
006000170914       // Operazioni iniziali?
006100170914       exsr RoutInz;
006200170914
006300170914       // Stacco progressivo univoco transazione
006400170914       exsr CalProg;
006500170914
006600170919       // Eseguo "download" della spedizione/status corrente
006700170914       exsr ExeStatus;
006800170914
006900170914       // Al termine rilascio i dati reperiti per avvio processo di traduzione
007000170919       exsr RlsVGD;
007100170914
007200170919       // Se arrivati sin qui => esito OK
007300170919       oReturnCode = 1;
007400170919
007500170914       // Gestione errore imprevisto
007600170914       On-Error;
007700170914
007800170914          // Accensione indicatore di errore
007900170914          *in31 = *on;
008000170914
008100170914       // Fine monitoring
008200170914       Endmon;
008300170919
008400170914       // Uscita
008500170914       exsr exeUscita;
008600170919
008700170919
008800170919
008900170919       //--------------------------------------------------------------
009000170919       // Routine inizializzazioni
009100170919       //--------------------------------------------------------------
009200170919       BEGSR RoutInz;
009300170919
009400170919         clear oReturnCode;
009500170919         clear wBufferOut;
009600170919         clear i;
009700170919         *in31 = *off;
009800170919
009900170919       ENDSR;
010000170914
010100170914
010200170914
010300170914       //--------------------------------------------------------------
010400170914       // Stacco progressivo univoco transazione
010500170914       //--------------------------------------------------------------
010600170914       BEGSR CalProg;
010700170914     C*
010800170914     C* Stacco progressivo univoco download
010900170914     C                   clear                   TIS7VASDS
011000170914     C                   eval      i§7VASOPZ = 'PRG'
011100170914     C                   call(e)   'TIS7VASR1'
011200170914     C                   parm                    TIS7VASDS
011300170914     C*
011400170914     C* Se OK => proseguo
011500170914     C                   if        not %error AND
011600170914     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
011700170914     C                   else
011800170914     C                   seton                                        31
011900170914     C                   exsr      exeUscita
012000170914     C                   endif
012100170914     C*
012200170914       ENDSR;
012300170919
012400170919
012500170919
012600170919       //--------------------------------------------------------------
012700170919       // Eseguo "download" della spedizione/status corrente
012800170919       //--------------------------------------------------------------
012900170919       BEGSR ExeStatus;
013000170919
013100170919        i = 1;
013200170919        dow i <= %elem(out_rows);
013300170919          wBufferOut = out_rows(i);
013400170919          select;
013500170919            when i = 3;
013600170919              wBufferOut = %scanrpl('§§§':%trim(iTKNUM):wBufferOut);
013700170919            when i = 8;
013800170919              wBufferOut = %scanrpl('§§§':%trim(iVBELN):wBufferOut);
013900170919            when i = 9;
014000170919              wBufferOut = %scanrpl('§§§':%trim(iTRACKN):wBufferOut);
014100170919            when i = 10;
014200170919              wBufferOut = %scanrpl('§§§':%trim(iTRKSTAT):wBufferOut);
014300170919            when i = 11;
014400170919              wBufferOut = %scanrpl('§§§':%trim(%char(iTRACKTSTMP)):wBufferOut);
014500170919            when i = 18;
014600170919              wBufferOut = %scanrpl('§§§':%trim(iVBELN):wBufferOut);
014700170919            when i = 19;
014800170919              wBufferOut = %scanrpl('§§§':%trim(%char(iTRACKTSTMP)):wBufferOut);
014900170919            when i = 21;
015000170919              wBufferOut = %scanrpl('§§§':%trim(iTRACKLOC):wBufferOut);
015100170919            when i = 22;
015200170919              wBufferOut = %scanrpl('§§§':%trim(iTRKSTAT):wBufferOut);
015300170919            when i = 23;
015400170919              wBufferOut = %scanrpl('§§§':%trim(iTRACKTEXT):wBufferOut);
015500170919          endsl;
015600170919
015700170919          exsr WriVGD;
015800170919
015900170919          i = i + 1;
016000170919        enddo;
016100170919
016200170919       ENDSR;
016300170914
016400170914
016500170914
016600170914       //--------------------------------------------------------------
016700170914       // Scrittura buffer di output
016800170914       //--------------------------------------------------------------
016900170914       BEGSR WriVGD;
017000170914     C*
017100170914     C                   clear                   tivgd000
017200170914     C                   eval      vgdDTA = wBufferOut
017300170919     C                   eval      vgdTIP = iTIP
017400170919     C                   eval      vgdKSU = iKSU
017500170914     C                   eval      vgdTSC = 'WW'
017600170914     C                   eval      vgdDAT = datcor
017700170914     C                   eval      vgdPRG = o§7VASPRG
017800170914     C                   eval      vgdPGM = procname
017900170914     C                   eval      vgdSTO = '?'
018000170914     C*
018100170914     C                   write     tivgd000
018200170914     C*
018300170914       ENDSR;
018400170914
018500170914
018600170914
018700170914       //--------------------------------------------------------------
018800170914       // Rilascio transazione di output
018900170914       //--------------------------------------------------------------
019000170914       BEGSR RlsVGD;
019100170914     C*
019200170914     C* Finalizzo la transazione
019300170914     C                   eval      i§7VASOPZ  = 'RLS'
019400170919     C                   eval      i§7VASTIP  = iTIP
019500170919     C                   eval      i§7VASKSU  = iKSU
019600170914     C                   eval      i§7VASTSC  = 'WW'
019700170914     C                   eval      i§7VASSTO  = '?'
019800171003     C                   eval      i§7VASSTTO = 'G'
019900170914     C                   eval      i§7VASPRG  = o§7VASPRG
020000170914     C                   call(e)   'TIS7VASR1'
020100170914     C                   parm                    TIS7VASDS
020200170914     C*
020300170914       ENDSR;
020400170914
020500170914
020600170914
020700170914       //--------------------------------------------------------------
020800170914       // Uscita
020900170914       //--------------------------------------------------------------
021000170914       BEGSR exeUscita;
021100170914
021200170919         // Se errore
021300170919         if *in31 = *on;
021400170919            oReturnCode = -1;
021500170914            dump(a);
021600170914         endif;
021700170914
021800170914         *inlr = *on;
021900170914         return;
022000170914
022100170914       ENDSR;
022200170914
022300170914
022400170914
022500170914       //--------------------------------------------------------------
022600170914       // *INZSR
022700170914       //--------------------------------------------------------------
022800170914       BEGSR  *inzsr;
022900170914     C*
023000170914     C* CALCOLA LA DATA CORRENTE
023100170914     C                   z-add     *zeros        datcor            8 0
023200170914     C                   eval      datcor = %dec(%date() : *ISO)
023300170914       ENDSR;
023400170914
023500170914
023600170914** OUTPUT
023700171002<?xml version="1.0" encoding="UTF-8"?>
023800171002<ns0:_-LSGIT_-VS_XSI_SHIPMENT xmlns:ns0="urn:sap-com:document:sap:rfc:functions">
023900171002<IV_TKNUM>§§§</IV_TKNUM>
024000171002<IV_XSITD>BRT</IV_XSITD>
024100171002<TRACK_ITEMS_HEAD>
024200171002<item>
024300171002<VBTYP>8</VBTYP>
024400171002<VBELN>§§§</VBELN>
024500171002<TRACKN>§§§</TRACKN>
024600171002<TRKSTAT>§§§</TRKSTAT>
024700171002<TRACKTSTMP>§§§</TRACKTSTMP>
024800171002<TZNZONE>CET</TRACKTZNZONE>
024900171002</item>
025000171002</TRACK_ITEMS_HEAD>
025100171002<TRACK_EVENTS>
025200171002<item>
025300171002<VBTYP>8</VBTYP>
025400171002<VBELN>§§§</VBELN>
025500171002<TRACKTSTMP>§§§</TRACKTSTMP>
025600171002<TZNZONE>CET</TRACKTZNZONE>
025700171002<TRACKLOC>§§§</TRACKLOC>
025800171002<TRKSTAT>§§§</TRKSTAT>
025900171002<TRACKTEXT>§§§</TRACKTEXT>
026000171002</item>
026100171002</TRACK_EVENTS>
