000100170516     H BNDDIR('UBBNDDIR':'QC2LE')
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
004200170516     **    con ACTGRP(*caller)
004300170516     **      CRTSRVPGM SRVPGM(GAITRAOBJ/UBBASE64R) EXPORT(*ALL)
004400170522     **             EXPORT(*SRCFILE) SRCFILE(GAITRASRC/SRCSRVPGM)
004500170522     **             TEXT('BASE64 - Metodi vari (standard e utente)  ')
004600170522     **             BNDDIR(UBBNDDIR QC2LE) TGTRLS(*CURRENT)
004700170516     **
004800170516     ** 3. Eventualmente si può cancellare il modulo.
004900170516     **
005000071128     **
005100071128     ***********************************************************************************************
005200080612
005300080612
005400080612      //********************************************************************************************
005500080612      //
005600080612      // Definizione file (globali)
005700080612      //
005800080612      //********************************************************************************************
005900071128
006000080612
006100071128      //********************************************************************************************
006200071128      //
006300080612      // Definizione prototipi procedure (globali)
006400071128      //
006500071128      //********************************************************************************************
006600161018     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64R
006700161018     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64R
006800161018     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64
006900161018     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64
007000080612
007100080612
007200071128      //********************************************************************************************
007300071128      //
007400080612      // Definizione variabili work (globali)
007500071128      //
007600071128      //********************************************************************************************
007700151126     D InitDone        S               N   INZ(*off)
007800161017     D
007900170517
008000170517     D b64_alphabet    ds
008100170517     D   alphabet                    64A   inz('-
008200170517     D                                     ABCDEFGHIJKLMNOPQRSTUVWXYZ-
008300170517     D                                     abcdefghijklmnopqrstuvwxyz-
008400170517     D                                     0123456789+/')
008500170517     D   base64f                      1A   dim(64)
008600170517     D                                     overlay(alphabet)
008700170517
008800170517     D b64_reverse     DS
008900170517     D   revalphabet                256A   inz(x'-
009000170517     D                                     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-
009100170517     D                                     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-
009200170517     D                                     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-
009300170517     D                                     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-
009400170517     D                                     FFFFFFFFFFFFFFFFFFFFFFFFFFFF3eFF-
009500170517     D                                     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-
009600170517     D                                     FF3fFFFFFFFFFFFFFFFFFFFFFFFFFFFF-
009700170517     D                                     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-
009800170517     D                                     FF1a1b1c1d1e1f202122FFFFFFFFFFFF-
009900170517     D                                     FF232425262728292a2bFFFFFFFFFFFF-
010000170517     D                                     FFFF2c2d2e2f30313233FFFFFFFFFFFF-
010100170517     D                                     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF-
010200170517     D                                     FF000102030405060708FFFFFFFFFFFF-
010300170517     D                                     FF090a0b0c0d0e0f1011FFFFFFFFFFFF-
010400170517     D                                     FFFF1213141516171819FFFFFFFFFFFF-
010500170517     D                                     3435363738393a3b3c3dFFFFFFFFFFFF-
010600170517     D                                     ')
010700170517     D   base64r                      3U 0 dim(255)
010800170517     D                                     overlay(revalphabet:2)
010900151126
011000160404
011100161018     P*--------------------------------------------------
011200161019     P* Procedure name: UBBASE64R_Encode_Binary
011300161018     P* Purpose:        Converte dati in BASE64
011400161018     P* Returns:        Integer (Status_Code)
011500161019     P* Parameter:      oBASE64enc_dta  => Dati convertiti (encodati)
011600161020     P* Parameter:      oBASE64enc_len  => Lunghezza dati convertiti (encodati)
011700161019     P* Parameter:      iBASE64src_dta  => Dati da convertire
011800161020     P* Parameter:      iBASE64src_len  => Lunghezza dati da convertire
011900161018     P*--------------------------------------------------
012000160404     P*
012100161019     P UBBASE64R_Encode_Binary...
012200160404     P                 B                   EXPORT
012300160404     D*
012400161019     D UBBASE64R_Encode_Binary...
012500160404     D                 PI            10I 0
012600161019     DoBASE64enc_dta              65535A   VARYING OPTIONS(*VARSIZE)
012700161020     DoBASE64enc_len                 10I 0
012800161019     DiBASE64src_dta              65535A   VARYING OPTIONS(*VARSIZE) CONST
012900161021     DiBASE64src_len                 10I 0 CONST
013000160404     D*
013100160404      //********************************************************************************************
013200160404      //
013300160404      // Definizione variabili work (locali)
013400160404      //
013500160404      //********************************************************************************************
013600160404     D locMethodCode   S             10I 0
013700160404     D
013800161018
013900160404      /FREE
014000161017        // Inizializzo opcode di metodo a errore
014100160404        locMethodCode = -1;
014200161017
014300161017
014400161017        // Monitorizzo l'intero processo
014500161017        Monitor;
014600161017
014700161017        // Inizializzo i parametri di output
014800161018        clear oBASE64enc_dta;
014900161020        clear oBASE64enc_len;
015000161018
015100161019        // Effettuo l'encoding in Base64
015200161019        B64coded_dst = oBASE64enc_dta;
015300161019        B64plain_src = iBASE64src_dta;
015400161020        B64src_len   = iBASE64src_len;
015500161019
015600161019        oBASE64enc_len = BASE64_encode_binary(B64coded_dst
015700161019                                             :B64plain_src
015800161020                                             :B64src_len);
015900161019
016000161019        // Se encodato
016100161019        if oBASE64enc_len > *zeros;
016200161019
016300161019           // Valorizzo parametro di output
016400161019           oBASE64enc_dta = B64coded_dst;
016500161019
016600161019           // Valorizzo opcode di metodo
016700161019           locMethodCode = 0;
016800161019
016900161019        endif;
017000161017
017100161017        On-Error;
017200161017
017300161017           // Esito chiamata già inizializzato a errore e parametri output già puliti
017400161017
017500161017        // Fine monitoring
017600161017        EndMon;
017700161017
017800160404
017900160404        return locMethodCode;
018000160404
018100160404      /END-FREE
018200160404
018300161019     P UBBASE64R_Encode_Binary...
018400160404     P                 E
018500161019
018600161019
018700161019
018800161019     P*--------------------------------------------------
018900161019     P* Procedure name: UBBASE64R_Encode
019000161019     P* Purpose:        Converte dati in BASE64
019100161019     P* Returns:        Integer (Status_Code)
019200161019     P* Parameter:      oBASE64enc_dta  => Dati convertiti (encodati)
019300161020     P* Parameter:      oBASE64enc_len  => Lunghezza dati convertiti (encodati)
019400161019     P* Parameter:      iBASE64src_ptr  => Puntatore dati da convertire
019500161020     P* Parameter:      iBASE64src_len  => Lunghezza dati da convertire
019600161019     P*--------------------------------------------------
019700161019     P*
019800161019     P UBBASE64R_Encode...
019900161019     P                 B                   EXPORT
020000161019     D*
020100161019     D UBBASE64R_Encode...
020200161019     D                 PI            10I 0
020300161019     DoBASE64enc_dta              65535A   VARYING OPTIONS(*VARSIZE)
020400161020     DoBASE64enc_len                 10I 0
020500161019     DiBASE64src_ptr                   *   VALUE OPTIONS(*STRING)
020600161021     DiBASE64src_len                 10I 0 CONST
020700161019     D*
020800161019      //********************************************************************************************
020900161019      //
021000161019      // Definizione variabili work (locali)
021100161019      //
021200161019      //********************************************************************************************
021300161019     D locMethodCode   S             10I 0
021400161019     D
021500161019
021600161019      /FREE
021700161019        // Inizializzo opcode di metodo a errore
021800161019        locMethodCode = -1;
021900161019
022000161019
022100161019        // Monitorizzo l'intero processo
022200161019        Monitor;
022300161019
022400161019        // Inizializzo i parametri di output
022500161019        clear oBASE64enc_dta;
022600161019        clear oBASE64enc_len;
022700161019
022800161019        // Effettuo l'encoding in Base64
022900161019        B64coded_dst = oBASE64enc_dta;
023000161019        B64src_plain = iBASE64src_ptr;
023100161020        B64src_len   = iBASE64src_len;
023200161019
023300161019        oBASE64enc_len = BASE64_encode(B64coded_dst
023400161019                                      :B64src_plain
023500161020                                      :B64src_len);
023600161019
023700161019        // Se encodato valorizzo parametro di output
023800161019        if oBASE64enc_len > *zeros;
023900161019
024000161019           // Valorizzo parametro di output
024100161019           oBASE64enc_dta = B64coded_dst;
024200161019
024300161019           // Valorizzo opcode di metodo
024400161019           locMethodCode = 0;
024500161019
024600161019        endif;
024700161019
024800161019        On-Error;
024900161019
025000161019           // Esito chiamata già inizializzato a errore e parametri output già puliti
025100161019
025200161019        // Fine monitoring
025300161019        EndMon;
025400161019
025500161019
025600161019        return locMethodCode;
025700161019
025800161019      /END-FREE
025900161019
026000161019     P UBBASE64R_Encode...
026100161019     P                 E
026200161019
026300161019
026400161019
026500161019     P*--------------------------------------------------
026600161019     P* Procedure name: UBBASE64R_Decode
026700170516     P* Purpose:        Converte dati da BASE64 fino a 64K
026800161019     P* Returns:        Integer (Status_Code)
026900161019     P* Parameter:      oBASE64dec_dta  => Dati convertiti (decodati)
027000161020     P* Parameter:      iBASE64src_ptr  => Puntatore dati da convertire
027100161020     P* Parameter:      oBASE64dec_len  => Lunghezza dati convertiti (decodati)
027200161020     P*--------------------------------------------------
027300161019     P*
027400161019     P UBBASE64R_Decode...
027500161019     P                 B                   EXPORT
027600161019     D*
027700161019     D UBBASE64R_Decode...
027800161019     D                 PI            10I 0
027900170516     DoBASE64dec_dta              65535A   OPTIONS(*VARSIZE)
028000161019     DiBASE64src_ptr                   *   VALUE OPTIONS(*STRING)
028100161020     DoBASE64dec_len                 10I 0
028200161019     D*
028300161019      //********************************************************************************************
028400161019      //
028500161019      // Definizione variabili work (locali)
028600161019      //
028700161019      //********************************************************************************************
028800161019     D locMethodCode   S             10I 0
028900161019     D
029000161019
029100161019      /FREE
029200161019        // Inizializzo opcode di metodo a errore
029300161019        locMethodCode = -1;
029400161019
029500161019
029600161019        // Monitorizzo l'intero processo
029700161019        Monitor;
029800161019
029900161019        // Inizializzo i parametri di output
030000161019        clear oBASE64dec_dta;
030100161019        clear oBASE64dec_len;
030200161019
030300161019        // Effettuo il decoding da Base64
030400161019        B64plain_dst = oBASE64dec_dta;
030500161019        B64coded_src = iBASE64src_ptr;
030600161019
030700161019        oBASE64dec_len = BASE64_decode(B64plain_dst
030800161019                                      :B64coded_src);
030900161019
031000161019        // Se encodato valorizzo parametro di output
031100161019        if oBASE64dec_len > *zeros;
031200161019
031300161019           // Valorizzo parametro di output
031400161019           oBASE64dec_dta = B64plain_dst;
031500161019
031600161019           // Valorizzo opcode di metodo
031700161019           locMethodCode = 0;
031800161019
031900161019        endif;
032000161019
032100161019        On-Error;
032200161019
032300161019           // Esito chiamata già inizializzato a errore e parametri output già puliti
032400161019
032500161019        // Fine monitoring
032600161019        EndMon;
032700161019
032800161019
032900161019        return locMethodCode;
033000161019
033100161019      /END-FREE
033200161019
033300161019     P UBBASE64R_Decode...
033400161019     P                 E
033500170517
033600170517
033700170517
033800170517     P*--------------------------------------------------
033900170517     P* Procedure name: UBBASE64R_EncodeLarge
034000170517     P* Purpose:        Converte dati in BASE64 anche oltre i 64K
034100170517     P* Returns:        Lunghezza dati encodati (se valore > oBASE64b64_siz possibile troncamento)
034200170517     P* Parameter:      iBASE64bin_ptr  => Puntatore a dati da encodare in Base64
034300170517     P* Parameter:      iBASE64bin_len  => Lunghezza dati da convertire
034400170517     P* Parameter:      oBASE64b64_ptr  => Puntatore a dati di output (encodati)
034500170517     P* Parameter:      oBASE64b64_siz  => Size are di memoria puntatore precenete
034600170517     P*--------------------------------------------------
034700170517     P*
034800170517     P UBBASE64R_EncodeLarge...
034900170517     P                 B                   EXPORT
035000170517     D*
035100170517     D UBBASE64R_EncodeLarge...
035200170517     D                 PI            10U 0
035300170517     DiBASE64bin_ptr                   *   VALUE
035400170517     DiBASE64bin_len                 10U 0 VALUE
035500170517     DoBASE64b64_ptr                   *   VALUE
035600170517     DoBASE64b64_siz                 10U 0 VALUE
035700170517     D*
035800170517      //********************************************************************************************
035900170517      //
036000170517      // Definizione variabili work (locali)
036100170517      //
036200170517      //********************************************************************************************
036300170517     D locMethodCode   S             10I 0
036400170517     D
036500170517
036600170517     D                 DS
036700170517     D   Numb                  1      2U 0 inz(0)
036800170517     D   Byte                  2      2A
036900170517
037000170517     D data            DS                  based(iBASE64bin_ptr)
037100170517     D   B1                           1A
037200170517     D   B2                           1A
037300170517     D   B3                           1A
037400170517
037500170517     D OutData         S              4A   based(oBASE64b64_ptr)
037600170517     D Temp            S              4A
037700170517     D Pos             S             10I 0
037800170517     D OutLen          S             10I 0
037900170517     D Save            S              1A
038000170517
038100170517
038200170517      /FREE
038300170517        // Inizializzo opcode di metodo a errore
038400170517        locMethodCode = -1;
038500170517
038600170517
038700170517        // Monitorizzo l'intero processo
038800170517        Monitor;
038900170517
039000170517        // Avvio conversione a "chunk"
039100170517        Pos = 1;
039200170517
039300170517        dow (Pos <= iBASE64bin_len);
039400170517
039500170517           // -------------------------------------------------
039600170517           // First output byte comes from bits 1-6 of input
039700170517           // -------------------------------------------------
039800170517           Byte = %bitand(B1: x'FC');
039900170517           Numb /= 4;
040000170517           %subst(Temp:1) = base64f(Numb+1);
040100170517
040200170517           // -------------------------------------------------
040300170517           // Second output byte comes from bits 7-8 of byte 1
040400170517           //                           and bits 1-4 of byte 2
040500170517           // -------------------------------------------------
040600170517           Byte = %bitand(B1: x'03');
040700170517           Numb *= 16;
040800170517
040900170517           if (Pos+1 <= iBASE64bin_len);
041000170517              Save = Byte;
041100170517              Byte = %bitand(B2: x'F0');
041200170517              Numb /= 16;
041300170517              Byte = %bitor(Save: Byte);
041400170517           endif;
041500170517
041600170517           %subst(Temp: 2) = base64f(Numb+1);
041700170517
041800170517           // -------------------------------------------------
041900170517           // Third output byte comes from bits 5-8 of byte 2
042000170517           //                          and bits 1-2 of byte 3
042100170517           // (or is set to '=' if there was only one byte)
042200170517           // -------------------------------------------------
042300170517           if (Pos+1 > iBASE64bin_len);
042400170517               %subst(Temp: 3) = '=';
042500170517           else;
042600170517               Byte = %bitand(B2: x'0F');
042700170517               Numb *= 4;
042800170517
042900170517               if (Pos+2 <= iBASE64bin_len);
043000170517                   Save = Byte;
043100170517                   Byte = %bitand(B3: x'C0');
043200170517                   Numb /= 64;
043300170517                   Byte = %bitor(Save: Byte);
043400170517               endif;
043500170517
043600170517                %subst(Temp:3) = base64f(Numb+1);
043700170517           endif;
043800170517
043900170517           // -------------------------------------------------
044000170517           // Fourth output byte comes from bits 3-8 of byte 3
044100170517           // (or is set to '=' if there was only one/two bytes)
044200170517           // -------------------------------------------------
044300170517           if (Pos+2 > iBASE64bin_len);
044400170517               %subst(Temp:4:1) = '=';
044500170517           else;
044600170517               Byte = %bitand(B3: x'3F');
044700170517               %subst(Temp:4) = base64f(Numb+1);
044800170517           endif;
044900170517
045000170517           // -------------------------------------------------
045100170517           //   Advance to next chunk of data.
045200170517           // -------------------------------------------------
045300170517           iBASE64bin_ptr += %size(data);
045400170517           Pos += %size(data);
045500170517           OutLen += %size(Temp);
045600170517
045700170517           if (OutLen <= oBASE64b64_siz);
045800170517              OutData = Temp;
045900170517              oBASE64b64_ptr += %size(Temp);
046000170517           endif;
046100170517
046200170517        enddo;
046300170517
046400170517        // Se decodato valorizzo parametro di output
046500170517        if Outlen > *zeros;
046600170517
046700170517           // Valorizzo opcode di metodo
046800170517           locMethodCode = Outlen;
046900170517
047000170517        endif;
047100170517
047200170517        On-Error;
047300170517
047400170517           // Esito chiamata già inizializzato a errore e parametri output già puliti
047500170517
047600170517        // Fine monitoring
047700170517        EndMon;
047800170517
047900170517
048000170517        return locMethodCode;
048100170517
048200170517      /END-FREE
048300170517
048400170517     P UBBASE64R_EncodeLarge...
048500170517     P                 E
048600170516
048700170516
048800170516
048900170516     P*--------------------------------------------------
049000170517     P* Procedure name: UBBASE64R_DecodeLarge
049100170517     P* Purpose:        Converte dati da BASE64 a Binary anche oltre i 64K
049200170517     P* Returns:        Lunghezza dati decodati (se valore > oBASE64bin_siz possibile troncamento)
049300170517     P* Parameter:      iBASE64b64_ptr  => Puntatore a dati in Base64 da decodare
049400170517     P* Parameter:      iBASE64b64_len  => Lunghezza dati in Base64
049500170517     P* Parameter:      oBASE64bin_ptr  => Puntatore a dati di output (decodati)
049600170517     P* Parameter:      oBASE64bin_siz  => Size are di memoria puntatore precenete
049700170516     P*--------------------------------------------------
049800170516     P*
049900170516     P UBBASE64R_DecodeLarge...
050000170516     P                 B                   EXPORT
050100170516     D*
050200170516     D UBBASE64R_DecodeLarge...
050300170517     D                 PI            10U 0
050400170517     DiBASE64b64_ptr                   *   VALUE
050500170517     DiBASE64b64_len                 10U 0 VALUE
050600170517     DoBASE64bin_ptr                   *   VALUE
050700170517     DoBASE64bin_siz                 10U 0 VALUE
050800170516     D*
050900170516      //********************************************************************************************
051000170516      //
051100170516      // Definizione variabili work (locali)
051200170516      //
051300170516      //********************************************************************************************
051400170517     D locMethodCode   S             10I 0
051500170517     D
051600170516
051700170516     D                 DS
051800170516     D   Numb                  1      2U 0 inz(0)
051900170516     D   Byte                  2      2A
052000170516
052100170517     D data            DS                  based(iBASE64b64_ptr)
052200170516     D   B1                           3U 0
052300170516     D   B2                           3U 0
052400170516     D   B3                           3U 0
052500170516     D   B4                           3U 0
052600170516
052700170517     D OutData         S              3A   based(oBASE64bin_ptr)
052800170516     D Temp            S              3A   varying
052900170516     D Pos             S             10I 0
053000170516     D OutLen          S             10I 0
053100170517
053200170516
053300170516      /FREE
053400170516        // Inizializzo opcode di metodo a errore
053500170516        locMethodCode = -1;
053600170516
053700170516
053800170516        // Monitorizzo l'intero processo
053900170516        Monitor;
054000170516
054100170516        // Avvio conversione a "chunk"
054200170516        Pos = 1;
054300170516
054400170517        dow (Pos <= iBASE64b64_len);
054500170517
054600170517           if (base64r(B1)=x'FF') OR
054700170517              (base64r(B2)=x'FF') OR
054800170517              (base64r(B3)=x'FF' and B3<>126) OR
054900170517              (base64r(B4)=x'FF' and B4<>126);
055000170517
055100170517              // Caratteri non validi => uscita con errore
055200170517              return locMethodCode;
055300170517
055400170517           endif;
055500170516
055600170516           // -------------------------------------------------
055700170516           // First output byte comes from bits 3-8 of byte 1
055800170516           //                          and bits 3-4 of byte 2
055900170516           // -------------------------------------------------
056000170516           Numb = base64r(B1) * 4
056100170516                + base64r(B2) / 16;
056200170516           Temp = Byte;
056300170516
056400170516           // -------------------------------------------------
056500170516           // Second output byte comes from bits 5-8 of byte 2
056600170516           //                           and bits 3-6 of byte 3
056700170516           // -------------------------------------------------
056800170516           if %subst(data: 3: 1) <> '=';
056900170516                numb = %bitand(base64r(B2):x'0f') * 16
057000170516                     + base64r(B3) / 4;
057100170516                Temp += Byte;
057200170516           endif;
057300170516
057400170516           // -------------------------------------------------
057500170516           // Third output byte comes from bits 7-8 of byte 3
057600170516           //                          and bits 3-8 of byte 4
057700170516           // (or is set to '=' if there was only one byte)
057800170516           // -------------------------------------------------
057900170516           if %subst(data: 4: 1) <> '=';
058000170516                numb = %bitand(base64r(B3):x'03') * 64
058100170516                     + base64r(B4);
058200170516                Temp += Byte;
058300170516           endif;
058400170516
058500170516           // -------------------------------------------------
058600170516           //   Advance to next chunk of data.
058700170516           // -------------------------------------------------
058800170517           iBASE64b64_ptr += %size(data);
058900170516           Pos += %size(data);
059000170516           OutLen += %len(Temp);
059100170516
059200170517           if (OutLen <= oBASE64bin_siz);
059300170516              OutData = Temp;
059400170517              oBASE64bin_ptr += %len(Temp);
059500170516           endif;
059600170516
059700170516        enddo;
059800170516
059900170516        // Se decodato valorizzo parametro di output
060000170516        if Outlen > *zeros;
060100170516
060200170516           // Valorizzo opcode di metodo
060300170517           locMethodCode = Outlen;
060400170516
060500170516        endif;
060600170516
060700170516        On-Error;
060800170516
060900170516           // Esito chiamata già inizializzato a errore e parametri output già puliti
061000170516
061100170516        // Fine monitoring
061200170516        EndMon;
061300170516
061400170516
061500170516        return locMethodCode;
061600170516
061700170516      /END-FREE
061800170516
061900170516     P UBBASE64R_DecodeLarge...
062000170516     P                 E
062100151126
