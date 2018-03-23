000100120423      //---------------------------------------------------------------
000200120423      //
000300131022      //?      Sistema Stato del Credito
000400120423      //
000500120423      //---------------------------------------------------------------
000600120423
000700120423     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800120423
000900120423      //---------------------------------------------------------------
001000120423      //?Dichiarazione file.
001100120423      //---------------------------------------------------------------
001200130314
001300131022     fAZORG01L  if   e           k disk
001400131022     fCNACO00F  uf   e           k disk
001500131022     fCNCLP00F  uf   e           k disk
001600131022     fCNIND00F  uf   e           k disk
001700131022     fCNIND02L  if   e           k disk    rename(CNIND000:CNIND02)
001800131022     f                                     prefix(inx:3)
001900131022     fCNIND13L  if   e           k disk    rename(CNIND000:CNIND13)
002000131022     f                                     prefix(inx:3)
002100131022     fTABEL00F  if   e           k disk
002200131025     fWFYCL00F  if   e             disk
002300131028     fTNVRMB12P o    e             printer oflind(*in99) usropn
002400120423
002500120423      //---------------------------------------------------------------
002600120423      //?Definizione costanti.
002700120423      //---------------------------------------------------------------
002800120423
002900120423      //---------------------------------------------------------------
003000120423      //?Definizione schiere.
003100120423      //---------------------------------------------------------------
003200120423
003300120423      //---------------------------------------------------------------
003400120423      //?Definizione aree dati.
003500120423      //---------------------------------------------------------------
003600120423
003700120423      //---------------------------------------------------------------
003800120423      //?Definizione strutture dati.
003900120423      //---------------------------------------------------------------
004000130214
004100130214       // -?Parametri ricevuti?
004200130214     d kpjba         e ds
004300131022
004400131022      // - Dati Organigramma filiali - campo ORGde3
004500131022     d og143         e ds
004600131022
004700131022     d ds_CNACO      e ds                  extname(CNACO00F) inz
004800131022     d ds_CNIND      e ds                  extname(CNIND00F) inz
004900131022     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
005000131022     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
005100131022
005200131022     d TIBS69ds      e ds                  inz
005300131022     d   I69tla      e                     inz('C')
005400131022
005500131022      // - Registra variazioni
005600131022     d TIBS73ds      e ds                  inz
005700131022
005800131022      // - Tabella 4W = Stato del credito
005900131022     d ds4W          e ds                  inz
006000131022
006100131022      // - Status
006200131022     d Psds           sds
006300131022     d   SDSpgm          *proc
006400131022     d   SDSjob              244    253
006500131022     d   SDSuse              254    263
006600120423
006700120423      //---------------------------------------------------------------
006800120423      //?Definizione variabili globali.
006900120423      //---------------------------------------------------------------
007000120423      // - Flags booleani
007100120423     d wEoF            s               n   inz(*off)
007200131022     d wMSG            s               n   inz('0')
007300131022
007400131025     d wData_6         s               d   datfmt(*ymd)
007500131022     d wOggi           s              8  0
007600131022     d wOggi6          s              6  0
007700131022     d w0030           s              3  0
007800131025     d sav_CLPcon      s                   like(CLPcon)
007900120423
008000120423      //---------------------------------------------------------------
008100120423      //?Definizione procedure esterne.
008200120423      //---------------------------------------------------------------
008300131022      // - Memorizzo variazioni
008400131022     d Tibs73r         pr                  extpgm('TIBS73R')
008500131022     d  tibs73ds                           likeds(TIBS73ds)
008600131022     d  ds_cnaco                           likeds(ds_CNACO)
008700131022     d  ds_cnind                           likeds(ds_CNIND)
008800131022     d  ds_cnclp                           likeds(ds_CNCLP)
008900131022     d  ds_fncls                           likeds(ds_FNCLS)
009000131022
009100131022      // - Sottomissione lavoro batch
009200131022     d bch10           pr                  extpgm('BCH10')
009300131022     d  kpjba                              likeds(KPJBA)
009400120423
009500120423      //---------------------------------------------------------------
009600120423      //?Definizione prototipi.
009700120423      //---------------------------------------------------------------
009800131022
009900131022       // -?Reperimento dati anagrafici cliente codificato
010000131022      /copy gaitrasrc/srcprotopr,tibs69r
010100120423
010200120423      //---------------------------------------------------------------
010300120423      //?Definizione key-list.
010400120423      //---------------------------------------------------------------
010500131022       // - File TABEL00F
010600131022     d k03tabel      e ds                  extname(TABEL00F:*key)
010700131022     d                                     prefix(k_)
010800131022     d                                     inz
010900120423
011000120423      //---------------------------------------------------------------
011100120423      //?Riepilogo indicatori.
011200120423      //---------------------------------------------------------------
011300120423
011400120423      //---------------------------------------------------------------
011500120423      //?M A I N - L I N E
011600120423      //---------------------------------------------------------------
011700130214
011800130214     c     *Entry        plist
011900130214     c                   parm                    kpjba
012000120423
012100120423      /free
012200120423
012300120423       //?Operazioni iniziali
012400120423       exsr RoutInz;
012500120423
012600120430       //?Elabora file
012700120430       exsr Elabora;
012800120423
012900120423       //?Operazioni finali
013000120423       exsr RoutEnd;
013100120423
013200120423       //--------------------------------------------------------------
013300120423       //?Operazioni iniziali.
013400120423       //--------------------------------------------------------------
013500120423       BEGSR RoutInz;
013600120706
013700130214       //?Impostazione opzioni per SQL?
013800130214         exec sql   set  option  DynUsrPrf = *Owner,
013900130214                                 CloSqlCsr = *EndMod;
014000131025
014100131025       //?Calcolo oggi
014200131025       wData_6   = %date();
014300131025       wOggi6    = %dec(wData_6);
014400131025       wOggi     = %dec(%date());
014500131023
014600131023       //?se devo Stampare preparo il file
014700131023         IF  %subst(kpjbu:1:1) = 'S';
014800131028           open TNVRMB12P;
014900131023           write YC08220T;
015000131023         ENDIF;
015100131025
015200131025         clear wTOTksc;
015300131025         clear wTOTksc33;
015400131025         clear wTOTksc36;
015500131025         clear wTOTksc39;
015600131025         clear wTOTksu;
015700131025         clear wTOTksu33;
015800131025         clear wTOTksu36;
015900131025         clear wTOTksu39;
016000120423
016100120423       ENDSR;
016200130204
016300130204       //--------------------------------------------------------------
016400130204       //?Elabora
016500130204       //--------------------------------------------------------------
016600130204       BEGSR elabora;
016700130304
016800130304         wEoF = *off;
016900130304
017000131025       //?parto a leggere dal WFYCL, solo 1 codice
017100130304         exec sql
017200131025         DECLARE WFYCL cursor FOR
017300131025         SELECT YCLksc
017400131025         from EDPMBTEMP/WFYCL00F
017500131025         GROUP BY YCLksc
017600131025         ORDER BY YCLksc;
017700130304
017800130304         exec sql
017900131025         open WFYCL;
018000130304         IF sqlcode < 0;
018100130304           wEoF = *on;
018200130304         ENDIF;
018300130304
018400130304         DOW not wEoF;
018500130304           exec sql
018600131025           FETCH NEXT from WFYCL into :YCLksc;
018700130304           IF sqlcod = 100 or sqlcod < 0;
018800130304             wEOF = *on;
018900130304             leave;
019000130304           ENDIF;
019100131022
019200131025       //?Recupero i dati del cliente
019300131025           clear tibs69ds;
019400131025           I69kac = YCLksc;
019500131025           I69kin = YCLksc;
019600131025           I69kcp = YCLksc;
019700131025           I69kcs = YCLksc;
019800131025           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
019900131025           IF  O69err <> *blanks;
020000131025             iter;
020100131025           ENDIF;
020200131028           IF  CLPcon <> ' 39';
020300131025             iter;
020400131025           ENDIF;
020500131025
020600131025           wTOTksu += 1;
020700131025           SELECT;
020800131025             WHEN  CLPcon = ' 33';
020900131025               wTOTksu33 += 1;
021000131025             WHEN  CLPcon = ' 36';
021100131025               wTOTksu36 += 1;
021200131025             WHEN  CLPcon = ' 39';
021300131025               wTOTksu39 += 1;
021400131025           ENDSL;
021500131025
021600131025           sav_CLPcon = CLPcon;
021700131025
021800131022         //?Esplodo il codice cliente per P.Iva o codice fiscale
021900131025           exsr  KSC_Esplodi;
022000130304
022100130304         ENDDO;
022200130304
022300130304       //?Chiudo il cursore
022400130304         exec sql
022500131025           close WFYCL;
022600130204
022700130204       ENDSR;
022800131022
022900131022       //--------------------------------------------------------------
023000131022       //?Esplodo il codice cliente.
023100131022       //--------------------------------------------------------------
023200131022       BEGSR KSC_Esplodi;
023300131022
023400131022       //?Esplodo il cliente per Partita IVA
023500131025         IF  INDiva <> *blanks and INDiva <> '00000000000' and
023600131025             %subst(INDiva:1:2) <> '$$';
023700131025           setll (ACOkut:ACOkcc:INDiva) CNIND02L;
023800131025           reade (ACOkut:ACOkcc:INDiva) CNIND02L;
023900131022           DOW  not %eof(CNIND02L);
024000131022         //?Per ogni KSC letto con stessa p.Iva aggiorno
024100131022             exsr  KSC_Blocca;
024200131025             reade (ACOkut:ACOkcc:INDiva) CNIND02L;
024300131022           ENDDO;
024400131022           leavesr;
024500131022         ENDIF;
024600131022
024700131022       //?Esplodo il cliente per Codice Fiscale
024800131025         IF  INDcdf <> *blanks;
024900131025           setll (ACOkut:ACOkcc:INDcdf:INDiva) CNIND13L;
025000131025           reade (ACOkut:ACOkcc:INDcdf:INDiva) CNIND13L;
025100131022           DOW  not %eof(CNIND13L);
025200131022         //?Per ogni KSC letto con stessa p.Iva aggiorno
025300131022             exsr  KSC_Blocca;
025400131025             reade (ACOkut:ACOkcc:INDcdf:INDiva) CNIND13L;
025500131022           ENDDO;
025600131022           leavesr;
025700131022         ENDIF;
025800131022
025900131022       ENDSR;
026000131022
026100131022       //--------------------------------------------------------------
026200131023       //?Blocco le anagrafiche.
026300131022       //--------------------------------------------------------------
026400131022       BEGSR KSC_Blocca;
026500131022
026600131022       //?Se cliente con NTW LOG o XXX non elaboro
026700131022         clear OG143;
026800131023         w0030 = %dec(%subst(%editc(INXksc:'X'):1:3):3:0);
026900131022         chain (w0030) AZORG01L;
027000131022         IF  %found(AZORG01L);
027100131022           OG143 = ORgde3;
027200131022         ENDIF;
027300131022         IF  §OGntw = 'XXX' or §OGntw = 'LOG';
027400131022           leavesr;
027500131022         ENDIF;
027600131022
027700131022       //?Recupero i dati del cliente
027800131022         clear tibs69ds;
027900131022         I69kac = INXksc;
028000131022         I69kin = INXksc;
028100131022         I69kcp = INXksc;
028200131022         I69kcs = INXksc;
028300131022         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
028400131022
028500131022       //?Se lo stato del credito non è varitato non aggiorno
028600131025         IF  %subst(CLPcon:2:2) = %subst(sav_CLPcon:2:2);
028700131022           leavesr;
028800131022         ENDIF;
028900131025
029000131025       //?Se lo stato del credito è maggiore non aggiorno
029100131025         IF  %subst(CLPcon:2:2) > %subst(sav_CLPcon:2:2);
029200131025           leavesr;
029300131025         ENDIF;
029400131023
029500131023       //?Stampa
029600131023         IF  %subst(kpjbu:1:1) = 'S';
029700131023           exsr Stampa;
029800131023         ENDIF;
029900131023       //?Aggiorna
030000131023         IF  %subst(kpjbu:1:1) = 'A';
030100131023           exsr Aggiorna;
030200131023         ENDIF;
030300131023
030400131023       ENDSR;
030500131023
030600131023       //--------------------------------------------------------------
030700131023       //?Stampa.
030800131023       //--------------------------------------------------------------
030900131023       BEGSR Stampa;
031000131023
031100131025         wTOTksc += 1;
031200131025         SELECT;
031300131025           WHEN  sav_CLPcon = ' 33';
031400131025             wTOTksc33 += 1;
031500131025           WHEN  sav_CLPcon = ' 36';
031600131025             wTOTksc36 += 1;
031700131025           WHEN  sav_CLPcon = ' 39';
031800131025             wTOTksc39 += 1;
031900131025         ENDSL;
032000131025
032100131023         IF  *in99;
032200131023           write YC08220T;
032300131023           eval *in99 = *off;
032400131023         ENDIF;
032500131023
032600131025         PRTyclksc = YCLksc;
032700131025         PRTstcred = %subst(sav_CLPcon:2:2);
032800131023         PRTksc = ACOksc;
032900131023         PRTrag = ACOrag;
033000131025         PRTiva = INDiva;
033100131025         PRTcdf = INDcdf;
033200131023         PRTmsg = 'Cliente da variare con stato del credito';
033300131025         PRTmsg = %trim(PRTmsg) + CLPcon;
033400131023         write YC08220D;
033500131023
033600131023       ENDSR;
033700131022
033800131023       //--------------------------------------------------------------
033900131023       //?Aggiorna.
034000131023       //--------------------------------------------------------------
034100131023       BEGSR Aggiorna;
034200131025
034300131025       //?Recupero i dati della tabella 4W stato del credito
034400131025         clear ds4W;
034500131025         k_TBLkut = 1;
034600131025         k_TBLcod = '4W';
034700131025         clear k_TBLkey;
034800131028         k_TBLkey = ' 39';
034900131025         chain %kds(K03tabel) TABEL00F;
035000131025         IF  %found( TABEL00F );
035100131025           ds4W = TBLuni;
035200131025         ENDIF;
035300131023
035400131022       //?Salvo immagine precedente
035500131022         clear tibs73ds ;
035600131022         IBS73immag = 'P' ;
035700131022         TIBS73R (tibs73ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
035800131022
035900131028       //?Metto condizioni pagamento = 000 (rimessa diretta)
036000131028       //?Metto blocco pagamenti se non già impostato
036100131028         chain(e) (ACOkut:ACOkcc:ACOksc) CNIND00F;
036200131028         IF  %error;
036300131028           leavesr;
036400131028         ENDIF;
036500131028         IF  %found(CNIND00F);
036600131028           IF  %subst(INDopz:1:1) = '0';
036700131028             %subst(INDopz:1:1) = %subst(§4Wpjbkamm:1:1);
036800131028           ENDIF;
036900131028           %subst(INDcdp:4:3) = §4Wcdp;
037000131028           update CNIND000;
037100131028         ENDIF;
037200131025
037300131028       //?Metto stato del credito 39
037400131028       //?Blocco servizio se non già bloccato
037500131022       //?Aggiorno stato del credito
037600131022         chain(e) (ACOkut:ACOkcc:ACOksc) CNCLP00F;
037700131022         IF  %error;
037800131022           leavesr;
037900131022         ENDIF;
038000131022         IF  %found(CNCLP00F);
038100131028           %subst(CLPcon:2:2) = '39';
038200131028           IF  ACOabl <> *blanks and CLPnar <> §4Wblc;
038300131028           ELSE;
038400131028             IF  ACOabl = *blanks;
038500131028               CLPnar = §4Wblc;
038600131028             ENDIF;
038700131028           ENDIF;
038800131022           update CNCLP000;
038900131022         ENDIF;
039000131028
039100131028       //?Blocco anagrafica cliente con causale 001
039200131028       //?Sfleggo anagrafica per la trasmissione
039300131028         chain(e) (ACOkut:ACOkcc:ACOksc) CNACO00F;
039400131028         IF  %error;
039500131028           leavesr;
039600131028         ENDIF;
039700131028         IF  %found(CNACO00F);
039800131028           IF  ACOabl = *blanks;
039900131028             ACOabl = §4Wtbl;
040000131028           ENDIF;
040100131028           ACOduv = wOggi6;
040200131028           clear ACOdtr;
040300131028           clear ACOftr;
040400131028           update CNACO000;
040500131028         ENDIF;
040600131022
040700131022       //?Salvo immagine successiva
040800131022         clear tibs73ds ;
040900131022         IBS73pru   = sdsuse ;
041000131022         IBS73noj   = sdsjob ;
041100131022         IBS73pgm   = 'YCO08200R';
041200131022         IBS73immag = 'D';
041300131022         IBS73cta   = 'M' ;
041400131022         TIBS73R (tibs73ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
041500131022
041600131022       ENDSR;
041700130125
041800120423       //--------------------------------------------------------------
041900120423       //?Operazioni finali.
042000120423       //--------------------------------------------------------------
042100120423       BEGSR RoutEnd;
042200131025
042300131025       //?Se Stampa
042400131025         IF  %subst(kpjbu:1:1) = 'S';
042500131025           write YC08220TT;
042600131025           write YC08220F;
042700131025         ENDIF;
042800131023
042900131023       //?Se aggiorna
043000131023         IF  %subst(kpjbu:1:1) = 'A';
043100131023         //?Sottometto l'azione di trasmissione anagrafica clienti a ProJ
043200131023           kcoaz = 'Y030';
043300131023           kqebi = 'KGIORNO';
043400131023           kbuff = *blanks;
043500131023           BCH10(kpjba);
043600131023         ENDIF;
043700120423
043800120423         *inLR = *on;
043900120423         return;
044000120423
044100120423       ENDSR;
044200051221
044300051221      /end-free
