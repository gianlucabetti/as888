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
002300131025     fTNVRMB11P o    e             printer oflind(*in99) usropn
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
014800131025           open TNVRMB11P;
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
020200131025           IF  CLPcon <> ' 33' and
020300131028               CLPcon <> ' 36';
020400131028               //CLPcon <> ' 36' and
020500131028               //CLPcon <> ' 39';
020600131025             iter;
020700131025           ENDIF;
020800131025
020900131025           wTOTksu += 1;
021000131025           SELECT;
021100131025             WHEN  CLPcon = ' 33';
021200131025               wTOTksu33 += 1;
021300131025             WHEN  CLPcon = ' 36';
021400131025               wTOTksu36 += 1;
021500131025             WHEN  CLPcon = ' 39';
021600131025               wTOTksu39 += 1;
021700131025           ENDSL;
021800131025
021900131025           sav_CLPcon = CLPcon;
022000131025
022100131022         //?Esplodo il codice cliente per P.Iva o codice fiscale
022200131025           exsr  KSC_Esplodi;
022300130304
022400130304         ENDDO;
022500130304
022600130304       //?Chiudo il cursore
022700130304         exec sql
022800131025           close WFYCL;
022900130204
023000130204       ENDSR;
023100131022
023200131022       //--------------------------------------------------------------
023300131022       //?Esplodo il codice cliente.
023400131022       //--------------------------------------------------------------
023500131022       BEGSR KSC_Esplodi;
023600131022
023700131022       //?Esplodo il cliente per Partita IVA
023800131025         IF  INDiva <> *blanks and INDiva <> '00000000000' and
023900131025             %subst(INDiva:1:2) <> '$$';
024000131025           setll (ACOkut:ACOkcc:INDiva) CNIND02L;
024100131025           reade (ACOkut:ACOkcc:INDiva) CNIND02L;
024200131022           DOW  not %eof(CNIND02L);
024300131022         //?Per ogni KSC letto con stessa p.Iva aggiorno
024400131022             exsr  KSC_Blocca;
024500131025             reade (ACOkut:ACOkcc:INDiva) CNIND02L;
024600131022           ENDDO;
024700131022           leavesr;
024800131022         ENDIF;
024900131022
025000131022       //?Esplodo il cliente per Codice Fiscale
025100131025         IF  INDcdf <> *blanks;
025200131025           setll (ACOkut:ACOkcc:INDcdf:INDiva) CNIND13L;
025300131025           reade (ACOkut:ACOkcc:INDcdf:INDiva) CNIND13L;
025400131022           DOW  not %eof(CNIND13L);
025500131022         //?Per ogni KSC letto con stessa p.Iva aggiorno
025600131022             exsr  KSC_Blocca;
025700131025             reade (ACOkut:ACOkcc:INDcdf:INDiva) CNIND13L;
025800131022           ENDDO;
025900131022           leavesr;
026000131022         ENDIF;
026100131022
026200131022       ENDSR;
026300131022
026400131022       //--------------------------------------------------------------
026500131023       //?Blocco le anagrafiche.
026600131022       //--------------------------------------------------------------
026700131022       BEGSR KSC_Blocca;
026800131022
026900131022       //?Se cliente con NTW LOG o XXX non elaboro
027000131022         clear OG143;
027100131023         w0030 = %dec(%subst(%editc(INXksc:'X'):1:3):3:0);
027200131022         chain (w0030) AZORG01L;
027300131022         IF  %found(AZORG01L);
027400131022           OG143 = ORgde3;
027500131022         ENDIF;
027600131022         IF  §OGntw = 'XXX' or §OGntw = 'LOG';
027700131022           leavesr;
027800131022         ENDIF;
027900131022
028000131022       //?Recupero i dati del cliente
028100131022         clear tibs69ds;
028200131022         I69kac = INXksc;
028300131022         I69kin = INXksc;
028400131022         I69kcp = INXksc;
028500131022         I69kcs = INXksc;
028600131022         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
028700131022
028800131022       //?Se lo stato del credito non è varitato non aggiorno
028900131025         IF  %subst(CLPcon:2:2) = %subst(sav_CLPcon:2:2);
029000131022           leavesr;
029100131022         ENDIF;
029200131025
029300131025       //?Se lo stato del credito è maggiore non aggiorno
029400131025         IF  %subst(CLPcon:2:2) > %subst(sav_CLPcon:2:2);
029500131025           leavesr;
029600131025         ENDIF;
029700131023
029800131023       //?Stampa
029900131023         IF  %subst(kpjbu:1:1) = 'S';
030000131023           exsr Stampa;
030100131023         ENDIF;
030200131023       //?Aggiorna
030300131023         IF  %subst(kpjbu:1:1) = 'A';
030400131023           exsr Aggiorna;
030500131023         ENDIF;
030600131023
030700131023       ENDSR;
030800131023
030900131023       //--------------------------------------------------------------
031000131023       //?Stampa.
031100131023       //--------------------------------------------------------------
031200131023       BEGSR Stampa;
031300131023
031400131025         wTOTksc += 1;
031500131025         SELECT;
031600131025           WHEN  sav_CLPcon = ' 33';
031700131025             wTOTksc33 += 1;
031800131025           WHEN  sav_CLPcon = ' 36';
031900131025             wTOTksc36 += 1;
032000131025           WHEN  sav_CLPcon = ' 39';
032100131025             wTOTksc39 += 1;
032200131025         ENDSL;
032300131025
032400131023         IF  *in99;
032500131023           write YC08220T;
032600131023           eval *in99 = *off;
032700131023         ENDIF;
032800131023
032900131025         PRTyclksc = YCLksc;
033000131025         PRTstcred = %subst(sav_CLPcon:2:2);
033100131023         PRTksc = ACOksc;
033200131023         PRTrag = ACOrag;
033300131025         PRTiva = INDiva;
033400131025         PRTcdf = INDcdf;
033500131023         PRTmsg = 'Cliente da variare con stato del credito';
033600131025         PRTmsg = %trim(PRTmsg) + CLPcon;
033700131023         write YC08220D;
033800131023
033900131023       ENDSR;
034000131022
034100131023       //--------------------------------------------------------------
034200131023       //?Aggiorna.
034300131023       //--------------------------------------------------------------
034400131023       BEGSR Aggiorna;
034500131025
034600131025       //?Recupero i dati della tabella 4W stato del credito
034700131025         clear ds4W;
034800131025         k_TBLkut = 1;
034900131025         k_TBLcod = '4W';
035000131025         clear k_TBLkey;
035100131025         %subst(k_TBLkey:2:2) = %subst(sav_CLPcon:2:2);
035200131025         chain %kds(K03tabel) TABEL00F;
035300131025         IF  %found( TABEL00F );
035400131025           ds4W = TBLuni;
035500131025         ENDIF;
035600131023
035700131022       //?Salvo immagine precedente
035800131022         clear tibs73ds ;
035900131022         IBS73immag = 'P' ;
036000131022         TIBS73R (tibs73ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
036100131022
036200131022       //?Aggiorno blocco pagamenti solo se non è già impostato
036300131022         IF  %subst(INDopz:1:1) = '0';
036400131023           chain(e) (ACOkut:ACOkcc:ACOksc) CNIND00F;
036500131022           IF  %error;
036600131022             leavesr;
036700131022           ENDIF;
036800131022           IF  %found(CNIND00F);
036900131022             %subst(INDopz:1:1) = %subst(§4Wpjbkamm:1:1);
037000131022             update CNIND000;
037100131022           ENDIF;
037200131022         ENDIF;
037300131022
037400131025       //?Se stato del credito 39 metto 36
037500131025         IF  %subst(sav_CLPcon:2:2) = '39';
037600131025           %subst(sav_CLPcon:2:2) = '36';
037700131025         ENDIF;
037800131025
037900131022       //?Aggiorno stato del credito
038000131022         chain(e) (ACOkut:ACOkcc:ACOksc) CNCLP00F;
038100131022         IF  %error;
038200131022           leavesr;
038300131022         ENDIF;
038400131022         IF  %found(CNCLP00F);
038500131025           %subst(CLPcon:2:2) = %subst(sav_CLPcon:2:2);
038600131022           update CNCLP000;
038700131022         ENDIF;
038800131022
038900131022       //?Sfleggo anagrafica per la trasmissione
039000131022         chain(e) (ACOkut:ACOkcc:ACOksc) CNACO00F;
039100131022         IF  %error;
039200131022           leavesr;
039300131022         ENDIF;
039400131022         IF  %found(CNACO00F);
039500131022           ACOduv = wOggi6;
039600131022           clear ACOdtr;
039700131022           clear ACOftr;
039800131022           update CNACO000;
039900131022         ENDIF;
040000131022
040100131022       //?Salvo immagine successiva
040200131022         clear tibs73ds ;
040300131022         IBS73pru   = sdsuse ;
040400131022         IBS73noj   = sdsjob ;
040500131022         IBS73pgm   = 'YCO08200R';
040600131022         IBS73immag = 'D';
040700131022         IBS73cta   = 'M' ;
040800131022         TIBS73R (tibs73ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
040900131022
041000131022       ENDSR;
041100130125
041200120423       //--------------------------------------------------------------
041300120423       //?Operazioni finali.
041400120423       //--------------------------------------------------------------
041500120423       BEGSR RoutEnd;
041600131025
041700131025       //?Se Stampa
041800131025         IF  %subst(kpjbu:1:1) = 'S';
041900131025           write YC08220TT;
042000131025           write YC08220F;
042100131025         ENDIF;
042200131023
042300131023       //?Se aggiorna
042400131023         IF  %subst(kpjbu:1:1) = 'A';
042500131023         //?Sottometto l'azione di trasmissione anagrafica clienti a ProJ
042600131023           kcoaz = 'Y030';
042700131023           kqebi = 'KGIORNO';
042800131023           kbuff = *blanks;
042900131023           BCH10(kpjba);
043000131023         ENDIF;
043100120423
043200120423         *inLR = *on;
043300120423         return;
043400120423
043500120423       ENDSR;
043600051221
043700051221      /end-free
