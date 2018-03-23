000100160503       //==============================================================
000200160503       //?TNVRFT2R - Elenco Numeri Fattura MANCANTI per Registro Iva.  ?
000300160503       //==============================================================
000400160503
000500160503       //--------------------------------------------------------------
000600160503       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160503       //--------------------------------------------------------------
000800160503
000900160608     /*CMD  ovrdbf file(CNNUM00F) tofile(FILTRAPRD/CNNUM00F) +
001000160608     /*CMD         ovrscope(*calllvl)
001100160609     /*END  dltovr file(CNNUM00F) lvl(*)
001200160503     /*END
001300160503
001400160503       //--------------------------------------------------------------
001500160503       //?Specifiche di controllo.                                     ?
001600160503       //--------------------------------------------------------------
001700160503
001800160503     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900160503     h dftactgrp(*no)
002000160503     h alwnull(*inputonly)
002100160503
002200160503       //--------------------------------------------------------------
002300160503       //?Dichiarazione file.                                          ?
002400160503       //--------------------------------------------------------------
002500160608
002600160608       // -?File Numeratore Fatt./Note x Reg./Libro +CN?
002700160608     fCNNUM00F  if   e           k disk    extfile(wLibFile1)
002800160608     f                                     usropn
002900160503
003000160512       // -?Lista Fatture Mancanti?
003100161021     fTNVRSMFT2Po    e             printer
003200160512     f                                     oflind(*in01)
003300160503
003400160503       //--------------------------------------------------------------
003500160503       //?Definizione costanti.                                        ?
003600160503       //--------------------------------------------------------------
003700160503
003800160503
003900160503       //--------------------------------------------------------------
004000160503       //?Definizione schiere.                                         ?
004100160503       //--------------------------------------------------------------
004200160503
004300160503       // -?Messaggi di errore?
004400160503     d sk_Msg          s             78    dim( 1)  ctdata  perrcd( 1)
004500160503
004600160503       //--------------------------------------------------------------
004700160503       //?Definizione aree dati.                                       ?
004800160503       //--------------------------------------------------------------
004900160503
005000160503       // -?Dati utente?
005100160503     d §AzUte        e ds                  extname(AZUTE00F)
005200160503     d                                     dtaara
005300160503     d §DatiUte      e ds                  extname(dDatiUte)
005400160503     d                                     dtaara
005500160503
005600160503       //--------------------------------------------------------------
005700160503       //?Definizione strutture dati.                                  ?
005800160503       //--------------------------------------------------------------
005900160503
006000160503       // -?Status ds?
006100160503     d Status         sds
006200160503     d   SDSpgm          *proc
006300160503     d*//SDSprm          *parms
006400160503     d*//SDSdta              191    198
006500160503     d*//SDSjob              244    253
006600160503     d   SDSusr              254    263
006700160503
006800160503       // -?Parametri ricevuti
006900160503     d KPJBA         e ds
007000160503     d TNVRFT2ds       ds                  inz  qualified
007100160503     d   VRFTdti                      8s 0 inz
007200160503     d   VRFTdtf                      8s 0 inz
007300160517     d   VRFTlii                      3s 0 inz
007400160517     d   VRFTlif                      3s 0 inz
007500160517     d   VRFTsst                      1    inz
007600160503
007700160503       // -?Dati elaborati via SQL?
007800160509     d Fattura_ds      ds                  inz  qualified
007900160509     d   Libro_Iva                    3  0 inz
008000160509     d   Nr_Fattura                   9  0 inz
008100160609     d   Dt_Fattura                   8  0 inz
008200160509     d   File                        10    inz
008300160509     d Fattura_Save    ds                  inz  likeds( Fattura_ds )
008400160503
008500160503       //--------------------------------------------------------------
008600160503       //?Definizione variabili globali.                               ?
008700160503       //--------------------------------------------------------------
008800160503
008900160503       // -?Flags booleani?
009000160503     d $EoF            s               n   inz
009100160503
009200160503       // -?Stringa SQL da eseguire?
009300160509     d wSQL            s          32740    inz  varying
009400160610     d wSQL2           s          32740    inz  varying
009500160608
009600160608       // -?Nomi estesi Libreria/File?
009700160608     d wLibFile1       s             21a   inz
009800160609       // -?Nomi Libreria?
009900160609     d wLibr1          s             10a   inz
010000160512
010100160512       // -?Campi di comodo per stampa?
010200160512     d wErr            s              1  0 inz
010300160609     d wAAS            s              4  0 inz
010400160609     d wLNP            s              3  0 inz
010500160609     d wNRS            s              2  0 inz
010600160609     d wNSP            s              7  0 inz
010700160609     d wDFT            s              8  0 inz
010800160609     d wNFT            s              6  0 inz
010900160609     d wFIV            s              3  0 inz
011000160609
011100160609       // -?Primo Numero Fattura per la Filiale nel periodo in esame?
011200160609     d wNumPrimaFatt...
011300160609     d                 s              9  0 inz
011400160608
011500160608       // -?Ultimo Numero Fattura per la Filiale e l'Anno in esame?
011600160609     d wNumUltimaFatt...
011700160609     d                 s              9  0 inz
011800160613       // -?Ultimo Numero Fattura elaborata per la Filiale nel periodo in esame?
011900160613     d wNumUltimaFatt_E...
012000160613     d                 s              9  0 inz
012100160609
012200160609       // -?Prima Data Fattura per la ricerca in FIARBT0F?
012300160609     d wDataLimiteFatt...
012400160609     d                 s              8  0 inz
012500160503
012600160512       // -?Date *ISO?
012700160509     d wDate_Iso1      s               d   datfmt(*ISO)    inz(*loval)
012800160509     d wDate_Iso2      s               d   datfmt(*ISO)    inz(*loval)
012900160512       // -?Date *EUR?
013000160509     d wDate_Eur1      s               d   datfmt(*EUR)    inz(*loval)
013100160509     d wDate_Eur2      s               d   datfmt(*EUR)    inz(*hival)
013200160503
013300160503       //--------------------------------------------------------------
013400160503       //?Definizione prototipi procedure e relativi parametri.        ?
013500160503       //--------------------------------------------------------------
013600160503
013700160503       // -?Reperimento dati utente?
013800160503     d TIBS34ds      e ds                  inz
013900160503      /copy gaitrasrc/srcProtoPR,TIBS34R
014000160509
014100160512      *// // -?Reperimento dati tabelle?
014200160512      *///copy gaitrasrc/srcProtoPI,TRULTAB
014300160512      *///copy gaitrasrc/srcProtoPR,TRULTAB
014400160503
014500160503       // -?Parametri API QCAPCMD (Process Commands)?
014600160503     d Qcmd            s           2048    inz  varying
014700160503      /copy qSysInc/qRpgleSrc,QCAPCMD
014800160503       // -?API QCAPCMD (Process Commands)?
014900160503      /copy gaitrasrc/srcProtoPR,QCAPCMD
015000160503
015100160503       // -?Parametri gestione errori API.?
015200160503      /copy qsysinc/qrpglesrc,QUSEC
015300160503
015400160503       //--------------------------------------------------------------
015500160503       //?Definizione key-list.                                        ?
015600160503       //--------------------------------------------------------------
015700160503
015800160608       // -?File CNNUM00F?
015900160608     d keyCNNUM00    e ds                  extname( CNNUM00F : *key )
016000160608     d                                     prefix( k_ )   inz
016100160503
016200160503       //--------------------------------------------------------------
016300160503       //?Riepilogo indicatori utilizzati.                             ?
016400160503       //--------------------------------------------------------------
016500160610       //  40 - Stampa soli totali
016600160503       //--------------------------------------------------------------
016700160503
016800160503       //--------------------------------------------------------------
016900160503       //?M A I N - L I N E                                            ?
017000160503       //--------------------------------------------------------------
017100160503
017200160503     c     *Entry        plist
017300160503     c                   parm                    KPJBA
017400160503
017500160503      /free
017600160503
017700160503       // -?Operazioni iniziali?
017800160503       exsr  sr_RoutInz;
017900160503
018000160512
018100160512       // -?Preparazione stringa e Dichiarazione cursore?
018200160512       exsr  sr_DeclareCursor;
018300160512
018400160512       // -?Apertura cursore?
018500160512       exsr  sr_OpenCursor;
018600160512
018700160512       // -?Estrazione dati?
018800160512       clear  $EoF;
018900160512       DoW  Not $EoF;
019000160512         exsr  sr_ReadCursor;
019100160512       EndDo;
019200160512
019300160512       // -?Chiusura cursore?
019400160512       exsr  sr_CloseCursor;
019500160512
019600160503
019700160503       // -?Operazioni finali?
019800160503       exsr  sr_RoutEnd;
019900160503
020000160503       //--------------------------------------------------------------
020100160503       //?Operazioni iniziali.                                         ?
020200160503       //--------------------------------------------------------------
020300160503       BEGSR  sr_RoutInz;
020400160503
020500160503         // -?Impostazione opzioni per SQL?
020600160503         exec sql   set  option  DynUsrPrf = *Owner,
020700160503                                 CloSqlCsr = *EndMod;
020800160503
020900160503         // -?Impostazione chiusura?
021000160503         *inLR = *on;
021100160609
021200160609         // -?Impostazione Data Limite per ricerca Fattura?
021300160609         wDate_Iso1      = %date() - %months(1);
021400160609         wDataLimiteFatt = ( %subdt( wDate_Iso1 : *years ) * 10000 ) +
021500160609                           ( %subdt( wDate_Iso1 : *months ) * 100 ) +
021600160609                           01;
021700160503
021800160509         // -?Ricezione parametri di input?
021900160509         TNVRFT2ds = KPJBU;
022000160509         wDate_Iso1 = %date( TNVRFT2ds.VRFTdti : *iso );
022100160509         wDate_Iso2 = %date( TNVRFT2ds.VRFTdtf : *iso );
022200160512         wDate_Eur1 = wDate_Iso1;
022300160512         wDate_Eur2 = wDate_Iso2;
022400160503
022500160503         // -?Reperimento dati job?
022600160503         exsr  sr_DatiJob;
022700160512
022800160512         // -?Stampa 1ª testata?
022900160512         T1Cpgm = SDSpgm;
023000160525         T1Cdta = *Date;
023100160512         T1Cora = %dec( %time() );
023200160512         PFTdti = %dec( wDate_Eur1 );
023300160512         PFTdtf = %dec( wDate_Eur2 );
023400160512         write  VRFTtxt;
023500160517         if  TNVRFT2ds.VRFTsst = *blank;
023600160517           write  VRFTtxt2;
023700160517         endif;
023800160517         *in01  = *off;
023900160513
024000160513         // -?Pulizia totali per filiale (Libro Iva)?
024100160513         clear  VRFTtot;
024200160608
024300160608         // -?Apertura file di Filiale?
024400160608         if  %subst( KNSIF : 7 : 3 ) = '201';
024500160609           wLibr1    = 'FILTRA201 ';
024600160608           wLibFile1 = 'FILTRA201/CNNUM00F';
024700160608         else;
024800160609           wLibr1    = 'FILTRAPRD ';
024900160608           wLibFile1 = 'FILTRAPRD/CNNUM00F';
025000160608         endif;
025100160608         open  CNNUM00F;
025200160503
025300160503       ENDSR;
025400160503
025500160503       //--------------------------------------------------------------
025600160503       //?Reperimento Dati del job (Utente/Operativi).                 ?
025700160503       //--------------------------------------------------------------
025800160503       BEGSR  sr_DatiJob;
025900160503
026000160503         in(E) §AzUte;
026100160503         if NOT %error;
026200160503           in(E) §DatiUte;
026300160503         endif;
026400160503         if %error or RSut = *blanks;
026500160503           clear TIBS34ds;
026600160503           tibs34r ( tibs34ds );
026700160503           in §AzUte;
026800160503           in §DatiUte;
026900160503         endif;
027000160503
027100160503       ENDSR;
027200160503
027300160503       //--------------------------------------------------------------
027400160512       //?Preparazione stringa e Dichiarazione Cursore.                ?
027500160503       //--------------------------------------------------------------
027600160512       BEGSR  sr_DeclareCursor;
027700160503
027800160512         // -?Preparazione stringa SQL?
027900160503         clear  wSQL;
028000160509
028100160517         wSQL  = 'select int( substr( REGserReg, 2, 3 ) ) as Filiale, +
028200160509                         REGnrReg as Nr_Fattura, +
028300160609                         ( year( REGdtReg ) * 10000 + +
028400160609                           month( REGdtReg ) * 100 + +
028500160609                           day( REGdtReg ) ) as Dt_Fattura, +
028600160509                         ''NDREG00F'' as File +
028700160509                    from PJBARSOC.NDREG00F +
028800160509                   where REGann = ''0'' +
028900160509                     and REGsocieta = ''201'' +
029000160509                     and REGctb = ''CG'' +
029100160512                     and REGdtReg between ''' + %char( wDate_Iso1 ) +
029200160512                                  ''' and ''' + %char( wDate_Iso2 ) +
029300160517                                  '''';
029400160517
029500160517         if  TNVRFT2ds.VRFTlii <> *zero  or
029600160517             TNVRFT2ds.VRFTlif <> *zero;
029700160517
029800160517           wSQL += ' and substr( REGserReg, 2, 3 ) between ''' +
029900160517                         %editc( TNVRFT2ds.VRFTlii : 'X' ) + ''' and ''' +
030000160517                         %editc( TNVRFT2ds.VRFTlif : 'X' ) + '''';
030100160517
030200160517         endif;
030300160517
030400160517         wSQL +=   ' and REGdocIva = ''1'' +
030500160509                     and left( REGserReg, 1) = ''2'' +
030600160509
030700160509                   UNION +
030800160509
030900160608                  select TASfiv as Filiale, TASnft as Nr_Fattura, +
031000160609                         TASdft as Dt_Fattura, +
031100160509                         ''TITASP0F'' as File +
031200160509                    from TITASP0F +
031300160509                   where TASnft > 0 +
031400160509                     and TASdft between ' +
031500160509                                %editc( TNVRFT2ds.VRFTdti : '3' ) +
031600160509                                  ' and ' +
031700160517                                %editc( TNVRFT2ds.VRFTdtf : '3' );
031800160517
031900160517         if  TNVRFT2ds.VRFTlii <> *zero  or
032000160517             TNVRFT2ds.VRFTlif <> *zero;
032100160517
032200160517           wSQL += ' and TASfiv between ' +
032300160517                         %char( TNVRFT2ds.VRFTlii ) + ' and ' +
032400160517                         %char( TNVRFT2ds.VRFTlif );
032500160517
032600160517         endif;
032700160509
032800160517         wSQL += ' UNION +
032900160509
033000160509                  select TASfiv as Filiale, TASnft as Nr_Fattura, +
033100160609                         TASdft as Dt_Fattura, +
033200160509                         ''TITAS10F'' as File +
033300160509                    from TITAS10F +
033400160509                   where TASnft > 0 +
033500160509                     and TASdft between ' +
033600160509                                %editc( TNVRFT2ds.VRFTdti : '3' ) +
033700160509                                  ' and ' +
033800160517                                %editc( TNVRFT2ds.VRFTdtf : '3' );
033900160517
034000160517         if  TNVRFT2ds.VRFTlii <> *zero  or
034100160517             TNVRFT2ds.VRFTlif <> *zero;
034200160517
034300160517           wSQL += ' and TASfiv between ' +
034400160517                         %char( TNVRFT2ds.VRFTlii ) + ' and ' +
034500160517                         %char( TNVRFT2ds.VRFTlif );
034600160517
034700160517         endif;
034800160509
034900160517         wSQL += ' UNION +
035000160509
035100160509                  select TASfiv as Filiale, TASnft as Nr_Fattura, +
035200160609                         TASdft as Dt_Fattura, +
035300160509                         ''TITAS00F'' as File +
035400160509                    from TITAS00F +
035500160509                   where TASnft > 0 +
035600160509                     and TASdft between ' +
035700160509                                %editc( TNVRFT2ds.VRFTdti : '3' ) +
035800160509                                  ' and ' +
035900160517                                %editc( TNVRFT2ds.VRFTdtf : '3' );
036000160517
036100160517         if  TNVRFT2ds.VRFTlii <> *zero  or
036200160517             TNVRFT2ds.VRFTlif <> *zero;
036300160517
036400160517           wSQL += ' and TASfiv between ' +
036500160517                         %char( TNVRFT2ds.VRFTlii ) + ' and ' +
036600160517                         %char( TNVRFT2ds.VRFTlif );
036700160517
036800160517         endif;
036900160509
037000160517         wSQL += ' order by Filiale, Nr_Fattura, File +
037100160509
037200160509                     for fetch only';
037300160503
037400160503
037500160503         exec sql   prepare S1   from :wSQL;
037600160503
037700160503
037800160512         // -?Dichiarazione del cursore?
037900160503         exec sql   declare C1   cursor for S1;
038000160503
038100160503         if  SQLcode < *zero;
038200160503           exsr  sr_PrintErr;
038300160503         endif;
038400160503
038500160503       ENDSR;
038600160503
038700160503       //--------------------------------------------------------------
038800160503       //?Apertura cursore.                                            ?
038900160503       //--------------------------------------------------------------
039000160503       BEGSR  sr_OpenCursor;
039100160503
039200160503         // -?Apertura del cursore?
039300160503         exec sql   open C1;
039400160503
039500160503         if  SQLcode < *zero;
039600160503           exsr  sr_PrintErr;
039700160503         endif;
039800160503
039900160503       ENDSR;
040000160503
040100160503       //--------------------------------------------------------------
040200160503       //?Lettura cursore.                                             ?
040300160503       //--------------------------------------------------------------
040400160503       BEGSR  sr_ReadCursor;
040500160503
040600160512         clear  Fattura_ds;
040700160503
040800160512         // -?Lettura del cursore?
040900160509         exec sql   fetch next   from C1   into :Fattura_ds;
041000160503
041100160503         Select;
041200160503
041300160503           // -?Fine File?
041400160503           When  SQLcode = 100;
041500160503             $EoF = *on;
041600160516             if  Fattura_Save.Libro_Iva <> *zero;
041700160516               exsr  sr_Fine_LibroIva;
041800160516             endif;
041900160503             leavesr;
042000160503
042100160503           // -?Errore SQL?
042200160503           When  SQLcode < *zero;
042300160503             exsr  sr_PrintErr;
042400160503
042500160509           // -?Controllo singola Fattura?
042600160503           Other;
042700160513             // -?Confronto ultima Fattura reperita con quella precedente?
042800160509             exsr  sr_Ctrl_Fatt;
042900160513             // -?Memorizzazione ultima Fattura reperita?
043000160512             Fattura_Save = Fattura_ds;
043100160503
043200160503         EndSl;
043300160503
043400160503       ENDSR;
043500160503
043600160503       //--------------------------------------------------------------
043700160503       //?Chiusura cursore.                                            ?
043800160503       //--------------------------------------------------------------
043900160503       BEGSR  sr_CloseCursor;
044000160503
044100160503         // -?Chiusura del cursore?
044200160503         exec sql   close C1;
044300160503
044400160503       ENDSR;
044500160503
044600160503       //--------------------------------------------------------------
044700160509       //?Controllo singola Fattura reperita.                          ?
044800160503       //--------------------------------------------------------------
044900160509       BEGSR  sr_Ctrl_Fatt;
045000160503
045100160609         // -?Salvataggio PRIMO numero Fattura per Filiale?
045200160609         if  Fattura_Save.Libro_Iva <> Fattura_ds.Libro_Iva;
045300160609           wNumPrimaFatt = Fattura_ds.Nr_Fattura;
045400160609         endif;
045500160609
045600160512         // -?Stessa Fattura in Entrambi i file: OK?
045700160512         if  Fattura_ds.Libro_Iva  = Fattura_Save.Libro_Iva   and
045800160512             Fattura_ds.Nr_Fattura = Fattura_Save.Nr_Fattura  and
045900160513             %subst( Fattura_ds.File   : 1 : 5 ) = 'TITAS'    and
046000160513             %subst( Fattura_Save.File : 1 : 5 ) = 'NDREG';
046100160512           leavesr;
046200160512         endif;
046300160512
046400160512         // -?Fattura successiva alla precedente (in file diverso): OK?
046500160512         if  Fattura_ds.Libro_Iva  = Fattura_Save.Libro_Iva       and
046600160512             Fattura_ds.Nr_Fattura = Fattura_Save.Nr_Fattura + 1  and
046700160608             %subst( Fattura_ds.File   : 1 : 5 ) = 'NDREG'        and
046800160513             %subst( Fattura_Save.File : 1 : 5 ) = 'TITAS';
046900160613           // -?Salvataggio ULTIMO numero Fattura per Filiale?
047000160613           wNumUltimaFatt_E = Fattura_ds.Nr_Fattura;
047100160512           leavesr;
047200160512         endif;
047300160516
047400160516
047500160516         // -?"Rottura" Libro Iva / Filiale?
047600160516         IF  Fattura_Save.Libro_Iva <> Fattura_ds.Libro_Iva;
047700160516
047800160516           if  Fattura_Save.Libro_Iva <> *zero;
047900160516             exsr  sr_Fine_LibroIva;
048000160516           endif;
048100160608
048200160608           // -?Reperimento Ultimo Numero Fattura dell'anno?
048300160608           //  ?(per scartare gli "storni" di quelle dell'anno precedente)?
048400160609           clear  wNumUltimaFatt;
048500160608           clear  keyCNNUM00;
048600160608           if  %subdt( wDate_Iso1 : *years ) >= 2000;
048700160608             k_NUMaac = %subdt( wDate_Iso1 : *years ) - 2000;
048800160608           else;
048900160608             k_NUMaac = %subdt( wDate_Iso1 : *years ) - 1900;
049000160608           endif;
049100160608           k_NUMrgs = 2;
049200160608           k_NUMlib = Fattura_ds.Libro_Iva;
049300160608           chain  %kds( keyCNNUM00 )  CNNUM000;
049400160608           if  %found(CNNUM00F);
049500160609             wNumUltimaFatt = NUMnum;
049600160608           endif;
049700160516
049800160516           // -?1° errore: Fattura n° 1 mancante in entrambi i file?
049900160516           //  ?(SE Data Iniziale = 1° gennaio)?
050000160516           if  %subdt( wDate_Iso1 : *months ) = 1  and
050100160516               %subdt( wDate_Iso1 : *days )   = 1  and
050200160516               Fattura_ds.Nr_Fattura         <> 1;
050300160516             wErr = 1;
050400160516             exsr  sr_CampiInStampa;
050500160516             exsr  sr_PrintErr;
050600160516             //leavesr;
050700160516           endif;
050800160516
050900160516         ENDIF;
051000160608
051100160608
051200160608         // -?"Storno" di fattura dell'anno precedente: da scartare?
051300160609         if  wNumUltimaFatt        > *zero       and
051400160609             Fattura_ds.Nr_Fattura > wNumUltimaFatt;
051500160608           leavesr;
051600160608         endif;
051700160516
051800160613         // -?Salvataggio ULTIMO numero Fattura per Filiale?
051900160613         wNumUltimaFatt_E = Fattura_ds.Nr_Fattura;
052000160511
052100160516         // -?"Dettaglio"?
052200160516
052300160513         // -?2° errore: Fattura mancante in solo 1 dei 2 file: TITAS?
052400160609         if  Fattura_ds.Libro_Iva  =  Fattura_Save.Libro_Iva   and
052500160609             Fattura_ds.Nr_Fattura <> Fattura_Save.Nr_Fattura  and
052600160609             Fattura_ds.File       =  Fattura_Save.File        and
052700160513             %subst( Fattura_Save.File : 1 : 5 ) = 'NDREG';
052800160513           wErr = 2;
052900160513           exsr  sr_CampiInStampa;
053000160513           exsr  sr_PrintErr;
053100160513           //leavesr;
053200160511         endif;
053300160512
053400160513         // -?3° errore: altra Fattura mancante in entrambi i file?
053500160609         if  Fattura_ds.Libro_Iva  =  Fattura_Save.Libro_Iva   and
053600160512             Fattura_ds.Nr_Fattura <> Fattura_Save.Nr_Fattura + 1;
053700160513           wErr = 3;
053800160512           exsr  sr_CampiInStampa;
053900160512           exsr  sr_PrintErr;
054000160512           //leavesr;
054100160512         endif;
054200160513
054300160513         // -?4° errore: Fattura mancante in solo 1 dei 2 file: NDREG?
054400160609         if  Fattura_ds.Libro_Iva  =  Fattura_Save.Libro_Iva   and
054500160609             Fattura_ds.Nr_Fattura <> Fattura_Save.Nr_Fattura  and
054600160609             Fattura_ds.File       =  Fattura_Save.File        and
054700160513             %subst( Fattura_Save.File : 1 : 5 ) = 'TITAS';
054800160513           wErr = 4;
054900160513           exsr  sr_CampiInStampa;
055000160513           exsr  sr_PrintErr;
055100160513           //leavesr;
055200160513         endif;
055300160513
055400160513         // -?2°+4° errore: Fatture diverse mancanti in 2 file diversi:?
055500160513         //  ?· Fattura_Save.Nr_Fattura = 1 (solo su NDREG)?
055600160513         //  ?· Fattura_ds.Nr_Fattura   = 2 (solo su TITAS)?
055700160609         if  Fattura_ds.Libro_Iva  =  Fattura_Save.Libro_Iva   and
055800160609             Fattura_ds.Nr_Fattura <> Fattura_Save.Nr_Fattura  and
055900160609             Fattura_ds.File       <> Fattura_Save.File        and
056000160609             %subst( Fattura_ds.File   : 1 : 5 ) = 'TITAS'     and
056100160513             %subst( Fattura_Save.File : 1 : 5 ) = 'NDREG';
056200160513           wErr = 2;
056300160513           exsr  sr_CampiInStampa;
056400160513           exsr  sr_PrintErr;
056500160513           wErr = 4;
056600160513           exsr  sr_CampiInStampa;
056700160513           exsr  sr_PrintErr;
056800160513           //leavesr;
056900160513         endif;
057000160503
057100160503       ENDSR;
057200160516
057300160516       //--------------------------------------------------------------
057400160516       //?Fine elaborazione del Libro Iva precedente.                  ?
057500160516       //--------------------------------------------------------------
057600160516       BEGSR  sr_Fine_LibroIva;
057700160516
057800160516         // -?5° errore: Fatt. precedente mancante in solo 1 dei file: TITAS?
057900160516         if  ( Fattura_ds.File =  Fattura_Save.File  or  $EoF )  and
058000160516             %subst( Fattura_Save.File : 1 : 5 ) = 'NDREG';
058100160516           wErr = 5;
058200160516           exsr  sr_CampiInStampa;
058300160516           exsr  sr_PrintErr;
058400160516           //leavesr;
058500160516         endif;
058600160516
058700160516         // -?6° errore: Fatt. precedente mancante in solo 1 dei file: NDREG?
058800160516         if  Fattura_ds.File   =  Fattura_Save.File              and
058900160516             %subst( Fattura_Save.File : 1 : 5 ) = 'TITAS';
059000160516           wErr = 6;
059100160516           exsr  sr_CampiInStampa;
059200160516           exsr  sr_PrintErr;
059300160516           //leavesr;
059400160516         endif;
059500160516
059600160516         // -?Stampa totali del Libro Iva precedente?
059700160516         if  ( PFTtotFM + PFTtotFS ) > *zero;
059800160516           exsr  sr_CampiTotale;
059900160610           *in40 = ( TNVRFT2ds.VRFTsst = *blank );
060000160516           write  VRFTtot;
060100160516           clear  VRFTtot;
060200160516         endif;
060300160516
060400160516         // -?Impostazione indicatore di Overflow per salto-pagina?
060500160517         //  ?(SE non è stata richiesta la stampa dei soli totali)?
060600160517         if  TNVRFT2ds.VRFTsst = *blank;
060700160517           *in01 = *on;
060800160517         endif;
060900160516
061000160516       ENDSR;
061100160512
061200160512       //--------------------------------------------------------------
061300160512       //?Impostazione dei campi di dettaglio in stampa.               ?
061400160512       //--------------------------------------------------------------
061500160512       BEGSR  sr_CampiInStampa;
061600160512
061700160512         clear  VRFTdet;
061800160512
061900160516         // -?Libro Iva / Filiale?
062000160516         if  ( wErr <> 5  and  wErr <> 6 )  or
062100160512             Fattura_ds.Libro_Iva  = Fattura_Save.Libro_Iva;
062200160512           PFTlbi  = Fattura_ds.Libro_Iva;
062300160512         else;
062400160512           PFTlbi  = Fattura_Save.Libro_Iva;
062500160512         endif;
062600160512
062700160512         // -?Numero iniziale Fattura mancante?
062800160512         Select;
062900160512           // -?1° errore: Fattura n° 1 mancante in entrambi i file?
063000160512           When  wErr = 1;
063100160512             PFTnft1 = 1;
063200160513           // -?2° errore: Fattura mancante su TITAS?
063300160512           When  wErr = 2;
063400160516             PFTnft1 = Fattura_Save.Nr_Fattura;
063500160512           // -?3° errore: altra Fattura mancante in entrambi i file?
063600160512           When  wErr = 3;
063700160512             PFTnft1 = Fattura_Save.Nr_Fattura + 1;
063800160513           // -?4° errore: Fattura mancante su NDREG?
063900160513           When  wErr = 4;
064000160516             PFTnft1 = Fattura_ds.Nr_Fattura;
064100160516           // -?5° errore: ultima Fattura mancante su TITAS?
064200160516           When  wErr = 5;
064300160516             PFTnft1 = Fattura_Save.Nr_Fattura;
064400160516           // -?6° errore: ultima Fattura mancante su NDREG?
064500160516           When  wErr = 6;
064600160516             PFTnft1 = Fattura_Save.Nr_Fattura;
064700160512         EndSl;
064800160512
064900160512         // -?Numero finale   Fattura mancante?
065000160516         if  ( wErr = 1   or  wErr = 3 )   and
065100160512             Fattura_ds.Nr_Fattura > PFTnft1 + 1;
065200160512           PFTnftX = '-';
065300160512           PFTnft2 = Fattura_ds.Nr_Fattura - 1;
065400160512         endif;
065500160512
065600160512         // -?Archivio in cui mancano le Fatture?
065700160512         select;
065800160516           when  wErr = 2  or  wErr = 5;
065900160513             PFTfile = 'TITAS     ';
066000160516           when  wErr = 4  or  wErr = 6;
066100160513             PFTfile = 'NDREG00F  ';
066200160513           other;
066300160512             PFTfile = 'Tutti';
066400160512         endsl;
066500160513
066600160513         // -?Aggiornamento TOTALI?
066700160513         if  PFTnft2 > *zero;
066800160513           PFTtotFM += ( PFTnft2 - PFTnft1 + 1);
066900160513         else;
067000160513           PFTtotFM += 1;
067100160513         endif;
067200160516         if  wErr = 4  or  wErr = 6;
067300160513           PFTtotFS += 1;
067400160513         endif;
067500160512
067600160512       ENDSR;
067700160513
067800160513       //--------------------------------------------------------------
067900160513       //?Impostazione dei campi di TOTALE in stampa.                  ?
068000160513       //--------------------------------------------------------------
068100160513       BEGSR  sr_CampiTotale;
068200160513
068300160513         // -?Libro Iva?
068400160513         PFTlbi    = Fattura_Save.Libro_Iva;
068500160513
068600160513         // -?Totale Fatture?
068700160613         //PFTtotNF  = wNumUltimaFatt - wNumPrimaFatt + 1;
068800160613         PFTtotNF  = wNumUltimaFatt_E - wNumPrimaFatt + 1;
068900160513
069000160609         // -?% Fatture mancanti?
069100160613         if  PFTtotNF <> *zero;
069200160613           eval(H) PFTtotPM  = ( PFTtotFM * 100 ) / PFTtotNF;
069300160613         else;
069400160613           clear  PFTtotPM;
069500160613         endif;
069600160609
069700160609         // -?% Fatture (Spedizioni) in FIARBT0F?
069800160613         if  PFTtotNF <> *zero;
069900160613           eval(H) PFTtotPT  = ( PFTtotST * 100 ) / PFTtotNF;
070000160613         else;
070100160613           clear  PFTtotPT;
070200160613         endif;
070300160513
070400160609         // -?% Fatture presenti solo su TITAS?
070500160613         if  PFTtotNF <> *zero;
070600160613           eval(H) PFTtotPS  = ( PFTtotFS * 100 ) / PFTtotNF;
070700160613         else;
070800160613           clear  PFTtotPS;
070900160613         endif;
071000160513
071100160513       ENDSR;
071200160503
071300160503       //--------------------------------------------------------------
071400160512       //?Stampa Fattura MANCANTE (o errore SQL).                      ?
071500160503       //--------------------------------------------------------------
071600160503       BEGSR  sr_PrintErr;
071700160503
071800160509         // -?Test OverFlow?
071900160512         if  *in01;
072000160512           write  VRFTseg;
072100160509           write  VRFTtxt;
072200160517           if  TNVRFT2ds.VRFTsst = *blank;
072300160517             write  VRFTtxt2;
072400160517           endif;
072500160512           *in01  = *off;
072600160509         endif;
072700160503
072800160517         Select;
072900160517
073000160517           // -?Stampa Errore SQL?
073100160517           When  wErr = *zero;
073200160517             // -?Segnalazione errore nello spool?
073300160517             pSQLcode  = SQLcode;
073400160517             pSQLstate = SQLstate;
073500160517             write  VRFTerr;
073600160517             // -?Dump?
073700160517             Dump(A);
073800160517             // -?Job-Log?
073900160517             Qcmd = 'DSPJOBLOG job(*) output(*print)';
074000160517             exsr  sr_ExecCmd;
074100160608             exsr  sr_RoutEnd;
074200160512
074300160517           // -?Stampa Dettaglio Fattura mancante?
074400160517           When  TNVRFT2ds.VRFTsst = *blank;
074500160517             write  VRFTdet;
074600160512
074700160517         EndSl;
074800160609
074900160609         // ·?Controllo esistenza Fatture in FIARBT0F?
075000160609         //  ?(SE mancanti in TITAS)?
075100160609         if  TNVRFT2ds.VRFTsst        = *blank                       and
075200160609             ( wErr = 1  or  wErr = 2  or  wErr = 3  or  wErr = 5 )  and
075300160609             Fattura_Save.Dt_Fattura >= wDataLimiteFatt;
075400160609           clear  wErr;
075500160609           exsr   sr_RicercaInFIARBT0F;
075600160609         endif;
075700160517
075800160517         clear  wErr;
075900160503
076000160503       ENDSR;
076100160609
076200160609       //--------------------------------------------------------------
076300160609       //?Estrazioni fatture presenti in FIARBT0F.                     ?
076400160609       //--------------------------------------------------------------
076500160609       BEGSR  sr_RicercaInFIARBT0F;
076600160609
076700160609         // -?Preparazione stringa SQL?
076800160610         wSQL2 =  'select ARBaas, ARBlnp, ARBnrs, ARBnsp, +
076900160610                          ARBdft, ARBnft, ARBfiv +
077000160610                     from ' + %trimR( wLibr1 ) + '/FIARBT0F +
077100160610                    where ARBfiv = ' + %trim( %editc( PFTlbi : '3' ) ) +
077200160610                    ' and ARBnft ';
077300160609
077400160609         if  PFTnft2 <= *zero;
077500160610           wSQL2 +=             '= ' + %trim( %editc( PFTnft1 : '3' ) );
077600160609         else;
077700160610           wSQL2 +=       'between ' + %trim( %editc( PFTnft1 : '3' ) ) +
077800160610                             ' and ' + %trim( %editc( PFTnft2 : '3' ) );
077900160609         endif;
078000160609
078100160610         wSQL2 += ' order by ARBfiv, ARBnft, ARBdft, +
078200160610                             ARBaas, ARBlnp, ARBnrs, ARBnsp +
078300160610                      for fetch only';
078400160609
078500160610         exec sql   prepare S2   from :wSQL2;
078600160609
078700160609         // -?Dichiarazione del cursore?
078800160609         exec sql   declare C2   cursor for S2;
078900160609
079000160609         // -?Apertura del cursore?
079100160609         exec sql   open C2;
079200160609
079300160609         if  SQLcode < *zero;
079400160609           exsr  sr_PrintErr;
079500160609         endif;
079600160609
079700160609         // -?Lettura del cursore?
079800160609         DoU  SQLcode = 100;
079900160609
080000160609           clear  VRFTdet;
080100160609           clear  wAAS;
080200160609           clear  wLNP;
080300160609           clear  wNRS;
080400160609           clear  wNSP;
080500160609           exec sql   fetch next   from C2   into :wAAS, :wLNP, :wNRS, :wNSP,
080600160609                                                  :wDFT, :wNFT, :wFIV;
080700160609
080800160609           Select;
080900160609             // -?Fine File?
081000160609             When  SQLcode = 100;
081100160609               iter;
081200160609             // -?Errore SQL?
081300160609             When  SQLcode < *zero;
081400160609               exsr  sr_PrintErr;
081500160609             // -?Aggiornamento Totale Spedizioni su FIARBT0F?
081600160609             //  ?e Stampa singola Fattura?
081700160609             Other;
081800160609               PFTtotST += 1;
081900160609               PFTmsg = 'Fatt. ' + %editc( wFIV : 'X' ) +
082000160609                             '-' + %editc( wNFT : '1' ) + ' / +
082100160609                         Sped. ' + %editc( wLNP : 'X' ) +
082200160609                             '-' + %editc( wNRS : 'Z' ) +
082300160609                             '-' + %editc( wNSP : '1' ) +
082400160609                         ' del ' + %editc( wAAS : '3' ) +
082500160609                        ' in FIARBT0F';
082600160609               if  *in01;
082700160609                 write  VRFTseg;
082800160609                 write  VRFTtxt;
082900160609                 if  TNVRFT2ds.VRFTsst = *blank;
083000160609                   write  VRFTtxt2;
083100160609                 endif;
083200160609                 *in01  = *off;
083300160609               endif;
083400160609               write  VRFTdet;
083500160609               clear  PFTmsg;
083600160609           EndSl;
083700160609
083800160609         EndDo;
083900160609
084000160609         // -?Chiusura del cursore?
084100160609         exec sql   close C2;
084200160609
084300160609       ENDSR;
084400160503
084500160503       //--------------------------------------------------------------
084600160503       //?Esecuzione del comando (già impostato).                      ?
084700160503       //--------------------------------------------------------------
084800160503       BEGSR  sr_ExecCmd;
084900160503
085000160503         clear Qcap0100;
085100160503         Qcabcsdh = *off;
085200160503         Qcapa    = *off;
085300160503         Qcacmdss = *off;
085400160503         Qcaerved = *allX'00';
085500160503
085600160503         clear Qusec;
085700160503         Qusbprv  = %size(Qusec);
085800160503
085900160503         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
086000160503                           %size(Qcap0100) : 'CPOP0100' : *omit :
086100160503                           0 : 0 : Qusec);
086200160503
086300160503         //if  Qusei <> *blank;
086400160503         //  ...;
086500160503         //endif;
086600160503
086700160503       ENDSR;
086800160503
086900160503       //--------------------------------------------------------------
087000160503       //?Operazioni finali.                                           ?
087100160503       //--------------------------------------------------------------
087200160503       BEGSR  sr_RoutEnd;
087300160512
087400160512         // -?Stampa Fine Lista?
087500160512         write  VRFTend;
087600160503
087700160512         // -?Uscita?
087800160503         return;
087900160503
088000160503       ENDSR;
088100160503
088200160503      /end-free
088300160503
088400160503       //--------------------------------------------------------------
088500160503       //?Definizione schiere a tempo di compilazione                  ?
088600160503       //--------------------------------------------------------------
088700160503
088800160511** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
088900160512Rilevato errore: consultare la stampa ed avvisare il CED                       1
