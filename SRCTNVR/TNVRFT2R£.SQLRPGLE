000100160503       //==============================================================
000200160503       //?TNVRFT2R - Elenco Numeri Fattura MANCANTI per Registro Iva.  ?
000300160503       //==============================================================
000400160503
000500160503       //--------------------------------------------------------------
000600160503       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160503       //--------------------------------------------------------------
000800160503
000900160503     /*END
001000160503
001100160503       //--------------------------------------------------------------
001200160503       //?Specifiche di controllo.                                     ?
001300160503       //--------------------------------------------------------------
001400160503
001500160503     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600160503     h dftactgrp(*no)
001700160503     h alwnull(*inputonly)
001800160503
001900160503       //--------------------------------------------------------------
002000160503       //?Dichiarazione file.                                          ?
002100160503       //--------------------------------------------------------------
002200160503
002300160512       // -?Lista Fatture Mancanti?
002400160512     fTNVRFT2P  o    e             printer
002500160512     f                                     oflind(*in01)
002600160503
002700160503       //--------------------------------------------------------------
002800160503       //?Definizione costanti.                                        ?
002900160503       //--------------------------------------------------------------
003000160503
003100160503
003200160503       //--------------------------------------------------------------
003300160503       //?Definizione schiere.                                         ?
003400160503       //--------------------------------------------------------------
003500160503
003600160503       // -?Messaggi di errore?
003700160503     d sk_Msg          s             78    dim( 1)  ctdata  perrcd( 1)
003800160503
003900160503       //--------------------------------------------------------------
004000160503       //?Definizione aree dati.                                       ?
004100160503       //--------------------------------------------------------------
004200160503
004300160503       // -?Dati utente?
004400160503     d §AzUte        e ds                  extname(AZUTE00F)
004500160503     d                                     dtaara
004600160503     d §DatiUte      e ds                  extname(dDatiUte)
004700160503     d                                     dtaara
004800160503
004900160503       //--------------------------------------------------------------
005000160503       //?Definizione strutture dati.                                  ?
005100160503       //--------------------------------------------------------------
005200160503
005300160503       // -?Status ds?
005400160503     d Status         sds
005500160503     d   SDSpgm          *proc
005600160503     d*//SDSprm          *parms
005700160503     d*//SDSdta              191    198
005800160503     d*//SDSjob              244    253
005900160503     d   SDSusr              254    263
006000160503
006100160503       // -?Parametri ricevuti
006200160503     d KPJBA         e ds
006300160503     d TNVRFT2ds       ds                  inz  qualified
006400160503     d   VRFTdti                      8s 0 inz
006500160503     d   VRFTdtf                      8s 0 inz
006600160517     d   VRFTlii                      3s 0 inz
006700160517     d   VRFTlif                      3s 0 inz
006800160517     d   VRFTsst                      1    inz
006900160503
007000160503       // -?Dati elaborati via SQL?
007100160509     d Fattura_ds      ds                  inz  qualified
007200160509     d   Libro_Iva                    3  0 inz
007300160509     d   Nr_Fattura                   9  0 inz
007400160509     d   File                        10    inz
007500160509     d Fattura_Save    ds                  inz  likeds( Fattura_ds )
007600160503
007700160503       //--------------------------------------------------------------
007800160503       //?Definizione variabili globali.                               ?
007900160503       //--------------------------------------------------------------
008000160503
008100160503       // -?Flags booleani?
008200160503     d $EoF            s               n   inz
008300160503
008400160503       // -?Stringa SQL da eseguire?
008500160509     d wSQL            s          32740    inz  varying
008600160512
008700160512       // -?Campi di comodo per stampa?
008800160512     d wErr            s              1  0 inz
008900160503
009000160512       // -?Date *ISO?
009100160509     d wDate_Iso1      s               d   datfmt(*ISO)    inz(*loval)
009200160509     d wDate_Iso2      s               d   datfmt(*ISO)    inz(*loval)
009300160512       // -?Date *EUR?
009400160509     d wDate_Eur1      s               d   datfmt(*EUR)    inz(*loval)
009500160509     d wDate_Eur2      s               d   datfmt(*EUR)    inz(*hival)
009600160503
009700160503       //--------------------------------------------------------------
009800160503       //?Definizione prototipi procedure e relativi parametri.        ?
009900160503       //--------------------------------------------------------------
010000160503
010100160503       // -?Reperimento dati utente?
010200160503     d TIBS34ds      e ds                  inz
010300160503      /copy gaitrasrc/srcProtoPR,TIBS34R
010400160509
010500160512      *// // -?Reperimento dati tabelle?
010600160512      *///copy gaitrasrc/srcProtoPI,TRULTAB
010700160512      *///copy gaitrasrc/srcProtoPR,TRULTAB
010800160503
010900160503       // -?Parametri API QCAPCMD (Process Commands)?
011000160503     d Qcmd            s           2048    inz  varying
011100160503      /copy qSysInc/qRpgleSrc,QCAPCMD
011200160503       // -?API QCAPCMD (Process Commands)?
011300160503      /copy gaitrasrc/srcProtoPR,QCAPCMD
011400160503
011500160503       // -?Parametri gestione errori API.?
011600160503      /copy qsysinc/qrpglesrc,QUSEC
011700160503
011800160503       //--------------------------------------------------------------
011900160503       //?Definizione key-list.                                        ?
012000160503       //--------------------------------------------------------------
012100160503
012200160503
012300160503       //--------------------------------------------------------------
012400160503       //?Riepilogo indicatori utilizzati.                             ?
012500160503       //--------------------------------------------------------------
012600160503       //--------------------------------------------------------------
012700160503
012800160503       //--------------------------------------------------------------
012900160503       //?M A I N - L I N E                                            ?
013000160503       //--------------------------------------------------------------
013100160503
013200160503     c     *Entry        plist
013300160503     c                   parm                    KPJBA
013400160503
013500160503      /free
013600160503
013700160503       // -?Operazioni iniziali?
013800160503       exsr  sr_RoutInz;
013900160503
014000160512
014100160512       // -?Preparazione stringa e Dichiarazione cursore?
014200160512       exsr  sr_DeclareCursor;
014300160512
014400160512       // -?Apertura cursore?
014500160512       exsr  sr_OpenCursor;
014600160512
014700160512       // -?Estrazione dati?
014800160512       clear  $EoF;
014900160512       DoW  Not $EoF;
015000160512         exsr  sr_ReadCursor;
015100160512       EndDo;
015200160512
015300160512       // -?Chiusura cursore?
015400160512       exsr  sr_CloseCursor;
015500160512
015600160503
015700160503       // -?Operazioni finali?
015800160503       exsr  sr_RoutEnd;
015900160503
016000160503       //--------------------------------------------------------------
016100160503       //?Operazioni iniziali.                                         ?
016200160503       //--------------------------------------------------------------
016300160503       BEGSR  sr_RoutInz;
016400160503
016500160503         // -?Impostazione opzioni per SQL?
016600160503         exec sql   set  option  DynUsrPrf = *Owner,
016700160503                                 CloSqlCsr = *EndMod;
016800160503
016900160503         // -?Impostazione chiusura?
017000160503         *inLR = *on;
017100160503
017200160509         // -?Ricezione parametri di input?
017300160509         TNVRFT2ds = KPJBU;
017400160509         wDate_Iso1 = %date( TNVRFT2ds.VRFTdti : *iso );
017500160509         wDate_Iso2 = %date( TNVRFT2ds.VRFTdtf : *iso );
017600160512         wDate_Eur1 = wDate_Iso1;
017700160512         wDate_Eur2 = wDate_Iso2;
017800160503
017900160503         // -?Reperimento dati job?
018000160503         exsr  sr_DatiJob;
018100160512
018200160512         // -?Stampa 1ª testata?
018300160512         T1Cpgm = SDSpgm;
018400160525         T1Cdta = *Date;
018500160512         T1Cora = %dec( %time() );
018600160512         PFTdti = %dec( wDate_Eur1 );
018700160512         PFTdtf = %dec( wDate_Eur2 );
018800160512         write  VRFTtxt;
018900160517         if  TNVRFT2ds.VRFTsst = *blank;
019000160517           write  VRFTtxt2;
019100160517         endif;
019200160517         *in01  = *off;
019300160513
019400160513         // -?Pulizia totali per filiale (Libro Iva)?
019500160513         clear  VRFTtot;
019600160503
019700160503       ENDSR;
019800160503
019900160503       //--------------------------------------------------------------
020000160503       //?Reperimento Dati del job (Utente/Operativi).                 ?
020100160503       //--------------------------------------------------------------
020200160503       BEGSR  sr_DatiJob;
020300160503
020400160503         in(E) §AzUte;
020500160503         if NOT %error;
020600160503           in(E) §DatiUte;
020700160503         endif;
020800160503         if %error or RSut = *blanks;
020900160503           clear TIBS34ds;
021000160503           tibs34r ( tibs34ds );
021100160503           in §AzUte;
021200160503           in §DatiUte;
021300160503         endif;
021400160503
021500160503       ENDSR;
021600160503
021700160503       //--------------------------------------------------------------
021800160512       //?Preparazione stringa e Dichiarazione Cursore.                ?
021900160503       //--------------------------------------------------------------
022000160512       BEGSR  sr_DeclareCursor;
022100160503
022200160512         // -?Preparazione stringa SQL?
022300160503         clear  wSQL;
022400160509
022500160517         wSQL  = 'select int( substr( REGserReg, 2, 3 ) ) as Filiale, +
022600160509                         REGnrReg as Nr_Fattura, +
022700160509                         ''NDREG00F'' as File +
022800160509                    from PJBARSOC.NDREG00F +
022900160509                   where REGann = ''0'' +
023000160509                     and REGsocieta = ''201'' +
023100160509                     and REGctb = ''CG'' +
023200160512                     and REGdtReg between ''' + %char( wDate_Iso1 ) +
023300160512                                  ''' and ''' + %char( wDate_Iso2 ) +
023400160517                                  '''';
023500160517
023600160517         if  TNVRFT2ds.VRFTlii <> *zero  or
023700160517             TNVRFT2ds.VRFTlif <> *zero;
023800160517
023900160517           wSQL += ' and substr( REGserReg, 2, 3 ) between ''' +
024000160517                         %editc( TNVRFT2ds.VRFTlii : 'X' ) + ''' and ''' +
024100160517                         %editc( TNVRFT2ds.VRFTlif : 'X' ) + '''';
024200160517
024300160517         endif;
024400160517
024500160517         wSQL +=   ' and REGdocIva = ''1'' +
024600160509                     and left( REGserReg, 1) = ''2'' +
024700160509
024800160509                   UNION +
024900160509
025000160509                  select TASfiv as Filiale, +
025100160509                         TASnft as Nr_Fattura, +
025200160509                         ''TITASP0F'' as File +
025300160509                    from TITASP0F +
025400160509                   where TASnft > 0 +
025500160509                     and TASdft between ' +
025600160509                                %editc( TNVRFT2ds.VRFTdti : '3' ) +
025700160509                                  ' and ' +
025800160517                                %editc( TNVRFT2ds.VRFTdtf : '3' );
025900160517
026000160517         if  TNVRFT2ds.VRFTlii <> *zero  or
026100160517             TNVRFT2ds.VRFTlif <> *zero;
026200160517
026300160517           wSQL += ' and TASfiv between ' +
026400160517                         %char( TNVRFT2ds.VRFTlii ) + ' and ' +
026500160517                         %char( TNVRFT2ds.VRFTlif );
026600160517
026700160517         endif;
026800160509
026900160517         wSQL += ' UNION +
027000160509
027100160509                  select TASfiv as Filiale, TASnft as Nr_Fattura, +
027200160509                         ''TITAS10F'' as File +
027300160509                    from TITAS10F +
027400160509                   where TASnft > 0 +
027500160509                     and TASdft between ' +
027600160509                                %editc( TNVRFT2ds.VRFTdti : '3' ) +
027700160509                                  ' and ' +
027800160517                                %editc( TNVRFT2ds.VRFTdtf : '3' );
027900160517
028000160517         if  TNVRFT2ds.VRFTlii <> *zero  or
028100160517             TNVRFT2ds.VRFTlif <> *zero;
028200160517
028300160517           wSQL += ' and TASfiv between ' +
028400160517                         %char( TNVRFT2ds.VRFTlii ) + ' and ' +
028500160517                         %char( TNVRFT2ds.VRFTlif );
028600160517
028700160517         endif;
028800160509
028900160517         wSQL += ' UNION +
029000160509
029100160509                  select TASfiv as Filiale, TASnft as Nr_Fattura, +
029200160509                         ''TITAS00F'' as File +
029300160509                    from TITAS00F +
029400160509                   where TASnft > 0 +
029500160509                     and TASdft between ' +
029600160509                                %editc( TNVRFT2ds.VRFTdti : '3' ) +
029700160509                                  ' and ' +
029800160517                                %editc( TNVRFT2ds.VRFTdtf : '3' );
029900160517
030000160517         if  TNVRFT2ds.VRFTlii <> *zero  or
030100160517             TNVRFT2ds.VRFTlif <> *zero;
030200160517
030300160517           wSQL += ' and TASfiv between ' +
030400160517                         %char( TNVRFT2ds.VRFTlii ) + ' and ' +
030500160517                         %char( TNVRFT2ds.VRFTlif );
030600160517
030700160517         endif;
030800160509
030900160517         wSQL += ' order by Filiale, Nr_Fattura, File +
031000160509
031100160509                     for fetch only';
031200160503
031300160503
031400160503         exec sql   prepare S1   from :wSQL;
031500160503
031600160503
031700160512         // -?Dichiarazione del cursore?
031800160503         exec sql   declare C1   cursor for S1;
031900160503
032000160503         if  SQLcode < *zero;
032100160503           exsr  sr_PrintErr;
032200160503         endif;
032300160503
032400160503       ENDSR;
032500160503
032600160503       //--------------------------------------------------------------
032700160503       //?Apertura cursore.                                            ?
032800160503       //--------------------------------------------------------------
032900160503       BEGSR  sr_OpenCursor;
033000160503
033100160503         // -?Apertura del cursore?
033200160503         exec sql   open C1;
033300160503
033400160503         if  SQLcode < *zero;
033500160503           exsr  sr_PrintErr;
033600160503         endif;
033700160503
033800160503       ENDSR;
033900160503
034000160503       //--------------------------------------------------------------
034100160503       //?Lettura cursore.                                             ?
034200160503       //--------------------------------------------------------------
034300160503       BEGSR  sr_ReadCursor;
034400160503
034500160512         clear  Fattura_ds;
034600160503
034700160512         // -?Lettura del cursore?
034800160509         exec sql   fetch next   from C1   into :Fattura_ds;
034900160503
035000160503         Select;
035100160503
035200160503           // -?Fine File?
035300160503           When  SQLcode = 100;
035400160503             $EoF = *on;
035500160516             if  Fattura_Save.Libro_Iva <> *zero;
035600160516               exsr  sr_Fine_LibroIva;
035700160516             endif;
035800160503             leavesr;
035900160503
036000160503           // -?Errore SQL?
036100160503           When  SQLcode < *zero;
036200160503             exsr  sr_PrintErr;
036300160503
036400160509           // -?Controllo singola Fattura?
036500160503           Other;
036600160513             // -?Confronto ultima Fattura reperita con quella precedente?
036700160509             exsr  sr_Ctrl_Fatt;
036800160513             // -?Memorizzazione ultima Fattura reperita?
036900160512             Fattura_Save = Fattura_ds;
037000160503
037100160503         EndSl;
037200160503
037300160503       ENDSR;
037400160503
037500160503       //--------------------------------------------------------------
037600160503       //?Chiusura cursore.                                            ?
037700160503       //--------------------------------------------------------------
037800160503       BEGSR  sr_CloseCursor;
037900160503
038000160503         // -?Chiusura del cursore?
038100160503         exec sql   close C1;
038200160503
038300160503       ENDSR;
038400160503
038500160503       //--------------------------------------------------------------
038600160509       //?Controllo singola Fattura reperita.                          ?
038700160503       //--------------------------------------------------------------
038800160509       BEGSR  sr_Ctrl_Fatt;
038900160503
039000160512         // -?Stessa Fattura in Entrambi i file: OK?
039100160512         if  Fattura_ds.Libro_Iva  = Fattura_Save.Libro_Iva   and
039200160512             Fattura_ds.Nr_Fattura = Fattura_Save.Nr_Fattura  and
039300160513             %subst( Fattura_ds.File   : 1 : 5 ) = 'TITAS'    and
039400160513             %subst( Fattura_Save.File : 1 : 5 ) = 'NDREG';
039500160512           leavesr;
039600160512         endif;
039700160512
039800160512         // -?Fattura successiva alla precedente (in file diverso): OK?
039900160512         if  Fattura_ds.Libro_Iva  = Fattura_Save.Libro_Iva       and
040000160512             Fattura_ds.Nr_Fattura = Fattura_Save.Nr_Fattura + 1  and
040100160513             %subst( Fattura_ds.File   : 1 : 5 ) = 'NDREG'    and
040200160513             %subst( Fattura_Save.File : 1 : 5 ) = 'TITAS';
040300160512           leavesr;
040400160512         endif;
040500160516
040600160516
040700160516         // -?"Rottura" Libro Iva / Filiale?
040800160516         IF  Fattura_Save.Libro_Iva <> Fattura_ds.Libro_Iva;
040900160516
041000160516           if  Fattura_Save.Libro_Iva <> *zero;
041100160516             exsr  sr_Fine_LibroIva;
041200160516           endif;
041300160516
041400160516           // -?1° errore: Fattura n° 1 mancante in entrambi i file?
041500160516           //  ?(SE Data Iniziale = 1° gennaio)?
041600160516           if  %subdt( wDate_Iso1 : *months ) = 1  and
041700160516               %subdt( wDate_Iso1 : *days )   = 1  and
041800160516               Fattura_ds.Nr_Fattura         <> 1;
041900160516             wErr = 1;
042000160516             exsr  sr_CampiInStampa;
042100160516             exsr  sr_PrintErr;
042200160516             //leavesr;
042300160516           endif;
042400160516
042500160516         ENDIF;
042600160516
042700160511
042800160516         // -?"Dettaglio"?
042900160516
043000160513         // -?2° errore: Fattura mancante in solo 1 dei 2 file: TITAS?
043100160516         if  Fattura_ds.Libro_Iva    =  Fattura_Save.Libro_Iva   and
043200160516             Fattura_ds.Nr_Fattura   <> Fattura_Save.Nr_Fattura  and
043300160516             Fattura_ds.File         =  Fattura_Save.File        and
043400160513             %subst( Fattura_Save.File : 1 : 5 ) = 'NDREG';
043500160513           wErr = 2;
043600160513           exsr  sr_CampiInStampa;
043700160513           exsr  sr_PrintErr;
043800160513           //leavesr;
043900160511         endif;
044000160512
044100160513         // -?3° errore: altra Fattura mancante in entrambi i file?
044200160516         if  Fattura_ds.Libro_Iva  =  Fattura_Save.Libro_Iva     and
044300160512             Fattura_ds.Nr_Fattura <> Fattura_Save.Nr_Fattura + 1;
044400160513           wErr = 3;
044500160512           exsr  sr_CampiInStampa;
044600160512           exsr  sr_PrintErr;
044700160512           //leavesr;
044800160512         endif;
044900160513
045000160513         // -?4° errore: Fattura mancante in solo 1 dei 2 file: NDREG?
045100160516         if  Fattura_ds.Libro_Iva    =  Fattura_Save.Libro_Iva   and
045200160516             Fattura_ds.Nr_Fattura   <> Fattura_Save.Nr_Fattura  and
045300160516             Fattura_ds.File         =  Fattura_Save.File        and
045400160513             %subst( Fattura_Save.File : 1 : 5 ) = 'TITAS';
045500160513           wErr = 4;
045600160513           exsr  sr_CampiInStampa;
045700160513           exsr  sr_PrintErr;
045800160513           //leavesr;
045900160513         endif;
046000160513
046100160513         // -?2°+4° errore: Fatture diverse mancanti in 2 file diversi:?
046200160513         //  ?· Fattura_Save.Nr_Fattura = 1 (solo su NDREG)?
046300160513         //  ?· Fattura_ds.Nr_Fattura   = 2 (solo su TITAS)?
046400160516         if  Fattura_ds.Libro_Iva    =  Fattura_Save.Libro_Iva   and
046500160516             Fattura_ds.Nr_Fattura   <> Fattura_Save.Nr_Fattura  and
046600160516             Fattura_ds.File         <> Fattura_Save.File        and
046700160516             %subst( Fattura_ds.File   : 1 : 5 ) = 'TITAS'       and
046800160513             %subst( Fattura_Save.File : 1 : 5 ) = 'NDREG';
046900160513           wErr = 2;
047000160513           exsr  sr_CampiInStampa;
047100160513           exsr  sr_PrintErr;
047200160513           wErr = 4;
047300160513           exsr  sr_CampiInStampa;
047400160513           exsr  sr_PrintErr;
047500160513           //leavesr;
047600160513         endif;
047700160503
047800160503       ENDSR;
047900160516
048000160516       //--------------------------------------------------------------
048100160516       //?Fine elaborazione del Libro Iva precedente.                  ?
048200160516       //--------------------------------------------------------------
048300160516       BEGSR  sr_Fine_LibroIva;
048400160516
048500160516         // -?5° errore: Fatt. precedente mancante in solo 1 dei file: TITAS?
048600160516         if  ( Fattura_ds.File =  Fattura_Save.File  or  $EoF )  and
048700160516             %subst( Fattura_Save.File : 1 : 5 ) = 'NDREG';
048800160516           wErr = 5;
048900160516           exsr  sr_CampiInStampa;
049000160516           exsr  sr_PrintErr;
049100160516           //leavesr;
049200160516         endif;
049300160516
049400160516         // -?6° errore: Fatt. precedente mancante in solo 1 dei file: NDREG?
049500160516         if  Fattura_ds.File   =  Fattura_Save.File              and
049600160516             %subst( Fattura_Save.File : 1 : 5 ) = 'TITAS';
049700160516           wErr = 6;
049800160516           exsr  sr_CampiInStampa;
049900160516           exsr  sr_PrintErr;
050000160516           //leavesr;
050100160516         endif;
050200160516
050300160516         // -?Stampa totali del Libro Iva precedente?
050400160516         if  ( PFTtotFM + PFTtotFS ) > *zero;
050500160516           exsr  sr_CampiTotale;
050600160516           write  VRFTtot;
050700160516           clear  VRFTtot;
050800160516         endif;
050900160516
051000160516         // -?Impostazione indicatore di Overflow per salto-pagina?
051100160517         //  ?(SE non è stata richiesta la stampa dei soli totali)?
051200160517         if  TNVRFT2ds.VRFTsst = *blank;
051300160517           *in01 = *on;
051400160517         endif;
051500160516
051600160516       ENDSR;
051700160512
051800160512       //--------------------------------------------------------------
051900160512       //?Impostazione dei campi di dettaglio in stampa.               ?
052000160512       //--------------------------------------------------------------
052100160512       BEGSR  sr_CampiInStampa;
052200160512
052300160512         clear  VRFTdet;
052400160512
052500160516         // -?Libro Iva / Filiale?
052600160516         if  ( wErr <> 5  and  wErr <> 6 )  or
052700160512             Fattura_ds.Libro_Iva  = Fattura_Save.Libro_Iva;
052800160512           PFTlbi  = Fattura_ds.Libro_Iva;
052900160512         else;
053000160512           PFTlbi  = Fattura_Save.Libro_Iva;
053100160512         endif;
053200160512
053300160512         // -?Numero iniziale Fattura mancante?
053400160512         Select;
053500160512           // -?1° errore: Fattura n° 1 mancante in entrambi i file?
053600160512           When  wErr = 1;
053700160512             PFTnft1 = 1;
053800160513           // -?2° errore: Fattura mancante su TITAS?
053900160512           When  wErr = 2;
054000160516             PFTnft1 = Fattura_Save.Nr_Fattura;
054100160512           // -?3° errore: altra Fattura mancante in entrambi i file?
054200160512           When  wErr = 3;
054300160512             PFTnft1 = Fattura_Save.Nr_Fattura + 1;
054400160513           // -?4° errore: Fattura mancante su NDREG?
054500160513           When  wErr = 4;
054600160516             PFTnft1 = Fattura_ds.Nr_Fattura;
054700160516           // -?5° errore: ultima Fattura mancante su TITAS?
054800160516           When  wErr = 5;
054900160516             PFTnft1 = Fattura_Save.Nr_Fattura;
055000160516           // -?6° errore: ultima Fattura mancante su NDREG?
055100160516           When  wErr = 6;
055200160516             PFTnft1 = Fattura_Save.Nr_Fattura;
055300160512         EndSl;
055400160512
055500160512         // -?Numero finale   Fattura mancante?
055600160516         if  ( wErr = 1   or  wErr = 3 )   and
055700160512             Fattura_ds.Nr_Fattura > PFTnft1 + 1;
055800160512           PFTnftX = '-';
055900160512           PFTnft2 = Fattura_ds.Nr_Fattura - 1;
056000160512         endif;
056100160512
056200160512         // -?Archivio in cui mancano le Fatture?
056300160512         select;
056400160516           when  wErr = 2  or  wErr = 5;
056500160513             PFTfile = 'TITAS     ';
056600160516           when  wErr = 4  or  wErr = 6;
056700160513             PFTfile = 'NDREG00F  ';
056800160513           other;
056900160512             PFTfile = 'Tutti';
057000160512         endsl;
057100160513
057200160513         // -?Aggiornamento TOTALI?
057300160513         if  PFTnft2 > *zero;
057400160513           PFTtotFM += ( PFTnft2 - PFTnft1 + 1);
057500160513         else;
057600160513           PFTtotFM += 1;
057700160513         endif;
057800160516         if  wErr = 4  or  wErr = 6;
057900160513           PFTtotFS += 1;
058000160513         endif;
058100160512
058200160512       ENDSR;
058300160513
058400160513       //--------------------------------------------------------------
058500160513       //?Impostazione dei campi di TOTALE in stampa.                  ?
058600160513       //--------------------------------------------------------------
058700160513       BEGSR  sr_CampiTotale;
058800160513
058900160513         // -?Libro Iva?
059000160513         PFTlbi    = Fattura_Save.Libro_Iva;
059100160513
059200160513         // -?Totale Fatture?
059300160513         PFTtotNF  = Fattura_Save.Nr_Fattura;
059400160513
059500160513         // -?% Fatture mancanti in toto?
059600160513         eval(H) PFTtotPM  = ( PFTtotFM * 100 ) / PFTtotNF;
059700160513
059800160513         // -?% Fatture presenti solo su Setras?
059900160513         eval(H) PFTtotPS  = ( PFTtotFS * 100 ) / PFTtotNF;
060000160513
060100160513       ENDSR;
060200160503
060300160503       //--------------------------------------------------------------
060400160512       //?Stampa Fattura MANCANTE (o errore SQL).                      ?
060500160503       //--------------------------------------------------------------
060600160503       BEGSR  sr_PrintErr;
060700160503
060800160509         // -?Test OverFlow?
060900160512         if  *in01;
061000160512           write  VRFTseg;
061100160509           write  VRFTtxt;
061200160517           if  TNVRFT2ds.VRFTsst = *blank;
061300160517             write  VRFTtxt2;
061400160517           endif;
061500160512           *in01  = *off;
061600160509         endif;
061700160503
061800160517         Select;
061900160517
062000160517           // -?Stampa Errore SQL?
062100160517           When  wErr = *zero;
062200160512
062300160517             // -?Segnalazione errore nello spool?
062400160517             pSQLcode  = SQLcode;
062500160517             pSQLstate = SQLstate;
062600160517             write  VRFTerr;
062700160517             // -?Dump?
062800160517             Dump(A);
062900160517             // -?Job-Log?
063000160517             Qcmd = 'DSPJOBLOG job(*) output(*print)';
063100160517             exsr  sr_ExecCmd;
063200160512
063300160517           // -?Stampa Dettaglio Fattura mancante?
063400160517           When  TNVRFT2ds.VRFTsst = *blank;
063500160517             write  VRFTdet;
063600160512
063700160517         EndSl;
063800160517
063900160517         clear  wErr;
064000160503
064100160503       ENDSR;
064200160503
064300160503       //--------------------------------------------------------------
064400160503       //?Esecuzione del comando (già impostato).                      ?
064500160503       //--------------------------------------------------------------
064600160503       BEGSR  sr_ExecCmd;
064700160503
064800160503         clear Qcap0100;
064900160503         Qcabcsdh = *off;
065000160503         Qcapa    = *off;
065100160503         Qcacmdss = *off;
065200160503         Qcaerved = *allX'00';
065300160503
065400160503         clear Qusec;
065500160503         Qusbprv  = %size(Qusec);
065600160503
065700160503         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
065800160503                           %size(Qcap0100) : 'CPOP0100' : *omit :
065900160503                           0 : 0 : Qusec);
066000160503
066100160503         //if  Qusei <> *blank;
066200160503         //  ...;
066300160503         //endif;
066400160503
066500160503       ENDSR;
066600160503
066700160503       //--------------------------------------------------------------
066800160503       //?Operazioni finali.                                           ?
066900160503       //--------------------------------------------------------------
067000160503       BEGSR  sr_RoutEnd;
067100160512
067200160512         // -?Stampa Fine Lista?
067300160512         write  VRFTend;
067400160503
067500160512         // -?Uscita?
067600160503         return;
067700160503
067800160503       ENDSR;
067900160503
068000160503      /end-free
068100160503
068200160503       //--------------------------------------------------------------
068300160503       //?Definizione schiere a tempo di compilazione                  ?
068400160503       //--------------------------------------------------------------
068500160503
068600160511** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
068700160512Rilevato errore: consultare la stampa ed avvisare il CED                       1
