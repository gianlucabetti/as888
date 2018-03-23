000100131113      *---------------------------------------------------------------*
000200131113      *?- - - - - - - - - - - -   P R O V A   - - - - - - - - - - - -?*
000300131113      *?                S T A M P A   S U   L A S E R                ?*
000400131113      *?         O R D I N E   D I   R I T I R O   M E R C E         ?*
000500131113      *---------------------------------------------------------------*
000600131113
000700131113       //--------------------------------------------------------------
000800131113       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900131113       //--------------------------------------------------------------
001000131113
001100131113     /*END
001200131113
001300131113       //--------------------------------------------------------------
001400131113       //?Specifiche di controllo.                                     ?
001500131113       //--------------------------------------------------------------
001600131113
001700131113     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800131113     h dftactgrp(*no)
001900131113     h bnddir('TRUL')
002000131113
002100131113       //--------------------------------------------------------------
002200131113       //?Dichiarazione file.                                          ?
002300131113       //--------------------------------------------------------------
002400131113
002500131113     fAZORG01L  if   e           k disk
002600131113      *
002700131113     fFIOR18P   o    e             printer usropn
002800131113
002900131113       //--------------------------------------------------------------
003000131113       //?Definizione costanti.                                        ?
003100131113       //--------------------------------------------------------------
003200131113
003300131113       // -?Numero dei Box per Pagina "A4"?
003400131113     d $BxP            c                   const(4)
003500131113
003600131113       // -?Spessore delle linee in inches/pollici (*memo)?
003700131113     d c_LineNarrow    c                   const(0.008)
003800131113     d c_LineMedium    c                   const(0.017)
003900131113     d c_LineWide      c                   const(0.025)
004000131113
004100131113       // -?Dimensioni base per il simil-logo BRT?
004200131113      /copy gaitrasrc/srcTNSY,LogoBRT_R1
004300131113
004400131113       // -?Posizioni "base" per:?
004500131113       //  ?- logo Brt in Pagina "A4"?
004600131113       //  ?· posizione superiore?
004700131113     d $PS0            c                   const(0.260)
004800131113       //  ?· posizione centrale?
004900131113     d $PS0_c          c                   const(0.350)
005000131113       //  ?· posizione inferiore?
005100131113     d $PS0_i          c                   const(0.440)
005200131113       //  ?- barcode?
005300131113     d $PS1            c                   const(2.350)
005400131113       //  ?- linee orizzontali (alta e bassa) del singolo box?
005500131113     d $LOA            c                   const(0.200)
005600131113     d $LOB            c                   const(2.350)
005700131113     d $LOC            c                   const(2.342)
005800131113     d $LOD            c                   const(2.825)
005900131113       //  ?- varie linee orizzontali interne al box?
006000131113     d $LO1            c                   const(0.525)
006100131113     d $LO2            c                   const(0.850)
006200131113     d $LO3            c                   const(1.500)
006300131113     d $LO4            c                   const(1.840)
006400131113
006500131113       // -?Intestazioni in stampa?
006600131113     d $BRT            c                   const('BRT S.p.A.')
006700131113
006800131113       //--------------------------------------------------------------
006900131113       //?Definizione schiere.                                         ?
007000131113       //--------------------------------------------------------------
007100131113
007200131114     d $Dati           s             50    dim(26) ctdata perrcd(1)
007300131114     d $Note           s             35    dim( 6) ctdata perrcd(1)
007400131113      * -?Quantità (editate) - a sx - e Unità di misura - a dx -?
007500131113     d $QUM            s             20    dim(3)  inz
007600131114
007700131114       //--------------------------------------------------------------
007800131114       //?Definizione aree dati.                                       ?
007900131114       //--------------------------------------------------------------
008000131114
008100131114       // -?Dati utente?
008200131114     d §AzUte        e ds                  extname(AZUTE00F)
008300131114     d                                     dtaara
008400131114     d §DatiUte      e ds                  extname(dDatiUte)
008500131114     d                                     dtaara
008600131113
008700131113       //--------------------------------------------------------------
008800131113       //?Definizione strutture dati.                                  ?
008900131113       //--------------------------------------------------------------
009000131113
009100131113       // -?Parametri ricevuti?
009200131113     d KPJBA         e ds
009300131113     d FIOR16DS      e ds                  inz
009400131113
009500131113       // -?Status ds?
009600131113     d Status         sds           333
009700131113     d  SDSpgm           *proc
009800131113     d  SDSprm           *parms
009900131113     d  SDSdta               191    198
010000131113     d  SDSjob               244    253
010100131113     d  SDSusr               254    263
010200131113
010300131113       // -?Codice a Barre della LDV?
010400131113     d BARcodeDS       ds            14    inz
010500131114     d   DSkeyOSR              1      5s 0 inz
010600131114     d     Ppoe                1      3s 0 inz
010700131114     d     Pnsr                4      5s 0 inz
010800131114     d   Pnor                  6     12s 0 inz
010900131114     d   Pnrv                 13     14s 0 inz
011000131113
011100131113       //--------------------------------------------------------------
011200131113       //?Definizione variabili globali.                               ?
011300131113       //--------------------------------------------------------------
011400131114
011500131114       // -?Flags Booleani?
011600131114     d $RimMitt        s               n   inz
011700131114     d $NoteAgg        s               n   inz
011800131114     d $PrtNote        s               n   inz
011900131114
012000131114       // -?Indici di schiera / Contatori?
012100131114     d xx              s              3  0 inz
012200131114     d xy              s              3  0 inz
012300131114     d yy              s              3  0 inz
012400131114     d wNrCopie        s              5  0 inz(1)
012500131113
012600131113       // -?Parametri per pgm. "QCMDEXC"?
012700131114     d*// Qcmd            s            156    inz
012800131114     d*// Qlen            s             15  5 inz(156)
012900131113
013000131113       // -?Contatori: da stampare, in fase di stampa, posiz. in "A4"?
013100131113     d wPage           s              5  0 inz
013200131113     d wBoxP           s              1  0 inz
013300131113       // -?Posizione iniziale nel calcolo delle posizioni per box?
013400131113     d wPosIni_B       s              5  3 inz
013500131113
013600131113       //--------------------------------------------------------------
013700131113       //?Definizione procedure usate.
013800131113       //--------------------------------------------------------------
013900131113
014000131114       // -?Reperimento dati utente?
014100131114     d TIBS34ds      e ds                  inz
014200131114      /copy gaitrasrc/srcProtoPR,TIBS34R
014300131114
014400131113       //--------------------------------------------------------------
014500131113       //?Definizione key-list.
014600131113       //--------------------------------------------------------------
014700131113
014800131114       //==============================================================
014900131114       //? RIEPILOGO INDICATORI                                      ?
015000131114       //--------------------------------------------------------------
015100131114       //?04   ?- ORM ESTERO: Stampa Euroexpress
015200131114       //?05   ?- ORM ESTERO: Stampa DPD
015300131114       //?06   ?- ORM ESTERO: SOLLECITO
015400131114       //?10   ?- ORM ESTERO VIA MAIL
015500131114       //?11   ?- ORM ESTERO VIA FILE
015600131114       //?20   ?- Condiziona "grassetto" in stampa su FIOR16P2
015700131114       //?21   ?- Comodo
015800131114       //?41   ?- Posizionamento nel 1° ¼ del foglio A4
015900131114       //?42   ?- Posizionamento nel 2° ¼ del foglio A4
016000131114       //?43   ?- Posizionamento nel 3° ¼ del foglio A4
016100131114       //?44   ?- Posizionamento nel 4° ¼ del foglio A4
016200131114       //?50   ?- Stampa BarCode
016300131114       //?60   ?- *Date >= §OGDDAO    (nuova stampa "aut + d:xxxxx")
016400131114       //?61   ?- Ora ritiro <= 12:30 (stampata in grassetto)
016500131114       //?62   ?- Stampa dati ORM per Ritiro Contestuale in grassetto
016600131114       //?70   ?- Stampa Rimesso Mittente: 1ª fase (stampa O.R.M.)
016700131114       //?71   ?- Stampa Rimesso Mittente: 2ª fase (dopo O.R.M.)
016800131114       //?90   ?- Errore QCmdExc
016900131114       //==============================================================
017000131113
017100131114       //--------------------------------------------------------------
017200131114       //?M A I N - L I N E                                            ?
017300131114       //--------------------------------------------------------------
017400131114
017500131113     c     *Entry        plist
017600131114       // -?£RimMitt - Stampa anche Rimesso Mittente?
017700131114     c                   parm                    £RimMitt          1
017800131114       // -?£NoteAgg - Stampa anche Note Aggiuntive (SE NON Rimesso Mittente)?
017900131114     c                   parm                    £NoteAgg          1
018000131114       // -?£NrCopie - num. copie da stampare?
018100131114     c                   parm                    £NrCopie         15 5
018200131113
018300131114      /free
018400131114
018500131114       // -?Operazioni iniziali?
018600131114       exsr  sr_RoutInz;
018700131114
018800131114       // -?Pulizia del PRTF?
018900131114       exsr  sr_ClrPRTF;
019000131114
019100131114       // -?Stampa del singolo O.R.M.?
019200131114       For  xx = 1  To  wNrCopie;
019300131114         exsr  sr_StampaORM;
019400131114       EndFor;
019500131114
019600131114       // -?Operazioni finali?
019700131114       exsr  sr_RoutEnd;
019800131114
019900131114       //--------------------------------------------------------------
020000131114       //?Operazioni iniziali.                                         ?
020100131114       //--------------------------------------------------------------
020200131114       BEGSR  sr_RoutInz;
020300131113
020400131114         *inLR  = *on;
020500131114
020600131114         // -?Verifica se richiesta la stampa di un Rimesso Mittente?
020700131114         if  %parms() > *zero  and  %addr(£RimMitt) <> *null;
020800131114           $RimMitt = (£RimMitt <> *blank  and  £RimMitt <> 'N');
020900131114         endif;
021000131114
021100131114         // -?Verifica se richiesta la stampa di Note Aggiuntive?
021200131114         //  ?(in alternativa al Rimesso Mittente)?
021300131114         if  %parms() > 1  and  %addr(£NoteAgg) <> *null;
021400131114           $NoteAgg = (£NoteAgg <> *blank  and  £NoteAgg <> 'N');
021500131114         endif;
021600131114
021700131114         // -?Verifica il n° di copie richiesto?
021800131114         if  %parms() > 2  and  %addr(£NrCopie) <> *null;
021900131114           if  £NrCopie > *zero  and  £NrCopie <= 99999;
022000131114             wNrCopie = £NrCopie;
022100131114           endif;
022200131114         endif;
022300131114
022400131114         // -?Apertura *PrtF?
022500131114         open  FIOR18P;
022600131114
022700131114         // -?Reperimento dati job?
022800131114         in(E) §AzUte;
022900131114         if NOT %error;
023000131114           in(E) §DatiUte;
023100131114         endif;
023200131114         if %error or RSut = *blanks;
023300131114           clear TIBS34ds;
023400131114           tibs34r ( tibs34ds );
023500131114           in §AzUte;
023600131114           in §DatiUte;
023700131114         endif;
023800131114
023900131114       ENDSR;
024000131114
024100131114       //--------------------------------------------------------------
024200131114       //?Pulizia dei campi di stampa.                                 ?
024300131114       //--------------------------------------------------------------
024400131114       BEGSR  sr_ClrPRTF;
024500131114
024600131114         clear  OR18logBRT;
024700131114         clear  OR18box;
024800131114         clear  OR18pos;
024900131114         clear  OR18orm;
025000131114         clear  OR18not;
025100131114         //clear  OR18nul;     (NON ha campi)
025200131114
025300131114         *in = *off;
025400131114
025500131114       ENDSR;
025600131113
025700131114       //--------------------------------------------------------------
025800131114       //?Stampa singolo O.R.M.                                        ?
025900131114       //--------------------------------------------------------------
026000131114       BEGSR  sr_StampaORM;
026100131114
026200131114         // -?*in60 = Stampa Distinte ORM automatiche "attiva"?
026300131114         *in60 = *on;
026400131114
026500131114         // -?*in70 = O.R.M. con Rimesso Mittente?
026600131114         *in70 = $RimMitt;
026700131114
026800131114         // -?*in71 = Stampa Ricevuta di ritiro (non adesso: DOPO!)?
026900131114         *in71 = *off;
027000131114
027100131114         // -?Impostazione dei campi in stampa:?
027200131114
027300131114         // -?Numero O.R.M.?
027400131114         Ppoe = 123;
027500131114         Pnsr = 12;
027600131114         Pnor = 7654321;
027700131114         Pnrv = 12;
027800131114         if  *in70;
027900131114           Ppoe2 = Ppoe;
028000131114           Pnsr2 = Pnsr;
028100131114           Pnor2 = Pnor;
028200131114           Pnrv2 = Pnrv;
028300131114         endif;
028400131114
028500131114         // -?Data emissione O.R.M.?
028600131114         Pdao = *date;
028700131114
028800131114         // -?Ora emissione O.R.M. ("hh:mm", senza ":ss")?
028900131114         Poao = %int( %subst( %char( %dec( %timestamp() ) )
029000131114                              : 9 : 4 ) );
029100131114         // -?Numero distinta consegna?
029200131114         Pndc = 654321;
029300131114
029400131114         // -?Padroncino: codice e nominativo?
029500131114         if  *in60;
029600131114           Ppdr = '7654321'
029700131114                +  ' '
029800131114                +  $Dati(01);
029900131114         else;
030000131114           Ppdr = %subst( $Dati(01) : 1 : %len( Ppdr ) - 5 );
030100131114         endif;
030200131114
030300131114         // -?Zona ritiro?
030400131114         if  NOT  *in60;
030500131114           Pzor = 'ZO';
030600131114         endif;
030700131114
030800131114         // -?Fil. ritiro?
030900131114         Ppor = DUTpou;
031000131114         Ppod = DUTdpo;
031100131114
031200131114         // -?Data ritiro?
031300131114         Pdar = *date;
031400131114
031500131114         // -?Ora ritiro?
031600131114         Porr = %int( ( %subdt( %timestamp() + %hours(2) : *H ) * 100 ) +
031700131114                      ( %subdt( %timestamp() + %hours(2) : *MN ) ) );
031800131114         // - -?Ora ritiro <= 12:30 (stampata in "grassetto")?
031900131114         *in61 = (Porr <= 1230);
032000131114
032100131114         // -?Priorità (lungo 1/0)?
032200131114         Psto = 9;
032300131114
032400131114         // -?O.R.M. commissionato?
032500131114         Pcom = 'C';
032600131114
032700131114         // -?Mittente?
032800131114         Prsr = $Dati(02);
032900131114         Pinr = $Dati(03);
033000131114         Pcar = $Dati(04);
033100131114         Plor = $Dati(05);
033200131114         Pprr = $Dati(06);
033300131114         Pnar = $Dati(07);
033400131114
033500131114         // -?Codice ritiro?
033600131114         Pcra = 1237654123;
033700131114
033800131114         // -?Referente?
033900131114         //if  Pnsr = *zero;
034000131114           Pter = '123/567890123456';
034100131114         //endif;
034200131114         Prer = $Dati(08);
034300131114         *in62  =  (Pnsr <> *zero);
034400131114
034500131114         // -?Indicazione "Sponda Idraulica"?
034600131114         Pspi = 'Sponda Idraulica';
034700131114
034800131114         // -?Quantità e Unità di Misura?
034900131114         exsr  sr_QUM;
035000131114         Pqum = %trim($Qum(1)) + '   '
035100131114              + %trim($Qum(2)) + '   '
035200131114              + %trim($Qum(3));
035300131114
035400131114         // -?Natura della merce?
035500131114         Pnam = $Dati(09);
035600131114
035700131114         // -?"Porto assegnato" o "Prepagato"?
035800131114         Ptor = 'Prepagato ';
035900131114         //Ptor = 'Assegnato ';
036000131114
036100131114         // -?Assegnazione al padroncino?
036200131114         Ptap = 'W';
036300131114
036400131114         // -?Tipo comunicazione O.R.M.?
036500131114         Ptco = 'X';
036600131114
036700131114         // -?Riferimento (NO se call center)?
036800131114         //if  Ptco <> 'C';
036900131114           Prfa = $Dati(10);
037000131114         //endif;
037100131114
037200131114         // -?Riferimento spedizione (se O.R.M. prepagato)?
037300131115         Par4 = 'Spedizione: ' + %editc(*year : 'Z') + '_'
037400131115                               + %editc(DUTpou : 'X') + '_'
037500131115                               + %editc(12 : 'Z') + '_'
037600131114                               + %editc(7654321 : 'Z');
037700131129         Pimp = 'Tot. Fatt.:'
037800131129              +  %triml( %editw( 1234567,89 : '       . 0 ,  -') ) + 'EUR';
037900131114
038000131114         // -?Ordinante?
038100131114         Pkor = 'ORDINANTE';
038200131114         Prso = $Dati(11);
038300131115         Pclo = %trim($Dati(12)) + '_' + %trim($Dati(13)) + '_';
038400131115         %subst( Pclo : %len(Pclo) - 2 ) = '_' + %trim($Dati(14) );
038500131115         //Pclo = %trim( $Dati(12) ) + '_'
038600131115         //     + %trim( %subst( $Dati(13) : 1 : 32 ) ) + '_'
038700131115         //     + %trim( $Dati(14) ) + '_'
038800131114         //     + %trim( $Dati(15) );
038900131114
039000131114         // -?Destinatario?
039100131114         Pkde = 'DESTINATARIO';
039200131114         Prsc = $Dati(16);
039300131115         Pclc = %trim($Dati(17)) + '_' + %trim($Dati(18)) + '_';
039400131115         %subst( Pclc : %len(Pclc) - 2 ) = '_' + %trim($Dati(19) );
039500131115         //Pclc = %trim( $Dati(17) ) + '_'
039600131115         //     + %trim( %subst( $Dati(18) : 1 : 32 ) ) + '_'
039700131115         //     + %trim( $Dati(19) ) + '_'
039800131114         //     + %trim( $Dati(20) );
039900131114
040000131114         // -?Note?
040100131114         Pno1 = $Note(1);
040200131114         Pno2 = $Note(2);
040300131129
040400131129         // -?Orari Apertura?
040500131129         Poa1 = ' 8:30  12:30 - 15:30  19:30';
040600131129         clear  Poa2;
040700131114
040800131114         // -?Note aggiuntive?
040900131114         // - -?disposizione tra ordinante (Prso) e destinatario (Prsc)?
041000131114         //    ?e note (Pno1/Pno2)?
041100131114         //select;
041200131114         //  when  Prso  = *blank  and  Prsc <> *blank;
041300131114         //    Pkor = 'NOTE';
041400131114         //    Prso = $Note(1);
041500131114         //    Pclo = $Note(2);
041600131114         //  when  Prso <> *blank  and  Prsc  = *blank;
041700131114         //    Pkde = 'NOTE';
041800131114         //    Prsc = $Note(1);
041900131114         //    Pclc = $Note(2);
042000131114         //  when  Prso  = *blank  and  Prsc  = *blank;
042100131114         //    Pkor = 'NOTE';
042200131114         //    Prso = $Note(1);
042300131114         //    Pclo = $Note(2);
042400131114         //    Pkde = 'NOTE';
042500131114         //    Prsc = $Note(3);
042600131114         //    Pclc = $Note(4);
042700131114         //endsl;
042800131114
042900131114         // -?Codice a barre: POE+NSR+NOR+NRV?
043000131114         Tbcd = BarCodeDS;
043100131114
043200131114         // - -?Verifico se codice a barre da stampare?
043300131114         *in50 = (Tbcd <> *zero);
043400131129
043500131129         // - -?Verifico se orari apertura da stampare?
043600131129         *in61 = (Poa1 <> *zero);
043700131114
043800131114
043900131114
044000131114         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
044100131114         exsr  sr_Posiz_Box;
044200131114
044300131114         // -?Stampa O.R.M.?
044400131114         WRITE  OR18orm;
044500131114
044600131114
044700131114
044800131114         Select;
044900131114
045000131114           // -Se?O.R.M.?con?Rimesso Mittente:?
045100131114           //  ?Stampa del Rimesso Mittente in pagina successiva?
045200131114           When  $RimMitt;
045300131114             exsr  sr_Prt_RimMitt;
045400131114
045500131114           // -Se?O.R.M.?senza?Rimesso Mittentema?con?Note Aggiuntive:?
045600131114           //  ?Stampa di eventuali Note Aggiuntive in pagina successiva?
045700131114           When  $NoteAgg;
045800131114             exsr  sr_Prt_Note;
045900131114
046000131114         EndSl;
046100131114
046200131114       ENDSR;
046300131114
046400131114       //--------------------------------------------------------------
046500131114       //?Impostazione Quantità ed Unità di Misura.                    ?
046600131114       //--------------------------------------------------------------
046700131114
046800131114       BEGSR  sr_QUM;
046900131114
047000131114         //clear  $Qum;       ?(già fatto)?
047100131114         clear  YY;
047200131114
047300131114         // -?Colli?
047400131114         if  YY < %elem($Qum);
047500131114           yy += 1;
047600131115           $Qum(yy) = %editc(12345 : '4') + '_Cl';
047700131114         endif;
047800131114
047900131114         // -?Peso?
048000131114         if  YY < %elem($Qum);
048100131114           yy += 1;
048200131115           $Qum(yy) = %editc(123456,7 : '4') + '_Kg';
048300131114         endif;
048400131114
048500131114         // -?Volume?
048600131114         //  ?(in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)?
048700131114         if  YY < %elem($Qum);
048800131114           yy += 1;
048900131115           $Qum(yy) = %editc(12,345 : '4') + '_Vl';
049000131114         endif;
049100131114
049200131114         // -?Bancali?
049300131114         //  ?(in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)?
049400131114         //  ?(in ultimo posto da preferire al VOLUME - come da FIOR09R)?
049500131114         if  YY < %elem($Qum);
049600131114           yy += 1;
049700131115           $Qum(yy) = %editc(12345 : '4') + '_Bn';
049800131114         endif;
049900131114
050000131114         // -?Bilici?
050100131114         if  YY < %elem($Qum);
050200131114           yy += 1;
050300131115           $Qum(yy) = %editc(12345 : '4') + '_Bl';
050400131114         endif;
050500131114
050600131114         // -?Motrici?
050700131114         if  YY < %elem($Qum);
050800131114           yy += 1;
050900131115           $Qum(yy) = %editc(12345 : '4') + '_Mt';
051000131114         endif;
051100131114
051200131114         // -?Autotreni?
051300131114         if  YY < %elem($Qum);
051400131114           yy += 1;
051500131115           $Qum(yy) = %editc(12345 : '4') + '_Au';
051600131114         endif;
051700131114
051800131114       ENDSR;
051900131113
052000131114       //--------------------------------------------------------------
052100131114       //?Stampa delle note aggiuntive in pagina successiva            ?
052200131114       //--------------------------------------------------------------
052300131114       BEGSR  sr_Prt_Note;
052400131114
052500131114         $PrtNote = *on;
052600131114
052700131114         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
052800131114         exsr  sr_Posiz_Box;
052900131114
053000131114         // -?Intestazione?
053100131114         write  OR18nul;
053200131114         Pnot = '            ' + '(Seguito)     '
053300131114              + 'O.R.M. numero '
053400131114                                 + %editw(Ppoe : '0   ') + ' '
053500131114                                 + %editc(Pnsr : 'Z')    + ' '
053600131114                                 + %editc(Pnor : 'Z')    + ' '
053700131114                                 + %editc(Pnrv : 'Z');
053800131114         WRITE  OR18not;
053900131114         write  OR18nul;
054000131114         Pnot = '            ' + 'NOTE:';
054100131114         WRITE  OR18not;
054200131114         write  OR18nul;
054300131114         xy = 3;
054400131114         DoW  XY <= %elem($Note);
054500131114           Pnot = $Note(xy) + ' ' + $Note(xy+1);
054600131114           xy += 2;
054700131114           WRITE  OR18not;
054800131114           // - -?Avendo massimo 6 righe di note: posso lasciare una riga?
054900131114           //    ?vuota tra una nota e l'altra?
055000131114           if  %elem($Note) <= 6;
055100131114             write  OR18nul;
055200131114           endif;
055300131114         EndDo;
055400131114
055500131114         reset  $PrtNote;
055600131114
055700131114       ENDSR;
055800131114
055900131114       //--------------------------------------------------------------
056000131114       //?Stampa del Rimesso Rittente.                                 ?
056100131114       //--------------------------------------------------------------
056200131114       BEGSR  sr_Prt_RimMitt;
056300131114
056400131114         //*in70 = *on;       ?(già così)?
056500131114         *in71 = *on;
056600131114
056700131114         // -?Tolgo impostazione di stampa del codice a barre?
056800131114         *in50 = *off;
056900131113
057000131113         // -?Impostazione "BRT" per ritiro della merce?
057100131113         Pbrt  = $Brt;
057200131113
057300131114         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
057400131114         exsr  sr_Posiz_Box;
057500131114
057600131114         // -?RIMESSO MITTENTE?
057700131114         WRITE  OR18orm;
057800131114
057900131114       ENDSR;
058000131113
058100131114       //--------------------------------------------------------------
058200131114       //?Posizionamento del box all'interno della pag.A4              ?
058300131114       //--------------------------------------------------------------
058400131114       BEGSR  sr_Posiz_BOX;
058500131114
058600131114         wPage += 1;
058700131114
058800131114         // -?Calcolo la posizione x box all'interno della pagina "A4"?
058900131114         wBoxP =  %rem( wPage : $BxP );
059000131114
059100131114         // - -?Se 4° ORM nella pagina, ma ORM con Rimesso Mittente:?
059200131114         //    ?devo saltare alla pag. A4 successiva: l'ORM in esame?
059300131114         //    ?dev'essere SEMPRE seguito dal rimesso mittente corri-?
059400131114         //    ?spondente (senza note aggiuntive).?
059500131114         IF  wBoxP = *zero;
059600131114           Select;
059700131114             When  Not  $RimMitt  or  ($RimMitt  and  *in71);
059800131114               wBoxP = $BxP;
059900131114             When  $RimMitt  and  Not  *in71;
060000131114               wPage += 1;
060100131114               wBoxP  = 1;
060200131114           EndSl;
060300131114         ENDIF;
060400131114
060500131114         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
060600131114         wPosIni_B = (2.835 * (wBoxP - 1));
060700131113         if  wBoxP > 2;
060800131113           wPosIni_B += 0.167;
060900131113         endif;
061000131113
061100131113         // -?Simil-Logo BRT?
061200131113         §PlblS  = wPosIni_B + $PS0 - 0.010;
061300131113         §PlblVS = 7.541;
061400131113         exsr  sr_SimilLogoBRT_;
061500131113
061600131114         // -?Posizionamento di vari campi e linee?
061700131114         §PS1 = wPosIni_B + $PS1;
061800131114         §LOA = wPosIni_B + $LOA;
061900131114         If  Not  $RimMitt  or  ($RimMitt  and  Not *in71);
062000131114           §LOB = wPosIni_B + $LOB;
062100131114           §LOC = wPosIni_B + $LOC;
062200131114           §LOD = wPosIni_B + $LOD;
062300131114         Else;
062400131114           §LOB = wPosIni_B + $LOD;
062500131114         EndIf;
062600131114         §LO1 = wPosIni_B + $LO1;
062700131114         §LO2 = wPosIni_B + $LO2;
062800131114         §LO3 = wPosIni_B + $LO3;
062900131114         §LO4 = wPosIni_B + $LO4;
063000131129if  1    if  *in61;
063100131129           §LO5 = wPosIni_B + $LOB;
063200131129e   1    endif;
063300131114         If  $RimMitt;
063400131114           §LO5 = wPosIni_B + $LOB;
063500131114           if  NOT *in71;
063600131114             §LO6 = §LOD - 0,10;
063700131114           else;
063800131114             §LO6 = §LOB - 0,10;
063900131114           endif;
064000131114         EndIf;
064100131114
064200131114         // -?Posizionam. nella pagina A4 in base al n° foglio in stampa?
064300131114         *in41 = (wBoxP = 1);
064400131114         *in42 = (wBoxP = 2);
064500131114         *in43 = (wBoxP = 3);
064600131114         *in44 = (wBoxP = 4);
064700131114         WRITE  OR18pos;
064800131114
064900131114         // -?box / barcode?
065000131114         if  $PrtNote = *off;
065100131114           WRITE  OR18box;
065200131114         endif;
065300131114
065400131114         // -?logo BRT?
065500131114         WRITE  OR18logBRT;
065600131114
065700131114       ENDSR;
065800131113
065900131113       //--------------------------------------------------------------
066000131113       //?Posizionamento del simil-logo BRT (unico)                    ?
066100131113       //--------------------------------------------------------------
066200131113      /copy gaitrasrc/srcTNSY,LogoBRT_R2
066300131114
066400131114       //--------------------------------------------------------------
066500131114       //?Operazioni finali.                                           ?
066600131114       //--------------------------------------------------------------
066700131114       BEGSR  sr_RoutEnd;
066800131114
066900131114         // -?Chiusura *PrtF?
067000131114         close  FIOR18P;
067100131114
067200131114         // -?Chiusura pgm?
067300131114         return;
067400131114
067500131114       ENDSR;
067600131114
067700131114      /end-free
067800131114
067900131114       //--------------------------------------------------------------
068000131114       //?Definizione schiere a tempo di compilazione                  ?
068100131114       //--------------------------------------------------------------
068200131113
068300131114** -?$DATI?- .+....2....+....3....+....4....+....5
068400131115DENOMIN._AUT/COOP_USO_GESTIONALE_.+....4..          1
068500131115RAGIONE_SOCIALE_RITIRO_.+....3....+....4..          2
068600131115INDIRIZZO_RITIRO_..2....+....3....+....4..          3
068700131114CAP-RITIRO                                          4
068800131115LOCALITA'_RITIRO_..2....+....3....+....4..          5
068900131114PR                                                  6
069000131114NAR                                                 7
069100131115REFERENTE_RITIRO_..2....+....3....+....4..          8
069200131115NATURA_DELLA_MERCE_2....+....3....+....4..          9
069300131115RIFERIM.ALFANUMERICO                               10
069400131115RAGIONE_SOCIALE_ORDINANTE_...3....+....4..         11
069500131114CAPor                                              12
069600131115LOCALITA'_ORDINANTE_....+....3....+....4..         13
069700131114PO                                                 14
069800131114NZO                                                15
069900131115RAGIONE_SOCIALE_DESTINATARIO_3....+....4..         16
070000131114CAPde                                              17
070100131115LOCALITA'_DESTINATARIO_.+....3....+....4..         18
070200131114PD                                                 19
070300131114NZD                                                20
070400131115** -?$NOTE?...+....2....+....3....+
070500131115TEST *_NOTA_01_....2....+....3....+
070600131115TEST *_NOTA_02_....2....+....3....+
070700131115TEST *_NOTA_03_....2....+....3....+
070800131115TEST *_NOTA_04_....2....+....3....+
070900131115TEST *_NOTA_05_....2....+....3....+
071000131115TEST *_NOTA_06_....2....+....3....+
