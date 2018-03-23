000100100511      //---------------------------------------------------------------
000200100511      //?TNVRCRMR - Lancio trascodifica
000300100511      //---------------------------------------------------------------
000400100511     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000500100511
000600100511      //---------------------------------------------------------------
000700100511      //?Dichiarazione file.
000800100511      //---------------------------------------------------------------
000900100511
001000100511      // - Organigramma
001100100511     fAZORG01L  if   e           k disk
001200100511
001300100511      // - Video
001400100511     fTNVRCRMD  cf   e             workstn
001500100511     f                                     indds(IndDspF)
001600100511     f                                     infds(InfDspF)
001700100511
001800100511      //---------------------------------------------------------------
001900100511      //?Definizione costanti.
002000100511      //---------------------------------------------------------------
002100100511
002200100511      // - Tasti funzionali a video
002300100511     d c_F01           c                   const(x'31')
002400100511     d c_F02           c                   const(x'32')
002500100511     d c_F03           c                   const(x'33')
002600100511     d c_F04           c                   const(x'34')
002700100511     d c_F05           c                   const(x'35')
002800100511     d c_F06           c                   const(x'36')
002900100511     d c_F07           c                   const(x'37')
003000100511     d c_F08           c                   const(x'38')
003100100511     d c_F09           c                   const(x'39')
003200100511     d c_F10           c                   const(x'3A')
003300100511     d c_F11           c                   const(x'3B')
003400100511     d c_F12           c                   const(x'3C')
003500100511     d c_F13           c                   const(x'B1')
003600100511     d c_F14           c                   const(x'B2')
003700100511     d c_F15           c                   const(x'B3')
003800100511     d c_F16           c                   const(x'B4')
003900100511     d c_F17           c                   const(x'B5')
004000100511     d c_F18           c                   const(x'B6')
004100100511     d c_F19           c                   const(x'B7')
004200100511     d c_F20           c                   const(x'B8')
004300100511     d c_F21           c                   const(x'B9')
004400100511     d c_F22           c                   const(x'BA')
004500100511     d c_F23           c                   const(x'BB')
004600100511     d c_F24           c                   const(x'BC')
004700100511     d c_Enter         c                   const(x'F1')
004800100511
004900100511     d Digits          c                   const('0123456789')
005000100511
005100100511      //---------------------------------------------------------------
005200100511      //?Definizione schiere.
005300100511      //---------------------------------------------------------------
005400100513
005500100513     d filiali         s              3a   dim(10)
005600100511
005700100511      //---------------------------------------------------------------
005800100511      //?Definizione aree dati.
005900100511      //---------------------------------------------------------------
006000100511
006100100511      //---------------------------------------------------------------
006200100511      //?Definizione strutture dati.
006300100511      //---------------------------------------------------------------
006400100511
006500100511      // - Status
006600100511     d Psds           sds
006700100511     d   SDSpgm          *proc
006800100511
006900100511      // - InfDS
007000100511     d InfDspF         ds
007100100511     d  dsp_aid              369    369a                                        AID byte
007200100511     d  dsp_rig              370    370
007300100511     d  dsp_col              371    371
007400100511
007500100511      // - Indicatori su DspF
007600100511     d IndDspF         ds
007700100511        // - Indicatori di gestione del subfile
007800100511     d  SflDsp_N                      1n   overlay(IndDspF : 30)
007900100511     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
008000100511     d  SflEnd                        1n   overlay(IndDspF : 32)
008100100511        // - Indicatori di errore
008200100511     d  ErrMessage                    1n   overlay(IndDspF : 28)
008300100511        // - Indicatore di pozionamento cursore
008400100511     d  PosCur01                      1n   overlay(IndDspF : 50)
008500100511     d  PosCur02                      1n   overlay(IndDspF : 51)
008600100511     d  PosCur03                      1n   overlay(IndDspF : 52)
008700100511     d  PosCur04                      1n   overlay(IndDspF : 53)
008800100511     d  PosCur05                      1n   overlay(IndDspF : 54)
008900100511     d  PosCur06                      1n   overlay(IndDspF : 55)
009000100511     d  PosCur07                      1n   overlay(IndDspF : 56)
009100100511     d  PosCur08                      1n   overlay(IndDspF : 57)
009200100511     d  PosCur09                      1n   overlay(IndDspF : 58)
009300100511     d  PosCur10                      1n   overlay(IndDspF : 59)
009400100511     d  PosCurLa                      1n   overlay(IndDspF : 60)
009500100511        // - Indicatori di errore generico
009600100511     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009700100511
009800100511      // Campi di comodo per gestione indicatori a video
009900100511     d WindDspF        ds                  inz
010000100511     d   WindDspFa             1     49    inz(*zero)
010100100511     d   WindDspFb            50     99    inz(*zero)
010200100511
010300100511     d KPJBA         e ds
010400100511
010500100511      //---------------------------------------------------------------
010600100511      //?Definizione variabili globali.
010700100511      //---------------------------------------------------------------
010800100511
010900100511      // - Flags booleani
011000100511     d $End            s               n   inz(*off)
011100100511     d $Fine           s               n   inz(*off)
011200100511     d $InzW01         s               n   inz(*on)
011300100511
011400100511      // - Campi associati al video
011500100511     d $Video          s              2    inz('W1')
011600100513
011700100513      // - Campi di comodo
011800100513     d wFil            s              3  0 inz
011900100511
012000100511      //---------------------------------------------------------------
012100100511      //?Definizione procedure esterne.
012200100511      //---------------------------------------------------------------
012300100511
012400100513      // - Crea file di work e flegga TNVIS
012500100511     d Tnvrcrm1        pr                  extpgm('TNVRCRM1')
012600100513     d  filiali                       3a   dim(10)
012700100513
012800100513      // - Aggiorna file di work
012900100513     d Tnvrcrm11       pr                  extpgm('TNVRCRM11')
013000100513     d  filiali                       3a   dim(10)
013100100513
013200100513      // - Scrive nuovi file trattative + note + attività
013300100513     d Tnvrcrm2        pr                  extpgm('TNVRCRM2')
013400100513     d  filiali                       3a   dim(10)
013500100513
013600100513      // - Scrive nuovi file stati potenziali + note
013700100513     d Tnvrcrm3        pr                  extpgm('TNVRCRM3')
013800100513     d  filiali                       3a   dim(10)
013900100513
014000100513      // - Scrive attività su stati potenziali
014100100513     d Tnvrcrm4        pr                  extpgm('TNVRCRM4')
014200100513     d  filiali                       3a   dim(10)
014300100513
014400100513      // - Scrive note clienti
014500100513     d Tnvrcrm5        pr                  extpgm('TNVRCRM5')
014600100513     d  filiali                       3a   dim(10)
014700100511
014800100511      //---------------------------------------------------------------
014900100511      //?prototipi
015000100511      //---------------------------------------------------------------
015100100511
015200100511      //---------------------------------------------------------------
015300100511      //?Definizione key-list.
015400100511      //---------------------------------------------------------------
015500100511
015600100511      //---------------------------------------------------------------
015700100511      //?Riepilogo indicatori.
015800100511      //---------------------------------------------------------------
015900100511
016000100511      // 28    : Emissione messaggio di errore a video
016100100511      // 50    : Errore: Filiale 01
016200100511      // 51    : Errore: Filiale 02
016300100511      // 52    : Errore: Filiale 03
016400100511      // 53    : Errore: Filiale 04
016500100511      // 54    : Errore: Filiale 05
016600100511      // 55    : Errore: Filiale 06
016700100511      // 56    : Errore: Filiale 07
016800100511      // 57    : Errore: Filiale 08
016900100511      // 58    : Errore: Filiale 09
017000100511      // 59    : Errore: Filiale 10
017100100511      // 60    : Errore: Tipo lancio
017200100511      // 99    : Generico di Errore
017300100511
017400100511      //---------------------------------------------------------------
017500100511      //?M A I N - L I N E
017600100511      //---------------------------------------------------------------
017700100511
017800100511     c     *Entry        plist
017900100511     c                   parm                    KPJBA
018000100511
018100100511      /free
018200100511
018300100511       //?Operazioni iniziali
018400100513       exsr RoutInz;
018500100511
018600100511       // Gestione video
018700100511       DOW $Fine = *off;
018800100511         select;
018900100511           when $Video = 'W1';
019000100511             exsr GesW01;
019100100511           other;
019200100511             $Fine = *on;
019300100511         endsl;
019400100511       ENDDO;
019500100511
019600100511       //?Operazioni finali
019700100513       exsr RoutEnd;
019800100511
019900100511       //--------------------------------------------------------------
020000100511       //?Operazioni iniziali.
020100100511       //--------------------------------------------------------------
020200100513       BEGSR RoutInz;
020300100511
020400100511       ENDSR;
020500100511
020600100511       //--------------------------------------------------------------
020700100511       //?Gestione videata W01 - Richiesta parametri.
020800100511       //--------------------------------------------------------------
020900100511       BEGSR GesW01;
021000100511
021100100511       //?Inizializzazione videata
021200100511         IF  $InzW01 = *on;
021300100511           exsr Inzw01;
021400100511           $InzW01 = *off;
021500100511         ENDIF;
021600100511
021700100511       //?Emissione videata
021800100511         exfmt WIND01;
021900100511         errMessage   = *off;
022000100511         errGenerico  = *off;
022100100511         clear W01msg;
022200100511
022300100511         SELECT;
022400100511
022500100511       //?F3 = Fine
022600100513           WHEN dsp_aid = c_F03;
022700100511             exsr F03W01;
022800100511             leavesr ;
022900100511
023000100511       //?F6 = Conferma
023100100511           WHEN dsp_aid = c_F06;
023200100511             exsr F06W01;
023300100514             leavesr ;
023400100511
023500100511       //?Enter
023600100511           OTHER;
023700100511       //?Controllo i dati
023800100511             exsr CtrW01;
023900100511             IF  ErrGenerico;
024000100511               leavesr;
024100100511             ENDIF;
024200100511         ENDSL;
024300100511
024400100511       ENDSR;
024500100513
024600100513       //--------------------------------------------------------------
024700100513       //?Caricamento videata W01.
024800100513       //--------------------------------------------------------------
024900100513       BEGSR InzW01;
025000100513
025100100513         clear W01fil;
025200100513         clear W02fil;
025300100513         clear W03fil;
025400100513         clear W04fil;
025500100513         clear W05fil;
025600100513         clear W06fil;
025700100513         clear W07fil;
025800100513         clear W08fil;
025900100513         clear W09fil;
026000100513         clear W10fil;
026100100513
026200100513       ENDSR;
026300100511
026400100511       //--------------------------------------------------------------
026500100511       //?Controllo videata W01.
026600100511       //--------------------------------------------------------------
026700100511       BEGSR CtrW01;
026800100511
026900100511         WindDspF = IndDspF;
027000100511         reset WindDspFb;
027100100511         IndDspF  = WindDspF;
027200100517
027300100517         clear W01dfil;
027400100517         clear W02dfil;
027500100517         clear W03dfil;
027600100517         clear W04dfil;
027700100517         clear W05dfil;
027800100517         clear W06dfil;
027900100517         clear W07dfil;
028000100517         clear W08dfil;
028100100517         clear W09dfil;
028200100517         clear W10dfil;
028300100511
028400100511       //?Controllo le filiali
028500100511         IF  W01fil > 0;
028600100511           wfil = W01fil;
028700100511           exsr CtrFil;
028800100511           IF  ErrGenerico;
028900100511             PosCur01 = *on;
029000100511             leavesr;
029100100511           ENDIF;
029200100511           W01dfil = ORGdes;
029300100511         ENDIF;
029400100511
029500100511         IF  W02fil > 0;
029600100511           wfil = W02fil;
029700100511           exsr CtrFil;
029800100511           W02dfil = ORGdes;
029900100511           IF  ErrGenerico;
030000100511             PosCur02 = *on;
030100100511             leavesr;
030200100511           ENDIF;
030300100511         ENDIF;
030400100511
030500100511         IF  W03fil > 0;
030600100511           wfil = W03fil;
030700100511           exsr CtrFil;
030800100511           W03dfil = ORGdes;
030900100511           IF  ErrGenerico;
031000100511             PosCur03 = *on;
031100100511             leavesr;
031200100511           ENDIF;
031300100511         ENDIF;
031400100511
031500100511         IF  W04fil > 0;
031600100511           wfil = W04fil;
031700100511           exsr CtrFil;
031800100511           W04dfil = ORGdes;
031900100511           IF  ErrGenerico;
032000100511             PosCur04 = *on;
032100100511             leavesr;
032200100511           ENDIF;
032300100511         ENDIF;
032400100511
032500100511         IF  W05fil > 0;
032600100511           wfil = W05fil;
032700100511           exsr CtrFil;
032800100511           W05dfil = ORGdes;
032900100511           IF  ErrGenerico;
033000100511             PosCur05 = *on;
033100100511             leavesr;
033200100511           ENDIF;
033300100511         ENDIF;
033400100511
033500100511         IF  W06fil > 0;
033600100511           wfil = W06fil;
033700100511           exsr CtrFil;
033800100511           W06dfil = ORGdes;
033900100511           IF  ErrGenerico;
034000100511             PosCur06 = *on;
034100100511             leavesr;
034200100511           ENDIF;
034300100511         ENDIF;
034400100511
034500100511         IF  W07fil > 0;
034600100511           wfil = W07fil;
034700100511           exsr CtrFil;
034800100511           W07dfil = ORGdes;
034900100511           IF  ErrGenerico;
035000100511             PosCur07 = *on;
035100100511             leavesr;
035200100511           ENDIF;
035300100511         ENDIF;
035400100511
035500100511         IF  W08fil > 0;
035600100511           wfil = W08fil;
035700100511           exsr CtrFil;
035800100511           W08dfil = ORGdes;
035900100511           IF  ErrGenerico;
036000100511             PosCur08 = *on;
036100100511             leavesr;
036200100511           ENDIF;
036300100511         ENDIF;
036400100511
036500100511         IF  W09fil > 0;
036600100511           wfil = W09fil;
036700100511           exsr CtrFil;
036800100511           W09dfil = ORGdes;
036900100511           IF  ErrGenerico;
037000100511             PosCur09 = *on;
037100100511             leavesr;
037200100511           ENDIF;
037300100511         ENDIF;
037400100511
037500100511         IF  W10fil > 0;
037600100511           wfil = W10fil;
037700100511           exsr CtrFil;
037800100511           W10dfil = ORGdes;
037900100511           IF  ErrGenerico;
038000100511             PosCur10 = *on;
038100100511             leavesr;
038200100511           ENDIF;
038300100511         ENDIF;
038400100511
038500100511       //?Almeno un lancio deve essere richiesto
038600100517         IF  W01work = *blanks and W01tratt = *blanks and
038700100517             W01pot  = *blanks and W01cli   = *blanks;
038800100511           ErrGenerico = *on;
038900100511           ErrMessage  = *on;
039000100511           W01msg = 'Richiedere almeno 1 lancio';
039100100511           PosCurLa = *on;
039200100511           leavesr;
039300100511         ENDIF;
039400100511
039500100511       //?Solo un lancio deve essere richiesto
039600100517         IF  W01work <> *blanks and W01tratt <> *blanks and
039700100517             W01pot  <> *blanks and W01cli   <> *blanks;
039800100511           ErrGenerico = *on;
039900100511           ErrMessage  = *on;
040000100511           W01msg = 'Richiedere solo 1 lancio';
040100100511           PosCurLa = *on;
040200100511           leavesr;
040300100511         ENDIF;
040400100511
040500100511       ENDSR;
040600100511
040700100511       //--------------------------------------------------------------
040800100511       //?Controllo filiale.
040900100511       //--------------------------------------------------------------
041000100511       BEGSR CtrFil;
041100100511
041200100511         chain wfil AZORG01L;
041300100511         IF  not %found(AZORG01L) or  ORGfva <> *blanks or
041400100511            (ORGfag <> 'F' and ORGfag <> 'A');
041500100511           ErrGenerico = *on;
041600100511           ErrMessage  = *on;
041700100511           W01msg = 'Filiale errata';
041800100511         ENDIF;
041900100513
042000100513       ENDSR;
042100100511
042200100511       //--------------------------------------------------------------
042300100511       //?Gestione tasto funzionale F03 da videata W01.
042400100511       //?F3=Fine
042500100511       //--------------------------------------------------------------
042600100511       BEGSR F03W01;
042700100511
042800100513       //?Esco
042900100511         $Fine = *on;
043000100511
043100100511       ENDSR;
043200100511
043300100511       //--------------------------------------------------------------
043400100511       //?Gestione tasto funzionale F06 da videata W01.
043500100511       //?F6=Conferma
043600100511       //--------------------------------------------------------------
043700100511       BEGSR F06W01;
043800100513
043900100513         clear Filiali;
044000100513         Filiali(01) = %editc(W01fil:'X');
044100100513         Filiali(02) = %editc(W02fil:'X');
044200100513         Filiali(03) = %editc(W03fil:'X');
044300100513         Filiali(04) = %editc(W04fil:'X');
044400100513         Filiali(05) = %editc(W05fil:'X');
044500100513         Filiali(06) = %editc(W06fil:'X');
044600100513         Filiali(07) = %editc(W07fil:'X');
044700100513         Filiali(08) = %editc(W08fil:'X');
044800100513         Filiali(09) = %editc(W09fil:'X');
044900100513         Filiali(10) = %editc(W10fil:'X');
045000100513
045100100513         SELECT;
045200100517
045300100517       //?Richiesto lancio file Work
045400100517           WHEN  W01work <> *blanks;
045500100517
045600100517         //?Richiamo pgm che crea file di work e flegga TNVIS
045700100517             tnvrcrm1  (filiali);
045800100517         //?Richiamo pgm che aggiorna file di work
045900100517             tnvrcrm11 (filiali);
046000100513
046100100513       //?Richiesto lancio trattative
046200100513           WHEN  W01tratt <> *blanks;
046300100517
046400100513         //?Richiamo pgm che scrive nuovi file trattative + note + attività
046500100513             tnvrcrm2  (filiali);
046600100513
046700100513       //?Richiesto lancio potenziali
046800100513           WHEN  W01pot <> *blanks;
046900100513
047000100513         //?Richiamo pgm che scrive nuovi stati potenziali + note
047100100514             tnvrcrm3  (filiali);
047200100513         //?Richiamo pgm che scrive nuove attività da potenziali
047300100513             tnvrcrm4  (filiali);
047400100513
047500100513       //?Richiesto lancio clienti
047600100513           WHEN  W01cli <> *blanks;
047700100513
047800100513         //?Richiamo pgm che scrive note clienti
047900100513             tnvrcrm5  (filiali);
048000100513
048100100513         ENDSL;
048200100514
048300100514         $Fine = *on;
048400100511
048500100511       ENDSR;
048600100511
048700100511
048800100511       //--------------------------------------------------------------
048900100511       //?Operazioni finali.
049000100511       //--------------------------------------------------------------
049100100511       BEGSR RoutEnd;
049200100511
049300100511         *inLR = *on;
049400100511         return;
049500100511
049600100511       ENDSR;
049700100511
049800100511      /end-free
