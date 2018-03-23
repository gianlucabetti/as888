000100120717       //==============================================================
000200120717       //?FNLSV1R * Inventario documenti da restituire al mittente     ?
000300120717       //==============================================================
000400120717
000500120717       //--------------------------------------------------------------
000600120717       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120717       //--------------------------------------------------------------
000800120717
000900120717     /*PRM  dbgview(*source)
001000120717     /*CMD  ovrdbf file(TNTBE01S) tofile(GAITRAGRPS/TNTBE01L) +
001100120717     /*CMD         ovrscope(*calllvl)
001200120717     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
001300120717     /*CMD         ovrscope(*calllvl)
001400120717     /*CMD  ovrdbf file(TITAS32C) tofile(GAITRAGRPS/TITAS32C) +
001500120717     /*CMD         ovrscope(*calllvl)
001600120717     /*CMD  ovrdbf file(TITA435C) tofile(GAITRAGRPS/TITA435C) +
001700120717     /*CMD         ovrscope(*calllvl)
001800120717     /*CMD  ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
001900120717     /*CMD         ovrscope(*calllvl)
002000120717     /*END  dltovr file(TITAS30C TITAS32C TITA435C FIAR531C +
002100120717     /*END              TNTBE01S) lvl(*)
002200120717     /*END
002300120717
002400120717       //--------------------------------------------------------------
002500120717       //?Specifiche di controllo.                                     ?
002600120717       //--------------------------------------------------------------
002700120717
002800120717     h decedit('0,') datedit(*ymd/) option(*nodebugio)
002900120717
003000120717       //--------------------------------------------------------------
003100120717       //?Dichiarazione file.                                          ?
003200120717       //--------------------------------------------------------------
003300120717
003400120717       // -?Tabelle?
003500120717     fTABEL00F  if   e           k disk
003600120717     fTNTBE01L  if   e           k disk
003700120717     fTNTBE01S  if   e           k disk    extfile(wTBE1S) usropn
003800120717     f                                     rename(TNTBE000:TNTBE00S)
003900120717       // -?Anagrafica clienti?
004000120717     fCNACO00F  if   e           k disk
004100120717     fFNLBL01L  if   e           k disk
004200120717       // -?Dati di Bolle SEDE?
004300120717     fTITAS30C  if   e           k disk    extfile(wTAS30) usropn
004400120717     fTITAS32C  if   e           k disk    extfile(wTAS32) usropn
004500120717     f                                     rename(TITAS000:TITAS002)
004600120717     f                                     rename(TITAS010:TITAS012)
004700120717     f                                     rename(TITASP00:TITASP02)
004800120717     fTITA435C  if   e           k disk    extfile(wTA435) usropn
004900120717     fFIAR531C  Uf A e           k disk    extfile(wFIAR5) usropn
005000120717       // -?File Video?
005100120717     fFNLSV1D   cf   e             workstn indds(IndDspF)
005200120717     f                                     infds(InfDspF)
005300120717
005400120717       //--------------------------------------------------------------
005500120717       //?Definizione costanti.                                        ?
005600120717       //--------------------------------------------------------------
005700120717
005800120717       // -?Tasti funzionali a video?
005900080807     d c_F01           c                   const(x'31')
006000080807     d c_F02           c                   const(x'32')
006100080807     d c_F03           c                   const(x'33')
006200120717     d c_F04           c                   const(x'34')
006300080807     d c_F05           c                   const(x'35')
006400080807     d c_F06           c                   const(x'36')
006500080807     d c_F07           c                   const(x'37')
006600080807     d c_F08           c                   const(x'38')
006700080807     d c_F09           c                   const(x'39')
006800080807     d c_F10           c                   const(x'3A')
006900120717     d c_F11           c                   const(x'3B')
007000080807     d c_F12           c                   const(x'3C')
007100080807     d c_F13           c                   const(x'B1')
007200080807     d c_F14           c                   const(x'B2')
007300080807     d c_F15           c                   const(x'B3')
007400080807     d c_F16           c                   const(x'B4')
007500080807     d c_F17           c                   const(x'B5')
007600080807     d c_F18           c                   const(x'B6')
007700080807     d c_F19           c                   const(x'B7')
007800080807     d c_F20           c                   const(x'B8')
007900080807     d c_F21           c                   const(x'B9')
008000080807     d c_F22           c                   const(x'BA')
008100080807     d c_F23           c                   const(x'BB')
008200080807     d c_F24           c                   const(x'BC')
008300120717     d c_Enter         c                   const(x'F1')
008400120717     d c_RollDown      c                   const(x'F4')
008500120717     d c_RollUp        c                   const(x'F5')
008600120717
008700120717       //--------------------------------------------------------------
008800120717       //?Definizione schiere.                                         ?
008900120717       //--------------------------------------------------------------
009000120717
009100120717       // -?Messaggi di errore?
009200120717     d Msg             s             78    dim(19)  ctdata  perrcd(1)
009300120717
009400120717       // -?Particolarità consegne che prevedono la restituzione della?
009500120717       //  ?BAM?
009600120717     d Sk_7R           s              2    dim(50)
009700120717
009800120717       //--------------------------------------------------------------
009900120717       //?Definizione aree dati.                                       ?
010000120717       //--------------------------------------------------------------
010100120717
010200120717       // -?Dati utente?
010300120717     d §AzUte        e ds                  extname(AZUTE00F)
010400120717     d                                     dtaara
010500120717     d §DatiUte      e ds                  extname(dDatiUte)
010600120717     d                                     dtaara
010700120717
010800120717       //--------------------------------------------------------------
010900120717       //?Definizione strutture dati.                                  ?
011000120717       //--------------------------------------------------------------
011100120717
011200120717       // -?InfDS?
011300080807     d InfDspF         ds
011400120717     d   dsp_aid             369    369a
011500120717     d*//sfl_rrn             376    377i 0
011600120717     d*//min_nrr             378    379i 0
011700120717     d*//num_rcds            380    381i 0
011800080807
011900120717       // -?Indicatori su DspF?
012000080807     d IndDspF         ds
012100120717         // -?Indicatori per Attibuti di visualizzazione?
012200120717     d   AbilitF12                     n   overlay(IndDspF : 01)
012300120717     d   VisTotBo                      n   overlay(IndDspF : 02)
012400120718     d   AbilitF16                     n   overlay(IndDspF : 03)
012500120717     d   VisNumer                      n   overlay(IndDspF : 05)
012600120717     d   VisAlfa                       n   overlay(IndDspF : 06)
012700120717     d   VisSped                       n   overlay(IndDspF : 07)
012800120717     d   PosCurs                       n   overlay(IndDspF : 08)
012900120717     d   IndManut                      n   overlay(IndDspF : 10)
013000120717         // -?Emissione messaggio di errore?
013100120717     d   errMessage                    n   overlay(IndDspF : 28)
013200120717         // -?Posizionamento cursore & Segnalazione errore?
013300120717     d   errBar1                       n   overlay(IndDspF : 40)
013400120717     d   errBar2                       n   overlay(IndDspF : 41)
013500120717     d   errBar3                       n   overlay(IndDspF : 42)
013600120717     d   errBar4                       n   overlay(IndDspF : 43)
013700120717     d   errBar5                       n   overlay(IndDspF : 44)
013800120717     d   errBar6                       n   overlay(IndDspF : 45)
013900120717     d   errBar7                       n   overlay(IndDspF : 46)
014000120717     d   errBar8                       n   overlay(IndDspF : 47)
014100120717     d   errKSC                        n   overlay(IndDspF : 50)
014200120717     d   errSPE                        n   overlay(IndDspF : 51)
014300120717     d   errBAR                        n   overlay(IndDspF : 52)
014400120717     d   errDRDT                       n   overlay(IndDspF : 53)
014500120717     d   errMTV                        n   overlay(IndDspF : 54)
014600120717         //   -?Riemissione videata?
014700120717     d   errGenerico                   n   overlay(IndDspF : 99)
014800080807
014900120717       // -?Struttura dati per sottocampare il barcode della spedizione?
015000120717     d ds_pis          ds                  inz
015100120717     d   ds_pisAAS                    2  0 inz
015200120717     d   ds_pisLNP                    3  0 inz
015300120717     d   ds_pisNRS                    2  0 inz
015400120717     d   ds_pisNSP                    7  0 inz
015500120717     d   ds_ChkDgt                    1  0 inz
015600120717
015700120717       // -?Parametri ricevuti?
015800120717     d KPJBA         e ds
015900120717
016000120717       // -?Tabella "7N" = Categorie Fogli?
016100120717     d ds7N          e ds                  inz
016200120717       // -?Tabella "7R" = Particolarità Consegne?
016300120717     d ds7R          e ds                  inz
016400120717
016500120717       // -?Tabella "JDC" = Clienti per ritorno immagini DOC?
016600120717     d dJDC          e ds                  inz
016700120717
016800120717       // -?Dati FIAR5 / trk "GEN"?
016900120717     d dAR5gen       e ds                  inz
017000120717       // -?Dati FIAR5 / trk "DOC"?
017100120717     d dAR5doc       e ds                  inz  qualified
017200120717
017300120717       //--------------------------------------------------------------
017400120717       //?Definizione variabili globali.                               ?
017500120717       //--------------------------------------------------------------
017600120717
017700120717       // -?Flags booleani?
017800120717     d $Fine           s               n   inz(*off)
017900120717     d $Inzd01         s               n   inz(*on)
018000120717     d $Inzd02         s               n   inz(*on)
018100120717     d wTitasP         s              1
018200120717
018300120717       // -?Indici di schiera?
018400120717     d xx              s              3  0 inz
018500120717     d IndX            s              3  0 inz
018600120717
018700120717       // -?Variabili per la gestione del video?
018800120717     D $Video          s              2    inz('D1')
018900120717
019000120717       // -?Nome esteso Libreria/File degli archivi di Sede?
019100120717     d wTBE1S          s             21    inz('GAITRAGRU /TNTBE01L')
019200120717     d wTAS30          s             21    inz('GAITRAGRU /TITAS30C')
019300120717     d wTAS32          s             21    inz('GAITRAGRU /TITAS32C')
019400120717     d wTA435          s             21    inz('GAITRAGRU /TITA435C')
019500120717     d wFIAR5          s             21    inz('GAITRAGRU /FIAR531C')
019600120717
019700120717       // -?Campi "data"?
019800120717     d DataSys         s               d   datfmt(*iso) inz(*sys)
019900120717     d DataIso         s               d   datfmt(*iso)
020000120717     d TimeIso         s               t   timfmt(*iso)
020100120717     d DataEur         s               d   datfmt(*eur)
020200120717
020300120717       // -?Campi di comodo?
020400120717     d ParManut        s              1
020500120717     d w007a           s              7
020600120717     d BarCode         s             14
020700120717     d Secolo          s              2  0
020800110520     d Kanno           s              4  0
020900120717     d ContaBolle      s              2  0 inz
021000120717     d Wprof           s              3
021100120717     d s_TASaas        s                   like(TASaas)  inz
021200120717     d s_TASlnp        s                   like(TASlnp)  inz
021300120717     d s_TASnrs        s                   like(TASnrs)  inz
021400120717     d s_TASnsp        s                   like(TASnsp)  inz
021500120717     d s_TASmgs        s                   like(TASmgs)  inz
021600120720     d s_TASccm        s                   like(TASccm)  inz
021700120717     d W5aas           s                   like(TASaas)  inz
021800120717     d W5lnp           s                   like(TASlnp)  inz
021900120717     d W5nrs           s                   like(TASnrs)  inz
022000120717     d W5nsp           s                   like(TASnsp)  inz
022100120717     d b_W5aas         s                   like(TASaas)  inz
022200120717     d b_W5lnp         s                   like(TASlnp)  inz
022300120717     d b_W5nrs         s                   like(TASnrs)  inz
022400120717     d b_W5nsp         s                   like(TASnsp)  inz
022500120717
022600120717       //--------------------------------------------------------------
022700120717       //?Definizione prototipi procedure.                             ?
022800120717       //--------------------------------------------------------------
022900120717
023000120717       // -?Reperimento dati utente?
023100120717     d TIBS34ds      e ds                  inz
023200120717      /copy gaitrasrc/srcProtoPR,TIBS34R
023300120717
023400120717       // -?Interrogazione sottoconti?
023500120717      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
023600080807      /copy gaitrasrc/srcprotopr,XALFA3BR
023700120717
023800120717       // -?Interrogazione particolarità consegne - tab. "7R" (TABEL)?
023900120717     d ds7Pqrs       e ds                  inz
024000120717     d   ds7ric      e                     inz('S')
024100120717     d   ds7ges      e                     inz('O')
024200120717     d   kscabi                       2    dim(30)  overlay(ds7cdp)
024300120717      /copy gaitrasrc/srcprotoPR,TRTB70R
024400120717
024500120717       // -?Calcolo check digit del barcode?
024600120717     d TRUL28ds      e ds                  inz
024700120717     d   I28tla      e                     inz('L')
024800120717     d   I28mod      e                     inz('BAR')
024900120717      /copy gaitrasrc/srcprotoPR,TRUL28R1
025000120717
025100120717       // -?Motivazioni errata ricezione documenti?
025200120717     d TNTB92ds      e ds                  inz
025300120717      /copy gaitrasrc/srcprotoPR,TNTB92R
025400120717
025500120717       // -?Stampa copia LdV x Scannerizzazione documenti clienti?
025600120717     d TNSB25ds      e ds                  inz
025700120717     d   D25ges      e                     inz('T')
025800120717      /copy gaitrasrc/srcProtoPR,TNSB25R
025900120717
026000120717       //--------------------------------------------------------------
026100120717       //?Definizione key-list.                                        ?
026200120717       //--------------------------------------------------------------
026300120717
026400120719       // -?File TNTBE01S (TNTBE01L di Sede)?
026500120717     d k05tntbe01    e ds                  extname(TNTBE01S : *key)
026600120717     d                                     prefix(k_)   inz
026700120717
026800120717       //--------------------------------------------------------------
026900120717       //?Specifiche di input.                                         ?
027000120717       //--------------------------------------------------------------
027100080826
027200120717       // -?TITAS30C?
027300120717     iTITAS000      81
027400120717     iTITAS010      82
027500120717     iTITASP00      83
027600120717
027700120717       // -?TITAS32C?
027800120717     iTITAS002      84
027900120717     iTITAS012      85
028000120717     iTITASP02      86
028100120717
028200120717       //--------------------------------------------------------------
028300120717       //?M A I N - L I N E                                            ?
028400120717       //--------------------------------------------------------------
028500120717
028600120717     c     *Entry        plist
028700120717     c                   parm                    KPJBA
028800120706      /free
028900120706
029000120717       // -?Operazioni iniziali?
029100120717       exsr  sr_RoutInz;
029200080807
029300120717       // -?Gestione video?
029400120717       DoW  $Fine = *off;
029500120717
029600080807         select;
029700120717           // -?Gestione videata D1 (selezioni)?
029800120717           when  $Video = 'D1';
029900120717             exsr  sr_GesD01;
030000120717           // -?Gestione videata D2 (num. spedizione / riferimenti)?
030100120717           when  $Video = 'D2';
030200120717             exsr  sr_GesD02;
030300120717           // -?Gestione videata D2 (inventario documento)?
030400120717           when  $Video = 'D3';
030500120717             exsr  sr_GesD03;
030600120717           // -?? ? ? ? ??
030700080807           other   ;
030800080807             $Fine = *on;
030900080807         endsl;
031000120717
031100120717       EndDo;
031200080807
031300120717       // -?Operazioni finali?
031400120717       exsr  sr_RoutEnd;
031500080807
031600080807       //--------------------------------------------------------------
031700080807       //?Operazioni iniziali.
031800080807       //--------------------------------------------------------------
031900120717       BEGSR  sr_RoutInz;
032000120717
032100120717         ParManut = %subst( kpjbu : 1 : %len(ParManut) );
032200120717
032300120717         // -?In base al flag si imposta la testata *pgm?
032400120717         if ParManut = 'M';
032500120717           V1Ctes = '*  M A N U T E N Z I O N E  *';
032600120717           IndManut = *on;
032700120717         else;
032800120717           V1Ctes = '**   I M M I S S I O N E   **';
032900120717           IndManut = *off;
033000120717         endif;
033100120717
033200120717         $InzD01 = *on;
033300080807
033400120717         // -?Reperimento dati job?
033500120717         exsr  sr_DatiJob;
033600120717
033700120717         // -?Data corrente?
033800120717         Secolo = %int( %subst( %editc( %dec(DataSys) : 'X') : 1 : 2) );
033900120717
034000120717         // -?Carico tutte le particolarità consegne che prevedono?
034100120717         //  ?la restituzione della BAM?
034200120717         clear  XX;
034300120717         setll  ( 1 : '7R' )  TABEL;
034400120717         reade  ( 1 : '7R' )  TABEL;
034500120717         dow  not %eof(TABEL00F);
034600120717           if  TBLflg = *blank;
034700120717             ds7R = TBLuni;
034800120717             if  §7R1bf = 'B';
034900120717               xx += 1;
035000120717               sk_7R(xx) = TBLkey;
035100120717             endif;
035200120717           endif;
035300120717           reade ( 1 : '7R' )  TABEL;
035400120717         enddo;
035500120717
035600120717         // -?Apertura TNTBE e FIAR5 di sede?
035700120717         if  %subst( KNSIF : 7 : 1 ) = 'P';
035800120717           %subst( wTBE1S : 7 : 4 ) = 'GRPS';
035900120717           %subst( wFIAR5 : 7 : 4 ) = 'GRPS';
036000120717         endif;
036100120717         open  TNTBE01S;
036200120717         open  FIAR531C;
036300080807
036400120717       ENDSR;
036500080807
036600080807       //--------------------------------------------------------------
036700080807       //?Reperimento Dati del job (Utente/Operativi).
036800080807       //--------------------------------------------------------------
036900120717       BEGSR  sr_DatiJob;
037000080807
037100080807         in(E) §AzUte;
037200080807         if NOT %error;
037300080807           in(E) §DatiUte;
037400080807         endif;
037500080807         if %error or RSut = *blanks;
037600080807           clear TIBS34ds;
037700080807           tibs34r(tibs34ds);
037800080807           in §AzUte;
037900080807           in §DatiUte;
038000080807         endif;
038100080807
038200080807       ENDSR;
038300120717
038400080807       //--------------------------------------------------------------
038500080807       //?Gestione video 1
038600080807       //--------------------------------------------------------------
038700120717       BEGSR  sr_GesD01;
038800080807
038900120717         // -?Inizializzazione videata?
039000080807         if  $Inzd01 = *on;
039100120717           exsr  sr_InzD01;
039200120717           $Inzd01  = *off;
039300080807         endif;
039400080807
039500120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
039600120717         AbilitF12 = *off;
039700120718         AbilitF16 = *off;
039800120717         if  Not errGenerico;
039900120717           write  LSV1T01;
040000120717           write  LSV1Z01;
040100080807         endif;
040200080807
040300120717         // -?Emissione videata?
040400120717         exfmt  LSV1D01;
040500080807
040600120717         reset  errMessage;
040700120717         reset  errGenerico;
040800120717         clear  V1Cmsg;
040900080807
041000120717         SELECT;
041100080807
041200120717           // -?F3=Fine?
041300120717           WHEN  dsp_aid = c_F03;
041400120717             $Fine = *on;
041500080807
041600120717           // -?F8=Gestione motivi errati documentazione?
041700120717           WHEN  dsp_aid = c_F08;
041800120717             clear  TNTB92ds;
041900120717             B92ke1 = %editc( DUTpou : 'X' );
042000120717             kpjbu  = TNTB92ds;
042100120717             callp  tntb92r ( kpjba );
042200120329
042300120717           // -?Invio?
042400080807           OTHER;
042500120717             exsr  sr_ContrD01;
042600120717             if  errGenerico = *on;
042700080807               leavesr;
042800120717             endif;
042900120717             $Video  = 'D2';
043000120717             $Inzd02 = *on;
043100080807
043200120717         ENDSL;
043300080807
043400080807       ENDSR;
043500120717
043600080807       //--------------------------------------------------------------
043700080807       //?Inizializzazione video1
043800080807       //--------------------------------------------------------------
043900120717       BEGSR  sr_InzD01;
044000080807
044100120717         clear  LSV1D01;
044200120717         clear  LSV1D02;
044300120717         clear  LSV1D03;
044400120717
044500120717         V1Ctip = 'A';
044600120717
044700120717       ENDSR;
044800120717
044900120717       //--------------------------------------------------------------
045000120717       //?Controlli video 1
045100120717       //--------------------------------------------------------------
045200120717       BEGSR  sr_ContrD01 ;
045300120717
045400120717         clear  errKSC;
045500120717
045600120717         select;
045700120717
045800120717           // -?Codice Cliente Mittente NON inserito (nè la Rag.Soc.)?
045900120717           when  V1Cksc = *zero  and  V1Dksc = *blank;
046000120717             errKSC = *on;
046100120717             V1Cmsg = Msg(01);
046200120717             errMessage  = *on;
046300120717             errGenerico = *on;
046400120717             leavesr;
046500120717
046600120717           // -?Ricerca Codice Cliente Mittente x Rag.Soc.?
046700120717           when  V1Cksc = *zero  and  V1Dksc <> *blank;
046800120717             xparsta = 9;
046900120717             xpardut = rsut;
047000120717             clear xparrag;
047100120717             xparrag = V1Dksc;
047200120717             xparkcc = DUTkci;
047300120717             clear  xparflr;
047400120717             clear  xpardit;
047500120717             xparnum = 1;
047600120717             clear  xparkcm;
047700120717             clear  xparksm;
047800120717             clear  xparkdm;
047900120717             clear  xparesci;
048000120717             clear  xparerr;
048100120717             clear  xpariva;
048200120717             clear  xparcdf;
048300120717             XALFA3BR ( xpardut : xparkut : xparrag : xparkcc : xparsta :
048400120717                        xparflr : xpardit : xparnum : xparkcm : xparksm :
048500120717                        xparkdm : xparesci: xparerr : xpariva : xparcdf );
048600120717             if  xparsta <> -1;
048700120717               w007a  = xparksm;
048800120717               V1Cksc = %int(w007a);
048900120717               V1Dksc = xparrag;
049000120717             else;
049100120717               clear  V1Dksc;
049200120717             endif;
049300120717             ErrGenerico = *on;
049400120717             write  LSV1T01;
049500120717             write  LSV1Z01;
049600120717             leavesr;
049700120717
049800120717           // -?Codice Cliente Mittente inserito?
049900120717           other;
050000120717             chain  ( 1 : DUTkci : V1Cksc )  CNACO000;
050100120717             if  Not %found(CNACO00F)  or  ACOflg <> *blank;
050200120717               errKSC = *on;
050300120717               V1Cmsg = Msg(02);
050400120717               errMessage  = *on;
050500120717               errGenerico = *on;
050600120717               clear  V1Dksc;
050700120717               leavesr;
050800120717             endif;
050900120717             V1Dksc = ACOrag;
051000120717
051100120717         endsl;
051200120717
051300120717         // -?Verifico se il cliente prevede la particolarità giusta?
051400120717         clear ds7Pqrs;
051500120717         ds7ksc  = V1Cksc;
051600120717         ds7ges  = 'O';
051700120717         ds7ric  = 'S';
051800120717         kpjbu   = ds7Pqrs;
051900120717         TRTB70R ( KPJBA );
052000120717         ds7Pqrs = kpjbu;
052100120717
052200120717         xx = 1;
052300120717         clear  IndX;
052400120717         dow  KscAbi(xx) <> *blank  and  IndX = *zero;
052500120717           IndX = %lookup( kscabi(xx) : sk_7R );
052600120717           xx  += 1;
052700120717         enddo;
052800120717
052900120717         if  IndX = *zero;
053000120717           errKSC = *on;
053100120717           V1Cmsg = Msg(03);
053200120717           errMessage  = *on;
053300120717           errGenerico = *on;
053400120717           leavesr;
053500120717         endif;
053600120717
053700120717       ENDSR;
053800120717
053900080807       //--------------------------------------------------------------
054000080807       //?Gestione video 2
054100080807       //--------------------------------------------------------------
054200120717       BEGSR  sr_GesD02;
054300080807
054400120717         // -?Inizializzazione videata?
054500080807         if  $Inzd02 = *on;
054600120717           exsr  sr_InzD02;
054700120717           $Inzd02  = *off;
054800080807         endif;
054900080807
055000120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
055100120717         AbilitF12 = *on;
055200120718         AbilitF16 = *off;
055300080807         if  errGenerico = *off;
055400120717           write  LSV1T01;
055500120717           write  LSV1Z01;
055600120717           write  LSV1D01;
055700120717           write  Protect;
055800080807         endif;
055900080807
056000120717         // -?Emissione videata?
056100120717         exfmt  LSV1D02;
056200080807
056300120717         reset  errMessage;
056400120717         reset  errGenerico;
056500120717         clear  V1cmsg;
056600080807
056700120717         SELECT;
056800080807
056900120717           // -?F3=Fine?
057000120717           WHEN  dsp_aid = c_F03;
057100120717             $Fine = *on;
057200120329
057300120717           // -?F8=Gestione motivi errati documentazione?
057400120717           WHEN  dsp_aid = c_F08;
057500120718             clear  TNTB92ds;
057600120718             B92ke1 = %editc( DUTpou : 'X' );
057700120718             kpjbu  = TNTB92ds;
057800120718             callp  tntb92r (kpjba);
057900120329
058000080807
058100120717           // -?F12=Ritorno?
058200120717           WHEN  dsp_aid = c_F12;
058300120717             $Video   = 'D1';
058400120717             VisTotBo = *off;
058500120718             unlock  FIAR531C;
058600120717             close(e) TITAS32C;
058700120717             close(e) TITAS30C;
058800120717             close(e) TITA435C;
058900080807
059000120717           // -?Invio?
059100080807           OTHER;
059200120717             exsr  sr_ContrD02 ;
059300120717             if  errGenerico = *on;
059400080807               leavesr;
059500120717             endif;
059600080807
059700120717         ENDSL;
059800080807
059900080807       ENDSR;
060000120717
060100120717       //--------------------------------------------------------------
060200120717       //?Inizializzazione video 2
060300120717       //--------------------------------------------------------------
060400120717       BEGSR  sr_InzD02;
060500120717
060600120717         clear  LSV1D02;
060700120717         clear  LSV1D03;
060800120717
060900120717         exsr  sr_InzParz02;
061000120717
061100120717         V1Caas = %subdt( %date( *date ) : *y );
061200120717
061300120717         // -?Campi video 3?
061400120717         V1Cfil = DUTpou;
061500120717
061600120717         if  %subst( knsif : 7 : 1) = 'P';
061700120717           %subst( wTAS30 : 7 : 4 ) = 'GRPS';
061800120717           %subst( wTAS32 : 7 : 4 ) = 'GRPS';
061900120717           %subst( wTA435 : 7 : 4 ) = 'GRPS';
062000120717         endif;
062100120717
062200120717         select;
062300120717           when  V1Ctip = 'N';
062400120717             VisNumer = *on;
062500120717             open  TITAS32C;
062600120717           when V1Ctip = 'A';
062700120717             VisAlfa = *on;
062800120717             open  TITA435C;
062900120717             open  TITAS30C;
063000120717           other;
063100120717             VisSped = *on;
063200120717             // -?Se barcode accendo indicatore per posizionamento cursore?
063300120717             if  V1Ctip = 'B';
063400120717               PosCurs = *on;
063500120717             endif;
063600120717             open  TITAS30C;
063700120717         endsl;
063800120717
063900120717       ENDSR;
064000120717
064100120717       //--------------------------------------------------------------
064200120717       //?Controlli video 2
064300120717       //--------------------------------------------------------------
064400120717       BEGSR  sr_ContrD02 ;
064500120717
064600120717         errSPE = *off;
064700120717         errBAR = *off;
064800120717         *in81  = *off;
064900120717         *in82  = *off;
065000120717         *in83  = *off;
065100120717         *in84  = *off;
065200120717         *in85  = *off;
065300120717         *in86  = *off;
065400120717         clear  wTitasP;
065500120717         clear  ContaBolle;
065600120717         clear  s_TASaas;
065700120717         clear  s_TASmgs;
065800120717         clear  s_TASlnp;
065900120717         clear  s_TASnrs;
066000120717         clear  s_TASnsp;
066100120720         clear  s_TASccm;
066200120717
066300120717         SELECT;
066400120717
066500120717           // -?Immissione per riferimenti mitt NUMERICO?
066600120717           WHEN VisNumer = *on;
066700120717             if V1Crmn = *zero;
066800120717               errSPE = *on;
066900120717               V1Cmsg = Msg(04);
067000120717               errMessage  = *on;
067100120717               errGenerico = *on;
067200120717               leavesr;
067300120717             endif;
067400120717
067500120717             setll  (V1Crmn : V1Cksc)  TITAS32C;
067600120717             reade  (V1Crmn : V1Cksc)  TITAS32C;
067700120717             DoW   Not  %eof(TITAS32C);
067800120717
067900120717               // -?Escludo se bolla figlia?
068000120717               chain  (TASaas : TASlnp : TASnrs : TASnsp)  FNLBL000;
068100120717               if  Not %found(FNLBL01L);
068200120717                 // -?La tengo solo se ha particolarità giusta?
068300120717                 IndX = %lookup( tasgma : sk_7R );
068400120717                 if  IndX > *zero  and  TASgma <> *blank;
068500120717                   if ContaBolle < 99;
068600120717                     ContaBolle += 1;
068700120717                     // -?Mi salvo se la bolla?
068800120717                     if s_TASaas = *zero;
068900120717                       s_TASaas = TASaas;
069000120717                       s_TASmgs = TASmgs;
069100120717                       s_TASlnp = TASlnp;
069200120717                       s_TASnrs = TASnrs;
069300120717                       s_TASnsp = TASnsp;
069400120720                       s_TASccm = TASccm;
069500120717                       if  *in86 = *on;
069600120717                         wTitasP = 'S';
069700120717                       endif;
069800120717                     endif;
069900120717                   endif;
070000120717                 endif;
070100120717               endif;
070200120717
070300120717               *in84 = *off;
070400120717               *in85 = *off;
070500120717               *in86 = *off;
070600120717               reade  (V1Crmn : V1Cksc)  TITAS32C;
070700120717
070800120717             EndDo;
070900120717
071000120717             if  ContaBolle <> 1;
071100120717               errSPE = *on;
071200120717               if  ContaBolle = *zero;
071300120717                 V1Cmsg = Msg(06);
071400120717               else      ;
071500120717                 V1Cmsg = Msg(05);
071600120717                 %subst( V1Cmsg : 40 : 2 ) = %char(ContaBolle);
071700120717               endif    ;
071800120717               errMessage  = *on;
071900120717               errGenerico = *on;
072000120717               leavesr;
072100120717             endif;
072200120717
072300120717             // -?Se la bolla è in TITASP non accetto più l'inserimento?
072400120717             if  wTitasP = 'S';
072500120717               errSPE = *on;
072600120717               V1Cmsg = Msg(15);
072700120717               errMessage  = *on;
072800120717               errGenerico = *on;
072900120717               leavesr;
073000120717             endif;
073100120717
073200120717           // -?Immissione per riferimenti mittente ALFANUMERICO?
073300120717           WHEN VisAlfa = *on;
073400120717             if V1Crma = *blank;
073500120717               errSPE = *on;
073600120717               V1Cmsg = Msg(07);
073700120717               errMessage  = *on;
073800120717               errGenerico = *on;
073900120717               leavesr;
074000120717             endif;
074100120717
074200120717             setll  (V1Crma)  TITA435C;
074300120717             reade  (V1Crma)  TITA435C;
074400120717             DoW  Not %eof(TITA435C);
074500120717
074600120717               *in81 = *off  ;
074700120717               *in82 = *off  ;
074800120717               *in83 = *off  ;
074900120717               chain  (TA4aas : TA4lnp : TA4nrs : TA4nsp)  TITAS30C;
075000120717               if  %found(TITAS30C)  and
075100120717                   TASccm = V1Cksc   and  TASgma <> *blank;
075200120717
075300120717                 // -?Escludo se bolla figlia?
075400120717                 chain  (TASaas : TASlnp : TASnrs : TASnsp)  FNLBL000;
075500120717                 if  Not %found(FNLBL01L);
075600120717
075700120717                   // -?La tengo solo se ha particolarità giusta?
075800120717                   IndX = %lookup( TASgma : sk_7R );
075900120717                   if  Indx > *zero;
076000120717                     if  ContaBolle < 99;
076100120717                       ContaBolle += 1;
076200120717                       // -?Memorizzo la prima bolla che trovo valida?
076300120717                       if  s_TASaas = *zero;
076400120717                         s_TASaas = TASaas;
076500120717                         s_TASmgs = TASmgs;
076600120717                         s_TASlnp = TASlnp;
076700120717                         s_TASnrs = TASnrs;
076800120717                         s_TASnsp = TASnsp;
076900120720                         s_TASccm = TASccm;
077000120717                         if  *in83 = *on;
077100120717                           wTitasP = 'S';
077200120717                         endif;
077300120717                       endif;
077400120717                     endif;
077500120717                   endif;
077600120717                 endif;
077700120717               endif;
077800120717
077900120717               reade  (V1Crma)  TITA435C;
078000120717
078100120717             EndDo;
078200120717
078300120717             if  ContaBolle <> 1;
078400120717               errSPE = *on;
078500120717               if  ContaBolle = *zero;
078600120717                 V1Cmsg = Msg(06);
078700120717               else;
078800120717                 V1Cmsg = Msg(08);
078900120717                 %subst( V1Cmsg : 42 : 2 ) = %char(ContaBolle);
079000120717               endif;
079100120717               errMessage  = *on;
079200120717               errGenerico = *on;
079300120717               leavesr;
079400120717             endif;
079500120717
079600120717             // -?Se la bolla è in TITASP non accetto più l'inserimento?
079700120717             if wTitasP = 'S';
079800120717               errSPE = *on;
079900120717               v1cmsg = Msg(15);
080000120717               errMessage  = *on;
080100120717               errGenerico = *on;
080200120717               leavesr;
080300120717             endif;
080400120717
080500120717           OTHER;
080600120717             // -?Immissione per numero di spedizione?
080700120717             if  (V1Cnsp = *zero  and  V1Cpis =  *zero )  or
080800120717                 (V1Cnsp > *zero  and  V1Cpis <> *zero );
080900120717               errSPE = *on;
081000120717               V1Cmsg = Msg(10);
081100120717               errMessage  = *on;
081200120717               errGenerico = *on;
081300120717               leavesr;
081400120717             endif;
081500120717
081600120717             // -?Se pieno numero di spedizione --> uso quello,?
081700120717             //  ?altrimenti il barcode?
081800120717             clear  ds_pis;
081900120717             if  V1Cpis <> *zero;
082000120717               ds_pis = %subst( %editc(V1Cpis : 'X') : 4 : 15 );
082100120717               // -?Controllo chkdigit?
082200120717               //BarCode = %editc( V1Cpis : 'X');
082300120717               BarCode = %subst( %editc( V1Cpis : 'X' ) : 4 : 15 );
082400120717               clear  TRUL28ds;
082500120717               i28mod = 'BAR';
082600120717               i28cod = BarCode;
082700120717               callp  TRUL28R1  (trul28ds);
082800120717               if  O28err <> *blank  or  O28cod <> ds_pis;
082900120717                 errBAR7 = *on;
083000120717                 errGenerico = *on;
083100120717                 leavesr;
083200120717               endif;
083300120717             endif;
083400120717
083500120717             *in81 = *off;
083600120717             *in82 = *off;
083700120717             *in83 = *off;
083800120717             if V1Cnsp > *zero;
083900120717               chain  (V1Caas : V1Clnp : V1Cnrs : V1Cnsp)  TITAS30C;
084000120717             else;
084100120717               Kanno = (Secolo*100) + ds_pisAAS;
084200120717               chain  (kanno : ds_pisLNP : ds_pisNRS : ds_pisNSP)  TITAS30C;
084300120717             endif;
084400120717
084500120717             if  Not  %found(TITAS30C);
084600120717               if  V1Cnsp > *zero;
084700120717                 errSPE = *on;
084800120717                 V1Cmsg = Msg(11);
084900120717                 ErrMessage  = *on;
085000120717               else;
085100120717                 errBAR1=*on;
085200120717               endif;
085300120717               errGenerico = *on;
085400120717               leavesr;
085500120717             else;
085600120717               if  TASccm <> V1Cksc;
085700120717                 if  V1Cnsp > *zero;
085800120717                   errSPE = *on;
085900120717                   V1Cmsg = Msg(12);
086000120717                   errMessage  = *on;
086100120717                 else;
086200120717                   errBAR2=*on;
086300120717                 endif;
086400120717                 errGenerico = *on;
086500120717                 leavesr;
086600120717               endif;
086700120717
086800120717               // -?Se la bolla non ha una particolarità che prevede la?
086900120717               //  ?restituzione del DDT --> errore?
087000120717               IndX = %lookup( TASgma : sk_7R);
087100120717               if  IndX = *zero  or  TASgma = *blank;
087200120717               if  V1Cnsp > *zero;
087300120717                 errSPE = *on;
087400120717                 V1Cmsg = Msg(14);
087500120717                 errMessage  = *on;
087600120717               else;
087700120717                 errBAR3=*on;
087800120717               endif  ;
087900120717               errGenerico = *on;
088000120717               leavesr;
088100120717             endif;
088200120717
088300120717             // -?Se la bolla è figlia errore?
088400120717             chain  (TASaas : TASlnp : TASnrs : TASnsp )  FNLBL000;
088500120717             If  %found(FNLBL01L);
088600120717               if  V1Cnsp > *zero;
088700120717                 errSPE = *on;
088800120717                 V1Cmsg = Msg(16);
088900120717                   errMessage  = *on;
089000120717                 else;
089100120717                   errBAR4 = *on;
089200120717                 endif;
089300120717                 errGenerico = *on;
089400120717                 leavesr;
089500120717               endif;
089600120717               // -?Se la bolla è in TITASP non accetto più l'inserimento?
089700120717               if  *in83 = *on;
089800120717                 if V1Cnsp > *zero;
089900120717                   errSPE = *on;
090000120717                   V1Cmsg = Msg(15);
090100120717                   errMessage  = *on;
090200120717                 else;
090300120717                   errBAR5 = *on;
090400120717                 endif;
090500120717                 errGenerico = *on;
090600120717                 leavesr;
090700120717               endif;
090800120717             EndIf;
090900120717
091000120717             // salvo il numero di spedizione
091100120717             s_TASaas = TASaas;
091200120717             s_TASmgs = TASmgs;
091300120717             s_TASlnp = TASlnp;
091400120717             s_TASnrs = TASnrs;
091500120717             s_TASnsp = TASnsp;
091600120720             s_TASccm = TASccm;
091700120717             if  *in83 = *on;
091800120717               wTitasP = 'S';
091900120717             endif;
092000120717
092100120717         ENDSL;
092200120717
092300120717         // -?Trovata la spedizione, avviso se già aggiornata in altra data?
092400120717         IF  s_TASaas <> w5aas  or  s_TASlnp <> w5lnp  or
092500120717             s_TASnrs <> w5nrs  or  s_TASnsp <> w5nsp;
092600120717           w5aas = s_TASaas;
092700120717           w5lnp = s_TASlnp;
092800120717           w5nrs = s_TASnrs;
092900120717           w5nsp = s_TASnsp;
093000120717
093100120717           if  IndManut = *off;
093200120717             chain  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'GEN')
093300120717                    FIAR531C;
093400120717           else;
093500120717             chain(n)  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'GEN')
093600120717                       FIAR531C;
093700120717           endif;
093800120717
093900120717           IF  %found(FIAR531C);
094000120717
094100120717             dAR5gen = AR5uni;
094200120717             DataIso = %date;
094300120717
094400120717             // -?Avviso se già aggiornata in altra data --> modo Immissione?
094500120717             If  IndManut = *off  ;
094600120717
094700120717               if  §AR5drdt <> *blank  and  §AR5drdt <> '99999999';
094800120717                 if  VisNumer = *off  and  VisAlfa = *off  and  V1Cnsp = *zero;
094900120717                   errBAR6 = *on;
095000120717                 else  ;
095100120717                   errSPE  = *on;
095200120717                   V1Cmsg  = Msg(13);
095300120717                   DataEur = %date( %int( §AR5drdt ) : *iso );
095400120717                   %subst(V1Cmsg : 36 : 10) =
095500120717                     %editw( %dec( dataeur ) : '  /  /    ' );
095600120717                   errMessage  = *on;
095700120717                 endif;
095800120717                 errGenerico = *on;
095900120717                 leavesr;
096000120717               endif;
096100120717
096200120717             Else;
096300120717
096400120717               // -?In manutenzione imposto la data?
096500120717               if  §AR5drdt <> *blank  and  §AR5drdt <> '99999999';
096600120717                 monitor;
096700120717                   DataIso = %date( %int( §AR5drdt ) );
096800120717                 on-error;
096900120717                   clear  DataIso;
097000120717                 endmon;
097100120717               else;
097200120717                 clear  DataIso;
097300120717               endif;
097400120717               if  DataIso > *loval;
097500120717                 DataEur = DataIso;
097600120717                 V1CDrdt = %dec(DataEur);
097700120717               endif;
097800120717
097900120717               // -?Data inserimento motivo?
098000120717               if §AR5dimtv <> *blank;
098100120717                 monitor;
098200120717                   DataIso = %date( %int( §AR5dimtv ) );
098300120717                 on-error;
098400120717                   clear  DataIso;
098500120717                 endmon;
098600120717               else;
098700120717                 clear  DataIso;
098800120717               endif;
098900120717               if  DataIso > *loval;
099000120717                 DataEur  = DataIso;
099100120717                 V1CDimtv = %dec(DataEur);
099200120717               endif;
099300120717
099400120717               // -?Codice motivo?
099500120717               if  §AR5mtvdt <> *blank;
099600120717                 V1Cmtvdt = %subst( §AR5mtvdt : 4 : 5 );
099700120717                 V1Cfil   = %int( %subst( §AR5mtvdt : 1 : 3 ) );
099800120717                 clear  TBEke1;
099900120717                 clear  tbeke2;
100000120717                 TBEke1 = %editc( V1Cfil : 'X' );
100100120717                 TBEke2 = V1Cmtvdt;
100200120717                 chain  ('MDE' : TBEke1 : TBEke2)  TNTBE000;
100300120717                 if  %found(TNTBE01L);
100400120717                   V1Dmtvdt = TBEuni;
100500120717                 endif  ;
100600120717               endif  ;
100700120717
100800120717             EndIf;
100900120717
101000120717           ENDIF;
101100120717
101200120717         ENDIF;
101300120717
101400120717         // -?Nessun errore => Se manutenzione emetto 3° videata?
101500120717         IF  IndManut = *on;
101600120717
101700120717           $Video = 'D3';
101800120717
101900120717         ELSE;
102000120717
102100120717           // -?Trovata la spedizione, avviso se presente motivo che?
102200120717           //  ?verrà tolto?
102300120717           If  §AR5mtvdt <> *blank  and
102400120717              (s_TASaas <> b_W5aas  or  s_TASlnp <> b_W5lnp  or
102500120717               s_TASnrs <> b_W5nrs  or  s_TASnsp <> b_W5nsp);
102600120717             b_W5aas = s_TASaas;
102700120717             b_W5lnp = s_TASlnp;
102800120717             b_W5nrs = s_TASnrs;
102900120717             b_W5nsp = S_TASnsp;
103000120717             if  VisNumer = *off  and  VisAlfa = *off  and  V1Cnsp = *zero;
103100120717               errBAR8 = *on;
103200120717             else  ;
103300120717               V1Cmsg = Msg(19);
103400120717               %subst( V1Cmsg : 35 : 8 ) = §AR5mtvdt;
103500120717               errSPE = *on;
103600120717               errMessage  = *on;
103700120717             endif  ;
103800120717             errGenerico = *on;
103900120717             leavesr;
104000120717           EndIf;
104100120717
104200120717           // -?Se nessun errore: aggiorno con Enter FIAR5 e contatore?
104300120717           If  Not %found(FIAR531C)  or
104400120717               §AR5drdt <> %char( %dec( DataIso ) );
104500120717
104600120717             if  Not %found(FIAR531C);
104700120717               exsr  ImpostaAR5;
104800120717             else;
104900120717               dAR5gen = AR5uni;
105000120717             endif;
105100120717
105200120717             DataIso  = %date;
105300120717             §AR5drdt = %char( %dec( DataIso ) );
105400120717             clear  §AR5fstd;
105500120717             clear  §AR5mtvdt;
105600120717             clear  §AR5dimtv;
105700120717             AR5uni = dAR5gen;
105800120717
105900120717             exsr  UpdateAR5;
106000120717
106100120717             // -?Totali bolle aggiornate?
106200120717             V1Ctot  += 1;
106300120717             VisTotBo = *on;
106400120717
106500120717             // -?Stampo LDV?
106600120719             //  ?Deciso da LB:?
106700120719             //  ?la stampa della bolla la si fa comunque?
106800120719             //  ?(SE NON già valorizzato dAR5doc.§AR5jfid)?
106900120719             //exsr  sr_StampaLDV;
107000120717
107100120717           Else;
107200120717
107300120717             unlock  FIAR531C;
107400120717
107500120717           EndIf;
107600120719
107700120719           // -?Stampo LDV?
107800120719           exsr  sr_StampaLDV;
107900120717
108000120717           exsr  sr_InzParz02;
108100120717
108200120717         ENDIF;
108300120717
108400120717
108500120717       ENDSR;
108600120717
108700120320       //--------------------------------------------------------------
108800120625       //?Gestione video 3
108900120320       //--------------------------------------------------------------
109000120717       BEGSR  sr_GesD03;
109100120320
109200120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
109300120718         AbilitF12 = *on;
109400120718         AbilitF16 = *on;
109500120320         if  errGenerico = *off;
109600120717           write  LSV1T01;
109700120717           write  LSV1Z01;
109800120717           write  LSV1D01;
109900120717           write  LSV1D02;
110000120717           write  Protect;
110100120320         endif;
110200120320
110300120717         // -?Emissione videata?
110400120717         exfmt  LSV1D03;
110500120328
110600120320         reset errMessage;
110700120320         reset errGenerico;
110800120320         clear V1cmsg;
110900120320
111000120717         SELECT;
111100120320
111200120717           // -?F3=Fine?
111300120717           WHEN  dsp_aid = c_F03;
111400120717             $Fine = *on;
111500120329
111600120717           // -?F8=Gestione motivi errati documentazione?
111700120717           WHEN  dsp_aid = c_F08;
111800120717             clear  TNTB92ds  ;
111900120717             B92ke1 = %editc( DUTpou : 'X' );
112000120717             kpjbu = TNTB92ds;
112100120717             callp  TNTB92R ( kpjba );
112200120320
112300120717           // -?F12=Ritorno?
112400120717           WHEN  dsp_aid = c_F12;
112500120717             $Video = 'D2';
112600120717             exsr  sr_InzParz02;
112700120328
112800120717           // -?F16=Elimino data inventario documenti se presente?
112900120717           WHEN  dsp_aid = c_F16;
113000120717             exsr  Annuldata  ;
113100120320
113200120717           // -?Invio o F6=Conferma?
113300120320           OTHER;
113400120717             exsr  sr_ContrD03 ;
113500120717             if  errGenerico = *on;
113600120320               leavesr;
113700120717             endif;
113800120717             if  dsp_aid = c_F06;
113900120717               exsr  AggioMotivo;
114000120717               $Video = 'D2';
114100120717             endif;
114200120320
114300120717         ENDSL;
114400120320
114500120320       ENDSR;
114600120717
114700120717       //--------------------------------------------------------------
114800120717       //?Controlli video 3
114900120717       //--------------------------------------------------------------
115000120717       BEGSR  sr_ContrD03  ;
115100120717
115200120717         ErrDrdt = *off;
115300120717         ErrMTV  = *off;
115400120717
115500120717         clear  V1Dmtvdt;
115600120717
115700120717         // -?Verifico '?' sulla causale?
115800120717         IndX = %scan( '?' : V1Cmtvdt );
115900120717         if  IndX > *zero;
116000120717           clear  V1Cmtvdt;
116100120717           clear  TNTB92ds;
116200120717           B92fun = '1';
116300120717           B92ke1 = %editc( V1Cfil : 'X' );
116400120717           kpjbu  = TNTB92ds;
116500120717           callp  TNTB92R  (kpjba);
116600120717           TNTB92ds = kpjbu;
116700120717           if B92ke2 <> *blank;
116800120717             V1Cmtvdt = B92ke2;
116900120717             V1Dmtvdt = B92des;
117000120717           endif;
117100120717         endif;
117200120717
117300120717         // -?Controllo causale se immessa?
117400120717         if  V1Cmtvdt <> *blank;
117500120717           clear  TBEke1;
117600120717           clear  TBEke2;
117700120717           TBEke1 = %editc( V1Cfil : 'X' );
117800120717           TBEke2 = V1Cmtvdt;
117900120717           chain  ('MDE' : TBEke1 : TBEke2)  TNTBE000;
118000120717           if  Not %found(TNTBE01L)  or  TBEatb <> *blank;
118100120717             errMTV = *on;
118200120717             V1Cmsg = Msg(18);
118300120717             errMessage  = *on;
118400120717             errGenerico = *on;
118500120717             leavesr;
118600120717           endif;
118700120717           V1Dmtvdt = TBEuni;
118800120717         endif  ;
118900120717
119000120717         // -?Se tolto MOTIVO => AZZERO LA DATA?
119100120717         if  V1Cmtvdt = *blAnk;
119200120717           clear  V1Cdimtv;
119300120717           V1Cfil = DUTpou;
119400120717         else;
119500120717           // -?Se motivo = a quello del file stessa data?
119600120717           if  V1Cmtvdt = %subst( §AR5mtvdt : 4 : 5 )  and
119700120717               §AR5dimtv > *zero;
119800120717             DataIso = %date( %int( §AR5dimtv ) );
119900120717           else;
120000120717             DataIso = %date;
120100120717           endif;
120200120717           DataEur  = DataIso;
120300120717           V1CDimtv = %dec(DataEur);
120400120717         endif;
120500120717
120600120717       ENDSR;
120700120717
120800120328       //--------------------------------------------------------------
120900120328       //?F16 - eliminazione data inventario documenti
121000120328       //--------------------------------------------------------------
121100120328       BEGSR  Annuldata   ;
121200120329
121300120328       // se data non presente errore
121400120329 1         if v1cdrdt=0  ;
121500120328           errdrdt=*on;
121600120328           v1cmsg = Msg(09);
121700120328           errMessage  = *on;
121800120328           errGenerico = *on;
121900120328           leavesr;
122000120329 1         endif  ;
122100120328
122200120328       // Updato pulendo la data   ;
122300120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
122400120329 1     if %found(fiar531c)  ;
122500120328       dar5gen=ar5uni           ;
122600120328
122700120330       §ar5drdt='99999999'      ;
122800120328       clear §ar5fstd           ;
122900120328       ar5uni=dar5gen           ;
123000120329
123100120329 2       if wtitasp='S'  ;
123200120329          update fiar5p00           ;
123300120329           else    ;
123400120329          update fiar5000           ;
123500120329 2     endif    ;
123600120328
123700120328 1     endif    ;
123800120329
123900120329       clear v1cdrdt   ;
124000120328
124100120328       ENDSR  ;
124200120328       //--------------------------------------------------------------
124300120328       //?F06 - Aggiorno motivo ddt errati
124400120328       //--------------------------------------------------------------
124500120328       BEGSR  Aggiomotivo ;
124600120328
124700120402 1     if §ar5mtvdt  <> %editc(v1cfil:'X')+v1cmtvdt   ;
124800120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
124900120328
125000120402 2     if not %found(fiar531c)  ;
125100120328       exsr ImpostaAR5          ;
125200120402 x2    else                     ;
125300120328       dar5gen=ar5uni           ;
125400120402 2     endif                    ;
125500120328
125600120402        if v1cmtvdt= *blanks  ;
125700120329         clear §ar5dimtv  ;
125800120402         clear §ar5mtvdt  ;
125900120329        else   ;
126000120402         §ar5mtvdt=%editc(v1cfil:'X') +v1cmtvdt   ;
126100120329         dataiso=%date             ;
126200120329         §ar5dimtv=%char(%dec(dataiso))   ;
126300120329        endif    ;
126400120329
126500120329       ar5uni=dar5gen  ;
126600120328
126700120329       exsr UpdateAR5  ;
126800120329
126900120402       endif    ;
127000120328
127100120717       exsr sr_InzParz02 ;
127200120328
127300120328       ENDSR  ;
127400120328       //--------------------------------------------------------------
127500120328       //?Imposta campi FIar5 per la write
127600120328       //--------------------------------------------------------------
127700120328       BEGSR ImpostaAR5         ;
127800120328
127900120328       clear fiar5000           ;
128000120328       clear fiar5p00           ;
128100120328       ar5aas=s_tasaas          ;
128200120328       ar5lnp=s_taslnp          ;
128300120328       ar5nrs=s_tasnrs          ;
128400120328       ar5nsp=s_tasnsp          ;
128500120328       ar5trd='GEN'             ;
128600120328       dataiso=%date             ;
128700120328       ar5dac=%dec(dataiso)     ;
128800120328       timeiso=%time             ;
128900120328       ar5orc=%dec(timeiso)     ;
129000120328       ar5ft1='R'               ;
129100120328       ar5dt1=%dec(dataiso)     ;
129200120328       ar5ft2='R'               ;
129300120328       ar5dt2=%dec(dataiso)     ;
129400120328       ar5ft3='R'               ;
129500120328       ar5dt3=%dec(dataiso)     ;
129600120328
129700120328       clear dar5gen            ;
129800120328       ENDSR                   ;
129900120329       //--------------------------------------------------------------
130000120329       //?Aggiorno record FIAR5
130100120329       //--------------------------------------------------------------
130200120329       BEGSR  UpdateAR5  ;
130300120329
130400120329 2     if not %found(fiar531c)  ;
130500120329 3       if wtitasp='S'  ;
130600120329         write  fiar5p00           ;
130700120329          else    ;
130800120329         write  fiar5000           ;
130900120329 3       endif  ;
131000120329
131100120329 x2    else   ;
131200120329 3       if wtitasp='S'  ;
131300120329          update fiar5p00           ;
131400120329           else    ;
131500120329          update fiar5000           ;
131600120329 3     endif    ;
131700120329 2     endif    ;
131800120329
131900120329       ENDSR  ;
132000120717
132100120717       //--------------------------------------------------------------
132200120717       //?Stampa copia LdV x Scannerizzazione Documenti Clienti       ?
132300120717       //--------------------------------------------------------------
132400120717       BEGSR  sr_StampaLDV;
132500120717
132600120717         // -?Aggancio tabella "JDC" per reperimento data attivazione?
132700120717         //  ?della scannerizzazione documenti cliente?
132800120717         clear  dJDC;
132900120717         clear  k05tntbe01;
133000120717         k_TBEcod = 'JDC';
133100120719         // -?NO => TASCCM se valorizzato, TASKSC altrimenti?
133200120719         // -?SÌ => TASCCM sempre e comunque?
133300120720         //if  s_TASccm <> *zero;
133400120720           k_TBEke1 = %editc( s_TASccm : 'X' );
133500120719         //else;
133600120720         //  k_TBEke1 = %editc( s_TASksc : 'X' );
133700120719         //endif;
133800120717
133900120717         chain  %kds( k05tntbe01 : 3 )  TNTBE00S;
134000120717
134100120717         if  %found(TNTBE01S);
134200120717           dJDC = TBEuni;
134300120717         endif;
134400120717
134500120717         // -?Scannerizzazione documenti per il cliente: NON prevista?
134600120717         //  ?o non ancora in vigore o già scaduta?
134700120717         if  NOT  %found(TNTBE01S)                or
134800120717             §JDCdti > (s_TASaas * 10000) + s_TASmgs  or
134900120717             §JDCdtf < (s_TASaas * 10000) + s_TASmgs;
135000120717           leavesr;
135100120717         endif;
135200120717
135300120717         // -?Reperimento rec. "DOC" da FIAR5?
135400120717         clear  dAR5doc;
135500120717
135600120717         chain(n)  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'DOC')
135700120717                   FIAR531C;
135800120717
135900120717         if  %found(FIAR531C);
136000120717           dAR5doc = AR5uni;
136100120717         endif;
136200120717
136300120717         // -?Documenti già inviati al mittente?
136400120717         if  dAR5doc.§AR5jfid <> *blank;
136500120717           leavesr;
136600120717         endif;
136700120717
136800120717         // -?Stampa copia LdV?
136900120717         reset  TNSB25ds;
137000120717         //D25ges = 'T'       ?(già così)?
137100120719         D25aas = s_TASaas;
137200120719         D25lnp = s_TASlnp;
137300120719         D25nrs = s_TASnrs;
137400120719         D25nsp = s_TASnsp;
137500120717
137600120717         tnsb25r ( TNSB25ds );
137700120717
137800120720         V1Ctst  += 1;
137900120720         VisTotBo = *on;
138000120717
138100120717       ENDSR;
138200120717
138300120329       //--------------------------------------------------------------
138400120329       //?Pulizia parziale campoi video 2
138500120329       //--------------------------------------------------------------
138600120717       BEGSR  sr_InzParz02;
138700120329
138800120717         clear  V1Crmn;
138900120717         clear  V1Crma;
139000120717         clear  V1Cnsp;
139100120717         clear  V1Cpis;
139200120717
139300120717         clear  W5aas;
139400120717         clear  W5lnp;
139500120717         clear  W5nrs;
139600120717         clear  W5nsp;
139700120717         clear  b_W5aas;
139800120717         clear  b_W5lnp;
139900120717         clear  b_W5nrs;
140000120717         clear  b_W5nsp;
140100120717
140200120717         clear  V1Cdrdt;
140300120717         clear  V1Cmtvdt;
140400120717         clear  V1Cdimtv;
140500120717         V1Cfil = DUTpou;
140600120717
140700120329       ENDSR  ;
140800120717
140900120717       //--------------------------------------------------------------
141000120717       //?Operazioni finali.
141100120717       //--------------------------------------------------------------
141200120717       BEGSR  sr_RoutEnd;
141300120717
141400120717         *inLR = *on;
141500120717
141600120717         return;
141700120717
141800120717       ENDSR;
141900080807
142000080807      /end-free
142100080807
142200120717** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
142300080826Immettere il codice Cliente Mittente                                           1
142400080826Codice Cliente Mittente inesistente                                            2
142500080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
142600080826Immettere il riferimento mittente NUMERICO                                     4
142700080826Per il rif. Mittente NUMERICO esistono xx bolle: inserire col numero di Spediz 5
142800120717Nessuna bolla trovata per il cliente con particolarità restituzione documenti  6
142900080826Immettere il riferimento mittente ALFANUMERICO                                 7
143000080826Per il RifMittente ALFANUMERICO esistono xx bolle:inserire col numero di Sped  8
143100120328Impossibile annullare : data non presente!                                     9
143200110520Immettere il numero di SPEDIZIONE  o il Barcode, in alternativa               10
143300080826Numero di SPEDIZIONE inesistente                                               1
143400080826La SPEDIZIONE non appartiene al cliente mittente indicato                      2
143500120521Già presente data ricezione doc.al xx/xx/xxxx:ENTER per forzare e riaggiornare 3
143600080909La SPEDIZIONE non prevede particolarità consegna con restituzione documenti    4
143700080901La spedizione è ormai troppo vecchia: impossibile eseguire l'inventario        5
143800120521Spedizione con legame bolla:inserire ricezione documenti sulla bolla originale 6
143900120717Errore nel Barcode                                                    (LIBERO) 7
144000120330Causale motivo inesistente                                                     8
144100120521Presente motivo documenti errati "xxxxxxxx": verrà eliminato                   9
