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
004200120822     fFNLBL02L  if   e           k disk    rename(fnlbl000:fnlblpre)
004300120717       // -?Dati di Bolle SEDE?
004400120717     fTITAS30C  if   e           k disk    extfile(wTAS30) usropn
004500120717     fTITAS32C  if   e           k disk    extfile(wTAS32) usropn
004600120717     f                                     rename(TITAS000:TITAS002)
004700120717     f                                     rename(TITAS010:TITAS012)
004800120717     f                                     rename(TITASP00:TITASP02)
004900120717     fTITA435C  if   e           k disk    extfile(wTA435) usropn
005000120717     fFIAR531C  Uf A e           k disk    extfile(wFIAR5) usropn
005100120717       // -?File Video?
005200120717     fFNLSV1D   cf   e             workstn indds(IndDspF)
005300120717     f                                     infds(InfDspF)
005400120717
005500120717       //--------------------------------------------------------------
005600120717       //?Definizione costanti.                                        ?
005700120717       //--------------------------------------------------------------
005800120717
005900120717       // -?Tasti funzionali a video?
006000080807     d c_F01           c                   const(x'31')
006100080807     d c_F02           c                   const(x'32')
006200080807     d c_F03           c                   const(x'33')
006300120717     d c_F04           c                   const(x'34')
006400080807     d c_F05           c                   const(x'35')
006500080807     d c_F06           c                   const(x'36')
006600080807     d c_F07           c                   const(x'37')
006700080807     d c_F08           c                   const(x'38')
006800080807     d c_F09           c                   const(x'39')
006900080807     d c_F10           c                   const(x'3A')
007000120717     d c_F11           c                   const(x'3B')
007100080807     d c_F12           c                   const(x'3C')
007200080807     d c_F13           c                   const(x'B1')
007300080807     d c_F14           c                   const(x'B2')
007400080807     d c_F15           c                   const(x'B3')
007500080807     d c_F16           c                   const(x'B4')
007600080807     d c_F17           c                   const(x'B5')
007700080807     d c_F18           c                   const(x'B6')
007800080807     d c_F19           c                   const(x'B7')
007900080807     d c_F20           c                   const(x'B8')
008000080807     d c_F21           c                   const(x'B9')
008100080807     d c_F22           c                   const(x'BA')
008200080807     d c_F23           c                   const(x'BB')
008300080807     d c_F24           c                   const(x'BC')
008400120717     d c_Enter         c                   const(x'F1')
008500120717     d c_RollDown      c                   const(x'F4')
008600120717     d c_RollUp        c                   const(x'F5')
008700120717
008800120717       //--------------------------------------------------------------
008900120717       //?Definizione schiere.                                         ?
009000120717       //--------------------------------------------------------------
009100120717
009200120717       // -?Messaggi di errore?
009300120717     d Msg             s             78    dim(19)  ctdata  perrcd(1)
009400120717
009500120717       // -?Particolarità consegne che prevedono la restituzione della?
009600120717       //  ?BAM?
009700120717     d Sk_7R           s              2    dim(50)
009800120717
009900120717       //--------------------------------------------------------------
010000120717       //?Definizione aree dati.                                       ?
010100120717       //--------------------------------------------------------------
010200120717
010300120717       // -?Dati utente?
010400120717     d §AzUte        e ds                  extname(AZUTE00F)
010500120717     d                                     dtaara
010600120717     d §DatiUte      e ds                  extname(dDatiUte)
010700120717     d                                     dtaara
010800120717
010900120717       //--------------------------------------------------------------
011000120717       //?Definizione strutture dati.                                  ?
011100120717       //--------------------------------------------------------------
011200120717
011300120717       // -?InfDS?
011400080807     d InfDspF         ds
011500120717     d   dsp_aid             369    369a
011600120717     d*//sfl_rrn             376    377i 0
011700120717     d*//min_nrr             378    379i 0
011800120717     d*//num_rcds            380    381i 0
011900080807
012000120717       // -?Indicatori su DspF?
012100080807     d IndDspF         ds
012200120717         // -?Indicatori per Attibuti di visualizzazione?
012300120717     d   AbilitF12                     n   overlay(IndDspF : 01)
012400120717     d   VisTotBo                      n   overlay(IndDspF : 02)
012500120718     d   AbilitF16                     n   overlay(IndDspF : 03)
012600120717     d   VisNumer                      n   overlay(IndDspF : 05)
012700120717     d   VisAlfa                       n   overlay(IndDspF : 06)
012800120717     d   VisSped                       n   overlay(IndDspF : 07)
012900120717     d   PosCurs                       n   overlay(IndDspF : 08)
013000120717     d   IndManut                      n   overlay(IndDspF : 10)
013100120717         // -?Emissione messaggio di errore?
013200120717     d   errMessage                    n   overlay(IndDspF : 28)
013300120717         // -?Posizionamento cursore & Segnalazione errore?
013400120717     d   errBar1                       n   overlay(IndDspF : 40)
013500120717     d   errBar2                       n   overlay(IndDspF : 41)
013600120717     d   errBar3                       n   overlay(IndDspF : 42)
013700120717     d   errBar4                       n   overlay(IndDspF : 43)
013800120717     d   errBar5                       n   overlay(IndDspF : 44)
013900120717     d   errBar6                       n   overlay(IndDspF : 45)
014000120717     d   errBar7                       n   overlay(IndDspF : 46)
014100120717     d   errBar8                       n   overlay(IndDspF : 47)
014200120717     d   errKSC                        n   overlay(IndDspF : 50)
014300120717     d   errSPE                        n   overlay(IndDspF : 51)
014400120717     d   errBAR                        n   overlay(IndDspF : 52)
014500120717     d   errDRDT                       n   overlay(IndDspF : 53)
014600120717     d   errMTV                        n   overlay(IndDspF : 54)
014700120717         //   -?Riemissione videata?
014800120717     d   errGenerico                   n   overlay(IndDspF : 99)
014900080807
015000120717       // -?Struttura dati per sottocampare il barcode della spedizione?
015100120717     d ds_pis          ds                  inz
015200120717     d   ds_pisAAS                    2  0 inz
015300120717     d   ds_pisLNP                    3  0 inz
015400120717     d   ds_pisNRS                    2  0 inz
015500120717     d   ds_pisNSP                    7  0 inz
015600120717     d   ds_ChkDgt                    1  0 inz
015700120717
015800120717       // -?Parametri ricevuti?
015900120717     d KPJBA         e ds
016000120717
016100120717       // -?Tabella "7N" = Categorie Fogli?
016200120717     d ds7N          e ds                  inz
016300120717       // -?Tabella "7R" = Particolarità Consegne?
016400120717     d ds7R          e ds                  inz
016500120717
016600120717       // -?Tabella "JDC" = Clienti per ritorno immagini DOC?
016700120717     d dJDC          e ds                  inz
016800120717
016900120717       // -?Dati FIAR5 / trk "GEN"?
017000120717     d dAR5gen       e ds                  inz
017100120717       // -?Dati FIAR5 / trk "DOC"?
017200120717     d dAR5doc       e ds                  inz  qualified
017300120717
017400120717       //--------------------------------------------------------------
017500120717       //?Definizione variabili globali.                               ?
017600120717       //--------------------------------------------------------------
017700120717
017800120717       // -?Flags booleani?
017900120717     d $Fine           s               n   inz(*off)
018000120717     d $Inzd01         s               n   inz(*on)
018100120717     d $Inzd02         s               n   inz(*on)
018200120717     d wTitasP         s              1
018300120717
018400120717       // -?Indici di schiera?
018500120717     d xx              s              3  0 inz
018600120717     d IndX            s              3  0 inz
018700120717
018800120717       // -?Variabili per la gestione del video?
018900120717     D $Video          s              2    inz('D1')
019000120717
019100120717       // -?Nome esteso Libreria/File degli archivi di Sede?
019200120717     d wTBE1S          s             21    inz('GAITRAGRU /TNTBE01L')
019300120717     d wTAS30          s             21    inz('GAITRAGRU /TITAS30C')
019400120717     d wTAS32          s             21    inz('GAITRAGRU /TITAS32C')
019500120717     d wTA435          s             21    inz('GAITRAGRU /TITA435C')
019600120717     d wFIAR5          s             21    inz('GAITRAGRU /FIAR531C')
019700120717
019800120717       // -?Campi "data"?
019900120717     d DataSys         s               d   datfmt(*iso) inz(*sys)
020000120717     d DataIso         s               d   datfmt(*iso)
020100120717     d TimeIso         s               t   timfmt(*iso)
020200120717     d DataEur         s               d   datfmt(*eur)
020300120717
020400120717       // -?Campi di comodo?
020500120822     d wtrov           s              1
020600120822     d ParManut        s              1
020700120717     d w007a           s              7
020800120717     d BarCode         s             14
020900120717     d Secolo          s              2  0
021000110520     d Kanno           s              4  0
021100120717     d ContaBolle      s              2  0 inz
021200120717     d Wprof           s              3
021300120717     d s_TASaas        s                   like(TASaas)  inz
021400120717     d s_TASlnp        s                   like(TASlnp)  inz
021500120717     d s_TASnrs        s                   like(TASnrs)  inz
021600120717     d s_TASnsp        s                   like(TASnsp)  inz
021700120717     d s_TASmgs        s                   like(TASmgs)  inz
021800120720     d s_TASccm        s                   like(TASccm)  inz
021900120822     d s_TAScca        s                   like(TAScca)  inz
022000120717     d W5aas           s                   like(TASaas)  inz
022100120717     d W5lnp           s                   like(TASlnp)  inz
022200120717     d W5nrs           s                   like(TASnrs)  inz
022300120717     d W5nsp           s                   like(TASnsp)  inz
022400120717     d b_W5aas         s                   like(TASaas)  inz
022500120717     d b_W5lnp         s                   like(TASlnp)  inz
022600120717     d b_W5nrs         s                   like(TASnrs)  inz
022700120717     d b_W5nsp         s                   like(TASnsp)  inz
022800120717
022900120717       //--------------------------------------------------------------
023000120717       //?Definizione prototipi procedure.                             ?
023100120717       //--------------------------------------------------------------
023200120717
023300120717       // -?Reperimento dati utente?
023400120717     d TIBS34ds      e ds                  inz
023500120717      /copy gaitrasrc/srcProtoPR,TIBS34R
023600120717
023700120717       // -?Interrogazione sottoconti?
023800120717      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
023900080807      /copy gaitrasrc/srcprotopr,XALFA3BR
024000120717
024100120717       // -?Interrogazione particolarità consegne - tab. "7R" (TABEL)?
024200120717     d ds7Pqrs       e ds                  inz
024300120717     d   ds7ric      e                     inz('S')
024400120717     d   ds7ges      e                     inz('O')
024500120717     d   kscabi                       2    dim(30)  overlay(ds7cdp)
024600120717      /copy gaitrasrc/srcprotoPR,TRTB70R
024700120717
024800120717       // -?Calcolo check digit del barcode?
024900120717     d TRUL28ds      e ds                  inz
025000120717     d   I28tla      e                     inz('L')
025100120717     d   I28mod      e                     inz('BAR')
025200120717      /copy gaitrasrc/srcprotoPR,TRUL28R1
025300120717
025400120717       // -?Motivazioni errata ricezione documenti?
025500120717     d TNTB92ds      e ds                  inz
025600120717      /copy gaitrasrc/srcprotoPR,TNTB92R
025700120717
025800120717       // -?Stampa copia LdV x Scannerizzazione documenti clienti?
025900120717     d TNSB25ds      e ds                  inz
026000120717     d   D25ges      e                     inz('T')
026100120717      /copy gaitrasrc/srcProtoPR,TNSB25R
026200120717
026300120717       //--------------------------------------------------------------
026400120717       //?Definizione key-list.                                        ?
026500120717       //--------------------------------------------------------------
026600120717
026700120719       // -?File TNTBE01S (TNTBE01L di Sede)?
026800120717     d k05tntbe01    e ds                  extname(TNTBE01S : *key)
026900120717     d                                     prefix(k_)   inz
027000120717
027100120717       //--------------------------------------------------------------
027200120717       //?Specifiche di input.                                         ?
027300120717       //--------------------------------------------------------------
027400080826
027500120717       // -?TITAS30C?
027600120717     iTITAS000      81
027700120717     iTITAS010      82
027800120717     iTITASP00      83
027900120717
028000120717       // -?TITAS32C?
028100120717     iTITAS002      84
028200120717     iTITAS012      85
028300120717     iTITASP02      86
028400120717
028500120717       //--------------------------------------------------------------
028600120717       //?M A I N - L I N E                                            ?
028700120717       //--------------------------------------------------------------
028800120717
028900120717     c     *Entry        plist
029000120717     c                   parm                    KPJBA
029100120706      /free
029200120706
029300120717       // -?Operazioni iniziali?
029400120717       exsr  sr_RoutInz;
029500080807
029600120717       // -?Gestione video?
029700120717       DoW  $Fine = *off;
029800120717
029900080807         select;
030000120717           // -?Gestione videata D1 (selezioni)?
030100120717           when  $Video = 'D1';
030200120717             exsr  sr_GesD01;
030300120717           // -?Gestione videata D2 (num. spedizione / riferimenti)?
030400120717           when  $Video = 'D2';
030500120717             exsr  sr_GesD02;
030600120717           // -?Gestione videata D2 (inventario documento)?
030700120717           when  $Video = 'D3';
030800120717             exsr  sr_GesD03;
030900120717           // -?? ? ? ? ??
031000080807           other   ;
031100080807             $Fine = *on;
031200080807         endsl;
031300120717
031400120717       EndDo;
031500080807
031600120717       // -?Operazioni finali?
031700120717       exsr  sr_RoutEnd;
031800080807
031900080807       //--------------------------------------------------------------
032000080807       //?Operazioni iniziali.
032100080807       //--------------------------------------------------------------
032200120717       BEGSR  sr_RoutInz;
032300120717
032400120717         ParManut = %subst( kpjbu : 1 : %len(ParManut) );
032500120717
032600120717         // -?In base al flag si imposta la testata *pgm?
032700120717         if ParManut = 'M';
032800120717           V1Ctes = '*  M A N U T E N Z I O N E  *';
032900120717           IndManut = *on;
033000120717         else;
033100120717           V1Ctes = '**   I M M I S S I O N E   **';
033200120717           IndManut = *off;
033300120717         endif;
033400120717
033500120717         $InzD01 = *on;
033600080807
033700120717         // -?Reperimento dati job?
033800120717         exsr  sr_DatiJob;
033900120717
034000120717         // -?Data corrente?
034100120717         Secolo = %int( %subst( %editc( %dec(DataSys) : 'X') : 1 : 2) );
034200120717
034300120717         // -?Carico tutte le particolarità consegne che prevedono?
034400120717         //  ?la restituzione della BAM?
034500120717         clear  XX;
034600120717         setll  ( 1 : '7R' )  TABEL;
034700120717         reade  ( 1 : '7R' )  TABEL;
034800120717         dow  not %eof(TABEL00F);
034900120717           if  TBLflg = *blank;
035000120717             ds7R = TBLuni;
035100120717             if  §7R1bf = 'B';
035200120717               xx += 1;
035300120717               sk_7R(xx) = TBLkey;
035400120717             endif;
035500120717           endif;
035600120717           reade ( 1 : '7R' )  TABEL;
035700120717         enddo;
035800120717
035900120717         // -?Apertura TNTBE e FIAR5 di sede?
036000120717         if  %subst( KNSIF : 7 : 1 ) = 'P';
036100120717           %subst( wTBE1S : 7 : 4 ) = 'GRPS';
036200120717           %subst( wFIAR5 : 7 : 4 ) = 'GRPS';
036300120717         endif;
036400120717         open  TNTBE01S;
036500120717         open  FIAR531C;
036600080807
036700120717       ENDSR;
036800080807
036900080807       //--------------------------------------------------------------
037000080807       //?Reperimento Dati del job (Utente/Operativi).
037100080807       //--------------------------------------------------------------
037200120717       BEGSR  sr_DatiJob;
037300080807
037400080807         in(E) §AzUte;
037500080807         if NOT %error;
037600080807           in(E) §DatiUte;
037700080807         endif;
037800080807         if %error or RSut = *blanks;
037900080807           clear TIBS34ds;
038000080807           tibs34r(tibs34ds);
038100080807           in §AzUte;
038200080807           in §DatiUte;
038300080807         endif;
038400080807
038500080807       ENDSR;
038600120717
038700080807       //--------------------------------------------------------------
038800080807       //?Gestione video 1
038900080807       //--------------------------------------------------------------
039000120717       BEGSR  sr_GesD01;
039100080807
039200120717         // -?Inizializzazione videata?
039300080807         if  $Inzd01 = *on;
039400120717           exsr  sr_InzD01;
039500120717           $Inzd01  = *off;
039600080807         endif;
039700080807
039800120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
039900120717         AbilitF12 = *off;
040000120718         AbilitF16 = *off;
040100120717         if  Not errGenerico;
040200120717           write  LSV1T01;
040300120717           write  LSV1Z01;
040400080807         endif;
040500080807
040600120717         // -?Emissione videata?
040700120717         exfmt  LSV1D01;
040800080807
040900120717         reset  errMessage;
041000120717         reset  errGenerico;
041100120717         clear  V1Cmsg;
041200080807
041300120717         SELECT;
041400080807
041500120717           // -?F3=Fine?
041600120717           WHEN  dsp_aid = c_F03;
041700120717             $Fine = *on;
041800080807
041900120717           // -?F8=Gestione motivi errati documentazione?
042000120717           WHEN  dsp_aid = c_F08;
042100120717             clear  TNTB92ds;
042200120717             B92ke1 = %editc( DUTpou : 'X' );
042300120717             kpjbu  = TNTB92ds;
042400120717             callp  tntb92r ( kpjba );
042500120329
042600120717           // -?Invio?
042700080807           OTHER;
042800120717             exsr  sr_ContrD01;
042900120717             if  errGenerico = *on;
043000080807               leavesr;
043100120717             endif;
043200120717             $Video  = 'D2';
043300120717             $Inzd02 = *on;
043400080807
043500120717         ENDSL;
043600080807
043700080807       ENDSR;
043800120717
043900080807       //--------------------------------------------------------------
044000080807       //?Inizializzazione video1
044100080807       //--------------------------------------------------------------
044200120717       BEGSR  sr_InzD01;
044300080807
044400120717         clear  LSV1D01;
044500120717         clear  LSV1D02;
044600120717         clear  LSV1D03;
044700120717
044800120717         V1Ctip = 'A';
044900120717
045000120717       ENDSR;
045100120717
045200120717       //--------------------------------------------------------------
045300120717       //?Controlli video 1
045400120717       //--------------------------------------------------------------
045500120717       BEGSR  sr_ContrD01 ;
045600120717
045700120717         clear  errKSC;
045800120717
045900120717         select;
046000120717
046100120717           // -?Codice Cliente Mittente NON inserito (nè la Rag.Soc.)?
046200120717           when  V1Cksc = *zero  and  V1Dksc = *blank;
046300120717             errKSC = *on;
046400120717             V1Cmsg = Msg(01);
046500120717             errMessage  = *on;
046600120717             errGenerico = *on;
046700120717             leavesr;
046800120717
046900120717           // -?Ricerca Codice Cliente Mittente x Rag.Soc.?
047000120717           when  V1Cksc = *zero  and  V1Dksc <> *blank;
047100120717             xparsta = 9;
047200120717             xpardut = rsut;
047300120717             clear xparrag;
047400120717             xparrag = V1Dksc;
047500120717             xparkcc = DUTkci;
047600120717             clear  xparflr;
047700120717             clear  xpardit;
047800120717             xparnum = 1;
047900120717             clear  xparkcm;
048000120717             clear  xparksm;
048100120717             clear  xparkdm;
048200120717             clear  xparesci;
048300120717             clear  xparerr;
048400120717             clear  xpariva;
048500120717             clear  xparcdf;
048600120717             XALFA3BR ( xpardut : xparkut : xparrag : xparkcc : xparsta :
048700120717                        xparflr : xpardit : xparnum : xparkcm : xparksm :
048800120717                        xparkdm : xparesci: xparerr : xpariva : xparcdf );
048900120717             if  xparsta <> -1;
049000120717               w007a  = xparksm;
049100120717               V1Cksc = %int(w007a);
049200120717               V1Dksc = xparrag;
049300120717             else;
049400120717               clear  V1Dksc;
049500120717             endif;
049600120717             ErrGenerico = *on;
049700120717             write  LSV1T01;
049800120717             write  LSV1Z01;
049900120717             leavesr;
050000120717
050100120717           // -?Codice Cliente Mittente inserito?
050200120717           other;
050300120717             chain  ( 1 : DUTkci : V1Cksc )  CNACO000;
050400120717             if  Not %found(CNACO00F)  or  ACOflg <> *blank;
050500120717               errKSC = *on;
050600120717               V1Cmsg = Msg(02);
050700120717               errMessage  = *on;
050800120717               errGenerico = *on;
050900120717               clear  V1Dksc;
051000120717               leavesr;
051100120717             endif;
051200120717             V1Dksc = ACOrag;
051300120717
051400120717         endsl;
051500120717
051600120717         // -?Verifico se il cliente prevede la particolarità giusta?
051700120717         clear ds7Pqrs;
051800120717         ds7ksc  = V1Cksc;
051900120717         ds7ges  = 'O';
052000120717         ds7ric  = 'S';
052100120717         kpjbu   = ds7Pqrs;
052200120717         TRTB70R ( KPJBA );
052300120717         ds7Pqrs = kpjbu;
052400120717
052500120717         xx = 1;
052600120717         clear  IndX;
052700120717         dow  KscAbi(xx) <> *blank  and  IndX = *zero;
052800120717           IndX = %lookup( kscabi(xx) : sk_7R );
052900120717           xx  += 1;
053000120717         enddo;
053100120717
053200120717         if  IndX = *zero;
053300120717           errKSC = *on;
053400120717           V1Cmsg = Msg(03);
053500120717           errMessage  = *on;
053600120717           errGenerico = *on;
053700120717           leavesr;
053800120717         endif;
053900120717
054000120717       ENDSR;
054100120717
054200080807       //--------------------------------------------------------------
054300080807       //?Gestione video 2
054400080807       //--------------------------------------------------------------
054500120717       BEGSR  sr_GesD02;
054600080807
054700120717         // -?Inizializzazione videata?
054800080807         if  $Inzd02 = *on;
054900120717           exsr  sr_InzD02;
055000120717           $Inzd02  = *off;
055100080807         endif;
055200080807
055300120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
055400120717         AbilitF12 = *on;
055500120718         AbilitF16 = *off;
055600080807         if  errGenerico = *off;
055700120717           write  LSV1T01;
055800120717           write  LSV1Z01;
055900120717           write  LSV1D01;
056000120717           write  Protect;
056100080807         endif;
056200080807
056300120717         // -?Emissione videata?
056400120717         exfmt  LSV1D02;
056500080807
056600120717         reset  errMessage;
056700120717         reset  errGenerico;
056800120717         clear  V1cmsg;
056900080807
057000120717         SELECT;
057100080807
057200120717           // -?F3=Fine?
057300120717           WHEN  dsp_aid = c_F03;
057400120717             $Fine = *on;
057500120329
057600120717           // -?F8=Gestione motivi errati documentazione?
057700120717           WHEN  dsp_aid = c_F08;
057800120718             clear  TNTB92ds;
057900120718             B92ke1 = %editc( DUTpou : 'X' );
058000120718             kpjbu  = TNTB92ds;
058100120718             callp  tntb92r (kpjba);
058200120329
058300080807
058400120717           // -?F12=Ritorno?
058500120717           WHEN  dsp_aid = c_F12;
058600120717             $Video   = 'D1';
058700120717             VisTotBo = *off;
058800120718             unlock  FIAR531C;
058900120717             close(e) TITAS32C;
059000120717             close(e) TITAS30C;
059100120717             close(e) TITA435C;
059200080807
059300120717           // -?Invio?
059400080807           OTHER;
059500120717             exsr  sr_ContrD02 ;
059600120717             if  errGenerico = *on;
059700080807               leavesr;
059800120717             endif;
059900080807
060000120717         ENDSL;
060100080807
060200080807       ENDSR;
060300120717
060400120717       //--------------------------------------------------------------
060500120717       //?Inizializzazione video 2
060600120717       //--------------------------------------------------------------
060700120717       BEGSR  sr_InzD02;
060800120717
060900120717         clear  LSV1D02;
061000120717         clear  LSV1D03;
061100120717
061200120717         exsr  sr_InzParz02;
061300120717
061400120717         V1Caas = %subdt( %date( *date ) : *y );
061500120717
061600120717         // -?Campi video 3?
061700120717         V1Cfil = DUTpou;
061800120717
061900120717         if  %subst( knsif : 7 : 1) = 'P';
062000120717           %subst( wTAS30 : 7 : 4 ) = 'GRPS';
062100120717           %subst( wTAS32 : 7 : 4 ) = 'GRPS';
062200120717           %subst( wTA435 : 7 : 4 ) = 'GRPS';
062300120717         endif;
062400120717
062500120717         select;
062600120717           when  V1Ctip = 'N';
062700120717             VisNumer = *on;
062800120717             open  TITAS32C;
062900120717           when V1Ctip = 'A';
063000120717             VisAlfa = *on;
063100120717             open  TITA435C;
063200120717             open  TITAS30C;
063300120717           other;
063400120717             VisSped = *on;
063500120717             // -?Se barcode accendo indicatore per posizionamento cursore?
063600120717             if  V1Ctip = 'B';
063700120717               PosCurs = *on;
063800120717             endif;
063900120717             open  TITAS30C;
064000120717         endsl;
064100120717
064200120717       ENDSR;
064300120717
064400120717       //--------------------------------------------------------------
064500120717       //?Controlli video 2
064600120717       //--------------------------------------------------------------
064700120717       BEGSR  sr_ContrD02 ;
064800120717
064900120717         errSPE = *off;
065000120717         errBAR = *off;
065100120717         *in81  = *off;
065200120717         *in82  = *off;
065300120717         *in83  = *off;
065400120717         *in84  = *off;
065500120717         *in85  = *off;
065600120717         *in86  = *off;
065700120717         clear  wTitasP;
065800120717         clear  ContaBolle;
065900120717         clear  s_TASaas;
066000120717         clear  s_TASmgs;
066100120717         clear  s_TASlnp;
066200120717         clear  s_TASnrs;
066300120717         clear  s_TASnsp;
066400120720         clear  s_TASccm;
066500120822         clear  s_TAScca;
066600120717
066700120717         SELECT;
066800120717
066900120717           // -?Immissione per riferimenti mitt NUMERICO?
067000120717           WHEN VisNumer = *on;
067100120717             if V1Crmn = *zero;
067200120717               errSPE = *on;
067300120717               V1Cmsg = Msg(04);
067400120717               errMessage  = *on;
067500120717               errGenerico = *on;
067600120717               leavesr;
067700120717             endif;
067800120717
067900120717             setll  (V1Crmn : V1Cksc)  TITAS32C;
068000120717             reade  (V1Crmn : V1Cksc)  TITAS32C;
068100120717             DoW   Not  %eof(TITAS32C);
068200120717
068300120717               // -?Escludo se bolla figlia?
068400120717               chain  (TASaas : TASlnp : TASnrs : TASnsp)  FNLBL000;
068500120717               if  Not %found(FNLBL01L);
068600120717                 // -?La tengo solo se ha particolarità giusta?
068700120717                 IndX = %lookup( tasgma : sk_7R );
068800120717                 if  IndX > *zero  and  TASgma <> *blank;
068900120717                   if ContaBolle < 99;
069000120717                     ContaBolle += 1;
069100120717                     // -?Mi salvo se la bolla?
069200120717                     if s_TASaas = *zero;
069300120717                       s_TASaas = TASaas;
069400120717                       s_TASmgs = TASmgs;
069500120717                       s_TASlnp = TASlnp;
069600120717                       s_TASnrs = TASnrs;
069700120717                       s_TASnsp = TASnsp;
069800120720                       s_TASccm = TASccm;
069900120822                       s_TAScca = TAScca;
070000120717                       if  *in86 = *on;
070100120717                         wTitasP = 'S';
070200120717                       endif;
070300120717                     endif;
070400120717                   endif;
070500120717                 endif;
070600120717               endif;
070700120717
070800120717               *in84 = *off;
070900120717               *in85 = *off;
071000120717               *in86 = *off;
071100120717               reade  (V1Crmn : V1Cksc)  TITAS32C;
071200120717
071300120717             EndDo;
071400120717
071500120717             if  ContaBolle <> 1;
071600120717               errSPE = *on;
071700120717               if  ContaBolle = *zero;
071800120717                 V1Cmsg = Msg(06);
071900120717               else      ;
072000120717                 V1Cmsg = Msg(05);
072100120717                 %subst( V1Cmsg : 40 : 2 ) = %char(ContaBolle);
072200120717               endif    ;
072300120717               errMessage  = *on;
072400120717               errGenerico = *on;
072500120717               leavesr;
072600120717             endif;
072700120717
072800120717             // -?Se la bolla è in TITASP non accetto più l'inserimento?
072900120717             if  wTitasP = 'S';
073000120717               errSPE = *on;
073100120717               V1Cmsg = Msg(15);
073200120717               errMessage  = *on;
073300120717               errGenerico = *on;
073400120717               leavesr;
073500120717             endif;
073600120717
073700120717           // -?Immissione per riferimenti mittente ALFANUMERICO?
073800120717           WHEN VisAlfa = *on;
073900120717             if V1Crma = *blank;
074000120717               errSPE = *on;
074100120717               V1Cmsg = Msg(07);
074200120717               errMessage  = *on;
074300120717               errGenerico = *on;
074400120717               leavesr;
074500120717             endif;
074600120717
074700120717             setll  (V1Crma)  TITA435C;
074800120717             reade  (V1Crma)  TITA435C;
074900120717             DoW  Not %eof(TITA435C);
075000120717
075100120717               *in81 = *off  ;
075200120717               *in82 = *off  ;
075300120717               *in83 = *off  ;
075400120717               chain  (TA4aas : TA4lnp : TA4nrs : TA4nsp)  TITAS30C;
075500120717               if  %found(TITAS30C)  and
075600120717                   TASccm = V1Cksc   and  TASgma <> *blank;
075700120717
075800120717                 // -?Escludo se bolla figlia?
075900120717                 chain  (TASaas : TASlnp : TASnrs : TASnsp)  FNLBL000;
076000120717                 if  Not %found(FNLBL01L);
076100120717
076200120717                   // -?La tengo solo se ha particolarità giusta?
076300120717                   IndX = %lookup( TASgma : sk_7R );
076400120717                   if  Indx > *zero;
076500120717                     if  ContaBolle < 99;
076600120717                       ContaBolle += 1;
076700120717                       // -?Memorizzo la prima bolla che trovo valida?
076800120717                       if  s_TASaas = *zero;
076900120717                         s_TASaas = TASaas;
077000120717                         s_TASmgs = TASmgs;
077100120717                         s_TASlnp = TASlnp;
077200120717                         s_TASnrs = TASnrs;
077300120717                         s_TASnsp = TASnsp;
077400120720                         s_TASccm = TASccm;
077500120822                         s_TAScca = TAScca;
077600120717                         if  *in83 = *on;
077700120717                           wTitasP = 'S';
077800120717                         endif;
077900120717                       endif;
078000120717                     endif;
078100120717                   endif;
078200120717                 endif;
078300120717               endif;
078400120717
078500120717               reade  (V1Crma)  TITA435C;
078600120717
078700120717             EndDo;
078800120717
078900120717             if  ContaBolle <> 1;
079000120717               errSPE = *on;
079100120717               if  ContaBolle = *zero;
079200120717                 V1Cmsg = Msg(06);
079300120717               else;
079400120717                 V1Cmsg = Msg(08);
079500120717                 %subst( V1Cmsg : 42 : 2 ) = %char(ContaBolle);
079600120717               endif;
079700120717               errMessage  = *on;
079800120717               errGenerico = *on;
079900120717               leavesr;
080000120717             endif;
080100120717
080200120717             // -?Se la bolla è in TITASP non accetto più l'inserimento?
080300120717             if wTitasP = 'S';
080400120717               errSPE = *on;
080500120717               v1cmsg = Msg(15);
080600120717               errMessage  = *on;
080700120717               errGenerico = *on;
080800120717               leavesr;
080900120717             endif;
081000120717
081100120717           OTHER;
081200120717             // -?Immissione per numero di spedizione?
081300120717             if  (V1Cnsp = *zero  and  V1Cpis =  *zero )  or
081400120717                 (V1Cnsp > *zero  and  V1Cpis <> *zero );
081500120717               errSPE = *on;
081600120717               V1Cmsg = Msg(10);
081700120717               errMessage  = *on;
081800120717               errGenerico = *on;
081900120717               leavesr;
082000120717             endif;
082100120717
082200120717             // -?Se pieno numero di spedizione --> uso quello,?
082300120717             //  ?altrimenti il barcode?
082400120717             clear  ds_pis;
082500120717             if  V1Cpis <> *zero;
082600120717               ds_pis = %subst( %editc(V1Cpis : 'X') : 4 : 15 );
082700120717               // -?Controllo chkdigit?
082800120717               //BarCode = %editc( V1Cpis : 'X');
082900120717               BarCode = %subst( %editc( V1Cpis : 'X' ) : 4 : 15 );
083000120717               clear  TRUL28ds;
083100120717               i28mod = 'BAR';
083200120717               i28cod = BarCode;
083300120717               callp  TRUL28R1  (trul28ds);
083400120717               if  O28err <> *blank  or  O28cod <> ds_pis;
083500120717                 errBAR7 = *on;
083600120717                 errGenerico = *on;
083700120717                 leavesr;
083800120717               endif;
083900120717             endif;
084000120717
084100120717             *in81 = *off;
084200120717             *in82 = *off;
084300120717             *in83 = *off;
084400120717             if V1Cnsp > *zero;
084500120717               chain  (V1Caas : V1Clnp : V1Cnrs : V1Cnsp)  TITAS30C;
084600120717             else;
084700120717               Kanno = (Secolo*100) + ds_pisAAS;
084800120717               chain  (kanno : ds_pisLNP : ds_pisNRS : ds_pisNSP)  TITAS30C;
084900120717             endif;
085000120717
085100120717             if  Not  %found(TITAS30C);
085200120717               if  V1Cnsp > *zero;
085300120717                 errSPE = *on;
085400120717                 V1Cmsg = Msg(11);
085500120717                 ErrMessage  = *on;
085600120717               else;
085700120717                 errBAR1=*on;
085800120717               endif;
085900120717               errGenerico = *on;
086000120717               leavesr;
086100120717             else;
086200120717               if  TASccm <> V1Cksc;
086300120717                 if  V1Cnsp > *zero;
086400120717                   errSPE = *on;
086500120717                   V1Cmsg = Msg(12);
086600120717                   errMessage  = *on;
086700120717                 else;
086800120717                   errBAR2=*on;
086900120717                 endif;
087000120717                 errGenerico = *on;
087100120717                 leavesr;
087200120717               endif;
087300120717
087400120717               // -?Se la bolla non ha una particolarità che prevede la?
087500120717               //  ?restituzione del DDT --> errore?
087600120717               IndX = %lookup( TASgma : sk_7R);
087700120717               if  IndX = *zero  or  TASgma = *blank;
087800120717               if  V1Cnsp > *zero;
087900120717                 errSPE = *on;
088000120717                 V1Cmsg = Msg(14);
088100120717                 errMessage  = *on;
088200120717               else;
088300120717                 errBAR3=*on;
088400120717               endif  ;
088500120717               errGenerico = *on;
088600120717               leavesr;
088700120717             endif;
088800120717
088900120717             // -?Se la bolla è figlia errore?
089000120717             chain  (TASaas : TASlnp : TASnrs : TASnsp )  FNLBL000;
089100120717             If  %found(FNLBL01L);
089200120717               if  V1Cnsp > *zero;
089300120717                 errSPE = *on;
089400120717                 V1Cmsg = Msg(16);
089500120717                   errMessage  = *on;
089600120717                 else;
089700120717                   errBAR4 = *on;
089800120717                 endif;
089900120717                 errGenerico = *on;
090000120717                 leavesr;
090100120717               endif;
090200120717               // -?Se la bolla è in TITASP non accetto più l'inserimento?
090300120717               if  *in83 = *on;
090400120717                 if V1Cnsp > *zero;
090500120717                   errSPE = *on;
090600120717                   V1Cmsg = Msg(15);
090700120717                   errMessage  = *on;
090800120717                 else;
090900120717                   errBAR5 = *on;
091000120717                 endif;
091100120717                 errGenerico = *on;
091200120717                 leavesr;
091300120717               endif;
091400120717             EndIf;
091500120717
091600120717             // salvo il numero di spedizione
091700120717             s_TASaas = TASaas;
091800120717             s_TASmgs = TASmgs;
091900120717             s_TASlnp = TASlnp;
092000120717             s_TASnrs = TASnrs;
092100120717             s_TASnsp = TASnsp;
092200120720             s_TASccm = TASccm;
092300120822             s_TAScca = TAScca;
092400120717             if  *in83 = *on;
092500120717               wTitasP = 'S';
092600120717             endif;
092700120717
092800120717         ENDSL;
092900120717
093000120717         // -?Trovata la spedizione, avviso se già aggiornata in altra data?
093100120717         IF  s_TASaas <> w5aas  or  s_TASlnp <> w5lnp  or
093200120717             s_TASnrs <> w5nrs  or  s_TASnsp <> w5nsp;
093300120717           w5aas = s_TASaas;
093400120717           w5lnp = s_TASlnp;
093500120717           w5nrs = s_TASnrs;
093600120717           w5nsp = s_TASnsp;
093700120717
093800120717           if  IndManut = *off;
093900120717             chain  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'GEN')
094000120717                    FIAR531C;
094100120717           else;
094200120717             chain(n)  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'GEN')
094300120717                       FIAR531C;
094400120717           endif;
094500120717
094600120717           IF  %found(FIAR531C);
094700120717
094800120717             dAR5gen = AR5uni;
094900120717             DataIso = %date;
095000120717
095100120717             // -?Avviso se già aggiornata in altra data --> modo Immissione?
095200120717             If  IndManut = *off  ;
095300120717
095400120717               if  §AR5drdt <> *blank  and  §AR5drdt <> '99999999';
095500120717                 if  VisNumer = *off  and  VisAlfa = *off  and  V1Cnsp = *zero;
095600120717                   errBAR6 = *on;
095700120717                 else  ;
095800120717                   errSPE  = *on;
095900120717                   V1Cmsg  = Msg(13);
096000120717                   DataEur = %date( %int( §AR5drdt ) : *iso );
096100120717                   %subst(V1Cmsg : 36 : 10) =
096200120717                     %editw( %dec( dataeur ) : '  /  /    ' );
096300120717                   errMessage  = *on;
096400120717                 endif;
096500120717                 errGenerico = *on;
096600120717                 leavesr;
096700120717               endif;
096800120717
096900120717             Else;
097000120717
097100120717               // -?In manutenzione imposto la data?
097200120717               if  §AR5drdt <> *blank  and  §AR5drdt <> '99999999';
097300120717                 monitor;
097400120717                   DataIso = %date( %int( §AR5drdt ) );
097500120717                 on-error;
097600120717                   clear  DataIso;
097700120717                 endmon;
097800120717               else;
097900120717                 clear  DataIso;
098000120717               endif;
098100120717               if  DataIso > *loval;
098200120717                 DataEur = DataIso;
098300120717                 V1CDrdt = %dec(DataEur);
098400120717               endif;
098500120717
098600120717               // -?Data inserimento motivo?
098700120717               if §AR5dimtv <> *blank;
098800120717                 monitor;
098900120717                   DataIso = %date( %int( §AR5dimtv ) );
099000120717                 on-error;
099100120717                   clear  DataIso;
099200120717                 endmon;
099300120717               else;
099400120717                 clear  DataIso;
099500120717               endif;
099600120717               if  DataIso > *loval;
099700120717                 DataEur  = DataIso;
099800120717                 V1CDimtv = %dec(DataEur);
099900120717               endif;
100000120717
100100120717               // -?Codice motivo?
100200120717               if  §AR5mtvdt <> *blank;
100300120717                 V1Cmtvdt = %subst( §AR5mtvdt : 4 : 5 );
100400120717                 V1Cfil   = %int( %subst( §AR5mtvdt : 1 : 3 ) );
100500120717                 clear  TBEke1;
100600120717                 clear  tbeke2;
100700120717                 TBEke1 = %editc( V1Cfil : 'X' );
100800120717                 TBEke2 = V1Cmtvdt;
100900120717                 chain  ('MDE' : TBEke1 : TBEke2)  TNTBE000;
101000120717                 if  %found(TNTBE01L);
101100120717                   V1Dmtvdt = TBEuni;
101200120717                 endif  ;
101300120717               endif  ;
101400120717
101500120717             EndIf;
101600120717
101700120717           ENDIF;
101800120717
101900120717         ENDIF;
102000120717
102100120717         // -?Nessun errore => Se manutenzione emetto 3° videata?
102200120717         IF  IndManut = *on;
102300120717
102400120717           $Video = 'D3';
102500120717
102600120717         ELSE;
102700120717
102800120717           // -?Trovata la spedizione, avviso se presente motivo che?
102900120717           //  ?verrà tolto?
103000120717           If  §AR5mtvdt <> *blank  and
103100120717              (s_TASaas <> b_W5aas  or  s_TASlnp <> b_W5lnp  or
103200120717               s_TASnrs <> b_W5nrs  or  s_TASnsp <> b_W5nsp);
103300120717             b_W5aas = s_TASaas;
103400120717             b_W5lnp = s_TASlnp;
103500120717             b_W5nrs = s_TASnrs;
103600120717             b_W5nsp = S_TASnsp;
103700120717             if  VisNumer = *off  and  VisAlfa = *off  and  V1Cnsp = *zero;
103800120717               errBAR8 = *on;
103900120717             else  ;
104000120717               V1Cmsg = Msg(19);
104100120717               %subst( V1Cmsg : 35 : 8 ) = §AR5mtvdt;
104200120717               errSPE = *on;
104300120717               errMessage  = *on;
104400120717             endif  ;
104500120717             errGenerico = *on;
104600120717             leavesr;
104700120717           EndIf;
104800120717
104900120717           // -?Se nessun errore: aggiorno con Enter FIAR5 e contatore?
105000120717           If  Not %found(FIAR531C)  or
105100120717               §AR5drdt <> %char( %dec( DataIso ) );
105200120717
105300120717             if  Not %found(FIAR531C);
105400120717               exsr  ImpostaAR5;
105500120717             else;
105600120717               dAR5gen = AR5uni;
105700120717             endif;
105800120717
105900120717             DataIso  = %date;
106000120717             §AR5drdt = %char( %dec( DataIso ) );
106100120717             clear  §AR5fstd;
106200120717             clear  §AR5mtvdt;
106300120717             clear  §AR5dimtv;
106400120717             AR5uni = dAR5gen;
106500120717
106600120717             exsr  UpdateAR5;
106700120717
106800120717             // -?Totali bolle aggiornate?
106900120717             V1Ctot  += 1;
107000120717             VisTotBo = *on;
107100120717
107200120717             // -?Stampo LDV?
107300120719             //  ?Deciso da LB:?
107400120719             //  ?la stampa della bolla la si fa comunque?
107500120719             //  ?(SE NON già valorizzato dAR5doc.§AR5jfid)?
107600120719             //exsr  sr_StampaLDV;
107700120717
107800120717           Else;
107900120717
108000120717             unlock  FIAR531C;
108100120717
108200120717           EndIf;
108300120719
108400120719           // -?Stampo LDV?
108500120719           exsr  sr_StampaLDV;
108600120717
108700120717           exsr  sr_InzParz02;
108800120717
108900120717         ENDIF;
109000120717
109100120717
109200120717       ENDSR;
109300120717
109400120320       //--------------------------------------------------------------
109500120625       //?Gestione video 3
109600120320       //--------------------------------------------------------------
109700120717       BEGSR  sr_GesD03;
109800120320
109900120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
110000120718         AbilitF12 = *on;
110100120718         AbilitF16 = *on;
110200120320         if  errGenerico = *off;
110300120717           write  LSV1T01;
110400120717           write  LSV1Z01;
110500120717           write  LSV1D01;
110600120717           write  LSV1D02;
110700120717           write  Protect;
110800120320         endif;
110900120320
111000120717         // -?Emissione videata?
111100120717         exfmt  LSV1D03;
111200120328
111300120320         reset errMessage;
111400120320         reset errGenerico;
111500120320         clear V1cmsg;
111600120320
111700120717         SELECT;
111800120320
111900120717           // -?F3=Fine?
112000120717           WHEN  dsp_aid = c_F03;
112100120717             $Fine = *on;
112200120329
112300120717           // -?F8=Gestione motivi errati documentazione?
112400120717           WHEN  dsp_aid = c_F08;
112500120717             clear  TNTB92ds  ;
112600120717             B92ke1 = %editc( DUTpou : 'X' );
112700120717             kpjbu = TNTB92ds;
112800120717             callp  TNTB92R ( kpjba );
112900120320
113000120717           // -?F12=Ritorno?
113100120717           WHEN  dsp_aid = c_F12;
113200120717             $Video = 'D2';
113300120717             exsr  sr_InzParz02;
113400120328
113500120717           // -?F16=Elimino data inventario documenti se presente?
113600120717           WHEN  dsp_aid = c_F16;
113700120717             exsr  Annuldata  ;
113800120320
113900120717           // -?Invio o F6=Conferma?
114000120320           OTHER;
114100120717             exsr  sr_ContrD03 ;
114200120717             if  errGenerico = *on;
114300120320               leavesr;
114400120717             endif;
114500120717             if  dsp_aid = c_F06;
114600120717               exsr  AggioMotivo;
114700120717               $Video = 'D2';
114800120717             endif;
114900120320
115000120717         ENDSL;
115100120320
115200120320       ENDSR;
115300120717
115400120717       //--------------------------------------------------------------
115500120717       //?Controlli video 3
115600120717       //--------------------------------------------------------------
115700120717       BEGSR  sr_ContrD03  ;
115800120717
115900120717         ErrDrdt = *off;
116000120717         ErrMTV  = *off;
116100120717
116200120717         clear  V1Dmtvdt;
116300120717
116400120717         // -?Verifico '?' sulla causale?
116500120717         IndX = %scan( '?' : V1Cmtvdt );
116600120717         if  IndX > *zero;
116700120717           clear  V1Cmtvdt;
116800120717           clear  TNTB92ds;
116900120717           B92fun = '1';
117000120717           B92ke1 = %editc( V1Cfil : 'X' );
117100120717           kpjbu  = TNTB92ds;
117200120717           callp  TNTB92R  (kpjba);
117300120717           TNTB92ds = kpjbu;
117400120717           if B92ke2 <> *blank;
117500120717             V1Cmtvdt = B92ke2;
117600120717             V1Dmtvdt = B92des;
117700120717           endif;
117800120717         endif;
117900120717
118000120717         // -?Controllo causale se immessa?
118100120717         if  V1Cmtvdt <> *blank;
118200120717           clear  TBEke1;
118300120717           clear  TBEke2;
118400120717           TBEke1 = %editc( V1Cfil : 'X' );
118500120717           TBEke2 = V1Cmtvdt;
118600120717           chain  ('MDE' : TBEke1 : TBEke2)  TNTBE000;
118700120717           if  Not %found(TNTBE01L)  or  TBEatb <> *blank;
118800120717             errMTV = *on;
118900120717             V1Cmsg = Msg(18);
119000120717             errMessage  = *on;
119100120717             errGenerico = *on;
119200120717             leavesr;
119300120717           endif;
119400120717           V1Dmtvdt = TBEuni;
119500120717         endif  ;
119600120717
119700120717         // -?Se tolto MOTIVO => AZZERO LA DATA?
119800120717         if  V1Cmtvdt = *blAnk;
119900120717           clear  V1Cdimtv;
120000120717           V1Cfil = DUTpou;
120100120717         else;
120200120717           // -?Se motivo = a quello del file stessa data?
120300120717           if  V1Cmtvdt = %subst( §AR5mtvdt : 4 : 5 )  and
120400120717               §AR5dimtv > *zero;
120500120717             DataIso = %date( %int( §AR5dimtv ) );
120600120717           else;
120700120717             DataIso = %date;
120800120717           endif;
120900120717           DataEur  = DataIso;
121000120717           V1CDimtv = %dec(DataEur);
121100120717         endif;
121200120717
121300120717       ENDSR;
121400120717
121500120328       //--------------------------------------------------------------
121600120328       //?F16 - eliminazione data inventario documenti
121700120328       //--------------------------------------------------------------
121800120328       BEGSR  Annuldata   ;
121900120329
122000120328       // se data non presente errore
122100120329 1         if v1cdrdt=0  ;
122200120328           errdrdt=*on;
122300120328           v1cmsg = Msg(09);
122400120328           errMessage  = *on;
122500120328           errGenerico = *on;
122600120328           leavesr;
122700120329 1         endif  ;
122800120328
122900120328       // Updato pulendo la data   ;
123000120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
123100120329 1     if %found(fiar531c)  ;
123200120328       dar5gen=ar5uni           ;
123300120328
123400120330       §ar5drdt='99999999'      ;
123500120328       clear §ar5fstd           ;
123600120328       ar5uni=dar5gen           ;
123700120329
123800120329 2       if wtitasp='S'  ;
123900120329          update fiar5p00           ;
124000120329           else    ;
124100120329          update fiar5000           ;
124200120329 2     endif    ;
124300120328
124400120328 1     endif    ;
124500120329
124600120329       clear v1cdrdt   ;
124700120328
124800120328       ENDSR  ;
124900120328       //--------------------------------------------------------------
125000120328       //?F06 - Aggiorno motivo ddt errati
125100120328       //--------------------------------------------------------------
125200120328       BEGSR  Aggiomotivo ;
125300120328
125400120402 1     if §ar5mtvdt  <> %editc(v1cfil:'X')+v1cmtvdt   ;
125500120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
125600120328
125700120402 2     if not %found(fiar531c)  ;
125800120328       exsr ImpostaAR5          ;
125900120402 x2    else                     ;
126000120328       dar5gen=ar5uni           ;
126100120402 2     endif                    ;
126200120328
126300120402        if v1cmtvdt= *blanks  ;
126400120329         clear §ar5dimtv  ;
126500120402         clear §ar5mtvdt  ;
126600120329        else   ;
126700120402         §ar5mtvdt=%editc(v1cfil:'X') +v1cmtvdt   ;
126800120329         dataiso=%date             ;
126900120329         §ar5dimtv=%char(%dec(dataiso))   ;
127000120329        endif    ;
127100120329
127200120329       ar5uni=dar5gen  ;
127300120328
127400120329       exsr UpdateAR5  ;
127500120329
127600120402       endif    ;
127700120328
127800120717       exsr sr_InzParz02 ;
127900120328
128000120328       ENDSR  ;
128100120328       //--------------------------------------------------------------
128200120328       //?Imposta campi FIar5 per la write
128300120328       //--------------------------------------------------------------
128400120328       BEGSR ImpostaAR5         ;
128500120328
128600120328       clear fiar5000           ;
128700120328       clear fiar5p00           ;
128800120328       ar5aas=s_tasaas          ;
128900120328       ar5lnp=s_taslnp          ;
129000120328       ar5nrs=s_tasnrs          ;
129100120328       ar5nsp=s_tasnsp          ;
129200120328       ar5trd='GEN'             ;
129300120328       dataiso=%date             ;
129400120328       ar5dac=%dec(dataiso)     ;
129500120328       timeiso=%time             ;
129600120328       ar5orc=%dec(timeiso)     ;
129700120328       ar5ft1='R'               ;
129800120328       ar5dt1=%dec(dataiso)     ;
129900120328       ar5ft2='R'               ;
130000120328       ar5dt2=%dec(dataiso)     ;
130100120328       ar5ft3='R'               ;
130200120328       ar5dt3=%dec(dataiso)     ;
130300120328
130400120328       clear dar5gen            ;
130500120328       ENDSR                   ;
130600120329       //--------------------------------------------------------------
130700120329       //?Aggiorno record FIAR5
130800120329       //--------------------------------------------------------------
130900120329       BEGSR  UpdateAR5  ;
131000120329
131100120329 2     if not %found(fiar531c)  ;
131200120329 3       if wtitasp='S'  ;
131300120329         write  fiar5p00           ;
131400120329          else    ;
131500120329         write  fiar5000           ;
131600120329 3       endif  ;
131700120329
131800120329 x2    else   ;
131900120329 3       if wtitasp='S'  ;
132000120329          update fiar5p00           ;
132100120329           else    ;
132200120329          update fiar5000           ;
132300120329 3     endif    ;
132400120329 2     endif    ;
132500120329
132600120329       ENDSR  ;
132700120717
132800120717       //--------------------------------------------------------------
132900120717       //?Stampa copia LdV x Scannerizzazione Documenti Clienti       ?
133000120717       //--------------------------------------------------------------
133100120717       BEGSR  sr_StampaLDV;
133200120717
133300120717         // -?Aggancio tabella "JDC" per reperimento data attivazione?
133400120717         //  ?della scannerizzazione documenti cliente?
133500120717         clear  dJDC;
133600120717         clear  k05tntbe01;
133700120717         k_TBEcod = 'JDC';
133800120719         // -?NO => TASCCM se valorizzato, TASKSC altrimenti?
133900120719         // -?SÌ => TASCCM sempre e comunque?
134000120720         //if  s_TASccm <> *zero;
134100120720           k_TBEke1 = %editc( s_TASccm : 'X' );
134200120719         //else;
134300120720         //  k_TBEke1 = %editc( s_TASksc : 'X' );
134400120719         //endif;
134500120717
134600120717         chain  %kds( k05tntbe01 : 3 )  TNTBE00S;
134700120717
134800120717         if  %found(TNTBE01S);
134900120717           dJDC = TBEuni;
135000120717         endif;
135100120717
135200120717         // -?Scannerizzazione documenti per il cliente: NON prevista?
135300120717         //  ?o non ancora in vigore o già scaduta?
135400120717         if  NOT  %found(TNTBE01S)                or
135500120717             §JDCdti > (s_TASaas * 10000) + s_TASmgs  or
135600120717             §JDCdtf < (s_TASaas * 10000) + s_TASmgs;
135700120717           leavesr;
135800120717         endif;
135900120717
136000120717         // -?Reperimento rec. "DOC" da FIAR5?
136100120717         clear  dAR5doc;
136200120717
136300120717         chain(n)  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'DOC')
136400120717                   FIAR531C;
136500120717
136600120717         if  %found(FIAR531C);
136700120717           dAR5doc = AR5uni;
136800120822         else ;
136900120822
137000120822         // -?se non trovata vedo se bolla legata: cerco la DOC dalla figlia
137100120822         if s_tascca<>' '   ;
137200120822
137300120822         lblaan=s_tasaas ;
137400120822         lbllpn=s_taslnp ;
137500120822         lblnrn=s_tasnrs ;
137600120822         lblnsn=s_tasnsp ;
137700120822         clear wtrov   ;
137800120822
137900120822         dow wtrov=' '  ;
138000120822               chain  (lblaan : lbllpn : lblnrn : lblnsn)  FNLBLpre;
138100120822               if  %found(FNLBL02L);
138200120822         chain(n)  (lblaan : lbllpn : lblnrn : lblnsn : 'DOC')
138300120822                   FIAR531C;
138400120822                  if  %found(FIAR531C);
138500120822                    dAR5doc = AR5uni;
138600120822                    wtrov='S'       ;
138700120822                  endif;
138800120822               else  ;
138900120822                wtrov='N'  ;
139000120822               endif;
139100120822
139200120822         enddo  ;
139300120717         endif;
139400120822         endif;
139500120717
139600120717         // -?Documenti già inviati al mittente?
139700120717         if  dAR5doc.§AR5jfid <> *blank;
139800120717           leavesr;
139900120717         endif;
140000120717
140100120717         // -?Stampa copia LdV?
140200120717         reset  TNSB25ds;
140300120717         //D25ges = 'T'       ?(già così)?
140400120719         D25aas = s_TASaas;
140500120719         D25lnp = s_TASlnp;
140600120719         D25nrs = s_TASnrs;
140700120719         D25nsp = s_TASnsp;
140800120717
140900120717         tnsb25r ( TNSB25ds );
141000120717
141100120720         V1Ctst  += 1;
141200120720         VisTotBo = *on;
141300120717
141400120717       ENDSR;
141500120717
141600120329       //--------------------------------------------------------------
141700120329       //?Pulizia parziale campoi video 2
141800120329       //--------------------------------------------------------------
141900120717       BEGSR  sr_InzParz02;
142000120329
142100120717         clear  V1Crmn;
142200120717         clear  V1Crma;
142300120717         clear  V1Cnsp;
142400120717         clear  V1Cpis;
142500120717
142600120717         clear  W5aas;
142700120717         clear  W5lnp;
142800120717         clear  W5nrs;
142900120717         clear  W5nsp;
143000120717         clear  b_W5aas;
143100120717         clear  b_W5lnp;
143200120717         clear  b_W5nrs;
143300120717         clear  b_W5nsp;
143400120717
143500120717         clear  V1Cdrdt;
143600120717         clear  V1Cmtvdt;
143700120717         clear  V1Cdimtv;
143800120717         V1Cfil = DUTpou;
143900120717
144000120329       ENDSR  ;
144100120717
144200120717       //--------------------------------------------------------------
144300120717       //?Operazioni finali.
144400120717       //--------------------------------------------------------------
144500120717       BEGSR  sr_RoutEnd;
144600120717
144700120717         *inLR = *on;
144800120717
144900120717         return;
145000120717
145100120717       ENDSR;
145200080807
145300080807      /end-free
145400080807
145500120717** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
145600080826Immettere il codice Cliente Mittente                                           1
145700080826Codice Cliente Mittente inesistente                                            2
145800080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
145900080826Immettere il riferimento mittente NUMERICO                                     4
146000080826Per il rif. Mittente NUMERICO esistono xx bolle: inserire col numero di Spediz 5
146100120717Nessuna bolla trovata per il cliente con particolarità restituzione documenti  6
146200080826Immettere il riferimento mittente ALFANUMERICO                                 7
146300080826Per il RifMittente ALFANUMERICO esistono xx bolle:inserire col numero di Sped  8
146400120328Impossibile annullare : data non presente!                                     9
146500110520Immettere il numero di SPEDIZIONE  o il Barcode, in alternativa               10
146600080826Numero di SPEDIZIONE inesistente                                               1
146700080826La SPEDIZIONE non appartiene al cliente mittente indicato                      2
146800120521Già presente data ricezione doc.al xx/xx/xxxx:ENTER per forzare e riaggiornare 3
146900080909La SPEDIZIONE non prevede particolarità consegna con restituzione documenti    4
147000080901La spedizione è ormai troppo vecchia: impossibile eseguire l'inventario        5
147100120521Spedizione con legame bolla:inserire ricezione documenti sulla bolla originale 6
147200120717Errore nel Barcode                                                    (LIBERO) 7
147300120330Causale motivo inesistente                                                     8
147400120521Presente motivo documenti errati "xxxxxxxx": verrà eliminato                   9
