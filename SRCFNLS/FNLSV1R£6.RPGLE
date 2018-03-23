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
009300120919     d Msg             s             78    dim(20)  ctdata  perrcd(1)
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
016800120919
016900120919       // -?Tabella "CLI" = Abilitazione clienti
017000120919     d dCLI          e ds                  inz
017100120717
017200120717       // -?Dati FIAR5 / trk "GEN"?
017300120717     d dAR5gen       e ds                  inz
017400120717       // -?Dati FIAR5 / trk "DOC"?
017500120717     d dAR5doc       e ds                  inz  qualified
017600120717
017700120717       //--------------------------------------------------------------
017800120717       //?Definizione variabili globali.                               ?
017900120717       //--------------------------------------------------------------
018000120717
018100120717       // -?Flags booleani?
018200120717     d $Fine           s               n   inz(*off)
018300120717     d $Inzd01         s               n   inz(*on)
018400120717     d $Inzd02         s               n   inz(*on)
018500120717     d wTitasP         s              1
018600120717
018700120717       // -?Indici di schiera?
018800120717     d xx              s              3  0 inz
018900120717     d IndX            s              3  0 inz
019000120717
019100120717       // -?Variabili per la gestione del video?
019200120717     D $Video          s              2    inz('D1')
019300120717
019400120717       // -?Nome esteso Libreria/File degli archivi di Sede?
019500120717     d wTBE1S          s             21    inz('GAITRAGRU /TNTBE01L')
019600120717     d wTAS30          s             21    inz('GAITRAGRU /TITAS30C')
019700120717     d wTAS32          s             21    inz('GAITRAGRU /TITAS32C')
019800120717     d wTA435          s             21    inz('GAITRAGRU /TITA435C')
019900120717     d wFIAR5          s             21    inz('GAITRAGRU /FIAR531C')
020000120717
020100120717       // -?Campi "data"?
020200120717     d DataSys         s               d   datfmt(*iso) inz(*sys)
020300120717     d DataIso         s               d   datfmt(*iso)
020400120717     d TimeIso         s               t   timfmt(*iso)
020500120717     d DataEur         s               d   datfmt(*eur)
020600120717
020700120717       // -?Campi di comodo?
020800120822     d wtrov           s              1
020900120822     d ParManut        s              1
021000120717     d w007a           s              7
021100120717     d BarCode         s             14
021200120717     d Secolo          s              2  0
021300110520     d Kanno           s              4  0
021400120717     d ContaBolle      s              2  0 inz
021500120717     d Wprof           s              3
021600120717     d s_TASaas        s                   like(TASaas)  inz
021700120717     d s_TASlnp        s                   like(TASlnp)  inz
021800120717     d s_TASnrs        s                   like(TASnrs)  inz
021900120717     d s_TASnsp        s                   like(TASnsp)  inz
022000120717     d s_TASmgs        s                   like(TASmgs)  inz
022100120720     d s_TASccm        s                   like(TASccm)  inz
022200120822     d s_TAScca        s                   like(TAScca)  inz
022300120717     d W5aas           s                   like(TASaas)  inz
022400120717     d W5lnp           s                   like(TASlnp)  inz
022500120717     d W5nrs           s                   like(TASnrs)  inz
022600120717     d W5nsp           s                   like(TASnsp)  inz
022700120717     d b_W5aas         s                   like(TASaas)  inz
022800120717     d b_W5lnp         s                   like(TASlnp)  inz
022900120717     d b_W5nrs         s                   like(TASnrs)  inz
023000120717     d b_W5nsp         s                   like(TASnsp)  inz
023100120717
023200120717       //--------------------------------------------------------------
023300120717       //?Definizione prototipi procedure.                             ?
023400120717       //--------------------------------------------------------------
023500120717
023600120717       // -?Reperimento dati utente?
023700120717     d TIBS34ds      e ds                  inz
023800120717      /copy gaitrasrc/srcProtoPR,TIBS34R
023900120717
024000120717       // -?Interrogazione sottoconti?
024100120717      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
024200080807      /copy gaitrasrc/srcprotopr,XALFA3BR
024300120717
024400120717       // -?Interrogazione particolarità consegne - tab. "7R" (TABEL)?
024500120717     d ds7Pqrs       e ds                  inz
024600120717     d   ds7ric      e                     inz('S')
024700120717     d   ds7ges      e                     inz('O')
024800120717     d   kscabi                       2    dim(30)  overlay(ds7cdp)
024900120717      /copy gaitrasrc/srcprotoPR,TRTB70R
025000120717
025100120717       // -?Calcolo check digit del barcode?
025200120717     d TRUL28ds      e ds                  inz
025300120717     d   I28tla      e                     inz('L')
025400120717     d   I28mod      e                     inz('BAR')
025500120717      /copy gaitrasrc/srcprotoPR,TRUL28R1
025600120717
025700120717       // -?Motivazioni errata ricezione documenti?
025800120717     d TNTB92ds      e ds                  inz
025900120717      /copy gaitrasrc/srcprotoPR,TNTB92R
026000120717
026100120717       // -?Stampa copia LdV x Scannerizzazione documenti clienti?
026200120717     d TNSB25ds      e ds                  inz
026300120717     d   D25ges      e                     inz('T')
026400120717      /copy gaitrasrc/srcProtoPR,TNSB25R
026500120717
026600120717       //--------------------------------------------------------------
026700120717       //?Definizione key-list.                                        ?
026800120717       //--------------------------------------------------------------
026900120717
027000120719       // -?File TNTBE01S (TNTBE01L di Sede)?
027100120717     d k05tntbe01    e ds                  extname(TNTBE01S : *key)
027200120717     d                                     prefix(k_)   inz
027300120717
027400120717       //--------------------------------------------------------------
027500120717       //?Specifiche di input.                                         ?
027600120717       //--------------------------------------------------------------
027700080826
027800120717       // -?TITAS30C?
027900120717     iTITAS000      81
028000120717     iTITAS010      82
028100120717     iTITASP00      83
028200120717
028300120717       // -?TITAS32C?
028400120717     iTITAS002      84
028500120717     iTITAS012      85
028600120717     iTITASP02      86
028700120717
028800120717       //--------------------------------------------------------------
028900120717       //?M A I N - L I N E                                            ?
029000120717       //--------------------------------------------------------------
029100120717
029200120717     c     *Entry        plist
029300120717     c                   parm                    KPJBA
029400120706      /free
029500120706
029600120717       // -?Operazioni iniziali?
029700120717       exsr  sr_RoutInz;
029800080807
029900120717       // -?Gestione video?
030000120717       DoW  $Fine = *off;
030100120717
030200080807         select;
030300120717           // -?Gestione videata D1 (selezioni)?
030400120717           when  $Video = 'D1';
030500120717             exsr  sr_GesD01;
030600120717           // -?Gestione videata D2 (num. spedizione / riferimenti)?
030700120717           when  $Video = 'D2';
030800120717             exsr  sr_GesD02;
030900120717           // -?Gestione videata D2 (inventario documento)?
031000120717           when  $Video = 'D3';
031100120717             exsr  sr_GesD03;
031200120717           // -?? ? ? ? ??
031300080807           other   ;
031400080807             $Fine = *on;
031500080807         endsl;
031600120717
031700120717       EndDo;
031800080807
031900120717       // -?Operazioni finali?
032000120717       exsr  sr_RoutEnd;
032100080807
032200080807       //--------------------------------------------------------------
032300080807       //?Operazioni iniziali.
032400080807       //--------------------------------------------------------------
032500120717       BEGSR  sr_RoutInz;
032600120717
032700120717         ParManut = %subst( kpjbu : 1 : %len(ParManut) );
032800120717
032900120717         // -?In base al flag si imposta la testata *pgm?
033000120717         if ParManut = 'M';
033100120717           V1Ctes = '*  M A N U T E N Z I O N E  *';
033200120717           IndManut = *on;
033300120717         else;
033400120717           V1Ctes = '**   I M M I S S I O N E   **';
033500120717           IndManut = *off;
033600120717         endif;
033700120717
033800120717         $InzD01 = *on;
033900080807
034000120717         // -?Reperimento dati job?
034100120717         exsr  sr_DatiJob;
034200120717
034300120717         // -?Data corrente?
034400120717         Secolo = %int( %subst( %editc( %dec(DataSys) : 'X') : 1 : 2) );
034500120717
034600120717         // -?Carico tutte le particolarità consegne che prevedono?
034700120717         //  ?la restituzione della BAM?
034800120717         clear  XX;
034900120717         setll  ( 1 : '7R' )  TABEL;
035000120717         reade  ( 1 : '7R' )  TABEL;
035100120717         dow  not %eof(TABEL00F);
035200120717           if  TBLflg = *blank;
035300120717             ds7R = TBLuni;
035400120717             if  §7R1bf = 'B';
035500120717               xx += 1;
035600120717               sk_7R(xx) = TBLkey;
035700120717             endif;
035800120717           endif;
035900120717           reade ( 1 : '7R' )  TABEL;
036000120717         enddo;
036100120717
036200120717         // -?Apertura TNTBE e FIAR5 di sede?
036300120717         if  %subst( KNSIF : 7 : 1 ) = 'P';
036400120717           %subst( wTBE1S : 7 : 4 ) = 'GRPS';
036500120717           %subst( wFIAR5 : 7 : 4 ) = 'GRPS';
036600120717         endif;
036700120717         open  TNTBE01S;
036800120717         open  FIAR531C;
036900080807
037000120717       ENDSR;
037100080807
037200080807       //--------------------------------------------------------------
037300080807       //?Reperimento Dati del job (Utente/Operativi).
037400080807       //--------------------------------------------------------------
037500120717       BEGSR  sr_DatiJob;
037600080807
037700080807         in(E) §AzUte;
037800080807         if NOT %error;
037900080807           in(E) §DatiUte;
038000080807         endif;
038100080807         if %error or RSut = *blanks;
038200080807           clear TIBS34ds;
038300080807           tibs34r(tibs34ds);
038400080807           in §AzUte;
038500080807           in §DatiUte;
038600080807         endif;
038700080807
038800080807       ENDSR;
038900120717
039000080807       //--------------------------------------------------------------
039100080807       //?Gestione video 1
039200080807       //--------------------------------------------------------------
039300120717       BEGSR  sr_GesD01;
039400080807
039500120717         // -?Inizializzazione videata?
039600080807         if  $Inzd01 = *on;
039700120717           exsr  sr_InzD01;
039800120717           $Inzd01  = *off;
039900080807         endif;
040000080807
040100120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
040200120717         AbilitF12 = *off;
040300120718         AbilitF16 = *off;
040400120717         if  Not errGenerico;
040500120717           write  LSV1T01;
040600120717           write  LSV1Z01;
040700080807         endif;
040800080807
040900120717         // -?Emissione videata?
041000120717         exfmt  LSV1D01;
041100080807
041200120717         reset  errMessage;
041300120717         reset  errGenerico;
041400120717         clear  V1Cmsg;
041500080807
041600120717         SELECT;
041700080807
041800120717           // -?F3=Fine?
041900120717           WHEN  dsp_aid = c_F03;
042000120717             $Fine = *on;
042100080807
042200120717           // -?F8=Gestione motivi errati documentazione?
042300120717           WHEN  dsp_aid = c_F08;
042400120717             clear  TNTB92ds;
042500120717             B92ke1 = %editc( DUTpou : 'X' );
042600120717             kpjbu  = TNTB92ds;
042700120717             callp  tntb92r ( kpjba );
042800120329
042900120717           // -?Invio?
043000080807           OTHER;
043100120717             exsr  sr_ContrD01;
043200120717             if  errGenerico = *on;
043300080807               leavesr;
043400120717             endif;
043500120717             $Video  = 'D2';
043600120717             $Inzd02 = *on;
043700080807
043800120717         ENDSL;
043900080807
044000080807       ENDSR;
044100120717
044200080807       //--------------------------------------------------------------
044300080807       //?Inizializzazione video1
044400080807       //--------------------------------------------------------------
044500120717       BEGSR  sr_InzD01;
044600080807
044700120717         clear  LSV1D01;
044800120717         clear  LSV1D02;
044900120717         clear  LSV1D03;
045000120717
045100120717         V1Ctip = 'A';
045200120717
045300120717       ENDSR;
045400120717
045500120717       //--------------------------------------------------------------
045600120717       //?Controlli video 1
045700120717       //--------------------------------------------------------------
045800120717       BEGSR  sr_ContrD01 ;
045900120717
046000120717         clear  errKSC;
046100120717
046200120717         select;
046300120717
046400120717           // -?Codice Cliente Mittente NON inserito (nè la Rag.Soc.)?
046500120717           when  V1Cksc = *zero  and  V1Dksc = *blank;
046600120717             errKSC = *on;
046700120717             V1Cmsg = Msg(01);
046800120717             errMessage  = *on;
046900120717             errGenerico = *on;
047000120717             leavesr;
047100120717
047200120717           // -?Ricerca Codice Cliente Mittente x Rag.Soc.?
047300120717           when  V1Cksc = *zero  and  V1Dksc <> *blank;
047400120717             xparsta = 9;
047500120717             xpardut = rsut;
047600120717             clear xparrag;
047700120717             xparrag = V1Dksc;
047800120717             xparkcc = DUTkci;
047900120717             clear  xparflr;
048000120717             clear  xpardit;
048100120717             xparnum = 1;
048200120717             clear  xparkcm;
048300120717             clear  xparksm;
048400120717             clear  xparkdm;
048500120717             clear  xparesci;
048600120717             clear  xparerr;
048700120717             clear  xpariva;
048800120717             clear  xparcdf;
048900120717             XALFA3BR ( xpardut : xparkut : xparrag : xparkcc : xparsta :
049000120717                        xparflr : xpardit : xparnum : xparkcm : xparksm :
049100120717                        xparkdm : xparesci: xparerr : xpariva : xparcdf );
049200120717             if  xparsta <> -1;
049300120717               w007a  = xparksm;
049400120717               V1Cksc = %int(w007a);
049500120717               V1Dksc = xparrag;
049600120717             else;
049700120717               clear  V1Dksc;
049800120717             endif;
049900120717             ErrGenerico = *on;
050000120717             write  LSV1T01;
050100120717             write  LSV1Z01;
050200120717             leavesr;
050300120717
050400120717           // -?Codice Cliente Mittente inserito?
050500120717           other;
050600120717             chain  ( 1 : DUTkci : V1Cksc )  CNACO000;
050700120717             if  Not %found(CNACO00F)  or  ACOflg <> *blank;
050800120717               errKSC = *on;
050900120717               V1Cmsg = Msg(02);
051000120717               errMessage  = *on;
051100120717               errGenerico = *on;
051200120717               clear  V1Dksc;
051300120717               leavesr;
051400120717             endif;
051500120717             V1Dksc = ACOrag;
051600120717
051700120717         endsl;
051800120717
051900120717         // -?Verifico se il cliente prevede la particolarità giusta?
052000120717         clear ds7Pqrs;
052100120717         ds7ksc  = V1Cksc;
052200120717         ds7ges  = 'O';
052300120717         ds7ric  = 'S';
052400120717         kpjbu   = ds7Pqrs;
052500120717         TRTB70R ( KPJBA );
052600120717         ds7Pqrs = kpjbu;
052700120717
052800120717         xx = 1;
052900120717         clear  IndX;
053000120717         dow  KscAbi(xx) <> *blank  and  IndX = *zero;
053100120717           IndX = %lookup( kscabi(xx) : sk_7R );
053200120717           xx  += 1;
053300120717         enddo;
053400120717
053500120717         if  IndX = *zero;
053600120717           errKSC = *on;
053700120717           V1Cmsg = Msg(03);
053800120717           errMessage  = *on;
053900120717           errGenerico = *on;
054000120717           leavesr;
054100120717         endif;
054200120919
054300120919         // - controllo se filiale gestibile
054400120919         clear  TBEke1;
054500120919         clear  tbeke2;
054600120919         TBEke1 = %editc( V1CKsc : 'X' );
054700120919         chain  ('CLI' : TBEke1 : TBEke2)  TNTBE000;
054800120919         // do errore se il cliente non è sulla tab.CLI oppure c'è ma annullato
054900120919         if  not %found(TNTBE01L)
055000120919          or %found(TNTBE01L) and TBEATB <> *blank;
055100120919           errKSC = *on;
055200120919           V1Cmsg = Msg(20);
055300120919           errMessage  = *on;
055400120919           errGenerico = *on;
055500120919           leavesr;
055600120919         else;
055700120919           dCLI = TBEuni;
055800120919           // do errore il PO utente non è tra le filiali abilitate alla
055900120919           // retituzione doc. al mittente
056000120920           if  %editc( DUTpou : 'X' ) <> §CLIFi1 and
056100120920               %editc( DUTpou : 'X' ) <> §CLIFi2 and
056200120920               %editc( DUTpou : 'X' ) <> §CLIFi3;
056300120919             errKSC = *on;
056400120919             V1Cmsg = Msg(20);
056500120919             errMessage  = *on;
056600120919             errGenerico = *on;
056700120919             leavesr;
056800120919           endif;
056900120919         endif;
057000120717
057100120717       ENDSR;
057200120717
057300080807       //--------------------------------------------------------------
057400080807       //?Gestione video 2
057500080807       //--------------------------------------------------------------
057600120717       BEGSR  sr_GesD02;
057700080807
057800120717         // -?Inizializzazione videata?
057900080807         if  $Inzd02 = *on;
058000120717           exsr  sr_InzD02;
058100120717           $Inzd02  = *off;
058200080807         endif;
058300080807
058400120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
058500120717         AbilitF12 = *on;
058600120718         AbilitF16 = *off;
058700080807         if  errGenerico = *off;
058800120717           write  LSV1T01;
058900120717           write  LSV1Z01;
059000120717           write  LSV1D01;
059100120717           write  Protect;
059200080807         endif;
059300080807
059400120717         // -?Emissione videata?
059500120717         exfmt  LSV1D02;
059600080807
059700120717         reset  errMessage;
059800120717         reset  errGenerico;
059900120717         clear  V1cmsg;
060000080807
060100120717         SELECT;
060200080807
060300120717           // -?F3=Fine?
060400120717           WHEN  dsp_aid = c_F03;
060500120717             $Fine = *on;
060600120329
060700120717           // -?F8=Gestione motivi errati documentazione?
060800120717           WHEN  dsp_aid = c_F08;
060900120718             clear  TNTB92ds;
061000120718             B92ke1 = %editc( DUTpou : 'X' );
061100120718             kpjbu  = TNTB92ds;
061200120718             callp  tntb92r (kpjba);
061300120329
061400080807
061500120717           // -?F12=Ritorno?
061600120717           WHEN  dsp_aid = c_F12;
061700120717             $Video   = 'D1';
061800120717             VisTotBo = *off;
061900120718             unlock  FIAR531C;
062000120717             close(e) TITAS32C;
062100120717             close(e) TITAS30C;
062200120717             close(e) TITA435C;
062300080807
062400120717           // -?Invio?
062500080807           OTHER;
062600120717             exsr  sr_ContrD02 ;
062700120717             if  errGenerico = *on;
062800080807               leavesr;
062900120717             endif;
063000080807
063100120717         ENDSL;
063200080807
063300080807       ENDSR;
063400120717
063500120717       //--------------------------------------------------------------
063600120717       //?Inizializzazione video 2
063700120717       //--------------------------------------------------------------
063800120717       BEGSR  sr_InzD02;
063900120717
064000120717         clear  LSV1D02;
064100120717         clear  LSV1D03;
064200120717
064300120717         exsr  sr_InzParz02;
064400120717
064500120717         V1Caas = %subdt( %date( *date ) : *y );
064600120717
064700120717         // -?Campi video 3?
064800120717         V1Cfil = DUTpou;
064900120717
065000120717         if  %subst( knsif : 7 : 1) = 'P';
065100120717           %subst( wTAS30 : 7 : 4 ) = 'GRPS';
065200120717           %subst( wTAS32 : 7 : 4 ) = 'GRPS';
065300120717           %subst( wTA435 : 7 : 4 ) = 'GRPS';
065400120717         endif;
065500120717
065600120717         select;
065700120717           when  V1Ctip = 'N';
065800120717             VisNumer = *on;
065900120717             open  TITAS32C;
066000120717           when V1Ctip = 'A';
066100120717             VisAlfa = *on;
066200120717             open  TITA435C;
066300120717             open  TITAS30C;
066400120717           other;
066500120717             VisSped = *on;
066600120717             // -?Se barcode accendo indicatore per posizionamento cursore?
066700120717             if  V1Ctip = 'B';
066800120717               PosCurs = *on;
066900120717             endif;
067000120717             open  TITAS30C;
067100120717         endsl;
067200120717
067300120717       ENDSR;
067400120717
067500120717       //--------------------------------------------------------------
067600120717       //?Controlli video 2
067700120717       //--------------------------------------------------------------
067800120717       BEGSR  sr_ContrD02 ;
067900120717
068000120717         errSPE = *off;
068100120717         errBAR = *off;
068200120717         *in81  = *off;
068300120717         *in82  = *off;
068400120717         *in83  = *off;
068500120717         *in84  = *off;
068600120717         *in85  = *off;
068700120717         *in86  = *off;
068800120717         clear  wTitasP;
068900120717         clear  ContaBolle;
069000120717         clear  s_TASaas;
069100120717         clear  s_TASmgs;
069200120717         clear  s_TASlnp;
069300120717         clear  s_TASnrs;
069400120717         clear  s_TASnsp;
069500120720         clear  s_TASccm;
069600120822         clear  s_TAScca;
069700120717
069800120717         SELECT;
069900120717
070000120717           // -?Immissione per riferimenti mitt NUMERICO?
070100120717           WHEN VisNumer = *on;
070200120717             if V1Crmn = *zero;
070300120717               errSPE = *on;
070400120717               V1Cmsg = Msg(04);
070500120717               errMessage  = *on;
070600120717               errGenerico = *on;
070700120717               leavesr;
070800120717             endif;
070900120717
071000120717             setll  (V1Crmn : V1Cksc)  TITAS32C;
071100120717             reade  (V1Crmn : V1Cksc)  TITAS32C;
071200120717             DoW   Not  %eof(TITAS32C);
071300120717
071400120717               // -?Escludo se bolla figlia?
071500120717               chain  (TASaas : TASlnp : TASnrs : TASnsp)  FNLBL000;
071600120717               if  Not %found(FNLBL01L);
071700120717                 // -?La tengo solo se ha particolarità giusta?
071800120717                 IndX = %lookup( tasgma : sk_7R );
071900120717                 if  IndX > *zero  and  TASgma <> *blank;
072000120717                   if ContaBolle < 99;
072100120717                     ContaBolle += 1;
072200120717                     // -?Mi salvo se la bolla?
072300120717                     if s_TASaas = *zero;
072400120717                       s_TASaas = TASaas;
072500120717                       s_TASmgs = TASmgs;
072600120717                       s_TASlnp = TASlnp;
072700120717                       s_TASnrs = TASnrs;
072800120717                       s_TASnsp = TASnsp;
072900120720                       s_TASccm = TASccm;
073000120822                       s_TAScca = TAScca;
073100120717                       if  *in86 = *on;
073200120717                         wTitasP = 'S';
073300120717                       endif;
073400120717                     endif;
073500120717                   endif;
073600120717                 endif;
073700120717               endif;
073800120717
073900120717               *in84 = *off;
074000120717               *in85 = *off;
074100120717               *in86 = *off;
074200120717               reade  (V1Crmn : V1Cksc)  TITAS32C;
074300120717
074400120717             EndDo;
074500120717
074600120717             if  ContaBolle <> 1;
074700120717               errSPE = *on;
074800120717               if  ContaBolle = *zero;
074900120717                 V1Cmsg = Msg(06);
075000120717               else      ;
075100120717                 V1Cmsg = Msg(05);
075200120717                 %subst( V1Cmsg : 40 : 2 ) = %char(ContaBolle);
075300120717               endif    ;
075400120717               errMessage  = *on;
075500120717               errGenerico = *on;
075600120717               leavesr;
075700120717             endif;
075800120717
075900120717             // -?Se la bolla è in TITASP non accetto più l'inserimento?
076000120717             if  wTitasP = 'S';
076100120717               errSPE = *on;
076200120717               V1Cmsg = Msg(15);
076300120717               errMessage  = *on;
076400120717               errGenerico = *on;
076500120717               leavesr;
076600120717             endif;
076700120717
076800120717           // -?Immissione per riferimenti mittente ALFANUMERICO?
076900120717           WHEN VisAlfa = *on;
077000120717             if V1Crma = *blank;
077100120717               errSPE = *on;
077200120717               V1Cmsg = Msg(07);
077300120717               errMessage  = *on;
077400120717               errGenerico = *on;
077500120717               leavesr;
077600120717             endif;
077700120717
077800120717             setll  (V1Crma)  TITA435C;
077900120717             reade  (V1Crma)  TITA435C;
078000120717             DoW  Not %eof(TITA435C);
078100120717
078200120717               *in81 = *off  ;
078300120717               *in82 = *off  ;
078400120717               *in83 = *off  ;
078500120717               chain  (TA4aas : TA4lnp : TA4nrs : TA4nsp)  TITAS30C;
078600120717               if  %found(TITAS30C)  and
078700120717                   TASccm = V1Cksc   and  TASgma <> *blank;
078800120717
078900120717                 // -?Escludo se bolla figlia?
079000120717                 chain  (TASaas : TASlnp : TASnrs : TASnsp)  FNLBL000;
079100120717                 if  Not %found(FNLBL01L);
079200120717
079300120717                   // -?La tengo solo se ha particolarità giusta?
079400120717                   IndX = %lookup( TASgma : sk_7R );
079500120717                   if  Indx > *zero;
079600120717                     if  ContaBolle < 99;
079700120717                       ContaBolle += 1;
079800120717                       // -?Memorizzo la prima bolla che trovo valida?
079900120717                       if  s_TASaas = *zero;
080000120717                         s_TASaas = TASaas;
080100120717                         s_TASmgs = TASmgs;
080200120717                         s_TASlnp = TASlnp;
080300120717                         s_TASnrs = TASnrs;
080400120717                         s_TASnsp = TASnsp;
080500120720                         s_TASccm = TASccm;
080600120822                         s_TAScca = TAScca;
080700120717                         if  *in83 = *on;
080800120717                           wTitasP = 'S';
080900120717                         endif;
081000120717                       endif;
081100120717                     endif;
081200120717                   endif;
081300120717                 endif;
081400120717               endif;
081500120717
081600120717               reade  (V1Crma)  TITA435C;
081700120717
081800120717             EndDo;
081900120717
082000120717             if  ContaBolle <> 1;
082100120717               errSPE = *on;
082200120717               if  ContaBolle = *zero;
082300120717                 V1Cmsg = Msg(06);
082400120717               else;
082500120717                 V1Cmsg = Msg(08);
082600120717                 %subst( V1Cmsg : 42 : 2 ) = %char(ContaBolle);
082700120717               endif;
082800120717               errMessage  = *on;
082900120717               errGenerico = *on;
083000120717               leavesr;
083100120717             endif;
083200120717
083300120717             // -?Se la bolla è in TITASP non accetto più l'inserimento?
083400120717             if wTitasP = 'S';
083500120717               errSPE = *on;
083600120717               v1cmsg = Msg(15);
083700120717               errMessage  = *on;
083800120717               errGenerico = *on;
083900120717               leavesr;
084000120717             endif;
084100120717
084200120717           OTHER;
084300120717             // -?Immissione per numero di spedizione?
084400120717             if  (V1Cnsp = *zero  and  V1Cpis =  *zero )  or
084500120717                 (V1Cnsp > *zero  and  V1Cpis <> *zero );
084600120717               errSPE = *on;
084700120717               V1Cmsg = Msg(10);
084800120717               errMessage  = *on;
084900120717               errGenerico = *on;
085000120717               leavesr;
085100120717             endif;
085200120717
085300120717             // -?Se pieno numero di spedizione --> uso quello,?
085400120717             //  ?altrimenti il barcode?
085500120717             clear  ds_pis;
085600120717             if  V1Cpis <> *zero;
085700120717               ds_pis = %subst( %editc(V1Cpis : 'X') : 4 : 15 );
085800120717               // -?Controllo chkdigit?
085900120717               //BarCode = %editc( V1Cpis : 'X');
086000120717               BarCode = %subst( %editc( V1Cpis : 'X' ) : 4 : 15 );
086100120717               clear  TRUL28ds;
086200120717               i28mod = 'BAR';
086300120717               i28cod = BarCode;
086400120717               callp  TRUL28R1  (trul28ds);
086500120717               if  O28err <> *blank  or  O28cod <> ds_pis;
086600120717                 errBAR7 = *on;
086700120717                 errGenerico = *on;
086800120717                 leavesr;
086900120717               endif;
087000120717             endif;
087100120717
087200120717             *in81 = *off;
087300120717             *in82 = *off;
087400120717             *in83 = *off;
087500120717             if V1Cnsp > *zero;
087600120717               chain  (V1Caas : V1Clnp : V1Cnrs : V1Cnsp)  TITAS30C;
087700120717             else;
087800120717               Kanno = (Secolo*100) + ds_pisAAS;
087900120717               chain  (kanno : ds_pisLNP : ds_pisNRS : ds_pisNSP)  TITAS30C;
088000120717             endif;
088100120717
088200120717             if  Not  %found(TITAS30C);
088300120717               if  V1Cnsp > *zero;
088400120717                 errSPE = *on;
088500120717                 V1Cmsg = Msg(11);
088600120717                 ErrMessage  = *on;
088700120717               else;
088800120717                 errBAR1=*on;
088900120717               endif;
089000120717               errGenerico = *on;
089100120717               leavesr;
089200120717             else;
089300120717               if  TASccm <> V1Cksc;
089400120717                 if  V1Cnsp > *zero;
089500120717                   errSPE = *on;
089600120717                   V1Cmsg = Msg(12);
089700120717                   errMessage  = *on;
089800120717                 else;
089900120717                   errBAR2=*on;
090000120717                 endif;
090100120717                 errGenerico = *on;
090200120717                 leavesr;
090300120717               endif;
090400120717
090500120717               // -?Se la bolla non ha una particolarità che prevede la?
090600120717               //  ?restituzione del DDT --> errore?
090700120717               IndX = %lookup( TASgma : sk_7R);
090800120717               if  IndX = *zero  or  TASgma = *blank;
090900120717               if  V1Cnsp > *zero;
091000120717                 errSPE = *on;
091100120717                 V1Cmsg = Msg(14);
091200120717                 errMessage  = *on;
091300120717               else;
091400120717                 errBAR3=*on;
091500120717               endif  ;
091600120717               errGenerico = *on;
091700120717               leavesr;
091800120717             endif;
091900120717
092000120717             // -?Se la bolla è figlia errore?
092100120717             chain  (TASaas : TASlnp : TASnrs : TASnsp )  FNLBL000;
092200120717             If  %found(FNLBL01L);
092300120717               if  V1Cnsp > *zero;
092400120717                 errSPE = *on;
092500120717                 V1Cmsg = Msg(16);
092600120717                   errMessage  = *on;
092700120717                 else;
092800120717                   errBAR4 = *on;
092900120717                 endif;
093000120717                 errGenerico = *on;
093100120717                 leavesr;
093200120717               endif;
093300120717               // -?Se la bolla è in TITASP non accetto più l'inserimento?
093400120717               if  *in83 = *on;
093500120717                 if V1Cnsp > *zero;
093600120717                   errSPE = *on;
093700120717                   V1Cmsg = Msg(15);
093800120717                   errMessage  = *on;
093900120717                 else;
094000120717                   errBAR5 = *on;
094100120717                 endif;
094200120717                 errGenerico = *on;
094300120717                 leavesr;
094400120717               endif;
094500120717             EndIf;
094600120717
094700120717             // salvo il numero di spedizione
094800120717             s_TASaas = TASaas;
094900120717             s_TASmgs = TASmgs;
095000120717             s_TASlnp = TASlnp;
095100120717             s_TASnrs = TASnrs;
095200120717             s_TASnsp = TASnsp;
095300120720             s_TASccm = TASccm;
095400120822             s_TAScca = TAScca;
095500120717             if  *in83 = *on;
095600120717               wTitasP = 'S';
095700120717             endif;
095800120717
095900120717         ENDSL;
096000120717
096100120717         // -?Trovata la spedizione, avviso se già aggiornata in altra data?
096200120717         IF  s_TASaas <> w5aas  or  s_TASlnp <> w5lnp  or
096300120717             s_TASnrs <> w5nrs  or  s_TASnsp <> w5nsp;
096400120717           w5aas = s_TASaas;
096500120717           w5lnp = s_TASlnp;
096600120717           w5nrs = s_TASnrs;
096700120717           w5nsp = s_TASnsp;
096800120717
096900120717           if  IndManut = *off;
097000120717             chain  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'GEN')
097100120717                    FIAR531C;
097200120717           else;
097300120717             chain(n)  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'GEN')
097400120717                       FIAR531C;
097500120717           endif;
097600120717
097700120717           IF  %found(FIAR531C);
097800120717
097900120717             dAR5gen = AR5uni;
098000120717             DataIso = %date;
098100120717
098200120717             // -?Avviso se già aggiornata in altra data --> modo Immissione?
098300120717             If  IndManut = *off  ;
098400120717
098500120717               if  §AR5drdt <> *blank  and  §AR5drdt <> '99999999';
098600120717                 if  VisNumer = *off  and  VisAlfa = *off  and  V1Cnsp = *zero;
098700120717                   errBAR6 = *on;
098800120717                 else  ;
098900120717                   errSPE  = *on;
099000120717                   V1Cmsg  = Msg(13);
099100120717                   DataEur = %date( %int( §AR5drdt ) : *iso );
099200120717                   %subst(V1Cmsg : 36 : 10) =
099300120717                     %editw( %dec( dataeur ) : '  /  /    ' );
099400120717                   errMessage  = *on;
099500120717                 endif;
099600120717                 errGenerico = *on;
099700120717                 leavesr;
099800120717               endif;
099900120717
100000120717             Else;
100100120717
100200120717               // -?In manutenzione imposto la data?
100300120717               if  §AR5drdt <> *blank  and  §AR5drdt <> '99999999';
100400120717                 monitor;
100500120717                   DataIso = %date( %int( §AR5drdt ) );
100600120717                 on-error;
100700120717                   clear  DataIso;
100800120717                 endmon;
100900120717               else;
101000120717                 clear  DataIso;
101100120717               endif;
101200120717               if  DataIso > *loval;
101300120717                 DataEur = DataIso;
101400120717                 V1CDrdt = %dec(DataEur);
101500120717               endif;
101600120717
101700120717               // -?Data inserimento motivo?
101800120717               if §AR5dimtv <> *blank;
101900120717                 monitor;
102000120717                   DataIso = %date( %int( §AR5dimtv ) );
102100120717                 on-error;
102200120717                   clear  DataIso;
102300120717                 endmon;
102400120717               else;
102500120717                 clear  DataIso;
102600120717               endif;
102700120717               if  DataIso > *loval;
102800120717                 DataEur  = DataIso;
102900120717                 V1CDimtv = %dec(DataEur);
103000120717               endif;
103100120717
103200120717               // -?Codice motivo?
103300120717               if  §AR5mtvdt <> *blank;
103400120717                 V1Cmtvdt = %subst( §AR5mtvdt : 4 : 5 );
103500120717                 V1Cfil   = %int( %subst( §AR5mtvdt : 1 : 3 ) );
103600120717                 clear  TBEke1;
103700120717                 clear  tbeke2;
103800120717                 TBEke1 = %editc( V1Cfil : 'X' );
103900120717                 TBEke2 = V1Cmtvdt;
104000120717                 chain  ('MDE' : TBEke1 : TBEke2)  TNTBE000;
104100120717                 if  %found(TNTBE01L);
104200120717                   V1Dmtvdt = TBEuni;
104300120717                 endif  ;
104400120717               endif  ;
104500120717
104600120717             EndIf;
104700120717
104800120717           ENDIF;
104900120717
105000120717         ENDIF;
105100120717
105200120717         // -?Nessun errore => Se manutenzione emetto 3° videata?
105300120717         IF  IndManut = *on;
105400120717
105500120717           $Video = 'D3';
105600120717
105700120717         ELSE;
105800120717
105900120717           // -?Trovata la spedizione, avviso se presente motivo che?
106000120717           //  ?verrà tolto?
106100120717           If  §AR5mtvdt <> *blank  and
106200120717              (s_TASaas <> b_W5aas  or  s_TASlnp <> b_W5lnp  or
106300120717               s_TASnrs <> b_W5nrs  or  s_TASnsp <> b_W5nsp);
106400120717             b_W5aas = s_TASaas;
106500120717             b_W5lnp = s_TASlnp;
106600120717             b_W5nrs = s_TASnrs;
106700120717             b_W5nsp = S_TASnsp;
106800120717             if  VisNumer = *off  and  VisAlfa = *off  and  V1Cnsp = *zero;
106900120717               errBAR8 = *on;
107000120717             else  ;
107100120717               V1Cmsg = Msg(19);
107200120717               %subst( V1Cmsg : 35 : 8 ) = §AR5mtvdt;
107300120717               errSPE = *on;
107400120717               errMessage  = *on;
107500120717             endif  ;
107600120717             errGenerico = *on;
107700120717             leavesr;
107800120717           EndIf;
107900120717
108000120717           // -?Se nessun errore: aggiorno con Enter FIAR5 e contatore?
108100120717           If  Not %found(FIAR531C)  or
108200120717               §AR5drdt <> %char( %dec( DataIso ) );
108300120717
108400120717             if  Not %found(FIAR531C);
108500120717               exsr  ImpostaAR5;
108600120717             else;
108700120717               dAR5gen = AR5uni;
108800120717             endif;
108900120717
109000120717             DataIso  = %date;
109100120717             §AR5drdt = %char( %dec( DataIso ) );
109200120717             clear  §AR5fstd;
109300120717             clear  §AR5mtvdt;
109400120717             clear  §AR5dimtv;
109500120717             AR5uni = dAR5gen;
109600120717
109700120717             exsr  UpdateAR5;
109800120717
109900120717             // -?Totali bolle aggiornate?
110000120717             V1Ctot  += 1;
110100120717             VisTotBo = *on;
110200120717
110300120717             // -?Stampo LDV?
110400120719             //  ?Deciso da LB:?
110500120719             //  ?la stampa della bolla la si fa comunque?
110600120719             //  ?(SE NON già valorizzato dAR5doc.§AR5jfid)?
110700120719             //exsr  sr_StampaLDV;
110800120717
110900120717           Else;
111000120717
111100120717             unlock  FIAR531C;
111200120717
111300120717           EndIf;
111400120719
111500120719           // -?Stampo LDV?
111600120719           exsr  sr_StampaLDV;
111700120717
111800120717           exsr  sr_InzParz02;
111900120717
112000120717         ENDIF;
112100120717
112200120717
112300120717       ENDSR;
112400120717
112500120320       //--------------------------------------------------------------
112600120625       //?Gestione video 3
112700120320       //--------------------------------------------------------------
112800120717       BEGSR  sr_GesD03;
112900120320
113000120717         // -?Emissione Testata e Piede con tasti funzionali abilitati?
113100120718         AbilitF12 = *on;
113200120718         AbilitF16 = *on;
113300120320         if  errGenerico = *off;
113400120717           write  LSV1T01;
113500120717           write  LSV1Z01;
113600120717           write  LSV1D01;
113700120717           write  LSV1D02;
113800120717           write  Protect;
113900120320         endif;
114000120320
114100120717         // -?Emissione videata?
114200120717         exfmt  LSV1D03;
114300120328
114400120320         reset errMessage;
114500120320         reset errGenerico;
114600120320         clear V1cmsg;
114700120320
114800120717         SELECT;
114900120320
115000120717           // -?F3=Fine?
115100120717           WHEN  dsp_aid = c_F03;
115200120717             $Fine = *on;
115300120329
115400120717           // -?F8=Gestione motivi errati documentazione?
115500120717           WHEN  dsp_aid = c_F08;
115600120717             clear  TNTB92ds  ;
115700120717             B92ke1 = %editc( DUTpou : 'X' );
115800120717             kpjbu = TNTB92ds;
115900120717             callp  TNTB92R ( kpjba );
116000120320
116100120717           // -?F12=Ritorno?
116200120717           WHEN  dsp_aid = c_F12;
116300120717             $Video = 'D2';
116400120717             exsr  sr_InzParz02;
116500120328
116600120717           // -?F16=Elimino data inventario documenti se presente?
116700120717           WHEN  dsp_aid = c_F16;
116800120717             exsr  Annuldata  ;
116900120320
117000120717           // -?Invio o F6=Conferma?
117100120320           OTHER;
117200120717             exsr  sr_ContrD03 ;
117300120717             if  errGenerico = *on;
117400120320               leavesr;
117500120717             endif;
117600120717             if  dsp_aid = c_F06;
117700120717               exsr  AggioMotivo;
117800120717               $Video = 'D2';
117900120717             endif;
118000120320
118100120717         ENDSL;
118200120320
118300120320       ENDSR;
118400120717
118500120717       //--------------------------------------------------------------
118600120717       //?Controlli video 3
118700120717       //--------------------------------------------------------------
118800120717       BEGSR  sr_ContrD03  ;
118900120717
119000120717         ErrDrdt = *off;
119100120717         ErrMTV  = *off;
119200120717
119300120717         clear  V1Dmtvdt;
119400120717
119500120717         // -?Verifico '?' sulla causale?
119600120717         IndX = %scan( '?' : V1Cmtvdt );
119700120717         if  IndX > *zero;
119800120717           clear  V1Cmtvdt;
119900120717           clear  TNTB92ds;
120000120717           B92fun = '1';
120100120717           B92ke1 = %editc( V1Cfil : 'X' );
120200120717           kpjbu  = TNTB92ds;
120300120717           callp  TNTB92R  (kpjba);
120400120717           TNTB92ds = kpjbu;
120500120717           if B92ke2 <> *blank;
120600120717             V1Cmtvdt = B92ke2;
120700120717             V1Dmtvdt = B92des;
120800120717           endif;
120900120717         endif;
121000120717
121100120717         // -?Controllo causale se immessa?
121200120717         if  V1Cmtvdt <> *blank;
121300120717           clear  TBEke1;
121400120717           clear  TBEke2;
121500120717           TBEke1 = %editc( V1Cfil : 'X' );
121600120717           TBEke2 = V1Cmtvdt;
121700120717           chain  ('MDE' : TBEke1 : TBEke2)  TNTBE000;
121800120717           if  Not %found(TNTBE01L)  or  TBEatb <> *blank;
121900120717             errMTV = *on;
122000120717             V1Cmsg = Msg(18);
122100120717             errMessage  = *on;
122200120717             errGenerico = *on;
122300120717             leavesr;
122400120717           endif;
122500120717           V1Dmtvdt = TBEuni;
122600120717         endif  ;
122700120717
122800120717         // -?Se tolto MOTIVO => AZZERO LA DATA?
122900120717         if  V1Cmtvdt = *blAnk;
123000120717           clear  V1Cdimtv;
123100120717           V1Cfil = DUTpou;
123200120717         else;
123300120717           // -?Se motivo = a quello del file stessa data?
123400120717           if  V1Cmtvdt = %subst( §AR5mtvdt : 4 : 5 )  and
123500120717               §AR5dimtv > *zero;
123600120717             DataIso = %date( %int( §AR5dimtv ) );
123700120717           else;
123800120717             DataIso = %date;
123900120717           endif;
124000120717           DataEur  = DataIso;
124100120717           V1CDimtv = %dec(DataEur);
124200120717         endif;
124300120717
124400120717       ENDSR;
124500120717
124600120328       //--------------------------------------------------------------
124700120328       //?F16 - eliminazione data inventario documenti
124800120328       //--------------------------------------------------------------
124900120328       BEGSR  Annuldata   ;
125000120329
125100120328       // se data non presente errore
125200120329 1         if v1cdrdt=0  ;
125300120328           errdrdt=*on;
125400120328           v1cmsg = Msg(09);
125500120328           errMessage  = *on;
125600120328           errGenerico = *on;
125700120328           leavesr;
125800120329 1         endif  ;
125900120328
126000120328       // Updato pulendo la data   ;
126100120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
126200120329 1     if %found(fiar531c)  ;
126300120328       dar5gen=ar5uni           ;
126400120328
126500120330       §ar5drdt='99999999'      ;
126600120328       clear §ar5fstd           ;
126700120328       ar5uni=dar5gen           ;
126800120329
126900120329 2       if wtitasp='S'  ;
127000120329          update fiar5p00           ;
127100120329           else    ;
127200120329          update fiar5000           ;
127300120329 2     endif    ;
127400120328
127500120328 1     endif    ;
127600120329
127700120329       clear v1cdrdt   ;
127800120328
127900120328       ENDSR  ;
128000120328       //--------------------------------------------------------------
128100120328       //?F06 - Aggiorno motivo ddt errati
128200120328       //--------------------------------------------------------------
128300120328       BEGSR  Aggiomotivo ;
128400120328
128500120402 1     if §ar5mtvdt  <> %editc(v1cfil:'X')+v1cmtvdt   ;
128600120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
128700120328
128800120402 2     if not %found(fiar531c)  ;
128900120328       exsr ImpostaAR5          ;
129000120402 x2    else                     ;
129100120328       dar5gen=ar5uni           ;
129200120402 2     endif                    ;
129300120328
129400120402        if v1cmtvdt= *blanks  ;
129500120329         clear §ar5dimtv  ;
129600120402         clear §ar5mtvdt  ;
129700120329        else   ;
129800120402         §ar5mtvdt=%editc(v1cfil:'X') +v1cmtvdt   ;
129900120329         dataiso=%date             ;
130000120329         §ar5dimtv=%char(%dec(dataiso))   ;
130100120329        endif    ;
130200120329
130300120329       ar5uni=dar5gen  ;
130400120328
130500120329       exsr UpdateAR5  ;
130600120329
130700120402       endif    ;
130800120328
130900120717       exsr sr_InzParz02 ;
131000120328
131100120328       ENDSR  ;
131200120328       //--------------------------------------------------------------
131300120328       //?Imposta campi FIar5 per la write
131400120328       //--------------------------------------------------------------
131500120328       BEGSR ImpostaAR5         ;
131600120328
131700120328       clear fiar5000           ;
131800120328       clear fiar5p00           ;
131900120328       ar5aas=s_tasaas          ;
132000120328       ar5lnp=s_taslnp          ;
132100120328       ar5nrs=s_tasnrs          ;
132200120328       ar5nsp=s_tasnsp          ;
132300120328       ar5trd='GEN'             ;
132400120328       dataiso=%date             ;
132500120328       ar5dac=%dec(dataiso)     ;
132600120328       timeiso=%time             ;
132700120328       ar5orc=%dec(timeiso)     ;
132800120328       ar5ft1='R'               ;
132900120328       ar5dt1=%dec(dataiso)     ;
133000120328       ar5ft2='R'               ;
133100120328       ar5dt2=%dec(dataiso)     ;
133200120328       ar5ft3='R'               ;
133300120328       ar5dt3=%dec(dataiso)     ;
133400120328
133500120328       clear dar5gen            ;
133600120328       ENDSR                   ;
133700120329       //--------------------------------------------------------------
133800120329       //?Aggiorno record FIAR5
133900120329       //--------------------------------------------------------------
134000120329       BEGSR  UpdateAR5  ;
134100120329
134200120329 2     if not %found(fiar531c)  ;
134300120329 3       if wtitasp='S'  ;
134400120329         write  fiar5p00           ;
134500120329          else    ;
134600120329         write  fiar5000           ;
134700120329 3       endif  ;
134800120329
134900120329 x2    else   ;
135000120329 3       if wtitasp='S'  ;
135100120329          update fiar5p00           ;
135200120329           else    ;
135300120329          update fiar5000           ;
135400120329 3     endif    ;
135500120329 2     endif    ;
135600120329
135700120329       ENDSR  ;
135800120717
135900120717       //--------------------------------------------------------------
136000120717       //?Stampa copia LdV x Scannerizzazione Documenti Clienti       ?
136100120717       //--------------------------------------------------------------
136200120717       BEGSR  sr_StampaLDV;
136300120717
136400120717         // -?Aggancio tabella "JDC" per reperimento data attivazione?
136500120717         //  ?della scannerizzazione documenti cliente?
136600120717         clear  dJDC;
136700120717         clear  k05tntbe01;
136800120717         k_TBEcod = 'JDC';
136900120719         // -?NO => TASCCM se valorizzato, TASKSC altrimenti?
137000120719         // -?SÌ => TASCCM sempre e comunque?
137100120720         //if  s_TASccm <> *zero;
137200120720           k_TBEke1 = %editc( s_TASccm : 'X' );
137300120719         //else;
137400120720         //  k_TBEke1 = %editc( s_TASksc : 'X' );
137500120719         //endif;
137600120717
137700120717         chain  %kds( k05tntbe01 : 3 )  TNTBE00S;
137800120717
137900120917         if  %found(TNTBE01S)  and  TBEatb = *blank;
138000120717           dJDC = TBEuni;
138100120717         endif;
138200120717
138300120717         // -?Scannerizzazione documenti per il cliente: NON prevista?
138400120717         //  ?o non ancora in vigore o già scaduta?
138500120917         if  NOT  %found(TNTBE01S)                    or
138600120917             TBEatb <> *blank                         or
138700120717             §JDCdti > (s_TASaas * 10000) + s_TASmgs  or
138800120717             §JDCdtf < (s_TASaas * 10000) + s_TASmgs;
138900120717           leavesr;
139000120717         endif;
139100120717
139200120717         // -?Reperimento rec. "DOC" da FIAR5?
139300120717         clear  dAR5doc;
139400120717
139500120717         chain(n)  (s_TASaas : s_TASlnp : s_TASnrs : s_TASnsp : 'DOC')
139600120717                   FIAR531C;
139700120717
139800120717         if  %found(FIAR531C);
139900120717           dAR5doc = AR5uni;
140000120822         else ;
140100120822
140200120822         // -?se non trovata vedo se bolla legata: cerco la DOC dalla figlia
140300120822         if s_tascca<>' '   ;
140400120822
140500120822         lblaan=s_tasaas ;
140600120822         lbllpn=s_taslnp ;
140700120822         lblnrn=s_tasnrs ;
140800120822         lblnsn=s_tasnsp ;
140900120822         clear wtrov   ;
141000120822
141100120822         dow wtrov=' '  ;
141200120822               chain  (lblaan : lbllpn : lblnrn : lblnsn)  FNLBLpre;
141300120822               if  %found(FNLBL02L);
141400120822         chain(n)  (lblaan : lbllpn : lblnrn : lblnsn : 'DOC')
141500120822                   FIAR531C;
141600120822                  if  %found(FIAR531C);
141700120822                    dAR5doc = AR5uni;
141800120822                    wtrov='S'       ;
141900120822                  endif;
142000120822               else  ;
142100120822                wtrov='N'  ;
142200120822               endif;
142300120822
142400120822         enddo  ;
142500120717         endif;
142600120822         endif;
142700120717
142800120717         // -?Documenti già inviati al mittente?
142900120717         if  dAR5doc.§AR5jfid <> *blank;
143000120717           leavesr;
143100120717         endif;
143200120717
143300120717         // -?Stampa copia LdV?
143400120717         reset  TNSB25ds;
143500120717         //D25ges = 'T'       ?(già così)?
143600120719         D25aas = s_TASaas;
143700120719         D25lnp = s_TASlnp;
143800120719         D25nrs = s_TASnrs;
143900120719         D25nsp = s_TASnsp;
144000120717
144100120717         tnsb25r ( TNSB25ds );
144200120717
144300120720         V1Ctst  += 1;
144400120720         VisTotBo = *on;
144500120717
144600120717       ENDSR;
144700120717
144800120329       //--------------------------------------------------------------
144900120329       //?Pulizia parziale campoi video 2
145000120329       //--------------------------------------------------------------
145100120717       BEGSR  sr_InzParz02;
145200120329
145300120717         clear  V1Crmn;
145400120717         clear  V1Crma;
145500120717         clear  V1Cnsp;
145600120717         clear  V1Cpis;
145700120717
145800120717         clear  W5aas;
145900120717         clear  W5lnp;
146000120717         clear  W5nrs;
146100120717         clear  W5nsp;
146200120717         clear  b_W5aas;
146300120717         clear  b_W5lnp;
146400120717         clear  b_W5nrs;
146500120717         clear  b_W5nsp;
146600120717
146700120717         clear  V1Cdrdt;
146800120717         clear  V1Cmtvdt;
146900120717         clear  V1Cdimtv;
147000120717         V1Cfil = DUTpou;
147100120717
147200120329       ENDSR  ;
147300120717
147400120717       //--------------------------------------------------------------
147500120717       //?Operazioni finali.
147600120717       //--------------------------------------------------------------
147700120717       BEGSR  sr_RoutEnd;
147800120717
147900120717         *inLR = *on;
148000120717
148100120717         return;
148200120717
148300120717       ENDSR;
148400080807
148500080807      /end-free
148600080807
148700120717** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
148800080826Immettere il codice Cliente Mittente                                           1
148900080826Codice Cliente Mittente inesistente                                            2
149000080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
149100080826Immettere il riferimento mittente NUMERICO                                     4
149200080826Per il rif. Mittente NUMERICO esistono xx bolle: inserire col numero di Spediz 5
149300120717Nessuna bolla trovata per il cliente con particolarità restituzione documenti  6
149400080826Immettere il riferimento mittente ALFANUMERICO                                 7
149500080826Per il RifMittente ALFANUMERICO esistono xx bolle:inserire col numero di Sped  8
149600120328Impossibile annullare : data non presente!                                     9
149700110520Immettere il numero di SPEDIZIONE  o il Barcode, in alternativa               10
149800080826Numero di SPEDIZIONE inesistente                                               1
149900080826La SPEDIZIONE non appartiene al cliente mittente indicato                      2
150000120521Già presente data ricezione doc.al xx/xx/xxxx:ENTER per forzare e riaggiornare 3
150100080909La SPEDIZIONE non prevede particolarità consegna con restituzione documenti    4
150200080901La spedizione è ormai troppo vecchia: impossibile eseguire l'inventario        5
150300120521Spedizione con legame bolla:inserire ricezione documenti sulla bolla originale 6
150400120717Errore nel Barcode                                                    (LIBERO) 7
150500120330Causale motivo inesistente                                                     8
150600120521Presente motivo documenti errati "xxxxxxxx": verrà eliminato                   9
150700120920Utente non abilitato alla gestione dei doc. del cliente - telefonare in sede  20
