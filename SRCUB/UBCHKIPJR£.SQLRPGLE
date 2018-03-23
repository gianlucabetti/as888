000100170808       //==============================================================
000200170808       //?UBCHKIPJR * Visualizza ERRORI Controllo IP Address del Lavoro?
000300170808       //==============================================================
000400170808
000500170808       //--------------------------------------------------------------
000600170808       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170808       //--------------------------------------------------------------
000800170808
000900170808     /*PRM  dbgview(*source)
001000170808     /*END
001100170808
001200170808       //--------------------------------------------------------------
001300170808       //?Specifiche di controllo.                                     ?
001400170808       //--------------------------------------------------------------
001500170808
001600170808     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170808     h dftactgrp(*no)
001800170808     h bnddir('TRUL')
001900170808
002000170808       //--------------------------------------------------------------
002100170808       //?Dichiarazione file.                                          ?
002200170808       //--------------------------------------------------------------
002300170809
002400170809       // -?Utenti aziendali?
002500170809     fAZUTE01L  if   e           k disk
002600170809
002700170809       // -?Organigramma Filiale/Aziendale?
002800170809     fAZORG01L  if   e           k disk
002900170809
003000170809       // -?Controllo Acessi all'As/400?
003100170809     fAZCAA00F  Uf   e             disk
003200170808
003300170808       // -?Video Gestione?
003400170808     fubChkIPJd cf   e             workstn
003500170808     f                                     sfile( ubIPJs01 : S01nrr )
003600170809     f                                     sfile( ubIPJsW1 : SW1nrr )
003700170808     f                                     indds( IndDspF )
003800170808     f                                     infds( InfDspF )
003900170808
004000170808       //--------------------------------------------------------------
004100170808       //?Definizione costanti.                                        ?
004200170808       //--------------------------------------------------------------
004300170808
004400170808       // -?Numero di record in ciascuna videata di subfile?
004500170808     d c_SflPag        c                   const(16)
004600170808
004700170808       // -?Numero massimo di record gestibili?
004800170808     d c_MaxRec        c                   const(9999)
004900170808
005000170808       // -?Tasti funzionali a video?
005100170808     d c_F01           c                   const(x'31')
005200170808     d c_F02           c                   const(x'32')
005300170808     d c_F03           c                   const(x'33')
005400170808     d c_F04           c                   const(x'34')
005500170808     d c_F05           c                   const(x'35')
005600170808     d c_F06           c                   const(x'36')
005700170808     d c_F07           c                   const(x'37')
005800170808     d c_F08           c                   const(x'38')
005900170808     d c_F09           c                   const(x'39')
006000170808     d c_F10           c                   const(x'3A')
006100170808     d c_F11           c                   const(x'3B')
006200170808     d c_F12           c                   const(x'3C')
006300170808     d c_F13           c                   const(x'B1')
006400170808     d c_F14           c                   const(x'B2')
006500170808     d c_F15           c                   const(x'B3')
006600170808     d c_F16           c                   const(x'B4')
006700170808     d c_F17           c                   const(x'B5')
006800170808     d c_F18           c                   const(x'B6')
006900170808     d c_F19           c                   const(x'B7')
007000170808     d c_F20           c                   const(x'B8')
007100170808     d c_F21           c                   const(x'B9')
007200170808     d c_F22           c                   const(x'BA')
007300170808     d c_F23           c                   const(x'BB')
007400170808     d c_F24           c                   const(x'BC')
007500170808     d c_Enter         c                   const(x'F1')
007600170808     d c_RollDown      c                   const(x'F4')
007700170808     d c_RollUp        c                   const(x'F5')
007800170808
007900170808       //--------------------------------------------------------------
008000170808       //?Definizione schiere.                                         ?
008100170808       //--------------------------------------------------------------
008200170808
008300170808       // -?Codici Messaggio in AZCAA00F?
008400170808     d sk_CdM          s             75    dim(10)  ctdata  perrcd( 1)
008500170809     d sk_CdM_prg      s              5    dim(10)  alt(sk_CdM)
008600170808
008700170808       // -?Messaggi di errore?
008800170808     d sk_Msg          s             78    dim( 9)  ctdata  perrcd( 1)
008900170808
009000170808       //--------------------------------------------------------------
009100170808       //?Definizione aree dati.                                       ?
009200170808       //--------------------------------------------------------------
009300170808
009400170808       // -?Dati utente?
009500170808     d §AzUte        e ds                  extname(AZUTE00F)
009600170808     d                                     dtaara
009700170808     d §DatiUte      e ds                  extname(dDatiUte)
009800170808     d                                     dtaara
009900170808
010000170808       //--------------------------------------------------------------
010100170808       //?Definizione strutture dati.                                  ?
010200170808       //--------------------------------------------------------------
010300170808
010400170808       // -?Status ds?
010500170808     d Status         sds
010600170808     d   SDSpgm          *proc
010700170808     d*//SDSprm          *parms
010800170808     d*//SDSusr              254    263
010900170808
011000170808       // -?InfDS?
011100170808     d InfDspF         ds
011200170808     d   dsp_aid             369    369a
011300170808     d*//sfl_rrn             376    377i 0
011400170808     d*//min_nrr             378    379i 0
011500170808     d*//num_rcds            380    381i 0
011600170808
011700170808       // -?Indicatori su DspF?
011800170808     d IndDspF         ds                  inz
011900170808         // -?Abilitazione tasti funzionali?
012000170808     d   $F5Attivo                     n   overlay( IndDspF : 05 )
012100170808     d   $F8Attivo                     n   overlay( IndDspF : 08 )
012200170808     d   $F9Attivo                     n   overlay( IndDspF : 09 )
012300170808     d   $F12Attivo                    n   overlay( IndDspF : 12 )
012400170808     d   $F13Attivo                    n   overlay( IndDspF : 13 )
012500170808         // -?Emissione messaggio di errore?
012600170808     d   $ErrMessage                   n   overlay( IndDspF : 28 )
012700170808         // -?Indicatori di gestione del subfile?
012800170808     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
012900170808     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
013000170808     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
013100170808     d   $SflEnd                       n   overlay( IndDspF : 33 )
013200170809     d   $WSflDsp_N                    n   overlay( IndDspF : 34 )
013300170809     d   $WSflDspCt_N                  n   overlay( IndDspF : 35 )
013400170809     d   $WSflNxtChg                   n   overlay( IndDspF : 36 )
013500170809     d   $WSflEnd                      n   overlay( IndDspF : 37 )
013600170808         // -?Indicatori per Attributi di visualizzazione?
013700170809     d   $Incompat                     n   overlay( IndDspF : 40 )
013800170808     d   $Ord_0                        n   overlay( IndDspF : 41 )
013900170808     d   $Ord_1                        n   overlay( IndDspF : 42 )
014000170808     d   $Ord_2                        n   overlay( IndDspF : 43 )
014100170808     d   $Ord_3                        n   overlay( IndDspF : 44 )
014200170808     d   $Ord_4                        n   overlay( IndDspF : 45 )
014300170808     d   $Ord_5                        n   overlay( IndDspF : 46 )
014400170809     d   $ProtectOPZ                   n   overlay( IndDspF : 50 )
014500170808         // -?Posizionamento cursore & segnalazione errore?
014600170809     d   $PosCurUTE                    n   overlay( IndDspF : 51 )
014700170809     d   $PosCurDEV                    n   overlay( IndDspF : 52 )
014800170809     d   $PosCurDATi                   n   overlay( IndDspF : 53 )
014900170809     d   $PosCurORAi                   n   overlay( IndDspF : 54 )
015000170809     d   $PosCurDATf                   n   overlay( IndDspF : 55 )
015100170809     d   $PosCurORAf                   n   overlay( IndDspF : 56 )
015200170809     d   $PosCurCDM1                   n   overlay( IndDspF : 57 )
015300170809     d   $PosCurCDM2                   n   overlay( IndDspF : 58 )
015400170809     d   $PosCurCDM3                   n   overlay( IndDspF : 59 )
015500170809     d   $PosCurCDM4                   n   overlay( IndDspF : 60 )
015600170809     d   $PosCurCDM5                   n   overlay( IndDspF : 61 )
015700170809     d   $PosCurCDM6                   n   overlay( IndDspF : 62 )
015800170809     d   $PosCurCDM7                   n   overlay( IndDspF : 63 )
015900170809     d   $PosCurCDM8                   n   overlay( IndDspF : 64 )
016000170809     d   $PosCurCDM9                   n   overlay( IndDspF : 65 )
016100170809     d   $PosCurCDMA                   n   overlay( IndDspF : 66 )
016200170809     d*//$PosCurTPM                    n   overlay( IndDspF : 67 )
016300170809     d   $PosCurSOLO                   n   overlay( IndDspF : 68 )
016400170808     d   $PosCurOPZ                    n   overlay( IndDspF : 70 )
016500170808         // -?Riemissione videata?
016600170808     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
016700170808
016800170808       // -?Parametri ricevuti?
016900170808     d V1Ccdm_ds       ds                  inz
017000170808     d   V1Ccdm1                           inz
017100170808     d   V1Ccdm2                           inz
017200170808     d   V1Ccdm3                           inz
017300170808     d   V1Ccdm4                           inz
017400170808     d   V1Ccdm5                           inz
017500170808     d   V1Ccdm6                           inz
017600170808     d   V1Ccdm7                           inz
017700170808     d   V1Ccdm8                           inz
017800170808     d   V1Ccdm9                           inz
017900170808     d   V1CcdmA                           inz
018000170808     d   sk_V1Ccdm             1     30  0 inz  dim(10)
018100170809
018200170809       // -?Parametri ricevuti?
018300170809     d WSMora_ds       ds                  inz   qualified
018400170809     d   WSMhh                        2s 0 inz
018500170809     d   WSMmm                        2s 0 inz
018600170809     d   WSMss                        2s 0 inz
018700170809     d   WSMora                1      6s 0 inz
018800170808
018900170808       // -?Parametri ricevuti?
019000170808     d KPJBA         e ds
019100170809
019200170809       // -?Descrizione 143 dell'Organigramma?
019300170809     d Og143         e ds                  inz   qualified
019400170808
019500170808       // -?Dati estratti via SQL?
019600170808     d AZCAAds       e ds                  inz  extname(AZCAA00F)
019700170808     d                                          qualified
019800170808     d wCAArrn         s             11  0 inz
019900170808
020000170808       //--------------------------------------------------------------
020100170808       //?Definizione variabili globali.                               ?
020200170808       //--------------------------------------------------------------
020300170808
020400170808       // -?Flags booleani?
020500170808     d $Fine           s               n   inz
020600170808     d $EoF            s               n   inz
020700170808     d $First          s               n   inz
020800170808     d $First_cdm      s               n   inz
020900170808     d $InzD01         s               n   inz(*on)
021000170808     d $InzS01         s               n   inz(*on)
021100170808     d $InzSW1         s               n   inz(*on)
021200170809     d $ForzaOpz       s               n   inz
021300170809     d $Conferma       s               n   inz
021400170808
021500170808       // -?Variabili per la gestione del video?
021600170808     d $Video          s              2    inz('S1')
021700170808     d $VideoPrec      s              2    inz
021800170808     d W_SflPag        s              3  0 inz
021900170808     d S01nrr          s                   inz   like(C1RcdNbr)
022000170808     d SW1nrr          s                   inz   like(C1RcdNbr)
022100170808     d SavS1Copz       s                   inz   like(S1Copz)
022200170809     d SavS1Cop1       s                   inz   like(S1Copz)
022300170808     d SavS01csr       s                   inz   like(C1RcdNbr)
022400170809     d SavS01cs1       s                   inz   like(C1RcdNbr)
022500170809     d SavS01cs2       s                   inz   like(C1RcdNbr)
022600170808     d wPag            s              4  0 inz
022700170808     d wRig            s              3  0 inz
022800170808     d wOrdSfl         s              1  0 inz
022900170809
023000170809       // -?Indici di Schiera?
023100170809     d xx              s              3  0 inz
023200170809
023300170809       // -?Filiale reperita dall'IP address?
023400170809     d wIPfil          s                   inz   like(ORGfil)
023500170808
023600170808       // -?Stringa SQL da eseguire?
023700170808     d wSql            s          32740    inz   varying
023800170808
023900170808       // -?Campi di comodo?
024000170808     d W1CdatI         s                   inz   like(V1CdatI)
024100170808     d W1CdatF         s                   inz   like(V1CdatF)
024200170808
024300170808       //--------------------------------------------------------------
024400170808       //?Definizione prototipi procedure usate.                       ?
024500170808       //--------------------------------------------------------------
024600170808
024700170808       // -?Reperimento dati utente?
024800170808     d TIBS34ds      e ds                  inz
024900170808      /copy gaitrasrc/srcProtoPR,TIBS34R
025000170808
025100170808       // -?Caricamento filiali gestite dall'utente?
025200170809     d TRUL06ds      e ds                  inz
025300170809     d   sk_POg                1     90    inz   dim(30)
025400170809      /copy gaitrasrc/srcProtoPR,TRUL06R
025500170809
025600170809       // -?Visualizzazione profilo utente - dettaglio?
025700170809     d TIBS31ds      e ds                  inz   qualified
025800170809     d TIBS31R         pr                  extpgm('TIBS31R')
025900170809     d   kpjba                             likeds(KPJBA)
026000170809     d   TIBS31ds                          likeds(TIBS31ds)
026100170808
026200170808       // -?Controllo formale / Inversione data?
026300170808     d WLBdat          ds                  inz
026400170809     d   G08dat                1      8  0 inz
026500170809     d   G08inv                9     16  0 inz
026600170809     d   G08err               17     17    inz('3')
026700170809     d   G08tgi               18     22  0 inz
026800170808      /copy gaitrasrc/srcProtoPR,XSRDA8
026900170808
027000170808       // -?API QCAPCMD (Process Commands)?
027100170808     d Qcmd            s           2048    inz  varying
027200170808      /copy qSysInc/qRpgleSrc,QCAPCMD
027300170808      /copy gaitrasrc/srcProtoPR,QCAPCMD
027400170808
027500170808       // -?Parametri gestione errori API.?
027600170808      /copy qsysinc/qRpgleSrc,QUSEC
027700170808
027800170808       //--------------------------------------------------------------
027900170808       //?Definizione key-list.                                        ?
028000170808       //--------------------------------------------------------------
028100170808
028200170808
028300170808       //--------------------------------------------------------------
028400170808       //?Riepilogo indicatori utilizzati.                             ?
028500170808       //--------------------------------------------------------------
028600170808       //--------------------------------------------------------------
028700170808
028800170808       //--------------------------------------------------------------
028900170808       //?M A I N - L I N E .                                          ?
029000170808       //--------------------------------------------------------------
029100170808
029200170808     c     *Entry        plist
029300170808     c                   parm                    KPJBA
029400170808
029500170808      /free
029600170808
029700170808       // -?Operazioni iniziali?
029800170808       exsr sr_RoutInz;
029900170808
030000170808       // -?Ciclo di gestione del file video?
030100170808       DoW  $Fine = *off;
030200170808
030300170808         select;
030400170808
030500170808           // -?Gestione videata D1 (Filtro di lancio)?
030600170808           when  $Video = 'D1';
030700170808             exsr  sr_GesD01;
030800170808
030900170808           // -?Gestione videata S1 (SubFile)?
031000170808           when  $Video = 'S1';
031100170808             exsr  sr_GesS01;
031200170808
031300170808           // -?Gestione window W1?
031400170808           when  $Video = 'W1';
031500170808             exsr  sr_GesSW1;
031600170808
031700170808           // -?? ? ? ? ??
031800170808           other;
031900170808             $Fine = *on;
032000170808
032100170808         endsl;
032200170808
032300170808       EndDo;
032400170808
032500170808       // -?Operazioni finali?
032600170808       exsr sr_RoutEnd;
032700170808
032800170808       //--------------------------------------------------------------
032900170808       //?Operazioni iniziali.                                         ?
033000170808       //--------------------------------------------------------------
033100170808       BEGSR  sr_RoutInz;
033200170808
033300170808         // -?Impostazione opzioni per SQL?
033400170808         exec sql   set  option  DynUsrPrf = *Owner,
033500170808                                 CloSqlCsr = *EndMod;
033600170808
033700170808         // -?Impostazione chiusura?
033800170808         *inLR = *on;
033900170808         reset  $Fine;
034000170808
034100170808         // -?Reperimento dati job?
034200170808         exsr  sr_DatiJob;
034300170808
034400170808         // -?Impostazione nome programma a video?
034500170808         V1Tpgm = SDSpgm;
034600170808
034700170808         // -?Impostazione videata iniziale?
034800170808         clear  UBIPJD01;
034900170808         clear  UBIPJC01;
035000170808         $Video = 'D1';
035100170808         reset  $InzD01;
035200170808         reset  $InzS01;
035300170808
035400170808       ENDSR;
035500170808
035600170808       //--------------------------------------------------------------
035700170808       //?Reperimento Dati del job (Utente/Operativi).                 ?
035800170808       //--------------------------------------------------------------
035900170808       BEGSR  sr_DatiJob;
036000170808
036100170808         in(e) §AzUte;
036200170808         if NOT %error;
036300170808           in(e) §DatiUte;
036400170808         endif;
036500170808         if %error or RSut = *blank;
036600170808           tibs34r ( tibs34ds );
036700170808           in §AzUte;
036800170808           in §DatiUte;
036900170808         endif;
037000170808
037100170808       ENDSR;
037200170808
037300170808       //--------------------------------------------------------------
037400170808       //?Gestione videata D01.                                        ?
037500170808       //--------------------------------------------------------------
037600170808       BEGSR  sr_GesD01;
037700170808
037800170808         // -?Inizializzazione videata?
037900170808         if  $InzD01 = *on;
038000170808           exsr  sr_InzD01;
038100170808           $InzD01 = *off;
038200170808         endif;
038300170808
038400170808         // -?Emissione Testata?
038500170808         write  ubIPJt01;
038600170808
038700170808         // -?Emissione videata (con Piede = tasti funzionali abilitati)?
038800170808         exfmt  ubIPJd01;
038900170808
039000170808         clear  VIDmsg;
039100170808         reset  $ErrMessage;
039200170808         reset  $ErrGenerico;
039300170808
039400170808         Select;
039500170808
039600170808           // -?F1=Codici Errore?
039700170808           When  dsp_aid = c_F01;
039800170808             $VideoPrec = $Video;
039900170808             $Video     = 'W1';
040000170808             $InzSW1    = *on;
040100170808
040200170808           // -?F3=Fine?
040300170808           When  dsp_aid = c_F03;
040400170808             $Fine  = *on;
040500170808
040600170808           // -?Invio?
040700170808           Other;
040800170808             exsr  sr_CtrD01;
040900170808             if  $ErrGenerico;
041000170808               leavesr;
041100170808             endif;
041200170808             $Video  = 'S1';
041300170808             $InzS01 = *on;
041400170808
041500170808         EndSl;
041600170808
041700170808       ENDSR;
041800170808
041900170808       //--------------------------------------------------------------
042000170808       //?Inizializzazione videata D01.                                ?
042100170808       //--------------------------------------------------------------
042200170808       BEGSR  sr_InzD01;
042300170808
042400170808         // -?Spegnimento indicatori di errore e posizionamento cursore?
042500170808         %subst( IndDspF : 28 ) = *off;
042600170808
042700170808         // -?Pulizia videata?
042800170808         clear  ubIPJd01;
042900170808
043000170808         // -?Impostazione iniziale dei campi a video?
043100170808         //*·V1Ccdm1  = 1;
043200170809         V1Ccdm2  = 2;
043300170808         V1Ccdm3  = 3;
043400170808         //*·V1Ccdm4  = 4;
043500170808         V1Ccdm5  = 5;
043600170808         V1Ccdm6  = 6;
043700170808         //*·V1Ccdm7  = 7;
043800170808         //*·V1Ccdm8  = 8;
043900170808         V1Ccdm9  = 9;
044000170808         V1CcdmA  = 10;
044100170808
044200170808       ENDSR;
044300170808
044400170808       //--------------------------------------------------------------
044500170808       //?Inizializzazione videata D01.                                ?
044600170808       //--------------------------------------------------------------
044700170808       BEGSR  sr_CtrD01;
044800170808
044900170808         // -?Spegnimento indicatori di errore e posizionamento cursore?
045000170808         %subst( IndDspF : 50 ) = *off;
045100170808
045200170808         // -?Data Collegamento iniziale?
045300170808         clear  WLBdat;
045400170808         if  V1CdatI <> *zero;
045500170808           G08dat = V1CdatI;
045600170808           xsrda8 ( WLBdat );
045700170808           if  G08err =  *on;
045800170809             $PosCurDATi  = *on;
045900170809             $ErrMessage  = *on;
046000170809             $ErrGenerico = *on;
046100170809             VIDmsg = sk_Msg(01);
046200170808             leavesr;
046300170808           endif;
046400170808           V1CdatI = G08dat;
046500170808         endif;
046600170808         W1CdatI = G08inv;
046700170808
046800170808         // -?Ora  Collegamento iniziale?
046900170808         clear  WSMora_ds;
047000170808         if  V1CoraI <> *zero;
047100170808           WSMora_ds.WSMora = V1CoraI;
047200170808           if  WSMora_ds.WSMhh < *zero  or  WSMora_ds.WSMhh > 23  or
047300170808               WSMora_ds.WSMmm < *zero  or  WSMora_ds.WSMmm > 59  or
047400170808               WSMora_ds.WSMss < *zero  or  WSMora_ds.WSMss > 59;
047500170809             $PosCurORAi  = *on;
047600170809             $ErrMessage  = *on;
047700170809             $ErrGenerico = *on;
047800170809             VIDmsg = sk_Msg(02);
047900170808             leavesr;
048000170808           endif;
048100170808         endif;
048200170808
048300170808         // -?Data Collegamento finale?
048400170808         clear  WLBdat;
048500170808         if  V1CdatF <> *zero;
048600170808           G08dat = V1CdatF;
048700170808           xsrda8 ( WLBdat );
048800170808           if  G08err =  *on;
048900170809             $PosCurDATf  = *on;
049000170809             $ErrMessage  = *on;
049100170809             $ErrGenerico = *on;
049200170809             VIDmsg = sk_Msg(01);
049300170808             leavesr;
049400170808           endif;
049500170808           V1CdatF = G08dat;
049600170808         endif;
049700170808         W1CdatF = G08inv;
049800170808
049900170808         // -?Ora  Collegamento finale?
050000170808         clear  WSMora_ds;
050100170808         if  V1CoraF <> *zero;
050200170809           WSMora_ds.WSMora = V1CoraF;
050300170808           if  WSMora_ds.WSMhh < *zero  or  WSMora_ds.WSMhh > 23  or
050400170808               WSMora_ds.WSMmm < *zero  or  WSMora_ds.WSMmm > 59  or
050500170808               WSMora_ds.WSMss < *zero  or  WSMora_ds.WSMss > 59;
050600170809             $PosCurORAf  = *on;
050700170809             $ErrMessage  = *on;
050800170809             $ErrGenerico = *on;
050900170809             VIDmsg = sk_Msg(02);
051000170808             leavesr;
051100170808           endif;
051200170808         endif;
051300170808
051400170808         // -?Codici Errore?
051500170809         if  V1Ccdm_ds <> *zero;
051600170808           For  xx = 1  To  %elem( sk_V1Ccdm );
051700170809             //*·sk_V1Ccdm(xx) = %abs( sk_V1Ccdm(xx) );
051800170809             select;
051900170809               when  sk_V1Ccdm(xx) = *zero;
052000170809               when  sk_V1Ccdm(xx) > *zero  and
052100170809                     sk_V1Ccdm(xx) < %elem( sk_V1Ccdm );
052200170809                 if  %subst( sk_CdM_prg( sk_V1Ccdm(xx) ) : 1 : 3 ) = '|--';
052300170809                   %subst( IndDspF : 55+xx ) = *on;
052400170809                   $ErrMessage  = *on;
052500170809                   $ErrGenerico = *on;
052600170809                   VIDmsg = sk_Msg(03);
052700170809                   leavesr;
052800170809                 endif;
052900170809               other;
053000170809                 VIDmsg = sk_Msg(03);
053100170809             endsl;
053200170808           EndFor;
053300170808         endif;
053400170808
053500170809         // -?Flag "Solo Utenti di Filiale con IP Address incompatibile"?
053600170809         if  V1Csolo <> *blank  and  V1Csolo <> 'S';
053700170809           $PosCurSOLO  = *on;
053800170809           $ErrMessage  = *on;
053900170809           $ErrGenerico = *on;
054000170809           VIDmsg = sk_Msg(04);
054100170808           leavesr;
054200170808         endif;
054300170808
054400170808       ENDSR;
054500170808
054600170808       //--------------------------------------------------------------
054700170808       //?Gestione subfile S01.                                        ?
054800170808       //--------------------------------------------------------------
054900170808       BEGSR  sr_GesS01;
055000170808
055100170808         // -?Inizializzazione videata?
055200170808         if  $InzS01 = *on;
055300170808           exsr  sr_InzS01;
055400170808           $InzS01 = *off;
055500170808         endif;
055600170808
055700170808         // -?(Dis)Abilitazioni CONDIZIONATE tasti funzionali in P01 (per C01)?
055800170808         $F8Attivo  = ( S01nrr > 1 );
055900170808         $F9Attivo  = ( S01nrr > *zero );
056000170808         $F13Attivo = $F8Attivo;
056100170808
056200170808         // -?Emissione Testata & Piede con tasti funzionali abilitati?
056300170808         write  ubIPJt01;
056400170808         write  ubIPJp01;
056500170808
056600170808         // -?Posizionamento cursore?
056700170808         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
056800170808         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
056900170808         //  ?si visualizza la pagina che vedeva l'utente quando ha?
057000170808         //  ?premuto l'ultimo tasto.?
057100170808         if  C1CsrRrn > *zero;
057200170808           C1RcdNbr = C1CsrRrn;
057300170808         else;
057400170808           C1RcdNbr = 1;
057500170808           write  ubIPJs00;         //(no rec.)?
057600170808         endif;
057700170808
057800170808         // -?Emissione videata?
057900170808         exfmt  ubIPJc01;
058000170808
058100170809         clear  VIDmsg;
058200170808         reset  $ErrMessage;
058300170808         reset  $ErrGenerico;
058400170809
058500170809         reset  $Conferma;
058600170809
058700170809         if  dsp_aid <> c_F03  and
058800170809             dsp_aid <> c_F13;
058900170809           clear  SavS01csr;
059000170809           clear  SavS01cs1;
059100170809           clear  SavS01cs2;
059200170809           clear  SavS1Copz;
059300170809           clear  SavS1Cop1;
059400170809         endif;
059500170808
059600170808         SELECT;
059700170808
059800170808           // -?F1=Window Codici Errore?
059900170808           WHEN  dsp_aid = c_F01;
060000170808             $VideoPrec = $Video;
060100170808             $Video     = 'W1';
060200170808             $InzSW1    = *on;
060300170808
060400170808           // -?F3=Fine?
060500170808           WHEN  dsp_aid = c_F03;
060600170808             exsr  sr_CloseCursor;
060700170808             $Fine = *on;
060800170808
060900170808           // -?F5=Refresh?
061000170809           WHEN  dsp_aid = c_F05;
061100170808             exsr  sr_CloseCursor;
061200170808             $InzS01 = *on;
061300170808
061400170808           // -?F8=Ordinamento?
061500170808           WHEN  dsp_aid = c_F08;
061600170808             exsr  sr_F08S01;
061700170808
061800170808           // -?F12=Ritorno?
061900170808           WHEN  dsp_aid = c_F12;
062000170808             exsr  sr_CloseCursor;
062100170808             $Video = 'D1';
062200170808
062300170808           // -?F13=Ripetizione Opzione?
062400170808           WHEN  dsp_aid = c_F13;
062500170808             exsr  sr_F13S01;
062600170808
062700170808           // -?Roll-Up?
062800170808           WHEN  dsp_aid = c_RollUp;
062900170808             if  S01nrr >= c_MaxRec;
063000170809               $ErrGenerico = *on;
063100170809               $ErrMessage  = *on;
063200170809               VIDmsg = sk_Msg(05);
063300170808             else;
063400170808               exsr sr_RollUpS01;
063500170808             endif;
063600170808
063700170808           // -?SubFile vuoto?
063800170808           WHEN  S01nrr = *zero;
063900170808
064000170808           // -?Invio?
064100170808           OTHER;
064200170808             // -?Gestione opzioni?
064300170808             if  S01nrr > *zero;
064400170808               exsr  sr_OpzS01;
064500170809               if  $ErrGenerico;
064600170808                 leavesr;
064700170808               endif;
064800170808             endif;
064900170808
065000170808         ENDSL;
065100170808
065200170808       ENDSR;
065300170808
065400170808       //--------------------------------------------------------------
065500170808       //?Inizializzazione subfile S01.                                ?
065600170808       //--------------------------------------------------------------
065700170808       BEGSR  sr_InzS01;
065800170808
065900170808         // -?Spegnimento degli indicatori di errore?
066000170808         %subst( IndDspF : 50 ) = *off;
066100170808
066200170808         // -?Pulizia subfile?
066300170808         $SflDsp_N    = *on;
066400170808         $SflDspCtl_N = *on;
066500170808         write  ubIPJc01;
066600170808         $SflDspCtl_N = *off;
066700170808         $SflEnd      = *off;
066800170808
066900170809         clear  W_SflPag;
067000170808         clear  C1RcdNbr;
067100170808         clear  C1CsrRrn;
067200170808         clear  S01nrr;
067300170809         clear  VIDmsg;
067400170808         $ErrMessage  = *off;
067500170808         $ErrGenerico = *off;
067600170808
067700170808         // -?Preparazione stringa SQL?
067800170808         exsr  sr_PrepSQL;
067900170808
068000170808         // -?Caricamento della 1ª pagina di dati del subfile?
068100170808         exsr  sr_OpenCursor;
068200170808         exsr  sr_ReadCursor;
068300170808         exsr  sr_RollUpS01;
068400170808
068500170808         // -?Impaginazione subfile?
068600170808         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
068700170808         if S01nrr > *zero;
068800170808           C1RcdNbr  = 1;
068900170808           C1CsrRrn  = 1;
069000170808         else;
069100170808           clear C1RcdNbr;
069200170808         endif;
069300170808
069400170808         // -?(Dis)Abilitazione tasti funzionali?
069500170808         // ·?F8 = Ordinamento x ...?
069600170808         $F8Attivo  = ( S01nrr > 1 );
069700170808         // ·?F9 = Altri Dati?
069800170808         $F9Attivo  = ( S01nrr > *zero );
069900170808         // ·?F13 = Ripetizione opz.?
070000170808         $F13Attivo = $F8Attivo;
070100170808
070200170808         // -?Impostazione F8 nel piede del subfile?
070300170808         select;
070400170808           when  wOrdSfl = *zero;
070500170808             V1PF08 = 'F8=Ord. x Utente    ';
070600170808           when  wOrdSfl = 1;
070700170808             V1PF08 = 'F8=Ord. x Device    ';
070800170808           when  wOrdSfl = 2;
070900170808             V1PF08 = 'F8=Ord. x IP Address';
071000170808           when  wOrdSfl = 3;
071100170808             V1PF08 = 'F8=Ord. x Data/Ora  ';
071200170808           when  wOrdSfl = 4;
071300170808             V1PF08 = 'F8=Ord. x Cod.Msg   ';
071400170808           when  wOrdSfl = 5;
071500170808             V1PF08 = 'F8=Ord. x Tipo Msg  ';
071600170808         endsl;
071700170808
071800170808       ENDSR;
071900170808
072000170808       //--------------------------------------------------------------
072100170808       //?Caricamento singola pagina nel subfile S01.                  ?
072200170808       //--------------------------------------------------------------
072300170808       BEGSR  sr_RollUpS01;
072400170808
072500170809         S01nrr     = (W_SflPag * c_SflPag);
072600170809         W_SflPag  += 1;
072700170809         $SflNxtChg = *off;
072800170808
072900170808         // -?Ciclo di caricamento dati nel subfile?
073000170808         DoW  S01nrr  <  (W_SflPag * c_SflPag)  and
073100170808              S01nrr  <  c_MaxRec               and
073200170809              SQLcode >= *zero                  and
073300170808              SQLcode <> 100;
073400170808
073500170808           clear  ubIPJs01;
073600170808           exsr  sr_RieS01;
073700170808
073800170808           // -?Scrittura record nel subfile S01?
073900170809           if  V1Csolo = *blank  or  $Incompat;
074000170809             S01nrr += 1;
074100170809             write  ubIPJs01;
074200170809           endif;
074300170808
074400170808           // -?Lettura record successivo?
074500170808           exsr  sr_ReadCursor;
074600170808
074700170808         EndDo;
074800170808
074900170808         // -?Visualizzazione del SFL (se ci sono dati)?
075000170809         $SflDsp_N = ( S01nrr <= *zero );
075100170808
075200170808         // -?Attivazione del SFLEND?
075300170809         $SflEnd = $EoF;
075400170808         if  S01nrr >= c_MaxRec;
075500170809           $ErrMessage  = *on;
075600170809           $ErrGenerico = *on;
075700170809           VIDmsg = sk_Msg(05);
075800170808         endif;
075900170808
076000170808         // -?Posizionamento cursore al 1° rcd della pagina?
076100170808         if  S01nrr > *zero;
076200170808           wPag     = S01nrr / c_SflPag;
076300170808           wRig     = S01nrr - (c_SflPag * wPag);
076400170808           C1RcdNbr = wPag * c_SflPag;
076500170808           if  wRig > *zero;
076600170808             C1RcdNbr += 1;
076700170808           else;
076800170808             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
076900170808           endif;
077000170808         else;
077100170808           clear  C1RcdNbr;
077200170808         endif;
077300170808
077400170808         C1CsrRrn = C1RcdNbr;
077500170808
077600170808       ENDSR;
077700170808
077800170808       //--------------------------------------------------------------
077900170808       //?Valorizzazione campi del singolo record nel subfile S01.     ?
078000170808       //--------------------------------------------------------------
078100170808       BEGSR  sr_RieS01;
078200170808
078300170808         // -?Campi Hidden?
078400170808         S1Hrrn     = wCAArrn;
078500170809         S1Htim     = AZCAAds.CAAtim;
078600170808
078700170808         // -?Campi di Output?
078800170808         S1Cute   = AZCAAds.CAAute;
078900170808         S1Cdev   = AZCAAds.CAAdev;
079000170808         S1Cip    = AZCAAds.CAAip;
079100170808
079200170808         reset  WLBdat;
079300170808         G08inv = AZCAAds.CAAtim / 1000000;
079400170808         xsrda8 ( WLBdat );
079500170808         if  G08err = *off;
079600170808           S1Cdat = G08dat;
079700170808         endif;
079800170808
079900170808         S1Cora   = AZCAAds.CAAtim - ( G08inv * 1000000 );
080000170808         S1Ccdm   = AZCAAds.CAAcdm;
080100170808         S1Ctpm   = AZCAAds.CAAtpm;
080200170808         S1Cnot   = AZCAAds.CAAnot;
080300170809
080400170809         // -?Controllo compatibilità di Filiale Utente con IP Address?
080500170809         if  AZCAAds.CAAcdm = 10;
080600170809           exsr  sr_Chk_dutPOU;
080700170809         endif;
080800170808
080900170808         // -?Impostazione attributi di visualizzazione?
081000170809         exsr  sr_DspAttrS01;
081100170808
081200170808       ENDSR;
081300170808
081400170808       //--------------------------------------------------------------
081500170808       //?Gestione opzioni del subfile S01                             ?
081600170808       //--------------------------------------------------------------
081700170808       BEGSR  sr_OpzS01;
081800170808
081900170808         // -?Spegnimento degli indicatori di errore?
082000170809         %subst( IndDspF : 50 ) = *off;
082100170809
082200170808         clear  SavS01csr;
082300170808
082400170808         // -?Ciclo di lettura subfile?
082500170809         readc  ubIPJs01;
082600170808
082700170809         DoW  Not %eof( ubChkIPJd );
082800170808
082900170809           $SflNxtChg = *off;
083000170809           %subst( IndDspF : 50 ) = *off;
083100170809           C1RcdNbr   = S01nrr;
083200170808
083300170808           select;
083400170808
083500170808             // -?Nessuna opzione?
083600170809             when  S1Copz = *zero;
083700170808
083800170808             // -?Opz. 4=Cancellazione registrazione collegamento?
083900170809             when  S1Copz = 4;
084000170809               exsr  sr_Opz04;
084100170809
084200170809             // -?Opz. 5=Visualizzazione utente?
084300170809             when  S1Copz = 5;
084400170809               exsr  sr_Opz05;
084500170808
084600170808             // -?Opzione errata?
084700170808             other;
084800170809               $ErrGenerico = *on;
084900170809               $ErrMessage  = *on;
085000170809               $PosCurOPZ   = *on;
085100170809               VIDmsg = sk_Msg(09);
085200170808
085300170808           endsl;
085400170808
085500170808           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
085600170808           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
085700170808           if  S1Copz   <> *zero  and
085800170808              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
085900170808             SavS01csr   = C1RcdNbr;
086000170808           endif;
086100170808
086200170808           // -?Aggiornamento sfl?
086300170808           select;
086400170808             when  $ErrMessage;
086500170809               $SflNxtChg  = *on;
086600170809               C1CsrRrn    = C1RcdNbr;
086700170808             when  $ErrGenerico;
086800170809               C1CsrRrn    = C1RcdNbr;
086900170808               clear  S1Copz;
087000170808             other;
087100170809               if  S1Copz = 4;
087200170809                 $InzS01 = *on;
087300170809               endif;
087400170809               $ProtectOPZ = ( S1Copz = 4 );
087500170809               clear  S1Copz;
087600170808           endsl;
087700170808
087800170808           exsr  sr_DspAttrS01;
087900170808           UPDATE  ubIPJs01;
088000170808
088100170809           if  $ErrGenerico  or  $ErrMessage;
088200170808             leave;
088300170808           endif;
088400170808
088500170808           readc  ubIPJs01;
088600170808
088700170808         ENDDO;
088800170808
088900170808         // -?Riposizionam. cursore sul record contenente la prima opz.?
089000170808         //  ?(se non sono stati rilevati errori)?
089100170808         if  NOT $ErrMessage   and   NOT $ErrGenerico   and
089200170808             SavS01csr > *zero;
089300170808           C1CsrRrn = SavS01csr;
089400170808         endif;
089500170808
089600170808         // -?Se richiesto il ri-caricamento del subfile: occorre?
089700170808         //  ?prima chiudere il cursore SQL (attualmente aperto)?
089800170808         if  $InzS01 = *on;
089900170808           exsr  sr_CloseCursor;
090000170808         endif;
090100170808
090200170808       ENDSR;
090300170808
090400170808       //--------------------------------------------------------------
090500170808       //?Impostazione attributi di visualizzazione nel subfile S1.    ?
090600170808       //--------------------------------------------------------------
090700170808       BEGSR  sr_DspAttrS01;
090800170808
090900170809         // -?Filiale Utente incompatibile con IP Address?
091000170809         $Incompat = ( S1Hin40 = *on );
091100170808
091200170808         // -?Ord. 0 = Utente?
091300170809         $Ord_0    = ( wOrdSfl = *zero );
091400170808
091500170808         // -?Ord. 1 = Device?
091600170809         $Ord_1    = ( wOrdSfl = 1 );
091700170808
091800170808         // -?Ord. 2 = IP Address?
091900170809         $Ord_2    = ( wOrdSfl = 2 );
092000170808
092100170808         // -?Ord. 3 = Data/Ora?
092200170809         $Ord_3    = ( wOrdSfl = 3 );
092300170808
092400170808         // -?Ord. 4 = Codice Messaggio?
092500170809         $Ord_4    = ( wOrdSfl = 4 );
092600170808
092700170808         // -?Ord. 5 = Tipo Messaggio?
092800170809         $Ord_5    = ( wOrdSfl = 5 );
092900170808
093000170808       ENDSR;
093100170808
093200170808       //--------------------------------------------------------------
093300170808       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
093400170808       //?F08-Cambia ordinamento sfl                                   ?
093500170808       //--------------------------------------------------------------
093600170808       BEGSR  sr_F08S01;
093700170809
093800170809         exsr  sr_CloseCursor;
093900170809
094000170809         select;
094100170809           // -?Subfile vuoto: nessun record da ordinare?
094200170809           when  S01nrr <= 1;
094300170809             clear  V1Pf08;
094400170809             leavesr;
094500170809           // -?Cambio ordinamento?
094600170809           when  wOrdSfl = 5;
094700170809             clear  wOrdSfl;
094800170809           other;
094900170809             wOrdSfl += 1;
095000170809         endsl;
095100170809
095200170809         $InzS01 = *on;
095300170808
095400170808       ENDSR;
095500170808
095600170808       //--------------------------------------------------------------
095700170808       //?Gestione tasto funzionale F13 da videata C01 / S01           ?
095800170808       //?F13-Ripetizione opzione                                      ?
095900170808       //--------------------------------------------------------------
096000170808       BEGSR  sr_F13S01;
096100170808
096200170808         // -?Reperimento pagina del sfl (e Nrr del suo primo record)?
096300170808         //  ?dalla posizione del cursore?
096400170808         wPag = C1CsrRrn / c_SflPag;
096500170808         wRig = %rem( C1CsrRrn : c_SflPag);
096600170808         if  wRig > *zero;
096700170808           wRig = (wPag * c_SflPag) + 1;
096800170808         else;                           // -?ultimo rec. della pag.?
096900170808           wRig = (wPag * c_SflPag) - c_SflPag + 1;
097000170808         endif;
097100170808         SavS01csr = wRig;
097200170808
097300170808         // -?CONTROLLO OPZIONI INSERITE NEL SUBFILE:?
097400170808
097500170808         // -?Reperimento della 1ª opzione inserita?
097600170808         //  ?nella pagina dov'è il cursore?
097700170809         readc  ubIPJs01;
097800170809         DoW  Not %eof(ubChkIPJd)  and  S1Copz <> *zero  and
097900170808              S01nrr < SavS01csr;
098000170809           $SflNxtChg = *on;
098100170809           update  ubIPJs01;
098200170809           readc  ubIPJs01;
098300170808         EndDo;
098400170809         if  Not %eof(ubChkIPJd)  and  S1Copz <> *zero;
098500170809           $SflNxtChg = *on;
098600170809           update  ubIPJs01;
098700170808         endif;
098800170808
098900170808         // -?Nessuna opzione nella pagina?
099000170809         if  %eof(ubChkIPJd)       or
099100170808             S01nrr <  SavS01csr   or
099200170808             S01nrr > (SavS01csr + c_SflPag - 1);
099300170808           C1RcdNbr  = SavS01csr;
099400170808           C1CsrRrn  = SavS01csr;
099500170808           clear  $ForzaOpz;
099600170808           clear  SavS1Copz;
099700170808           clear  SavS1Cop1;
099800170809           clear  SavS01cs1;
099900170809           clear  SavS01cs2;
100000170809           $ErrGenerico = *on;
100100170809           $ErrMessage  = *on;
100200170809           $PosCurOPZ   = *on;
100300170809           VIDmsg = sk_Msg(06);
100400170808           leavesr;
100500170808         endif;
100600170808
100700170808         // -?Memorizzazione 1ª opzione nella pagina?
100800170808         SavS01csr = S01nrr;
100900170808         SavS1Copz = S1Copz;
101000170808         if  S01nrr <> SavS01cs1  or
101100170808             S1Copz <> SavS1Cop1;
101200170808           clear  $ForzaOpz;
101300170808         endif;
101400170808         SavS01cs1 = S01nrr;
101500170808         SavS1Cop1 = S1Copz;
101600170808
101700170808         // -?Reperimento delle altre eventuali opzioni?
101800170809         readc  ubIPJs01;
101900170809         DOW  Not %eof(ubChkIPJd);
102000170808
102100170808           // ·?Aggiornamento subfile?
102200170808           if  S1Copz <> *zero;
102300170809             $SflNxtChg = *on;
102400170809             update  ubIPJs01;
102500170808           endif;
102600170808
102700170808           // ·?Reperita una 2ª opzione?
102800170808           Select;
102900170808
103000170808             // ·?Vuota!?
103100170808             When  S1Copz = *zero;
103200170808
103300170808             // ·?1ª opzione nella pagina del cursore  e?
103400170808             //  ?2ª opzione successiva diversa:?
103500170808             //  ?messaggio di avviso di "ricopertura"?
103600170808             When  (SavS01csr >= wRig              and
103700170808                    S01nrr    >  SavS01csr)        and
103800170808                    S1Copz    <> SavS1Copz         and
103900170808                    Not $ForzaOpz;
104000170808               $ForzaOpz = *on;
104100170808               SavS01cs2 = S01nrr;
104200170808               C1RcdNbr  = SavS01csr;
104300170808               C1CsrRrn  = SavS01csr;
104400170809               $ErrGenerico = *on;
104500170809               $ErrMessage  = *on;
104600170809               $PosCurOPZ   = *on;
104700170809               VIDmsg = sk_Msg(07);
104800170808               leavesr;
104900170808
105000170808             // ·?2ª opzione sul record dove è posizionato il cursore?
105100170808             //  ?(la nuova da considerare per la duplicazione)?
105200170808             When  C1CsrRrn = S01nrr;
105300170808               SavS01csr = S01nrr;
105400170808               SavS1Copz = S1Copz;
105500170808
105600170808             // ·?Più opzioni nella stessa videata e?
105700170808             //  ?cursore su nessuna delle opzioni?
105800170808             //  ?(riposizionare il cursore)?
105900170808             //  ?Se stessa opzione in videate diverse: vince la?
106000170808             //  ?posizione della prima opzione.?
106100170808             When  C1CsrRrn <> SavS01csr  and
106200170808                   C1CsrRrn <> S01nrr     and
106300170808                   S01nrr   <  wRig + c_SflPag;
106400170808               SavS01cs2 = S01nrr;
106500170808               C1RcdNbr  = SavS01csr;
106600170808               C1CsrRrn  = SavS01csr;
106700170809               $ErrGenerico = *on;
106800170809               $ErrMessage  = *on;
106900170809               $PosCurOPZ   = *on;
107000170809               VIDmsg = sk_Msg(08);
107100170808               leavesr;
107200170808
107300170808           EndSl;
107400170808
107500170809           readc  ubIPJs01;
107600170808
107700170808         ENDDO;
107800170808
107900170808         // ·?Opzione NON abilitata?
108000170809         if  SavS1Copz <> 1  and  SavS1Copz <> 2  and
108100170809             SavS1Copz <> 3  and  SavS1Copz <> 4  and
108200170809             SavS1Copz <> 5  and  SavS1Copz <> 6;
108300170808           C1RcdNbr  = SavS01csr;
108400170808           C1CsrRrn  = SavS01csr;
108500170809           $ErrGenerico = *on;
108600170809           $ErrMessage  = *on;
108700170809           $PosCurOPZ   = *on;
108800170809           VIDmsg = sk_Msg(09);
108900170808           leavesr;
109000170808         endif;
109100170808
109200170808         // -?Ripetizione opzione sul resto del subfile?
109300170808         S01nrr = SavS01csr + 1;
109400170809         chain  S01nrr  ubIPJs01;
109500170809         DoW  %found(ubChkIPJd);
109600170809           $SflNxtChg = *on;
109700170808           S1Copz = SavS1Copz;
109800170809           update  ubIPJs01;
109900170808           S01nrr += 1;
110000170809           chain  S01nrr  ubIPJs01;
110100170808         EndDo;
110200170808
110300170808       ENDSR;
110400170809
110500170809       //--------------------------------------------------------------
110600170809       //?Gestione window-subfile SW1 - Elenco Codici Errore.          ?
110700170809       //--------------------------------------------------------------
110800170809       BEGSR  sr_GesSW1;
110900170809
111000170809         // -?Inizializzazione subfile?
111100170809         if  $InzSW1;
111200170809           exsr  sr_InzSW1;
111300170809           $InzSW1 = *off;
111400170809         endif;
111500170809
111600170809         // -?Posizionamento cursore?
111700170809         //CWRcdNbr = 1;
111800170809
111900170809         // -?Emissione Window?
112000170809         write  ubIPJpW1;
112100170809         exfmt  ubIPJcW1;
112200170809
112300170809         reset  $ErrMessage;
112400170809         reset  $ErrGenerico;
112500170809         clear  VIDmsg;
112600170809
112700170809         // -?F12=Ritorno?
112800170809         if  dsp_aid = c_F12;
112900170809           $Video = $VideoPrec;
113000170809         endif;
113100170809
113200170809       ENDSR;
113300170809
113400170809       //--------------------------------------------------------------
113500170809       //?Inizializzazione window-subfile SW1.                         ?
113600170809       //--------------------------------------------------------------
113700170809       BEGSR  sr_InzSW1;
113800170809
113900170809         // -?Pulizia subfile?
114000170809         $WSflDsp_N   = *on;
114100170809         $WSflDspCt_N = *on;
114200170809         write  ubIPJsW1;
114300170809         $WSflDspCt_N = *off;
114400170809         $WSflEnd     = *off;
114500170809
114600170809         //clear  CWRcdNbr;
114700170809         //clear  CWCsrRrn;
114800170809         clear  SW1nrr;
114900170809         clear  VIDmsg;
115000170809         $ErrMessage  = *off;
115100170809         $ErrGenerico = *off;
115200170809
115300170809         // -?Caricamento completo del subfile?
115400170809         For  xx = 1  To  %elem( sk_CdM );
115500170809           If  %subst( sk_CdM_prg(xx) : 1 : 3 ) <> '|--';
115600170809             SW1cdm  = xx;
115700170809             SW1tpm  = %subst( sk_CdM(xx) : 75 : 1);
115800170809             SW1not  = %subst( sk_CdM(xx) : 1 : %len(SW1not) );
115900170809             SW1nrr += 1;
116000170809             write  ubIPJsW1;
116100170809           EndIf;
116200170809         EndFor;
116300170809
116400170809         // -?Visualizzazione del SFL (se ci sono dati)?
116500170809         $WSflDsp_N = ( SW1nrr <= *zero );
116600170809
116700170809         // -?Attivazione del SFLEND?
116800170809         $WSflEnd   = *on;
116900170809
117000170809         // -?Impaginazione subfile e?
117100170809         //  ?posizionamento cursore al 1° rcd del subfile?
117200170809         //if  SW1nrr > *zero;
117300170809         //  CWRcdNbr = 1;
117400170809         //else;
117500170809         //  clear  CWRcdNbr;
117600170809         //endif;
117700170809         //
117800170809         //CWCsrRrn = CWRcdNbr;
117900170809
118000170809       ENDSR;
118100170809
118200170809       //--------------------------------------------------------------
118300170809       //?Gestione opzione 4=Cancellazione.                            ?
118400170809       //--------------------------------------------------------------
118500170809       BEGSR  sr_Opz04;
118600170809
118700170809         // -?Richiesta conferma per cancellazione record?
118800170809         If  Not $Conferma;
118900170809           clear  ubIPJw02;
119000170809                   // *...+....1....+....2....+....3....+....4
119100170809           W02txt1 = 'Premere F6=Conferma per confermare la   ';
119200170809           W02txt2 = 'cancellazione di queste registrazioni.  ';
119300170809           W02txt3 = 'Premere F12=Ritorno altrimenti.         ';
119400170809           DoW  dsp_aid <> c_F06  and  dsp_aid <> c_F12;
119500170809             exfmt  ubIPJw02;
119600170809           EndDo;
119700170809           $Conferma = ( dsp_aid = c_F06 );
119800170809         EndIf;
119900170809
120000170809         // -?Premuto F12=Ritorno?
120100170809         if  Not $Conferma;
120200170809           leavesr;
120300170809         endif;
120400170809
120500170809         // -?Cancellazione record?
120600170809         chain  S1Hrrn  AZCAA000;
120700170809
120800170809         if  %found( AZCAA00F );
120900170809           delete  AZCAA000;
121000170809         endif;
121100170809
121200170809       ENDSR;
121300170809
121400170809       //--------------------------------------------------------------
121500170809       //?Gestione opzione 5=Visualizzazione Utente.                   ?
121600170809       //--------------------------------------------------------------
121700170809       BEGSR  sr_Opz05;
121800170809
121900170809         clear  TIBS31ds;
122000170809         TIBS31ds.I31ute = S1Cute;
122100170809         TIBS31ds.I31Opz = %editc( S1Copz : 'X' );
122200170809
122300170809         TIBS31R ( KPJBA : TIBS31ds );
122400170809
122500170809         Select;
122600170809           When  TIBS31ds.O31err <> *blank;
122700170809             $ErrGenerico = *on;
122800170809             $ErrMessage  = *on;
122900170809             VIDmsg = TIBS31ds.O31msg;
123000170809             leavesr;
123100170809           When  TIBS31ds.O31tfu =  'F03';
123200170809             $ErrGenerico = *on;
123300170809             leavesr;
123400170809           When  TIBS31ds.O31tfu =  'F12';
123500170809             leavesr;
123600170809         EndSl;
123700170809
123800170809       ENDSR;
123900170809
124000170809       //--------------------------------------------------------------
124100170809       //?Controllo Filiale dell'utente.                               ?
124200170809       //--------------------------------------------------------------
124300170809       BEGSR  sr_Chk_dutPOU;
124400170809
124500170809         // -?Reperimento filiale dall'IP Terminale?
124600170809         // ·?2° "." ("10.XXX.") dovrebbe essere nella 7ª posizione?
124700170809         xx = %scan( '.' : AZCAAds.CAAip : 4 );
124800170809         if  xx = *zero  or  xx = 4  or  xx > 7;
124900170809           S1Hin40 = *on;
125000170809           leavesr;
125100170809         endif;
125200170809         Monitor;
125300170809           wIPfil = %int( %subst( AZCAAds.CAAip : 4 : ( xx - 4 ) ) );
125400170809           On-Error;
125500170809             S1Hin40 = *on;
125600170809             leavesr;
125700170809         EndMon;
125800170809
125900170809         // -?Controllo corrispondenza diretta IP Terminale / Fil. Utente?
126000170809         chain  ( wIPfil )  AZORG;
126100170809
126200170809         if  NOT  %found(AZORG01L)  and
126300170809             %subst( AZCAAds.CAAdev : 1 : 1 ) = 'P';
126400170809           // -?Reperimento filiale dal device?
126500170809           Monitor;
126600170809             wIPfil = %int( %subst( AZCAAds.CAAdev : 2 : 3 ) );
126700170809             On-Error;
126800170809               S1Hin40 = *on;
126900170809               leavesr;
127000170809           EndMon;
127100170809           chain  ( wIPfil )  AZORG;
127200170809         endif;
127300170809
127400170809         if  NOT  %found(AZORG01L);
127500170809           S1Hin40 = *on;
127600170809           leavesr;
127700170809         endif;
127800170809
127900170809         // -?Reperimento dati dell'Utente?
128000170809         chain  ( AZCAAds.CAAute )  AZUTE000;
128100170809         if  NOT  %found(AZUTE01L);
128200170809           S1Hin40 = *on;
128300170809           leavesr;
128400170809         endif;
128500170809
128600170809         // -?IP terminale ed IP filiale corrispondenti: OK?
128700170809         //  ?(se §OGIIP = *blank => si utilizza il codice filiale)?
128800170809         Og143 = orgDE3;
128900170809         if  wIPfil = UTEfil or  %editc( wIPfil : 'X' ) = Og143.§ogIIP;
129000170809           leavesr;
129100170809         endif;
129200170809
129300170809         // -?Reperimento Filiali a cui l'utente è "abilitato"?
129400170809         clear  sk_POg;
129500170809         clear  TRUL06ds;
129600170809         D06cod = 'PP';
129700170809         D06key = %editc( UTEfil : 'X' );
129800170809         KPJBU    = TRUL06ds;
129900170809         trul06r ( kpjba );
130000170809         TRUL06ds = TRUL06ds;
130100170809
130200170809         // -?L'indirizzo IP del device è di una delle filiali a cui è?
130300170809         //  ?legato l'utente => uscita: OK?
130400170809         if  %lookup( %editc( wIPfil : 'X' ) : sk_POg ) > *zero;
130500170809           exsr  sr_RoutEnd;
130600170809         endif;
130700170809
130800170809       ENDSR;
130900170808
131000170808       //--------------------------------------------------------------
131100170808       //?Preparazione stringa SQL S1.                                 ?
131200170808       //--------------------------------------------------------------
131300170808       BEGSR  sr_PrepSQL;
131400170808
131500170808         $EoF       = *off;
131600170808         $First     = *on;
131700170808         $First_cdm = *on;
131800170808
131900170808         wSQL = 'select rrn(AZCAA00F), AZCAA00F.* from AZCAA00F';
132000170808
132100170808         // -?Selezione per Utente?
132200170808         If  V1Cute <> *blank;
132300170808           if  $First;
132400170808             wSQL += ' where';
132500170808           else;
132600170808             wSQL += ' and';
132700170808           endif;
132800170808           wSQL += ' CAAute like ''' + %trimR( V1Cute ) + '%''';
132900170808           $First = *off;
133000170808         EndIf;
133100170808
133200170808         // -?Selezione per Device?
133300170808         If  V1Cdev <> *blank;
133400170808           if  $First;
133500170808             wSQL += ' where';
133600170808           else;
133700170808             wSQL += ' and';
133800170808           endif;
133900170808           wSQL += ' CAAdev like ''' + %trimR( V1Cdev ) + '%''';
134000170808           $First = *off;
134100170808         EndIf;
134200170808
134300170808         // -?Selezione per Data/Ora collegamento Iniziali?
134400170808         If  V1CdatI <> *zero  or  V1CoraI <> *zero;
134500170808           if  $First;
134600170808             wSQL += ' where';
134700170808           else;
134800170808             wSQL += ' and';
134900170808           endif;
135000170808           wSQL += ' CAAtim >= ' +
135100170808                   %trim( %char( ( W1CdatI * 1000000 ) + V1CoraI ) );
135200170808           $First = *off;
135300170808         EndIf;
135400170808
135500170808         // -?Selezione per Data/Ora collegamento Finali?
135600170808         If  V1CdatF <> *zero  or  V1CoraF <> *zero;
135700170808           if  $First;
135800170808             wSQL += ' where';
135900170808           else;
136000170808             wSQL += ' and';
136100170808           endif;
136200170808           wSQL += ' CAAtim <= ' +
136300170808                   %trim( %char( ( W1CdatF * 1000000 ) + V1CoraF ) );
136400170808           $First = *off;
136500170808         EndIf;
136600170808
136700170808         // -?Selezione per Codice Messaggio?
136800170808         if  V1Ccdm_ds <> *zero;
136900170808           if  $First;
137000170808             wSQL += ' where';
137100170808           else;
137200170808             wSQL += ' and';
137300170808           endif;
137400170808           wSQL += ' CAAcdm in (';
137500170808           $First_cdm = *on;
137600170808           For  xx = 1  To  %elem(sk_V1Ccdm);
137700170808             if  sk_V1Ccdm(xx) <> *zero;
137800170808               if  NOT $First_cdm;
137900170808                 wSQL += ', ';
138000170808               endif;
138100170808               wSQL += %trim( %char( sk_V1Ccdm(xx) ) );
138200170808               $First_cdm = *off;
138300170808             endif;
138400170808           EndFor;
138500170808           wSQL += ')';
138600170808           $First = *off;
138700170808         endif;
138800170808
138900170808         // -?Ordinamento?
139000170808         select;
139100170808           // -?Ordinamento per Utente?
139200170808           when  wOrdSfl = *zero;
139300170808             wSQL += ' order by CAAute, CAAtim';
139400170808           // -?Ordinamento per Device?
139500170808           when  wOrdSfl = 1;
139600170808             wSQL += ' order by CAAdev, CAAtim';
139700170808           // -?Ordinamento per IP Address?
139800170808           when  wOrdSfl = 2;
139900170808             wSQL += ' order by CAAip, CAAtim';
140000170808           // -?Ordinamento per Data/Ora?
140100170808           when  wOrdSfl = 3;
140200170808             wSQL += ' order by CAAtim, CAAute';
140300170808           // -?Ordinamento per Codice Messaggio?
140400170808           when  wOrdSfl = 4;
140500170808             wSQL += ' order by CAAcdm, CAAtim';
140600170808           // -?Ordinamento per Tipo Messaggio?
140700170808           when  wOrdSfl = 5;
140800170808             wSQL += ' order by CAAtpm, CAAcdm, CAAtim';
140900170808         endsl;
141000170808
141100170808         wSQL += ' for fetch only';
141200170808
141300170808       ENDSR;
141400170808
141500170808       //--------------------------------------------------------------
141600170808       //?Apertura cursore C1.                                         ?
141700170808       //--------------------------------------------------------------
141800170808       BEGSR  sr_OpenCursor;
141900170808
142000170808         // -?Dichiarazione cursore?
142100170808         exec sql   prepare S1   from :wSQL;
142200170808         exec sql   declare C1   cursor for S1;
142300170808
142400170808         // -?Apertura del cursore?
142500170808         exec sql   open C1;
142600170808
142700170808         if  SQLcode < *zero;
142800170808           $EoF = *on;
142900170808           exsr  sr_PrintErrSQL;
143000170808         endif;
143100170808
143200170808       ENDSR;
143300170808
143400170808       //--------------------------------------------------------------
143500170808       //?Chiusura cursore C1.                                         ?
143600170808       //--------------------------------------------------------------
143700170808       BEGSR  sr_CloseCursor;
143800170808
143900170808         // -?Chiusura del cursore?
144000170808         exec sql   close C1;
144100170808
144200170808       ENDSR;
144300170808
144400170808       //--------------------------------------------------------------
144500170808       //?Lettura cursore C1.                                          ?
144600170808       //--------------------------------------------------------------
144700170808       BEGSR  sr_ReadCursor;
144800170808
144900170808         clear  AZCAAds;
145000170808
145100170808         exec sql   fetch next   from C1   into :wCAArrn, :AZCAAds;
145200170808
145300170808         select;
145400170808           when  SQLcode = 100;
145500170808             $EoF = *on;
145600170808           when  SQLcode < *zero;
145700170808             $EoF = *on;
145800170808             exsr  sr_PrintErrSQL;
145900170808         endsl;
146000170808
146100170808       ENDSR;
146200170808
146300170808       //--------------------------------------------------------------
146400170808       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
146500170808       //--------------------------------------------------------------
146600170808       BEGSR  sr_PrintErrSQL;
146700170808
146800170808         // -?Stampa del Dump?
146900170808         Dump(A);
147000170808
147100170808         // -?Stampa del Job-Log?
147200170808         Qcmd = 'DSPJOBLOG job(*) output(*print)';
147300170808         exsr  sr_ExecCmd;
147400170808
147500170808         // -?Chiusura del programma?
147600170808         exsr  sr_RoutEnd;
147700170808
147800170808       ENDSR;
147900170808
148000170808       //--------------------------------------------------------------
148100170808       //?Esecuzione del comando (già impostato).                      ?
148200170808       //--------------------------------------------------------------
148300170808       BEGSR  sr_ExecCmd;
148400170808
148500170808         clear Qcap0100;
148600170808         Qcabcsdh = *off;
148700170808         Qcapa    = *off;
148800170808         Qcacmdss = *off;
148900170808         Qcaerved = *allX'00';
149000170808
149100170808         clear Qusec;
149200170808         Qusbprv  = %size(Qusec);
149300170808
149400170808         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
149500170808                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
149600170808                           0 : 0 : Qusec);
149700170808
149800170808         // -?Invio *msg per segnalazione errore?
149900170808         //if  Qusei <> *blank;
150000170808         //  ...;
150100170808         //endif;
150200170808
150300170808       ENDSR;
150400170808
150500170808       //--------------------------------------------------------------
150600170808       //?Operazioni finali.                                         ?
150700170808       //--------------------------------------------------------------
150800170808       BEGSR  sr_RoutEnd;
150900170808
151000170808         // -?Chiusura *pgm?
151100170808         return;
151200170808
151300170808       ENDSR;
151400170808
151500170808      /end-free
151600170808
151700170808       //--------------------------------------------------------------
151800170808       //?Schiere a tempo di compilazione.                             ?
151900170808       //--------------------------------------------------------------
152000170808
152100170809....+.*..1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
152200170809** --?sk_CdM:?Codici Messaggio in AZCAA (da *pgm "ubChkIPJ") --------* Tipo Progr?
152300170808Dati dell'utente non reperibili                                       |   0|-- 1 NON usato
152400170808Errore nell'esecuzione dell'API?QDCRDEVD (Retrieve Device Description)|   1|   2
152500170808Errore nel reperimento dell'indirizzo IP del dispositivo              |   1|   3
152600170808Dispositivo NON di tipo IP (terminale)                                |   1|-- 4 NON usato
152700170808Indirizzo IP errato: non inizia con "10."                             |   1|   5
152800170808Indirizzo IP errato: non trovato carattere "." nella 7ª posizione     |   1|   6
152900170808Codice Filiale non numerico                                           |   0|-- 7 NON usato
153000170808Filiale utente non trovata in anagrafica                              |   0|-- 8 NON usato
153100170808Utente non abilitato alla Filiale del dispositivo                     |   1|   9
153200170808Utente non abilitato alla Filiale reperita dall'IP del dispositivo    |   1|  10
153300170809** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
153400170808Data formalmente errata                                                        1
153500170808Ora formalmente errata                                                         2
153600170808Codice Messaggio errato: previsti valori da 01 a 10                            3
153700170808Inserito un valore errato                                                      4
153800170809Raggiunto il limite massimo dei record caricabili a video (9999)               5
153900170809Immettere un'opzione su questa pagina prima di premere F13                     6
154000170809Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa 7
154100170809Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F13 8
154200170809Opzione errata                                                                 9
