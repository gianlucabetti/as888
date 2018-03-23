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
019400170810
019500170810       // -?Flags Abilitazioni utenti aziendali?
019600170810     d dUte01        e ds                  inz   qualified
019700170808
019800170808       // -?Dati estratti via SQL?
019900170808     d AZCAAds       e ds                  inz  extname(AZCAA00F)
020000170808     d                                          qualified
020100170808     d wCAArrn         s             11  0 inz
020200170808
020300170808       //--------------------------------------------------------------
020400170808       //?Definizione variabili globali.                               ?
020500170808       //--------------------------------------------------------------
020600170808
020700170808       // -?Flags booleani?
020800170808     d $Fine           s               n   inz
020900170808     d $EoF            s               n   inz
021000170808     d $First          s               n   inz
021100170808     d $First_cdm      s               n   inz
021200170808     d $InzD01         s               n   inz(*on)
021300170808     d $InzS01         s               n   inz(*on)
021400170808     d $InzSW1         s               n   inz(*on)
021500170809     d $ForzaOpz       s               n   inz
021600170809     d $Conferma       s               n   inz
021700170808
021800170808       // -?Variabili per la gestione del video?
021900170808     d $Video          s              2    inz('S1')
022000170808     d $VideoPrec      s              2    inz
022100170808     d W_SflPag        s              3  0 inz
022200170808     d S01nrr          s                   inz   like(C1RcdNbr)
022300170808     d SW1nrr          s                   inz   like(C1RcdNbr)
022400170808     d SavS1Copz       s                   inz   like(S1Copz)
022500170809     d SavS1Cop1       s                   inz   like(S1Copz)
022600170808     d SavS01csr       s                   inz   like(C1RcdNbr)
022700170809     d SavS01cs1       s                   inz   like(C1RcdNbr)
022800170809     d SavS01cs2       s                   inz   like(C1RcdNbr)
022900170808     d wPag            s              4  0 inz
023000170808     d wRig            s              3  0 inz
023100170808     d wOrdSfl         s              1  0 inz
023200170809
023300170809       // -?Indici di Schiera?
023400170809     d xx              s              3  0 inz
023500170809
023600170809       // -?Filiale reperita dall'IP address?
023700170809     d wIPfil          s                   inz   like(ORGfil)
023800170808
023900170808       // -?Stringa SQL da eseguire?
024000170808     d wSql            s          32740    inz   varying
024100170808
024200170808       // -?Campi di comodo?
024300170808     d W1CdatI         s                   inz   like(V1CdatI)
024400170808     d W1CdatF         s                   inz   like(V1CdatF)
024500170808
024600170808       //--------------------------------------------------------------
024700170808       //?Definizione prototipi procedure usate.                       ?
024800170808       //--------------------------------------------------------------
024900170808
025000170808       // -?Reperimento dati utente?
025100170808     d TIBS34ds      e ds                  inz
025200170808      /copy gaitrasrc/srcProtoPR,TIBS34R
025300170808
025400170808       // -?Caricamento filiali gestite dall'utente?
025500170809     d TRUL06ds      e ds                  inz
025600170809     d   sk_POg                1     90    inz   dim(30)
025700170809      /copy gaitrasrc/srcProtoPR,TRUL06R
025800170809
025900170809       // -?Visualizzazione profilo utente - dettaglio?
026000170809     d TIBS31ds      e ds                  inz   qualified
026100170809     d TIBS31R         pr                  extpgm('TIBS31R')
026200170809     d   kpjba                             likeds(KPJBA)
026300170809     d   TIBS31ds                          likeds(TIBS31ds)
026400170808
026500170808       // -?Controllo formale / Inversione data?
026600170808     d WLBdat          ds                  inz
026700170809     d   G08dat                1      8  0 inz
026800170809     d   G08inv                9     16  0 inz
026900170809     d   G08err               17     17    inz('3')
027000170809     d   G08tgi               18     22  0 inz
027100170808      /copy gaitrasrc/srcProtoPR,XSRDA8
027200170808
027300170808       // -?API QCAPCMD (Process Commands)?
027400170808     d Qcmd            s           2048    inz  varying
027500170808      /copy qSysInc/qRpgleSrc,QCAPCMD
027600170808      /copy gaitrasrc/srcProtoPR,QCAPCMD
027700170808
027800170808       // -?Parametri gestione errori API.?
027900170808      /copy qsysinc/qRpgleSrc,QUSEC
028000170808
028100170808       //--------------------------------------------------------------
028200170808       //?Definizione key-list.                                        ?
028300170808       //--------------------------------------------------------------
028400170808
028500170808
028600170808       //--------------------------------------------------------------
028700170808       //?Riepilogo indicatori utilizzati.                             ?
028800170808       //--------------------------------------------------------------
028900170808       //--------------------------------------------------------------
029000170808
029100170808       //--------------------------------------------------------------
029200170808       //?M A I N - L I N E .                                          ?
029300170808       //--------------------------------------------------------------
029400170808
029500170808     c     *Entry        plist
029600170808     c                   parm                    KPJBA
029700170808
029800170808      /free
029900170808
030000170808       // -?Operazioni iniziali?
030100170808       exsr sr_RoutInz;
030200170808
030300170808       // -?Ciclo di gestione del file video?
030400170808       DoW  $Fine = *off;
030500170808
030600170808         select;
030700170808
030800170808           // -?Gestione videata D1 (Filtro di lancio)?
030900170808           when  $Video = 'D1';
031000170808             exsr  sr_GesD01;
031100170808
031200170808           // -?Gestione videata S1 (SubFile)?
031300170808           when  $Video = 'S1';
031400170808             exsr  sr_GesS01;
031500170808
031600170808           // -?Gestione window W1?
031700170808           when  $Video = 'W1';
031800170808             exsr  sr_GesSW1;
031900170808
032000170808           // -?? ? ? ? ??
032100170808           other;
032200170808             $Fine = *on;
032300170808
032400170808         endsl;
032500170808
032600170808       EndDo;
032700170808
032800170808       // -?Operazioni finali?
032900170808       exsr sr_RoutEnd;
033000170808
033100170808       //--------------------------------------------------------------
033200170808       //?Operazioni iniziali.                                         ?
033300170808       //--------------------------------------------------------------
033400170808       BEGSR  sr_RoutInz;
033500170808
033600170808         // -?Impostazione opzioni per SQL?
033700170808         exec sql   set  option  DynUsrPrf = *Owner,
033800170808                                 CloSqlCsr = *EndMod;
033900170808
034000170808         // -?Impostazione chiusura?
034100170808         *inLR = *on;
034200170808         reset  $Fine;
034300170808
034400170808         // -?Reperimento dati job?
034500170808         exsr  sr_DatiJob;
034600170808
034700170808         // -?Impostazione nome programma a video?
034800170808         V1Tpgm = SDSpgm;
034900170808
035000170808         // -?Impostazione videata iniziale?
035100170808         clear  UBIPJD01;
035200170808         clear  UBIPJC01;
035300170808         $Video = 'D1';
035400170808         reset  $InzD01;
035500170808         reset  $InzS01;
035600170808
035700170808       ENDSR;
035800170808
035900170808       //--------------------------------------------------------------
036000170808       //?Reperimento Dati del job (Utente/Operativi).                 ?
036100170808       //--------------------------------------------------------------
036200170808       BEGSR  sr_DatiJob;
036300170808
036400170808         in(e) §AzUte;
036500170808         if NOT %error;
036600170808           in(e) §DatiUte;
036700170808         endif;
036800170808         if %error or RSut = *blank;
036900170808           tibs34r ( tibs34ds );
037000170808           in §AzUte;
037100170808           in §DatiUte;
037200170808         endif;
037300170808
037400170808       ENDSR;
037500170808
037600170808       //--------------------------------------------------------------
037700170808       //?Gestione videata D01.                                        ?
037800170808       //--------------------------------------------------------------
037900170808       BEGSR  sr_GesD01;
038000170808
038100170808         // -?Inizializzazione videata?
038200170808         if  $InzD01 = *on;
038300170808           exsr  sr_InzD01;
038400170808           $InzD01 = *off;
038500170808         endif;
038600170808
038700170808         // -?Emissione Testata?
038800170808         write  ubIPJt01;
038900170808
039000170808         // -?Emissione videata (con Piede = tasti funzionali abilitati)?
039100170808         exfmt  ubIPJd01;
039200170808
039300170808         clear  VIDmsg;
039400170808         reset  $ErrMessage;
039500170808         reset  $ErrGenerico;
039600170808
039700170808         Select;
039800170808
039900170808           // -?F1=Codici Errore?
040000170808           When  dsp_aid = c_F01;
040100170808             $VideoPrec = $Video;
040200170808             $Video     = 'W1';
040300170808             $InzSW1    = *on;
040400170808
040500170808           // -?F3=Fine?
040600170808           When  dsp_aid = c_F03;
040700170808             $Fine  = *on;
040800170808
040900170808           // -?Invio?
041000170808           Other;
041100170808             exsr  sr_CtrD01;
041200170808             if  $ErrGenerico;
041300170808               leavesr;
041400170808             endif;
041500170808             $Video  = 'S1';
041600170808             $InzS01 = *on;
041700170808
041800170808         EndSl;
041900170808
042000170808       ENDSR;
042100170808
042200170808       //--------------------------------------------------------------
042300170808       //?Inizializzazione videata D01.                                ?
042400170808       //--------------------------------------------------------------
042500170808       BEGSR  sr_InzD01;
042600170808
042700170808         // -?Spegnimento indicatori di errore e posizionamento cursore?
042800170808         %subst( IndDspF : 28 ) = *off;
042900170808
043000170808         // -?Pulizia videata?
043100170808         clear  ubIPJd01;
043200170808
043300170808         // -?Impostazione iniziale dei campi a video?
043400170810         //*·//*·V1Ccdm1  = 1;
043500170810         //*·V1Ccdm2  = 2;
043600170810         //*·V1Ccdm3  = 3;
043700170810         //*·//*·V1Ccdm4  = 4;
043800170810         //*·V1Ccdm5  = 5;
043900170810         //*·V1Ccdm6  = 6;
044000170810         //*·//*·V1Ccdm7  = 7;
044100170810         //*·//*·V1Ccdm8  = 8;
044200170810         //*·V1Ccdm9  = 9;
044300170810         //*·V1CcdmA  = 10;
044400170808
044500170808       ENDSR;
044600170808
044700170808       //--------------------------------------------------------------
044800170808       //?Inizializzazione videata D01.                                ?
044900170808       //--------------------------------------------------------------
045000170808       BEGSR  sr_CtrD01;
045100170808
045200170808         // -?Spegnimento indicatori di errore e posizionamento cursore?
045300170808         %subst( IndDspF : 50 ) = *off;
045400170808
045500170808         // -?Data Collegamento iniziale?
045600170808         clear  WLBdat;
045700170808         if  V1CdatI <> *zero;
045800170808           G08dat = V1CdatI;
045900170808           xsrda8 ( WLBdat );
046000170808           if  G08err =  *on;
046100170809             $PosCurDATi  = *on;
046200170809             $ErrMessage  = *on;
046300170809             $ErrGenerico = *on;
046400170809             VIDmsg = sk_Msg(01);
046500170808             leavesr;
046600170808           endif;
046700170808           V1CdatI = G08dat;
046800170808         endif;
046900170808         W1CdatI = G08inv;
047000170808
047100170808         // -?Ora  Collegamento iniziale?
047200170808         clear  WSMora_ds;
047300170808         if  V1CoraI <> *zero;
047400170808           WSMora_ds.WSMora = V1CoraI;
047500170808           if  WSMora_ds.WSMhh < *zero  or  WSMora_ds.WSMhh > 23  or
047600170808               WSMora_ds.WSMmm < *zero  or  WSMora_ds.WSMmm > 59  or
047700170808               WSMora_ds.WSMss < *zero  or  WSMora_ds.WSMss > 59;
047800170809             $PosCurORAi  = *on;
047900170809             $ErrMessage  = *on;
048000170809             $ErrGenerico = *on;
048100170809             VIDmsg = sk_Msg(02);
048200170808             leavesr;
048300170808           endif;
048400170808         endif;
048500170808
048600170808         // -?Data Collegamento finale?
048700170808         clear  WLBdat;
048800170808         if  V1CdatF <> *zero;
048900170808           G08dat = V1CdatF;
049000170808           xsrda8 ( WLBdat );
049100170808           if  G08err =  *on;
049200170809             $PosCurDATf  = *on;
049300170809             $ErrMessage  = *on;
049400170809             $ErrGenerico = *on;
049500170809             VIDmsg = sk_Msg(01);
049600170808             leavesr;
049700170808           endif;
049800170808           V1CdatF = G08dat;
049900170808         endif;
050000170808         W1CdatF = G08inv;
050100170808
050200170808         // -?Ora  Collegamento finale?
050300170808         clear  WSMora_ds;
050400170808         if  V1CoraF <> *zero;
050500170809           WSMora_ds.WSMora = V1CoraF;
050600170808           if  WSMora_ds.WSMhh < *zero  or  WSMora_ds.WSMhh > 23  or
050700170808               WSMora_ds.WSMmm < *zero  or  WSMora_ds.WSMmm > 59  or
050800170808               WSMora_ds.WSMss < *zero  or  WSMora_ds.WSMss > 59;
050900170809             $PosCurORAf  = *on;
051000170809             $ErrMessage  = *on;
051100170809             $ErrGenerico = *on;
051200170809             VIDmsg = sk_Msg(02);
051300170808             leavesr;
051400170808           endif;
051500170808         endif;
051600170808
051700170810         //*·// -?Codici Errore?
051800170810         //*·if  V1Ccdm_ds <> *zero;
051900170810         //*·  For  xx = 1  To  %elem( sk_V1Ccdm );
052000170810         //*·    //*·sk_V1Ccdm(xx) = %abs( sk_V1Ccdm(xx) );
052100170810         //*·    select;
052200170810         //*·      when  sk_V1Ccdm(xx) = *zero;
052300170810         //*·      when  sk_V1Ccdm(xx) > *zero  and
052400170810         //*·            sk_V1Ccdm(xx) < %elem( sk_V1Ccdm );
052500170810         //*·        if  %subst( sk_CdM_prg( sk_V1Ccdm(xx) ) : 1 : 3 ) = '|--';
052600170810         //*·          %subst( IndDspF : 56+xx ) = *on;
052700170810         //*·          $ErrMessage  = *on;
052800170810         //*·          $ErrGenerico = *on;
052900170810         //*·          VIDmsg = sk_Msg(03);
053000170810         //*·          leavesr;
053100170810         //*·        endif;
053200170810         //*·      other;
053300170810         //*·        VIDmsg = sk_Msg(03);
053400170810         //*·    endsl;
053500170810         //*·  EndFor;
053600170810         //*·endif;
053700170808
053800170809         // -?Flag "Solo Utenti di Filiale con IP Address incompatibile"?
053900170809         if  V1Csolo <> *blank  and  V1Csolo <> 'S';
054000170809           $PosCurSOLO  = *on;
054100170809           $ErrMessage  = *on;
054200170809           $ErrGenerico = *on;
054300170809           VIDmsg = sk_Msg(04);
054400170808           leavesr;
054500170808         endif;
054600170808
054700170808       ENDSR;
054800170808
054900170808       //--------------------------------------------------------------
055000170808       //?Gestione subfile S01.                                        ?
055100170808       //--------------------------------------------------------------
055200170808       BEGSR  sr_GesS01;
055300170808
055400170808         // -?Inizializzazione videata?
055500170808         if  $InzS01 = *on;
055600170808           exsr  sr_InzS01;
055700170808           $InzS01 = *off;
055800170808         endif;
055900170808
056000170808         // -?(Dis)Abilitazioni CONDIZIONATE tasti funzionali in P01 (per C01)?
056100170808         $F8Attivo  = ( S01nrr > 1 );
056200170808         $F9Attivo  = ( S01nrr > *zero );
056300170808         $F13Attivo = $F8Attivo;
056400170808
056500170808         // -?Emissione Testata & Piede con tasti funzionali abilitati?
056600170808         write  ubIPJt01;
056700170808         write  ubIPJp01;
056800170808
056900170808         // -?Posizionamento cursore?
057000170808         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
057100170808         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
057200170808         //  ?si visualizza la pagina che vedeva l'utente quando ha?
057300170808         //  ?premuto l'ultimo tasto.?
057400170808         if  C1CsrRrn > *zero;
057500170808           C1RcdNbr = C1CsrRrn;
057600170808         else;
057700170808           C1RcdNbr = 1;
057800170808           write  ubIPJs00;         //(no rec.)?
057900170808         endif;
058000170808
058100170808         // -?Emissione videata?
058200170808         exfmt  ubIPJc01;
058300170808
058400170809         clear  VIDmsg;
058500170808         reset  $ErrMessage;
058600170808         reset  $ErrGenerico;
058700170809
058800170809         reset  $Conferma;
058900170809
059000170809         if  dsp_aid <> c_F03  and
059100170809             dsp_aid <> c_F13;
059200170809           clear  SavS01csr;
059300170809           clear  SavS01cs1;
059400170809           clear  SavS01cs2;
059500170809           clear  SavS1Copz;
059600170809           clear  SavS1Cop1;
059700170809         endif;
059800170808
059900170808         SELECT;
060000170808
060100170808           // -?F1=Window Codici Errore?
060200170808           WHEN  dsp_aid = c_F01;
060300170808             $VideoPrec = $Video;
060400170808             $Video     = 'W1';
060500170808             $InzSW1    = *on;
060600170808
060700170808           // -?F3=Fine?
060800170808           WHEN  dsp_aid = c_F03;
060900170808             exsr  sr_CloseCursor;
061000170808             $Fine = *on;
061100170808
061200170808           // -?F5=Refresh?
061300170809           WHEN  dsp_aid = c_F05;
061400170808             exsr  sr_CloseCursor;
061500170808             $InzS01 = *on;
061600170808
061700170808           // -?F8=Ordinamento?
061800170808           WHEN  dsp_aid = c_F08;
061900170808             exsr  sr_F08S01;
062000170808
062100170808           // -?F12=Ritorno?
062200170808           WHEN  dsp_aid = c_F12;
062300170808             exsr  sr_CloseCursor;
062400170808             $Video = 'D1';
062500170808
062600170808           // -?F13=Ripetizione Opzione?
062700170808           WHEN  dsp_aid = c_F13;
062800170808             exsr  sr_F13S01;
062900170808
063000170808           // -?Roll-Up?
063100170808           WHEN  dsp_aid = c_RollUp;
063200170808             if  S01nrr >= c_MaxRec;
063300170809               $ErrGenerico = *on;
063400170809               $ErrMessage  = *on;
063500170809               VIDmsg = sk_Msg(05);
063600170808             else;
063700170808               exsr sr_RollUpS01;
063800170808             endif;
063900170808
064000170808           // -?SubFile vuoto?
064100170808           WHEN  S01nrr = *zero;
064200170808
064300170808           // -?Invio?
064400170808           OTHER;
064500170808             // -?Gestione opzioni?
064600170808             if  S01nrr > *zero;
064700170808               exsr  sr_OpzS01;
064800170809               if  $ErrGenerico;
064900170808                 leavesr;
065000170808               endif;
065100170808             endif;
065200170808
065300170808         ENDSL;
065400170808
065500170808       ENDSR;
065600170808
065700170808       //--------------------------------------------------------------
065800170808       //?Inizializzazione subfile S01.                                ?
065900170808       //--------------------------------------------------------------
066000170808       BEGSR  sr_InzS01;
066100170808
066200170808         // -?Spegnimento degli indicatori di errore?
066300170808         %subst( IndDspF : 50 ) = *off;
066400170808
066500170808         // -?Pulizia subfile?
066600170808         $SflDsp_N    = *on;
066700170808         $SflDspCtl_N = *on;
066800170808         write  ubIPJc01;
066900170808         $SflDspCtl_N = *off;
067000170808         $SflEnd      = *off;
067100170808
067200170809         clear  W_SflPag;
067300170808         clear  C1RcdNbr;
067400170808         clear  C1CsrRrn;
067500170808         clear  S01nrr;
067600170809         clear  VIDmsg;
067700170808         $ErrMessage  = *off;
067800170808         $ErrGenerico = *off;
067900170808
068000170808         // -?Preparazione stringa SQL?
068100170808         exsr  sr_PrepSQL;
068200170808
068300170808         // -?Caricamento della 1ª pagina di dati del subfile?
068400170808         exsr  sr_OpenCursor;
068500170808         exsr  sr_ReadCursor;
068600170808         exsr  sr_RollUpS01;
068700170808
068800170808         // -?Impaginazione subfile?
068900170808         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
069000170808         if S01nrr > *zero;
069100170808           C1RcdNbr  = 1;
069200170808           C1CsrRrn  = 1;
069300170808         else;
069400170808           clear C1RcdNbr;
069500170808         endif;
069600170808
069700170808         // -?(Dis)Abilitazione tasti funzionali?
069800170808         // ·?F8 = Ordinamento x ...?
069900170808         $F8Attivo  = ( S01nrr > 1 );
070000170808         // ·?F9 = Altri Dati?
070100170808         $F9Attivo  = ( S01nrr > *zero );
070200170808         // ·?F13 = Ripetizione opz.?
070300170808         $F13Attivo = $F8Attivo;
070400170808
070500170808         // -?Impostazione F8 nel piede del subfile?
070600170808         select;
070700170810           when  wOrdSfl = *zero;
070800170808             V1PF08 = 'F8=Ord. x Device    ';
070900170810           when  wOrdSfl = 1;
071000170808             V1PF08 = 'F8=Ord. x IP Address';
071100170810           when  wOrdSfl = 2;
071200170808             V1PF08 = 'F8=Ord. x Data/Ora  ';
071300170810           when  wOrdSfl = 3;
071400170808             V1PF08 = 'F8=Ord. x Cod.Msg   ';
071500170810           when  wOrdSfl = 4;
071600170808             V1PF08 = 'F8=Ord. x Tipo Msg  ';
071700170810           when  wOrdSfl = 5;
071800170810             V1PF08 = 'F8=Ord. x Utente    ';
071900170808         endsl;
072000170808
072100170808       ENDSR;
072200170808
072300170808       //--------------------------------------------------------------
072400170808       //?Caricamento singola pagina nel subfile S01.                  ?
072500170808       //--------------------------------------------------------------
072600170808       BEGSR  sr_RollUpS01;
072700170808
072800170809         S01nrr     = (W_SflPag * c_SflPag);
072900170809         W_SflPag  += 1;
073000170809         $SflNxtChg = *off;
073100170808
073200170808         // -?Ciclo di caricamento dati nel subfile?
073300170808         DoW  S01nrr  <  (W_SflPag * c_SflPag)  and
073400170808              S01nrr  <  c_MaxRec               and
073500170809              SQLcode >= *zero                  and
073600170808              SQLcode <> 100;
073700170808
073800170808           clear  ubIPJs01;
073900170808           exsr  sr_RieS01;
074000170808
074100170808           // -?Scrittura record nel subfile S01?
074200170809           if  V1Csolo = *blank  or  $Incompat;
074300170809             S01nrr += 1;
074400170809             write  ubIPJs01;
074500170809           endif;
074600170808
074700170808           // -?Lettura record successivo?
074800170808           exsr  sr_ReadCursor;
074900170808
075000170808         EndDo;
075100170808
075200170808         // -?Visualizzazione del SFL (se ci sono dati)?
075300170809         $SflDsp_N = ( S01nrr <= *zero );
075400170808
075500170808         // -?Attivazione del SFLEND?
075600170809         $SflEnd = $EoF;
075700170808         if  S01nrr >= c_MaxRec;
075800170809           $ErrMessage  = *on;
075900170809           $ErrGenerico = *on;
076000170809           VIDmsg = sk_Msg(05);
076100170808         endif;
076200170808
076300170808         // -?Posizionamento cursore al 1° rcd della pagina?
076400170808         if  S01nrr > *zero;
076500170808           wPag     = S01nrr / c_SflPag;
076600170808           wRig     = S01nrr - (c_SflPag * wPag);
076700170808           C1RcdNbr = wPag * c_SflPag;
076800170808           if  wRig > *zero;
076900170808             C1RcdNbr += 1;
077000170808           else;
077100170808             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
077200170808           endif;
077300170808         else;
077400170808           clear  C1RcdNbr;
077500170808         endif;
077600170808
077700170808         C1CsrRrn = C1RcdNbr;
077800170808
077900170808       ENDSR;
078000170808
078100170808       //--------------------------------------------------------------
078200170808       //?Valorizzazione campi del singolo record nel subfile S01.     ?
078300170808       //--------------------------------------------------------------
078400170808       BEGSR  sr_RieS01;
078500170808
078600170808         // -?Campi Hidden?
078700170808         S1Hrrn     = wCAArrn;
078800170809         S1Htim     = AZCAAds.CAAtim;
078900170808
079000170808         // -?Campi di Output?
079100170808         S1Cute   = AZCAAds.CAAute;
079200170808         S1Cdev   = AZCAAds.CAAdev;
079300170808         S1Cip    = AZCAAds.CAAip;
079400170808
079500170808         reset  WLBdat;
079600170808         G08inv = AZCAAds.CAAtim / 1000000;
079700170808         xsrda8 ( WLBdat );
079800170808         if  G08err = *off;
079900170808           S1Cdat = G08dat;
080000170808         endif;
080100170808
080200170808         S1Cora   = AZCAAds.CAAtim - ( G08inv * 1000000 );
080300170808         S1Ccdm   = AZCAAds.CAAcdm;
080400170808         S1Ctpm   = AZCAAds.CAAtpm;
080500170808         S1Cnot   = AZCAAds.CAAnot;
080600170809
080700170809         // -?Controllo compatibilità di Filiale Utente con IP Address?
080800170810         //  ?(NON per utenti EDP*)?
080900170810         if  AZCAAds.CAAcdm = 10  and
081000170810             %subst( AZCAAds.CAAute : 1 : 3 ) <> 'EDP';
081100170809           exsr  sr_Chk_dutPOU;
081200170809         endif;
081300170808
081400170808         // -?Impostazione attributi di visualizzazione?
081500170809         exsr  sr_DspAttrS01;
081600170808
081700170808       ENDSR;
081800170808
081900170808       //--------------------------------------------------------------
082000170808       //?Gestione opzioni del subfile S01                             ?
082100170808       //--------------------------------------------------------------
082200170808       BEGSR  sr_OpzS01;
082300170808
082400170808         // -?Spegnimento degli indicatori di errore?
082500170809         %subst( IndDspF : 50 ) = *off;
082600170809
082700170808         clear  SavS01csr;
082800170808
082900170808         // -?Ciclo di lettura subfile?
083000170809         readc  ubIPJs01;
083100170808
083200170809         DoW  Not %eof( ubChkIPJd );
083300170808
083400170809           $SflNxtChg = *off;
083500170809           %subst( IndDspF : 50 ) = *off;
083600170809           C1RcdNbr   = S01nrr;
083700170808
083800170808           select;
083900170808
084000170808             // -?Nessuna opzione?
084100170809             when  S1Copz = *zero;
084200170808
084300170808             // -?Opz. 4=Cancellazione registrazione collegamento?
084400170809             when  S1Copz = 4;
084500170809               exsr  sr_Opz04;
084600170809
084700170809             // -?Opz. 5=Visualizzazione utente?
084800170809             when  S1Copz = 5;
084900170809               exsr  sr_Opz05;
085000170808
085100170808             // -?Opzione errata?
085200170808             other;
085300170809               $ErrGenerico = *on;
085400170809               $ErrMessage  = *on;
085500170809               $PosCurOPZ   = *on;
085600170809               VIDmsg = sk_Msg(09);
085700170808
085800170808           endsl;
085900170808
086000170808           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
086100170808           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
086200170808           if  S1Copz   <> *zero  and
086300170808              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
086400170808             SavS01csr   = C1RcdNbr;
086500170808           endif;
086600170808
086700170808           // -?Aggiornamento sfl?
086800170808           select;
086900170808             when  $ErrMessage;
087000170809               $SflNxtChg  = *on;
087100170809               C1CsrRrn    = C1RcdNbr;
087200170808             when  $ErrGenerico;
087300170809               C1CsrRrn    = C1RcdNbr;
087400170808               clear  S1Copz;
087500170808             other;
087600170810               if  S1Copz = 4;
087700170809                 $InzS01 = *on;
087800170809               endif;
087900170810               $ProtectOPZ = ( S1Copz = 4 );
088000170809               clear  S1Copz;
088100170808           endsl;
088200170808
088300170808           exsr  sr_DspAttrS01;
088400170808           UPDATE  ubIPJs01;
088500170808
088600170809           if  $ErrGenerico  or  $ErrMessage;
088700170808             leave;
088800170808           endif;
088900170808
089000170808           readc  ubIPJs01;
089100170808
089200170808         ENDDO;
089300170808
089400170808         // -?Riposizionam. cursore sul record contenente la prima opz.?
089500170808         //  ?(se non sono stati rilevati errori)?
089600170808         if  NOT $ErrMessage   and   NOT $ErrGenerico   and
089700170808             SavS01csr > *zero;
089800170808           C1CsrRrn = SavS01csr;
089900170808         endif;
090000170808
090100170808         // -?Se richiesto il ri-caricamento del subfile: occorre?
090200170808         //  ?prima chiudere il cursore SQL (attualmente aperto)?
090300170808         if  $InzS01 = *on;
090400170808           exsr  sr_CloseCursor;
090500170808         endif;
090600170808
090700170808       ENDSR;
090800170808
090900170808       //--------------------------------------------------------------
091000170808       //?Impostazione attributi di visualizzazione nel subfile S1.    ?
091100170808       //--------------------------------------------------------------
091200170808       BEGSR  sr_DspAttrS01;
091300170808
091400170809         // -?Filiale Utente incompatibile con IP Address?
091500170809         $Incompat = ( S1Hin40 = *on );
091600170808
091700170808         // -?Ord. 0 = Utente?
091800170809         $Ord_0    = ( wOrdSfl = *zero );
091900170808
092000170808         // -?Ord. 1 = Device?
092100170809         $Ord_1    = ( wOrdSfl = 1 );
092200170808
092300170808         // -?Ord. 2 = IP Address?
092400170809         $Ord_2    = ( wOrdSfl = 2 );
092500170808
092600170808         // -?Ord. 3 = Data/Ora?
092700170809         $Ord_3    = ( wOrdSfl = 3 );
092800170808
092900170808         // -?Ord. 4 = Codice Messaggio?
093000170809         $Ord_4    = ( wOrdSfl = 4 );
093100170808
093200170808         // -?Ord. 5 = Tipo Messaggio?
093300170809         $Ord_5    = ( wOrdSfl = 5 );
093400170808
093500170808       ENDSR;
093600170808
093700170808       //--------------------------------------------------------------
093800170808       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
093900170808       //?F08-Cambia ordinamento sfl                                   ?
094000170808       //--------------------------------------------------------------
094100170808       BEGSR  sr_F08S01;
094200170809
094300170809         exsr  sr_CloseCursor;
094400170809
094500170809         select;
094600170809           // -?Subfile vuoto: nessun record da ordinare?
094700170809           when  S01nrr <= 1;
094800170809             clear  V1Pf08;
094900170809             leavesr;
095000170809           // -?Cambio ordinamento?
095100170809           when  wOrdSfl = 5;
095200170809             clear  wOrdSfl;
095300170809           other;
095400170809             wOrdSfl += 1;
095500170809         endsl;
095600170809
095700170809         $InzS01 = *on;
095800170808
095900170808       ENDSR;
096000170808
096100170808       //--------------------------------------------------------------
096200170808       //?Gestione tasto funzionale F13 da videata C01 / S01           ?
096300170808       //?F13-Ripetizione opzione                                      ?
096400170808       //--------------------------------------------------------------
096500170808       BEGSR  sr_F13S01;
096600170808
096700170808         // -?Reperimento pagina del sfl (e Nrr del suo primo record)?
096800170808         //  ?dalla posizione del cursore?
096900170808         wPag = C1CsrRrn / c_SflPag;
097000170808         wRig = %rem( C1CsrRrn : c_SflPag);
097100170808         if  wRig > *zero;
097200170808           wRig = (wPag * c_SflPag) + 1;
097300170808         else;                           // -?ultimo rec. della pag.?
097400170808           wRig = (wPag * c_SflPag) - c_SflPag + 1;
097500170808         endif;
097600170808         SavS01csr = wRig;
097700170808
097800170808         // -?CONTROLLO OPZIONI INSERITE NEL SUBFILE:?
097900170808
098000170808         // -?Reperimento della 1ª opzione inserita?
098100170808         //  ?nella pagina dov'è il cursore?
098200170809         readc  ubIPJs01;
098300170809         DoW  Not %eof(ubChkIPJd)  and  S1Copz <> *zero  and
098400170808              S01nrr < SavS01csr;
098500170809           $SflNxtChg = *on;
098600170809           update  ubIPJs01;
098700170809           readc  ubIPJs01;
098800170808         EndDo;
098900170809         if  Not %eof(ubChkIPJd)  and  S1Copz <> *zero;
099000170809           $SflNxtChg = *on;
099100170809           update  ubIPJs01;
099200170808         endif;
099300170808
099400170808         // -?Nessuna opzione nella pagina?
099500170809         if  %eof(ubChkIPJd)       or
099600170808             S01nrr <  SavS01csr   or
099700170808             S01nrr > (SavS01csr + c_SflPag - 1);
099800170808           C1RcdNbr  = SavS01csr;
099900170808           C1CsrRrn  = SavS01csr;
100000170808           clear  $ForzaOpz;
100100170808           clear  SavS1Copz;
100200170808           clear  SavS1Cop1;
100300170809           clear  SavS01cs1;
100400170809           clear  SavS01cs2;
100500170809           $ErrGenerico = *on;
100600170809           $ErrMessage  = *on;
100700170809           $PosCurOPZ   = *on;
100800170809           VIDmsg = sk_Msg(06);
100900170808           leavesr;
101000170808         endif;
101100170808
101200170808         // -?Memorizzazione 1ª opzione nella pagina?
101300170808         SavS01csr = S01nrr;
101400170808         SavS1Copz = S1Copz;
101500170808         if  S01nrr <> SavS01cs1  or
101600170808             S1Copz <> SavS1Cop1;
101700170808           clear  $ForzaOpz;
101800170808         endif;
101900170808         SavS01cs1 = S01nrr;
102000170808         SavS1Cop1 = S1Copz;
102100170808
102200170808         // -?Reperimento delle altre eventuali opzioni?
102300170809         readc  ubIPJs01;
102400170809         DOW  Not %eof(ubChkIPJd);
102500170808
102600170808           // ·?Aggiornamento subfile?
102700170808           if  S1Copz <> *zero;
102800170809             $SflNxtChg = *on;
102900170809             update  ubIPJs01;
103000170808           endif;
103100170808
103200170808           // ·?Reperita una 2ª opzione?
103300170808           Select;
103400170808
103500170808             // ·?Vuota!?
103600170808             When  S1Copz = *zero;
103700170808
103800170808             // ·?1ª opzione nella pagina del cursore  e?
103900170808             //  ?2ª opzione successiva diversa:?
104000170808             //  ?messaggio di avviso di "ricopertura"?
104100170808             When  (SavS01csr >= wRig              and
104200170808                    S01nrr    >  SavS01csr)        and
104300170808                    S1Copz    <> SavS1Copz         and
104400170808                    Not $ForzaOpz;
104500170808               $ForzaOpz = *on;
104600170808               SavS01cs2 = S01nrr;
104700170808               C1RcdNbr  = SavS01csr;
104800170808               C1CsrRrn  = SavS01csr;
104900170809               $ErrGenerico = *on;
105000170809               $ErrMessage  = *on;
105100170809               $PosCurOPZ   = *on;
105200170809               VIDmsg = sk_Msg(07);
105300170808               leavesr;
105400170808
105500170808             // ·?2ª opzione sul record dove è posizionato il cursore?
105600170808             //  ?(la nuova da considerare per la duplicazione)?
105700170808             When  C1CsrRrn = S01nrr;
105800170808               SavS01csr = S01nrr;
105900170808               SavS1Copz = S1Copz;
106000170808
106100170808             // ·?Più opzioni nella stessa videata e?
106200170808             //  ?cursore su nessuna delle opzioni?
106300170808             //  ?(riposizionare il cursore)?
106400170808             //  ?Se stessa opzione in videate diverse: vince la?
106500170808             //  ?posizione della prima opzione.?
106600170808             When  C1CsrRrn <> SavS01csr  and
106700170808                   C1CsrRrn <> S01nrr     and
106800170808                   S01nrr   <  wRig + c_SflPag;
106900170808               SavS01cs2 = S01nrr;
107000170808               C1RcdNbr  = SavS01csr;
107100170808               C1CsrRrn  = SavS01csr;
107200170809               $ErrGenerico = *on;
107300170809               $ErrMessage  = *on;
107400170809               $PosCurOPZ   = *on;
107500170809               VIDmsg = sk_Msg(08);
107600170808               leavesr;
107700170808
107800170808           EndSl;
107900170808
108000170809           readc  ubIPJs01;
108100170808
108200170808         ENDDO;
108300170808
108400170808         // ·?Opzione NON abilitata?
108500170809         if  SavS1Copz <> 1  and  SavS1Copz <> 2  and
108600170809             SavS1Copz <> 3  and  SavS1Copz <> 4  and
108700170809             SavS1Copz <> 5  and  SavS1Copz <> 6;
108800170808           C1RcdNbr  = SavS01csr;
108900170808           C1CsrRrn  = SavS01csr;
109000170809           $ErrGenerico = *on;
109100170809           $ErrMessage  = *on;
109200170809           $PosCurOPZ   = *on;
109300170809           VIDmsg = sk_Msg(09);
109400170808           leavesr;
109500170808         endif;
109600170808
109700170808         // -?Ripetizione opzione sul resto del subfile?
109800170808         S01nrr = SavS01csr + 1;
109900170809         chain  S01nrr  ubIPJs01;
110000170809         DoW  %found(ubChkIPJd);
110100170809           $SflNxtChg = *on;
110200170808           S1Copz = SavS1Copz;
110300170809           update  ubIPJs01;
110400170808           S01nrr += 1;
110500170809           chain  S01nrr  ubIPJs01;
110600170808         EndDo;
110700170808
110800170808       ENDSR;
110900170809
111000170809       //--------------------------------------------------------------
111100170809       //?Gestione window-subfile SW1 - Elenco Codici Errore.          ?
111200170809       //--------------------------------------------------------------
111300170809       BEGSR  sr_GesSW1;
111400170809
111500170809         // -?Inizializzazione subfile?
111600170809         if  $InzSW1;
111700170809           exsr  sr_InzSW1;
111800170809           $InzSW1 = *off;
111900170809         endif;
112000170809
112100170809         // -?Posizionamento cursore?
112200170809         //CWRcdNbr = 1;
112300170809
112400170809         // -?Emissione Window?
112500170809         write  ubIPJpW1;
112600170809         exfmt  ubIPJcW1;
112700170809
112800170809         reset  $ErrMessage;
112900170809         reset  $ErrGenerico;
113000170809         clear  VIDmsg;
113100170809
113200170809         // -?F12=Ritorno?
113300170809         if  dsp_aid = c_F12;
113400170809           $Video = $VideoPrec;
113500170809         endif;
113600170809
113700170809       ENDSR;
113800170809
113900170809       //--------------------------------------------------------------
114000170809       //?Inizializzazione window-subfile SW1.                         ?
114100170809       //--------------------------------------------------------------
114200170809       BEGSR  sr_InzSW1;
114300170809
114400170809         // -?Pulizia subfile?
114500170809         $WSflDsp_N   = *on;
114600170809         $WSflDspCt_N = *on;
114700170810         write  ubIPJcW1;
114800170809         $WSflDspCt_N = *off;
114900170809         $WSflEnd     = *off;
115000170809
115100170810         //*·clear  CWRcdNbr;
115200170810         //*·clear  CWCsrRrn;
115300170809         clear  SW1nrr;
115400170809         clear  VIDmsg;
115500170809         $ErrMessage  = *off;
115600170809         $ErrGenerico = *off;
115700170809
115800170809         // -?Caricamento completo del subfile?
115900170809         For  xx = 1  To  %elem( sk_CdM );
116000170809           If  %subst( sk_CdM_prg(xx) : 1 : 3 ) <> '|--';
116100170809             SW1cdm  = xx;
116200170809             SW1tpm  = %subst( sk_CdM(xx) : 75 : 1);
116300170809             SW1not  = %subst( sk_CdM(xx) : 1 : %len(SW1not) );
116400170809             SW1nrr += 1;
116500170809             write  ubIPJsW1;
116600170809           EndIf;
116700170809         EndFor;
116800170809
116900170809         // -?Visualizzazione del SFL (se ci sono dati)?
117000170809         $WSflDsp_N = ( SW1nrr <= *zero );
117100170809
117200170809         // -?Attivazione del SFLEND?
117300170809         $WSflEnd   = *on;
117400170809
117500170810         //*·// -?Impaginazione subfile e?
117600170810         //*·//  ?posizionamento cursore al 1° rcd del subfile?
117700170810         //*·if  SW1nrr > *zero;
117800170810         //*·  CWRcdNbr = 1;
117900170810         //*·  CWCsrRrn = 1;
118000170810         //*·else;
118100170810         //*·  clear  CWRcdNbr;
118200170810         //*·endif;
118300170810
118400170809
118500170809       ENDSR;
118600170809
118700170809       //--------------------------------------------------------------
118800170809       //?Gestione opzione 4=Cancellazione.                            ?
118900170809       //--------------------------------------------------------------
119000170809       BEGSR  sr_Opz04;
119100170809
119200170809         // -?Richiesta conferma per cancellazione record?
119300170809         If  Not $Conferma;
119400170809           clear  ubIPJw02;
119500170809                   // *...+....1....+....2....+....3....+....4
119600170809           W02txt1 = 'Premere F6=Conferma per confermare la   ';
119700170809           W02txt2 = 'cancellazione di queste registrazioni.  ';
119800170809           W02txt3 = 'Premere F12=Ritorno altrimenti.         ';
119900170809           DoW  dsp_aid <> c_F06  and  dsp_aid <> c_F12;
120000170809             exfmt  ubIPJw02;
120100170809           EndDo;
120200170809           $Conferma = ( dsp_aid = c_F06 );
120300170809         EndIf;
120400170809
120500170809         // -?Premuto F12=Ritorno?
120600170809         if  Not $Conferma;
120700170810           $ErrGenerico = *on;
120800170809           leavesr;
120900170809         endif;
121000170809
121100170809         // -?Cancellazione record?
121200170809         chain  S1Hrrn  AZCAA000;
121300170809
121400170809         if  %found( AZCAA00F );
121500170809           delete  AZCAA000;
121600170809         endif;
121700170809
121800170809       ENDSR;
121900170809
122000170809       //--------------------------------------------------------------
122100170809       //?Gestione opzione 5=Visualizzazione Utente.                   ?
122200170809       //--------------------------------------------------------------
122300170809       BEGSR  sr_Opz05;
122400170809
122500170809         clear  TIBS31ds;
122600170809         TIBS31ds.I31ute = S1Cute;
122700170809         TIBS31ds.I31Opz = %editc( S1Copz : 'X' );
122800170809
122900170809         TIBS31R ( KPJBA : TIBS31ds );
123000170809
123100170809         Select;
123200170809           When  TIBS31ds.O31err <> *blank;
123300170809             $ErrGenerico = *on;
123400170809             $ErrMessage  = *on;
123500170809             VIDmsg = TIBS31ds.O31msg;
123600170809             leavesr;
123700170809           When  TIBS31ds.O31tfu =  'F03';
123800170809             $ErrGenerico = *on;
123900170809             leavesr;
124000170809           When  TIBS31ds.O31tfu =  'F12';
124100170809             leavesr;
124200170809         EndSl;
124300170809
124400170809       ENDSR;
124500170809
124600170809       //--------------------------------------------------------------
124700170809       //?Controllo Filiale dell'utente.                               ?
124800170809       //--------------------------------------------------------------
124900170809       BEGSR  sr_Chk_dutPOU;
125000170809
125100170809         // -?Reperimento filiale dall'IP Terminale?
125200170809         // ·?2° "." ("10.XXX.") dovrebbe essere nella 7ª posizione?
125300170809         xx = %scan( '.' : AZCAAds.CAAip : 4 );
125400170809         if  xx = *zero  or  xx = 4  or  xx > 7;
125500170809           S1Hin40 = *on;
125600170809           leavesr;
125700170809         endif;
125800170809         Monitor;
125900170809           wIPfil = %int( %subst( AZCAAds.CAAip : 4 : ( xx - 4 ) ) );
126000170809           On-Error;
126100170809             S1Hin40 = *on;
126200170809             leavesr;
126300170809         EndMon;
126400170810
126500170810         // -?Reperimento filiale dall'Utente?
126600170810         chain  ( AZCAAds.CAAute )  AZUTE000;
126700170810         if  NOT  %found(AZUTE01L);
126800170810           S1Hin40 = *on;
126900170810           leavesr;
127000170810         endif;
127100170810
127200170810         // -?Valorizzazione "Flags Abilitazioni utenti aziendali"?
127300170810         dUte01 = UTEfaf;
127400170809
127500170809         // -?Controllo corrispondenza diretta IP Terminale / Fil. Utente?
127600170810         chain  ( UTEfil )  AZORG;
127700170809         if  NOT  %found(AZORG01L);
127800170809           S1Hin40 = *on;
127900170809           leavesr;
128000170809         endif;
128100170810
128200170810         // -?Valorizzazione "143ª descrizione dell'organigramma"?
128300170810         Og143 = orgDE3;
128400170809
128500170809         // -?IP terminale ed IP filiale corrispondenti: OK?
128600170809         //  ?(se §OGIIP = *blank => si utilizza il codice filiale)?
128700170809         if  wIPfil = UTEfil or  %editc( wIPfil : 'X' ) = Og143.§ogIIP;
128800170809           leavesr;
128900170809         endif;
129000170810
129100170810         // -?IP terminale e "Filiale Ufficio" (prevista in AZUTE00F)?
129200170810         //  ?corrispondenti: OK?
129300170810         if  %editc( wIPfil : 'X' ) = dUte01.§uteFUF;
129400170810           leavesr;
129500170810         endif;
129600170809
129700170810         // -?Reperimento Filiali a cui l'utente è (quasi) abilitato?
129800170809         clear  sk_POg;
129900170809         clear  TRUL06ds;
130000170809         D06cod = 'PP';
130100170810         if  dUte01.§UTEfuf <= *zero;
130200170810           D06key = %editc( UTEfil : 'X' );
130300170810         else;
130400170810           D06key = dUte01.§UTEfuf;
130500170810         endif;
130600170809         KPJBU    = TRUL06ds;
130700170809         trul06r ( kpjba );
130800170810         TRUL06ds = KPJBU;
130900170809
131000170809         // -?L'indirizzo IP del device è di una delle filiali a cui è?
131100170809         //  ?legato l'utente => uscita: OK?
131200170809         if  %lookup( %editc( wIPfil : 'X' ) : sk_POg ) > *zero;
131300170810           leavesr;
131400170809         endif;
131500170810
131600170810         // -?Verifica se l'indirizzo IP del device è comunque previsto?
131700170810         //  ?in una delle filiali a cui è abilitato l'utente?
131800170810         clear  Og143;
131900170810         For  xx = 1  to  %elem( sk_POg );
132000170810
132100170810           if  sk_POg(xx) <= *zero;
132200170810             leave;
132300170810           endif;
132400170810
132500170810           // -?Per farlo: verifica in AZORG se almeno una delle fil.?
132600170810           //  ?a cui è abilitato l'utente  ha un indirizzo IP uguale?
132700170810           //  ?a quello del device?
132800170810           If  sk_POg(xx) <> %editc( UTEfil : 'X' );
132900170810             chain  ( %int( sk_POg(xx) ) )  AZORG;
133000170810             if  %found(AZORG01L);
133100170810               Og143 = orgDE3;
133200170810               // -?Al primo indirizzo IP uguale => uscita (OK)?
133300170810               if  Og143.§ogIIP = %editc( wIPfil : 'X' );
133400170810                 leavesr;
133500170810               endif;
133600170810             endif;
133700170810           EndIf;
133800170810
133900170810         EndFor;
134000170810
134100170810         S1Hin40 = *on;
134200170810         //*·leavesr;
134300170809
134400170809       ENDSR;
134500170808
134600170808       //--------------------------------------------------------------
134700170808       //?Preparazione stringa SQL S1.                                 ?
134800170808       //--------------------------------------------------------------
134900170808       BEGSR  sr_PrepSQL;
135000170808
135100170808         $EoF       = *off;
135200170808         $First     = *on;
135300170808         $First_cdm = *on;
135400170808
135500170808         wSQL = 'select rrn(AZCAA00F), AZCAA00F.* from AZCAA00F';
135600170808
135700170808         // -?Selezione per Utente?
135800170808         If  V1Cute <> *blank;
135900170808           if  $First;
136000170808             wSQL += ' where';
136100170808           else;
136200170808             wSQL += ' and';
136300170808           endif;
136400170808           wSQL += ' CAAute like ''' + %trimR( V1Cute ) + '%''';
136500170808           $First = *off;
136600170808         EndIf;
136700170808
136800170808         // -?Selezione per Device?
136900170808         If  V1Cdev <> *blank;
137000170808           if  $First;
137100170808             wSQL += ' where';
137200170808           else;
137300170808             wSQL += ' and';
137400170808           endif;
137500170808           wSQL += ' CAAdev like ''' + %trimR( V1Cdev ) + '%''';
137600170808           $First = *off;
137700170808         EndIf;
137800170808
137900170808         // -?Selezione per Data/Ora collegamento Iniziali?
138000170808         If  V1CdatI <> *zero  or  V1CoraI <> *zero;
138100170808           if  $First;
138200170808             wSQL += ' where';
138300170808           else;
138400170808             wSQL += ' and';
138500170808           endif;
138600170808           wSQL += ' CAAtim >= ' +
138700170808                   %trim( %char( ( W1CdatI * 1000000 ) + V1CoraI ) );
138800170808           $First = *off;
138900170808         EndIf;
139000170808
139100170808         // -?Selezione per Data/Ora collegamento Finali?
139200170808         If  V1CdatF <> *zero  or  V1CoraF <> *zero;
139300170808           if  $First;
139400170808             wSQL += ' where';
139500170808           else;
139600170808             wSQL += ' and';
139700170808           endif;
139800170808           wSQL += ' CAAtim <= ' +
139900170808                   %trim( %char( ( W1CdatF * 1000000 ) + V1CoraF ) );
140000170808           $First = *off;
140100170808         EndIf;
140200170808
140300170808         // -?Selezione per Codice Messaggio?
140400170808         if  V1Ccdm_ds <> *zero;
140500170808           if  $First;
140600170808             wSQL += ' where';
140700170808           else;
140800170808             wSQL += ' and';
140900170808           endif;
141000170808           wSQL += ' CAAcdm in (';
141100170808           $First_cdm = *on;
141200170808           For  xx = 1  To  %elem(sk_V1Ccdm);
141300170808             if  sk_V1Ccdm(xx) <> *zero;
141400170808               if  NOT $First_cdm;
141500170808                 wSQL += ', ';
141600170808               endif;
141700170808               wSQL += %trim( %char( sk_V1Ccdm(xx) ) );
141800170808               $First_cdm = *off;
141900170808             endif;
142000170808           EndFor;
142100170808           wSQL += ')';
142200170808           $First = *off;
142300170808         endif;
142400170808
142500170808         // -?Ordinamento?
142600170808         select;
142700170808           // -?Ordinamento per Utente?
142800170808           when  wOrdSfl = *zero;
142900170808             wSQL += ' order by CAAute, CAAtim';
143000170808           // -?Ordinamento per Device?
143100170808           when  wOrdSfl = 1;
143200170808             wSQL += ' order by CAAdev, CAAtim';
143300170808           // -?Ordinamento per IP Address?
143400170808           when  wOrdSfl = 2;
143500170808             wSQL += ' order by CAAip, CAAtim';
143600170808           // -?Ordinamento per Data/Ora?
143700170808           when  wOrdSfl = 3;
143800170808             wSQL += ' order by CAAtim, CAAute';
143900170808           // -?Ordinamento per Codice Messaggio?
144000170808           when  wOrdSfl = 4;
144100170808             wSQL += ' order by CAAcdm, CAAtim';
144200170808           // -?Ordinamento per Tipo Messaggio?
144300170808           when  wOrdSfl = 5;
144400170808             wSQL += ' order by CAAtpm, CAAcdm, CAAtim';
144500170808         endsl;
144600170808
144700170808         wSQL += ' for fetch only';
144800170808
144900170808       ENDSR;
145000170808
145100170808       //--------------------------------------------------------------
145200170808       //?Apertura cursore C1.                                         ?
145300170808       //--------------------------------------------------------------
145400170808       BEGSR  sr_OpenCursor;
145500170808
145600170808         // -?Dichiarazione cursore?
145700170808         exec sql   prepare S1   from :wSQL;
145800170808         exec sql   declare C1   cursor for S1;
145900170808
146000170808         // -?Apertura del cursore?
146100170808         exec sql   open C1;
146200170808
146300170808         if  SQLcode < *zero;
146400170808           $EoF = *on;
146500170808           exsr  sr_PrintErrSQL;
146600170808         endif;
146700170808
146800170808       ENDSR;
146900170808
147000170808       //--------------------------------------------------------------
147100170808       //?Chiusura cursore C1.                                         ?
147200170808       //--------------------------------------------------------------
147300170808       BEGSR  sr_CloseCursor;
147400170808
147500170808         // -?Chiusura del cursore?
147600170808         exec sql   close C1;
147700170808
147800170808       ENDSR;
147900170808
148000170808       //--------------------------------------------------------------
148100170808       //?Lettura cursore C1.                                          ?
148200170808       //--------------------------------------------------------------
148300170808       BEGSR  sr_ReadCursor;
148400170808
148500170808         clear  AZCAAds;
148600170808
148700170808         exec sql   fetch next   from C1   into :wCAArrn, :AZCAAds;
148800170808
148900170808         select;
149000170808           when  SQLcode = 100;
149100170808             $EoF = *on;
149200170808           when  SQLcode < *zero;
149300170808             $EoF = *on;
149400170808             exsr  sr_PrintErrSQL;
149500170808         endsl;
149600170808
149700170808       ENDSR;
149800170808
149900170808       //--------------------------------------------------------------
150000170808       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
150100170808       //--------------------------------------------------------------
150200170808       BEGSR  sr_PrintErrSQL;
150300170808
150400170808         // -?Stampa del Dump?
150500170808         Dump(A);
150600170808
150700170808         // -?Stampa del Job-Log?
150800170808         Qcmd = 'DSPJOBLOG job(*) output(*print)';
150900170808         exsr  sr_ExecCmd;
151000170808
151100170808         // -?Chiusura del programma?
151200170808         exsr  sr_RoutEnd;
151300170808
151400170808       ENDSR;
151500170808
151600170808       //--------------------------------------------------------------
151700170808       //?Esecuzione del comando (già impostato).                      ?
151800170808       //--------------------------------------------------------------
151900170808       BEGSR  sr_ExecCmd;
152000170808
152100170808         clear Qcap0100;
152200170808         Qcabcsdh = *off;
152300170808         Qcapa    = *off;
152400170808         Qcacmdss = *off;
152500170808         Qcaerved = *allX'00';
152600170808
152700170808         clear Qusec;
152800170808         Qusbprv  = %size(Qusec);
152900170808
153000170808         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
153100170808                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
153200170808                           0 : 0 : Qusec);
153300170808
153400170808         // -?Invio *msg per segnalazione errore?
153500170808         //if  Qusei <> *blank;
153600170808         //  ...;
153700170808         //endif;
153800170808
153900170808       ENDSR;
154000170808
154100170808       //--------------------------------------------------------------
154200170808       //?Operazioni finali.                                         ?
154300170808       //--------------------------------------------------------------
154400170808       BEGSR  sr_RoutEnd;
154500170808
154600170808         // -?Chiusura *pgm?
154700170808         return;
154800170808
154900170808       ENDSR;
155000170808
155100170808      /end-free
155200170808
155300170808       //--------------------------------------------------------------
155400170808       //?Schiere a tempo di compilazione.                             ?
155500170808       //--------------------------------------------------------------
155600170808
155700170809....+.*..1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
155800170809** --?sk_CdM:?Codici Messaggio in AZCAA (da *pgm "ubChkIPJ") --------* Tipo Progr?
155900170808Dati dell'utente non reperibili                                       |   0|-- 1 NON usato
156000170810Errore nell'esecuzione dell'API?QDCRDEVD=Retrieve Device Description  |   1|   2
156100170808Errore nel reperimento dell'indirizzo IP del dispositivo              |   1|   3
156200170808Dispositivo NON di tipo IP (terminale)                                |   1|-- 4 NON usato
156300170808Indirizzo IP errato: non inizia con "10."                             |   1|   5
156400170808Indirizzo IP errato: non trovato carattere "." nella 7ª posizione     |   1|   6
156500170808Codice Filiale non numerico                                           |   0|-- 7 NON usato
156600170808Filiale utente non trovata in anagrafica                              |   0|-- 8 NON usato
156700170808Utente non abilitato alla Filiale del dispositivo                     |   1|   9
156800170808Utente non abilitato alla Filiale reperita dall'IP del dispositivo    |   1|  10
156900170809** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
157000170808Data formalmente errata                                                        1
157100170808Ora formalmente errata                                                         2
157200170810Codice Messaggio errato: previsti valori da 01 a 10                            3 - Libero
157300170808Inserito un valore errato                                                      4
157400170809Raggiunto il limite massimo dei record caricabili a video (9999)               5
157500170809Immettere un'opzione su questa pagina prima di premere F13                     6
157600170809Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa 7
157700170809Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F13 8
157800170809Opzione errata                                                                 9
