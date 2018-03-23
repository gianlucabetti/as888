000100160318       //==============================================================
000200160322       //?FIMS13R * Dati per Report SIM Magazzino.                     ?
000300160318       //==============================================================
000400160318
000500160318       //--------------------------------------------------------------
000600160318       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160318       //--------------------------------------------------------------
000800160318
000900160318     /*PRM  dbgview(*source)
001000160318     /*END
001100160318
001200160318       //--------------------------------------------------------------
001300160318       //?Specifiche di controllo.                                     ?
001400160318       //--------------------------------------------------------------
001500160318
001600160318     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160412     h dftactgrp(*no) bnddir('TRUL')
001800160318     h alwnull(*inputonly)
001900160318
002000160318       //--------------------------------------------------------------
002100160318       //?Dichiarazione file.                                          ?
002200160318       //--------------------------------------------------------------
002300160318
002400160321       // -?Organigrammi aziendali?
002500160321     fAZORG01L  if   e           k disk
002600160408       // -?Tabelle?
002700160408     fTNTBE01L  if   e           k disk
002800160413
002900160318       // -?Macchinone smistacolli - Attività smistamento?
003000170201     fFNMSA02L  if   e           k disk    usropn
003100160413
003200160321       // -?Statistiche Arrivi?
003300160322     fFISAR06L  if   e           k disk    usropn
003400160413
003500160322       // -?Statistiche a Terra?
003600160322     fFISAT02L  if   e           k disk    usropn
003700160413
003800160413       // -?Fogli Vari?
003900160413     fFNFVV02L  if   e           k disk    usropn
004000160318
004100160318       // -?Video Gestione?
004200160318     fFIMS13D   cf   e             workstn
004300160412     f                                     indds( IndDspF )
004400160412     f                                     infds( InfDspF )
004500160412     f                                     sfile( MS13S06 : S06nrr )
004600160412     f                                     sfile( MS13S07 : S07nrr )
004700160318
004800160318       //--------------------------------------------------------------
004900160318       //?Definizione costanti.                                        ?
005000160318       //--------------------------------------------------------------
005100160412
005200160412       // -?Numero di record in ciascuna videata di subfile?
005300160526     d c_SflPag        c                   const(10)
005400160412     d c_SflPag7       c                   const(10)
005500160412
005600160412       // -?Numero massimo di record gestibili in un SubFile?
005700160412     d c_MaxRec        c                   const(9999)
005800160412
005900160412       // -?Costante per controllo "caratteri solo numerici"?
006000160412     d c_Digits        c                   const('0123456789')
006100160318
006200160318       // -?Tasti funzionali a video?
006300160318     d c_F01           c                   const(x'31')
006400160318     d c_F02           c                   const(x'32')
006500160318     d c_F03           c                   const(x'33')
006600160318     d c_F04           c                   const(x'34')
006700160318     d c_F05           c                   const(x'35')
006800160318     d c_F06           c                   const(x'36')
006900160318     d c_F07           c                   const(x'37')
007000160318     d c_F08           c                   const(x'38')
007100160318     d c_F09           c                   const(x'39')
007200160318     d c_F10           c                   const(x'3A')
007300160318     d c_F11           c                   const(x'3B')
007400160318     d c_F12           c                   const(x'3C')
007500160318     d c_F13           c                   const(x'B1')
007600160318     d c_F14           c                   const(x'B2')
007700160318     d c_F15           c                   const(x'B3')
007800160318     d c_F16           c                   const(x'B4')
007900160318     d c_F17           c                   const(x'B5')
008000160318     d c_F18           c                   const(x'B6')
008100160318     d c_F19           c                   const(x'B7')
008200160318     d c_F20           c                   const(x'B8')
008300160318     d c_F21           c                   const(x'B9')
008400160318     d c_F22           c                   const(x'BA')
008500160318     d c_F23           c                   const(x'BB')
008600160318     d c_F24           c                   const(x'BC')
008700160318     d c_Enter         c                   const(x'F1')
008800160318     d c_RollDown      c                   const(x'F4')
008900160318     d c_RollUp        c                   const(x'F5')
009000161107
009100161107       // -?Attributi di Visualizzazione?
009200161107     d c_DspAtrNorm    c                   const(x'20')
009300161107     d c_DspAtr_RI     c                   const(x'21')
009400161107     d c_DspAtr_HI     c                   const(x'22')
009500161107     d c_DspAtr_BL     c                   const(x'28')
009600161107     d c_Color_Wht     c                   const(x'22')
009700161107     d c_Color_Wht_RI  c                   const(x'23')
009800161107     d c_Color_Wht_UL  c                   const(x'26')
009900161107     d c_Color_Red     c                   const(x'28')
010000161107     d c_Color_Red_RI  c                   const(x'29')
010100161107     d c_Color_Red_BL  c                   const(x'2A')
010200161107     d c_Color_Red_UL  c                   const(x'2C')
010300160318
010400160318       //--------------------------------------------------------------
010500160318       //?Definizione schiere.                                         ?
010600160318       //--------------------------------------------------------------
010700160318
010800160318       // -?Messaggi di errore?
010900161107     d sk_Msg          s             78    dim(15)  ctdata  perrcd(1)
011000160408
011100160408       // -?Cod. Postazioni Fisse x tipo Applicazione = Incompatibili?
011200160408     d sk_MSL_Pep      s            - 9    dim(20)  like(dMSL.§MSLpep)
011300160408     d                                     inz
011400160408       // -?N° Entrata Post.Fisse x tipo Applicazione = Incompatibili?
011500160408     d sk_MSL_Ent      s                   dim(20)  like(dMSL.§MSLent)
011600160408     d                                     inz
011700160408       // -?Descriz.   Post.Fisse x tipo Applicazione = Incompatibili?
011800160408     d sk_MSL_Des      s                   dim(20)  like(dMSL.§MSLdes)
011900160408     d                                     inz
012000161028     d sk_MSL_De5      s                   dim(20)  like(dMSL.§MSLde5)
012100161028     d                                     inz
012200160408       // -?Colli per Postaz. Fissa (tipo Applicazione = Incompatibili)?
012300160408     d sk_Ncl_Incomp   s              5  0 dim(20)
012400160408     d                                     inz
012500160318
012600160318       //--------------------------------------------------------------
012700160318       //?Definizione aree dati.                                       ?
012800160318       //--------------------------------------------------------------
012900160318
013000160318       // -?Dati utente?
013100160318     d §AzUte        e ds                  extname(AZUTE00F)
013200160318     d                                     dtaara
013300160318     d §DatiUte      e ds                  extname(dDatiUte)
013400160318     d                                     dtaara
013500160318
013600160318       //--------------------------------------------------------------
013700160318       //?Definizione strutture dati.                                  ?
013800160318       //--------------------------------------------------------------
013900160318
014000160318       // -?Status ds?
014100160318     d Status         sds
014200160318     d   SDSpgm          *proc
014300160318     d*//SDSprm          *parms
014400160318     d*//SDSdta              191    198
014500160526     d   SDSjob              244    253
014600160318     d   SDSusr              254    263
014700160318
014800160318       // -?InfDS?
014900160318     d InfDspF         ds
015000160318     d   dsp_aid             369    369a
015100160318
015200160318       // -?Indicatori su DspF?
015300160318     d IndDspF         ds                  inz
015400160318         // -?Abilitazione tasti funzionali?
015500160711     d   $F5Attivo                     n   overlay( IndDspF : 05 )
015600160413     d   $F8Attivo                     n   overlay( IndDspF : 08 )
015700160413     d   $F9Attivo                     n   overlay( IndDspF : 09 )
015800160413     d   $F10Attivo                    n   overlay( IndDspF : 10 )
015900160413     d   $F12Attivo                    n   overlay( IndDspF : 12 )
016000160413     d   $RlUpAttivo                   n   overlay( IndDspF : 25 )
016100160413     d   $RlDnAttivo                   n   overlay( IndDspF : 26 )
016200160318         // ·?Emissione messaggio di errore?
016300160413     d   $ErrMessage                   n   overlay( IndDspF : 28 )
016400160412         // ·?Indicatori di gestione del subfile?
016500160412     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
016600160412     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
016700160412     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
016800160413     d   $SflEnd                       n   overlay( IndDspF : 33 )
016900160413     d   $Sf7Dsp_N                     n   overlay( IndDspF : 34 )
017000160413     d   $Sf7DspCtl_N                  n   overlay( IndDspF : 35 )
017100160413     d   $Sf7NxtChg                    n   overlay( IndDspF : 36 )
017200160413     d   $Sf7End                       n   overlay( IndDspF : 37 )
017300160318         // ·?Indicatori per Attibuti di visualizzazione?
017400160413     d   $MinValidi                    n   overlay( IndDspF : 40 )
017500160729     d*//$NoFISAR                      n   overlay( IndDspF : 41 )
017600160729     d*//$NoFISAT                      n   overlay( IndDspF : 42 )
017700160318         // ·?Posizionamento cursore & Segnalazione errore?
017800160413     d   $PosCurOpz                    n   overlay( IndDspF : 50 )
017900160413     d   $PosCurFIL                    n   overlay( IndDspF : 51 )
018000160413     d   $PosCurAPL                    n   overlay( IndDspF : 52 )
018100160413     d   $PosCurDFV                    n   overlay( IndDspF : 53 )
018200160413     d   $PosCurNPG                    n   overlay( IndDspF : 54 )
018300160413     d   $PosCurDLI                    n   overlay( IndDspF : 55 )
018400160413     d   $PosCurOLI                    n   overlay( IndDspF : 56 )
018500161107     d   $PosCurDLF                    n   overlay( IndDspF : 57 )
018600161107     d   $PosCurOLF                    n   overlay( IndDspF : 58 )
018700161107     d   $PosCurSMV                    n   overlay( IndDspF : 59 )
018800160318         // ·?Riemissione videata?
018900160413     d   $ErrGenerico                  n   overlay( IndDspF : 90 )
019000160408
019100160412       // -?Orari a video?
019200160408     d wHhMm_ds        ds             4    qualified  inz
019300160408     d   wHH                          2s 0 inz
019400160408     d   wMM                          2s 0 inz
019500160413
019600160413       // -?Data (aaaammgg) + Ora (hhmmss)?
019700160413     d ds_DtOraR       ds            14    qualified  inz
019800160413     d   Data                         8s 0 inz
019900160413     d   Ora                          6s 0 inz
020000160412
020100160412       // -?Dati estratti via SQL?
020200160413     d FIPSL00F      e ds                  based( NullPtr )
020300160413     d                                     qualified
020400160413     d ds_FIPSL6       ds                  qualified  inz
020500160413     d   PSLusr                            like(FIPSL00F.PSLusr)
020600160413     d                                     inz
020700160412     d   wNrBarCode                   5  0 inz
020800160413     d ds_FIPSL7       ds                  qualified  inz
020900160413     d   PSLnpg                            like(FIPSL00F.PSLnpg)
021000160413     d                                     inz
021100160415     d   PSLnfv                            like(FIPSL00F.PSLnfv)
021200160415     d                                     inz
021300160413     d   PSLbarCo                          like(FIPSL00F.PSLbarCo)
021400160413     d                                     inz
021500160413     d   PSLusr                            like(FIPSL00F.PSLusr)
021600160413     d                                     inz
021700160413     d   PSLdtOraR                         like(FIPSL00F.PSLdtOraR)
021800160413     d                                     inz
021900160413
022000160318       // -?Parametri ricevuti?
022100160318     d KPJBA         e ds
022200160318
022300160711       // -?Tab. DRM = Dati di Default per Report Magazzino?
022400160711     d dDRM          e ds                  qualified  inz
022500160318       // -?Tab. MTP = Macchinone tipo applicazione?
022600160318     d dMTP          e ds                  qualified  inz
022700160318       // -?Tab. KPI = Obiettivi di produttività complessiva?
022800160318     d dKPI          e ds                  qualified  inz
022900161003     d dKPI_K          ds                  likeds(dKPI)
023000161003     d                                                inz
023100160408       // -?Tab. MSL = Macchinone Smistacolli - Layout?
023200160408     d dMSL          e ds                  qualified  inz
023300160321
023400160321       // -?148ª descrizione dell'Organigramma?
023500160321     d Og148         e ds                  qualified  inz
023600160318
023700160318       //--------------------------------------------------------------
023800160318       //?Definizione variabili globali.                               ?
023900160318       //--------------------------------------------------------------
024000160318
024100160318       // -?Flags booleani?
024200160318     d $Fine           s               n   inz
024300160318     d $InzD01         s               n   inz(*on)
024400160318     d $InzD02         s               n   inz(*on)
024500160412     d $InzD03         s               n   inz(*on)
024600160412     d $InzS06         s               n   inz(*on)
024700160412     d $InzS07         s               n   inz(*on)
024800160412     d $EoF            s               n   inz
024900160412     d $EoF7           s               n   inz
025000160408
025100160408       // -?Indici di schiera?
025200160408     d xP              s              3  0 inz
025300160408     d xx              s              3  0 inz
025400160411     d xPT             s              3  0 inz
025500160318
025600160318       // -?Variabili per la gestione del video?
025700160318     d $Video          s              2    inz('D1')
025800160412     d $VideoPrec      s              2    inz
025900160412       // -?Variabili per la gestione dei subfile?
026000160412     d S06nrr          s              5i 0 inz
026100160412     d S07nrr          s              5i 0 inz
026200160412     d w_SflPag        s              3  0 inz
026300160412     d w_SflPag7       s              3  0 inz
026400160412     d wPag            s              5  0 inz
026500160412     d wRig            s              3  0 inz
026600160412       // -?"Pagina _ di _" visualizzabile in D03 e D04?
026700160408     d wPag_C          s              1  0 inz
026800160408     d wPag_T          s              1  0 inz
026900160412       // -?Ultima Postazione Fissa x Incompatibili visualizzata in D04?
027000160408     d wSave_P_I       s              3  0 inz
027100160412
027200160412       // -?Stringhe SQL da eseguire?
027300160413     d wSql            s          32740    inz   varying
027400160413     d wSql7           s          32740    inz   varying
027500160318
027600160318       // -?Campi di calcolo?
027700160318       // ·?Minuti Lavorati?
027800160318     d wMinLav         s              5  0 inz
027900160321     d wMinLavM        s              5  0 inz
028000160321     d wMINxNCL_ob     s              5  0 inz
028100160321     d wMINxNCL_obM    s              5  0 inz
028200160321       // ·?Colli Elaborati?
028300160321     d wNcl            s              9  0 inz
028400160321     d wNclM           s              9  0 inz
028500160722     d wNcl_K          s              9  0 inz
028600160722     d wNclM_K         s              9  0 inz
028700160321       // ·?Data/Ora prima lettura?
028800160318     d W2Ddli          s                   like(MSAdtl)     inz
028900160414     d W2Doli          s                   like(MSAorl)     inz
029000160318     d W2DdliM         s                   like(MSAdtl)     inz
029100160414     d W2DoliM         s                   like(MSAorl)     inz
029200160321       // ·?Data/Ora ultima lettura?
029300160318     d W2Ddlf          s                   like(MSAdtl)     inz
029400160414     d W2Dolf          s                   like(MSAorl)     inz
029500160318     d W2DdlfM         s                   like(MSAdtl)     inz
029600160414     d W2DolfM         s                   like(MSAorl)     inz
029700160321
029800160321       // -?Campi di Comodo?
029900160411     d WWWdfv          s                   like(V1Cdfv)     inz
030000160411     d WWWdli          s                   like(V1Cdli)     inz
030100161107     d WWWdlf          s                   like(V1Cdlf)     inz(*hival)
030200160408     d wDesP           s                   like(V2Dtc01)    inz
030300160411     d wNumP           s              5  0                  inz
030400160321     d §min            s              4  0 inz                                  *minuti totali
030500160321     d §gg             s              2  0 inz                                  *giorni trascorsi
030600160321     d §hh             s              2  0 inz                                  *ore    trascorse
030700160321     d §mm             s              2  0 inz                                  *minuti trascorsi
030800160321     d wResto          s              4  0 inz                                  *minuti oltre l'ora
030900160414     d W2Dkpi          s              6  1 inz
031000160413     d wNFV_9_0        s              9  0 inz
031100160413     d wNFV_x_SQL      s          32740    inz   varying
031200160413     d wDate_EUR       s               d   datfmt(*EUR)
031300160413     d                                     inz(*loval)
031400161107      *
031500161107     d                 ds                  inz
031600161107     d wTempo                          z   inz(z'1901-01-01-01.00.00.000000')
031700161107     d   wData                         d   datfmt(*iso) overlay(wtempo)
031800161107     d   wOra                          t   timfmt(*iso) overlay(wtempo:12)
031900161107     d wTempo1                         z   inz(z'1901-01-01-01.00.00.000000')
032000161107     d wTempo2                         z   inz(z'1901-01-01-01.00.00.000000')
032100160318
032200160318       //--------------------------------------------------------------
032300160318       //?Definizione prototipi procedure usate.                       ?
032400160318       //--------------------------------------------------------------
032500160318
032600160318       // -?Reperimento dati utente?
032700160318     d TIBS34ds      e ds                  inz
032800160318      /copy gaitrasrc/srcProtoPR,TIBS34R
032900160318
033000160318       // -?Reperimento dati tabelle?
033100160318      /copy gaitrasrc/srcProtoPI,TRULTAB
033200160318      /copy gaitrasrc/srcProtoPR,TRULTAB
033300160408
033400160411       // -?Ricerca/Controllo tabelle (TNTBE)?
033500160408     d TIBS02ds      e ds                  inz
033600160408     d   T02mod      e                     inz('R')
033700160408     d   T02cod      e                     inz('MTP')
033800160408      /copy gaitrasrc/srcProtoPR,TIBS02R
033900160711
034000160711       // -?Selezione/Visualizzazione tab. "DRM"?
034100161028     d tntbDRMds     e ds                  inz  qualified
034200160712     d   oDRMerr     e                     inz(*off)
034300160711     d tntbDRMr        pr                  extpgm('TNTBDRMR')
034400160711     d   kpjba                             likeds(KPJBA)
034500160411
034600160411       // -?Caricamento £x?
034700160411     d ds_L1           ds                  inz
034800161028     d   sk_L1                        3s 0 inz  dim(30)
034900160411     d TRUL06ds      e ds                  inz
035000160411      /copy gaitrasrc/srcProtoPR,TRUL06R
035100160318
035200160318       // -?Controllo/Inversione Date?
035300160318     d WLBdat          ds                  inz
035400160321     d   G08dat                       8  0 inz
035500160321     d   G08inv                       8  0 inz
035600160321     d   G08err                       1    inz('3')
035700160321     d   G08tgi                       5  0 inz
035800160318      /copy gaitrasrc/srcProtoPR,XSRDA8
035900160322
036000160322       // -?Interrogazione Letture Smistacolli?
036100160322     d FIMS00ds      e ds                  inz
036200160322     d fims08r         pr                  extpgm('FIMS08R')
036300160322     d   kpjba                             likeds(KPJBA)
036400160322     d   fims00ds                          likeds(FIMS00ds)
036500160413
036600160413       // -?API QCAPCMD (Process Commands)?
036700160413     d Qcmd            s           2048    inz  varying
036800160413      /copy qSysInc/qRpgleSrc,QCAPCMD
036900160413      /copy gaitrasrc/srcProtoPR,QCAPCMD
037000160413       // -?Parametri gestione errori API.?
037100160413      /copy qsysinc/qRpgleSrc,QUSEC
037200160318
037300160318       //--------------------------------------------------------------
037400160318       //?Definizione key-list.                                        ?
037500160318       //--------------------------------------------------------------
037600160318
037700170201       // -?File FNMSA02L?
037800170201     d keyFNMSA02    e ds                  extname( FNMSA02L : *key )
037900160321     d                                     prefix(k_)   inz
038000160323
038100160321       // -?File FISAR06L?
038200160321     d keyFISAR06    e ds                  extname( FISAR06L : *key )
038300160321     d                                     prefix(k_)   inz
038400160323
038500160323       // -?File FISAT02L?
038600160323     d keyFISAT02    e ds                  extname( FISAT02L : *key )
038700160323     d                                     prefix(k_)   inz
038800160413
038900160413       // -?File FNFVV02L?
039000160413     d keyFNFVV02    e ds                  extname( FNFVV02L : *key )
039100160413     d                                     prefix(k_)   inz
039200160318
039300160318       //--------------------------------------------------------------
039400160318       //?Riepilogo indicatori utilizzati.                             ?
039500160318       //--------------------------------------------------------------
039600160318
039700160318
039800160318       //--------------------------------------------------------------
039900160318       //?M A I N - L I N E                                            ?
040000160318       //--------------------------------------------------------------
040100160318
040200160318     c     *Entry        plist
040300160318     c                   parm                    KPJBA
040400160318
040500160318      /free
040600160318
040700160318       // -?Operazioni iniziali?
040800160318       exsr  sr_RoutInz;
040900160318
041000160318       // -?Gestione videata?
041100160318       DoW  Not $Fine;
041200160412
041300160318         select;
041400160412
041500160408           // -?Filtro di lancio?
041600160318           when  $Video = 'D1';
041700160318             exsr  sr_GesD01;
041800160412
041900160408           // -?Dettaglio?
042000160412           when  $Video = 'D3'  or  $Video = 'D4'  or  $Video = 'D5';
042100160413             exsr  sr_GesD030405;
042200160412
042300160413           // -?Spunte da PDA: Totali?
042400160413           when  $Video = 'S6';
042500160413             exsr  sr_GesS06;
042600160413
042700160413           // -?Spunte da PDA: Dettaglio?
042800160413           when  $Video = 'S7';
042900160413             exsr  sr_GesS07;
043000160412
043100160408           // -?? ? ??
043200160318           other;
043300160318             $Fine = *on;
043400160412
043500160318         endsl;
043600160412
043700160318       EndDo;
043800160318
043900160318       // -?Operazioni finali?
044000160318       exsr  sr_RoutEnd;
044100160318
044200160318       //--------------------------------------------------------------
044300160318       //?Operazioni iniziali.                                         ?
044400160318       //--------------------------------------------------------------
044500160318       BEGSR  sr_RoutInz;
044600160412
044700160412         // -?Impostazione opzioni per SQL?
044800160412         exec sql   set  option  DynUsrPrf = *Owner,
044900160412                                 CloSqlCsr = *EndMod;
045000160318
045100160318         // -?Impostazione chiusura?
045200160318         *inLR = *on;
045300160318
045400160318         // -?Reperimento dati job?
045500160318         exsr  sr_DatiJob;
045600160318
045700160318         // -?Impostazione nome programma a video?
045800160318         V1Tpgm = SDSpgm;
045900160318
046000160318       ENDSR;
046100160318
046200160318       //--------------------------------------------------------------
046300160318       //?Reperimento Dati del job (Utente/Operativi).                 ?
046400160318       //--------------------------------------------------------------
046500160318       BEGSR  sr_DatiJob;
046600160318
046700160318         in(e) §AzUte;
046800160318         if NOT %error;
046900160318           in(e) §DatiUte;
047000160318         endif;
047100160318         if %error or RSut = *blank;
047200160318           tibs34r ( tibs34ds );
047300160318           in §AzUte;
047400160318           in §DatiUte;
047500160318         endif;
047600160318
047700160318       ENDSR;
047800160318
047900160318       //--------------------------------------------------------------
048000160318       //?Gestione videata D01.                                        ?
048100160318       //--------------------------------------------------------------
048200160318       BEGSR  sr_GesD01;
048300160318
048400160318         // -?Inizializzazione videata?
048500160318         if  $InzD01 = *on;
048600160318           exsr  sr_InzD01;
048700160318           $InzD01 = *off;
048800160318         endif;
048900160413
049000160413         // -?(Dis)Attivazione tasti funzionali a video?
049100160711         $F5Attivo   = *on;
049200160408         $F8Attivo   = *off;
049300160412         $F9Attivo   = *off;
049400160408         $F12Attivo  = *off;
049500160408         $RlUpAttivo = *off;
049600160408         $RlDnAttivo = *off;
049700160408         $MinValidi  = ( %subst( kNmUs : 1 : 3 ) = 'EDP' );
049800160318
049900160318         // -?Emissione Testata e Piede con tasti funzionali abilitati?
050000160318         write  MS13T01;
050100160318         write  MS13P01;
050200160318
050300160318         // -?Emissione videata?
050400160318         exfmt  MS13D01;
050500160318
050600160321         clear  VIDmsg;
050700160318         reset  $ErrMessage;
050800160318         reset  $ErrGenerico;
050900160318
051000160318         Select;
051100160322
051200160322           // -?Errore: sistema informativo errato?
051300160322           When  %subst( KNSIF : 1 : 3 ) <> 'FIL';
051400160322             $Fine  = *on;
051500160318
051600160318           // -?F3=Fine?
051700160318           When  dsp_aid = c_F03;
051800160318             $Fine  = *on;
051900160711
052000160711           // -?F5=Tipi Applicazione per Filiale (tab. "DRM")?
052100160711           When  dsp_aid = c_F05;
052200160711             exsr  sr_CtrlFIL;
052300160711             if  $ErrGenerico;
052400160711               leavesr;
052500160711             endif;
052600160711             reset  tntbDRMds;
052700160711             tntbDRMds.iDRMopz = '5';
052800160711             tntbDRMds.iDRMfil = V1Cfil;
052900160711             kpjbu     = tntbDRMds;
053000160711             tntbDRMr ( kpjba );
053100160711             tntbDRMds = kpjbu;
053200160711             if  tntbDRMds.oDRMerr = *on;
053300160711               if  tntbDRMds.oDRMmsg = *blank;
053400160711                 VIDmsg = sk_Msg(14);
053500160711               else;
053600160711                 VIDmsg = tntbDRMds.oDRMmsg;
053700160711               endif;
053800160711               $ErrMessage  = *on;
053900160711               $ErrGenerico = *on;
054000160711               leavesr;
054100160711             endif;
054200160318
054300160318           // -?Enter?
054400160318           Other;
054500160318             exsr  sr_CtrD01;
054600160318             if  $ErrGenerico;
054700160318               leavesr;
054800160318             endif;
054900160318             $InzD02 = *on;
055000160408             if  V1Cnpg = 2;
055100160412               $Video  = 'D3';
055200161216             elseIf  V1Cnpg = 5;
055300160412               $Video  = 'D5';
055400161216             else;
055500161216               $VideoPrec   = $Video;
055600161216               $Video  = 'S6';
055700161216               $InzS06 = *on;
055800161216               exsr  sr_InzD02;
055900160408             endif;
056000160318
056100160318         EndSl;
056200160318
056300160318       ENDSR;
056400160318
056500160318       //--------------------------------------------------------------
056600160318       //?Inizializzazione videata D01.                                ?
056700160318       //--------------------------------------------------------------
056800160318       BEGSR  sr_InzD01;
056900160318
057000160318         // -?Spegnimento degli indicatori di errore?
057100160413         %subst( IndDspF : 50 ) = *off;
057200160318
057300160318         // -?Pulizia videata?
057400160318         clear  MS13D01;
057500160711         clear  MS13D02;
057600160318
057700160318         // -?Impostazione iniziale dei campi a video?
057800160711         V1CfilA = %editc( DUTpou : 'X' );
057900160711         //V1Capl  = 'V';
058000160711         V1Cdfv  = *date;
058100160711         V1Cnpg  = 5;
058200160711         V1Csmv  = 'S';
058300160318
058400160318         // -?Decodifiche?
058500160318         exsr  sr_CtrD01;
058600160321         clear  $ErrGenerico;
058700160321         clear  $ErrMessage;
058800160318         clear  VIDmsg;
058900160322
059000160322         // -?Controllo del sistema informativo?
059100160322         if  %subst( KNSIF : 1 : 3 ) <> 'FIL';
059200160322           VIDmsg = sk_Msg(01);
059300160322           $ErrMessage  = *on;
059400160322           $ErrGenerico = *on;
059500160322           leavesr;
059600160322         endif;
059700160322
059800160322         // -?Apertura archivi del s.i. di Filiale?
059900170201         open  FNMSA02L;
060000160322         open  FISAR06L;
060100160322         open  FISAT02L;
060200160413         open  FNFVV02L;
060300160318
060400160318       ENDSR;
060500160318
060600160318       //--------------------------------------------------------------
060700160318       //?Inizializzazione videata D01.                                ?
060800160318       //--------------------------------------------------------------
060900160318       BEGSR  sr_CtrD01;
061000160318
061100160318         // -?Controllo/Decodifica Filiale?
061200160711         exsr  sr_CtrlFIL;
061300160711         if  $ErrGenerico  and
061400160711             Not  $InzD01;
061500160711           leavesr;
061600160711         endif;
061700160318
061800160318         // -?Data Foglio?
061900160318         clear  WLBdat;
062000160318         G08dat = V1Cdfv;
062100160318         XSRDA8 ( WLBdat );
062200160321         if  G08err = '1';
062300160321           VIDmsg = sk_Msg(06);
062400160321           $PosCurDFV   = *on;
062500160321           $ErrMessage  = *on;
062600160321           $ErrGenerico = *on;
062700160321           if  Not  $InzD01;
062800160318             leavesr;
062900160318           endif;
063000160318         endif;
063100160318         V1Cdfv = G08dat;
063200160411         WWWdfv = G08inv;
063300160318
063400160318         // -?Categoria Foglio?
063500161216         if  V1Cnpg <> 2  and  V1Cnpg <> 5  and
063600161216             V1Cnpg <> 3  and  V1Cnpg <> 4;
063700160321           VIDmsg = sk_Msg(07);
063800160321           $PosCurNPG   = *on;
063900160321           $ErrMessage  = *on;
064000160321           $ErrGenerico = *on;
064100160321           if  Not  $InzD01;
064200160318             leavesr;
064300160318           endif;
064400160318         endif;
064500160408
064600160408         // -?Data Lettura iniziale?
064700161107         reset  WWWdli;
064800160408         clear  WLBdat;
064900160408         if  V1Cdli <> *zero;
065000160408           G08dat = V1Cdli;
065100160408           XSRDA8 ( WLBdat );
065200160408           if  G08err = '1';
065300160408             VIDmsg = sk_Msg(08);
065400160408             $PosCurDLI   = *on;
065500160408             $ErrMessage  = *on;
065600160408             $ErrGenerico = *on;
065700160408             if  Not  $InzD01;
065800160408               leavesr;
065900160408             endif;
066000160408           endif;
066100161107           WWWdli = G08inv;
066200160408         endif;
066300160408         V1Cdli = G08dat;
066400160408
066500160408         // -?Ora Lettura iniziale?
066600160408         wHhMm_ds = %editc( V1Coli : 'X' );
066700160408         if  wHhMm_ds.wHH < 00  or  wHhMm_ds.wHH > 23  or
066800160408             wHhMm_ds.wMM < 00  or  wHhMm_ds.wMM > 59;
066900160408           VIDmsg = sk_Msg(09);
067000160408           $PosCurOLI   = *on;
067100160408           $ErrMessage  = *on;
067200160408           $ErrGenerico = *on;
067300160408           if  Not  $InzD01;
067400160408             leavesr;
067500160408           endif;
067600160408         endif;
067700161107
067800161107         // -?Data Lettura finale?
067900161107         reset  WWWdlf;
068000161107         clear  WLBdat;
068100161107         if  V1Cdlf <> *zero;
068200161107           G08dat = V1Cdlf;
068300161107           XSRDA8 ( WLBdat );
068400161107           if  G08err = '1';
068500161107             VIDmsg = sk_Msg(08);
068600161107             $PosCurDLF   = *on;
068700161107             $ErrMessage  = *on;
068800161107             $ErrGenerico = *on;
068900161107             if  Not  $InzD01;
069000161107               leavesr;
069100161107             endif;
069200161107           endif;
069300161107           WWWdlf = G08inv;
069400161107         endif;
069500161107         V1Cdlf = G08dat;
069600161107
069700161107         // -?Ora Lettura finale?
069800161107         wHhMm_ds = %editc( V1Colf : 'X' );
069900161107         if  wHhMm_ds.wHH < 00  or  wHhMm_ds.wHH > 23  or
070000161107             wHhMm_ds.wMM < 00  or  wHhMm_ds.wMM > 59;
070100161107           VIDmsg = sk_Msg(09);
070200161107           $PosCurOLF   = *on;
070300161107           $ErrMessage  = *on;
070400161107           $ErrGenerico = *on;
070500161107           if  Not  $InzD01;
070600161107             leavesr;
070700161107           endif;
070800161107         endif;
070900161107
071000161107         // -?Range Data-Ora Letture?
071100161107         if  ( V1Cdli + V1Coli + V1Cdlf + V1Colf ) <> *zero;
071200161107
071300161107           if  WWWdli > *zero;
071400161107             wData = %date( WWWdli : *iso );
071500161107           else;
071600161107             reset  wData;
071700161107           endif;
071800161107           wOra    = %time( V1Coli * 100 : *iso );
071900161107           wTempo1 = wTempo;
072000161107
072100161107           if  WWWdlf > *zero;
072200161107             wData = %date( WWWdlf : *iso );
072300161107           else;
072400161107             reset  wData;
072500161107           endif;
072600161107           wOra    = %time( V1Colf * 100 : *iso );
072700161107           wTempo2 = wTempo;
072800161107
072900161107           if  wTempo1 > wTempo2;
073000161107             VIDmsg = sk_Msg(15);
073100161107             $PosCurDLI   = *on;
073200161107             $ErrMessage  = *on;
073300161107             $ErrGenerico = *on;
073400161107             if  Not  $InzD01;
073500161107               leavesr;
073600161107             endif;
073700161107           endif;
073800161107
073900161107         endif;
074000160324
074100160324         // -?Selezione dei Soli Minuti con Colli Validi?
074200160324         if  V1Csmv <> 'S'  and  V1Csmv <> 'N';
074300160408           VIDmsg = sk_Msg(10);
074400160324           $PosCurSMV   = *on;
074500160324           $ErrMessage  = *on;
074600160324           $ErrGenerico = *on;
074700160324           if  Not  $InzD01;
074800160324             leavesr;
074900160324           endif;
075000160324         endif;
075100160318
075200160318       ENDSR;
075300160711
075400160711       //--------------------------------------------------------------
075500160711       //?Controllo della Filiale.                                     ?
075600160711       //--------------------------------------------------------------
075700160711       BEGSR  sr_CtrlFIL;
075800160711
075900160711         clear  V1Dfil;
076000160711
076100160711         if  %scan( '?' : V1CfilA ) > *zero;
076200160711           V1CfilA = *zero;
076300160711           reset  tntbDRMds;
076400160711           tntbDRMds.iDRMopz = '1';
076500160711           kpjbu     = tntbDRMds;
076600160711           tntbDRMr ( KPJBA );
076700160711           tntbDRMds = kpjbu;
076800160711           if  tntbDRMds.oDRMfil > *zero;
076900160711             V1CfilA = %editc( tntbDRMds.oDRMfil : 'X' );
077000160711           endif;
077100160711           $ErrGenerico = *on;
077200160711         endif;
077300160711
077400160711         if  %check( c_Digits : V1CfilA ) > *zero;
077500160711           VIDmsg = sk_Msg(03);
077600160711           $PosCurFIL   = *on;
077700160711           $ErrMessage  = *on;
077800160711           $ErrGenerico = *on;
077900160711           if  Not  $InzD01;
078000160711             leavesr;
078100160711           endif;
078200160711         endif;
078300160711
078400160711         V1Cfil = %int( V1CfilA );
078500160711
078600160711         chain  ( V1Cfil )  AZORG;
078700160711         if  %found(AZORG01L);
078800160711           V1Dfil = ORGdes;
078900160711         else;
079000160711           VIDmsg = sk_Msg(03);
079100160711           $PosCurFIL   = *on;
079200160711           $ErrMessage  = *on;
079300160711           $ErrGenerico = *on;
079400160711           if  Not  $InzD01;
079500160711             leavesr;
079600160711           endif;
079700160711         endif;
079800160711
079900160711         Og148 = ORGde8;
080000160711         if  Og148.§OGlpo <> '1';
080100160711           VIDmsg = sk_Msg(04);
080200160711           $PosCurFIL   = *on;
080300160711           $ErrMessage  = *on;
080400160711           $ErrGenerico = *on;
080500160711           if  Not  $InzD01;
080600160711             leavesr;
080700160711           endif;
080800160711         endif;
080900161028
081000180226         if  %subst( kNmUs : 1 : 3 ) <> 'EDP'  and
081100180226             %subst( kNmUs : 1 : 3 ) <> 'ISP'  and
081200180226             V1Cfil <> SIMfel;
081300161028           VIDmsg = %trimR( sk_Msg(03) ) +
081400161028                    ': non in gestione all''utente';
081500161028           $PosCurFIL   = *on;
081600161028           $ErrMessage  = *on;
081700161028           $ErrGenerico = *on;
081800161028           if  Not  $InzD01;
081900161028             leavesr;
082000161028           endif;
082100161028         endif;
082200160711
082300160711         // -?Ricerca/Controllo/Decodifica Tipo Applicazione?
082400160711         clear  V1Dapl;
082500160711         //*// -?Ricerca?
082600160711         //*if  %scan( '?' : V1Capl ) > *zero;
082700160711         //*  reset  TIBS02ds;
082800160711         //*  //T02mod = 'R';    ?(già così)?
082900160711         //*  //T02cod = 'MTP';  ?(già così)?
083000160711         //*  TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
083100160711         //*  if  T02err = *blank;
083200160711         //*    V1Capl = T02ke1;
083300160711         //*  endif;
083400160711         //*endif;
083500160711         // -?Reperimento Tipo Applicazione per fil. da tab. "DRM"?
083600160711         clear  dDRM;
083700160711         if  getTabella ( c_Tntbe : 'DRM' : %editc( V1Cfil : 'X' ) :
083800160711                          *blank :
083900160711                          *omit : *omit :
084000160711                          *omit : *omit :
084100160711                          *omit : *omit : *omit : *omit :
084200160711                          *omit : *omit : *omit : *omit :
084300160711                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
084400160711                          = *zero      AND
084500160711             ds_TNTBE.TBEatb = *blank;
084600160711           dDRM   = ds_TNTBE.TBEuni;
084700161216           if  V1Cnpg = 5;
084800161216             V1Capl = dDRM.§DRMaFv5;
084900161017           else;
085000161216             V1Capl = dDRM.§DRMaFv2;
085100161017           endif;
085200160711         else;
085300160711           VIDmsg = sk_Msg(14);
085400160711           $PosCurFIL   = *on;
085500160711           $ErrMessage  = *on;
085600160711           $ErrGenerico = *on;
085700160711           if  Not  $InzD01;
085800160711             leavesr;
085900160711           endif;
086000160711         endif;
086100160711         // -?Controllo/Decodifica?
086200160711         clear  dMTP;
086300160711         if  getTabella ( c_Tntbe : 'MTP' : V1Capl : *blank :
086400160711                          *omit : *omit :
086500160711                          *omit : *omit :
086600160711                          *omit : *omit : *omit : *omit :
086700160711                          *omit : *omit : *omit : *omit :
086800160711                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
086900160711                          = *zero      AND
087000160711             ds_TNTBE.TBEatb = *blank;
087100160711           dMTP   = ds_TNTBE.TBEuni;
087200160711           V1Dapl = dMTP.§MTPdes;
087300160711         else;
087400160711           //*VIDmsg = sk_Msg(05);
087500160711           //*$PosCurAPL   = *on;
087600160711           //*$ErrMessage  = *on;
087700160711           //*$ErrGenerico = *on;
087800160711           //*if  Not  $InzD01;
087900160711           //*  leavesr;
088000160711           //*endif;
088100160711           V1Dapl = *all'? ';
088200160711         endif;
088300160711
088400160711       ENDSR;
088500160318
088600160318       //--------------------------------------------------------------
088700160413       //?Gestione videate D02/D03/D04/D05.                            ?
088800160318       //--------------------------------------------------------------
088900160413       BEGSR  sr_GesD030405;
089000160318
089100160318         // -?Inizializzazione videata?
089200160318         if  $InzD02 = *on;
089300161216           exsr  sr_InzD02;
089400160413           exsr  sr_InzD030405;
089500160318           $InzD02 = *off;
089600160318         endif;
089700160408
089800160413         // -?(Dis)Attivazione tasti funzionali a video?
089900160711         $F5Attivo   = *off;
090000161028         $F8Attivo   = ( %subst( kNmUs : 1 : 3 ) = 'EDP' );
090100160930         //*·$F9Attivo   = ( V1Cnpg = 2 );
090200160930         $F9Attivo   = *on;
090300160408         $F12Attivo  = *on;
090400160408         $RlUpAttivo = *off;
090500160408         $RlDnAttivo = *off;
090600160729         if  ( $Video  = 'D3'  or  $Video = 'D5' )  and
090700160729             ( V2DtcpI + V2DtcpIM ) > *zero;
090800160408           $RlUpAttivo = *on;
090900160408         endif;
091000160412         if  $Video = 'D4';
091100160408           if  wPag_C < wPag_T;
091200160408             $RlUpAttivo = *on;
091300160408           endif;
091400160408           $RlDnAttivo = *on;
091500160408         endif;
091600160318
091700160318         // -?Emissione Testata e Piede con tasti funzionali abilitati?
091800160318         write  MS13T01;
091900160318         write  MS13P01;
092000160318
092100160318         // -?Emissione videata?
092200160408         write  MS13D02;
092300160412
092400160408         select;
092500160412
092600160408           // -?Arrivi - 1ª videata?
092700160412           when  V1Cnpg = 2  and  $Video = 'D3';
092800160412             exfmt  MS13D03;
092900160412
093000160729           // -?Arrivi/Partenze - 2ª videata?
093100160729           //*when  V1Cnpg = 2  and  $Video = 'D4';
093200160729           when  $Video = 'D4';
093300160412             exfmt  MS13D04;
093400160412
093500160729           // -?Partenze - 1ª videata?
093600160729           when  V1Cnpg = 5  and  $Video = 'D5';
093700160412             exfmt  MS13D05;
093800160412
093900160408           // -?? ? ??
094000160408           other;
094100160408             $Fine = *on;
094200160408             leavesr;
094300160412
094400160408         endsl;
094500160318
094600160321         clear  VIDmsg;
094700160318         reset  $ErrMessage;
094800160318         reset  $ErrGenerico;
094900160318
095000160318         Select;
095100160318
095200160318           // -?F3=Fine?
095300160318           When  dsp_aid = c_F03;
095400160318             $Fine  = *on;
095500160318
095600160318           // -?F12=Ritorno?
095700160318           When  dsp_aid = c_F12;
095800160318             $Video = 'D1';
095900160322
096000160322           // -?F8=Dettaglio Letture VdL/CML/DiskC?
096100160322           When  dsp_aid = c_F08;
096200160322             exsr  sr_CallFIMS08;
096300160412
096400160729           // -?F9=Spunte Manuali?
096500160412           When  dsp_aid = c_F09;
096600160412             $VideoPrec  = $Video;
096700160412             $Video      = 'S6';
096800160412             $InzS06     = *on;
096900160408
097000160408           // -?Roll-Up?
097100160729           When  dsp_aid  = c_RollUp  and  wPag_C < wPag_T;
097200160412             $Video  = 'D4';
097300160408             wPag_C += 1;
097400161020             //*·exsr  sr_Set_NrPag;
097500160412             exsr  sr_InzD04;
097600160408
097700160408           // -?Roll-Down?
097800160729           When  $Video  = 'D4'       and
097900160408                 dsp_aid = c_RollDown;
098000160408             select;
098100160729               when  wPag_C = 2;
098200160729                 if  V1Cnpg = 2;
098300160729                   $Video = 'D3';
098400160729                 else;
098500160729                   $Video = 'D5';
098600160729                 endif;
098700160411                 wPag_C -= 1;
098800160411                 exsr  sr_Set_NrPag;
098900160408               when  wPag_C > 2;
099000160412                 $Video  = 'D4';
099100160408                 wPag_C -= 1;
099200161020                 //*·exsr  sr_Set_NrPag;
099300160412                 exsr  sr_InzD04;
099400160408             endsl;
099500160318
099600160318         EndSl;
099700160318
099800160318       ENDSR;
099900161216
100000161216       //--------------------------------------------------------------
100100161216       //?Inizializzazione videate D02.                                ?
100200161216       //--------------------------------------------------------------
100300161216       BEGSR  sr_InzD02;
100400161216
100500161216         // -?Spegnimento degli indicatori di errore?
100600161216         %subst( IndDspF : 50 ) = *off;
100700161216
100800161216         // -?Pulizia videate?
100900161216         //clear  MS13D02;    ?- NO: contiene campi di D01!?
101000161216
101100161216         // -?Decodifica Categoria Foglio (in D01)?
101200161216         select;
101300161216           when  V1Cnpg = 2;
101400161216             V2Dnpg = 'Arrivi';
101500161216           when  V1Cnpg = 3;
101600170110             //*·V2Dnpg = 'Inventario';
101700170110             V2Dnpg = 'IMA';
101800161216           when  V1Cnpg = 4;
101900161216             V2Dnpg = 'Consegne';
102000161216           when  V1Cnpg = 5;
102100161216             V2Dnpg = 'Entrata';
102200161216         endsl;
102300161216
102400161216         // -?Richiesti "Solo minuti con colli validi"?
102500161216         //  ?(e range di date di lettura)?
102600161216         clear  V2Dsmv;
102700161216         select;
102800161216           when  V1Csmv = 'S'  and  ( V1Cdli + V1Cdlf )  = *zero;
102900161216             evalR V2text = c_Color_Red + '(Solo minuti con colli validi)';
103000161216           when  ( V1Cdli + V1Cdlf ) <> *zero;
103100161216             V2text = 'Letture     dalle ore' + c_DspAtr_HI
103200161216                                              + %editw( V1Coli : '0 :  ' )
103300161216                                              + c_DspAtrNorm +
103400161216                                       ' del' + c_DspAtr_HI
103500161216                                              + %editc( V1Cdli : 'Y' )
103600161216                                              + c_DspAtrNorm +
103700161216                               '    alle ore' + c_DspATR_HI
103800161216                                              + %editw( V1Colf : '0 :  ' )
103900161216                                              + c_DspAtrNorm +
104000161216                                       ' del' + c_DspAtr_HI
104100161216                                              + %editc( V1Cdlf : 'Y' )
104200161216                                              + c_DspAtrNorm;
104300161216         endsl;
104400161219
104500161219         // -?Caricamento F.V. da elaborare?
104600161219         //  ?- NON più solo per categoria 2=Arrivi -?
104700161219         exsr  sr_Intab_FV_Elab;
104800161216
104900161216       ENDSR;
105000160318
105100160318       //--------------------------------------------------------------
105200161216       //?Inizializzazione videate D03/D04/D05.                        ?
105300160318       //--------------------------------------------------------------
105400160413       BEGSR  sr_InzD030405;
105500160318
105600160318         // -?Spegnimento degli indicatori di errore?
105700160413         %subst( IndDspF : 50 ) = *off;
105800160318
105900160408         // -?Pulizia videate?
106000160412         clear  MS13D03;
106100160412         clear  MS13D04;
106200160412         clear  MS13D05;
106300160321
106400160321         clear  wMinLav;
106500160321         clear  wMinLavM;
106600160321         clear  wMINxNCL_ob;
106700160321         clear  wMINxNCL_obM;
106800160321         clear  wNcl;
106900160321         clear  wNclM;
107000160722         clear  wNcl_K;
107100160722         clear  wNclM_K;
107200160321         clear  W2Ddli;
107300160321         clear  W2DdliM;
107400160321         clear  W2Ddlf;
107500160321         clear  W2DdlfM;
107600160408         clear  wPag_C;
107700160408         clear  wPag_T;
107800160408         clear  sk_Ncl_Incomp;
107900160411
108000160411         // -?Reperimento Tab. "MSL" (VDL - Layout Postazioni)?
108100160411         exsr  sr_Tab_MSL;
108200160318
108300160318         // -?Reperimento Obiettivi di Produttività?
108400160318         //  ?di un Impianto Automatico Smistacolli?
108500160318         clear  dKPI;
108600160321         if  getTabella ( c_Tntbe : 'KPI' : %editc( V1Cnpg : 'X' ) : V1Capl :
108700160318                          *omit : *omit :
108800160318                          *omit : *omit :
108900160318                          *omit : *omit : *omit : *omit :
109000160318                          *omit : *omit : *omit : *omit :
109100160318                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
109200160318                          = *zero      AND
109300160318               ds_TNTBE.TBEatb = *blank;
109400160318           dKPI   = ds_TNTBE.TBEuni;
109500160318         endif;
109600160318
109700160322         // -?Lettura Attività Smistamento del foglio da caricare?
109800160408         // ·?Tipo Applicazione "V" (o quella a video)?
109900160322         exsr  sr_ElabFNMSA;
110000160722         // ·?Tipo Applicazione "P" (RPVP)?
110100160408         exsr  sr_ElabFNMSA_P;
110200160411         // ·?Tipo Applicazione "I" (Fissi/Incompatibili)?
110300160408         exsr  sr_ElabFNMSA_I;
110400160729         if  V1Cnpg = 5  and  V2DtcpIm > *zero;
110500160729           V2DtcpI += V2DtcpIm;
110600160729         endif;
110700160321
110800160411         // -?Lettura Statistica Arrivi?
110900160411         //  ?della data del foglio da caricare?
111000160411         if  V1Cnpg = 2;
111100160411           exsr  sr_ElabFISAR;
111200160411         endif;
111300160322
111400160411         // -?Lettura Statistica Partenze A Terra?
111500160411         //  ?della data del foglio da caricare?
111600160411         if  V1Cnpg = 5;
111700160411           exsr  sr_ElabFISAT;
111800160411         endif;
111900160411
112000160411         // -?Impostazione del numero pagina da visualizzare?
112100160411         exsr  sr_Set_NrPag;
112200160318
112300160318       ENDSR;
112400160412
112500160412       //--------------------------------------------------------------
112600160412       //?Preparazione videata D04.                                    ?
112700160412       //--------------------------------------------------------------
112800160412       BEGSR  sr_InzD04;
112900161020
113000161020         clear  MS13D04;
113100161020
113200161020         // -?Impostazione del numero pagina da visualizzare?
113300161020         exsr  sr_Set_NrPag;
113400160412
113500160412         // -?Calcolo della 1ª Postazione da visualizzare?
113600160412         select;
113700160412           when  wPag_C < 2;
113800160412             leavesr;
113900160412           when  wPag_C = 2;
114000160412             clear  xP;
114100160412           when  wPag_C > 2;
114200160412             xP = 10;
114300160412         endsl;
114400160412
114500160412         // -?Ciclo di caricamento dei dati da visualizzare?
114600160412         //  ?(per max 10 postazioni)?
114700160412         For  xx = 1  To 10;
114800160412
114900160412           xP += 1;
115000160412           exsr  sr_RepPostFissa;
115100160412
115200160412           select;
115300160412             when  wDesP = *blank  and  wNumP = *zero;
115400160412             when  xx  = 1;
115500160412               V2Dtc01 = wDesP;
115600160414               V2Ntc01 = wNumP;
115700160412             when  xx  = 2;
115800160412               V2Dtc02 = wDesP;
115900160414               V2Ntc02 = wNumP;
116000160412             when  xx  = 3;
116100160412               V2Dtc03 = wDesP;
116200160414               V2Ntc03 = wNumP;
116300160412             when  xx  = 4;
116400160412               V2Dtc04 = wDesP;
116500160414               V2Ntc04 = wNumP;
116600160412             when  xx  = 5;
116700160412               V2Dtc05 = wDesP;
116800160414               V2Ntc05 = wNumP;
116900160412             when  xx  = 6;
117000160412               V2Dtc06 = wDesP;
117100160414               V2Ntc06 = wNumP;
117200160412             when  xx  = 7;
117300160412               V2Dtc07 = wDesP;
117400160414               V2Ntc07 = wNumP;
117500160412             when  xx  = 8;
117600160412               V2Dtc08 = wDesP;
117700160414               V2Ntc08 = wNumP;
117800160412             when  xx  = 9;
117900160412               V2Dtc09 = wDesP;
118000160414               V2Ntc09 = wNumP;
118100160412             when  xx  = 10;
118200160412               V2Dtc10 = wDesP;
118300160414               V2Ntc10 = wNumP;
118400160412           endsl;
118500160412
118600160412         EndFor;
118700160412
118800160412       ENDSR;
118900161020
119000161020       //--------------------------------------------------------------
119100161020       //?Impostazione del n° di pagina da visualizzare in D03/D04/D05.?
119200161020       //--------------------------------------------------------------
119300161020       BEGSR  sr_Set_NrPag;
119400161020
119500161020         clear  V2Dpag;
119600161020         clear  V2Dseg;
119700161020
119800161020         if  wPag_T > 1;
119900161020           V2Dpag = 'Pag. ' + %editc( wPag_C : 'X' ) +
120000161020                    ' di '  + %editc( wPag_T : 'X' );
120100161020           if  wPag_C < wPag_T;
120200161020             V2Dseg = 'Segue...';
120300161020           else;
120400161020             V2Dseg = '   Fine.';
120500161020           endif;
120600161020         endif;
120700161020
120800161020       ENDSR;
120900160412
121000160412       //--------------------------------------------------------------
121100160413       //?Gestione subfile S06.                                        ?
121200160412       //--------------------------------------------------------------
121300160413       BEGSR  sr_GesS06;
121400160412
121500160413         // -?Inizializzazione subfile S06?
121600160413         if  $InzS06 = *on;
121700160413           exsr  sr_InzS06;
121800160413           $InzS06 = *off;
121900160413         endif;
122000160412
122100160413         // -?(Dis)Attivazione tasti funzionali a video?
122200160412         $F8Attivo   = *off;
122300160412         $F9Attivo   = *off;
122400160526         //*$F10Attivo  = ( $Video = 'S6'  and  kNmUs = 'EDPSM     ' );
122500160526         $F10Attivo  = ( $Video = 'S6'  and
122600170522                         %subst( SDSjob : 1 : 9 ) = 'P04685668' );
122700160412         $F12Attivo  = *on;
122800160412         $RlUpAttivo = *off;
122900160412         $RlDnAttivo = *off;
123000160412
123100160412         // -?Posizionamento cursore?
123200160413         //  ?C6CsrRrn contiene il numero di riga del SubFile su cui?
123300160413         //  ?era posizionato il cursore; impostandolo in C6RcdNbr?
123400160412         //  ?si visualizza la pagina che era visualizzata quando?
123500160412         //  ?l'utente ha premuto l'ultimo tasto.?
123600160413         if  C6CsrRrn > *zero;
123700160413           C6RcdNbr = C6CsrRrn;
123800160413         else;
123900160413           C6RcdNbr = 1;
124000160413           write  MS13D06;          //(no rec.)?
124100160413         endif;
124200160413
124300160413         // -?Emissione Testata e Piede con tasti funzionali abilitati?
124400160413         write  MS13T01;
124500160413         write  MS13D02;
124600160413         write  MS13Z06;
124700160413         write  MS13P02;
124800160412
124900160412         // -?Emissione videata?
125000160413         exfmt  MS13C06;
125100160412
125200160413         clear  VIDmsg;
125300160412         reset  $ErrMessage;
125400160412         reset  $ErrGenerico;
125500160412
125600160412         Select;
125700160412
125800160412           // -?F3=Fine?
125900160412           When  dsp_aid = c_F03;
126000160413             if  $Video  = 'S7';
126100160413               exsr  sr_CloseCursor7;
126200160413             endif;
126300160412             exsr  sr_CloseCursor;
126400160412             $Fine = *on;
126500160413
126600160413           // -?F10=Dettaglio BarCode (solo da C06)?
126700160413           When  dsp_aid = c_F10;
126800160413             $Video  = 'S7';
126900160413             $InzS07 = *on;
127000160412
127100160412           // -?F12=Ritorno?
127200160412           When  dsp_aid = c_F12;
127300160413             exsr  sr_CloseCursor;
127400160413             $Video = $VideoPrec;
127500160412
127600160412           // -?Roll-Up?
127700160413           When  dsp_aid = c_RollUp;
127800160412             // -?Verifica raggiungimento del limite di records?
127900160412             //  ?registrabili nel subfile (9999)?
128000160413             if  S06nrr = *hival;
128100160413               $ErrGenerico = *on;
128200160413               $ErrMessage  = *on;
128300160413               VIDmsg = sk_Msg(11);
128400160413               leavesr;
128500160413             endif;
128600160413             exsr sr_RollUpS06;
128700160412
128800160412           // -?SubFile vuoto?
128900160413           When  S06nrr = *zero;
129000160412
129100160413           // -?Invio (solo da C06)?
129200160413           Other;
129300160412             // -?Gestione opzioni?
129400160412             exsr  sr_OpzS06;
129500160412             if  $ErrGenerico;
129600160412               leavesr;
129700160412             endif;
129800160412
129900160412         EndSl;
130000160412
130100160412       ENDSR;
130200160412
130300160412       //--------------------------------------------------------------
130400160413       //?Inizializzazione subfile S06.                                ?
130500160412       //--------------------------------------------------------------
130600160412       BEGSR  sr_InzS06;
130700160413
130800161219         //*·// -?Caricamento F.V. da elaborare?
130900161219         //*·//  ?- NON più solo per categoria 2=Arrivi -?
131000161219         //*   ?(caricamento spostato nella subr. "sr_InzD02")?
131100161219         //*·exsr  sr_Intab_FV_Elab;
131200160412
131300160412         // -?Pulizia subfile?
131400160412         $SflDsp_N    = *on;
131500160412         $SflDspCtl_N = *on;
131600160412         write  MS13C06;
131700160412         $SflDspCtl_N = *off;
131800160412         $SflEnd      = *off;
131900160412
132000160412         $SflNxtChg   = *off;
132100160412
132200160412         clear  w_SflPag;
132300160412         clear  C6RcdNbr;
132400160412         clear  C6CsrRrn;
132500160412         clear  S06nrr;
132600160413         clear  VIDmsg;
132700160412         $ErrMessage  = *off;
132800160412         $ErrGenerico = *off;
132900160412
133000160412         // -?Spegnimento degli indicatori di errore?
133100160413         %subst( IndDspF : 50 ) = *off;
133200160412
133300160412         // -?Caricamento della 1ª pagina di dati del subfile?
133400160412         // -?Preparazione stringa SQL?
133500160412         exsr  sr_PrepSQL;
133600160412         // -?Apertura del cursore?
133700160412         exsr  sr_OpenCursor;
133800160412         // -?Lettura del file?
133900160412         exsr  sr_ReadCursor;
134000160412         DoW  Not $EoF  and  S06nrr < c_SflPag;
134100160412           exsr  sr_RollUpS06;
134200160412         EndDo;
134300160412
134400160412         // -?Visualizzazione del SFL (se ci sono dati)?
134500160412         $SflDsp_N = ( S06nrr <= *zero );
134600160412
134700160412         // -?Attivazione del SFLEND?
134800160412         $SflEnd   = ( $EoF = *on );
134900160412
135000160412         // -?Impaginazione subfile?
135100160412         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
135200160412         if  S06nrr > *zero;
135300160412           C6RcdNbr = 1;
135400160412           C6CsrRrn = 1;
135500160412         else;
135600160412           clear  C6RcdNbr;
135700160412         endif;
135800160412
135900160412       ENDSR;
136000160412
136100160412       //--------------------------------------------------------------
136200160412       //?Caricamento singola pagine SubFile S06.                      ?
136300160412       //--------------------------------------------------------------
136400160412       BEGSR  sr_RollUpS06;
136500160412
136600160412         S06nrr     = ( w_SflPag * c_SflPag );
136700160412         w_SflPag  += 1;
136800160412         $SflNxtChg = *off;
136900160412
137000160412         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
137100160412         DoW  $EoF   = *off    and
137200160412              S06nrr < *hival  and
137300160412              S06nrr < ( w_SflPag * c_SflPag );
137400160412
137500160412           // -?Caricamento dati nel record del subfile?
137600160412           exsr  sr_RieRecS06;
137700160412           S06nrr += 1;
137800160412           write  MS13S06;
137900160412
138000160412           // -?Lettura record successivo nell'archivio?
138100160412           exsr  sr_ReadCursor;
138200160412
138300160412         EndDo;
138400160412
138500160412         // -?Visualizzazione del SFL (se ci sono dati)?
138600160412         $SflDsp_N = ( S06nrr <= *zero );
138700160412
138800160412         // -?Attivazione (eventuale) del SFLEND?
138900160412         $SflEnd   = ( $EoF = *on );
139000160412
139100160412         // -?Posizionamento cursore al 1° rcd della pagina?
139200160412         if  S06nrr > *zero;
139300160412           wPag     = S06nrr / c_SflPag;
139400160412           wRig     = S06nrr - ( c_SflPag * wPag );
139500160412           C6RcdNbr = wPag * c_SflPag;
139600160412           if  wRig > *zero;
139700160412             C6RcdNbr = C6RcdNbr + 1;
139800160412           else;
139900160412             C6RcdNbr = C6RcdNbr - c_SflPag + 1;
140000160412           endif;
140100160412         else;
140200160412           clear  C6RcdNbr;
140300160412         endif;
140400160412
140500160412         C6CsrRrn = C6RcdNbr;
140600160412
140700160412       ENDSR;
140800160412
140900160412       //--------------------------------------------------------------
141000160412       //?Caricamento singolo record nel SubFile S06.                  ?
141100160412       //--------------------------------------------------------------
141200160412       BEGSR  sr_RieRecS06;
141300160412
141400160412         clear  MS13S06;
141500160412
141600160412         // -?Campi Hidden?
141700160412         // ·?Utente?
141800160413         H06usr = ds_FIPSL6.PSLusr;
141900160412
142000160412         // -?Campi Output?
142100160412         // ·?Utente?
142200160413         S06usr = %subst( ds_FIPSL6.PSLusr : 6 : %len(S06usr) );
142300160412         // ·?Numero BarCode letti?
142400160413         S06ncl = ds_FIPSL6.wNrBarCode;
142500160526
142600160526         // -?Attributi di Visualizzazione?
142700160526         // ·?Gestione Opzione + F10?
142800160526         $F10Attivo  = ( $Video = 'S6'  and
142900170522                         %subst( SDSjob : 1 : 9 ) = 'P04685668' );
143000160412
143100160412       ENDSR;
143200160412
143300160412       //--------------------------------------------------------------
143400160412       //?Gestione opzioni nel SubFile S06.                            ?
143500160412       //--------------------------------------------------------------
143600160412       BEGSR  sr_OpzS06;
143700160412
143800160412         // -?Ciclo di lettura subfile?
143900160412         readc  MS13S06;
144000160412
144100160412         DOW  NOT  %eof(FIMS13D);
144200160412
144300160413           %subst( IndDspF : 50 ) = *off;
144400160412           $SflNxtChg  = *off;
144500160412           C6RcdNbr    = S06nrr;
144600160412
144700160412           Select;
144800160412
144900160412             // -?Nessuna opzione?
145000160412             When  S06opz = *blank;
145100160412
145200160412             // -?5 = Visualizzazione BarCode per Profilo Spunta?
145300160413             When  S06opz = '5';
145400160412               $Video  = 'S7';
145500160412               $InzS07 = *on;
145600160412               DoW  $Video = 'S7';
145700160412                 exsr  sr_GesS07;
145800160412               EndDo;
145900160412               $Video = 'S6';
146000160412
146100160412             // -?? = Opzione NON valida?
146200160412             Other;
146300160412               $ErrGenerico = *on;
146400160412               $ErrMessage  = *on;
146500160412               $PosCurOpz   = *on;
146600160412               VIDmsg = sk_Msg(12);
146700160412
146800160412           EndSl;
146900160412
147000160412           // -?Aggiornamento subfile?
147100160412           Select;
147200160412             When  $ErrMessage;
147300160412               $SflNxtChg = *on;
147400160412               C6CsrRrn   = C6RcdNbr;
147500160412             When  $ErrGenerico;
147600160412               $SflNxtChg = *on;
147700160412               C6CsrRrn   = C6RcdNbr;
147800160412               clear  S06opz;
147900160412             Other;
148000160412               C6CsrRrn   = C6RcdNbr;
148100160412               clear  S06opz;
148200160412           EndSl;
148300160526           $F10Attivo  = ( $Video = 'S6'  and
148400170522                           %subst( SDSjob : 1 : 9 ) = 'P04685668' );
148500160412
148600160412           update  MS13S06;
148700160412
148800160412           // -?Uscita dal ciclo di lettura?
148900160412           if  $ErrMessage  or  $ErrGenerico;
149000160412             clear  $InzS06;
149100160412             leave;
149200160412           endif;
149300160412
149400160412           // -?Lettura rec. variato sucessivo?
149500160412           readc  MS13S06;
149600160412
149700160412         EndDo;
149800160412
149900160412         // -?Se richiesto il RI-caricamento del subfile:?
150000160412         //  ?occorre chiudere il cursore SQL?
150100160412         If  $InzS06  and  Not $errGenerico;
150200160412           exsr  sr_CloseCursor;
150300160412         EndIf;
150400160412
150500160412       ENDSR;
150600160413
150700160413       //--------------------------------------------------------------
150800160413       //?Gestione subfile S07.                                        ?
150900160413       //--------------------------------------------------------------
151000160413       BEGSR  sr_GesS07;
151100160413
151200160413         // -?Inizializzazione subfile S07?
151300160413         if  $InzS07 = *on;
151400160413           exsr  sr_InzS07;
151500160413           $InzS07 = *off;
151600160413         endif;
151700160413
151800160413         // -?(Dis)Attivazione tasti funzionali a video?
151900160413         $F8Attivo   = *off;
152000160413         $F9Attivo   = *off;
152100160413         $F10Attivo  = *off;
152200160413         $F12Attivo  = *on;
152300160413         $RlUpAttivo = *off;
152400160413         $RlDnAttivo = *off;
152500160413
152600160413         // -?Posizionamento cursore?
152700160413         //  ?C7CsrRrn contiene il numero di riga del SubFile su cui?
152800160413         //  ?era posizionato il cursore; impostandolo in C7RcdNbr?
152900160413         //  ?si visualizza la pagina che era visualizzata quando?
153000160413         //  ?l'utente ha premuto l'ultimo tasto.?
153100160413         if  C7CsrRrn > *zero;
153200160413           C7RcdNbr = C7CsrRrn;
153300160413         else;
153400160413           C7RcdNbr = 1;
153500160413           write  MS13D06;          //(no rec.)?
153600160413         endif;
153700160413
153800160413         // -?Emissione Testata e Piede con tasti funzionali abilitati?
153900160413         write  MS13T01;
154000160413         write  MS13D02;
154100160413         write  MS13Z06;
154200160413         write  MS13P02;
154300160413
154400160413         // -?Emissione videata?
154500160413         exfmt  MS13C07;
154600160413
154700160413         clear  VIDmsg;
154800160413         reset  $ErrMessage;
154900160413         reset  $ErrGenerico;
155000160413
155100160413         Select;
155200160413
155300160413           // -?F3=Fine?
155400160413           When  dsp_aid = c_F03;
155500160413             exsr  sr_CloseCursor7;
155600160413             exsr  sr_CloseCursor;
155700160413             $Fine = *on;
155800160413             clear  $Video;
155900160413
156000160413           // -?F12=Ritorno?
156100160413           When  dsp_aid = c_F12;
156200160413             exsr  sr_CloseCursor7;
156300160413             $Video = 'S6';
156400160413
156500160413           // -?Roll-Up?
156600160413           When  dsp_aid = c_RollUp;
156700160413             // -?Verifica raggiungimento del limite di records?
156800160413             //  ?registrabili nel subfile (9999)?
156900160413             if  S07nrr = *hival;
157000160413               $ErrGenerico = *on;
157100160413               $ErrMessage  = *on;
157200160413               VIDmsg = sk_Msg(11);
157300160413               leavesr;
157400160413             endif;
157500160413             exsr sr_RollUpS07;
157600160413
157700160413         EndSl;
157800160413
157900160413       ENDSR;
158000160413
158100160413       //--------------------------------------------------------------
158200160413       //?Inizializzazione subfile S07.                                ?
158300160413       //--------------------------------------------------------------
158400160413       BEGSR  sr_InzS07;
158500160413
158600160413         // -?Pulizia subfile?
158700160413         $Sf7Dsp_N    = *on;
158800160413         $Sf7DspCtl_N = *on;
158900160413         write  MS13C07;
159000160413         $Sf7DspCtl_N = *off;
159100160413         $Sf7End      = *off;
159200160413
159300160413         $Sf7NxtChg   = *off;
159400160413
159500160413         clear  w_SflPag7;
159600160413         clear  C7RcdNbr;
159700160413         clear  C7CsrRrn;
159800160413         clear  S07nrr;
159900160413         clear  VIDmsg;
160000160413         $ErrMessage  = *off;
160100160413         $ErrGenerico = *off;
160200160413
160300160413         // -?Spegnimento degli indicatori di errore?
160400160413         %subst( IndDspF : 50 ) = *off;
160500160413
160600160413         // -?Caricamento della 1ª pagina di dati del subfile?
160700160413         // -?Preparazione stringa SQL?
160800160413         exsr  sr_PrepSQL7;
160900160413         // -?Apertura del cursore?
161000160413         exsr  sr_OpenCursor7;
161100160413         // -?Lettura del file?
161200160413         exsr  sr_ReadCursor7;
161300160413         DoW  Not $EoF7  and  S07nrr < c_SflPag7;
161400160413           exsr  sr_RollUpS07;
161500160413         EndDo;
161600160413
161700160413         // -?Visualizzazione del SFL (se ci sono dati)?
161800160413         $Sf7Dsp_N = ( S07nrr <= *zero );
161900160413
162000160413         // -?Attivazione del SFLEND?
162100160413         $Sf7End   = ( $EoF7 = *on );
162200160413
162300160413         // -?Impaginazione subfile?
162400160413         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
162500160413         if  S07nrr > *zero;
162600160413           C7RcdNbr = 1;
162700160413           C7CsrRrn = 1;
162800160413         else;
162900160413           clear  C7RcdNbr;
163000160413         endif;
163100160413
163200160413       ENDSR;
163300160413
163400160413       //--------------------------------------------------------------
163500160413       //?Caricamento singola pagine SubFile S07.                      ?
163600160413       //--------------------------------------------------------------
163700160413       BEGSR  sr_RollUpS07;
163800160413
163900160413         S07nrr     = ( w_SflPag7 * c_SflPag7 );
164000160413         w_SflPag7 += 1;
164100160413         $Sf7NxtChg = *off;
164200160413
164300160413         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
164400160413         DoW  $EoF7  = *off    and
164500160413              S07nrr < *hival  and
164600160413              S07nrr < ( w_SflPag7 * c_SflPag7 );
164700160413
164800160413           // -?Caricamento dati nel record del subfile?
164900160413           exsr  sr_RieRecS07;
165000160413           S07nrr += 1;
165100160413           write  MS13S07;
165200160413
165300160413           // -?Lettura record successivo nell'archivio?
165400160413           exsr  sr_ReadCursor7;
165500160413
165600160413         EndDo;
165700160413
165800160413         // -?Visualizzazione del SFL (se ci sono dati)?
165900160413         $Sf7Dsp_N = ( S07nrr <= *zero );
166000160413
166100160413         // -?Attivazione (eventuale) del SFLEND?
166200160413         $Sf7End   = ( $EoF7 = *on );
166300160413
166400160413         // -?Posizionamento cursore al 1° rcd della pagina?
166500160413         if  S07nrr > *zero;
166600160413           wPag     = S07nrr / c_SflPag7;
166700160413           wRig     = S07nrr - ( c_SflPag7 * wPag );
166800160413           C7RcdNbr = wPag * c_SflPag7;
166900160413           if  wRig > *zero;
167000160413             C7RcdNbr = C7RcdNbr + 1;
167100160413           else;
167200160413             C7RcdNbr = C7RcdNbr - c_SflPag7 + 1;
167300160413           endif;
167400160413         else;
167500160413           clear  C7RcdNbr;
167600160413         endif;
167700160413
167800160413         C7CsrRrn = C7RcdNbr;
167900160413
168000160413       ENDSR;
168100160413
168200160413       //--------------------------------------------------------------
168300160413       //?Caricamento singolo record nel SubFile S07.                  ?
168400160413       //--------------------------------------------------------------
168500160413       BEGSR  sr_RieRecS07;
168600160413
168700160413         clear  MS13S07;
168800160413
168900160413         // -?Campi Output?
169000160413         // ·?BarCode?
169100160413         S07barC   = ds_FIPSL7.PSLbarCo;
169200160413         // ·?Utente Spunta?
169300160413         S07usr    = ds_FIPSL7.PSLusr;
169400160413         // ·?Data/Ora Rilevazione?
169500160413         if  %check( c_Digits : ds_FIPSL7.PSLdtOraR ) = *zero;
169600160413           ds_DtOraR = ds_FIPSL7.PSLdtOraR;
169700160413           wDate_EUR = %date( ds_DtOraR.Data : *iso );
169800160413           S07dtOraR = %editc( %dec( wDate_EUR ) : 'Y' ) + ' - ' +
169900160413                       %editw( ds_DtOraR.Ora : '0 :  :  ' );
170000160413         else;
170100160413           S07dtOraR = %subst( ds_FIPSL7.PSLdtOraR :  1 : 4 ) + '-' +
170200160413                       %subst( ds_FIPSL7.PSLdtOraR :  5 : 2 ) + '-' +
170300160413                       %subst( ds_FIPSL7.PSLdtOraR :  7 : 2 ) + '   ' +
170400160413                       %subst( ds_FIPSL7.PSLdtOraR :  9 : 2 ) + ':' +
170500160413                       %subst( ds_FIPSL7.PSLdtOraR : 11 : 2 ) + ':' +
170600160413                       %subst( ds_FIPSL7.PSLdtOraR : 13 : 2 );
170700160413         endif;
170800160413         // ·?Numero Foglio?
170900160413         if  %check( c_Digits : ds_FIPSL7.PSLnfv ) = *zero;
171000160413           evalR S07nfv  = %editc( %int( ds_FIPSL7.PSLnfv ) : '1' );
171100160413         else;
171200160413           S07nfv  = ds_FIPSL7.PSLnfv;
171300160413         endif;
171400160413
171500160413       ENDSR;
171600161219
171700161219       //--------------------------------------------------------------
171800161219       //?Caricamento F.V. da elaborare.                               ?
171900161219       //?(NON più solo per Categoria 2=Arrivi)                        ?
172000161219       //--------------------------------------------------------------
172100161219       BEGSR  sr_Intab_FV_Elab;
172200161219
172300161219         clear  wNFV_9_0;
172400161219         clear  wNFV_x_SQL;
172500161219         clear  keyFNFVV02;
172600161219
172700161219         //*·IF  V1Cnpg = 2;
172800161219
172900161219           k_FVVnpg  = V1Cnpg;
173000161219           k_FVVdfv  = WWWdfv;
173100161219           k_FVVfgs  = V1Cfil;
173200161219           setll  %kds( keyFNFVV02 )  FNFVV000;
173300161219           reade  %kds( keyFNFVV02 )  FNFVV000;
173400161219
173500161219           DoW  Not %eof(FNFVV02L);
173600161219
173700161219             if  FVVatb = *blank;
173800161219
173900161219               if  FVVnpg <> 3  or  FVVspg  = 'A';
174000161219
174100161219                 if  wNFV_9_0 <> *zero;
174200161219                   wNFV_x_SQL += ', ';
174300161219                 endif;
174400161219
174500161219                 wNFV_9_0 = FVVnfv;
174600161219                 wNFV_x_SQL   += '''' + %editc( wNFV_9_0 : 'X' ) + '''';
174700161219
174800161219               endif;
174900161219
175000161219             endif;
175100161219             reade  %kds( keyFNFVV02 )  FNFVV000;
175200161219
175300161219           EndDo;
175400161219
175500161219           if  wNFV_x_SQL = *blank;
175600161219             wNFV_x_SQL = '0';
175700161219           endif;
175800161219
175900161219         //*·ENDIF;
176000161219
176100161219       ENDSR;
176200160412
176300160412       //--------------------------------------------------------------
176400160412       //?Impostazione dati per singola Postazione Fissa x Arrivi.     ?
176500160412       //--------------------------------------------------------------
176600160412       BEGSR  sr_RepPostFissa;
176700160412
176800160412         clear  wDesP;
176900160412         clear  wNumP;
177000160412
177100160412         select;
177200160412           // -?Postazione errata?
177300160412           when  ( sk_MSL_Ent(xP) <= *zero  or  sk_MSL_Ent(xP) > 20 );
177400160412             leavesr;
177500160413           //*// -?Postazione vuota?
177600160413           //*when  sk_Ncl_Incomp( sk_MSL_Ent(xP) ) = *zero;
177700160413           //*  leavesr;
177800160412         endsl;
177900160412
178000160412         // -?Descrizione Postazione?
178100161028         select;
178200161028           when  sk_MSL_Pep(xP) = *blank;
178300161028             wDesP = *all'? ';
178400161028           when  V1Cnpg = 5;
178500161028             wDesP = sk_MSL_Pep(xP) + ' - ' + sk_MSL_De5(xP);
178600161028           when  V1Cnpg <> 5;
178700161028             wDesP = sk_MSL_Pep(xP) + ' - ' + sk_MSL_Des(xP);
178800161028         endsl;
178900160412
179000160412         // -?Numero Colli x Postazione?
179100160412         wNumP = sk_Ncl_Incomp( sk_MSL_Ent(xP) );
179200160412
179300160412       ENDSR;
179400160411
179500160411       //--------------------------------------------------------------
179600160411       //?Memorizzaz. Postaz. Fisse da tab."MSL" (Layout Postaz. VDL). ?
179700160411       //--------------------------------------------------------------
179800160411       BEGSR  sr_Tab_MSL;
179900160411
180000160411         clear  xP;
180100160411         clear  sk_MSL_Pep;
180200160411         clear  sk_MSL_Ent;
180300160411         clear  sk_MSL_Des;
180400161028         clear  sk_MSL_De5;
180500160411
180600160411         setll  ( 'MSL' : %editc( V1Cfil : 'X' ) )  TNTBE000;
180700160411         reade  ( 'MSL' : %editc( V1Cfil : 'X' ) )  TNTBE000;
180800160411
180900160411         // -?Ciclo di memorizzazione Postazioni Fisse (Incompatibili)?
181000160411         DoW  Not  %eof(TNTBE01L)  and  xP < %elem(sk_MSL_Pep);
181100160411
181200160411           dMSL = TBEuni;
181300160411
181400160711           //*if  TBEatb = *blank  and  dMSL.§MSLapl = 'I';
181500161025           if  TBEatb = *blank  and  ( dMSL.§MSLapl = dDRM.§DRMaFiss
181600161216                                and    V1Cnpg      <> 5
181700161025                                 or    dMSL.§MSLap5 = dDRM.§DRMaFiss
181800161025                                and    V1Cnpg       = 5 );
181900160411
182000160411             xP += 1;
182100160411             sk_MSL_Pep(xP) = %subst( dMSL.§MSLpep : 10 );
182200160411             sk_MSL_Ent(xP) = dMSL.§MSLent;
182300160411             sk_MSL_Des(xP) = dMSL.§MSLdes;
182400161028             sk_MSL_De5(xP) = dMSL.§MSLde5;
182500160411
182600160411           endif;
182700160411
182800160411           reade  ( 'MSL' : %editc( V1Cfil : 'X' ) )  TNTBE000;
182900160411
183000160411         EndDo;
183100160411
183200160411       ENDSR;
183300160322
183400160322       //--------------------------------------------------------------
183500160322       //?Elaborazione Attività Smistamento del Macchinone Smistacolli.?
183600160322       //--------------------------------------------------------------
183700160322       BEGSR  sr_ElabFNMSA;
183800160322
183900160322         // -?Ciclo di lettura dati del foglio da caricare?
184000170201         clear  keyFNMSA02;
184100160322         k_MSAapl  = V1Capl;
184200160322         k_MSAfil  = V1Cfil;
184300160411         k_MSAdfv  = WWWdfv;
184400160322         k_MSAnpg  = V1Cnpg;
184500160411         k_MSAdtl  = WWWdli;
184600160408         k_MSAorl  = V1Coli;
184700170201         setll  %kds( keyFNMSA02 )  FNMSA000;
184800170201         reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
184900160322
185000170201         //*·DoW  Not %eof(FNMSA02L);
185100170201         DoW  Not %eof(FNMSA02L)  and
185200161107              ( ( MSAdtl * 10000 ) + MSAorl ) <=
185300161107              ( ( WWWdlf * 10000 ) + V1Colf );
185400160322           Select;
185500160322
185600160415             // -?"Letture" INIZIALI da scartare?
185700160415             //When  V1Csmv = 'S'  and  MSApesInd = *zero;
185800160415             When      V1Csmv    = 'S'  and
185900160415                   ( ( MSAtcl   <> 999  and  W2Ddli    = *zero )    or
186000160415                     ( MSAtcl    = 999  and  W2DdliM   = *zero ) )  and
186100160415                       MSApesInd = *zero;
186200160324
186300160322             // -?ARRIVI?
186400161010             When  MSAtcl <> 999  or  MSAapl = 'P';
186500160322
186600160322               // -?Data/Ora iniziali?
186700160322               if  W2Ddli  = *zero;
186800160322                 W2Ddli  = MSAdtl;
186900160414                 W2Doli  = MSAorl;
187000160322               endif;
187100160322
187200160322               // -?Data/Ora finali?
187300160322               W2Ddlf    = MSAdtl;
187400160414               W2Dolf    = MSAorl;
187500160322
187600160322               // -?Minuti lavorati (per KPI)?
187700160322               wMinLav  += 1;
187800160322
187900160322               // -?Colli lavorati (per KPI)?
188000160322               wNcl     += MSApep001 +
188100160322                           MSApep002 +
188200160322                           MSApep003 +
188300160322                           MSApep004 +
188400160322                           MSApep005 +
188500160322                           MSApep006 +
188600160322                           MSApep007 +
188700160322                           MSApep008 +
188800160322                           MSApep009 +
188900160322                           MSApep010 +
189000160322                           MSApep011 +
189100160322                           MSApep012 +
189200160322                           MSApep013 +
189300160322                           MSApep014 +
189400160322                           MSApep015 +
189500160322                           MSApep016 +
189600160322                           MSApep017 +
189700160322                           MSApep018 +
189800160322                           MSApep019 +
189900160322                           MSApep020;    // -?ANCHE post.20!?
190000160722
190100160722               // -?Colli lavorati (Picking su Rulliera)?
190200160722               wNcl_K   += ( MSApesind - MSAnlp - MSApusLos );
190300160322
190400160322               // -?Minuti lavorati per Colli obiettivo (per KPI)?
190500160322               //  ?(SENZA post.20!)?
190600160322               if  MSApep001 > *zero;
190700160322                 wMINxNCL_ob  += 1;
190800160322               endif;
190900160322               if  MSApep002 > *zero;
191000160322                 wMINxNCL_ob  += 1;
191100160322               endif;
191200160322               if  MSApep003 > *zero;
191300160322                 wMINxNCL_ob  += 1;
191400160322               endif;
191500160322               if  MSApep004 > *zero;
191600160322                 wMINxNCL_ob  += 1;
191700160322               endif;
191800160322               if  MSApep005 > *zero;
191900160322                 wMINxNCL_ob  += 1;
192000160322               endif;
192100160322               if  MSApep006 > *zero;
192200160322                 wMINxNCL_ob  += 1;
192300160322               endif;
192400160322               if  MSApep007 > *zero;
192500160322                 wMINxNCL_ob  += 1;
192600160322               endif;
192700160322               if  MSApep008 > *zero;
192800160322                 wMINxNCL_ob  += 1;
192900160322               endif;
193000160322               if  MSApep009 > *zero;
193100160322                 wMINxNCL_ob  += 1;
193200160322               endif;
193300160322               if  MSApep010 > *zero;
193400160322                 wMINxNCL_ob  += 1;
193500160322               endif;
193600160322               if  MSApep011 > *zero;
193700160322                 wMINxNCL_ob  += 1;
193800160322               endif;
193900160322               if  MSApep012 > *zero;
194000160322                 wMINxNCL_ob  += 1;
194100160322               endif;
194200160322               if  MSApep013 > *zero;
194300160322                 wMINxNCL_ob  += 1;
194400160322               endif;
194500160322               if  MSApep014 > *zero;
194600160322                 wMINxNCL_ob  += 1;
194700160322               endif;
194800160322               if  MSApep015 > *zero;
194900160322                 wMINxNCL_ob  += 1;
195000160322               endif;
195100160322               if  MSApep016 > *zero;
195200160322                 wMINxNCL_ob  += 1;
195300160322               endif;
195400160322               if  MSApep017 > *zero;
195500160322                 wMINxNCL_ob  += 1;
195600160322               endif;
195700160322               if  MSApep018 > *zero;
195800160322                 wMINxNCL_ob  += 1;
195900160322               endif;
196000160322               if  MSApep019 > *zero;
196100160322                 wMINxNCL_ob  += 1;
196200160322               endif;
196300160415               //if  MSApep020 > *zero; ?(SENZA post.20!)?
196400160322               //  wMINxNCL_ob  += 1;
196500160322               //endif;
196600160322
196700160322               // -?Colli in Overflow?
196800160526               if  MSAapl = 'V';
196900160526                 V2Dovrf  += ( MSApesInd - MSApusSor );
197000160526               endif;
197100160322
197200160322               // -?Colli non letti da scanner?
197300160322               V2Dnol   += MSAnls;
197400160322
197500160322             // -?PICKING (MOVIMENTO)?
197600160324             When  MSAtcl =  999;
197700160322
197800160322               // -?Data/Ora iniziali?
197900160322               if  W2DdliM = *zero;
198000160322                 W2DdliM = MSAdtl;
198100160414                 W2DoliM = MSAorl;
198200160322               endif;
198300160322
198400160322               // -?Data/Ora finali?
198500160322               W2DdlfM   = MSAdtl;
198600160414               W2DolfM   = MSAorl;
198700160322
198800160322               // -?Minuti lavorati (per KPI)?
198900160322               wMinLavM += 1;
199000160322
199100160322               // -?Colli lavorati (per KPI)?
199200160322               wNclM    += MSApep001 +
199300160322                           MSApep002 +
199400160322                           MSApep003 +
199500160322                           MSApep004 +
199600160322                           MSApep005 +
199700160322                           MSApep006 +
199800160322                           MSApep007 +
199900160322                           MSApep008 +
200000160322                           MSApep009 +
200100160322                           MSApep010 +
200200160322                           MSApep011 +
200300160322                           MSApep012 +
200400160322                           MSApep013 +
200500160322                           MSApep014 +
200600160322                           MSApep015 +
200700160322                           MSApep016 +
200800160322                           MSApep017 +
200900160322                           MSApep018 +
201000160322                           MSApep019 +
201100160322                           MSApep020;    // -?ANCHE post.20!?
201200160722
201300160722               // -?Colli lavorati (Picking su Rulliera)?
201400160722               wNclM_K  += ( MSApesind - MSAnlp - MSApusLos );
201500160322
201600160322               // -?Minuti lavorati per Colli obiettivo (per KPI)?
201700160322               //  ?(SENZA post.20!)?
201800160322               if  MSApep001 > *zero;
201900160322                 wMINxNCL_obM += 1;
202000160322               endif;
202100160322               if  MSApep002 > *zero;
202200160322                 wMINxNCL_obM += 1;
202300160322               endif;
202400160322               if  MSApep003 > *zero;
202500160322                 wMINxNCL_obM += 1;
202600160322               endif;
202700160322               if  MSApep004 > *zero;
202800160322                 wMINxNCL_obM += 1;
202900160322               endif;
203000160322               if  MSApep005 > *zero;
203100160322                 wMINxNCL_obM += 1;
203200160322               endif;
203300160322               if  MSApep006 > *zero;
203400160322                 wMINxNCL_obM += 1;
203500160322               endif;
203600160322               if  MSApep007 > *zero;
203700160322                 wMINxNCL_obM += 1;
203800160322               endif;
203900160322               if  MSApep008 > *zero;
204000160322                 wMINxNCL_obM += 1;
204100160322               endif;
204200160322               if  MSApep009 > *zero;
204300160322                 wMINxNCL_obM += 1;
204400160322               endif;
204500160322               if  MSApep010 > *zero;
204600160322                 wMINxNCL_obM += 1;
204700160322               endif;
204800160322               if  MSApep011 > *zero;
204900160322                 wMINxNCL_obM += 1;
205000160322               endif;
205100160322               if  MSApep012 > *zero;
205200160322                 wMINxNCL_obM += 1;
205300160322               endif;
205400160322               if  MSApep013 > *zero;
205500160322                 wMINxNCL_obM += 1;
205600160322               endif;
205700160322               if  MSApep014 > *zero;
205800160322                 wMINxNCL_obM += 1;
205900160322               endif;
206000160322               if  MSApep015 > *zero;
206100160322                 wMINxNCL_obM += 1;
206200160322               endif;
206300160322               if  MSApep016 > *zero;
206400160322                 wMINxNCL_obM += 1;
206500160322               endif;
206600160322               if  MSApep017 > *zero;
206700160322                 wMINxNCL_obM += 1;
206800160322               endif;
206900160322               if  MSApep018 > *zero;
207000160322                 wMINxNCL_obM += 1;
207100160322               endif;
207200160322               if  MSApep019 > *zero;
207300160322                 wMINxNCL_obM += 1;
207400160322               endif;
207500160415               //if  MSApep020 > *zero; ?(SENZA post.20!)?
207600160322               //  wMINxNCL_obM += 1;
207700160322               //endif;
207800160322
207900160322               // -?Colli in Overflow?
208000160526               if  MSAapl = 'V';
208100160526                 V2DovrfM += ( MSApesInd - MSApusSor );
208200160526               endif;
208300160322
208400160322               // -?Colli non letti da scanner?
208500160322               V2DnolM  += MSAnls;
208600160322
208700160322           EndSl;
208800160322
208900170201           reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
209000160322
209100160322         EndDo;
209200160323
209300160323         // -?NON trovate letture?
209400160323         if  ( wMinLav + wMinLavM ) = *zero;
209500160323           leavesr;
209600160323         endif;
209700160322
209800160322         // -?Data/Ora iniziale Arrivi?
209900160414         if  W2Ddli > *zero;
210000160414           reset  WLBdat;
210100160414           G08inv = W2Ddli;
210200160414           XSRDA8 ( WLBdat );
210300160414           V2Ddli = G08dat;
210400160414           V2Doli = %editw( W2Doli : '0 :  ' );
210500160414         endif;
210600160322
210700160322         // -?Data/Ora finale Arrivi?
210800160414         if  W2Ddlf > *zero;
210900160414           reset  WLBdat;
211000160414           G08inv = W2Ddlf;
211100160414           XSRDA8 ( WLBdat );
211200160414           V2Ddlf = G08dat;
211300160414           V2Dolf = %editw( W2Dolf : '0 :  ' );
211400160414         endif;
211500160322
211600160322         // -?Data/Ora iniziale Picking (Movimento)?
211700160414         if  W2DdliM > *zero;
211800160414           reset  WLBdat;
211900160414           G08inv  = W2DdliM;
212000160414           XSRDA8 ( WLBdat );
212100160414           V2DdliM = G08dat;
212200160414           V2DoliM = %editw( W2DoliM : '0 :  ' );
212300160414         endif;
212400160322
212500160322         // -?Data/Ora finale Picking (Movimento)?
212600160414         if  W2DdlfM > *zero;
212700160414           reset  WLBdat;
212800160414           G08inv = W2DdlfM;
212900160414           XSRDA8 ( WLBdat );
213000160414           V2DdlfM = G08dat;
213100160414           V2DolfM = %editw( W2DolfM : '0 :  ' );
213200160414         endif;
213300160322
213400160414         If  dKPI.§KPIpba > *zero;
213500160414
213600160414           // -?KPI ARRIVI - effettivo & obiettivo?
213700160414           if  wNcl  > *zero  or  wMINxNCL_ob  > *zero;
213800160414             // -?Calcolo tempo effettivamente lavorato (HH:MM)?
213900160414             §min  = wMinLav;
214000160414             clear  §hh;
214100160414             clear  §mm;
214200160414             exsr  sr_CalcHML;
214300160414             // -?KPI = Media Pacchi: Tot. Pacchi / Tempo lavorato?
214400160414             //  ?(Effettivo e Obiettivo)?
214500160414             if  wMinLav  = *zero;
214600160414               wMinLav  = 1;
214700160414             endif;
214800160414             eval(h)  W2Dkpi  = wNcl / wMinLav;
214900160414             V2DkpiE  = %editc( W2Dkpi : '3' );
215000160414             eval(h)  W2Dkpi  = ( dKPI.§KPIpba * wMINxNCL_ob ) / wMinLav;
215100160414             V2DkpiO  = %editc( W2Dkpi : '3' );
215200160414           endif;
215300160414
215400160414           // -?KPI PICKING (MOVIMENTO) - effettivo & obiettivo?
215500160414           if  wNclM > *zero  or  wMINxNCL_obM > *zero;
215600160414             // -?Calcolo tempo effettivamente lavorato (HH:MM)?
215700160414             §min  = wMinLavM;
215800160414             clear  §hh;
215900160414             clear  §mm;
216000160414             exsr  sr_CalcHML;
216100160414             // -?KPI = Media Pacchi: Tot. Pacchi / Tempo lavorato?
216200160414             //  ?(Effettivo e Obiettivo)?
216300160414             if  wMinLavM = *zero;
216400160414               wMinLavM = 1;
216500160414             endif;
216600160414             eval(h)  W2Dkpi  = wNclM / wMinLavM;
216700160414             V2DkpiEm = %editc( W2Dkpi : '3' );
216800160414             eval(h)  W2Dkpi  = ( dKPI.§KPIpba * wMINxNCL_obM ) / wMinLavM;
216900160414             V2DkpiOm = %editc( W2Dkpi : '3' );
217000160414           endif;
217100160414
217200160414         EndIf;
217300160408
217400160408         // -?VDL/CAR: Colli caricati?
217500160722         If  V1Capl = 'K';
217600160722           if  wNcl_K  <= 99999;
217700160722             V2Dtcv   = wNcl_K;
217800160722           else;
217900160722             V2Dtcv   = 99999;
218000160722           endif;
218100160722           if  wNclM_K <= 99999;
218200160722             V2DtcvM  = wNclM_K;
218300160722           else;
218400160722             V2DtcvM  = 99999;
218500160722           endif;
218600160722         Else;
218700160722           if  wNcl  <= 99999;
218800160722             V2Dtcv   = wNcl;
218900160722           else;
219000160722             V2Dtcv   = 99999;
219100160722           endif;
219200160722           if  wNclM <= 99999;
219300160722             V2DtcvM  = wNclM;
219400160722           else;
219500160722             V2DtcvM  = 99999;
219600160722           endif;
219700160722         EndIf;
219800160722
219900160722         // -?Ricalcolo dati "Movimento (Picking)"   SE?
220000160722         //  ?fil. 006-Padova Interporto?
220100160729         if  V1Cfil = 006  and  V1Cnpg = 2;
220200160722           exsr  sr_ElabFNMSA_M_006;
220300160722         endif;
220400160322
220500160322       ENDSR;
220600160722
220700160722       //--------------------------------------------------------------
220800160722       //?Elaborazione Attività Smistamento del Macchinone Smistacolli ?
220900160722       //?- Movimento (Picking) - per la fil. 006 (Padova Interporto). ?
221000160722       //--------------------------------------------------------------
221100160722       BEGSR  sr_ElabFNMSA_M_006;
221200161108
221300161108         clear  W2DdliM;
221400161108         clear  W2DoliM;
221500161108         clear  W2DdlfM;
221600161108         clear  W2DolfM;
221700161108         clear  V2DdliM;
221800161108         clear  V2DoliM;
221900161108         clear  V2DdlfM;
222000161108         clear  V2DolfM;
222100161108         clear  wMinLavM;
222200161108         clear  wNclM;
222300161108         clear  wNclM_K;
222400161108         clear  wMINxNCL_obM;
222500161108         clear  V2DnolM;
222600161003
222700161003         // -?Reperimento Obiettivi di Produttività?
222800161003         //  ?di un Impianto Automatico Smistacolli?
222900161003         clear  dKPI_K;
223000161003         if  getTabella ( c_Tntbe : 'KPI' : %editc( V1Cnpg : 'X' ) : 'K' :
223100161003                          *omit : *omit :
223200161003                          *omit : *omit :
223300161003                          *omit : *omit : *omit : *omit :
223400161003                          *omit : *omit : *omit : *omit :
223500161003                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
223600161003                          = *zero      AND
223700161003               ds_TNTBE.TBEatb = *blank;
223800161003           dKPI_K  = ds_TNTBE.TBEuni;
223900161003         endif;
224000160722
224100160722         // -?Ciclo di lettura dati del foglio da caricare?
224200170201         clear  keyFNMSA02;
224300160722         k_MSAapl  = 'K';
224400160722         k_MSAfil  = V1Cfil;
224500160722         k_MSAdfv  = WWWdfv;
224600160722         k_MSAnpg  = V1Cnpg;
224700160722         k_MSAdtl  = WWWdli;
224800160722         k_MSAorl  = V1Coli;
224900170201         setll  %kds( keyFNMSA02 )  FNMSA000;
225000170201         reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
225100160722
225200170201         //*·DoW  Not %eof(FNMSA02L);
225300170201         DoW  Not %eof(FNMSA02L)  and
225400161107              ( ( MSAdtl * 10000 ) + MSAorl ) <=
225500161107              ( ( WWWdlf * 10000 ) + V1Colf );
225600160722
225700160722           // -?"Letture" INIZIALI da scartare?
225800160722           If      V1Csmv    = 'S'  and
225900160722               ( ( MSAtcl   <> 999  and  W2Ddli    = *zero )    or
226000160722                 ( MSAtcl    = 999  and  W2DdliM   = *zero ) )  and
226100160722                   MSApesInd = *zero;
226200160722
226300160722           // -?PICKING (MOVIMENTO)?
226400160722           Else;
226500160722
226600160722             // -?Data/Ora iniziali?
226700160722             if  W2DdliM = *zero;
226800160722               W2DdliM = MSAdtl;
226900160722               W2DoliM = MSAorl;
227000160722             endif;
227100160722
227200160722             // -?Data/Ora finali?
227300160722             W2DdlfM   = MSAdtl;
227400160722             W2DolfM   = MSAorl;
227500160722
227600160722             // -?Minuti lavorati (per KPI)?
227700160722             wMinLavM += 1;
227800160722
227900160722             // -?Colli lavorati (per KPI)?
228000160722             wNclM    += MSApep001 +
228100160722                         MSApep002 +
228200160722                         MSApep003 +
228300160722                         MSApep004 +
228400160722                         MSApep005 +
228500160722                         MSApep006 +
228600160722                         MSApep007 +
228700160722                         MSApep008 +
228800160722                         MSApep009 +
228900160722                         MSApep010 +
229000160722                         MSApep011 +
229100160722                         MSApep012 +
229200160722                         MSApep013 +
229300160722                         MSApep014 +
229400160722                         MSApep015 +
229500160722                         MSApep016 +
229600160722                         MSApep017 +
229700160722                         MSApep018 +
229800160722                         MSApep019 +
229900160722                         MSApep020;    // -?ANCHE post.20!?
230000160722
230100160722             // -?Colli lavorati (Picking su Rulliera)?
230200160722             wNclM_K  += ( MSApesind - MSAnlp - MSApusLos );
230300160722
230400160722             // -?Minuti lavorati per Colli obiettivo (per KPI)?
230500160722             //  ?(SENZA post.20!)?
230600160722             if  MSApep001 > *zero;
230700160722               wMINxNCL_obM += 1;
230800160722             endif;
230900160722             if  MSApep002 > *zero;
231000160722               wMINxNCL_obM += 1;
231100160722             endif;
231200160722             if  MSApep003 > *zero;
231300160722               wMINxNCL_obM += 1;
231400160722             endif;
231500160722             if  MSApep004 > *zero;
231600160722               wMINxNCL_obM += 1;
231700160722             endif;
231800160722             if  MSApep005 > *zero;
231900160722               wMINxNCL_obM += 1;
232000160722             endif;
232100160722             if  MSApep006 > *zero;
232200160722               wMINxNCL_obM += 1;
232300160722             endif;
232400160722             if  MSApep007 > *zero;
232500160722               wMINxNCL_obM += 1;
232600160722             endif;
232700160722             if  MSApep008 > *zero;
232800160722               wMINxNCL_obM += 1;
232900160722             endif;
233000160722             if  MSApep009 > *zero;
233100160722               wMINxNCL_obM += 1;
233200160722             endif;
233300160722             if  MSApep010 > *zero;
233400160722               wMINxNCL_obM += 1;
233500160722             endif;
233600160722             if  MSApep011 > *zero;
233700160722               wMINxNCL_obM += 1;
233800160722             endif;
233900160722             if  MSApep012 > *zero;
234000160722               wMINxNCL_obM += 1;
234100160722             endif;
234200160722             if  MSApep013 > *zero;
234300160722               wMINxNCL_obM += 1;
234400160722             endif;
234500160722             if  MSApep014 > *zero;
234600160722               wMINxNCL_obM += 1;
234700160722             endif;
234800160722             if  MSApep015 > *zero;
234900160722               wMINxNCL_obM += 1;
235000160722             endif;
235100160722             if  MSApep016 > *zero;
235200160722               wMINxNCL_obM += 1;
235300160722             endif;
235400160722             if  MSApep017 > *zero;
235500160722               wMINxNCL_obM += 1;
235600160722             endif;
235700160722             if  MSApep018 > *zero;
235800160722               wMINxNCL_obM += 1;
235900160722             endif;
236000160722             if  MSApep019 > *zero;
236100160722               wMINxNCL_obM += 1;
236200160722             endif;
236300160722             //if  MSApep020 > *zero; ?(SENZA post.20!)?
236400160722             //  wMINxNCL_obM += 1;
236500160722             //endif;
236600160722
236700160722             // -?Colli non letti da scanner?
236800160722             V2DnolM  += MSAnls;
236900160722
237000160722           EndIf;
237100160722
237200170201           reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
237300160722
237400160722         EndDo;
237500160722
237600160722         // -?NON trovate letture?
237700160722         if  wMinLavM = *zero;
237800160722           leavesr;
237900160722         endif;
238000160722
238100160722         // -?Data/Ora iniziale Picking (Movimento)?
238200160722         if  W2DdliM > *zero;
238300160722           reset  WLBdat;
238400160722           G08inv  = W2DdliM;
238500160722           XSRDA8 ( WLBdat );
238600160722           V2DdliM = G08dat;
238700160722           V2DoliM = %editw( W2DoliM : '0 :  ' );
238800160722         endif;
238900160722
239000160722         // -?Data/Ora finale Picking (Movimento)?
239100160722         if  W2DdlfM > *zero;
239200160722           reset  WLBdat;
239300160722           G08inv = W2DdlfM;
239400160722           XSRDA8 ( WLBdat );
239500160722           V2DdlfM = G08dat;
239600160722           V2DolfM = %editw( W2DolfM : '0 :  ' );
239700160722         endif;
239800160722
239900161003         If  dKPI_K.§KPIpba > *zero;
240000160722
240100160722           // -?KPI PICKING (MOVIMENTO) - effettivo & obiettivo?
240200160722           if  wNclM > *zero  or  wMINxNCL_obM > *zero;
240300160722             // -?Calcolo tempo effettivamente lavorato (HH:MM)?
240400160722             §min  = wMinLavM;
240500160722             clear  §hh;
240600160722             clear  §mm;
240700160722             exsr  sr_CalcHML;
240800160722             // -?KPI = Media Pacchi: Tot. Pacchi / Tempo lavorato?
240900160722             //  ?(Effettivo e Obiettivo)?
241000160722             if  wMinLavM = *zero;
241100160722               wMinLavM = 1;
241200160722             endif;
241300160722             eval(h)  W2Dkpi  = wNclM / wMinLavM;
241400160722             V2DkpiEm = %editc( W2Dkpi : '3' );
241500161003             eval(h)  W2Dkpi  = ( dKPI_K.§KPIpba * wMINxNCL_obM ) / wMinLavM;
241600160722             V2DkpiOm = %editc( W2Dkpi : '3' );
241700160722           endif;
241800160722
241900160722         EndIf;
242000160722
242100160722         // -?VDL/CAR: Colli caricati?
242200160722         if  wNclM_K <= 99999;
242300160722           V2DtcvM  = wNclM_K;
242400160722         else;
242500160722           V2DtcvM  = 99999;
242600160722         endif;
242700160722
242800160722       ENDSR;
242900160408
243000160408       //--------------------------------------------------------------
243100160408       //?Elaborazione Attività Smistamento per "FISSI RPVP".          ?
243200160408       //--------------------------------------------------------------
243300160408       BEGSR  sr_ElabFNMSA_P;
243400160408
243500160408         // -?Ciclo di lettura dati del foglio da caricare?
243600170201         clear  keyFNMSA02;
243700160711         //*k_MSAapl  = 'P';
243800160711         k_MSAapl  = dDRM.§DRMaRPVP;
243900160408         k_MSAfil  = V1Cfil;
244000160411         k_MSAdfv  = WWWdfv;
244100160408         k_MSAnpg  = V1Cnpg;
244200160411         k_MSAdtl  = WWWdli;
244300160408         k_MSAorl  = V1Coli;
244400170201         setll  %kds( keyFNMSA02 )  FNMSA000;
244500170201         reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
244600160408
244700170201         //*·DoW  Not %eof(FNMSA02L);
244800170201         DoW  Not %eof(FNMSA02L)  and
244900161107              ( ( MSAdtl * 10000 ) + MSAorl ) <=
245000161107              ( ( WWWdlf * 10000 ) + V1Colf );
245100161007
245200161007           // -?Colli lavorati?
245300161007           V2Dtcp   += MSApep001 +
245400161007                       MSApep002 +
245500161007                       MSApep003 +
245600161007                       MSApep004 +
245700161007                       MSApep005 +
245800161007                       MSApep006 +
245900161007                       MSApep007 +
246000161007                       MSApep008 +
246100161007                       MSApep009 +
246200161007                       MSApep010 +
246300161007                       MSApep011 +
246400161007                       MSApep012 +
246500161007                       MSApep013 +
246600161007                       MSApep014 +
246700161007                       MSApep015 +
246800161007                       MSApep016 +
246900161007                       MSApep017 +
247000161007                       MSApep018 +
247100161007                       MSApep019 +
247200161007                       MSApep020;    // -?ANCHE post.20!?
247300161007
247400161007           // -?Data/Ora iniziali?
247500161007           if  MSAapl  = V1Capl  and
247600161007               V2Ddli  = *zero   and  W2Ddli  = *zero  and
247700161007               W2Ddli  = *zero;
247800161007             W2Ddli  = MSAdtl;
247900161007             W2Doli  = MSAorl;
248000161007           endif;
248100161007
248200161007           // -?Data/Ora finali?
248300161007           if  MSAapl  = V1Capl  and
248400161007               V2Ddli  = *zero   and  V2Ddlf  = *zero;
248500161007             W2Ddlf    = MSAdtl;
248600161007             W2Dolf    = MSAorl;
248700161007           endif;
248800161007
248900161007           // -?Minuti lavorati (per KPI)?
249000161007           if  MSAapl  = V1Capl  and
249100161007               V2Ddli  = *zero   and  V2Ddlf  = *zero;
249200161007             wMinLav  += 1;
249300161007           endif;
249400160408
249500170201           reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
249600160408
249700160408         EndDo;
249800160414
249900160414         // -?NON trovate letture?
250000160414         if  ( wMinLav + wMinLavM ) = *zero;
250100160414           leavesr;
250200160414         endif;
250300160414
250400160414         If  V2Ddli  = *zero  and  V2Ddlf  = *zero;
250500160414
250600160414           // -?Data/Ora iniziale Arrivi?
250700160414           if  W2Ddli > *zero;
250800160414             reset  WLBdat;
250900160414             G08inv = W2Ddli;
251000160414             XSRDA8 ( WLBdat );
251100160414             V2Ddli = G08dat;
251200160414             V2Doli = %editw( W2Doli : '0 :  ' );
251300160414           endif;
251400160414
251500160414           // -?Data/Ora finale Arrivi?
251600160414           if  W2Ddlf > *zero;
251700160414             reset  WLBdat;
251800160414             G08inv = W2Ddlf;
251900160414             XSRDA8 ( WLBdat );
252000160414             V2Ddlf = G08dat;
252100160414             V2Dolf = %editw( W2Dolf : '0 :  ' );
252200160414           endif;
252300160414
252400160414         EndIf;
252500160414
252600160414         If  V2DdliM = *zero  and  V2DdlfM = *zero;
252700160414
252800160414           // -?Data/Ora iniziale Picking (Movimento)?
252900160414           if  W2DdliM > *zero;
253000160414             reset  WLBdat;
253100160414             G08inv = W2DdliM;
253200160414             XSRDA8 ( WLBdat );
253300160414             V2DdliM = G08dat;
253400160414             V2DoliM = %editw( W2DoliM : '0 :  ' );
253500160414           endif;
253600160414
253700160414           // -?Data/Ora finale Picking (Movimento)?
253800160414           if  W2DdlfM > *zero;
253900160414             reset  WLBdat;
254000160414             G08inv = W2DdlfM;
254100160414             XSRDA8 ( WLBdat );
254200160414             V2DdlfM = G08dat;
254300160414             V2DolfM = %editw( W2DolfM : '0 :  ' );
254400160414           endif;
254500160414
254600160414         EndIf;
254700160408
254800160408       ENDSR;
254900160408
255000160408       //--------------------------------------------------------------
255100160408       //?Elaborazione Attività Smistamento per "FISSI".               ?
255200160408       //--------------------------------------------------------------
255300160408       BEGSR  sr_ElabFNMSA_I;
255400160408
255500160408         // -?Ciclo di lettura dati del foglio da caricare?
255600170201         clear  keyFNMSA02;
255700160711         //*k_MSAapl  = 'I';
255800160711         k_MSAapl  = dDRM.§DRMaFiss;
255900160408         k_MSAfil  = V1Cfil;
256000160411         k_MSAdfv  = WWWdfv;
256100160408         k_MSAnpg  = V1Cnpg;
256200160411         k_MSAdtl  = WWWdli;
256300160408         k_MSAorl  = V1Coli;
256400170201         setll  %kds( keyFNMSA02 )  FNMSA000;
256500170201         reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
256600160408
256700170201         //*·DoW  Not %eof(FNMSA02L);
256800170201         DoW  Not %eof(FNMSA02L)  and
256900161107              ( ( MSAdtl * 10000 ) + MSAorl ) <=
257000161107              ( ( WWWdlf * 10000 ) + V1Colf );
257100160408
257200160408           Select;
257300160408
257400160408             // -?ARRIVI?
257500160408             When  MSAtcl <> 999;
257600160408
257700160408               // -?Colli lavorati?
257800160408               V2DtcpI  += MSApep001 +
257900160408                           MSApep002 +
258000160408                           MSApep003 +
258100160408                           MSApep004 +
258200160408                           MSApep005 +
258300160408                           MSApep006 +
258400160408                           MSApep007 +
258500160408                           MSApep008 +
258600160408                           MSApep009 +
258700160408                           MSApep010 +
258800160408                           MSApep011 +
258900160408                           MSApep012 +
259000160408                           MSApep013 +
259100160408                           MSApep014 +
259200160408                           MSApep015 +
259300160408                           MSApep016 +
259400160408                           MSApep017 +
259500160408                           MSApep018 +
259600160408                           MSApep019 +
259700160408                           MSApep020;    // -?ANCHE post.20!?
259800160408
259900160408             // -?PICKING (MOVIMENTO)?
260000160408             When  MSAtcl =  999;
260100160408
260200160408               // -?Colli lavorati?
260300160408               V2DtcpIM += MSApep001 +
260400160408                           MSApep002 +
260500160408                           MSApep003 +
260600160408                           MSApep004 +
260700160408                           MSApep005 +
260800160408                           MSApep006 +
260900160408                           MSApep007 +
261000160408                           MSApep008 +
261100160408                           MSApep009 +
261200160408                           MSApep010 +
261300160408                           MSApep011 +
261400160408                           MSApep012 +
261500160408                           MSApep013 +
261600160408                           MSApep014 +
261700160408                           MSApep015 +
261800160408                           MSApep016 +
261900160408                           MSApep017 +
262000160408                           MSApep018 +
262100160408                           MSApep019 +
262200160408                           MSApep020;    // -?ANCHE post.20!?
262300160408
262400160408           EndSl;
262500160408
262600160408           // -?Colli Lavorati per Postazione?
262700160408           sk_Ncl_Incomp(01) += MSApep001;
262800160408           sk_Ncl_Incomp(02) += MSApep002;
262900160408           sk_Ncl_Incomp(03) += MSApep003;
263000160408           sk_Ncl_Incomp(04) += MSApep004;
263100160408           sk_Ncl_Incomp(05) += MSApep005;
263200160408           sk_Ncl_Incomp(06) += MSApep006;
263300160408           sk_Ncl_Incomp(07) += MSApep007;
263400160408           sk_Ncl_Incomp(08) += MSApep008;
263500160408           sk_Ncl_Incomp(09) += MSApep009;
263600160408           sk_Ncl_Incomp(10) += MSApep010;
263700160408           sk_Ncl_Incomp(11) += MSApep011;
263800160408           sk_Ncl_Incomp(12) += MSApep012;
263900160408           sk_Ncl_Incomp(13) += MSApep013;
264000160408           sk_Ncl_Incomp(14) += MSApep014;
264100160408           sk_Ncl_Incomp(15) += MSApep015;
264200160408           sk_Ncl_Incomp(16) += MSApep016;
264300160408           sk_Ncl_Incomp(17) += MSApep017;
264400160408           sk_Ncl_Incomp(18) += MSApep018;
264500160408           sk_Ncl_Incomp(19) += MSApep019;
264600160408           sk_Ncl_Incomp(20) += MSApep020;
264700160408
264800170201           reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
264900160408
265000160408         EndDo;
265100160408
265200160408         // -?Calcolo numero pagine visualizzabili?
265300160408         clear  wPag_T;
265400160408         select;
265500160408           when  ( V2DtcpI + V2DtcpIM ) = *zero;
265600160408             wPag_T = 1;
265700160408           when  sk_MSL_Pep(11) <> *blank;
265800160408             wPag_T = 3;
265900160408           other;
266000160408             wPag_T = 2;
266100160408         endsl;
266200160408
266300160729         // -?Impostazione 1ª videata da emettere (D03 o D05)?
266400160729         //*clear  wPag_C;
266500160729         //*if  V1Cnpg = 2;
266600160408           wPag_C = 1;
266700160729         //*endif;
266800160408
266900160408       ENDSR;
267000160322
267100160322       //--------------------------------------------------------------
267200160322       //?Elaborazione Statistiche Arrivi.                             ?
267300160322       //--------------------------------------------------------------
267400160322       BEGSR  sr_ElabFISAR;
267500160322
267600160322         // -?Calcolo TERMINAL?
267700160323
267800160729         //*$NoFISAR = *on;
267900160322
268000160322         // -?Tipo Rec. "Normale"?
268100160322         clear  keyFISAR06;
268200160322         k_SARtfa = V1Cfil;
268300160411         k_SARdre = WWWdfv;
268400160322         k_SARlna = k_SARtfa;
268500160322         chain  %kds( keyFISAR06 )  FISAR000;
268600160322         if  %found(FISAR06L);
268700160729           //*$NoFISAR = *off;
268800160408           V2Dter   = SARcll;
268900160322         endif;
269000160322
269100160322         // -?Tipo Rec. "C/Servizio"?
269200160322         k_SARtre = 'C';
269300160322         chain  %kds( keyFISAR06 )  FISAR000;
269400160322         if  %found(FISAR06L);
269500160729           //*$NoFISAR = *off;
269600160408           V2Dter  += SARcll;
269700160322         endif;
269800160322
269900160322         // -?Tipo Rec. "Transito"?
270000160322         k_SARtre = 'T';
270100160322         chain  %kds( keyFISAR06 )  FISAR000;
270200160322         if  %found(FISAR06L);
270300160729           //*$NoFISAR = *off;
270400160408           V2Dter  += SARcll;
270500160322         endif;
270600160322
270700160322         //// -?Tipo Rec. "Solo Terminal"?
270800160322         //k_SARtre = 'R';
270900160322         //chain  %kds( keyFISAR06 )  FISAR000;
271000160322         //if  %found(FISAR06L);
271100160729         //  //*$NoFISAR = *off;
271200160408         //  V2Dter  += SARcll;
271300160322         //endif;
271400160322
271500160322       ENDSR;
271600160322
271700160322       //--------------------------------------------------------------
271800161028       //?Elaborazione Statistiche Partenza a Terra.                   ?
271900160322       //--------------------------------------------------------------
272000160322       BEGSR  sr_ElabFISAT;
272100160411
272200160411         // -?Reperimento Filiali dell'Area ("£1")?
272300160411         clear  sk_L1;
272400160411         clear  KPJBU;
272500160411         clear  TRUL06ds;
272600160411         D06cod = '£1';
272700160411         D06key = %editc( V1Cfil : 'X' ) + '    S';
272800160411         D06drf = WWWdfv;
272900160411         KPJBU  = TRUL06ds;
273000160411         trul06r ( KPJBA );
273100160411         TRUL06ds = KPJBU;
273200160411         ds_L1 = D06lia;
273300160322
273400160322         // -?Calcolo METRI CUBI in IMP?
273500160323
273600160729         //*$NoFISAT = *on;
273700160411
273800160411         clear  keyFISAT02;
273900160411         k_SATfle = V1Cfil;
274000160415         //k_SATtfa = *zero;       ?(già così)?
274100160415         //k_SATlnp = sk_L1(xx);   ?(no: dev'essere vuoto - per i tot.)?
274200160411         k_SATdre = WWWdfv;
274300160323
274400160323         // -?Tipo Rec. "Totale Partenze" (tipo rec. " ")?
274500160411         //k_SATtre = *blank;
274600160411         For  xx = 1  To  %elem( sk_L1 );
274700160411
274800160411           if  sk_L1(xx) = *zero;
274900160411             leave;
275000160411           endif;
275100160411
275200160411           k_SATlnp = sk_L1(xx);
275300160411           chain  %kds( keyFISAT02 )  FISAT000;
275400160411           If  %found( FISAT02L );
275500160729             //*$NoFISAT = *off;
275600170410             V2Dm3i  += SATvne +
275700170410                        SATvnd +
275800170410                        SATvnc +
275900170410                        SATvnh +
276000170410                        SATvnx;
276100160411           EndIf;
276200160411
276300160411         EndFor;
276400160323
276500160323         // -?Tipo Rec. "C/Servizi" (tipo rec. "C")?
276600160411         k_SATtre = 'C';
276700160411         For  xx = 1  To  %elem( sk_L1 );
276800160411
276900160411           if  sk_L1(xx) = *zero;
277000160411             leave;
277100160411           endif;
277200160411
277300160411           k_SATlnp = sk_L1(xx);
277400160411           chain  %kds( keyFISAT02 )  FISAT000;
277500160411           If  %found( FISAT02L );
277600160729             //*$NoFISAT = *off;
277700170410             V2Dm3i  += SATvne +
277800170410                        SATvnd +
277900170410                        SATvnc +
278000170410                        SATvnh +
278100170410                        SATvnx;
278200160411           EndIf;
278300160411
278400160411         EndFor;
278500160323
278600160323         // -?Tipo Rec. "Transiti" (tipo rec. "T")?
278700160323         k_SATtre = 'T';
278800160411         For  xx = 1  To  %elem( sk_L1 );
278900160411
279000160411           if  sk_L1(xx) = *zero;
279100160411             leave;
279200160411           endif;
279300160411
279400160411           k_SATlnp = sk_L1(xx);
279500160411           chain  %kds( keyFISAT02 )  FISAT000;
279600160411           If  %found( FISAT02L );
279700160729             //*$NoFISAT = *off;
279800170410             V2Dm3i  += SATvne +
279900170410                        SATvnd +
280000170410                        SATvnc +
280100170410                        SATvnh +
280200170410                        SATvnx;
280300160411           EndIf;
280400160411
280500160411         EndFor;
280600160322
280700160323       ENDSR;
280800160318
280900160318       //--------------------------------------------------------------
281000160318       //?Calcola le ore e i minuti da minuti totali                   ?
281100160318       //?INPUT  --> §min: minuti totali trascorsi                     ?
281200160318       //?OUTPUT --> §gg:  giorni trascorsi                            ?
281300160318       //?           §hh:  ore    trascorse                            ?
281400160318       //?           §mm:  minuti trascorsi                            ?
281500160318       //--------------------------------------------------------------
281600160321       BEGSR  sr_CalcHML;
281700160318
281800160318         clear  wResto;
281900160318         if  §min >= 1440;     // -?i mm formano più di 1 gg?
282000160318           §gg    = %div( §min : 1440 );      // -?giorni?
282100160318           wResto = %rem( §min : 1440 );      // -?ore/minuti di resto?
282200160318         else;                 // -?i mm non formano gg?
282300160318           wResto = §min;                     // -?ore/minuti?
282400160318         endif;
282500160318
282600160318         §hh = %div( wResto : 60 );      // -?ore lavorate?
282700160318         §mm = %rem( wResto : 60 );      // -?minuti di resto?
282800160318
282900160318       ENDSR;
283000160322
283100160322       //--------------------------------------------------------------
283200160322       //?Richiamo *pgm Letture Smistacolli.                           ?
283300160322       //--------------------------------------------------------------
283400160322       BEGSR  sr_CallFIMS08;
283500160322
283600160322         // -?Richiesta categoria "specifica"?
283700170202         clear  MS13W01;
283800160526         If  V1Capl = 'V'  and  V1Cnpg = 2;
283900160322           exfmt  MS13W01;
284000160322           if  dsp_aid = c_F12;
284100160322             leavesr;
284200160322           endif;
284300160322         EndIf;
284400160322
284500160322         // -?Impostazione dati nella DS-parametri?
284600160322         clear  FIMS00ds;
284700160322         //clear  D00op0;      //?- livello procedura?
284800160322         //clear  D00op1;      //?- livello programma?
284900160322         D00f03 = *off;        //?- premuto F03?
285000160322         D00f12 = *off;        //?- premuto F12?
285100160322         D00err = *off;        //?- errore?
285200160322         //clear  D00msg;      //?- messaggio ritorno?
285300160322         D00fil = V1Cfil;      //?- filiale?
285400170202         //*·if  $Video = 'D5'  or  $Video = 'D4'  or  V1Capl <> 'V';
285500170202         if  V1Capl <> 'V'  or  V1Cnpg = 5;
285600160414           D00apl = V1Capl;    //?- tipo applicazione?
285700160414         else;
285800160414           D00apl = W1Capl;    //?- tipo applicazione?
285900160414         endif;
286000160411         D00dfv = WWWdfv;      //?- data foglio?
286100160322         D00npg = V1Cnpg;      //?- categoria foglio?
286200160322         D00mtd = 1;           //?- min totali dettaglio?
286300160322         D00mtr = 30;          //?- min totali riepilogo?
286400160322         select;
286500170202           //*·when  $Video <> 'D5'  and  $Video <> 'D4'  and
286600170202           //*·      W1Capl =  'V'   and  W1Cnpg =  'A';
286700170202           when  V1Cnpg = 2    and
286800170202                 W1Capl = 'V'  and  W1Cnpg = 'A';
286900160322             D00dli = W2Ddli;
287000160414             D00oli = W2Doli;
287100160322             D00dlf = W2Ddlf;
287200160414             D00olf = W2Dolf;
287300170202           //*·when  $Video <> 'D5'  and  $Video <> 'D4'  and
287400170202           //*·      W1Capl =  'V'   and  W1Cnpg =  'M';
287500170202           when  V1Cnpg = 2     and
287600170202                 W1Capl = 'V'  and  W1Cnpg = 'M';
287700160322             D00dli = W2DdliM;
287800160414             D00oli = W2DoliM;
287900160322             D00dlf = W2DdlfM;
288000160414             D00olf = W2DolfM;
288100170202           //*·when  $Video =  'D5'  or   $Video =  'D4';
288200170202           when  V1Cnpg = 5;
288300160414             D00dli = W2Ddli;
288400160414             D00oli = W2Doli;
288500160414             D00dlf = W2Ddlf;
288600160414             D00olf = W2Dolf;
288700161107           when  WWWdli > *zero  or  WWWdlf > *zero;
288800160411             D00dli = WWWdli;
288900160411             D00oli = V1Coli;
289000161107             //*·if  W2Ddlf >= W2DdlfM;
289100161107             //*·  D00dlf = W2Ddlf;
289200161107             //*·else;
289300161107             //*·  D00dlf = W2DdlfM;
289400161107             //*·endif;
289500161107             //*·D00olf = 2359;
289600161107             D00dlf = WWWdlf;
289700161107             D00olf = V1Colf;
289800160322         endsl;
289900160322
290000160322         // -?Richiamo *pgm?
290100160322         FIMS08R ( kpjba : FIMS00ds );
290200160322
290300160322         // -?Verifica uscita?
290400160322         select;
290500160322           // ·?Premuto F3: ritorno a D01?
290600160322           when  D00f03 = *on;
290700160322             $Video = 'D1';
290800160322           // ·?Premuto F12: si rimane in D02/D03?
290900160322           when  D00f12 = *on;
291000160322           // ·?Restituito errore: lo si visualizza?
291100160322           when  D00msg <> *blank;
291200160322             VIDmsg = D00msg;
291300160322             $ErrMessage  = *on;
291400160322             $ErrGenerico = *on;
291500160322         endsl;
291600160322
291700160322       ENDSR;
291800160412
291900160412       //--------------------------------------------------------------
292000160412       //?Preparazione stringa SQL per estrazione dati da FIPSL00F.    ?
292100160412       //--------------------------------------------------------------
292200160412       BEGSR  sr_PrepSQL;
292300160412
292400160415         wSQL = 'with FIPSL as ( select PSLnpg, PSLbarCo, PSLusr, +
292500160415                                        max(PSLdtOraR) as Data_Ora_Rilev +
292600160415                                   from FIPSL00F +
292700160415                                  where PSLnpg = ' + %editc( V1Cnpg : 'X' ) +
292800160415                                  ' and PSLnfv in (' + wNFV_x_SQL + ') +
292900160415                                    and PSLfil = ' + %editc( V1Cfil : 'X' ) +
293000161215                                  ' and PSLdtOraR between ''' +
293100161215                                        %editc( WWWdli : 'X' ) +
293200161215                                        %editc( V1Coli : 'X' ) + '00'' +
293300161215                                                  and ''' +
293400161215                                        %editc( WWWdlf : 'X' ) +
293500161215                                        %editc( V1Colf : 'X' ) + '99'' +
293600161215                               group by PSLnpg, PSLbarCo, PSLusr +
293700160415                               order by PSLnpg, PSLbarCo, PSLusr +
293800160413                               ) +
293900160413
294000160414               select PSLusr, count(*) +
294100160413                 from FIPSL +
294200160414             group by PSLusr +
294300160413
294400160414             order by PSLusr +
294500160413
294600160413                  for fetch only';
294700160412
294800160412       ENDSR;
294900160412
295000160412       //--------------------------------------------------------------
295100160412       //?Apertura cursore.                                            ?
295200160412       //--------------------------------------------------------------
295300160412       BEGSR  sr_OpenCursor;
295400160412
295500160412         // -?Dichiarazione cursore?
295600160412         exec sql   prepare S6   from :wSQL;
295700160412         exec sql   declare C6   cursor for S6;
295800160412
295900160412         if  SQLcode < *zero;
296000160412           $EoF  = *on;
296100160412           exsr  sr_PrintERR;
296200160412         endif;
296300160412
296400160412         // -?Apertura del cursore?
296500160412         exec sql   open C6;
296600160412
296700160412         if  SQLcode < *zero;
296800160412           $EoF  = *on;
296900160412           exsr  sr_PrintERR;
297000160412         endif;
297100160412
297200160412       ENDSR;
297300160412
297400160412       //--------------------------------------------------------------
297500160412       //?Chiusura cursore.                                            ?
297600160412       //--------------------------------------------------------------
297700160412       BEGSR  sr_CloseCursor;
297800160412
297900160412         // -?Chiusura del cursore?
298000160412         exec sql   close C6;
298100160412
298200160412       ENDSR;
298300160412
298400160412       //--------------------------------------------------------------
298500160412       //?Lettura cursore.                                             ?
298600160412       //--------------------------------------------------------------
298700160412       BEGSR  sr_ReadCursor;
298800160412
298900160412         $EoF  = *off;
299000160412         clear  ds_FIPSL6;
299100160412
299200160412         exec sql   fetch next   from C6   into :ds_FIPSL6;
299300160412
299400160412         select;
299500160412           when  SQLcode = 100;
299600160412             $EoF  = *on;
299700160412           when  SQLcode < *zero;
299800160412             $EoF  = *on;
299900160412             exsr  sr_PrintERR;
300000160412         endsl;
300100160412
300200160412       ENDSR;
300300160413
300400160413       //--------------------------------------------------------------
300500160413       //?Preparazione stringa SQL per estrazione dett. da FIPSL00F.   ?
300600160413       //--------------------------------------------------------------
300700160413       BEGSR  sr_PrepSQL7;
300800160413
300900160415         wSql7 = 'with FIPSL as ( select PSLnpg, PSLbarCo, PSLusr, +
301000160415                                         max(PSLdtOraR) as Data_Ora_Rilev +
301100160413                                    from FIPSL00F +
301200160413                                   where PSLnpg = ' + %editc( V1Cnpg : 'X' ) +
301300160413                                   ' and PSLnfv in (' + wNFV_x_SQL + ') +
301400161215                                     and PSLfil = ' + %editc( V1Cfil : 'X' ) +
301500161215                                   ' and PSLdtOraR between ''' +
301600161215                                         %editc( WWWdli : 'X' ) +
301700161215                                         %editc( V1Coli : 'X' ) + '00'' +
301800161215                                                   and ''' +
301900161215                                         %editc( WWWdlf : 'X' ) +
302000161215                                         %editc( V1Colf : 'X' ) + '99''';
302100160413
302200160413         if  dsp_aid <> c_F10  and
302300160413             S06opz  =  '5';
302400160413           wSql7 +=                ' and PSLusr = ''' + H06usr + '''';
302500160413         endif;
302600160413
302700160415         wSql7 +=             ' group by PSLnpg, PSLbarCo, PSLusr +
302800160415                                order by PSLnpg, PSLbarCo, PSLusr +
302900160413                                ) +
303000161215
303100160415               select FIPSL00F.PSLnpg, FIPSL00F.PSLnfv, FIPSL00F.PSLbarCo, +
303200160415                      FIPSL00F.PSLusr, FIPSL00F.PSLdtOraR +
303300160415
303400160415                 from FIPSL00F inner join FIPSL +
303500160415                   on FIPSL00F.PSLnpg    = FIPSL.PSLnpg +
303600160415                  and FIPSL00F.PSLbarCo  = FIPSL.PSLbarCo +
303700160415                  and FIPSL00F.PSLusr    = FIPSL.PSLusr +
303800160415                  and FIPSL00F.PSLdtOraR = FIPSL.Data_Ora_Rilev +
303900160415
304000160415             order by PSLnpg, PSLbarCo, PSLdtOraR, PSLnfv, PSLusr +
304100160413
304200160413                  for fetch only';
304300160413
304400160413       ENDSR;
304500160413
304600160413       //--------------------------------------------------------------
304700160413       //?Apertura cursore.                                            ?
304800160413       //--------------------------------------------------------------
304900160413       BEGSR  sr_OpenCursor7;
305000160413
305100160413         // -?Dichiarazione cursore?
305200160413         exec sql   prepare S7   from :wSql7;
305300160413         exec sql   declare C7   cursor for S7;
305400160413
305500160413         if  SQLcode < *zero;
305600160413           $EoF7 = *on;
305700160413           exsr  sr_PrintERR;
305800160413         endif;
305900160413
306000160413         // -?Apertura del cursore?
306100160413         exec sql   open C7;
306200160413
306300160413         if  SQLcode < *zero;
306400160413           $EoF7 = *on;
306500160413           exsr  sr_PrintERR;
306600160413         endif;
306700160413
306800160413       ENDSR;
306900160413
307000160413       //--------------------------------------------------------------
307100160413       //?Chiusura cursore.                                            ?
307200160413       //--------------------------------------------------------------
307300160413       BEGSR  sr_CloseCursor7;
307400160413
307500160413         // -?Chiusura del cursore?
307600160413         exec sql   close C7;
307700160413
307800160413       ENDSR;
307900160413
308000160413       //--------------------------------------------------------------
308100160413       //?Lettura cursore.                                             ?
308200160413       //--------------------------------------------------------------
308300160413       BEGSR  sr_ReadCursor7;
308400160413
308500160413         $EoF7 = *off;
308600160413         clear  ds_FIPSL7;
308700160413
308800160413         exec sql   fetch next   from C7   into :ds_FIPSL7;
308900160413
309000160413         select;
309100160413           when  SQLcode = 100;
309200160413             $EoF7 = *on;
309300160413           when  SQLcode < *zero;
309400160413             $EoF7 = *on;
309500160413             exsr  sr_PrintERR;
309600160413         endsl;
309700160413
309800160413       ENDSR;
309900160412
310000160412       //--------------------------------------------------------------
310100160412       //?Stampa segnalazione dell'errore rilevato via SQL             ?
310200160412       //--------------------------------------------------------------
310300160412       BEGSR  sr_PrintERR;
310400160412
310500160412         // -?Stampa del Dump?
310600160412         Dump(A);
310700160412
310800160412         // -?Stampa del Job-Log?
310900160412         Qcmd = 'DSPJOBLOG job(*) output(*print)';
311000160412         exsr  sr_ExecCmd;
311100160412
311200160412         // -?Chiusura del programma?
311300160412         exsr  sr_RoutEnd;
311400160412
311500160412       ENDSR;
311600160412
311700160412       //--------------------------------------------------------------
311800160412       //?Esecuzione del comando (già impostato).                      ?
311900160412       //--------------------------------------------------------------
312000160412       BEGSR  sr_ExecCmd;
312100160412
312200160412         clear Qcap0100;
312300160412         Qcabcsdh = *off;
312400160412         Qcapa    = *off;
312500160412         Qcacmdss = *off;
312600160412         Qcaerved = *allX'00';
312700160412
312800160412         clear Qusec;
312900160412         Qusbprv  = %size(Qusec);
313000160412
313100160412         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
313200160412                           %size(Qcap0100) : 'CPOP0100' : *omit :
313300160412                           0 : 0 : Qusec);
313400160412
313500160412         //if  Qusei <> *blank;
313600160412         //  ...;
313700160412         //endif;
313800160412
313900160412       ENDSR;
314000160318
314100160318       //--------------------------------------------------------------
314200160318       //?Operazioni finali.                                           ?
314300160318       //--------------------------------------------------------------
314400160318       BEGSR  sr_RoutEnd;
314500160321
314600160321         // -?Chiusura funzioni precedentemente aperte?
314700160321         cloTabella ( c_Tntbe );
314800160413
314900160413         // -?Chiusura archivi?
315000170201         close  FNMSA02L;
315100160413         close  FISAR06L;
315200160413         close  FISAT02L;
315300160413         close  FNFVV02L;
315400160318
315500160318         // -?Uscita dal *pgm?
315600160318         return;
315700160318
315800160318       ENDSR;
315900160318
316000160318      /end-free
316100160318
316200160318       //--------------------------------------------------------------
316300160318       //?Definizione schiere a tempo di compilazione                  ?
316400160318       //--------------------------------------------------------------
316500160318
316600160318** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
316700160412Funzione eseguibile SOLO in ambiente di Filiale: premere "Invio" per uscire     1
316800160318Utente NON abilitato alla funzione                                              2
316900160318Filiale errata                                                                  3
317000160318Filiale NON di 1° livello                                                       4
317100160318Tipo Applicazione errato                                                        5
317200161107Data Foglio formalmente errata                                                  6
317300160318Categoria Foglio errata                                                         7
317400160408Data Lettura formalmente errata                                                 8
317500160408Ora Lettura formalmente errata                                                  9
317600160408Valore NON ammesso per il campo                                                10
317700160412Raggiunto il n° massimo delle spunte interrogabili a video                     11
317800160412Opzione errata                                                                 12
317900160415Rilevato errore durante l'estrazione dei dati: avvisare CED                    13
318000160711Filiale non prevista per il Report Magazzino                                   14
318100161107Orario di lettura iniziale successivo all'orario di lettura finale             15
