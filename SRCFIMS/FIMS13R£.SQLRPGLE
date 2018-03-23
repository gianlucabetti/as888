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
081000161028         if  V1Cfil <> SIMfel;
081100161028           VIDmsg = %trimR( sk_Msg(03) ) +
081200161028                    ': non in gestione all''utente';
081300161028           $PosCurFIL   = *on;
081400161028           $ErrMessage  = *on;
081500161028           $ErrGenerico = *on;
081600161028           if  Not  $InzD01;
081700161028             leavesr;
081800161028           endif;
081900161028         endif;
082000160711
082100160711         // -?Ricerca/Controllo/Decodifica Tipo Applicazione?
082200160711         clear  V1Dapl;
082300160711         //*// -?Ricerca?
082400160711         //*if  %scan( '?' : V1Capl ) > *zero;
082500160711         //*  reset  TIBS02ds;
082600160711         //*  //T02mod = 'R';    ?(già così)?
082700160711         //*  //T02cod = 'MTP';  ?(già così)?
082800160711         //*  TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
082900160711         //*  if  T02err = *blank;
083000160711         //*    V1Capl = T02ke1;
083100160711         //*  endif;
083200160711         //*endif;
083300160711         // -?Reperimento Tipo Applicazione per fil. da tab. "DRM"?
083400160711         clear  dDRM;
083500160711         if  getTabella ( c_Tntbe : 'DRM' : %editc( V1Cfil : 'X' ) :
083600160711                          *blank :
083700160711                          *omit : *omit :
083800160711                          *omit : *omit :
083900160711                          *omit : *omit : *omit : *omit :
084000160711                          *omit : *omit : *omit : *omit :
084100160711                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
084200160711                          = *zero      AND
084300160711             ds_TNTBE.TBEatb = *blank;
084400160711           dDRM   = ds_TNTBE.TBEuni;
084500161216           if  V1Cnpg = 5;
084600161216             V1Capl = dDRM.§DRMaFv5;
084700161017           else;
084800161216             V1Capl = dDRM.§DRMaFv2;
084900161017           endif;
085000160711         else;
085100160711           VIDmsg = sk_Msg(14);
085200160711           $PosCurFIL   = *on;
085300160711           $ErrMessage  = *on;
085400160711           $ErrGenerico = *on;
085500160711           if  Not  $InzD01;
085600160711             leavesr;
085700160711           endif;
085800160711         endif;
085900160711         // -?Controllo/Decodifica?
086000160711         clear  dMTP;
086100160711         if  getTabella ( c_Tntbe : 'MTP' : V1Capl : *blank :
086200160711                          *omit : *omit :
086300160711                          *omit : *omit :
086400160711                          *omit : *omit : *omit : *omit :
086500160711                          *omit : *omit : *omit : *omit :
086600160711                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
086700160711                          = *zero      AND
086800160711             ds_TNTBE.TBEatb = *blank;
086900160711           dMTP   = ds_TNTBE.TBEuni;
087000160711           V1Dapl = dMTP.§MTPdes;
087100160711         else;
087200160711           //*VIDmsg = sk_Msg(05);
087300160711           //*$PosCurAPL   = *on;
087400160711           //*$ErrMessage  = *on;
087500160711           //*$ErrGenerico = *on;
087600160711           //*if  Not  $InzD01;
087700160711           //*  leavesr;
087800160711           //*endif;
087900160711           V1Dapl = *all'? ';
088000160711         endif;
088100160711
088200160711       ENDSR;
088300160318
088400160318       //--------------------------------------------------------------
088500160413       //?Gestione videate D02/D03/D04/D05.                            ?
088600160318       //--------------------------------------------------------------
088700160413       BEGSR  sr_GesD030405;
088800160318
088900160318         // -?Inizializzazione videata?
089000160318         if  $InzD02 = *on;
089100161216           exsr  sr_InzD02;
089200160413           exsr  sr_InzD030405;
089300160318           $InzD02 = *off;
089400160318         endif;
089500160408
089600160413         // -?(Dis)Attivazione tasti funzionali a video?
089700160711         $F5Attivo   = *off;
089800161028         $F8Attivo   = ( %subst( kNmUs : 1 : 3 ) = 'EDP' );
089900160930         //*·$F9Attivo   = ( V1Cnpg = 2 );
090000160930         $F9Attivo   = *on;
090100160408         $F12Attivo  = *on;
090200160408         $RlUpAttivo = *off;
090300160408         $RlDnAttivo = *off;
090400160729         if  ( $Video  = 'D3'  or  $Video = 'D5' )  and
090500160729             ( V2DtcpI + V2DtcpIM ) > *zero;
090600160408           $RlUpAttivo = *on;
090700160408         endif;
090800160412         if  $Video = 'D4';
090900160408           if  wPag_C < wPag_T;
091000160408             $RlUpAttivo = *on;
091100160408           endif;
091200160408           $RlDnAttivo = *on;
091300160408         endif;
091400160318
091500160318         // -?Emissione Testata e Piede con tasti funzionali abilitati?
091600160318         write  MS13T01;
091700160318         write  MS13P01;
091800160318
091900160318         // -?Emissione videata?
092000160408         write  MS13D02;
092100160412
092200160408         select;
092300160412
092400160408           // -?Arrivi - 1ª videata?
092500160412           when  V1Cnpg = 2  and  $Video = 'D3';
092600160412             exfmt  MS13D03;
092700160412
092800160729           // -?Arrivi/Partenze - 2ª videata?
092900160729           //*when  V1Cnpg = 2  and  $Video = 'D4';
093000160729           when  $Video = 'D4';
093100160412             exfmt  MS13D04;
093200160412
093300160729           // -?Partenze - 1ª videata?
093400160729           when  V1Cnpg = 5  and  $Video = 'D5';
093500160412             exfmt  MS13D05;
093600160412
093700160408           // -?? ? ??
093800160408           other;
093900160408             $Fine = *on;
094000160408             leavesr;
094100160412
094200160408         endsl;
094300160318
094400160321         clear  VIDmsg;
094500160318         reset  $ErrMessage;
094600160318         reset  $ErrGenerico;
094700160318
094800160318         Select;
094900160318
095000160318           // -?F3=Fine?
095100160318           When  dsp_aid = c_F03;
095200160318             $Fine  = *on;
095300160318
095400160318           // -?F12=Ritorno?
095500160318           When  dsp_aid = c_F12;
095600160318             $Video = 'D1';
095700160322
095800160322           // -?F8=Dettaglio Letture VdL/CML/DiskC?
095900160322           When  dsp_aid = c_F08;
096000160322             exsr  sr_CallFIMS08;
096100160412
096200160729           // -?F9=Spunte Manuali?
096300160412           When  dsp_aid = c_F09;
096400160412             $VideoPrec  = $Video;
096500160412             $Video      = 'S6';
096600160412             $InzS06     = *on;
096700160408
096800160408           // -?Roll-Up?
096900160729           When  dsp_aid  = c_RollUp  and  wPag_C < wPag_T;
097000160412             $Video  = 'D4';
097100160408             wPag_C += 1;
097200161020             //*·exsr  sr_Set_NrPag;
097300160412             exsr  sr_InzD04;
097400160408
097500160408           // -?Roll-Down?
097600160729           When  $Video  = 'D4'       and
097700160408                 dsp_aid = c_RollDown;
097800160408             select;
097900160729               when  wPag_C = 2;
098000160729                 if  V1Cnpg = 2;
098100160729                   $Video = 'D3';
098200160729                 else;
098300160729                   $Video = 'D5';
098400160729                 endif;
098500160411                 wPag_C -= 1;
098600160411                 exsr  sr_Set_NrPag;
098700160408               when  wPag_C > 2;
098800160412                 $Video  = 'D4';
098900160408                 wPag_C -= 1;
099000161020                 //*·exsr  sr_Set_NrPag;
099100160412                 exsr  sr_InzD04;
099200160408             endsl;
099300160318
099400160318         EndSl;
099500160318
099600160318       ENDSR;
099700161216
099800161216       //--------------------------------------------------------------
099900161216       //?Inizializzazione videate D02.                                ?
100000161216       //--------------------------------------------------------------
100100161216       BEGSR  sr_InzD02;
100200161216
100300161216         // -?Spegnimento degli indicatori di errore?
100400161216         %subst( IndDspF : 50 ) = *off;
100500161216
100600161216         // -?Pulizia videate?
100700161216         //clear  MS13D02;    ?- NO: contiene campi di D01!?
100800161216
100900161216         // -?Decodifica Categoria Foglio (in D01)?
101000161216         select;
101100161216           when  V1Cnpg = 2;
101200161216             V2Dnpg = 'Arrivi';
101300161216           when  V1Cnpg = 3;
101400170110             //*·V2Dnpg = 'Inventario';
101500170110             V2Dnpg = 'IMA';
101600161216           when  V1Cnpg = 4;
101700161216             V2Dnpg = 'Consegne';
101800161216           when  V1Cnpg = 5;
101900161216             V2Dnpg = 'Entrata';
102000161216         endsl;
102100161216
102200161216         // -?Richiesti "Solo minuti con colli validi"?
102300161216         //  ?(e range di date di lettura)?
102400161216         clear  V2Dsmv;
102500161216         select;
102600161216           when  V1Csmv = 'S'  and  ( V1Cdli + V1Cdlf )  = *zero;
102700161216             evalR V2text = c_Color_Red + '(Solo minuti con colli validi)';
102800161216           when  ( V1Cdli + V1Cdlf ) <> *zero;
102900161216             V2text = 'Letture     dalle ore' + c_DspAtr_HI
103000161216                                              + %editw( V1Coli : '0 :  ' )
103100161216                                              + c_DspAtrNorm +
103200161216                                       ' del' + c_DspAtr_HI
103300161216                                              + %editc( V1Cdli : 'Y' )
103400161216                                              + c_DspAtrNorm +
103500161216                               '    alle ore' + c_DspATR_HI
103600161216                                              + %editw( V1Colf : '0 :  ' )
103700161216                                              + c_DspAtrNorm +
103800161216                                       ' del' + c_DspAtr_HI
103900161216                                              + %editc( V1Cdlf : 'Y' )
104000161216                                              + c_DspAtrNorm;
104100161216         endsl;
104200161219
104300161219         // -?Caricamento F.V. da elaborare?
104400161219         //  ?- NON più solo per categoria 2=Arrivi -?
104500161219         exsr  sr_Intab_FV_Elab;
104600161216
104700161216       ENDSR;
104800160318
104900160318       //--------------------------------------------------------------
105000161216       //?Inizializzazione videate D03/D04/D05.                        ?
105100160318       //--------------------------------------------------------------
105200160413       BEGSR  sr_InzD030405;
105300160318
105400160318         // -?Spegnimento degli indicatori di errore?
105500160413         %subst( IndDspF : 50 ) = *off;
105600160318
105700160408         // -?Pulizia videate?
105800160412         clear  MS13D03;
105900160412         clear  MS13D04;
106000160412         clear  MS13D05;
106100160321
106200160321         clear  wMinLav;
106300160321         clear  wMinLavM;
106400160321         clear  wMINxNCL_ob;
106500160321         clear  wMINxNCL_obM;
106600160321         clear  wNcl;
106700160321         clear  wNclM;
106800160722         clear  wNcl_K;
106900160722         clear  wNclM_K;
107000160321         clear  W2Ddli;
107100160321         clear  W2DdliM;
107200160321         clear  W2Ddlf;
107300160321         clear  W2DdlfM;
107400160408         clear  wPag_C;
107500160408         clear  wPag_T;
107600160408         clear  sk_Ncl_Incomp;
107700160411
107800160411         // -?Reperimento Tab. "MSL" (VDL - Layout Postazioni)?
107900160411         exsr  sr_Tab_MSL;
108000160318
108100160318         // -?Reperimento Obiettivi di Produttività?
108200160318         //  ?di un Impianto Automatico Smistacolli?
108300160318         clear  dKPI;
108400160321         if  getTabella ( c_Tntbe : 'KPI' : %editc( V1Cnpg : 'X' ) : V1Capl :
108500160318                          *omit : *omit :
108600160318                          *omit : *omit :
108700160318                          *omit : *omit : *omit : *omit :
108800160318                          *omit : *omit : *omit : *omit :
108900160318                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
109000160318                          = *zero      AND
109100160318               ds_TNTBE.TBEatb = *blank;
109200160318           dKPI   = ds_TNTBE.TBEuni;
109300160318         endif;
109400160318
109500160322         // -?Lettura Attività Smistamento del foglio da caricare?
109600160408         // ·?Tipo Applicazione "V" (o quella a video)?
109700160322         exsr  sr_ElabFNMSA;
109800160722         // ·?Tipo Applicazione "P" (RPVP)?
109900160408         exsr  sr_ElabFNMSA_P;
110000160411         // ·?Tipo Applicazione "I" (Fissi/Incompatibili)?
110100160408         exsr  sr_ElabFNMSA_I;
110200160729         if  V1Cnpg = 5  and  V2DtcpIm > *zero;
110300160729           V2DtcpI += V2DtcpIm;
110400160729         endif;
110500160321
110600160411         // -?Lettura Statistica Arrivi?
110700160411         //  ?della data del foglio da caricare?
110800160411         if  V1Cnpg = 2;
110900160411           exsr  sr_ElabFISAR;
111000160411         endif;
111100160322
111200160411         // -?Lettura Statistica Partenze A Terra?
111300160411         //  ?della data del foglio da caricare?
111400160411         if  V1Cnpg = 5;
111500160411           exsr  sr_ElabFISAT;
111600160411         endif;
111700160411
111800160411         // -?Impostazione del numero pagina da visualizzare?
111900160411         exsr  sr_Set_NrPag;
112000160318
112100160318       ENDSR;
112200160412
112300160412       //--------------------------------------------------------------
112400160412       //?Preparazione videata D04.                                    ?
112500160412       //--------------------------------------------------------------
112600160412       BEGSR  sr_InzD04;
112700161020
112800161020         clear  MS13D04;
112900161020
113000161020         // -?Impostazione del numero pagina da visualizzare?
113100161020         exsr  sr_Set_NrPag;
113200160412
113300160412         // -?Calcolo della 1ª Postazione da visualizzare?
113400160412         select;
113500160412           when  wPag_C < 2;
113600160412             leavesr;
113700160412           when  wPag_C = 2;
113800160412             clear  xP;
113900160412           when  wPag_C > 2;
114000160412             xP = 10;
114100160412         endsl;
114200160412
114300160412         // -?Ciclo di caricamento dei dati da visualizzare?
114400160412         //  ?(per max 10 postazioni)?
114500160412         For  xx = 1  To 10;
114600160412
114700160412           xP += 1;
114800160412           exsr  sr_RepPostFissa;
114900160412
115000160412           select;
115100160412             when  wDesP = *blank  and  wNumP = *zero;
115200160412             when  xx  = 1;
115300160412               V2Dtc01 = wDesP;
115400160414               V2Ntc01 = wNumP;
115500160412             when  xx  = 2;
115600160412               V2Dtc02 = wDesP;
115700160414               V2Ntc02 = wNumP;
115800160412             when  xx  = 3;
115900160412               V2Dtc03 = wDesP;
116000160414               V2Ntc03 = wNumP;
116100160412             when  xx  = 4;
116200160412               V2Dtc04 = wDesP;
116300160414               V2Ntc04 = wNumP;
116400160412             when  xx  = 5;
116500160412               V2Dtc05 = wDesP;
116600160414               V2Ntc05 = wNumP;
116700160412             when  xx  = 6;
116800160412               V2Dtc06 = wDesP;
116900160414               V2Ntc06 = wNumP;
117000160412             when  xx  = 7;
117100160412               V2Dtc07 = wDesP;
117200160414               V2Ntc07 = wNumP;
117300160412             when  xx  = 8;
117400160412               V2Dtc08 = wDesP;
117500160414               V2Ntc08 = wNumP;
117600160412             when  xx  = 9;
117700160412               V2Dtc09 = wDesP;
117800160414               V2Ntc09 = wNumP;
117900160412             when  xx  = 10;
118000160412               V2Dtc10 = wDesP;
118100160414               V2Ntc10 = wNumP;
118200160412           endsl;
118300160412
118400160412         EndFor;
118500160412
118600160412       ENDSR;
118700161020
118800161020       //--------------------------------------------------------------
118900161020       //?Impostazione del n° di pagina da visualizzare in D03/D04/D05.?
119000161020       //--------------------------------------------------------------
119100161020       BEGSR  sr_Set_NrPag;
119200161020
119300161020         clear  V2Dpag;
119400161020         clear  V2Dseg;
119500161020
119600161020         if  wPag_T > 1;
119700161020           V2Dpag = 'Pag. ' + %editc( wPag_C : 'X' ) +
119800161020                    ' di '  + %editc( wPag_T : 'X' );
119900161020           if  wPag_C < wPag_T;
120000161020             V2Dseg = 'Segue...';
120100161020           else;
120200161020             V2Dseg = '   Fine.';
120300161020           endif;
120400161020         endif;
120500161020
120600161020       ENDSR;
120700160412
120800160412       //--------------------------------------------------------------
120900160413       //?Gestione subfile S06.                                        ?
121000160412       //--------------------------------------------------------------
121100160413       BEGSR  sr_GesS06;
121200160412
121300160413         // -?Inizializzazione subfile S06?
121400160413         if  $InzS06 = *on;
121500160413           exsr  sr_InzS06;
121600160413           $InzS06 = *off;
121700160413         endif;
121800160412
121900160413         // -?(Dis)Attivazione tasti funzionali a video?
122000160412         $F8Attivo   = *off;
122100160412         $F9Attivo   = *off;
122200160526         //*$F10Attivo  = ( $Video = 'S6'  and  kNmUs = 'EDPSM     ' );
122300160526         $F10Attivo  = ( $Video = 'S6'  and
122400170522                         %subst( SDSjob : 1 : 9 ) = 'P04685668' );
122500160412         $F12Attivo  = *on;
122600160412         $RlUpAttivo = *off;
122700160412         $RlDnAttivo = *off;
122800160412
122900160412         // -?Posizionamento cursore?
123000160413         //  ?C6CsrRrn contiene il numero di riga del SubFile su cui?
123100160413         //  ?era posizionato il cursore; impostandolo in C6RcdNbr?
123200160412         //  ?si visualizza la pagina che era visualizzata quando?
123300160412         //  ?l'utente ha premuto l'ultimo tasto.?
123400160413         if  C6CsrRrn > *zero;
123500160413           C6RcdNbr = C6CsrRrn;
123600160413         else;
123700160413           C6RcdNbr = 1;
123800160413           write  MS13D06;          //(no rec.)?
123900160413         endif;
124000160413
124100160413         // -?Emissione Testata e Piede con tasti funzionali abilitati?
124200160413         write  MS13T01;
124300160413         write  MS13D02;
124400160413         write  MS13Z06;
124500160413         write  MS13P02;
124600160412
124700160412         // -?Emissione videata?
124800160413         exfmt  MS13C06;
124900160412
125000160413         clear  VIDmsg;
125100160412         reset  $ErrMessage;
125200160412         reset  $ErrGenerico;
125300160412
125400160412         Select;
125500160412
125600160412           // -?F3=Fine?
125700160412           When  dsp_aid = c_F03;
125800160413             if  $Video  = 'S7';
125900160413               exsr  sr_CloseCursor7;
126000160413             endif;
126100160412             exsr  sr_CloseCursor;
126200160412             $Fine = *on;
126300160413
126400160413           // -?F10=Dettaglio BarCode (solo da C06)?
126500160413           When  dsp_aid = c_F10;
126600160413             $Video  = 'S7';
126700160413             $InzS07 = *on;
126800160412
126900160412           // -?F12=Ritorno?
127000160412           When  dsp_aid = c_F12;
127100160413             exsr  sr_CloseCursor;
127200160413             $Video = $VideoPrec;
127300160412
127400160412           // -?Roll-Up?
127500160413           When  dsp_aid = c_RollUp;
127600160412             // -?Verifica raggiungimento del limite di records?
127700160412             //  ?registrabili nel subfile (9999)?
127800160413             if  S06nrr = *hival;
127900160413               $ErrGenerico = *on;
128000160413               $ErrMessage  = *on;
128100160413               VIDmsg = sk_Msg(11);
128200160413               leavesr;
128300160413             endif;
128400160413             exsr sr_RollUpS06;
128500160412
128600160412           // -?SubFile vuoto?
128700160413           When  S06nrr = *zero;
128800160412
128900160413           // -?Invio (solo da C06)?
129000160413           Other;
129100160412             // -?Gestione opzioni?
129200160412             exsr  sr_OpzS06;
129300160412             if  $ErrGenerico;
129400160412               leavesr;
129500160412             endif;
129600160412
129700160412         EndSl;
129800160412
129900160412       ENDSR;
130000160412
130100160412       //--------------------------------------------------------------
130200160413       //?Inizializzazione subfile S06.                                ?
130300160412       //--------------------------------------------------------------
130400160412       BEGSR  sr_InzS06;
130500160413
130600161219         //*·// -?Caricamento F.V. da elaborare?
130700161219         //*·//  ?- NON più solo per categoria 2=Arrivi -?
130800161219         //*   ?(caricamento spostato nella subr. "sr_InzD02")?
130900161219         //*·exsr  sr_Intab_FV_Elab;
131000160412
131100160412         // -?Pulizia subfile?
131200160412         $SflDsp_N    = *on;
131300160412         $SflDspCtl_N = *on;
131400160412         write  MS13C06;
131500160412         $SflDspCtl_N = *off;
131600160412         $SflEnd      = *off;
131700160412
131800160412         $SflNxtChg   = *off;
131900160412
132000160412         clear  w_SflPag;
132100160412         clear  C6RcdNbr;
132200160412         clear  C6CsrRrn;
132300160412         clear  S06nrr;
132400160413         clear  VIDmsg;
132500160412         $ErrMessage  = *off;
132600160412         $ErrGenerico = *off;
132700160412
132800160412         // -?Spegnimento degli indicatori di errore?
132900160413         %subst( IndDspF : 50 ) = *off;
133000160412
133100160412         // -?Caricamento della 1ª pagina di dati del subfile?
133200160412         // -?Preparazione stringa SQL?
133300160412         exsr  sr_PrepSQL;
133400160412         // -?Apertura del cursore?
133500160412         exsr  sr_OpenCursor;
133600160412         // -?Lettura del file?
133700160412         exsr  sr_ReadCursor;
133800160412         DoW  Not $EoF  and  S06nrr < c_SflPag;
133900160412           exsr  sr_RollUpS06;
134000160412         EndDo;
134100160412
134200160412         // -?Visualizzazione del SFL (se ci sono dati)?
134300160412         $SflDsp_N = ( S06nrr <= *zero );
134400160412
134500160412         // -?Attivazione del SFLEND?
134600160412         $SflEnd   = ( $EoF = *on );
134700160412
134800160412         // -?Impaginazione subfile?
134900160412         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
135000160412         if  S06nrr > *zero;
135100160412           C6RcdNbr = 1;
135200160412           C6CsrRrn = 1;
135300160412         else;
135400160412           clear  C6RcdNbr;
135500160412         endif;
135600160412
135700160412       ENDSR;
135800160412
135900160412       //--------------------------------------------------------------
136000160412       //?Caricamento singola pagine SubFile S06.                      ?
136100160412       //--------------------------------------------------------------
136200160412       BEGSR  sr_RollUpS06;
136300160412
136400160412         S06nrr     = ( w_SflPag * c_SflPag );
136500160412         w_SflPag  += 1;
136600160412         $SflNxtChg = *off;
136700160412
136800160412         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
136900160412         DoW  $EoF   = *off    and
137000160412              S06nrr < *hival  and
137100160412              S06nrr < ( w_SflPag * c_SflPag );
137200160412
137300160412           // -?Caricamento dati nel record del subfile?
137400160412           exsr  sr_RieRecS06;
137500160412           S06nrr += 1;
137600160412           write  MS13S06;
137700160412
137800160412           // -?Lettura record successivo nell'archivio?
137900160412           exsr  sr_ReadCursor;
138000160412
138100160412         EndDo;
138200160412
138300160412         // -?Visualizzazione del SFL (se ci sono dati)?
138400160412         $SflDsp_N = ( S06nrr <= *zero );
138500160412
138600160412         // -?Attivazione (eventuale) del SFLEND?
138700160412         $SflEnd   = ( $EoF = *on );
138800160412
138900160412         // -?Posizionamento cursore al 1° rcd della pagina?
139000160412         if  S06nrr > *zero;
139100160412           wPag     = S06nrr / c_SflPag;
139200160412           wRig     = S06nrr - ( c_SflPag * wPag );
139300160412           C6RcdNbr = wPag * c_SflPag;
139400160412           if  wRig > *zero;
139500160412             C6RcdNbr = C6RcdNbr + 1;
139600160412           else;
139700160412             C6RcdNbr = C6RcdNbr - c_SflPag + 1;
139800160412           endif;
139900160412         else;
140000160412           clear  C6RcdNbr;
140100160412         endif;
140200160412
140300160412         C6CsrRrn = C6RcdNbr;
140400160412
140500160412       ENDSR;
140600160412
140700160412       //--------------------------------------------------------------
140800160412       //?Caricamento singolo record nel SubFile S06.                  ?
140900160412       //--------------------------------------------------------------
141000160412       BEGSR  sr_RieRecS06;
141100160412
141200160412         clear  MS13S06;
141300160412
141400160412         // -?Campi Hidden?
141500160412         // ·?Utente?
141600160413         H06usr = ds_FIPSL6.PSLusr;
141700160412
141800160412         // -?Campi Output?
141900160412         // ·?Utente?
142000160413         S06usr = %subst( ds_FIPSL6.PSLusr : 6 : %len(S06usr) );
142100160412         // ·?Numero BarCode letti?
142200160413         S06ncl = ds_FIPSL6.wNrBarCode;
142300160526
142400160526         // -?Attributi di Visualizzazione?
142500160526         // ·?Gestione Opzione + F10?
142600160526         $F10Attivo  = ( $Video = 'S6'  and
142700170522                         %subst( SDSjob : 1 : 9 ) = 'P04685668' );
142800160412
142900160412       ENDSR;
143000160412
143100160412       //--------------------------------------------------------------
143200160412       //?Gestione opzioni nel SubFile S06.                            ?
143300160412       //--------------------------------------------------------------
143400160412       BEGSR  sr_OpzS06;
143500160412
143600160412         // -?Ciclo di lettura subfile?
143700160412         readc  MS13S06;
143800160412
143900160412         DOW  NOT  %eof(FIMS13D);
144000160412
144100160413           %subst( IndDspF : 50 ) = *off;
144200160412           $SflNxtChg  = *off;
144300160412           C6RcdNbr    = S06nrr;
144400160412
144500160412           Select;
144600160412
144700160412             // -?Nessuna opzione?
144800160412             When  S06opz = *blank;
144900160412
145000160412             // -?5 = Visualizzazione BarCode per Profilo Spunta?
145100160413             When  S06opz = '5';
145200160412               $Video  = 'S7';
145300160412               $InzS07 = *on;
145400160412               DoW  $Video = 'S7';
145500160412                 exsr  sr_GesS07;
145600160412               EndDo;
145700160412               $Video = 'S6';
145800160412
145900160412             // -?? = Opzione NON valida?
146000160412             Other;
146100160412               $ErrGenerico = *on;
146200160412               $ErrMessage  = *on;
146300160412               $PosCurOpz   = *on;
146400160412               VIDmsg = sk_Msg(12);
146500160412
146600160412           EndSl;
146700160412
146800160412           // -?Aggiornamento subfile?
146900160412           Select;
147000160412             When  $ErrMessage;
147100160412               $SflNxtChg = *on;
147200160412               C6CsrRrn   = C6RcdNbr;
147300160412             When  $ErrGenerico;
147400160412               $SflNxtChg = *on;
147500160412               C6CsrRrn   = C6RcdNbr;
147600160412               clear  S06opz;
147700160412             Other;
147800160412               C6CsrRrn   = C6RcdNbr;
147900160412               clear  S06opz;
148000160412           EndSl;
148100160526           $F10Attivo  = ( $Video = 'S6'  and
148200170522                           %subst( SDSjob : 1 : 9 ) = 'P04685668' );
148300160412
148400160412           update  MS13S06;
148500160412
148600160412           // -?Uscita dal ciclo di lettura?
148700160412           if  $ErrMessage  or  $ErrGenerico;
148800160412             clear  $InzS06;
148900160412             leave;
149000160412           endif;
149100160412
149200160412           // -?Lettura rec. variato sucessivo?
149300160412           readc  MS13S06;
149400160412
149500160412         EndDo;
149600160412
149700160412         // -?Se richiesto il RI-caricamento del subfile:?
149800160412         //  ?occorre chiudere il cursore SQL?
149900160412         If  $InzS06  and  Not $errGenerico;
150000160412           exsr  sr_CloseCursor;
150100160412         EndIf;
150200160412
150300160412       ENDSR;
150400160413
150500160413       //--------------------------------------------------------------
150600160413       //?Gestione subfile S07.                                        ?
150700160413       //--------------------------------------------------------------
150800160413       BEGSR  sr_GesS07;
150900160413
151000160413         // -?Inizializzazione subfile S07?
151100160413         if  $InzS07 = *on;
151200160413           exsr  sr_InzS07;
151300160413           $InzS07 = *off;
151400160413         endif;
151500160413
151600160413         // -?(Dis)Attivazione tasti funzionali a video?
151700160413         $F8Attivo   = *off;
151800160413         $F9Attivo   = *off;
151900160413         $F10Attivo  = *off;
152000160413         $F12Attivo  = *on;
152100160413         $RlUpAttivo = *off;
152200160413         $RlDnAttivo = *off;
152300160413
152400160413         // -?Posizionamento cursore?
152500160413         //  ?C7CsrRrn contiene il numero di riga del SubFile su cui?
152600160413         //  ?era posizionato il cursore; impostandolo in C7RcdNbr?
152700160413         //  ?si visualizza la pagina che era visualizzata quando?
152800160413         //  ?l'utente ha premuto l'ultimo tasto.?
152900160413         if  C7CsrRrn > *zero;
153000160413           C7RcdNbr = C7CsrRrn;
153100160413         else;
153200160413           C7RcdNbr = 1;
153300160413           write  MS13D06;          //(no rec.)?
153400160413         endif;
153500160413
153600160413         // -?Emissione Testata e Piede con tasti funzionali abilitati?
153700160413         write  MS13T01;
153800160413         write  MS13D02;
153900160413         write  MS13Z06;
154000160413         write  MS13P02;
154100160413
154200160413         // -?Emissione videata?
154300160413         exfmt  MS13C07;
154400160413
154500160413         clear  VIDmsg;
154600160413         reset  $ErrMessage;
154700160413         reset  $ErrGenerico;
154800160413
154900160413         Select;
155000160413
155100160413           // -?F3=Fine?
155200160413           When  dsp_aid = c_F03;
155300160413             exsr  sr_CloseCursor7;
155400160413             exsr  sr_CloseCursor;
155500160413             $Fine = *on;
155600160413             clear  $Video;
155700160413
155800160413           // -?F12=Ritorno?
155900160413           When  dsp_aid = c_F12;
156000160413             exsr  sr_CloseCursor7;
156100160413             $Video = 'S6';
156200160413
156300160413           // -?Roll-Up?
156400160413           When  dsp_aid = c_RollUp;
156500160413             // -?Verifica raggiungimento del limite di records?
156600160413             //  ?registrabili nel subfile (9999)?
156700160413             if  S07nrr = *hival;
156800160413               $ErrGenerico = *on;
156900160413               $ErrMessage  = *on;
157000160413               VIDmsg = sk_Msg(11);
157100160413               leavesr;
157200160413             endif;
157300160413             exsr sr_RollUpS07;
157400160413
157500160413         EndSl;
157600160413
157700160413       ENDSR;
157800160413
157900160413       //--------------------------------------------------------------
158000160413       //?Inizializzazione subfile S07.                                ?
158100160413       //--------------------------------------------------------------
158200160413       BEGSR  sr_InzS07;
158300160413
158400160413         // -?Pulizia subfile?
158500160413         $Sf7Dsp_N    = *on;
158600160413         $Sf7DspCtl_N = *on;
158700160413         write  MS13C07;
158800160413         $Sf7DspCtl_N = *off;
158900160413         $Sf7End      = *off;
159000160413
159100160413         $Sf7NxtChg   = *off;
159200160413
159300160413         clear  w_SflPag7;
159400160413         clear  C7RcdNbr;
159500160413         clear  C7CsrRrn;
159600160413         clear  S07nrr;
159700160413         clear  VIDmsg;
159800160413         $ErrMessage  = *off;
159900160413         $ErrGenerico = *off;
160000160413
160100160413         // -?Spegnimento degli indicatori di errore?
160200160413         %subst( IndDspF : 50 ) = *off;
160300160413
160400160413         // -?Caricamento della 1ª pagina di dati del subfile?
160500160413         // -?Preparazione stringa SQL?
160600160413         exsr  sr_PrepSQL7;
160700160413         // -?Apertura del cursore?
160800160413         exsr  sr_OpenCursor7;
160900160413         // -?Lettura del file?
161000160413         exsr  sr_ReadCursor7;
161100160413         DoW  Not $EoF7  and  S07nrr < c_SflPag7;
161200160413           exsr  sr_RollUpS07;
161300160413         EndDo;
161400160413
161500160413         // -?Visualizzazione del SFL (se ci sono dati)?
161600160413         $Sf7Dsp_N = ( S07nrr <= *zero );
161700160413
161800160413         // -?Attivazione del SFLEND?
161900160413         $Sf7End   = ( $EoF7 = *on );
162000160413
162100160413         // -?Impaginazione subfile?
162200160413         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
162300160413         if  S07nrr > *zero;
162400160413           C7RcdNbr = 1;
162500160413           C7CsrRrn = 1;
162600160413         else;
162700160413           clear  C7RcdNbr;
162800160413         endif;
162900160413
163000160413       ENDSR;
163100160413
163200160413       //--------------------------------------------------------------
163300160413       //?Caricamento singola pagine SubFile S07.                      ?
163400160413       //--------------------------------------------------------------
163500160413       BEGSR  sr_RollUpS07;
163600160413
163700160413         S07nrr     = ( w_SflPag7 * c_SflPag7 );
163800160413         w_SflPag7 += 1;
163900160413         $Sf7NxtChg = *off;
164000160413
164100160413         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
164200160413         DoW  $EoF7  = *off    and
164300160413              S07nrr < *hival  and
164400160413              S07nrr < ( w_SflPag7 * c_SflPag7 );
164500160413
164600160413           // -?Caricamento dati nel record del subfile?
164700160413           exsr  sr_RieRecS07;
164800160413           S07nrr += 1;
164900160413           write  MS13S07;
165000160413
165100160413           // -?Lettura record successivo nell'archivio?
165200160413           exsr  sr_ReadCursor7;
165300160413
165400160413         EndDo;
165500160413
165600160413         // -?Visualizzazione del SFL (se ci sono dati)?
165700160413         $Sf7Dsp_N = ( S07nrr <= *zero );
165800160413
165900160413         // -?Attivazione (eventuale) del SFLEND?
166000160413         $Sf7End   = ( $EoF7 = *on );
166100160413
166200160413         // -?Posizionamento cursore al 1° rcd della pagina?
166300160413         if  S07nrr > *zero;
166400160413           wPag     = S07nrr / c_SflPag7;
166500160413           wRig     = S07nrr - ( c_SflPag7 * wPag );
166600160413           C7RcdNbr = wPag * c_SflPag7;
166700160413           if  wRig > *zero;
166800160413             C7RcdNbr = C7RcdNbr + 1;
166900160413           else;
167000160413             C7RcdNbr = C7RcdNbr - c_SflPag7 + 1;
167100160413           endif;
167200160413         else;
167300160413           clear  C7RcdNbr;
167400160413         endif;
167500160413
167600160413         C7CsrRrn = C7RcdNbr;
167700160413
167800160413       ENDSR;
167900160413
168000160413       //--------------------------------------------------------------
168100160413       //?Caricamento singolo record nel SubFile S07.                  ?
168200160413       //--------------------------------------------------------------
168300160413       BEGSR  sr_RieRecS07;
168400160413
168500160413         clear  MS13S07;
168600160413
168700160413         // -?Campi Output?
168800160413         // ·?BarCode?
168900160413         S07barC   = ds_FIPSL7.PSLbarCo;
169000160413         // ·?Utente Spunta?
169100160413         S07usr    = ds_FIPSL7.PSLusr;
169200160413         // ·?Data/Ora Rilevazione?
169300160413         if  %check( c_Digits : ds_FIPSL7.PSLdtOraR ) = *zero;
169400160413           ds_DtOraR = ds_FIPSL7.PSLdtOraR;
169500160413           wDate_EUR = %date( ds_DtOraR.Data : *iso );
169600160413           S07dtOraR = %editc( %dec( wDate_EUR ) : 'Y' ) + ' - ' +
169700160413                       %editw( ds_DtOraR.Ora : '0 :  :  ' );
169800160413         else;
169900160413           S07dtOraR = %subst( ds_FIPSL7.PSLdtOraR :  1 : 4 ) + '-' +
170000160413                       %subst( ds_FIPSL7.PSLdtOraR :  5 : 2 ) + '-' +
170100160413                       %subst( ds_FIPSL7.PSLdtOraR :  7 : 2 ) + '   ' +
170200160413                       %subst( ds_FIPSL7.PSLdtOraR :  9 : 2 ) + ':' +
170300160413                       %subst( ds_FIPSL7.PSLdtOraR : 11 : 2 ) + ':' +
170400160413                       %subst( ds_FIPSL7.PSLdtOraR : 13 : 2 );
170500160413         endif;
170600160413         // ·?Numero Foglio?
170700160413         if  %check( c_Digits : ds_FIPSL7.PSLnfv ) = *zero;
170800160413           evalR S07nfv  = %editc( %int( ds_FIPSL7.PSLnfv ) : '1' );
170900160413         else;
171000160413           S07nfv  = ds_FIPSL7.PSLnfv;
171100160413         endif;
171200160413
171300160413       ENDSR;
171400161219
171500161219       //--------------------------------------------------------------
171600161219       //?Caricamento F.V. da elaborare.                               ?
171700161219       //?(NON più solo per Categoria 2=Arrivi)                        ?
171800161219       //--------------------------------------------------------------
171900161219       BEGSR  sr_Intab_FV_Elab;
172000161219
172100161219         clear  wNFV_9_0;
172200161219         clear  wNFV_x_SQL;
172300161219         clear  keyFNFVV02;
172400161219
172500161219         //*·IF  V1Cnpg = 2;
172600161219
172700161219           k_FVVnpg  = V1Cnpg;
172800161219           k_FVVdfv  = WWWdfv;
172900161219           k_FVVfgs  = V1Cfil;
173000161219           setll  %kds( keyFNFVV02 )  FNFVV000;
173100161219           reade  %kds( keyFNFVV02 )  FNFVV000;
173200161219
173300161219           DoW  Not %eof(FNFVV02L);
173400161219
173500161219             if  FVVatb = *blank;
173600161219
173700161219               if  FVVnpg <> 3  or  FVVspg  = 'A';
173800161219
173900161219                 if  wNFV_9_0 <> *zero;
174000161219                   wNFV_x_SQL += ', ';
174100161219                 endif;
174200161219
174300161219                 wNFV_9_0 = FVVnfv;
174400161219                 wNFV_x_SQL   += '''' + %editc( wNFV_9_0 : 'X' ) + '''';
174500161219
174600161219               endif;
174700161219
174800161219             endif;
174900161219             reade  %kds( keyFNFVV02 )  FNFVV000;
175000161219
175100161219           EndDo;
175200161219
175300161219           if  wNFV_x_SQL = *blank;
175400161219             wNFV_x_SQL = '0';
175500161219           endif;
175600161219
175700161219         //*·ENDIF;
175800161219
175900161219       ENDSR;
176000160412
176100160412       //--------------------------------------------------------------
176200160412       //?Impostazione dati per singola Postazione Fissa x Arrivi.     ?
176300160412       //--------------------------------------------------------------
176400160412       BEGSR  sr_RepPostFissa;
176500160412
176600160412         clear  wDesP;
176700160412         clear  wNumP;
176800160412
176900160412         select;
177000160412           // -?Postazione errata?
177100160412           when  ( sk_MSL_Ent(xP) <= *zero  or  sk_MSL_Ent(xP) > 20 );
177200160412             leavesr;
177300160413           //*// -?Postazione vuota?
177400160413           //*when  sk_Ncl_Incomp( sk_MSL_Ent(xP) ) = *zero;
177500160413           //*  leavesr;
177600160412         endsl;
177700160412
177800160412         // -?Descrizione Postazione?
177900161028         select;
178000161028           when  sk_MSL_Pep(xP) = *blank;
178100161028             wDesP = *all'? ';
178200161028           when  V1Cnpg = 5;
178300161028             wDesP = sk_MSL_Pep(xP) + ' - ' + sk_MSL_De5(xP);
178400161028           when  V1Cnpg <> 5;
178500161028             wDesP = sk_MSL_Pep(xP) + ' - ' + sk_MSL_Des(xP);
178600161028         endsl;
178700160412
178800160412         // -?Numero Colli x Postazione?
178900160412         wNumP = sk_Ncl_Incomp( sk_MSL_Ent(xP) );
179000160412
179100160412       ENDSR;
179200160411
179300160411       //--------------------------------------------------------------
179400160411       //?Memorizzaz. Postaz. Fisse da tab."MSL" (Layout Postaz. VDL). ?
179500160411       //--------------------------------------------------------------
179600160411       BEGSR  sr_Tab_MSL;
179700160411
179800160411         clear  xP;
179900160411         clear  sk_MSL_Pep;
180000160411         clear  sk_MSL_Ent;
180100160411         clear  sk_MSL_Des;
180200161028         clear  sk_MSL_De5;
180300160411
180400160411         setll  ( 'MSL' : %editc( V1Cfil : 'X' ) )  TNTBE000;
180500160411         reade  ( 'MSL' : %editc( V1Cfil : 'X' ) )  TNTBE000;
180600160411
180700160411         // -?Ciclo di memorizzazione Postazioni Fisse (Incompatibili)?
180800160411         DoW  Not  %eof(TNTBE01L)  and  xP < %elem(sk_MSL_Pep);
180900160411
181000160411           dMSL = TBEuni;
181100160411
181200160711           //*if  TBEatb = *blank  and  dMSL.§MSLapl = 'I';
181300161025           if  TBEatb = *blank  and  ( dMSL.§MSLapl = dDRM.§DRMaFiss
181400161216                                and    V1Cnpg      <> 5
181500161025                                 or    dMSL.§MSLap5 = dDRM.§DRMaFiss
181600161025                                and    V1Cnpg       = 5 );
181700160411
181800160411             xP += 1;
181900160411             sk_MSL_Pep(xP) = %subst( dMSL.§MSLpep : 10 );
182000160411             sk_MSL_Ent(xP) = dMSL.§MSLent;
182100160411             sk_MSL_Des(xP) = dMSL.§MSLdes;
182200161028             sk_MSL_De5(xP) = dMSL.§MSLde5;
182300160411
182400160411           endif;
182500160411
182600160411           reade  ( 'MSL' : %editc( V1Cfil : 'X' ) )  TNTBE000;
182700160411
182800160411         EndDo;
182900160411
183000160411       ENDSR;
183100160322
183200160322       //--------------------------------------------------------------
183300160322       //?Elaborazione Attività Smistamento del Macchinone Smistacolli.?
183400160322       //--------------------------------------------------------------
183500160322       BEGSR  sr_ElabFNMSA;
183600160322
183700160322         // -?Ciclo di lettura dati del foglio da caricare?
183800170201         clear  keyFNMSA02;
183900160322         k_MSAapl  = V1Capl;
184000160322         k_MSAfil  = V1Cfil;
184100160411         k_MSAdfv  = WWWdfv;
184200160322         k_MSAnpg  = V1Cnpg;
184300160411         k_MSAdtl  = WWWdli;
184400160408         k_MSAorl  = V1Coli;
184500170201         setll  %kds( keyFNMSA02 )  FNMSA000;
184600170201         reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
184700160322
184800170201         //*·DoW  Not %eof(FNMSA02L);
184900170201         DoW  Not %eof(FNMSA02L)  and
185000161107              ( ( MSAdtl * 10000 ) + MSAorl ) <=
185100161107              ( ( WWWdlf * 10000 ) + V1Colf );
185200160322           Select;
185300160322
185400160415             // -?"Letture" INIZIALI da scartare?
185500160415             //When  V1Csmv = 'S'  and  MSApesInd = *zero;
185600160415             When      V1Csmv    = 'S'  and
185700160415                   ( ( MSAtcl   <> 999  and  W2Ddli    = *zero )    or
185800160415                     ( MSAtcl    = 999  and  W2DdliM   = *zero ) )  and
185900160415                       MSApesInd = *zero;
186000160324
186100160322             // -?ARRIVI?
186200161010             When  MSAtcl <> 999  or  MSAapl = 'P';
186300160322
186400160322               // -?Data/Ora iniziali?
186500160322               if  W2Ddli  = *zero;
186600160322                 W2Ddli  = MSAdtl;
186700160414                 W2Doli  = MSAorl;
186800160322               endif;
186900160322
187000160322               // -?Data/Ora finali?
187100160322               W2Ddlf    = MSAdtl;
187200160414               W2Dolf    = MSAorl;
187300160322
187400160322               // -?Minuti lavorati (per KPI)?
187500160322               wMinLav  += 1;
187600160322
187700160322               // -?Colli lavorati (per KPI)?
187800160322               wNcl     += MSApep001 +
187900160322                           MSApep002 +
188000160322                           MSApep003 +
188100160322                           MSApep004 +
188200160322                           MSApep005 +
188300160322                           MSApep006 +
188400160322                           MSApep007 +
188500160322                           MSApep008 +
188600160322                           MSApep009 +
188700160322                           MSApep010 +
188800160322                           MSApep011 +
188900160322                           MSApep012 +
189000160322                           MSApep013 +
189100160322                           MSApep014 +
189200160322                           MSApep015 +
189300160322                           MSApep016 +
189400160322                           MSApep017 +
189500160322                           MSApep018 +
189600160322                           MSApep019 +
189700160322                           MSApep020;    // -?ANCHE post.20!?
189800160722
189900160722               // -?Colli lavorati (Picking su Rulliera)?
190000160722               wNcl_K   += ( MSApesind - MSAnlp - MSApusLos );
190100160322
190200160322               // -?Minuti lavorati per Colli obiettivo (per KPI)?
190300160322               //  ?(SENZA post.20!)?
190400160322               if  MSApep001 > *zero;
190500160322                 wMINxNCL_ob  += 1;
190600160322               endif;
190700160322               if  MSApep002 > *zero;
190800160322                 wMINxNCL_ob  += 1;
190900160322               endif;
191000160322               if  MSApep003 > *zero;
191100160322                 wMINxNCL_ob  += 1;
191200160322               endif;
191300160322               if  MSApep004 > *zero;
191400160322                 wMINxNCL_ob  += 1;
191500160322               endif;
191600160322               if  MSApep005 > *zero;
191700160322                 wMINxNCL_ob  += 1;
191800160322               endif;
191900160322               if  MSApep006 > *zero;
192000160322                 wMINxNCL_ob  += 1;
192100160322               endif;
192200160322               if  MSApep007 > *zero;
192300160322                 wMINxNCL_ob  += 1;
192400160322               endif;
192500160322               if  MSApep008 > *zero;
192600160322                 wMINxNCL_ob  += 1;
192700160322               endif;
192800160322               if  MSApep009 > *zero;
192900160322                 wMINxNCL_ob  += 1;
193000160322               endif;
193100160322               if  MSApep010 > *zero;
193200160322                 wMINxNCL_ob  += 1;
193300160322               endif;
193400160322               if  MSApep011 > *zero;
193500160322                 wMINxNCL_ob  += 1;
193600160322               endif;
193700160322               if  MSApep012 > *zero;
193800160322                 wMINxNCL_ob  += 1;
193900160322               endif;
194000160322               if  MSApep013 > *zero;
194100160322                 wMINxNCL_ob  += 1;
194200160322               endif;
194300160322               if  MSApep014 > *zero;
194400160322                 wMINxNCL_ob  += 1;
194500160322               endif;
194600160322               if  MSApep015 > *zero;
194700160322                 wMINxNCL_ob  += 1;
194800160322               endif;
194900160322               if  MSApep016 > *zero;
195000160322                 wMINxNCL_ob  += 1;
195100160322               endif;
195200160322               if  MSApep017 > *zero;
195300160322                 wMINxNCL_ob  += 1;
195400160322               endif;
195500160322               if  MSApep018 > *zero;
195600160322                 wMINxNCL_ob  += 1;
195700160322               endif;
195800160322               if  MSApep019 > *zero;
195900160322                 wMINxNCL_ob  += 1;
196000160322               endif;
196100160415               //if  MSApep020 > *zero; ?(SENZA post.20!)?
196200160322               //  wMINxNCL_ob  += 1;
196300160322               //endif;
196400160322
196500160322               // -?Colli in Overflow?
196600160526               if  MSAapl = 'V';
196700160526                 V2Dovrf  += ( MSApesInd - MSApusSor );
196800160526               endif;
196900160322
197000160322               // -?Colli non letti da scanner?
197100160322               V2Dnol   += MSAnls;
197200160322
197300160322             // -?PICKING (MOVIMENTO)?
197400160324             When  MSAtcl =  999;
197500160322
197600160322               // -?Data/Ora iniziali?
197700160322               if  W2DdliM = *zero;
197800160322                 W2DdliM = MSAdtl;
197900160414                 W2DoliM = MSAorl;
198000160322               endif;
198100160322
198200160322               // -?Data/Ora finali?
198300160322               W2DdlfM   = MSAdtl;
198400160414               W2DolfM   = MSAorl;
198500160322
198600160322               // -?Minuti lavorati (per KPI)?
198700160322               wMinLavM += 1;
198800160322
198900160322               // -?Colli lavorati (per KPI)?
199000160322               wNclM    += MSApep001 +
199100160322                           MSApep002 +
199200160322                           MSApep003 +
199300160322                           MSApep004 +
199400160322                           MSApep005 +
199500160322                           MSApep006 +
199600160322                           MSApep007 +
199700160322                           MSApep008 +
199800160322                           MSApep009 +
199900160322                           MSApep010 +
200000160322                           MSApep011 +
200100160322                           MSApep012 +
200200160322                           MSApep013 +
200300160322                           MSApep014 +
200400160322                           MSApep015 +
200500160322                           MSApep016 +
200600160322                           MSApep017 +
200700160322                           MSApep018 +
200800160322                           MSApep019 +
200900160322                           MSApep020;    // -?ANCHE post.20!?
201000160722
201100160722               // -?Colli lavorati (Picking su Rulliera)?
201200160722               wNclM_K  += ( MSApesind - MSAnlp - MSApusLos );
201300160322
201400160322               // -?Minuti lavorati per Colli obiettivo (per KPI)?
201500160322               //  ?(SENZA post.20!)?
201600160322               if  MSApep001 > *zero;
201700160322                 wMINxNCL_obM += 1;
201800160322               endif;
201900160322               if  MSApep002 > *zero;
202000160322                 wMINxNCL_obM += 1;
202100160322               endif;
202200160322               if  MSApep003 > *zero;
202300160322                 wMINxNCL_obM += 1;
202400160322               endif;
202500160322               if  MSApep004 > *zero;
202600160322                 wMINxNCL_obM += 1;
202700160322               endif;
202800160322               if  MSApep005 > *zero;
202900160322                 wMINxNCL_obM += 1;
203000160322               endif;
203100160322               if  MSApep006 > *zero;
203200160322                 wMINxNCL_obM += 1;
203300160322               endif;
203400160322               if  MSApep007 > *zero;
203500160322                 wMINxNCL_obM += 1;
203600160322               endif;
203700160322               if  MSApep008 > *zero;
203800160322                 wMINxNCL_obM += 1;
203900160322               endif;
204000160322               if  MSApep009 > *zero;
204100160322                 wMINxNCL_obM += 1;
204200160322               endif;
204300160322               if  MSApep010 > *zero;
204400160322                 wMINxNCL_obM += 1;
204500160322               endif;
204600160322               if  MSApep011 > *zero;
204700160322                 wMINxNCL_obM += 1;
204800160322               endif;
204900160322               if  MSApep012 > *zero;
205000160322                 wMINxNCL_obM += 1;
205100160322               endif;
205200160322               if  MSApep013 > *zero;
205300160322                 wMINxNCL_obM += 1;
205400160322               endif;
205500160322               if  MSApep014 > *zero;
205600160322                 wMINxNCL_obM += 1;
205700160322               endif;
205800160322               if  MSApep015 > *zero;
205900160322                 wMINxNCL_obM += 1;
206000160322               endif;
206100160322               if  MSApep016 > *zero;
206200160322                 wMINxNCL_obM += 1;
206300160322               endif;
206400160322               if  MSApep017 > *zero;
206500160322                 wMINxNCL_obM += 1;
206600160322               endif;
206700160322               if  MSApep018 > *zero;
206800160322                 wMINxNCL_obM += 1;
206900160322               endif;
207000160322               if  MSApep019 > *zero;
207100160322                 wMINxNCL_obM += 1;
207200160322               endif;
207300160415               //if  MSApep020 > *zero; ?(SENZA post.20!)?
207400160322               //  wMINxNCL_obM += 1;
207500160322               //endif;
207600160322
207700160322               // -?Colli in Overflow?
207800160526               if  MSAapl = 'V';
207900160526                 V2DovrfM += ( MSApesInd - MSApusSor );
208000160526               endif;
208100160322
208200160322               // -?Colli non letti da scanner?
208300160322               V2DnolM  += MSAnls;
208400160322
208500160322           EndSl;
208600160322
208700170201           reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
208800160322
208900160322         EndDo;
209000160323
209100160323         // -?NON trovate letture?
209200160323         if  ( wMinLav + wMinLavM ) = *zero;
209300160323           leavesr;
209400160323         endif;
209500160322
209600160322         // -?Data/Ora iniziale Arrivi?
209700160414         if  W2Ddli > *zero;
209800160414           reset  WLBdat;
209900160414           G08inv = W2Ddli;
210000160414           XSRDA8 ( WLBdat );
210100160414           V2Ddli = G08dat;
210200160414           V2Doli = %editw( W2Doli : '0 :  ' );
210300160414         endif;
210400160322
210500160322         // -?Data/Ora finale Arrivi?
210600160414         if  W2Ddlf > *zero;
210700160414           reset  WLBdat;
210800160414           G08inv = W2Ddlf;
210900160414           XSRDA8 ( WLBdat );
211000160414           V2Ddlf = G08dat;
211100160414           V2Dolf = %editw( W2Dolf : '0 :  ' );
211200160414         endif;
211300160322
211400160322         // -?Data/Ora iniziale Picking (Movimento)?
211500160414         if  W2DdliM > *zero;
211600160414           reset  WLBdat;
211700160414           G08inv  = W2DdliM;
211800160414           XSRDA8 ( WLBdat );
211900160414           V2DdliM = G08dat;
212000160414           V2DoliM = %editw( W2DoliM : '0 :  ' );
212100160414         endif;
212200160322
212300160322         // -?Data/Ora finale Picking (Movimento)?
212400160414         if  W2DdlfM > *zero;
212500160414           reset  WLBdat;
212600160414           G08inv = W2DdlfM;
212700160414           XSRDA8 ( WLBdat );
212800160414           V2DdlfM = G08dat;
212900160414           V2DolfM = %editw( W2DolfM : '0 :  ' );
213000160414         endif;
213100160322
213200160414         If  dKPI.§KPIpba > *zero;
213300160414
213400160414           // -?KPI ARRIVI - effettivo & obiettivo?
213500160414           if  wNcl  > *zero  or  wMINxNCL_ob  > *zero;
213600160414             // -?Calcolo tempo effettivamente lavorato (HH:MM)?
213700160414             §min  = wMinLav;
213800160414             clear  §hh;
213900160414             clear  §mm;
214000160414             exsr  sr_CalcHML;
214100160414             // -?KPI = Media Pacchi: Tot. Pacchi / Tempo lavorato?
214200160414             //  ?(Effettivo e Obiettivo)?
214300160414             if  wMinLav  = *zero;
214400160414               wMinLav  = 1;
214500160414             endif;
214600160414             eval(h)  W2Dkpi  = wNcl / wMinLav;
214700160414             V2DkpiE  = %editc( W2Dkpi : '3' );
214800160414             eval(h)  W2Dkpi  = ( dKPI.§KPIpba * wMINxNCL_ob ) / wMinLav;
214900160414             V2DkpiO  = %editc( W2Dkpi : '3' );
215000160414           endif;
215100160414
215200160414           // -?KPI PICKING (MOVIMENTO) - effettivo & obiettivo?
215300160414           if  wNclM > *zero  or  wMINxNCL_obM > *zero;
215400160414             // -?Calcolo tempo effettivamente lavorato (HH:MM)?
215500160414             §min  = wMinLavM;
215600160414             clear  §hh;
215700160414             clear  §mm;
215800160414             exsr  sr_CalcHML;
215900160414             // -?KPI = Media Pacchi: Tot. Pacchi / Tempo lavorato?
216000160414             //  ?(Effettivo e Obiettivo)?
216100160414             if  wMinLavM = *zero;
216200160414               wMinLavM = 1;
216300160414             endif;
216400160414             eval(h)  W2Dkpi  = wNclM / wMinLavM;
216500160414             V2DkpiEm = %editc( W2Dkpi : '3' );
216600160414             eval(h)  W2Dkpi  = ( dKPI.§KPIpba * wMINxNCL_obM ) / wMinLavM;
216700160414             V2DkpiOm = %editc( W2Dkpi : '3' );
216800160414           endif;
216900160414
217000160414         EndIf;
217100160408
217200160408         // -?VDL/CAR: Colli caricati?
217300160722         If  V1Capl = 'K';
217400160722           if  wNcl_K  <= 99999;
217500160722             V2Dtcv   = wNcl_K;
217600160722           else;
217700160722             V2Dtcv   = 99999;
217800160722           endif;
217900160722           if  wNclM_K <= 99999;
218000160722             V2DtcvM  = wNclM_K;
218100160722           else;
218200160722             V2DtcvM  = 99999;
218300160722           endif;
218400160722         Else;
218500160722           if  wNcl  <= 99999;
218600160722             V2Dtcv   = wNcl;
218700160722           else;
218800160722             V2Dtcv   = 99999;
218900160722           endif;
219000160722           if  wNclM <= 99999;
219100160722             V2DtcvM  = wNclM;
219200160722           else;
219300160722             V2DtcvM  = 99999;
219400160722           endif;
219500160722         EndIf;
219600160722
219700160722         // -?Ricalcolo dati "Movimento (Picking)"   SE?
219800160722         //  ?fil. 006-Padova Interporto?
219900160729         if  V1Cfil = 006  and  V1Cnpg = 2;
220000160722           exsr  sr_ElabFNMSA_M_006;
220100160722         endif;
220200160322
220300160322       ENDSR;
220400160722
220500160722       //--------------------------------------------------------------
220600160722       //?Elaborazione Attività Smistamento del Macchinone Smistacolli ?
220700160722       //?- Movimento (Picking) - per la fil. 006 (Padova Interporto). ?
220800160722       //--------------------------------------------------------------
220900160722       BEGSR  sr_ElabFNMSA_M_006;
221000161108
221100161108         clear  W2DdliM;
221200161108         clear  W2DoliM;
221300161108         clear  W2DdlfM;
221400161108         clear  W2DolfM;
221500161108         clear  V2DdliM;
221600161108         clear  V2DoliM;
221700161108         clear  V2DdlfM;
221800161108         clear  V2DolfM;
221900161108         clear  wMinLavM;
222000161108         clear  wNclM;
222100161108         clear  wNclM_K;
222200161108         clear  wMINxNCL_obM;
222300161108         clear  V2DnolM;
222400161003
222500161003         // -?Reperimento Obiettivi di Produttività?
222600161003         //  ?di un Impianto Automatico Smistacolli?
222700161003         clear  dKPI_K;
222800161003         if  getTabella ( c_Tntbe : 'KPI' : %editc( V1Cnpg : 'X' ) : 'K' :
222900161003                          *omit : *omit :
223000161003                          *omit : *omit :
223100161003                          *omit : *omit : *omit : *omit :
223200161003                          *omit : *omit : *omit : *omit :
223300161003                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
223400161003                          = *zero      AND
223500161003               ds_TNTBE.TBEatb = *blank;
223600161003           dKPI_K  = ds_TNTBE.TBEuni;
223700161003         endif;
223800160722
223900160722         // -?Ciclo di lettura dati del foglio da caricare?
224000170201         clear  keyFNMSA02;
224100160722         k_MSAapl  = 'K';
224200160722         k_MSAfil  = V1Cfil;
224300160722         k_MSAdfv  = WWWdfv;
224400160722         k_MSAnpg  = V1Cnpg;
224500160722         k_MSAdtl  = WWWdli;
224600160722         k_MSAorl  = V1Coli;
224700170201         setll  %kds( keyFNMSA02 )  FNMSA000;
224800170201         reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
224900160722
225000170201         //*·DoW  Not %eof(FNMSA02L);
225100170201         DoW  Not %eof(FNMSA02L)  and
225200161107              ( ( MSAdtl * 10000 ) + MSAorl ) <=
225300161107              ( ( WWWdlf * 10000 ) + V1Colf );
225400160722
225500160722           // -?"Letture" INIZIALI da scartare?
225600160722           If      V1Csmv    = 'S'  and
225700160722               ( ( MSAtcl   <> 999  and  W2Ddli    = *zero )    or
225800160722                 ( MSAtcl    = 999  and  W2DdliM   = *zero ) )  and
225900160722                   MSApesInd = *zero;
226000160722
226100160722           // -?PICKING (MOVIMENTO)?
226200160722           Else;
226300160722
226400160722             // -?Data/Ora iniziali?
226500160722             if  W2DdliM = *zero;
226600160722               W2DdliM = MSAdtl;
226700160722               W2DoliM = MSAorl;
226800160722             endif;
226900160722
227000160722             // -?Data/Ora finali?
227100160722             W2DdlfM   = MSAdtl;
227200160722             W2DolfM   = MSAorl;
227300160722
227400160722             // -?Minuti lavorati (per KPI)?
227500160722             wMinLavM += 1;
227600160722
227700160722             // -?Colli lavorati (per KPI)?
227800160722             wNclM    += MSApep001 +
227900160722                         MSApep002 +
228000160722                         MSApep003 +
228100160722                         MSApep004 +
228200160722                         MSApep005 +
228300160722                         MSApep006 +
228400160722                         MSApep007 +
228500160722                         MSApep008 +
228600160722                         MSApep009 +
228700160722                         MSApep010 +
228800160722                         MSApep011 +
228900160722                         MSApep012 +
229000160722                         MSApep013 +
229100160722                         MSApep014 +
229200160722                         MSApep015 +
229300160722                         MSApep016 +
229400160722                         MSApep017 +
229500160722                         MSApep018 +
229600160722                         MSApep019 +
229700160722                         MSApep020;    // -?ANCHE post.20!?
229800160722
229900160722             // -?Colli lavorati (Picking su Rulliera)?
230000160722             wNclM_K  += ( MSApesind - MSAnlp - MSApusLos );
230100160722
230200160722             // -?Minuti lavorati per Colli obiettivo (per KPI)?
230300160722             //  ?(SENZA post.20!)?
230400160722             if  MSApep001 > *zero;
230500160722               wMINxNCL_obM += 1;
230600160722             endif;
230700160722             if  MSApep002 > *zero;
230800160722               wMINxNCL_obM += 1;
230900160722             endif;
231000160722             if  MSApep003 > *zero;
231100160722               wMINxNCL_obM += 1;
231200160722             endif;
231300160722             if  MSApep004 > *zero;
231400160722               wMINxNCL_obM += 1;
231500160722             endif;
231600160722             if  MSApep005 > *zero;
231700160722               wMINxNCL_obM += 1;
231800160722             endif;
231900160722             if  MSApep006 > *zero;
232000160722               wMINxNCL_obM += 1;
232100160722             endif;
232200160722             if  MSApep007 > *zero;
232300160722               wMINxNCL_obM += 1;
232400160722             endif;
232500160722             if  MSApep008 > *zero;
232600160722               wMINxNCL_obM += 1;
232700160722             endif;
232800160722             if  MSApep009 > *zero;
232900160722               wMINxNCL_obM += 1;
233000160722             endif;
233100160722             if  MSApep010 > *zero;
233200160722               wMINxNCL_obM += 1;
233300160722             endif;
233400160722             if  MSApep011 > *zero;
233500160722               wMINxNCL_obM += 1;
233600160722             endif;
233700160722             if  MSApep012 > *zero;
233800160722               wMINxNCL_obM += 1;
233900160722             endif;
234000160722             if  MSApep013 > *zero;
234100160722               wMINxNCL_obM += 1;
234200160722             endif;
234300160722             if  MSApep014 > *zero;
234400160722               wMINxNCL_obM += 1;
234500160722             endif;
234600160722             if  MSApep015 > *zero;
234700160722               wMINxNCL_obM += 1;
234800160722             endif;
234900160722             if  MSApep016 > *zero;
235000160722               wMINxNCL_obM += 1;
235100160722             endif;
235200160722             if  MSApep017 > *zero;
235300160722               wMINxNCL_obM += 1;
235400160722             endif;
235500160722             if  MSApep018 > *zero;
235600160722               wMINxNCL_obM += 1;
235700160722             endif;
235800160722             if  MSApep019 > *zero;
235900160722               wMINxNCL_obM += 1;
236000160722             endif;
236100160722             //if  MSApep020 > *zero; ?(SENZA post.20!)?
236200160722             //  wMINxNCL_obM += 1;
236300160722             //endif;
236400160722
236500160722             // -?Colli non letti da scanner?
236600160722             V2DnolM  += MSAnls;
236700160722
236800160722           EndIf;
236900160722
237000170201           reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
237100160722
237200160722         EndDo;
237300160722
237400160722         // -?NON trovate letture?
237500160722         if  wMinLavM = *zero;
237600160722           leavesr;
237700160722         endif;
237800160722
237900160722         // -?Data/Ora iniziale Picking (Movimento)?
238000160722         if  W2DdliM > *zero;
238100160722           reset  WLBdat;
238200160722           G08inv  = W2DdliM;
238300160722           XSRDA8 ( WLBdat );
238400160722           V2DdliM = G08dat;
238500160722           V2DoliM = %editw( W2DoliM : '0 :  ' );
238600160722         endif;
238700160722
238800160722         // -?Data/Ora finale Picking (Movimento)?
238900160722         if  W2DdlfM > *zero;
239000160722           reset  WLBdat;
239100160722           G08inv = W2DdlfM;
239200160722           XSRDA8 ( WLBdat );
239300160722           V2DdlfM = G08dat;
239400160722           V2DolfM = %editw( W2DolfM : '0 :  ' );
239500160722         endif;
239600160722
239700161003         If  dKPI_K.§KPIpba > *zero;
239800160722
239900160722           // -?KPI PICKING (MOVIMENTO) - effettivo & obiettivo?
240000160722           if  wNclM > *zero  or  wMINxNCL_obM > *zero;
240100160722             // -?Calcolo tempo effettivamente lavorato (HH:MM)?
240200160722             §min  = wMinLavM;
240300160722             clear  §hh;
240400160722             clear  §mm;
240500160722             exsr  sr_CalcHML;
240600160722             // -?KPI = Media Pacchi: Tot. Pacchi / Tempo lavorato?
240700160722             //  ?(Effettivo e Obiettivo)?
240800160722             if  wMinLavM = *zero;
240900160722               wMinLavM = 1;
241000160722             endif;
241100160722             eval(h)  W2Dkpi  = wNclM / wMinLavM;
241200160722             V2DkpiEm = %editc( W2Dkpi : '3' );
241300161003             eval(h)  W2Dkpi  = ( dKPI_K.§KPIpba * wMINxNCL_obM ) / wMinLavM;
241400160722             V2DkpiOm = %editc( W2Dkpi : '3' );
241500160722           endif;
241600160722
241700160722         EndIf;
241800160722
241900160722         // -?VDL/CAR: Colli caricati?
242000160722         if  wNclM_K <= 99999;
242100160722           V2DtcvM  = wNclM_K;
242200160722         else;
242300160722           V2DtcvM  = 99999;
242400160722         endif;
242500160722
242600160722       ENDSR;
242700160408
242800160408       //--------------------------------------------------------------
242900160408       //?Elaborazione Attività Smistamento per "FISSI RPVP".          ?
243000160408       //--------------------------------------------------------------
243100160408       BEGSR  sr_ElabFNMSA_P;
243200160408
243300160408         // -?Ciclo di lettura dati del foglio da caricare?
243400170201         clear  keyFNMSA02;
243500160711         //*k_MSAapl  = 'P';
243600160711         k_MSAapl  = dDRM.§DRMaRPVP;
243700160408         k_MSAfil  = V1Cfil;
243800160411         k_MSAdfv  = WWWdfv;
243900160408         k_MSAnpg  = V1Cnpg;
244000160411         k_MSAdtl  = WWWdli;
244100160408         k_MSAorl  = V1Coli;
244200170201         setll  %kds( keyFNMSA02 )  FNMSA000;
244300170201         reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
244400160408
244500170201         //*·DoW  Not %eof(FNMSA02L);
244600170201         DoW  Not %eof(FNMSA02L)  and
244700161107              ( ( MSAdtl * 10000 ) + MSAorl ) <=
244800161107              ( ( WWWdlf * 10000 ) + V1Colf );
244900161007
245000161007           // -?Colli lavorati?
245100161007           V2Dtcp   += MSApep001 +
245200161007                       MSApep002 +
245300161007                       MSApep003 +
245400161007                       MSApep004 +
245500161007                       MSApep005 +
245600161007                       MSApep006 +
245700161007                       MSApep007 +
245800161007                       MSApep008 +
245900161007                       MSApep009 +
246000161007                       MSApep010 +
246100161007                       MSApep011 +
246200161007                       MSApep012 +
246300161007                       MSApep013 +
246400161007                       MSApep014 +
246500161007                       MSApep015 +
246600161007                       MSApep016 +
246700161007                       MSApep017 +
246800161007                       MSApep018 +
246900161007                       MSApep019 +
247000161007                       MSApep020;    // -?ANCHE post.20!?
247100161007
247200161007           // -?Data/Ora iniziali?
247300161007           if  MSAapl  = V1Capl  and
247400161007               V2Ddli  = *zero   and  W2Ddli  = *zero  and
247500161007               W2Ddli  = *zero;
247600161007             W2Ddli  = MSAdtl;
247700161007             W2Doli  = MSAorl;
247800161007           endif;
247900161007
248000161007           // -?Data/Ora finali?
248100161007           if  MSAapl  = V1Capl  and
248200161007               V2Ddli  = *zero   and  V2Ddlf  = *zero;
248300161007             W2Ddlf    = MSAdtl;
248400161007             W2Dolf    = MSAorl;
248500161007           endif;
248600161007
248700161007           // -?Minuti lavorati (per KPI)?
248800161007           if  MSAapl  = V1Capl  and
248900161007               V2Ddli  = *zero   and  V2Ddlf  = *zero;
249000161007             wMinLav  += 1;
249100161007           endif;
249200160408
249300170201           reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
249400160408
249500160408         EndDo;
249600160414
249700160414         // -?NON trovate letture?
249800160414         if  ( wMinLav + wMinLavM ) = *zero;
249900160414           leavesr;
250000160414         endif;
250100160414
250200160414         If  V2Ddli  = *zero  and  V2Ddlf  = *zero;
250300160414
250400160414           // -?Data/Ora iniziale Arrivi?
250500160414           if  W2Ddli > *zero;
250600160414             reset  WLBdat;
250700160414             G08inv = W2Ddli;
250800160414             XSRDA8 ( WLBdat );
250900160414             V2Ddli = G08dat;
251000160414             V2Doli = %editw( W2Doli : '0 :  ' );
251100160414           endif;
251200160414
251300160414           // -?Data/Ora finale Arrivi?
251400160414           if  W2Ddlf > *zero;
251500160414             reset  WLBdat;
251600160414             G08inv = W2Ddlf;
251700160414             XSRDA8 ( WLBdat );
251800160414             V2Ddlf = G08dat;
251900160414             V2Dolf = %editw( W2Dolf : '0 :  ' );
252000160414           endif;
252100160414
252200160414         EndIf;
252300160414
252400160414         If  V2DdliM = *zero  and  V2DdlfM = *zero;
252500160414
252600160414           // -?Data/Ora iniziale Picking (Movimento)?
252700160414           if  W2DdliM > *zero;
252800160414             reset  WLBdat;
252900160414             G08inv = W2DdliM;
253000160414             XSRDA8 ( WLBdat );
253100160414             V2DdliM = G08dat;
253200160414             V2DoliM = %editw( W2DoliM : '0 :  ' );
253300160414           endif;
253400160414
253500160414           // -?Data/Ora finale Picking (Movimento)?
253600160414           if  W2DdlfM > *zero;
253700160414             reset  WLBdat;
253800160414             G08inv = W2DdlfM;
253900160414             XSRDA8 ( WLBdat );
254000160414             V2DdlfM = G08dat;
254100160414             V2DolfM = %editw( W2DolfM : '0 :  ' );
254200160414           endif;
254300160414
254400160414         EndIf;
254500160408
254600160408       ENDSR;
254700160408
254800160408       //--------------------------------------------------------------
254900160408       //?Elaborazione Attività Smistamento per "FISSI".               ?
255000160408       //--------------------------------------------------------------
255100160408       BEGSR  sr_ElabFNMSA_I;
255200160408
255300160408         // -?Ciclo di lettura dati del foglio da caricare?
255400170201         clear  keyFNMSA02;
255500160711         //*k_MSAapl  = 'I';
255600160711         k_MSAapl  = dDRM.§DRMaFiss;
255700160408         k_MSAfil  = V1Cfil;
255800160411         k_MSAdfv  = WWWdfv;
255900160408         k_MSAnpg  = V1Cnpg;
256000160411         k_MSAdtl  = WWWdli;
256100160408         k_MSAorl  = V1Coli;
256200170201         setll  %kds( keyFNMSA02 )  FNMSA000;
256300170201         reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
256400160408
256500170201         //*·DoW  Not %eof(FNMSA02L);
256600170201         DoW  Not %eof(FNMSA02L)  and
256700161107              ( ( MSAdtl * 10000 ) + MSAorl ) <=
256800161107              ( ( WWWdlf * 10000 ) + V1Colf );
256900160408
257000160408           Select;
257100160408
257200160408             // -?ARRIVI?
257300160408             When  MSAtcl <> 999;
257400160408
257500160408               // -?Colli lavorati?
257600160408               V2DtcpI  += MSApep001 +
257700160408                           MSApep002 +
257800160408                           MSApep003 +
257900160408                           MSApep004 +
258000160408                           MSApep005 +
258100160408                           MSApep006 +
258200160408                           MSApep007 +
258300160408                           MSApep008 +
258400160408                           MSApep009 +
258500160408                           MSApep010 +
258600160408                           MSApep011 +
258700160408                           MSApep012 +
258800160408                           MSApep013 +
258900160408                           MSApep014 +
259000160408                           MSApep015 +
259100160408                           MSApep016 +
259200160408                           MSApep017 +
259300160408                           MSApep018 +
259400160408                           MSApep019 +
259500160408                           MSApep020;    // -?ANCHE post.20!?
259600160408
259700160408             // -?PICKING (MOVIMENTO)?
259800160408             When  MSAtcl =  999;
259900160408
260000160408               // -?Colli lavorati?
260100160408               V2DtcpIM += MSApep001 +
260200160408                           MSApep002 +
260300160408                           MSApep003 +
260400160408                           MSApep004 +
260500160408                           MSApep005 +
260600160408                           MSApep006 +
260700160408                           MSApep007 +
260800160408                           MSApep008 +
260900160408                           MSApep009 +
261000160408                           MSApep010 +
261100160408                           MSApep011 +
261200160408                           MSApep012 +
261300160408                           MSApep013 +
261400160408                           MSApep014 +
261500160408                           MSApep015 +
261600160408                           MSApep016 +
261700160408                           MSApep017 +
261800160408                           MSApep018 +
261900160408                           MSApep019 +
262000160408                           MSApep020;    // -?ANCHE post.20!?
262100160408
262200160408           EndSl;
262300160408
262400160408           // -?Colli Lavorati per Postazione?
262500160408           sk_Ncl_Incomp(01) += MSApep001;
262600160408           sk_Ncl_Incomp(02) += MSApep002;
262700160408           sk_Ncl_Incomp(03) += MSApep003;
262800160408           sk_Ncl_Incomp(04) += MSApep004;
262900160408           sk_Ncl_Incomp(05) += MSApep005;
263000160408           sk_Ncl_Incomp(06) += MSApep006;
263100160408           sk_Ncl_Incomp(07) += MSApep007;
263200160408           sk_Ncl_Incomp(08) += MSApep008;
263300160408           sk_Ncl_Incomp(09) += MSApep009;
263400160408           sk_Ncl_Incomp(10) += MSApep010;
263500160408           sk_Ncl_Incomp(11) += MSApep011;
263600160408           sk_Ncl_Incomp(12) += MSApep012;
263700160408           sk_Ncl_Incomp(13) += MSApep013;
263800160408           sk_Ncl_Incomp(14) += MSApep014;
263900160408           sk_Ncl_Incomp(15) += MSApep015;
264000160408           sk_Ncl_Incomp(16) += MSApep016;
264100160408           sk_Ncl_Incomp(17) += MSApep017;
264200160408           sk_Ncl_Incomp(18) += MSApep018;
264300160408           sk_Ncl_Incomp(19) += MSApep019;
264400160408           sk_Ncl_Incomp(20) += MSApep020;
264500160408
264600170201           reade  %kds( keyFNMSA02 : 4 )  FNMSA000;
264700160408
264800160408         EndDo;
264900160408
265000160408         // -?Calcolo numero pagine visualizzabili?
265100160408         clear  wPag_T;
265200160408         select;
265300160408           when  ( V2DtcpI + V2DtcpIM ) = *zero;
265400160408             wPag_T = 1;
265500160408           when  sk_MSL_Pep(11) <> *blank;
265600160408             wPag_T = 3;
265700160408           other;
265800160408             wPag_T = 2;
265900160408         endsl;
266000160408
266100160729         // -?Impostazione 1ª videata da emettere (D03 o D05)?
266200160729         //*clear  wPag_C;
266300160729         //*if  V1Cnpg = 2;
266400160408           wPag_C = 1;
266500160729         //*endif;
266600160408
266700160408       ENDSR;
266800160322
266900160322       //--------------------------------------------------------------
267000160322       //?Elaborazione Statistiche Arrivi.                             ?
267100160322       //--------------------------------------------------------------
267200160322       BEGSR  sr_ElabFISAR;
267300160322
267400160322         // -?Calcolo TERMINAL?
267500160323
267600160729         //*$NoFISAR = *on;
267700160322
267800160322         // -?Tipo Rec. "Normale"?
267900160322         clear  keyFISAR06;
268000160322         k_SARtfa = V1Cfil;
268100160411         k_SARdre = WWWdfv;
268200160322         k_SARlna = k_SARtfa;
268300160322         chain  %kds( keyFISAR06 )  FISAR000;
268400160322         if  %found(FISAR06L);
268500160729           //*$NoFISAR = *off;
268600160408           V2Dter   = SARcll;
268700160322         endif;
268800160322
268900160322         // -?Tipo Rec. "C/Servizio"?
269000160322         k_SARtre = 'C';
269100160322         chain  %kds( keyFISAR06 )  FISAR000;
269200160322         if  %found(FISAR06L);
269300160729           //*$NoFISAR = *off;
269400160408           V2Dter  += SARcll;
269500160322         endif;
269600160322
269700160322         // -?Tipo Rec. "Transito"?
269800160322         k_SARtre = 'T';
269900160322         chain  %kds( keyFISAR06 )  FISAR000;
270000160322         if  %found(FISAR06L);
270100160729           //*$NoFISAR = *off;
270200160408           V2Dter  += SARcll;
270300160322         endif;
270400160322
270500160322         //// -?Tipo Rec. "Solo Terminal"?
270600160322         //k_SARtre = 'R';
270700160322         //chain  %kds( keyFISAR06 )  FISAR000;
270800160322         //if  %found(FISAR06L);
270900160729         //  //*$NoFISAR = *off;
271000160408         //  V2Dter  += SARcll;
271100160322         //endif;
271200160322
271300160322       ENDSR;
271400160322
271500160322       //--------------------------------------------------------------
271600161028       //?Elaborazione Statistiche Partenza a Terra.                   ?
271700160322       //--------------------------------------------------------------
271800160322       BEGSR  sr_ElabFISAT;
271900160411
272000160411         // -?Reperimento Filiali dell'Area ("£1")?
272100160411         clear  sk_L1;
272200160411         clear  KPJBU;
272300160411         clear  TRUL06ds;
272400160411         D06cod = '£1';
272500160411         D06key = %editc( V1Cfil : 'X' ) + '    S';
272600160411         D06drf = WWWdfv;
272700160411         KPJBU  = TRUL06ds;
272800160411         trul06r ( KPJBA );
272900160411         TRUL06ds = KPJBU;
273000160411         ds_L1 = D06lia;
273100160322
273200160322         // -?Calcolo METRI CUBI in IMP?
273300160323
273400160729         //*$NoFISAT = *on;
273500160411
273600160411         clear  keyFISAT02;
273700160411         k_SATfle = V1Cfil;
273800160415         //k_SATtfa = *zero;       ?(già così)?
273900160415         //k_SATlnp = sk_L1(xx);   ?(no: dev'essere vuoto - per i tot.)?
274000160411         k_SATdre = WWWdfv;
274100160323
274200160323         // -?Tipo Rec. "Totale Partenze" (tipo rec. " ")?
274300160411         //k_SATtre = *blank;
274400160411         For  xx = 1  To  %elem( sk_L1 );
274500160411
274600160411           if  sk_L1(xx) = *zero;
274700160411             leave;
274800160411           endif;
274900160411
275000160411           k_SATlnp = sk_L1(xx);
275100160411           chain  %kds( keyFISAT02 )  FISAT000;
275200160411           If  %found( FISAT02L );
275300160729             //*$NoFISAT = *off;
275400170410             V2Dm3i  += SATvne +
275500170410                        SATvnd +
275600170410                        SATvnc +
275700170410                        SATvnh +
275800170410                        SATvnx;
275900160411           EndIf;
276000160411
276100160411         EndFor;
276200160323
276300160323         // -?Tipo Rec. "C/Servizi" (tipo rec. "C")?
276400160411         k_SATtre = 'C';
276500160411         For  xx = 1  To  %elem( sk_L1 );
276600160411
276700160411           if  sk_L1(xx) = *zero;
276800160411             leave;
276900160411           endif;
277000160411
277100160411           k_SATlnp = sk_L1(xx);
277200160411           chain  %kds( keyFISAT02 )  FISAT000;
277300160411           If  %found( FISAT02L );
277400160729             //*$NoFISAT = *off;
277500170410             V2Dm3i  += SATvne +
277600170410                        SATvnd +
277700170410                        SATvnc +
277800170410                        SATvnh +
277900170410                        SATvnx;
278000160411           EndIf;
278100160411
278200160411         EndFor;
278300160323
278400160323         // -?Tipo Rec. "Transiti" (tipo rec. "T")?
278500160323         k_SATtre = 'T';
278600160411         For  xx = 1  To  %elem( sk_L1 );
278700160411
278800160411           if  sk_L1(xx) = *zero;
278900160411             leave;
279000160411           endif;
279100160411
279200160411           k_SATlnp = sk_L1(xx);
279300160411           chain  %kds( keyFISAT02 )  FISAT000;
279400160411           If  %found( FISAT02L );
279500160729             //*$NoFISAT = *off;
279600170410             V2Dm3i  += SATvne +
279700170410                        SATvnd +
279800170410                        SATvnc +
279900170410                        SATvnh +
280000170410                        SATvnx;
280100160411           EndIf;
280200160411
280300160411         EndFor;
280400160322
280500160323       ENDSR;
280600160318
280700160318       //--------------------------------------------------------------
280800160318       //?Calcola le ore e i minuti da minuti totali                   ?
280900160318       //?INPUT  --> §min: minuti totali trascorsi                     ?
281000160318       //?OUTPUT --> §gg:  giorni trascorsi                            ?
281100160318       //?           §hh:  ore    trascorse                            ?
281200160318       //?           §mm:  minuti trascorsi                            ?
281300160318       //--------------------------------------------------------------
281400160321       BEGSR  sr_CalcHML;
281500160318
281600160318         clear  wResto;
281700160318         if  §min >= 1440;     // -?i mm formano più di 1 gg?
281800160318           §gg    = %div( §min : 1440 );      // -?giorni?
281900160318           wResto = %rem( §min : 1440 );      // -?ore/minuti di resto?
282000160318         else;                 // -?i mm non formano gg?
282100160318           wResto = §min;                     // -?ore/minuti?
282200160318         endif;
282300160318
282400160318         §hh = %div( wResto : 60 );      // -?ore lavorate?
282500160318         §mm = %rem( wResto : 60 );      // -?minuti di resto?
282600160318
282700160318       ENDSR;
282800160322
282900160322       //--------------------------------------------------------------
283000160322       //?Richiamo *pgm Letture Smistacolli.                           ?
283100160322       //--------------------------------------------------------------
283200160322       BEGSR  sr_CallFIMS08;
283300160322
283400160322         // -?Richiesta categoria "specifica"?
283500170202         clear  MS13W01;
283600160526         If  V1Capl = 'V'  and  V1Cnpg = 2;
283700160322           exfmt  MS13W01;
283800160322           if  dsp_aid = c_F12;
283900160322             leavesr;
284000160322           endif;
284100160322         EndIf;
284200160322
284300160322         // -?Impostazione dati nella DS-parametri?
284400160322         clear  FIMS00ds;
284500160322         //clear  D00op0;      //?- livello procedura?
284600160322         //clear  D00op1;      //?- livello programma?
284700160322         D00f03 = *off;        //?- premuto F03?
284800160322         D00f12 = *off;        //?- premuto F12?
284900160322         D00err = *off;        //?- errore?
285000160322         //clear  D00msg;      //?- messaggio ritorno?
285100160322         D00fil = V1Cfil;      //?- filiale?
285200170202         //*·if  $Video = 'D5'  or  $Video = 'D4'  or  V1Capl <> 'V';
285300170202         if  V1Capl <> 'V'  or  V1Cnpg = 5;
285400160414           D00apl = V1Capl;    //?- tipo applicazione?
285500160414         else;
285600160414           D00apl = W1Capl;    //?- tipo applicazione?
285700160414         endif;
285800160411         D00dfv = WWWdfv;      //?- data foglio?
285900160322         D00npg = V1Cnpg;      //?- categoria foglio?
286000160322         D00mtd = 1;           //?- min totali dettaglio?
286100160322         D00mtr = 30;          //?- min totali riepilogo?
286200160322         select;
286300170202           //*·when  $Video <> 'D5'  and  $Video <> 'D4'  and
286400170202           //*·      W1Capl =  'V'   and  W1Cnpg =  'A';
286500170202           when  V1Cnpg = 2    and
286600170202                 W1Capl = 'V'  and  W1Cnpg = 'A';
286700160322             D00dli = W2Ddli;
286800160414             D00oli = W2Doli;
286900160322             D00dlf = W2Ddlf;
287000160414             D00olf = W2Dolf;
287100170202           //*·when  $Video <> 'D5'  and  $Video <> 'D4'  and
287200170202           //*·      W1Capl =  'V'   and  W1Cnpg =  'M';
287300170202           when  V1Cnpg = 2     and
287400170202                 W1Capl = 'V'  and  W1Cnpg = 'M';
287500160322             D00dli = W2DdliM;
287600160414             D00oli = W2DoliM;
287700160322             D00dlf = W2DdlfM;
287800160414             D00olf = W2DolfM;
287900170202           //*·when  $Video =  'D5'  or   $Video =  'D4';
288000170202           when  V1Cnpg = 5;
288100160414             D00dli = W2Ddli;
288200160414             D00oli = W2Doli;
288300160414             D00dlf = W2Ddlf;
288400160414             D00olf = W2Dolf;
288500161107           when  WWWdli > *zero  or  WWWdlf > *zero;
288600160411             D00dli = WWWdli;
288700160411             D00oli = V1Coli;
288800161107             //*·if  W2Ddlf >= W2DdlfM;
288900161107             //*·  D00dlf = W2Ddlf;
289000161107             //*·else;
289100161107             //*·  D00dlf = W2DdlfM;
289200161107             //*·endif;
289300161107             //*·D00olf = 2359;
289400161107             D00dlf = WWWdlf;
289500161107             D00olf = V1Colf;
289600160322         endsl;
289700160322
289800160322         // -?Richiamo *pgm?
289900160322         FIMS08R ( kpjba : FIMS00ds );
290000160322
290100160322         // -?Verifica uscita?
290200160322         select;
290300160322           // ·?Premuto F3: ritorno a D01?
290400160322           when  D00f03 = *on;
290500160322             $Video = 'D1';
290600160322           // ·?Premuto F12: si rimane in D02/D03?
290700160322           when  D00f12 = *on;
290800160322           // ·?Restituito errore: lo si visualizza?
290900160322           when  D00msg <> *blank;
291000160322             VIDmsg = D00msg;
291100160322             $ErrMessage  = *on;
291200160322             $ErrGenerico = *on;
291300160322         endsl;
291400160322
291500160322       ENDSR;
291600160412
291700160412       //--------------------------------------------------------------
291800160412       //?Preparazione stringa SQL per estrazione dati da FIPSL00F.    ?
291900160412       //--------------------------------------------------------------
292000160412       BEGSR  sr_PrepSQL;
292100160412
292200160415         wSQL = 'with FIPSL as ( select PSLnpg, PSLbarCo, PSLusr, +
292300160415                                        max(PSLdtOraR) as Data_Ora_Rilev +
292400160415                                   from FIPSL00F +
292500160415                                  where PSLnpg = ' + %editc( V1Cnpg : 'X' ) +
292600160415                                  ' and PSLnfv in (' + wNFV_x_SQL + ') +
292700160415                                    and PSLfil = ' + %editc( V1Cfil : 'X' ) +
292800161215                                  ' and PSLdtOraR between ''' +
292900161215                                        %editc( WWWdli : 'X' ) +
293000161215                                        %editc( V1Coli : 'X' ) + '00'' +
293100161215                                                  and ''' +
293200161215                                        %editc( WWWdlf : 'X' ) +
293300161215                                        %editc( V1Colf : 'X' ) + '99'' +
293400161215                               group by PSLnpg, PSLbarCo, PSLusr +
293500160415                               order by PSLnpg, PSLbarCo, PSLusr +
293600160413                               ) +
293700160413
293800160414               select PSLusr, count(*) +
293900160413                 from FIPSL +
294000160414             group by PSLusr +
294100160413
294200160414             order by PSLusr +
294300160413
294400160413                  for fetch only';
294500160412
294600160412       ENDSR;
294700160412
294800160412       //--------------------------------------------------------------
294900160412       //?Apertura cursore.                                            ?
295000160412       //--------------------------------------------------------------
295100160412       BEGSR  sr_OpenCursor;
295200160412
295300160412         // -?Dichiarazione cursore?
295400160412         exec sql   prepare S6   from :wSQL;
295500160412         exec sql   declare C6   cursor for S6;
295600160412
295700160412         if  SQLcode < *zero;
295800160412           $EoF  = *on;
295900160412           exsr  sr_PrintERR;
296000160412         endif;
296100160412
296200160412         // -?Apertura del cursore?
296300160412         exec sql   open C6;
296400160412
296500160412         if  SQLcode < *zero;
296600160412           $EoF  = *on;
296700160412           exsr  sr_PrintERR;
296800160412         endif;
296900160412
297000160412       ENDSR;
297100160412
297200160412       //--------------------------------------------------------------
297300160412       //?Chiusura cursore.                                            ?
297400160412       //--------------------------------------------------------------
297500160412       BEGSR  sr_CloseCursor;
297600160412
297700160412         // -?Chiusura del cursore?
297800160412         exec sql   close C6;
297900160412
298000160412       ENDSR;
298100160412
298200160412       //--------------------------------------------------------------
298300160412       //?Lettura cursore.                                             ?
298400160412       //--------------------------------------------------------------
298500160412       BEGSR  sr_ReadCursor;
298600160412
298700160412         $EoF  = *off;
298800160412         clear  ds_FIPSL6;
298900160412
299000160412         exec sql   fetch next   from C6   into :ds_FIPSL6;
299100160412
299200160412         select;
299300160412           when  SQLcode = 100;
299400160412             $EoF  = *on;
299500160412           when  SQLcode < *zero;
299600160412             $EoF  = *on;
299700160412             exsr  sr_PrintERR;
299800160412         endsl;
299900160412
300000160412       ENDSR;
300100160413
300200160413       //--------------------------------------------------------------
300300160413       //?Preparazione stringa SQL per estrazione dett. da FIPSL00F.   ?
300400160413       //--------------------------------------------------------------
300500160413       BEGSR  sr_PrepSQL7;
300600160413
300700160415         wSql7 = 'with FIPSL as ( select PSLnpg, PSLbarCo, PSLusr, +
300800160415                                         max(PSLdtOraR) as Data_Ora_Rilev +
300900160413                                    from FIPSL00F +
301000160413                                   where PSLnpg = ' + %editc( V1Cnpg : 'X' ) +
301100160413                                   ' and PSLnfv in (' + wNFV_x_SQL + ') +
301200161215                                     and PSLfil = ' + %editc( V1Cfil : 'X' ) +
301300161215                                   ' and PSLdtOraR between ''' +
301400161215                                         %editc( WWWdli : 'X' ) +
301500161215                                         %editc( V1Coli : 'X' ) + '00'' +
301600161215                                                   and ''' +
301700161215                                         %editc( WWWdlf : 'X' ) +
301800161215                                         %editc( V1Colf : 'X' ) + '99''';
301900160413
302000160413         if  dsp_aid <> c_F10  and
302100160413             S06opz  =  '5';
302200160413           wSql7 +=                ' and PSLusr = ''' + H06usr + '''';
302300160413         endif;
302400160413
302500160415         wSql7 +=             ' group by PSLnpg, PSLbarCo, PSLusr +
302600160415                                order by PSLnpg, PSLbarCo, PSLusr +
302700160413                                ) +
302800161215
302900160415               select FIPSL00F.PSLnpg, FIPSL00F.PSLnfv, FIPSL00F.PSLbarCo, +
303000160415                      FIPSL00F.PSLusr, FIPSL00F.PSLdtOraR +
303100160415
303200160415                 from FIPSL00F inner join FIPSL +
303300160415                   on FIPSL00F.PSLnpg    = FIPSL.PSLnpg +
303400160415                  and FIPSL00F.PSLbarCo  = FIPSL.PSLbarCo +
303500160415                  and FIPSL00F.PSLusr    = FIPSL.PSLusr +
303600160415                  and FIPSL00F.PSLdtOraR = FIPSL.Data_Ora_Rilev +
303700160415
303800160415             order by PSLnpg, PSLbarCo, PSLdtOraR, PSLnfv, PSLusr +
303900160413
304000160413                  for fetch only';
304100160413
304200160413       ENDSR;
304300160413
304400160413       //--------------------------------------------------------------
304500160413       //?Apertura cursore.                                            ?
304600160413       //--------------------------------------------------------------
304700160413       BEGSR  sr_OpenCursor7;
304800160413
304900160413         // -?Dichiarazione cursore?
305000160413         exec sql   prepare S7   from :wSql7;
305100160413         exec sql   declare C7   cursor for S7;
305200160413
305300160413         if  SQLcode < *zero;
305400160413           $EoF7 = *on;
305500160413           exsr  sr_PrintERR;
305600160413         endif;
305700160413
305800160413         // -?Apertura del cursore?
305900160413         exec sql   open C7;
306000160413
306100160413         if  SQLcode < *zero;
306200160413           $EoF7 = *on;
306300160413           exsr  sr_PrintERR;
306400160413         endif;
306500160413
306600160413       ENDSR;
306700160413
306800160413       //--------------------------------------------------------------
306900160413       //?Chiusura cursore.                                            ?
307000160413       //--------------------------------------------------------------
307100160413       BEGSR  sr_CloseCursor7;
307200160413
307300160413         // -?Chiusura del cursore?
307400160413         exec sql   close C7;
307500160413
307600160413       ENDSR;
307700160413
307800160413       //--------------------------------------------------------------
307900160413       //?Lettura cursore.                                             ?
308000160413       //--------------------------------------------------------------
308100160413       BEGSR  sr_ReadCursor7;
308200160413
308300160413         $EoF7 = *off;
308400160413         clear  ds_FIPSL7;
308500160413
308600160413         exec sql   fetch next   from C7   into :ds_FIPSL7;
308700160413
308800160413         select;
308900160413           when  SQLcode = 100;
309000160413             $EoF7 = *on;
309100160413           when  SQLcode < *zero;
309200160413             $EoF7 = *on;
309300160413             exsr  sr_PrintERR;
309400160413         endsl;
309500160413
309600160413       ENDSR;
309700160412
309800160412       //--------------------------------------------------------------
309900160412       //?Stampa segnalazione dell'errore rilevato via SQL             ?
310000160412       //--------------------------------------------------------------
310100160412       BEGSR  sr_PrintERR;
310200160412
310300160412         // -?Stampa del Dump?
310400160412         Dump(A);
310500160412
310600160412         // -?Stampa del Job-Log?
310700160412         Qcmd = 'DSPJOBLOG job(*) output(*print)';
310800160412         exsr  sr_ExecCmd;
310900160412
311000160412         // -?Chiusura del programma?
311100160412         exsr  sr_RoutEnd;
311200160412
311300160412       ENDSR;
311400160412
311500160412       //--------------------------------------------------------------
311600160412       //?Esecuzione del comando (già impostato).                      ?
311700160412       //--------------------------------------------------------------
311800160412       BEGSR  sr_ExecCmd;
311900160412
312000160412         clear Qcap0100;
312100160412         Qcabcsdh = *off;
312200160412         Qcapa    = *off;
312300160412         Qcacmdss = *off;
312400160412         Qcaerved = *allX'00';
312500160412
312600160412         clear Qusec;
312700160412         Qusbprv  = %size(Qusec);
312800160412
312900160412         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
313000160412                           %size(Qcap0100) : 'CPOP0100' : *omit :
313100160412                           0 : 0 : Qusec);
313200160412
313300160412         //if  Qusei <> *blank;
313400160412         //  ...;
313500160412         //endif;
313600160412
313700160412       ENDSR;
313800160318
313900160318       //--------------------------------------------------------------
314000160318       //?Operazioni finali.                                           ?
314100160318       //--------------------------------------------------------------
314200160318       BEGSR  sr_RoutEnd;
314300160321
314400160321         // -?Chiusura funzioni precedentemente aperte?
314500160321         cloTabella ( c_Tntbe );
314600160413
314700160413         // -?Chiusura archivi?
314800170201         close  FNMSA02L;
314900160413         close  FISAR06L;
315000160413         close  FISAT02L;
315100160413         close  FNFVV02L;
315200160318
315300160318         // -?Uscita dal *pgm?
315400160318         return;
315500160318
315600160318       ENDSR;
315700160318
315800160318      /end-free
315900160318
316000160318       //--------------------------------------------------------------
316100160318       //?Definizione schiere a tempo di compilazione                  ?
316200160318       //--------------------------------------------------------------
316300160318
316400160318** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
316500160412Funzione eseguibile SOLO in ambiente di Filiale: premere "Invio" per uscire     1
316600160318Utente NON abilitato alla funzione                                              2
316700160318Filiale errata                                                                  3
316800160318Filiale NON di 1° livello                                                       4
316900160318Tipo Applicazione errato                                                        5
317000161107Data Foglio formalmente errata                                                  6
317100160318Categoria Foglio errata                                                         7
317200160408Data Lettura formalmente errata                                                 8
317300160408Ora Lettura formalmente errata                                                  9
317400160408Valore NON ammesso per il campo                                                10
317500160412Raggiunto il n° massimo delle spunte interrogabili a video                     11
317600160412Opzione errata                                                                 12
317700160415Rilevato errore durante l'estrazione dei dati: avvisare CED                    13
317800160711Filiale non prevista per il Report Magazzino                                   14
317900161107Orario di lettura iniziale successivo all'orario di lettura finale             15
