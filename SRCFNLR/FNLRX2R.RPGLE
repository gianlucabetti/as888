000100131030       //==============================================================
000200131121       //?Gestione Telefonate ad Autotrasportatori.                    ?
000300131030       //==============================================================
000400131030
000500131030       //--------------------------------------------------------------
000600131030       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700131030       //--------------------------------------------------------------
000800131030
000900131121     /*PRM  dbgview(*source)
001000131030     /*END
001100131030
001200131030       //--------------------------------------------------------------
001300131030       //?Specifiche di controllo.                                     ?
001400131030       //--------------------------------------------------------------
001500131030
001600131030     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001700131122     h dftactgrp(*no)
001800131122     h bnddir('TRUL')
001900131030
002000131030       //--------------------------------------------------------------
002100131030       //?Dichiarazione file.                                          ?
002200131030       //--------------------------------------------------------------
002300131125
002400131125       // -?Tabelle?
002500131125     fTNTBE01L  if   e           k disk
002600131122
002700131120       // -?Anagrafica Autotrasportatori/Cooperative?
002800131120     fFIAPD01L  if   e           k disk
002900131121
003000131121       // -?Estensione Anagrafica Autotrasportatori - dati aggiuntivi?
003100140131     fFIAPD41L  if   e           k disk
003200131030
003300131030       // -?Bolle Arrivi: Testate?
003400131121     fFNARB01L  if   e           k disk
003500131030
003600131030       // -?Distinte Uscita AUT Consegne/Ritiri?
003700131204     fFIDST01L  if   e           k disk
003800131121
003900131121       // -?Estensione testata bolla?
004000131121     fFIAR501L  if   e           k disk
004100131121
004200131121       // -?Tassazione Bolla?
004300131121     fFIAR601L  if   e           k disk
004400131211
004500131211       // -?Dati C/Assegno?
004600131211     fFIAR901L  if   e           k disk
004700131030
004800131211       // -?Estensione Descrizioni: Note LdV ed Estens. Rag.Soc.Dest.?
004900131122     fFIAR401L  if   e           k disk
005000131211       // -?Estensione Descrizioni: Spedizione abbinata all'O.R.M.?
005100131211       //  ?per reperimento Importo O.R.M. Prepagato (da FIAR6)?
005200131210     fFIAR404L  if   e           k disk    rename( FIAR4000 : FIAR4004 )
005300131126
005400131126       // -?Bolle Arrivi: Note?
005500131126     fFIARN12L  if   e           k disk
005600131120
005700131120       // -?Ordini Ritiro Merce?
005800131122     fFNORM01L  if   e           k disk
005900131211
006000131211       // -?Ordini Ritiro Merce: Estensione?
006100131211     fFNORE01L  if   e           k disk
006200140121
006300140121       // -?Ordini Ritiro Merce: Note AUT?
006400140121     fFNORT01L  if   e           k disk
006500140206
006600140206       // -?Archivio contatti AUT (per Distinta/AUT)?
006700140206     fFICAU02L  if   e           k disk    rename( FICAU000 : FICAU002 )
006800140206     f                                     prefix( CA2 : 3 )
006900131205
007000131205
007100140206       // -?Archivio contatti AUT (per AUT/Data_Ora_ins)?
007200131205     fFICAU01L  Uf   e           k disk
007300140121
007400140131       // -?O.R.M. Estensione testata - GeoRefer/Giri?
007500140121     fFNORG01L  Uf   e           k disk
007600131030
007700131205
007800131030       // -?Video?
007900131129     fFNLRX2D   cf   e             workstn
008000131120     f                                     sfile(LRX2S01 : S01nrr)
008100131030     f                                     indds(IndDspF)
008200131030     f                                     infds(InfDspF)
008300131030
008400131030       //--------------------------------------------------------------
008500131030       //?Definizione costanti.                                        ?
008600131030       //--------------------------------------------------------------
008700140131
008800140131       // -?Comandi?
008900140131     d c_Cmd_Inserito  c                   const('I')
009000131121
009100131121       // -?Tipi Oggetto Telefonate?
009200131121     d c_Consegna      c                   const('C')
009300131121     d c_Ritiro        c                   const('R')
009400131205
009500131211       // -?Tipi Oggetto Reclamo?
009600131205     d c_Spedizione    c                   const('S')
009700131205     d c_ORM           c                   const('O')
009800131205
009900131205       // -?Causali Esito Telefonata?
010000131205     d c_Eseguita      c                   const('ES')
010100131205     d c_Rifiutata     c                   const('RF')
010200131211
010300131211       // -?Motivo Apertura Richiesta Assistenza "cieca"?
010400140506     d c_IDA06mad      c                   const(' 40')
010500131122
010600131122       // -?Numero di record in ciascuna pagina di subfile?
010700140131     d c_SflPag        c                   const(14)
010800131122
010900131122       // -?Numero massimo di record gestibili nel subfile?
011000131122     d c_MaxRec        c                   const(9999)
011100131030
011200131030       // -?Costante per controllo "caratteri solo numerici"?
011300131030     d c_Digits        c                   const('0123456789')
011400140203
011500140203       // -?Costanti per conversione caratteri maiuscoli in minuscoli e viceversa?
011600140203     d c_Up            c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
011700140203     d c_Lo            c                   const('abcdefghijklmnopqrstuvwxyz')
011800131030
011900131030       // -?Tasti funzionali a video?
012000131030     d c_F01           c                   const(x'31')
012100131030     d c_F02           c                   const(x'32')
012200131030     d c_F03           c                   const(x'33')
012300131030     d c_F04           c                   const(x'34')
012400131030     d c_F05           c                   const(x'35')
012500131030     d c_F06           c                   const(x'36')
012600131030     d c_F07           c                   const(x'37')
012700131030     d c_F08           c                   const(x'38')
012800131030     d c_F09           c                   const(x'39')
012900131030     d c_F10           c                   const(x'3A')
013000131030     d c_F11           c                   const(x'3B')
013100131030     d c_F12           c                   const(x'3C')
013200131030     d c_F13           c                   const(x'B1')
013300131030     d c_F14           c                   const(x'B2')
013400131030     d c_F15           c                   const(x'B3')
013500131030     d c_F16           c                   const(x'B4')
013600131030     d c_F17           c                   const(x'B5')
013700131030     d c_F18           c                   const(x'B6')
013800131030     d c_F19           c                   const(x'B7')
013900131030     d c_F20           c                   const(x'B8')
014000131030     d c_F21           c                   const(x'B9')
014100131030     d c_F22           c                   const(x'BA')
014200131030     d c_F23           c                   const(x'BB')
014300131030     d c_F24           c                   const(x'BC')
014400131030     d c_Enter         c                   const(x'F1')
014500131030     d c_RollDown      c                   const(x'F4')
014600131030     d c_RollUp        c                   const(x'F5')
014700131030
014800131030       //--------------------------------------------------------------
014900131030       //?Definizione schiere.                                         ?
015000131030       //--------------------------------------------------------------
015100131030
015200131030       // -?Messaggi di errore?
015300140203     d sk_Msg          s             78    dim( 4)  ctdata  perrcd( 1)
015400131125
015500131125       // -?Tipologiche Causali Variazioni bolle?
015600131125     d sk_TCV          s              1    dim(10)  inz
015700131125     d sk_TCVd         s              4    dim(10)  inz
015800131030
015900131030       //--------------------------------------------------------------
016000131030       //?Definizione aree dati.                                       ?
016100131030       //--------------------------------------------------------------
016200131030
016300131030       // -?Dati utente?
016400131030     d §AzUte        e ds                  extname(AZUTE00F)
016500131030     d                                     dtaara
016600131030     d §DatiUte      e ds                  extname(dDatiUte)
016700131030     d                                     dtaara
016800131030
016900131030       //--------------------------------------------------------------
017000131030       //?Definizione strutture dati.                                  ?
017100131030       //--------------------------------------------------------------
017200131030
017300131030       // -?Status ds?
017400131030     d Status         sds
017500131030     d   SDSpgmName      *proc
017600131205     d*//SDSparms        *parms
017700131205     d   SDSjobName          244    253                                         Job name
017800131205     d   SDSjobUser          254    263                                         User name
017900131205     d*//SDSjobNumber        264    269s 0                                      Job number
018000131030
018100131030       // -?InfDS?
018200131030     d InfDspF         ds
018300131030     d   dsp_aid             369    369a
018400131030     d   sfl_rrn             376    377i 0
018500131030     d   min_nrr             378    379i 0
018600131030     d   num_rcds            380    381i 0
018700131030
018800131030       // -?Indicatori su DspF?
018900131030     d IndDspF         ds                  inz
019000131121         // -?Abilitazione tasti funzionali?
019100131128     d   F1Attivo                      n   overlay(IndDspF :  1)
019200131204     d   F9Attivo                      n   overlay(IndDspF :  9)
019300131205     d   F11Attivo                     n   overlay(IndDspF : 11)
019400131030         // -?Emissione messaggio di errore?
019500131030     d   ErrMessage                    n   overlay(IndDspF : 28)
019600131121         // -?Indicatori di gestione del subfile 1?
019700131030     d   SflDsp_N                      n   overlay(IndDspF : 30)
019800131030     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
019900131030     d   SflNxtChg                     n   overlay(IndDspF : 32)
020000131030     d   SflEnd                        n   overlay(IndDspF : 33)
020100131209         // -?Indicatori per Attibuti di visualizzazione videata DS1?
020200131206     d   Ord_x_OraIns                  n   overlay(IndDspF : 39)
020300131204     d   RevImgUCO                     n   overlay(IndDspF : 40)
020400131204     d   RevImgCMD                     n   overlay(IndDspF : 41)
020500131204     d   RevImgVAR                     n   overlay(IndDspF : 42)
020600131204     d   RevImgTSP                     n   overlay(IndDspF : 43)
020700131204     d   VisualFDEP                    n   overlay(IndDspF : 44)
020800131129     d   VisPartGiac                   n   overlay(IndDspF : 45)
020900131129     d   VisPartCons                   n   overlay(IndDspF : 46)
021000131129     d   VisPartVarie                  n   overlay(IndDspF : 47)
021100131204     d   VisualCASS                    n   overlay(IndDspF : 48)
021200131204     d   VisualIFT                     n   overlay(IndDspF : 49)
021300131209         // -?Posizionamento cursore nel Sfl & segnalazione errore?
021400131121     d   PosCurOPZ                     n   overlay(IndDspF : 50)
021500131209         // -?Indicatori per Attibuti di visualizzazione videata DO1?
021600131209     d   VisualORMPP                   n   overlay(IndDspF : 51)
021700131209     d   VisualORMRR                   n   overlay(IndDspF : 52)
021800131030         //   -?Riemissione videata?
021900131030     d   ErrGenerico                   n   overlay(IndDspF : 99)
022000140203
022100140203       // -?Salvataggio campi del fmt DS1 (Spedizioni)?
022200140203     d SaveDS1         ds                  inz   qualified
022300140203     d   V1Cpdr                            inz   like(V1Cpdr)
022400140203     d   V1Dpdr                            inz   like(V1Dpdr)
022500140203     d   V1Duco                            inz   like(V1Duco)
022600140203     d   V1Dcmd                            inz   like(V1Dcmd)
022700140203     d   V1Dvar                            inz   like(V1Dvar)
022800140203     d   V1Clnp                            inz   like(V1Clnp)
022900140203     d   V1Cnrs                            inz   like(V1Cnrs)
023000140203     d   V1Cnsp                            inz   like(V1Cnsp)
023100140203     d   V1Dtsp                            inz   like(V1Dtsp)
023200140203     d   V1Drsd                            inz   like(V1Drsd)
023300140203     d   V1Dind                            inz   like(V1Dind)
023400140203     d   V1Dlpd                            inz   like(V1Dlpd)
023500140203     d   V1Cncl                            inz   like(V1Cncl)
023600140203     d   V1Cpkb                            inz   like(V1Cpkb)
023700140203     d   V1Dref                            inz   like(V1Dref)
023800140203     d*//V1Igga                            inz   like(V1Igga)
023900140203     d   V1Cgga                            inz   like(V1Cgga)
024000140203     d*//V1Igma                            inz   like(V1Igma)
024100140203     d   V1Cgma                            inz   like(V1Cgma)
024200140203     d*//V1Igva                            inz   like(V1Igva)
024300140203     d   V1Cgva                            inz   like(V1Cgva)
024400140203     d   V1Ccas                            inz   like(V1Ccas)
024500140203     d   V1Cvca                            inz   like(V1Cvca)
024600140203     d   V1Cift                            inz   like(V1Cift)
024700140203     d   V1Cdiv                            inz   like(V1Cdiv)
024800140203     d   V1Dnot8                           inz   like(V1Dnot8)
024900140203     d   V1Dnot9                           inz   like(V1Dnot9)
025000140203     d   V1Cnt1                            inz   like(V1Cnt1)
025100140203     d   V1Cnt2                            inz   like(V1Cnt2)
025200140203     d   V1Cnt3                            inz   like(V1Cnt3)
025300140203     d   V1Cnt4                            inz   like(V1Cnt4)
025400140203     d   V1nra1                            inz   like(V1nra1)
025500140203     d   V1nra2                            inz   like(V1nra2)
025600140203
025700140203       // -?Salvataggio campi del fmt DO1 (O.R.M.)?
025800140203     d SaveDO1         ds                  inz   qualified
025900140203     d   V1Cpdr                            inz   like(V1Cpdr)
026000140203     d   V1Dpdr                            inz   like(V1Dpdr)
026100140203     d   V1Duco                            inz   like(V1Duco)
026200140203     d   V1Dcmd                            inz   like(V1Dcmd)
026300140203     d   V1Dvar                            inz   like(V1Dvar)
026400140203     d   V1Cpoe                            inz   like(V1Cpoe)
026500140203     d   V1Cnsr                            inz   like(V1Cnsr)
026600140203     d   V1Cnor                            inz   like(V1Cnor)
026700140203     d   V1Cnrv                            inz   like(V1Cnrv)
026800140203     d   V1oraAMda                         inz   like(V1oraAMda)
026900140203     d   V1oraAMa                          inz   like(V1oraAMa)
027000140203     d*//V1oraSep                          inz   like(V1oraSep)
027100140203     d   V1oraPMda                         inz   like(V1oraPMda)
027200140203     d   V1oraPMa                          inz   like(V1oraPMa)
027300140203     d   V1datN                            inz   like(V1datN)
027400140203     d   V1Drsr                            inz   like(V1Drsr)
027500140203     d   V1Dinr                            inz   like(V1Dinr)
027600140203     d   V1Dlor                            inz   like(V1Dlor)
027700140203     d   V1Dprr                            inz   like(V1Dprr)
027800140203     d   V1Dcar                            inz   like(V1Dcar)
027900140203     d   V1Dnar                            inz   like(V1Dnar)
028000140203     d   V1orr                             inz   like(V1orr)
028100140203     d   V1Dref                            inz   like(V1Dref)
028200140203     d   V1Cncl                            inz   like(V1Cncl)
028300140203     d   V1Cpkg                            inz   like(V1Cpkg)
028400140203     d   V1Cbnc                            inz   like(V1Cbnc)
028500140203     d   V1Cvlm                            inz   like(V1Cvlm)
028600140203     d   V1Cspi                            inz   like(V1Cspi)
028700140203     d   V1Dnot1                           inz   like(V1Dnot1)
028800140203     d   V1Dnot2                           inz   like(V1Dnot2)
028900140203     d   V1Cnt1                            inz   like(V1Cnt1)
029000140203     d   V1Cnt2                            inz   like(V1Cnt2)
029100140203     d   V1nra1                            inz   like(V1nra1)
029200140203     d   V1nra2                            inz   like(V1nra2)
029300131121
029400131121       // -?DS per Numero Spedizione?
029500131125     d ds_Spedizione   ds                  inz   qualified
029600131125     d   wLNP                         3s 0 inz
029700131125     d   wNRS                         2s 0 inz
029800131125     d   wNSP                         7s 0 inz
029900131125     d   wAAS                         4s 0 inz
030000131121
030100131121       // -?DS per Numero O.R.M.?
030200131125     d ds_ORM          ds                  inz   qualified
030300131125     d   wPOE                         3s 0 inz
030400131125     d   wNSR                         2s 0 inz
030500131125     d   wNOR                         7s 0 inz
030600131125     d   wNRV                         2s 0 inz
030700131122
030800131122       // -?DS per Data + Ora?
030900131129     d ds_DataOra      ds                  inz   qualified
031000131125     d   wDate                        8s 0 inz
031100131125     d   wTime                        6s 0 inz
031200131129     d     wTime4              9     12s 0 inz
031300131125
031400131125       // -?Tipologiche Causali Variazioni bolle?
031500131125     d ds_TVA          ds                  inz
031600140213     d   sk_TVA                       1    inz  overlay(ds_TVA : 1)
031700131125     d                                     dim(10)
031800131030
031900131030       // -?Parametri ricevuti?
032000131030     d KPJBA         e ds
032100131125     d FNLRX2ds      e ds                  inz   qualified
032200131120
032300131120       // -?Tabella CMD = Comandi da impartire al PDA o all'AUT?
032400131127     d dCMD          e ds                  inz
032500131120       // -?Tabella TCV = Tipologie Causali Variazioni bolle?
032600131127     d dTCV          e ds                  inz
032700131121       // -?Tabella 3A = Codici Bolla?
032800131204     d*// ds3A          e ds                  inz
032900131121       // -?Tabella 5E = Tipi Servizio?
033000131121     d ds5E          e ds                  inz
033100140130
033200140130       // -?DS campo DSTFLR di FIDST00F?
033300140130     d dDSTflr       e ds                  inz   qualified
033400131121
033500131121       // -?DS Tipo record "TEL" di FIAPD40F?
033600140131     d dAPD4tel      e ds                  inz   qualified
033700131122
033800131122       // -?DS Tipo record "GEN" di FIAR500F (Estensione)?
033900131125     d dAR5gen       e ds                  inz   qualified
034000131210
034100131210       // -?Flag operativi di FNORM00F?
034200131210     d dORM01        e ds                  inz   qualified
034300140121
034400140121       // -?Flag operativi di FNORG00F?
034500140121     d dORG01        e ds                  inz   qualified
034600140506
034700140506       // -?Flag operativi di FITGD00F.TGDflo?
034800140506     d dTGD01        e ds                  inz   qualified
034900131211
035000131211       // -?Ds per rcd "O " FNORE - orari apertura?
035100131211     d dOREorari     e ds                  inz   qualified
035200131030
035300131030       //--------------------------------------------------------------
035400131030       //?Definizione variabili globali.                               ?
035500131030       //--------------------------------------------------------------
035600131030
035700131030       // -?Flags booleani?
035800131030     d $Fine           s               n   inz
035900131030     d $EoF            s               n   inz
036000131030     d $InzS01         s               n   inz(*on)
036100140203     d $InzD01         s               n   inz(*on)
036200131128     d $InzW01         s               n   inz(*on)
036300140226     d $InzW02         s               n   inz(*on)
036400131121     d $RecOK          s               n   inz
036500131128     d $UltContPrec    s               n   inz
036600140203     d $RecUpdated     s               n   inz
036700131122
036800131122       // -?Indici di schiera?
036900131122     d xx              s              3  0 inz
037000131125     d yy              s              3  0 inz
037100131030
037200131030       // -?Variabili per la gestione del video?
037300131125     d $Video          s              2    inz('S1')
037400131104     d w_SflPag        s              3  0 inz
037500131030     d wPag            s              4  0 inz
037600131030     d wRig            s              3  0 inz
037700140108     d wOrdSfl         s              1  0 inz
037800131030     d S01nrr          s                   like(C1RcdNbr) inz
037900131030     d SavS01csr       s                   like(C1RcdNbr) inz
038000131212     d SavMAXnrr       s                   like(C1RcdNbr) inz
038100140226     d W2rig           s              3  0 inz(15)
038200140226     d*// W2col           s              3  0 inz(54)
038300131030
038400131030       // -?Variabili di comodo?
038500131121     d wDate_EUR       s               d   datfmt(*eur)  inz(*loval)
038600131205     d wTimeStamp      s               z   inz(*loval)
038700131127     d wTipTVA         s              1    inz
038800131210     d w0112           s             11  2 inz
038900140108     d wDesTVA         s                   like(V1Dvar)   inz
039000131204
039100131204       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
039200131204
039300131204       //--------------------------------------------------------------
039400131204       // -?Constants?
039500131204       //--------------------------------------------------------------
039600131204       // -?MaxKey - Maximum number of key fields allowed by this program?
039700131204     d MaxKey          c                   const(2)
039800131204       // -?Sort order:?
039900131204       //  ?1 = Sort in an ascending sequence?
040000131204       //  ?2 = Sort in a descending sequence?
040100131204     d Ascendente      c                   const(1)
040200131204     d Discendente     c                   const(2)
040300131204       // -?Key data type:?
040400131204       //  ? 0 = Signed binary?
040500131204       //  ? 2 = Signed zoned decimal?
040600131204       //  ? 3 = Signed packed decimal?
040700131204       //  ? 6 = Character with no national language sort sequence applied,?
040800131204       //  ?     if specified?
040900131204       //  ? 7 = Unsigned packed decimal?
041000131204       //  ?     All numbers will have the sign forced positive ('F'X).?
041100131204       //  ? 8 = Unsigned zoned decimal?
041200131204       //  ?     All numbers will have the sign forced positive ('F'X).?
041300131204       //  ? 9 = Unsigned binary?
041400131204       //  ?14 = Date in form of DD/MM/YY?
041500131204       //  ?15 = Date in form of DD.MM.YYYY?
041600131204     d Numero          c                   const(2)
041700131204     d Carattere       c                   const(6)
041800131204       //
041900131204     d Put             c                   const(1)
042000131204     d EndPut          c                   const(2)
042100131204     d Get             c                   const(3)
042200131204
042300131204       //--------------------------------------------------------------
042400131204       // -?Data Structures?
042500131204       //--------------------------------------------------------------
042600131204       //  ?SflRcd     - The entire subfile record to pass to the sort?
042700131204     d SflRcd          ds                  inz
042800131204     d   S1Hfgs                            inz
042900131204     d   S1HdtOins                         inz
043000131204     d   S1Htor                            inz
043100131204     d   S1Hogg                            inz
043200140110     d   S1Hcmd                            inz
043300131204     d   S01opz                            inz
043400140110     d   S01tor                            inz
043500131204     d   S01cmd                            inz
043600131204     d   S01var                            inz
043700131204     d   S01rsc                            inz
043800131206     d   S01hmi                            inz
043900131204     d   S01ogg                            inz
044000131204     d   Selected                     1a   inz
044100131204       //  ?QLGSCB     - The sort request block for the QLGSORT API?
044200131204      /copy QSYSINC/QRPGLESRC,QLGSORT
044300131204       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
044400131204     d QLGKL                         16    dim(MaxKey)
044500131204     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
044600131204     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
044700131204     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
044800131204     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
044900131204       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
045000131204      /copy QSYSINC/QRPGLESRC,QLGSRTIO
045100131204       //  ?QUSEC      - Error structure for the QLGSORT API?
045200131204      /copy QSYSINC/QRPGLESRC,QUSEC
045300131204
045400131204       //--------------------------------------------------------------
045500131204       // -?Standalone fields?
045600131204       //--------------------------------------------------------------
045700131204     d NotUsed         s             16a   inz
045800131204       //  ?ReturnSize - Size of list returned by sort API?
045900131204     d ReturnSize      s              9b 0 inz
046000131204       //  ?SizeList   - Size of list?
046100131204     d SizeList        s              9b 0 inz
046200131204       //  ?Nrr        - Relative record number for subfile?
046300131204     d Nrr             s              5i 0 inz
046400131204     d RrnLast         s              5i 0 inz
046500131030
046600131030       //--------------------------------------------------------------
046700131030       //?Definizione prototipi procedure.                             ?
046800131030       //--------------------------------------------------------------
046900131030
047000131030       // -?Reperimento dati utente?
047100131030     d TIBS34ds      e ds                  inz
047200131030      /copy gaitrasrc/srcProtoPR,TIBS34R
047300131030
047400131120       // -?Reperimento dati tabelle?
047500131120      /copy gaitrasrc/srcProtoPI,TRULTAB
047600131120      /copy gaitrasrc/srcProtoPR,TRULTAB
047700131120
047800131120       // -?Controllo/Decodifica cliente?
047900131120      /copy gaitrasrc/srcProtoPI,TIBS69R
048000131120      /copy gaitrasrc/srcProtoPR,TIBS69R
048100131121
048200131121       // -?Interrogazione Bolle Arrivi?
048300131121      /copy gaitrasrc/srcProtoPI,FNLR36R
048400131121      /copy gaitrasrc/srcProtoPR,FNLR36R
048500131128
048600131128       // -?Interrogazione Particolarità?
048700131128     d DS7PQRS       e ds                  inz   qualified
048800131128     d   DS7ric      e                     inz('S')
048900131128     d   DS7ges      e                     inz('V')
049000131128       // -?       "             "       Giacenza?
049100131128     d trtb69r         pr                  extpgm('TRTB69R')
049200131128     d   kpjba                             likeds(KPJBA)
049300131128       // -?       "             "       Consegna?
049400131128     d trtb70r         pr                  extpgm('TRTB70R')
049500131128     d   kpjba                             likeds(KPJBA)
049600131128       // -?       "             "       Varie?
049700131128     d trtb73r         pr                  extpgm('TRTB73R')
049800131128     d   kpjba                             likeds(KPJBA)
049900131122
050000131122       // -?Interrogazione O.R.M.?
050100131122     d Parm01          ds                  inz   qualified
050200131125     d   ppoe                         3  0 inz
050300131125     d   pnor                         7  0 inz
050400131125     d   pnsr                         2  0 inz
050500131125     d   pnrv                         2  0 inz
050600131125     d   psce                         1    inz
050700131125     d   pfgs                         3  0 inz
050800131125     d   ppor                         3  0 inz
050900131125     d   pdtr                         8  0 inz
051000131125     d   pmdb                        10    inz
051100131125     d   pprb                        10    inz
051200131125     d   pdts                         8  0 inz
051300131125     d   prmp                         1    inz
051400131125     d   pbrc                         1    inz
051500131125     d   pref                         2    inz
051600131125     d   pmio                         1    inz
051700131122     d FIDNA1ds      e ds                  inz   qualified
051800131122     d fior07r         pr                  extpgm('FIOR07R')
051900131122     d   kpjba                             likeds(KPJBA)
052000131122     d   fidnA1ds                          likeds(FIDNA1ds)
052100140130
052200140130       // -?Monitor attività autotrasportatore con PDA?
052300140130     d FNLVP0ds      e ds                  inz
052400140130      /copy gaitrasrc/srcProtoPR,FNLVP0R2
052500140130
052600140130       // -?Dettaglio Interrogazione DISTINTE?
052700140130     d FIDG311ds     e ds                  inz
052800140130      /copy gaitrasrc/srcProtoPR,FIDG311R
052900131205
053000131205       // -?Scrittura Richiesta Assistenza?
053100140506     d FIDNA6ds      e ds                  inz
053200140506     d fidnA6r         pr                  extpgm('FIDNA6R')
053300140506     d   fidnA6ds                          likeds(FIDNA6ds)
053400131204
053500131204       // -?Ordinamento subfile?
053600131204      /copy gaitrasrc/srcProtoPR,QLGSRTIO
053700131030
053800131030       //--------------------------------------------------------------
053900131030       //?Definizione key-list.                                        ?
054000131030       //--------------------------------------------------------------
054100131030
054200131120       // -?File FICAU01L?
054300131120     d keyFICAU01    e ds                  extname(FICAU01L : *key)
054400131126     d                                     prefix(k_)    inz
054500140206       // -?File FICAU02L?
054600140206     d keyFICAU02    e ds                  extname(FICAU02L : *key)
054700140206     d                                     prefix(k_CA2 : 3)   inz
054800131120
054900131120       // -?File FIAPD01L?
055000131120     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
055100131126     d                                     prefix(k_)    inz
055200131121
055300131121       // -?File FIAPD41L?
055400140131     d keyFIAPD41    e ds                  extname(FIAPD41L : *key)
055500140131     d                                     prefix(k_)    inz
055600131030
055700131030       // -?File FNARB01L?
055800131030     d keyFNARB01    e ds                  extname(FNARB01L : *key)
055900131126     d                                     prefix(k_)    inz
056000131030
056100131030       // -?File FIDST01L?
056200131204     d keyFIDST01    e ds                  extname(FIDST01L : *key)
056300131204     d                                     prefix(k_)    inz
056400131126
056500131126       // -?File FIARN12L?
056600131126     d keyFIARN12    e ds                  extname(FIARN12L : *key)
056700131126     d                                     prefix(k_)    inz
056800131030
056900131122       // -?File FIAR401L?
057000131122     d keyFIAR401    e ds                  extname(FIAR401L : *key)
057100131126     d                                     prefix(k_)    inz
057200131210       // -?File FIAR404L?
057300131210     d keyFIAR404    e ds                  extname(FIAR404L : *key)
057400131210     d                                     prefix(k_AR44:3)    inz
057500131122
057600131122       // -?File FIAR501L?
057700131122     d keyFIAR501    e ds                  extname(FIAR501L : *key)
057800131126     d                                     prefix(k_)    inz
057900131122
058000131122       // -?File FIAR601L?
058100131122     d keyFIAR601    e ds                  extname(FIAR601L : *key)
058200131126     d                                     prefix(k_)    inz
058300131122
058400131122       // -?File FIAR901L?
058500131122     d keyFIAR901    e ds                  extname(FIAR901L : *key)
058600131126     d                                     prefix(k_)    inz
058700131120
058800131120       // -?File FNORM01L?
058900131122     d keyFNORM01    e ds                  extname(FNORM01L : *key)
059000131126     d                                     prefix(k_)    inz
059100140121
059200140121       // -?File FNORT01L?
059300140121     d keyFNORT01    e ds                  extname(FNORT01L : *key)
059400140121     d                                     prefix(k_)    inz
059500140121
059600140121       // -?File FNORG01L?
059700140121     d keyFNORG01    e ds                  extname(FNORG01L : *key)
059800140121     d                                     prefix(k_)    inz
059900131211
060000131211       // -?File FNORE01L?
060100131211     d keyFNORE01    e ds                  extname(FNORE01L : *key)
060200131211     d                                     prefix(k_)    inz
060300131030
060400131030       //--------------------------------------------------------------
060500131030       //?Riepilogo indicatori utilizzati.                             ?
060600131030       //--------------------------------------------------------------
060700131030
060800131030
060900131030       //--------------------------------------------------------------
061000131030       //?M A I N - L I N E                                            ?
061100131030       //--------------------------------------------------------------
061200131030
061300131030     c     *Entry        plist
061400131030     c                   parm                    KPJBA
061500131030
061600131030      /free
061700131030
061800131030       // -?Operazioni iniziali?
061900131030       exsr sr_RoutInz;
062000131030
062100131030       // -?Ciclo di gestione del file video?
062200131030       DoW  $Fine = *off;
062300131030
062400131030         select;
062500131030
062600131120           // -?Gestione subfile S01?
062700131030           when $Video = 'S1';
062800131030             exsr sr_GesS01;
062900131120
063000131120           // -?Gestione videata DS1 (singola spedizione)?
063100140207           //  ?- gestita all'interno della subr. sr_GesS01 -?
063200140207           //when  $Video = 'DS';
063300140207           //  exsr sr_GesD01;
063400131120
063500131120           // -?Gestione videata DO1 (singolo O.R.M.)?
063600140207           //  ?- gestita all'interno della subr. sr_GesS01 -?
063700140207           //when  $Video = 'DO';
063800140207           //  exsr sr_GesD01;
063900131030
064000131030           // -?? ? ??
064100131030           other;
064200131030             $Fine = *on;
064300131030
064400131030         endsl;
064500131030
064600131030       EndDo;
064700131030
064800131030       // -?Operazioni finali?
064900131030       exsr sr_RoutEnd;
065000131030
065100131030       //--------------------------------------------------------------
065200131030       //?Operazioni iniziali.                                         ?
065300131030       //--------------------------------------------------------------
065400131030       BEGSR  sr_RoutInz;
065500131030
065600131030         // -?Impostazione chiusura?
065700131030         *inLR = *on;
065800131121
065900131121         // -?Ricezione parametri ed impostazione parametri di output?
066000131121         FNLRX2ds = KPJBU;
066100131122         FNLRX2ds.oX2upd = *off;
066200131125         clear  FNLRX2ds.oX2fxx;
066300131122         FNLRX2ds.oX2err = *off;
066400131125         clear  FNLRX2ds.oX2msg;
066500131030
066600131030         // -?Reperimento dati job?
066700131030         exsr  sr_DatiJob;
066800131104
066900131104         // -?Impostazione nome programma a video?
067000131104         V1Tpgm = SDSpgmName;
067100131030
067200131030         // -?Pulizia / impostazione iniziale dei campi chiave?
067300131120         clear  keyFICAU01;
067400140206         clear  keyFICAU02;
067500131120         clear  keyFIAPD01;
067600140131         clear  keyFIAPD41;
067700131104         clear  keyFNARB01;
067800131125         clear  keyFIAR501;
067900131122         clear  keyFIAR401;
068000131126         clear  keyFIARN12;
068100131204         clear  keyFIDST01;
068200131122         clear  keyFNORM01;
068300131211         clear  keyFNORE01;
068400131210         clear  keyFIAR404;
068500131121         k_APDtip   = 'A';
068600140131         k_APD4tipA = 'A';
068700140131         k_APD4trd  = 'TEL';
068800131121         k_AR5trd   = 'GEN';
068900131126         k_ARNcdn   = 'LDV';
069000131126         k_ARNgst   = 'S';
069100131204         k_DSTnpg   = 4;
069200131211         k_OREtrc   = 'O ';
069300131125
069400131125         // -?Caricamento schiera tab. "TCV" = Tipologiche Causali Variazioni bolle?
069500131125         setll  ('TCV')  TNTBE000;
069600131125         reade  ('TCV')  TNTBE000;
069700131125         DoW  Not  %eof(TNTBE01L);
069800131125           xx += 1;
069900131125           sk_TCV(xx)  = TBEke1;
070000131125           sk_TCVd(xx) = TBEuni;
070100131125           reade  ('TCV')  TNTBE000;
070200131125         EndDo;
070300131030
070400131030       ENDSR;
070500131030
070600131030       //--------------------------------------------------------------
070700131030       //?Reperimento Dati del job (Utente/Operativi).                 ?
070800131030       //--------------------------------------------------------------
070900131030       BEGSR  sr_DatiJob;
071000131030
071100131030         in(e) §AzUte;
071200131030         if NOT %error;
071300131030           in(e) §DatiUte;
071400131030         endif;
071500131030         if %error or RSut = *blank;
071600131030           tibs34r ( tibs34ds );
071700131030           in §AzUte;
071800131030           in §DatiUte;
071900131030         endif;
072000131030
072100131030       ENDSR;
072200131030
072300131030       //--------------------------------------------------------------
072400131030       //?Gestione subfile S01.                                        ?
072500131030       //--------------------------------------------------------------
072600131030       BEGSR  sr_GesS01;
072700131030
072800131030         // -?Inizializzazione videata?
072900131030         if  $InzS01 = *on;
073000131030           exsr  sr_InzS01;
073100131030           $InzS01 = *off;
073200131030         endif;
073300140131
073400140131         // -?(Dis)Abilitazione Tasti Funzionali?
073500140131         //  E' meglio lasciarli qui, perchè F1 ri-(dis)abilitato?
073600140207         //  nella subr. "sr_GesD01": se viene (dis)abilitato là, qui?
073700140131         //  non verrebbe più re-impostato.?
073800140131         F1Attivo  = (S01nrr > 1);
073900140131         F9Attivo  = (S01nrr > *zero);
074000140131         F11Attivo = (S01nrr > 1);
074100131030
074200131030         // -?Emissione Testata & Piede?
074300131120         write  LRX2T01;
074400131120         write  LRX2P01;
074500131030
074600131030         // -?Posizionamento cursore?
074700131030         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
074800131030         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
074900131030         //  ?si visualizza la pagina che vedeva l'utente quando ha?
075000131030         //  ?premuto l'ultimo tasto.?
075100131030         if  C1CsrRrn > *zero;
075200131030           C1RcdNbr = C1CsrRrn;
075300131030         else;
075400131030           C1RcdNbr = 1;
075500131120           write  LRX2S00;             //?(no rec.)?
075600131030         endif;
075700131030
075800131030         // -?Emissione videata?
075900131120         exfmt  LRX2C01;
076000131030
076100131030         reset  ErrGenerico;
076200131030         reset  ErrMessage;
076300131030         clear  V1Dmsg;
076400131030
076500131030         Select;
076600131204
076700131204           // -?F1=Selezione Totale?
076800131204           When  dsp_aid = c_F01;
076900131204             exsr  sr_F01S01;
077000131030
077100131030           // -?F3=Fine?
077200131030           When  dsp_aid = c_F03;
077300131122             FNLRX2ds.oX2fxx = '03';
077400131030             $Fine = *on;
077500140131
077600140131           // -?F4=Distinta?
077700140131           When  dsp_aid = c_F04;
077800140131             exsr  sr_F04S01;
077900131030
078000131030           // -?F5=Rivisualizza?
078100131030           When  dsp_aid = c_F05;
078200131030             $InzS01 = *on;
078300131205
078400131205           // -?F11=Ordinamento Sfl?
078500131205           When  dsp_aid = c_F11;
078600131205             exsr  sr_F11S01;
078700131104
078800131104           // -?F12=Ritorno?
078900131104           When  dsp_aid = c_F12;
079000131122             FNLRX2ds.oX2fxx = '12';
079100131104             $Video = 'D1';
079200131104
079300131104           // -?Roll-Up?
079400131120           When  dsp_aid = c_RollUp;
079500131212             if  SavMAXnrr >= c_MaxRec  and  Not  $EoF;
079600131104               ErrGenerico = *on;
079700131104               ErrMessage  = *on;
079800131120               V1Dmsg = sk_Msg(01);
079900131204             endif;
080000131030
080100131030           // -?SubFile vuoto?
080200131212           When  SavMAXnrr = *zero;
080300131030
080400131030           // -?Invio?
080500131030           Other;
080600131030             // -?Gestione opzioni?
080700131030             exsr  sr_OpzS01;
080800131030             if  ErrGenerico;
080900131030               leavesr;
081000131030             endif;
081100131030
081200131204         EndSl;
081300131030
081400131030       ENDSR;
081500131030
081600131030       //--------------------------------------------------------------
081700131030       //?Inizializzazione subfile S01.                                ?
081800131030       //--------------------------------------------------------------
081900131030       BEGSR  sr_InzS01;
082000131030
082100131030         // -?Spegnimento degli indicatori di errore?
082200131030         %subst(IndDspF : 50) = *off;
082300131125
082400131125         // -?Decodifica autotrasportatore?
082500131205         k_APDpdr = FNLRX2ds.iX2aut;
082600131125         chain  %kds( keyFIAPD01 )  FIAPD000;
082700131125         if  Not %found(FIAPD01L)  or  APDatb <> *blank;
082800131125           clear  APDrsc;
082900131125         endif;
083000131125
083100140131         // -?Reperimento suo n° telefonico?
083200140131         k_APD4pdr = FNLRX2ds.iX2aut;
083300140131         chain  %kds( keyFIAPD41 )  FIAPD400;
083400140131         if  %found(FIAPD41L)  and  APD4atb = *blank;
083500140131           dAPD4tel = APD4uni;
083600140131         endif;
083700131205
083800131205         // -?Reperimento dati della Distinta Consegna?
083900131205         k_DSTfgs = FNLRX2ds.iX2fgs;
084000131205         k_DSTnfv = FNLRX2ds.iX2dst;
084100131205         chain  %kds( keyFIDST01 )  FIDST000;
084200131205         if  Not  %found(FIDST01L);
084300131205           clear  DSTpda;
084400140130           clear  DSTflr;
084500131205         endif;
084600140130
084700140130         dDSTflr = DSTflr;
084800131125
084900131125         // -?Impostazione campi di "testata"?
085000131205         C1Cpdr   = FNLRX2ds.iX2aut;
085100131125         C1Dpdr   = APDrsc;
085200140131         C1TelPdr = dAPD4tel.§APD4tel;
085300140131         //select;
085400140131         //  when  DSTpda = 'O';
085500140131         //    C1Dpda = DSTpda + '=O.R.M.   ';
085600140131         //  when  DSTpda = 'C';
085700140131         //    C1Dpda = DSTpda + '=Consegne ';
085800140131         //  when  DSTpda = 'E';
085900140131         //    C1Dpda = DSTpda + '=ORM+Cons.';
086000140131         //  when  DSTpda = 'N';
086100140131         //    C1Dpda = DSTpda + '=NO       ';
086200140131         //endsl;
086300140131         if  DSTpda = 'N';
086400140131           C1Dpda = 'NO';
086500140131         else;
086600140131           C1Dpda = DSTpda;
086700140131         endif;
086800140131         //select;
086900140131         //  when  dDSTflr.§DSTtstPda = *blank;
087000140131         //  when  dDSTflr.§DSTtstPda = 'O';
087100140131         //    C1Dtst = 'Test O.R.M.   ';
087200140131         //  when  dDSTflr.§DSTtstPda = 'C';
087300140131         //    C1Dtst = 'Test Consegne ';
087400140131         //  when  dDSTflr.§DSTtstPda = 'E';
087500140131         //    C1Dtst = 'Test ORM+Cons.';
087600140131         //endsl;
087700140203         C1Dtst = %xlate( c_Up : c_Lo : dDSTflr.§DSTtstPda );
087800131030
087900131030         // -?Pulizia subfile?
088000131030         SflDsp_N    = *on;
088100131030         SflDspCtl_N = *on;
088200131120         write  LRX2C01;
088300131030         SflDspCtl_N = *off;
088400131030         SflEnd      = *off;
088500131030
088600131104         clear  w_SflPag;
088700131030         clear  C1RcdNbr;
088800131030         clear  C1CsrRrn;
088900131212         clear  SavMAXnrr;
089000131030         clear  S01nrr;
089100131030         clear  V1Dmsg;
089200131030         ErrMessage  = *off;
089300131030         ErrGenerico = *off;
089400131030
089500131104         // -?Caricamento 1ª pagina dei dati nel subfile?
089600131121         clear  keyFICAU01;
089700131121         k_CAUfgs = FNLRX2ds.iX2fgs;
089800131205         k_CAUpdr = FNLRX2ds.iX2aut;
089900131205         //k_CAUnfv = FNLRX2ds.iX2dst;       ?(NON in chiave)?
090000131121         setll  %kds(keyFICAU01 : 2)  FICAU000;
090100131126         exsr  sr_ReadFICAU;
090200131204         exsr  sr_CaricaS01;
090300131030
090400131030         // -?Impaginazione subfile?
090500131030         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
090600131212         if S01nrr > *zero;
090700131030           C1RcdNbr  = 1;
090800131030           C1CsrRrn  = 1;
090900131030         else;
091000131104           clear  C1RcdNbr;
091100131030         endif;
091200131204
091300131204         // -?Ordinamento subfile?
091400131212         if  S01nrr > *zero;
091500131204           exsr  sr_SortSfl;
091600131204         endif;
091700131204
091800131204         // -?Eventuale emissione *msg di superamento limite max di rec.?
091900131204         //  ?visualizzabili nel subfile?
092000131212         if  S01nrr >= c_MaxRec  and  Not $EoF;
092100131204           ErrGenerico = *on;
092200131204           ErrMessage  = *on;
092300131204           V1Dmsg = sk_Msg(01);
092400131204         endif;
092500131030
092600131030       ENDSR;
092700131030
092800131030       //--------------------------------------------------------------
092900131204       //?Caricamento completo del subfile S01                         ?
093000131030       //--------------------------------------------------------------
093100131204       BEGSR  sr_CaricaS01;
093200131104
093300131204         clear  S01nrr;
093400131104         SflNxtChg = *off;
093500131030
093600131204         // -?Ciclo di caricamento completo di dati nel subfile?
093700131204         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
093800131204         //  ?l'eventuale ordinamento)?
093900131204         DoW  S01nrr < c_MaxRec  and  Not $EoF;
094000131122
094100131126           clear  LRX2S01;
094200131126
094300131126           // ·?Reperimento dati Comando da impartire al PDA o all'AUT?
094400131126           If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
094500131127             clear  dCMD;
094600131126             if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
094700131126                              *omit : *omit :
094800131126                              *omit : *omit :
094900131126                              *omit : *omit : *omit : *omit :
095000131126                              *omit : *omit : *omit : *omit :
095100131126                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
095200131126                              = *zero      AND
095300131126                   ds_TNTBE.TBEatb = *blank;
095400131127               dCMD = ds_TNTBE.TBEuni;
095500131126             else;
095600131204               §CMDdes  = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ';
095700131126             endif;
095800131126           Else;
095900131204             §CMDdes  = CAUdesCmd;
096000131126           EndIf;
096100131126
096200131126           // ·?Reperimento dati Tipologiche Causali Variazioni bolle?
096300140131           If  CAUcmd <> c_Cmd_Inserito;
096400140131             If  CAUdesVa = *blank;
096500140131               wTipTVA = 'B';
096600140213               ds_TVA  = CAUtva;
096700140131               exsr  sr_GetDesTCV;
096800140131               S01var  = wDesTVA;
096900140131             Else;
097000140131               S01var  = CAUdesVa;
097100140131             EndIf;
097200140131           EndIf;
097300131126
097400131204
097500131205           Select;
097600131204             // ·?Reperimento dati Spedizione?
097700131205             When  CAUtor = c_Consegna;
097800131126               exsr  sr_GetSped;
097900131127               //S01rsc = ARBrsd + AR4not;
098000131127               S01rsc = ARBrsd;
098100131204             // ·?Reperimento dati O.R.M.?
098200131205             When  CAUtor = c_Ritiro;
098300131126               exsr  sr_GetORM;
098400131126               S01rsc = ORMrsr;
098500131205           EndSl;
098600131204
098700131204           // ·?Campi hidden?
098800131204           S1Hfgs    = CAUfgs;
098900131204           S1HdtOins = CAUdtOins;
099000131204           S1Htor    = CAUtor;
099100131204           S1Hogg    = CAUogg;
099200140206           //S1Hcmd    = §CMDdes;
099300140206           S1Hcmd    = CAUdesCmd;
099400131204           // ·?Campi di Output?
099500131204           S01tor    = CAUtor;
099600140206           //S01cmd    = §CMDdes;
099700140206           S01cmd    = CAUdesCmd;
099800131205           //S01var    = CAUdesVa;    ?(già "impostato" - o costruito)?
099900131206           S01hmi    = %int( %subst( CAUdtOins : 9 : 4 ) );
100000131204           select;
100100131204             when  CAUtor = c_Consegna;
100200131204               S01ogg = 'Spedizione: ' + %editc( ds_Spedizione.wLNP : 'X' ) +
100300131204                                   ' ' + %editc( ds_Spedizione.wNRS : 'Z' ) +
100400131204                                   ' ' + %editc( ds_Spedizione.wNSP : '3' ) +
100500131204                                  '  ' + %editc( ds_Spedizione.wAAS : 'Z' );
100600131204             when  CAUtor = c_Ritiro;
100700131204               S01ogg = 'O.R.M.: ' + %editc( ds_ORM.wPOE : 'X' ) +
100800131204                               ' ' + %editc( ds_ORM.wNSR : 'Z' ) +
100900131204                               ' ' + %editc( ds_ORM.wNOR : '3' ) +
101000131204                               ' ' + %editc( ds_ORM.wNRV : 'Z' );
101100131204           endsl;
101200131126
101300131126           // -?Scrittura record nel subfile?
101400131206           Ord_x_OraIns = (wOrdSfl = 1);
101500131126           S01nrr += 1;
101600131126           write  LRX2S01;
101700131030
101800131104           // -?Lettura record successivo?
101900131126           exsr  sr_ReadFICAU;
102000131030
102100131030         EndDo;
102200131204
102300131030
102400131212         // -?Memorizzazione n° rec. caricati nel subfile?
102500131212         //  ?(servirà all'ordinamento)?
102600131212         SavMAXnrr = S01nrr;
102700131212
102800131030         // -?Visualizzazione del SFL (se ci sono dati)?
102900131030         SflDsp_N = (S01nrr <= *zero);
103000131030
103100131030         // -?Attivazione del SFLEND?
103200131030         SflEnd = ( $EoF );
103300131030
103400131204         // -?Posizionamento cursore al 1° rcd del subfile?
103500131030         if  S01nrr > *zero;
103600131204           C1RcdNbr = 1;
103700131030         else;
103800131030           clear  C1RcdNbr;
103900131030         endif;
104000131030
104100131030         C1CsrRrn = C1RcdNbr;
104200131030
104300131030       ENDSR;
104400131204
104500131204       //--------------------------------------------------------------
104600131204       //?Gestione tasto funzionale F1 da videata C01 / S01            ?
104700131204       //?F1-Selezione Totale                                          ?
104800131204       //--------------------------------------------------------------
104900131204       BEGSR  sr_F01S01;
105000131204
105100131212         RrnLast  = SavMAXnrr;
105200131204
105300131204         For  S01nrr = 1  To  RrnLast;
105400131204
105500131204           chain  S01nrr  LRX2S01;
105600131204           if  Not %found(FNLRX2D);
105700131204             iter;
105800131204           endif;
105900131204
106000131204           S01opz = 1;
106100131204           SflNxtChg = *on;
106200131206           Ord_x_OraIns = (wOrdSfl = 1);
106300131204           update  LRX2S01;
106400131204
106500131204         EndFor;
106600131204
106700131204       ENDSR;
106800140131
106900140131       //--------------------------------------------------------------
107000140131       //?Gestione tasto funzionale F4 da videata C01 / S01            ?
107100140131       //?F4-Spedizioni nella distinta                                ?
107200140131       //--------------------------------------------------------------
107300140131       BEGSR  sr_F04S01;
107400140131
107500140131         // -?Visualizzazione fasi della distinta se PDA?
107600140131         //  ?(questo *pgm dovrebbe "durare" fino ad agosto 2014...)?
107700140131         If  DSTpda = 'E'  and  dDSTflr.§DSTtstPda = *blank;
107800140131
107900140131           clear  KPJBU;
108000140131           clear  FNLVP0ds;
108100140131           LVP0fgs = FNLRX2ds.iX2fgs;
108200140131           LVP0ndc = FNLRX2ds.iX2dst;
108300140131           //LVP0aut = FNLRX2ds.iX2daut;
108400140131           LVP0aut = DSTpdr;
108500140131           FNLVP0R2( KPJBA : FNLVP0ds );
108600140131
108700140131         // -?Visualizzazione Dettaglio Distinta?
108800140131         Else;
108900140131
109000140131           clear  FIDG311ds;
109100140131           F31fgs     = FNLRX2ds.iX2fgs;
109200140131           F31npg     = DSTnpg;
109300140131           F31numDis  = FNLRX2ds.iX2dst;
109400140131           F31DatDis  = DSTdfv;
109500140131           //F31DatiVis = *blank;
109600140131           F31priDati = c_Consegna;
109700140131
109800140131           kpjbu = FIDG311ds;
109900140131           FIDG311R ( KPJBA );
110000140131
110100140131           FIDG311ds = KPJBU;
110200140131           if  F31tFunz = '03';
110300140131             $Fine = *on;
110400140131             FNLRX2ds.oX2fxx = F31tFunz;
110500140131           endif;
110600140131
110700140131         EndIf;
110800140131
110900140131       ENDSR;
111000131204
111100131204       //--------------------------------------------------------------
111200131205       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
111300131205       //?F11-Cambia ordinamento sfl                                   ?
111400131204       //--------------------------------------------------------------
111500131205       BEGSR  sr_F11S01;
111600131204
111700131204         if  wOrdSfl < 1;
111800131204           wOrdSfl += 1;
111900131204         else;
112000131204           clear  wOrdSfl;
112100131204         endif;
112200131204
112300131204         // -?Se subfile vuoto: nessun record da ordinare?
112400131204         //  ?Altrimenti: basta riordinarlo?
112500131212         if  SavMAXnrr > *zero;
112600131204           exsr  sr_SortSfl;
112700131204         endif;
112800131204
112900131204       ENDSR;
113000131204
113100131204       //--------------------------------------------------------------
113200131204       //?Ordinamento subfile                                          ?
113300131204       //?  This subroutine sorts the subfile records.                 ?
113400131204       //--------------------------------------------------------------
113500131204       BEGSR  sr_SortSfl;
113600131204
113700131212         RrnLast  = SavMAXnrr;
113800131204
113900131204         C1RcdNbr = 1;
114000131204         clear  C1CsrRrn;
114100131204         clear  S01nrr;
114200131204         clear  V1Dmsg;
114300131204         ErrMessage  = *off;
114400131204         SflNxtChg   = *off;
114500131204         ErrGenerico = *off;
114600131204         %subst(IndDspF : 50) = *off;
114700131204
114800131204         //?___________________________________________________________?
114900131204         //?Initialize the key fields to sort on.?
115000131204         //?There is one extra field not in the subfile -- Selected --?
115100131204         //?that is added to the record. This field is used to place?
115200131204         //?selected records in front of nonselected records.?
115300131204         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
115400131204
115500131204         clear  QLGKL;
115600131204         clear  QLGSKL;
115700131204         clear  QLGSCB;
115800131204         clear  QLGSCB00;
115900131204
116000131204         // -?Gestione della scelta ordinamento:?
116100131204         Select;
116200131204
116300131204           //  ?Ordinamento per Comando (desc)?
116400131204           When  wOrdSfl = *zero;
116500131204             // -?2 campi chiave?
116600131204             QLGNBRK  = 2;
116700131204             // -?1° campo: Descrizione Comando (S1Hcmd)?
116800131204             //            ?a posizone   39, 20 byte, char, descending.?
116900131204             QLGSP    = 39;
117000131204             QLGSS    = %size(S1Hcmd);
117100131204             QLGDT    = Carattere;
117200131204             QLGSO    = Discendente;
117300131204             QLGKL(1) = QLGSKL;
117400131204             // -?2° campo: Data Inserimento (S1HdtOins)?
117500131204             //            ?a posizone    4, 14 byte, char, ascending.?
117600131204             QLGSP    = 4;
117700131204             QLGSS    = %size(S1HdtOins);
117800131204             QLGDT    = Carattere;
117900131204             QLGSO    = Ascendente;
118000131204             QLGKL(2) = QLGSKL;
118100131204
118200131205           //  ?Ordinamento per Data/Ora Inserimento?
118300131204           When  wOrdSfl = 1;
118400131204             // -?1 campo chiave?
118500131204             QLGNBRK  = 1;
118600131204             // -?1° campo: Data Inserimento (S1HdtOins)?
118700131204             //            ?a posizone    4, 14 byte, char, ascending.?
118800131204             QLGSP    = 4;
118900131204             QLGSS    = %size(S1HdtOins);
119000131204             QLGDT    = Carattere;
119100131204             QLGSO    = Ascendente;
119200131204             QLGKL(1) = QLGSKL;
119300131204
119400131204         EndSl;
119500131204
119600131204         // -?Load other sort parameters.?
119700131204         QLGLB   = 80 + 16 * MaxKey;
119800131204         QLGRL   = %size(SflRcd) - 1;
119900131204         QLGRT   = 8;
120000131204         QLGOKL  = 80;
120100131204         QLGLKE  = 16;
120200131204         QLGLSS  = 290;
120300131204         // -?Initialize Sort I/O API fields.?
120400131204         QLGRL00 = QLGRL;
120500131204         QLGRC00 = 1;
120600131204         clear  QUSEI;
120700131204         QUSBPRV = %size(QUSEC);
120800131204
120900131204         // -?First step - Initialize the sort routine.?
121000131204         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
121100131204                      SizeList : ReturnSize : QUSEC );
121200131204
121300131204         // -?Next step - Write records to I/O routine.?
121400131204         QLGRT00  = Put;
121500131204         For  Nrr = 1  To  RrnLast;
121600131204           chain  Nrr  LRX2S01;
121700131204           // -?Solo le righe con Selected = 'Y' sono riordinate,?
121800131204           //  ?quindi per fare un ordinamento di tutte le righe?
121900131204           //  ?metto 'Y' sempre.?
122000131204           Selected = 'Y';
122100131204           clear  QUSEI;
122200131204           QUSBPRV  = %size(QUSEC);
122300131204           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
122400131204                         SizeList : NotUsed : QUSEC );
122500131204         EndFor;
122600131204
122700131204         // -?Next step - Signal end of input, clear subfile for reload.?
122800131204         QLGRT00 = EndPut;
122900131204         clear  QUSEI;
123000131204         QUSBPRV = %size(QUSEC);
123100131204         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
123200131204                       SizeList : NotUsed : QUSEC );
123300131204
123400131204         // -?Pulizia subfile?
123500131204         SflDsp_N    = *on;
123600131204         SflDspCtl_N = *on;
123700131204         write  LRX2C01;
123800131204         SflDspCtl_N = *off;
123900131204         SflEnd      = *off;
124000131204
124100131204         // -?Final step - Write the records back to the subfile.?
124200131204         QLGRT00  = Get;
124300131204         For  Nrr = 1  To RrnLast;
124400131204           clear  QUSEI;
124500131204           QUSBPRV = %size(QUSEC);
124600131204           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
124700131204                         QLGRL00  : NotUsed : QUSEC );
124800131204           // -?Ripristino indicatori utilizzati nel sfl rec?
124900131206           Ord_x_OraIns = (wOrdSfl = 1);
125000131204           SflNxtChg  = (S01opz <> *zero);
125100131204           S01nrr = Nrr;
125200131204           write  LRX2S01;
125300131204         EndFor;
125400131204
125500131204         C1CsrRrn = 1;
125600131204
125700131204         // -?Visualizzazione del SFL (se ci sono dati)?
125800131204         SflDsp_N = (S01nrr <= *zero);
125900131204
126000131204         // -?Attivazione del SFLEND?
126100131204         SflEnd = $EoF;
126200131204
126300131205         // -?Impostazione F11 nel piede del subfile?
126400131204         select;
126500131204           when  S01nrr  = *zero;
126600131205             clear  V1PF11;
126700131204           when  wOrdSfl = 0;
126800131205             V1PF11 = 'F11=Ord.OraIns';
126900131204           when  wOrdSfl = 1;
127000131205             V1PF11 = 'F11=Ord. x Cmd';
127100131204         endsl;
127200131204
127300131204       ENDSR;
127400131127
127500131127       //--------------------------------------------------------------
127600131127       //?Gestione opzioni del subfile S01.                            ?
127700131127       //--------------------------------------------------------------
127800131127       BEGSR  sr_OpzS01;
127900131127
128000131127         clear  SavS01csr;
128100131127
128200131127         // -?Ciclo di lettura subfile?
128300131127         readc  LRX2S01;
128400131127
128500131129         DoW  Not %eof(FNLRX2D);
128600131127
128700131127           SflNxtChg = *off;
128800131127           %subst(IndDspF : 50) = *off;
128900131127           C1RcdNbr = S01nrr;
129000131127
129100131127           select;
129200131127
129300131127             // -?Nessuna opzione?
129400131127             when  S01opz = *zero;
129500131127
129600131127             // -?Opz. 1=Gestione?
129700131127             when  S01opz = 1;
129800131127               select;
129900131127                 when  S1Htor = c_Consegna;
130000131127                   $Video  = 'DS';
130100140203                   $InzD01 = *on;
130200131127                   doW  $Video = 'DS';
130300140207                     exsr  sr_GesD01;
130400131127                   enddo;
130500131127                 when  S1Htor = c_Ritiro;
130600131127                   $Video  = 'DO';
130700140203                   $InzD01 = *on;
130800131127                   doW  $Video = 'DO';
130900140207                     exsr  sr_GesD01;
131000131127                   enddo;
131100131127                 other;
131200131127                   ErrGenerico = *on;
131300131127                   ErrMessage  = *on;
131400131127                   PosCurOPZ   = *on;
131500131127                   V1Dmsg = sk_Msg(02);
131600131127               endsl;
131700131127
131800131127             // -?Opzione errata?
131900131127             other;
132000131127               ErrGenerico = *on;
132100131127               ErrMessage  = *on;
132200131127               PosCurOPZ   = *on;
132300131127               V1Dmsg = sk_Msg(02);
132400131127
132500131127           endsl;
132600131127
132700131127           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
132800131127           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
132900131127           if  S01opz  <> *zero    and
133000131127              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
133100131127             SavS01csr   = C1RcdNbr;
133200131127           endif;
133300131127
133400131127           // -?Aggiornamento sfl?
133500131127           select;
133600131127             when  ErrMessage;
133700131127               SflNxtChg = *on;
133800131127               C1CsrRrn  = C1RcdNbr;
133900131127             when  ErrGenerico;
134000131127               C1CsrRrn  = C1RcdNbr;
134100131127               clear  S01opz;
134200131127             other;
134300131127               clear  S01opz;
134400131127           endsl;
134500131127
134600131206           Ord_x_OraIns = (wOrdSfl = 1);
134700131206
134800131127           UPDATE  LRX2S01;
134900131127
135000131127           if  ErrGenerico   or   ErrMessage;
135100131127             leave;
135200131127           endif;
135300131127
135400131127           readc  LRX2S01;
135500131127
135600131127         ENDDO;
135700131127
135800131127         // -?Riposizionam. cursore sul record contenente la prima opz.?
135900131127         //  ?(se non sono stati rilevati errori)?
136000131127         if  NOT ErrMessage   and   NOT ErrGenerico   and
136100131127             SavS01csr > *zero;
136200131127           C1CsrRrn = SavS01csr;
136300131127         endif;
136400131127
136500131127       ENDSR;
136600131127
136700131127       //--------------------------------------------------------------
136800131127       //?Lettura file FICAU01L e selezione del record.                ?
136900131127       //--------------------------------------------------------------
137000131127       BEGSR  sr_ReadFICAU;
137100131127
137200131205         reade(N)  %kds(keyFICAU01 : 2)  FICAU000;
137300131127         $EoF = ( %eof(FICAU01L) );
137400131127
137500131127         // -?Selezione singolo record?
137600131212         DoW  Not  $EoF;
137700131127
137800131127           exsr  sr_SelezRec;
137900131127
138000131127           if  $RecOK;
138100131127             leave;
138200131127           endif;
138300131127
138400131205           reade(N)  %kds(keyFICAU01 : 2)  FICAU000;
138500131127           $EoF = ( %eof(FICAU01L) );
138600131127
138700131127         EndDo;
138800131127
138900131127       ENDSR;
139000131121
139100131121       //--------------------------------------------------------------
139200131122       //?Selezione del singolo record in input.                       ?
139300131121       //--------------------------------------------------------------
139400131121       BEGSR  sr_SelezRec;
139500131121
139600131121         $RecOK = *off;
139700131122
139800131122         Select;
139900131121
140000131121           // -?Filiale Gestione diversa?
140100131121           //  ?(IMPOSSIBILE perchè 1ª chiave)?
140200131121           //When  CAUfgs <> FNLRX2ds.iX2fgs;
140300131121           //  setll  (*hival)  FICAU000;
140400131121           //  leavesr;
140500131205
140600131205           // -?Autotrasportatore diverso da quello selezionato?
140700131205           //  ?(IMPOSSIBILE perchè 2ª chiave)?
140800131205           //When  CAUpdr <> FNLRX2ds.iX2aut;
140900131205           //  leavesr;
141000131205
141100131205           // -?Distinta Consegna diversa da quella selezionata?
141200140207           //When  FNLRX2ds.iX2dst <> *zero   and
141300140207           //      FNLRX2ds.iX2dst <> CAUnfv  and
141400140207           //      CAUtor          =  c_Consegna;
141500140207           When  FNLRX2ds.iX2dst <> *zero   and
141600140207                 FNLRX2ds.iX2dst <> CAUnfv;
141700131205             leavesr;
141800131121
141900131121           // -?Record Annullato?
142000131121           When  CAUatb <> *blank;
142100131121             leavesr;
142200131121
142300131121           // -?Ammessi Tipi Oggetto "C" (consegna) e "R" (ritiro)?
142400131121           When  CAUtor <> c_Consegna  and  CAUtor <> c_Ritiro;
142500131121             leavesr;
142600131126
142700131126           // -?Solo Telefonate SENZA risposta?
142800131126           When  CAUflt <> 'T';
142900131126             leavesr;
143000131126           When  CAUdtOtel <> *blank  and  CAUdtOtel <> *zero;
143100131126             leavesr;
143200131121
143300131121         EndSl;
143400131121
143500131121         // -?Telefonata da caricare nel subfile?
143600131121         $RecOK = *on;
143700131121
143800131121       ENDSR;
143900131030
144000131030       //--------------------------------------------------------------
144100140207       //?Gestione videata DS1 (spedizione) o DO1 (O.R.M.)             ?
144200131030       //--------------------------------------------------------------
144300140207       BEGSR  sr_GesD01;
144400140207
144500140207         // -?Inizializzazione videata?
144600140207         //  ?(sempre e comunque, per reperire i dati aggiornati)?
144700140207         clear  $RecUpdated;
144800140207         select;
144900140207           when  $Video = 'DS';
145000140207             exsr  sr_InzDS1;
145100140207             exsr  sr_AttrVisDS1;
145200140207           when  $Video = 'DO';
145300140207             exsr  sr_InzDO1;
145400140207             exsr  sr_AttrVisDO1;
145500140207         endsl;
145600140207
145700140207         // -?(Dis)Abilitazione tasti funzionali a video?
145800140207         exsr  sr_AbilFxxD01;
145900140207
146000140207         // -?Verifica dell'oggetto: confronto con i dati precedentemente?
146100140207         //  ?memorizzati  (qualcuno dei quali potrebbe essere stato?
146200140207         //  ?modificato da un altro utente)?
146300140207         if  Not  $InzD01;
146400140207           exsr  sr_Verifica_Ogg;
146500140207         endif;
146600140207
146700140207         $InzD01 = *off;
146800140207
146900140207         // -?Eventuale avviso di dati variati?
147000140207         if  $RecUpdated;
147100140207           ErrGenerico = *on;
147200140207           ErrMessage  = *on;
147300140207           V1Dmsg = sk_Msg(04);
147400140207         endif;
147500131120
147600131120         // -?Emissione videata?
147700131120         write  LRX2T01;
147800140207         select;
147900140207           when  $Video = 'DS';
148000140207             exfmt  LRX2DS1;
148100140207           when  $Video = 'DO';
148200140207             exfmt  LRX2DO1;
148300140207         endsl;
148400131120
148500131120         clear  V1Dmsg;
148600131120         reset  ErrMessage;
148700140203         reset  ErrGenerico;
148800140203
148900140203         // -?Memorizzazione dati della videata per verificarne variazioni?
149000140203         if  dsp_aid <> c_F03  and  dsp_aid <> c_F12;
149100140203           exsr  sr_Memo_Dati;
149200140203         endif;
149300131120
149400131120         SELECT;
149500131127
149600131127           // -?F1=Ultimo Contatto?
149700131127           WHEN  dsp_aid = c_F01;
149800131128             exsr  sr_GesW01;
149900131122
150000140203           // -?F3=Fine?
150100140203           //  ?(ritorno al subfile S01 interrompendone la lettura)?
150200131122           WHEN  dsp_aid = c_F03;
150300140108             //clear  $Video;
150400140108             //FNLRX2ds.oX2fxx = '03';
150500140108             //ErrGenerico = *on;
150600140108             //$Fine = *on;
150700140108             ErrGenerico = *on;
150800140108             $Video = 'S1';
150900140226
151000140226           // -?F5=Altri Dati?
151100140226           WHEN  dsp_aid = c_F05;
151200140226             exsr  sr_GesW02;
151300131120
151400131120           // -?F7=Interrogazione spedizione?
151500140207           WHEN  dsp_aid = c_F07  and  $Video = 'DS';
151600131121             exsr  sr_VisualSped;
151700140207
151800140207           // -?F8=Interrogazione O.R.M.?
151900140207           WHEN  dsp_aid = c_F08  and  $Video = 'DO';
152000140207             exsr  sr_VisualORM;
152100131120
152200131120           // -?F12=Ritorno?
152300140203           //  ?(ritorno al subfile S01 proseguendone la lettura)?
152400131120           WHEN  dsp_aid = c_F12;
152500140108             //ErrGenerico = *on;
152600131120             $Video = 'S1';
152700131120
152800140108           // -?Invio o F6=Accettato?
152900131120           OTHER;
153000140207             select;
153100140207               when  $Video = 'DS';
153200140207                 exsr  sr_CtrDS1;
153300140207               when  $Video = 'DO';
153400140207                 exsr  sr_CtrDO1;
153500140207             endsl;
153600140207             if  ErrGenerico = *on;
153700131120               leavesr;
153800131120             endif;
153900140108             // -?F6=Accettato (Esegue)?
154000140108             if  dsp_aid = c_F06;
154100140121               exsr  sr_F06D01;
154200131205             endif;
154300131120
154400131120         ENDSL;
154500131030
154600131030       ENDSR;
154700140207
154800140207       //--------------------------------------------------------------
154900140207       //?Abilitazione tasti funzionali video DS1 / DO1.               ?
155000140207       //--------------------------------------------------------------
155100140207       BEGSR  sr_AbilFxxD01;
155200140207
155300140207         // -?F1 = Ultimo Contatto?
155400140207         F1Attivo = ($UltContPrec = *on);
155500140207
155600140207       ENDSR;
155700131121
155800131121       //--------------------------------------------------------------
155900131121       //?Inizializazzione videata DS1 (spedizione).                   ?
156000131121       //--------------------------------------------------------------
156100131121       BEGSR  sr_InzDS1;
156200131121
156300131122         ds_Spedizione = S1Hogg;
156400140206
156500140206         // -?Impostazione chiave di accesso per la v.l. 02L?
156600140206         clear  keyFICAU02;
156700140206         k_CA2fgs    = FNLRX2ds.iX2fgs;
156800140206         k_CA2nfv    = FNLRX2ds.iX2dst;
156900140206         k_CA2pdr    = FNLRX2ds.iX2aut;
157000140206         k_CA2tor    = S1Htor;
157100140206         k_CA2ogg    = S1Hogg;
157200140207         k_CA2dtOins = *hival;
157300131121
157400131121         // -?Pulizia videata?
157500131121         clear  LRX2DS1;
157600131128         clear  $UltContPrec;
157700131122
157800131122         // -?Contatto ad Autotrasportatore?
157900140203         clear  sk_TVA;
158000131122         clear  keyFICAU01;
158100140207         //k_CAUfgs    = S1Hfgs;
158200140207         //k_CAUpdr    = C1Cpdr;
158300140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
158400140206         k_CAUpdr    = FNLRX2ds.iX2aut;
158500131122         k_CAUdtOins = S1HdtOins;
158600131122         k_CAUtor    = S1Htor;
158700131125         k_CAUogg    = S1Hogg;
158800140203         chain(N)  %kds( keyFICAU01 )  FICAU000;
158900140203
159000140203         // -?Rec. NON trovato: ricerca con data/ora inserimento più?
159100140203         //  ?recenti (segnalando errore SE non trovato nuovo rec.)?
159200140203         //  ?e riemissione della videata?
159300140203         if  Not %found(FICAU01L)  or  CAUatb <> *blank;
159400140203
159500140203           exsr  sr_Ricarica_Ogg;
159600140203
159700140203           if  Not $RecUpdated;
159800140203             ErrGenerico = *on;
159900140203             ErrMessage  = *on;
160000140203             V1Dmsg = sk_Msg(03);
160100140203             leavesr;
160200140203           endif;
160300140203
160400140203         endif;
160500131121
160600131121         // -?Decodifica autotrasportatore?
160700140203         exsr  sr_GetAUT;
160800131122
160900131122         // -?Comando da impartire al PDA o all'AUT?
161000140203         exsr  sr_GetCMD;
161100131122
161200131122         // -?Tipologica Causale Variazione bolla?
161300140131         //  ?(SE NON "INSERITO")?
161400140203         exsr  sr_GetTVA;
161500131121
161600131121         // -?Spedizione?
161700131122         exsr  sr_GetSped;
161800131121         if  Not  %found(FNARB01L);
161900131121           leavesr;
162000131121         endif;
162100131122
162200131122         V1Clnp = ds_Spedizione.wLNP;
162300131122         V1Cnrs = ds_Spedizione.wNRS;
162400131122         V1Cnsp = ds_Spedizione.wNSP;
162500131204         //wDate_EUR = %date( (ds_Spedizione.wAAS * 10000) + ARBmgs : *iso );
162600131204         //V1Cdsp = %dec( wDate_EUR );
162700131127
162800131127         // -?Colli?
162900131127         V1Cncl = ARBncl;
163000131127
163100131127         // -?Peso?
163200131127         V1Cpkb = ARBpkb;
163300131127
163400131127         // -?Particolarità?
163500131127         V1Cgga = ARBgga;
163600131127         V1Cgma = ARBgma;
163700131127         V1Cgva = ARBgva;
163800131126
163900131127         // -?Codice Bolla (tab. "3A")?
164000131204         //clear  ds3A;
164100131204         //if  getTabella ( c_Tabel : '3A' : ARBcbo : *omit :
164200131204         //                 *omit : *omit :
164300131204         //                 *omit : *omit :
164400131204         //                 *omit : *omit : *omit : *omit :
164500131204         //                 *omit : *omit : *omit : *omit :
164600131204         //                 ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
164700131204         //                 = *zero      AND
164800131204         //      ds_TNTBE.TBEatb = *blank;
164900131204         //  ds3A = ds_TNTBE.TBEuni;
165000131204         //else;
165100131204         //  §3Ades = *all'? ';
165200131204         //endif;
165300131204         //
165400131204         //V1Ccbo = ARBcbo;
165500131204         //V1Dcbo = §3Ades;
165600131126
165700131126         // -?Tipo Servizio (tab. "5E")?
165800131126         clear  ds5E;
165900131126         if  getTabella ( c_Tabel : '5E' : ARBtsp : *omit :
166000131126                          *omit : *omit :
166100131126                          *omit : *omit :
166200131126                          *omit : *omit : *omit : *omit :
166300131126                          *omit : *omit : *omit : *omit :
166400131126                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
166500131126                          = *zero      AND
166600131126               ds_TNTBE.TBEatb = *blank;
166700131126           ds5E = ds_TNTBE.TBEuni;
166800131126         else;
166900131126           §5Edes = *all'? ';
167000131126         endif;
167100131126
167200131205         evalr V1Dtsp = %trimr(§5Ed08);
167300131122
167400131127         // -?Cliente Mittente?
167500131127         //if  ARBccm <> *zero;
167600131127         //  V1Cccm = ARBccm;
167700131127         //else;
167800131127         //  if  %subst(§3Atb1:1:1) = 'F';
167900131127         //    V1Cccm = ARBksc;
168000131127         //  endif;
168100131127         //endif;
168200131209         //V1Drsm = ARBrsm;
168300131209         //V1Drmo = ARBrmo;
168400131122
168500131127         //// -?Cliente Destinatario?
168600131122         //if  %subst(§3Atb1:1:1) = 'A';
168700131122         //  V1Cccd = ARBksc;
168800131122         //endif;
168900131122         //// -?Se c'è 2ª bolla imposto LNA9999?
169000131122         //if  §3Atb2 <> *blank;
169100131122         //  V1Cccd = (ARBlna * 10000) + 9999;
169200131122         //endif;
169300131209         V1Drsd = ARBrsd + AR4not;
169400131209         V1Dind = ARBind;
169500131209         //V1Dcad = ARBcad;
169600131209         //V1Dlpd = %trim( ARBlod ) + '  ' + ARBprd + ' ' + ARBnzd;
169700131209         V1Dlpd = %trim( ARBcad ) + ' ' + %trim( ARBlod ) + '  ' +
169800131209                  ARBprd + ' ' + ARBnzd;
169900131121
170000131121         // -?Distinta?
170100131205         ////k_DSTfgs = (ARBpdc / 10000);
170200131205         ////k_DSTnfv = ARBndc;
170300131205         //k_DSTfgs = FNLRX2ds.iX2fgs;
170400131205         //k_DSTnfv = FNLRX2ds.iX2dst;
170500131205         //chain  %kds( keyFIDST01 )  FIDST000;
170600131205         //if  Not  %found(FIDST01L);
170700131205         //  clear  DSTpda;
170800131205         //endif;
170900131121
171000131122         // -?Referente Consegna (estensione testata bolla: trk "GEN")?
171100131121         clear  dAR5gen;
171200131121         k_AR5aas = ds_Spedizione.wAAS;
171300131121         k_AR5lnp = ds_Spedizione.wLNP;
171400131121         k_AR5nrs = ds_Spedizione.wNRS;
171500131121         k_AR5nsp = ds_Spedizione.wNSP;
171600131205         //k_AR5trd   = 'GEN';        ?(già così)?
171700131121         chain  %kds(keyFIAR501 : 5)  FIAR5000;
171800131121         if  %found(FIAR501L)  and  AR5atb = *blank;
171900131121           dAR5gen = AR5uni;
172000131121         endif;
172100131122
172200131204         select;
172300131204           when  dAR5gen.§AR5ref  <> *blank  and
172400131204                 dAR5gen.§AR5telD <> *blank;
172500131209             V1Dref = %trimr( dAR5gen.§AR5ref ) + ' - ' +
172600131204                      %triml( dAR5gen.§AR5telD );
172700131204           when  dAR5gen.§AR5ref  <> *blank;
172800131209             V1Dref = dAR5gen.§AR5ref;
172900131204           when  dAR5gen.§AR5telD <> *blank;
173000131209             V1Dref = dAR5gen.§AR5telD;
173100131204         endsl;
173200131121
173300131121         // -?Dati C/Assegno?
173400131121         chain  %kds( keyFNARB01 )  FIAR9000;
173500131122         if  Not %found(FIAR901L);
173600131121           clear  AR9cas;
173700131121           clear  AR9vca;
173800131121         endif;
173900131122
174000131122         V1Ccas = AR9cas;
174100131122         V1Cvca = AR9vca;
174200131121
174300131122         // -?Dati Fattura (Tassazione Bolla - filiale)?
174400131122         clear  keyFIAR601;
174500131121         k_AR6aas = ds_Spedizione.wAAS;
174600131121         k_AR6lnp = ds_Spedizione.wLNP;
174700131121         k_AR6nrs = ds_Spedizione.wNRS;
174800131121         k_AR6nsp = ds_Spedizione.wNSP;
174900131122         // -?C'è 2ª bolla (?)?
175000131122         k_AR6trc = '2';
175100131122         clear  AR6div;
175200131122         clear  AR6ift;
175300131122         chain  %kds(keyFIAR601)  FIAR6000;
175400131122         If  %found(FIAR601L);
175500131122           if  AR6ift = *zero;
175600131122             clear  AR6div;
175700131122           endif;
175800131122         // -?1ª bolla?
175900131122         Else;
176000131122           k_AR6trc = '1';
176100131122           chain  %kds(keyFIAR601)  FIAR6000;
176200131122           if  %found(FIAR601L)  and  AR6ift = *zero;
176300131122             clear  AR6div;
176400131122           endif;
176500131122         EndIf;
176600131122
176700131122         V1Cift = AR6ift;
176800131122         V1Cdiv = AR6div;
176900131121
177000131126         // -?Note LdV?
177100131122         clear  keyFIAR401;
177200131122         k_AR4aas = ds_Spedizione.wAAS;
177300131122         k_AR4lnp = ds_Spedizione.wLNP;
177400131122         k_AR4nrs = ds_Spedizione.wNRS;
177500131122         k_AR4nsp = ds_Spedizione.wNSP;
177600131122         k_AR4trc = '8';
177700131122         chain  %kds(keyFIAR401)  FIAR4000;
177800131122         if  %found(FIAR401L);
177900131122           V1Dnot8 = AR4not;
178000131122         endif;
178100131122         k_AR4trc = '9';
178200131122         chain  %kds(keyFIAR401)  FIAR4000;
178300131122         if  %found(FIAR401L);
178400131122           V1Dnot9 = AR4not;
178500131122         endif;
178600131122
178700131126         // -?Note Bolla?
178800131126         k_ARNaas = ds_Spedizione.wAAS;
178900131126         k_ARNlnp = ds_Spedizione.wLNP;
179000131126         k_ARNnrs = ds_Spedizione.wNRS;
179100131126         k_ARNnsp = ds_Spedizione.wNSP;
179200131205         //k_ARNcdn = 'LDV';          ?(già impostato)?
179300131205         //k_ARNgst = 'S';            ?(già impostato)?
179400131126         setll  %kds(keyFIARN12 : 6)  FIARN000;
179500131126         reade  %kds(keyFIARN12 : 6)  FIARN000;
179600131126         DoW  Not %eof(FIARN12L);
179700131126           select;
179800131126             when  V1Cnt1 = *blank;
179900131126               V1Cnt1 = ARNnob;
180000131126             when  V1Cnt2 = *blank;
180100131126               V1Cnt2 = ARNnob;
180200131126             when  V1Cnt3 = *blank;
180300131126               V1Cnt3 = ARNnob;
180400131126             when  V1Cnt4 = *blank;
180500131126               V1Cnt4 = ARNnob;
180600140121             other;
180700140121               leave;
180800131126           endsl;
180900131126           reade  %kds(keyFIARN12 : 6)  FIARN000;
181000131126         EndDo;
181100131128
181200140108         // -?Note R.A.: solo input?
181300131122
181400131126         // -?Reperimento Ultimo contatto precedente?
181500140203         exsr  sr_GetUltContPrec;
181600131121
181700131121       ENDSR;
181800131127
181900131127       //--------------------------------------------------------------
182000131127       //?Impostazione attributi di visualizzazione video DS1.         ?
182100131127       //--------------------------------------------------------------
182200131127       BEGSR  sr_AttrVisDS1;
182300131127
182400131204         // ·?Ultimo Contatto?
182500131204         RevImgUCO  = (V1Duco <> *blank);
182600131204
182700131127         // ·?Comando?
182800131127         RevImgCMD  = (V1Dcmd <> *blank);
182900131204
183000131204         // ·?Variazioni?
183100131204         RevImgVAR  = (V1Dvar <> *blank);
183200131127
183300131127         // ·?Tipo Servizio?
183400131127         RevImgTSP  = (ARBtsp = 'E'  or  ARBtsp = 'H');
183500131127
183600131127         // ·?Fermo Deposito?
183700131127         VisualFDEP = (ARBffd <> *blank);
183800131127
183900131127         // ·?Contrassegno?
184000131129         VisualCASS = (V1Ccas > *zero);
184100131127
184200131127         // ·?Importo Fattura?
184300131129         VisualIFT  = (V1Cift > *zero);
184400131129
184500131129         // ·?Interrogazione Particolarità Giacenza?
184600131129         VisPartGiac  = (V1Cgga <> *blank);
184700131129
184800131129         // ·?Interrogazione Particolarità Consegna?
184900131129         VisPartCons  = (V1Cgma <> *blank);
185000131129
185100131129         // ·?Interrogazione Particolarità Varia?
185200131129         VisPartVarie = (V1Cgva <> *blank);
185300131127
185400131127       ENDSR;
185500131121
185600131121       //--------------------------------------------------------------
185700131121       //?Controllo videata DS1 (spedizione).                          ?
185800131121       //--------------------------------------------------------------
185900131121       BEGSR  sr_CtrDS1;
186000131121
186100131121         // -?Spegnimento indicatori di posizionamento cursore?
186200131127         %subst(IndDspF : 60) = *off;
186300131127
186400131127         // -?Interrogazione Particolarità Giacenza?
186500131127         if  V1Igga = '?';
186600131128           reset  ds7PQRS;
186700131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
186800131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
186900131128           ds7PQRS.DS7cod = V1Cgga;
187000131127           KPJBU = ds7PQRS;
187100131127           trtb69r ( kpjba );
187200131127           clear  V1Igga;
187300131127           ErrGenerico = *on;
187400131127           leavesr;
187500131127         endif;
187600131127
187700131127         // -?Interrogazione Particolarità Consegna?
187800131127         if  V1Igma = '?';
187900131128           reset  ds7PQRS;
188000131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
188100131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
188200131128           ds7PQRS.DS7cod = V1Cgma;
188300131127           KPJBU = ds7PQRS;
188400131127           trtb70r ( kpjba );
188500131127           clear  V1Igma;
188600131127           ErrGenerico = *on;
188700131127           leavesr;
188800131127         endif;
188900131127
189000131127         // -?Interrogazione Particolarità Varie?
189100131127         if  V1Igva = '?';
189200131128           reset  ds7PQRS;
189300131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
189400131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
189500131128           ds7PQRS.DS7cod = V1Cgva;
189600131127           KPJBU = ds7PQRS;
189700131127           trtb73r ( kpjba );
189800131127           clear  V1Igva;
189900131127           ErrGenerico = *on;
190000131127           leavesr;
190100131127         endif;
190200131128
190300131128         clear  V1Igga;
190400131128         clear  V1Igma;
190500131128         clear  V1Igva;
190600131121
190700131121       ENDSR;
190800131121
190900131121       //--------------------------------------------------------------
191000131122       //?Richiamo *pgm x Interrogazione Spedizione (in filiale).      ?
191100131121       //--------------------------------------------------------------
191200131121       BEGSR  sr_VisualSped;
191300131121
191400131121         clear  FNLR36ds;
191500131121         FNLR36ds.LR36aas = k_ARBaas;
191600131121         FNLR36ds.LR36lnp = k_ARBlnp;
191700131121         FNLR36ds.LR36nrs = k_ARBnrs;
191800131121         FNLR36ds.LR36nsp = k_ARBnsp;
191900131121         FNLR36ds.LR36flg = '1';
192000131121         kpjbu = FNLR36ds;
192100131121
192200131121         fnlr36r ( kpjba );
192300131121
192400131121         FNLR36ds = kpjbu;
192500131121         if  FNLR36ds.LR36f03 = *on;
192600131121           $Video = 'S1';
192700131121           ErrGenerico = *on;
192800131121         endif;
192900131121
193000131121       ENDSR;
193100131209
193200131209       //--------------------------------------------------------------
193300131209       //?Inizializzazione videata DO1 (O.R.M.).                       ?
193400131209       //--------------------------------------------------------------
193500131209       BEGSR  sr_InzDO1;
193600131209
193700131209         ds_ORM = S1Hogg;
193800140207
193900140207         // -?Impostazione chiave di accesso per la v.l. 02L?
194000140207         clear  keyFICAU02;
194100140207         k_CA2fgs    = FNLRX2ds.iX2fgs;
194200140207         k_CA2nfv    = FNLRX2ds.iX2dst;
194300140207         k_CA2pdr    = FNLRX2ds.iX2aut;
194400140207         k_CA2tor    = S1Htor;
194500140207         k_CA2ogg    = S1Hogg;
194600140207         k_CA2dtOins = *hival;
194700131209
194800131209         // -?Pulizia videata?
194900131209         clear  LRX2DO1;
195000131209         clear  $UltContPrec;
195100131209
195200131209         // -?Contatto ad Autotrasportatore?
195300140203         clear  sk_TVA;
195400131209         clear  keyFICAU01;
195500140206         //k_CAUfgs    = S1Hfgs;
195600140206         //k_CAUpdr    = C1Cpdr;
195700140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
195800140206         k_CAUpdr    = FNLRX2ds.iX2aut;
195900131209         k_CAUdtOins = S1HdtOins;
196000131209         k_CAUtor    = S1Htor;
196100131209         k_CAUogg    = S1Hogg;
196200140203         chain(N)  %kds( keyFICAU01 )  FICAU000;
196300140203
196400140203         // -?Rec. NON trovato: ricerca con data/ora inserimento più?
196500140203         //  ?recenti (segnalando errore SE non trovato nuovo rec.)?
196600140203         //  ?e riemissione della videata?
196700131209         if  Not %found(FICAU01L)  or  CAUatb <> *blank;
196800140203
196900140203           exsr  sr_Ricarica_Ogg;
197000140203
197100140203           if  Not $RecUpdated;
197200140203             ErrGenerico = *on;
197300140203             ErrMessage  = *on;
197400140203             V1Dmsg = sk_Msg(03);
197500140203             leavesr;
197600140203           endif;
197700140203
197800131209         endif;
197900131209
198000131209         // -?Decodifica autotrasportatore?
198100140203         exsr  sr_GetAUT;
198200131209
198300131209         // -?Comando da impartire al PDA o all'AUT?
198400140203         exsr  sr_GetCMD;
198500131209
198600131209         // -?Tipologica Causale Variazione bolla?
198700140131         //  ?(SE NON "INSERITO")?
198800140203         exsr  sr_GetTVA;
198900131209
199000131209         // -?O.R.M.?
199100131209         exsr  sr_GetORM;
199200131209         if  Not  %found(FNORM01L);
199300131209           leavesr;
199400131209         endif;
199500131209
199600131209         V1Cpoe = ds_ORM.wPOE;
199700131209         V1Cnsr = ds_ORM.wNSR;
199800131209         V1Cnor = ds_ORM.wNOR;
199900131209         V1Cnrv = ds_ORM.wNRV;
200000131211
200100131211         // -?Orari di Apertura del cliente?
200200131211         //  ?Recupero dati da estensione FNORE?
200300131211         //  ?Rcd "O " => Orari apertura?
200400131211         clear dOREorari;
200500131211         k_OREpoe = ds_ORM.wPOE;
200600131211         k_OREnsr = ds_ORM.wNSR;
200700131211         k_OREnor = ds_ORM.wNOR;
200800131211         k_OREnrv = ds_ORM.wNRV;
200900131211
201000131211         chain  %kds(keyFNORE01)  FNORE000;
201100131211
201200131211         if  %found(FNORE01L);
201300131211           dOREorari = OREdati;
201400131211         endif;
201500131211         V1oraAMda = dOREorari.§OREoramda;
201600131211         V1oraAMa  = dOREorari.§OREorama;
201700131211         V1oraPMda = dOREorari.§OREorapda;
201800131211         V1oraPMa  = dOREorari.§OREorapa;
201900131210
202000131211         // -?Ricevuta di Ritiro?
202100131210         if  dORM01.§ORsrm = 'S';
202200131210           V1Datn = 'Ricevuta di Ritiro';
202300131210         endif;
202400140121
202500140121         // -?Ora merce pronta?
202600140121         V1orr  = ORMorr;
202700131209
202800131209         // -?Colli?
202900131209         V1Cncl = ORMncl;
203000131209
203100131209         // -?Peso?
203200131209         V1Cpkg = ORMpkg;
203300131209
203400131209         // -?Bancali?
203500131209         V1Cbnc = ORMbnc;
203600131209
203700131209         // -?Volume?
203800131209         V1Cvlm = ORMvlm;
203900131209
204000131209         // -?Spunda Idraulica?
204100131209         if  ORMspi = 'S';
204200131209           V1Cspi = 'SI';
204300131209         else;
204400131209           V1Cspi = 'NO';
204500131209         endif;
204600131209
204700131209         // -?Cliente Mittente?
204800131209         V1Drsr = ORMrsr;
204900131209         V1Dinr = ORMinr;
205000131209         V1Dlor = ORMlor;
205100131209         V1Dprr = ORMprr;
205200131209         V1Dcar = ORMcar;
205300131209         V1Dnar = ORMnar;
205400131209         //V1Dlpr = %trim( ORMcar ) + ' ' + %trim( ORMlor ) + '  ' +
205500131209         //         ORMprr + ' ' + ORMnar;
205600131209
205700131209         // -?Referente?
205800131209         select;
205900131209           when  ORMrer <> *blank  and  ORMter <> *blank;
206000131209             V1Dref = %trimr( ORMrer ) + ' - ' + %triml( ORMter );
206100131209           when  ORMrer  <> *blank;
206200131209             V1Dref = ORMrer;
206300131209           when  ORMter <> *blank;
206400131209             V1Dref = ORMter;
206500131209         endsl;
206600131210
206700131210         // -?Importo Prepagato?
206800131210         If  ORMtor = 'P';
206900131210
207000131210           clear  keyFIAR404;
207100131210           k_AR44trc = 'M';
207200131210           k_AR44n14 = ds_ORM;
207300131210           chain  %kds(keyFIAR404)  FIAR4004;
207400131210
207500131210           if  %found(FIAR404L);
207600131210
207700131210             clear  keyFIAR601;
207800131210             k_AR6aas = AR4aas;
207900131210             k_AR6lnp = AR4lnp;
208000131210             k_AR6nrs = AR4nrs;
208100131210             k_AR6nsp = AR4nsp;
208200131210             chain  %kds(keyFIAR601 : 4)  FIAR6000;
208300131210
208400131210             if  %found(FIAR601L);
208500131210               w0112 = AR6ift;
208600131210               V1Datn = 'Prepagato: ' + %triml( %editc( w0112 : 'K' ) )
208700131210                                      + AR6div;
208800131210             endif;
208900131210
209000131210           endif;
209100131210
209200131210         EndIf;
209300131209
209400131209         // -?Note O.R.M.?
209500131209         V1Dnot1 = ORMno1;
209600131209         V1Dnot2 = ORMno2;
209700131209
209800131209         // -?Note AUT?
209900140121         clear  keyFNORT01;
210000140121         k_ORTpoe = ds_ORM.wPOE;
210100140121         k_ORTnsr = ds_ORM.wNSR;
210200140121         k_ORTnor = ds_ORM.wNOR;
210300140121         k_ORTnrv = ds_ORM.wNRV;
210400140121         setll  %kds( keyFNORT01 )  FNORT000;
210500140121         reade  %kds( keyFNORT01 : 4 )  FNORT000;
210600140121         DoW  Not %eof(FNORT01L);
210700140121           select;
210800140123             when  ORTgst <> 'S';
210900140121             when  V1Cnt1 = *blank;
211000140121               V1Cnt1 = ORTnob;
211100140121             when  V1Cnt2 = *blank;
211200140121               V1Cnt2 = ORTnob;
211300140121             //when  V1Cnt3 = *blank;
211400140121             //  V1Cnt3 = ORTnob;
211500140121             //when  V1Cnt4 = *blank;
211600140121             //  V1Cnt4 = ORTnob;
211700140121             other;
211800140121               leave;
211900140121           endsl;
212000140121           reade  %kds( keyFNORT01 : 4 )  FNORT000;
212100140121         EndDo;
212200131209
212300140108         // -?Note R.A.: solo input?
212400131209
212500131209         // -?Reperimento Ultimo contatto precedente?
212600140203         exsr  sr_GetUltContPrec;
212700131209
212800131209       ENDSR;
212900131209
213000131209       //--------------------------------------------------------------
213100131209       //?Impostazione attributi di visualizzazione video DO1.         ?
213200131209       //--------------------------------------------------------------
213300131209       BEGSR  sr_AttrVisDO1;
213400131209
213500131209         // ·?Ultimo Contatto?
213600131209         RevImgUCO   = (V1Duco <> *blank);
213700131209
213800131209         // ·?Comando?
213900131209         RevImgCMD   = (V1Dcmd <> *blank);
214000131209
214100131209         // ·?Variazioni?
214200131209         RevImgVAR   = (V1Dvar <> *blank);
214300131209
214400131209         // ·?O.R.M. PrePagato con importo?
214500131210         VisualORMPP = (ORMtor = 'P');
214600131209
214700131209         // ·?Ricevuta di Ritiro?
214800131210         VisualORMRR = (dORM01.§ORsrm = 'S');
214900131209
215000131209       ENDSR;
215100131209
215200131209       //--------------------------------------------------------------
215300131209       //?Controllo dati nella videata D01.                            ?
215400131209       //--------------------------------------------------------------
215500131209       BEGSR  sr_CtrDO1;
215600131209
215700131209         // -?Spegnimento indicatori di posizionamento cursore?
215800140207         %subst(IndDspF : 60) = *off;
215900131209
216000131209       ENDSR;
216100131209
216200131209       //--------------------------------------------------------------
216300131209       //?Richiamo *pgm x Interrogazione O.R.M. (in filiale).          ?
216400131209       //--------------------------------------------------------------
216500131209       BEGSR  sr_VisualORM;
216600131209
216700131209         clear  Parm01;
216800131209         Parm01.pPOE = ds_ORM.wPOE;
216900131209         Parm01.pNSR = ds_ORM.wNSR;
217000131209         Parm01.pNOR = ds_ORM.wNOR;
217100131209         Parm01.pNRV = ds_ORM.wNRV;
217200131209         Parm01.pSCE = '5';
217300131209         kpjbu = Parm01;
217400131209
217500131209         clear  FIDNA1ds;
217600131209         FIDNA1ds.iA1ins = 'N';
217700131209
217800131209         fior07r ( kpjba : FIDNA1ds );
217900131209
218000131209       ENDSR;
218100131128
218200131128       //--------------------------------------------------------------
218300131128       //?Gestione videata W01 (Visualizzazione Ultimo Contatto).      ?
218400131128       //--------------------------------------------------------------
218500131128       BEGSR  sr_GesW01;
218600131128
218700131128         // -?Inizializzazione videata?
218800140122         //$VideoPrec = $Video;
218900140122         //$Video  = 'W1';
219000131128         if  $InzW01 = *on;
219100131128           exsr  sr_InzW01;
219200131128           $InzW01 = *off;
219300131128         endif;
219400131128
219500131128         // -?Emissione window?
219600131128         DoU  dsp_aid = c_F12;
219700131128           exfmt  LRX2W01;
219800131128         EndDo;
219900131128
220000131128         // -?F12=Ritorno?
220100140122         //if  dsp_aid = c_F12;
220200140122         //  $Video  = $VideoPrec;
220300140122         //endif;
220400131128
220500131128       ENDSR;
220600131128
220700131128       //--------------------------------------------------------------
220800131128       //?Inizializzazione window W01.                                 ?
220900131128       //--------------------------------------------------------------
221000131128       BEGSR  sr_InzW01;
221100131128
221200131128         // -?Reperimento Ultimo contatto precedente?
221300140203         //  ?GIÀ effettuato nella subr. "sr_InzDS1/DO1"?
221400131128
221500131128
221600131128         clear  LRX2W01;
221700131128
221800131128         // -?Profilo Utente Telefonata Precedente?
221900140213         W1pruTel   = CA2pruTel;
222000131128
222100131128         // -?Data/Ora Inserimento Telefonata Precedente?
222200140213         ds_DataOra = CA2dtOins;
222300131128         wDate_EUR  = %date( ds_DataOra.wDate );
222400131128         W1dtTel    = %dec( wDate_EUR );
222500131128         W1orTel    = ds_DataOra.wTime;
222600131128
222700131128         // -?Descrizione Comando Telefonata precedente?
222800131128         clear  dCMD;
222900140213         If  CA2desCmd = *blank  and  CA2cmd <> *blank;
223000131128           if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
223100131128                            *omit : *omit :
223200131128                            *omit : *omit :
223300131128                            *omit : *omit : *omit : *omit :
223400131128                            *omit : *omit : *omit : *omit :
223500131128                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
223600131128                            = *zero      AND
223700131128                 ds_TNTBE.TBEatb = *blank;
223800131128             dCMD = ds_TNTBE.TBEuni;
223900131128           else;
224000140213             §CMDdes = CA2cmd + '-' + '? ? ? ? ? ? ? ? ? ?';
224100131128           endif;
224200131128         Else;
224300140213           §CMDdes = %triml( CA2desCmd );
224400131128         EndIf;
224500131128         W1desCmd = §CMDdes;
224600131128
224700131128         // -?Descrizione Variazione Telefonata precedente?
224800140213         If  CA2desVA = *blank;
224900140213           wTipTVA = 'B';
225000140213           ds_TVA  = CA2tva;
225100140213           exsr  sr_GetDesTCV;
225200140213           W1desVa1 = wDesTVA;
225300140213         Else;
225400140213           W1desVa1 = %subst( CA2desVA : 1 : %size(W1desVa1) );
225500140213           W1desVa2 = %subst( CA2desVA : %size(W1desVa1) + 1 );
225600140213         EndIf;
225700131128
225800131128       ENDSR;
225900140226
226000140226       //--------------------------------------------------------------
226100140226       //?Gestione videata W02 (Visualizzazione Altri Dati).           ?
226200140226       //--------------------------------------------------------------
226300140226       BEGSR  sr_GesW02;
226400140226
226500140226         // -?Inizializzazione videata?
226600140226         //$VideoPrec = $Video;
226700140226         //$Video  = 'W2';
226800140226         if  $InzW02 = *on;
226900140226           exsr  sr_InzW02;
227000140226           $InzW02 = *off;
227100140226         endif;
227200140226
227300140226         // -?Emissione window?
227400140226         DoU  dsp_aid = c_F12;
227500140226           exfmt  LRX2W02;
227600140226         EndDo;
227700140226
227800140226         // -?F12=Ritorno?
227900140226         //if  dsp_aid = c_F12;
228000140226         //  $Video  = $VideoPrec;
228100140226         //endif;
228200140226
228300140226       ENDSR;
228400140226
228500140226       //--------------------------------------------------------------
228600140226       //?Inizializzazione window W02.                                 ?
228700140226       //--------------------------------------------------------------
228800140226       BEGSR  sr_InzW02;
228900140226
229000140226         clear  LRX2W02;
229100140226
229200140226         // -?Profilo Utente Inserimento Telefonata?
229300140226         W2pruTel   = CAUpruIns;
229400140226
229500140226         // -?Data/Ora Inserimento Telefonata?
229600140226         ds_DataOra = CAUdtOins;
229700140226         wDate_EUR  = %date( ds_DataOra.wDate );
229800140226         W2dtTel    = %dec( wDate_EUR );
229900140226         W2orTel    = ds_DataOra.wTime;
230000140226
230100140226         // -?Posizionamento (verticale) Window?
230200140226         select;
230300140226           when  $Video  = 'DS';
230400140226             W2rig = 16;
230500140226           when  $Video  = 'DO';
230600140226             W2rig = 14;
230700140226           //other;
230800140226           //  W2rig = 2;
230900140226         endsl;
231000140226
231100140226       ENDSR;
231200140207
231300140207       //--------------------------------------------------------------
231400140207       //?Reperimento dati della Spedizione (Consegna).                ?
231500140207       //--------------------------------------------------------------
231600140207       BEGSR  sr_GetSped;
231700140207
231800140207         ds_Spedizione = CAUogg;
231900140207
232000140207         // -?Reperimento dati da Bolle Arrivi: Testate (FNARB)?
232100140207         clear  keyFNARB01;
232200140207         k_ARBaas = ds_Spedizione.wAAS;
232300140207         k_ARBlnp = ds_Spedizione.wLNP;
232400140207         k_ARBnrs = ds_Spedizione.wNRS;
232500140207         k_ARBnsp = ds_Spedizione.wNSP;
232600140207
232700140207         chain  %kds( keyFNARB01 )  FNARB000;
232800140207
232900140207         if  Not  %found(FNARB01L);
233000140207           ARBrsd = *all'? ';
233100140207         endif;
233200140207
233300140207         // -?Estensione ragione sociale destinatario da FIAR4 rec. "D"?
233400140207         clear  keyFIAR401;
233500140207         k_AR4aas = ds_Spedizione.wAAS;
233600140207         k_AR4lnp = ds_Spedizione.wLNP;
233700140207         k_AR4nrs = ds_Spedizione.wNRS;
233800140207         k_AR4nsp = ds_Spedizione.wNSP;
233900140207         k_AR4trc = 'D';
234000140207
234100140207         chain  %kds(keyFIAR401)  FIAR4000;
234200140207
234300140207         if  Not  %found(FIAR401L);
234400140207           clear  AR4not;
234500140207         endif;
234600140207
234700140207       ENDSR;
234800140207
234900140207       //--------------------------------------------------------------
235000140207       //?Reperimento dati dell'Ordine Ritiro Merce (Ritiro).          ?
235100140207       //--------------------------------------------------------------
235200140207       BEGSR  sr_GetORM;
235300140207
235400140207         ds_ORM = CAUogg;
235500140207
235600140207         clear  keyFNORM01;
235700140207         k_ORMpoe = ds_ORM.wPOE;
235800140207         k_ORMnsr = ds_ORM.wNSR;
235900140207         k_ORMnor = ds_ORM.wNOR;
236000140207         k_ORMnrv = ds_ORM.wNRV;
236100140207
236200140207         chain  %kds( keyFNORM01 )  FNORM000;
236300140207
236400140207         if  Not  %found(FNORM01L);
236500140207           ORMrsr = *all'? ';
236600140207           clear  dORM01;
236700140207         else;
236800140207           dORM01 = ORMflo;
236900140207         endif;
237000140207
237100140207       ENDSR;
237200140203
237300140203       //--------------------------------------------------------------
237400140203       //?Reperimento ed Impostazione a video delle informazioni AUT.  ?
237500140203       //--------------------------------------------------------------
237600140203       BEGSR  sr_GetAUT;
237700140203
237800140203         // -?Decodifica autotrasportatore?
237900140203         k_APDpdr = CAUpdr;
238000140203         chain  %kds( keyFIAPD01 )  FIAPD000;
238100140203         if  Not %found(FIAPD01L)  or  APDatb <> *blank;
238200140203           clear  APDrsc;
238300140203         endif;
238400140203
238500140203         V1Cpdr = CAUpdr;
238600140203         V1Dpdr = APDrsc;
238700140203
238800140203         // -?Num. Telefono Autotrasportatore?
238900140203         //k_APD4pdr = CAUpdr;
239000140203         //chain  %kds( keyFIAPD41 )  FIAPD400;
239100140203         //if  %found(FIAPD41L)  and  APD4atb = *blank;
239200140203         //  dAPD4tel = APD4uni;
239300140203         //endif;
239400140203         //
239500140203         //V1Dtel = dAPD4tel.§APD4tel;
239600140203
239700140203       ENDSR;
239800140203
239900140203       //--------------------------------------------------------------
240000140203       //?Reperimento ed Impostazione a video delle informazioni CMD.  ?
240100140203       //--------------------------------------------------------------
240200140203       BEGSR  sr_GetCMD;
240300140203
240400140203         // -?Comando da impartire al PDA o all'AUT?
240500140203         clear  dCMD;
240600140203         If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
240700140203           if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
240800140203                            *omit : *omit :
240900140203                            *omit : *omit :
241000140203                            *omit : *omit : *omit : *omit :
241100140203                            *omit : *omit : *omit : *omit :
241200140203                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
241300140203                            = *zero      AND
241400140203                 ds_TNTBE.TBEatb = *blank;
241500140203             dCMD = ds_TNTBE.TBEuni;
241600140203           else;
241700140203             §CMDdes = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ?';
241800140203           endif;
241900140203         Else;
242000140203           §CMDdes = %triml( CAUdesCmd );
242100140203         EndIf;
242200140203
242300140203         if  §CMDdes <> *blank;
242400140203           xx = ( %size(V1Dcmd) - %len( %trim(§CMDdes) ) ) / 2;
242500140203           %subst( V1Dcmd : xx + 1 ) = §CMDdes;
242600140203         endif;
242700140203
242800140203       ENDSR;
242900140203
243000140203       //--------------------------------------------------------------
243100140203       //?Reperimento ed Impostazione a video delle informazioni TVA.  ?
243200140203       //--------------------------------------------------------------
243300140203       BEGSR  sr_GetTVA;
243400140203
243500140203         // -?Tipologica Causale Variazione bolla?
243600140203         //  ?(SE NON "INSERITO")?
243700140203         If  CAUcmd <> c_Cmd_Inserito;
243800140203           If  CAUdesVA = *blank;
243900140203             wTipTVA = *blank;
244000140213             ds_TVA  = CAUtva;
244100140203             exsr  sr_GetDesTCV;
244200140203             V1Dvar  = wDesTVA;
244300140203             //if  %subst( wDesTVA : %size( V1Dvar ) + 1 ) <> *blank;
244400140203             //  %subst( V1Dvar : %size( V1Dvar ) - 3 ) = ' (+)';
244500140203             //endif;
244600140203           Else;
244700140203             V1Dvar = CAUdesVa;
244800140203             //if  %subst( CAUdesVA : %size( V1Dvar ) + 1 ) <> *blank;
244900140203             //  %subst( V1Dvar : %size( V1Dvar ) - 3 ) = ' (+)';
245000140203             //endif;
245100140203           EndIf;
245200140203         EndIf;
245300140203
245400140203       ENDSR;
245500131205
245600131205       //--------------------------------------------------------------
245700131205       //?Reperimento dati della Spedizione (Consegna).                ?
245800131205       //--------------------------------------------------------------
245900131205       BEGSR  sr_GetDesTCV;
246000131205
246100131205         clear  wDesTVA;
246200131205
246300131205         For  xx = 1  To  %elem(sk_TVA);
246400131205
246500131205           clear  dTCV;
246600131205
246700131205           If  sk_TVA(xx) <> *blank;
246800131205
246900131205             yy = %lookup( sk_TVA(xx) : sk_TCV );
247000131205
247100131205             If  yy > *zero;
247200131205
247300131205               dTCV = sk_TCVd(yy);
247400131205               If  wDesTVA = *blank;
247500131205                 select;
247600131205                   when  wTipTVA = 'B';
247700131205                     wDesTVA = %triml( §TCVdesB );
247800131205                   when  wTipTVA = 'V';
247900131205                     wDesTVA = %triml( §TCVdesV );
248000131205                   other;
248100131205                     wDesTVA = %triml( §TCVdes );
248200131205                 endsl;
248300131205               Else;
248400131205                 select;
248500131205                   when  wTipTVA = 'B';
248600131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
248700131205                               %triml( §TCVdesB );
248800131205                   when  wTipTVA = 'V';
248900131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
249000131205                               %triml( §TCVdesV );
249100131205                   other;
249200131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
249300131205                               %triml( §TCVdes );
249400131205                 endsl;
249500131205               EndIf;
249600131205
249700131205             Else;
249800131205
249900131205               §TCVdesB  = '???' + sk_TVA(xx) + '???';
250000131205               if  wDesTVA = *blank;
250100131205                 wDesTVA = %triml( §TCVdesB );
250200131205               else;
250300131205                 wDesTVA = %trimr( wDesTVA ) + '-' +
250400131205                           %triml( §TCVdesB );
250500131205               endif;
250600131205
250700131205             EndIf;
250800131205
250900131205           EndIf;
251000131205
251100131205         EndFor;
251200131205
251300131205       ENDSR;
251400140203
251500140203       //--------------------------------------------------------------
251600140203       //?Reperimento ed Impostazione a video dell'eventuale           ?
251700140203       //?Ultimo Contatto Precedente.                                  ?
251800140203       //--------------------------------------------------------------
251900140203       BEGSR  sr_GetUltContPrec;
252000140203
252100140203         // -?Reperimento Ultimo contatto precedente?
252200140206         //clear  keyFICAU02;      ?(già impostata nella subr. sr_InzDS1/DO1)?
252300140206         //k_CA2fgs    = CAUfgs;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252400140206         //k_CA2nfv    = CAUnfv;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252500140206         //k_CA2pdr    = CAUpdr;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252600140206         //k_CA2tor    = CAUtor;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252700140206         //k_CA2ogg    = CAUogg;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252800140206         k_CA2dtOins = CAUdtOins;
252900140206
253000140206         chain  %kds( keyFICAU02 )  FICAU002;
253100140206         reade  %kds( keyFICAU02 : 5)  FICAU002;
253200140206
253300140203         // -?NON?trovato alcun contatto precedente?
253400140206         if  %eof(FICAU02L);
253500140203           leavesr;
253600140203         endif;
253700140206
253800140203         // -?Trovato l'ultimo contatto precedente?
253900140203         $UltContPrec = *on;
254000140206         ds_DataOra = CA2dtOins;
254100140203         V1Duco     = 'Ultimo Contatto alle ' +
254200140203                      %editw( ds_DataOra.wTime4 : '  :  ' );
254300140203         // *...+....1....+....2....+.
254400140203         // Ultimo Contatto alle 12:34
254500140203
254600140203       ENDSR;
254700140203
254800140203       //--------------------------------------------------------------
254900140203       //?Reperimento (nuovo) ultimo record per l'oggetto in esame.    ?
255000140203       //--------------------------------------------------------------
255100140203       BEGSR  sr_Verifica_Ogg;
255200140203
255300140203         select;
255400140203
255500140203           when  S1Htor = c_Consegna;
255600140203             $RecUpdated = ( V1Cpdr  <> SaveDS1.V1Cpdr   or
255700140203                             V1Dpdr  <> SaveDS1.V1Dpdr   or
255800140203                             V1Duco  <> SaveDS1.V1Duco   or
255900140203                             V1Dcmd  <> SaveDS1.V1Dcmd   or
256000140203                             V1Dvar  <> SaveDS1.V1Dvar   or
256100140203                             V1Clnp  <> SaveDS1.V1Clnp   or
256200140203                             V1Cnrs  <> SaveDS1.V1Cnrs   or
256300140203                             V1Cnsp  <> SaveDS1.V1Cnsp   or
256400140203                             V1Dtsp  <> SaveDS1.V1Dtsp   or
256500140203                             V1Drsd  <> SaveDS1.V1Drsd   or
256600140203                             V1Dind  <> SaveDS1.V1Dind   or
256700140203                             V1Dlpd  <> SaveDS1.V1Dlpd   or
256800140203                             V1Cncl  <> SaveDS1.V1Cncl   or
256900140203                             V1Cpkb  <> SaveDS1.V1Cpkb   or
257000140203                             V1Dref  <> SaveDS1.V1Dref   or
257100140203                           //V1Igga  <> SaveDS1.V1Igga   or
257200140203                             V1Cgga  <> SaveDS1.V1Cgga   or
257300140203                           //V1Igma  <> SaveDS1.V1Igma   or
257400140203                             V1Cgma  <> SaveDS1.V1Cgma   or
257500140203                           //V1Igva  <> SaveDS1.V1Igva   or
257600140203                             V1Cgva  <> SaveDS1.V1Cgva   or
257700140203                             V1Ccas  <> SaveDS1.V1Ccas   or
257800140203                             V1Cvca  <> SaveDS1.V1Cvca   or
257900140203                             V1Cift  <> SaveDS1.V1Cift   or
258000140203                             V1Cdiv  <> SaveDS1.V1Cdiv   or
258100140203                             V1Dnot8 <> SaveDS1.V1Dnot8  or
258200140203                             V1Dnot9 <> SaveDS1.V1Dnot9  or
258300140203                             V1Cnt1  <> SaveDS1.V1Cnt1   or
258400140203                             V1Cnt2  <> SaveDS1.V1Cnt2   or
258500140203                             V1Cnt3  <> SaveDS1.V1Cnt3   or
258600140203                             V1Cnt4  <> SaveDS1.V1Cnt4   or
258700140203                             V1Nra1  <> SaveDS1.V1Nra1   or
258800140203                             V1Nra2  <> SaveDS1.V1Nra2 );
258900140203
259000140203           when  S1Htor = c_Ritiro;
259100140203             $RecUpdated = ( V1Cpdr    <> SaveDO1.V1Cpdr     or
259200140203                             V1Dpdr    <> SaveDO1.V1Dpdr     or
259300140203                             V1Duco    <> SaveDO1.V1Duco     or
259400140203                             V1Dcmd    <> SaveDO1.V1Dcmd     or
259500140203                             V1Dvar    <> SaveDO1.V1Dvar     or
259600140203                             V1Cpoe    <> SaveDO1.V1Cpoe     or
259700140203                             V1Cnsr    <> SaveDO1.V1Cnsr     or
259800140203                             V1Cnor    <> SaveDO1.V1Cnor     or
259900140203                             V1Cnrv    <> SaveDO1.V1Cnrv     or
260000140203                             V1oraAMda <> SaveDO1.V1oraAMda  or
260100140203                             V1oraAMa  <> SaveDO1.V1oraAMa   or
260200140203                           //V1oraSep  <> SaveDO1.V1oraSep   or
260300140203                             V1oraPMda <> SaveDO1.V1oraPMda  or
260400140203                             V1oraPMa  <> SaveDO1.V1oraPMa   or
260500140203                             V1datN    <> SaveDO1.V1datN     or
260600140203                             V1Drsr    <> SaveDO1.V1Drsr     or
260700140203                             V1Dinr    <> SaveDO1.V1Dinr     or
260800140203                             V1Dlor    <> SaveDO1.V1Dlor     or
260900140203                             V1Dprr    <> SaveDO1.V1Dprr     or
261000140203                             V1Dcar    <> SaveDO1.V1Dcar     or
261100140203                             V1Dnar    <> SaveDO1.V1Dnar     or
261200140203                             V1orr     <> SaveDO1.V1orr      or
261300140203                             V1Dref    <> SaveDO1.V1Dref     or
261400140203                             V1Cncl    <> SaveDO1.V1Cncl     or
261500140203                             V1Cpkg    <> SaveDO1.V1Cpkg     or
261600140203                             V1Cbnc    <> SaveDO1.V1Cbnc     or
261700140203                             V1Cvlm    <> SaveDO1.V1Cvlm     or
261800140203                             V1Cspi    <> SaveDO1.V1Cspi     or
261900140203                             V1Dnot1   <> SaveDO1.V1Dnot1    or
262000140203                             V1Dnot2   <> SaveDO1.V1Dnot2    or
262100140203                             V1Cnt1    <> SaveDO1.V1Cnt1     or
262200140203                             V1Cnt2    <> SaveDO1.V1Cnt2     or
262300140203                             V1nra1    <> SaveDO1.V1nra1     or
262400140203                             V1nra2    <> SaveDO1.V1nra2 );
262500140203
262600140203         endsl;
262700140203
262800140203       ENDSR;
262900140203
263000140203       //--------------------------------------------------------------
263100140203       //?Memorizzazione dati da confrontare per rilevare variazioni.  ?
263200140203       //--------------------------------------------------------------
263300140203       BEGSR  sr_Memo_Dati;
263400140203
263500140203         Select;
263600140203
263700140203           When  S1Htor = c_Consegna;
263800140203             SaveDS1.V1Cpdr  = V1Cpdr;
263900140203             SaveDS1.V1Dpdr  = V1Dpdr;
264000140203             SaveDS1.V1Duco  = V1Duco;
264100140203             SaveDS1.V1Dcmd  = V1Dcmd;
264200140203             SaveDS1.V1Dvar  = V1Dvar;
264300140203             SaveDS1.V1Clnp  = V1Clnp;
264400140203             SaveDS1.V1Cnrs  = V1Cnrs;
264500140203             SaveDS1.V1Cnsp  = V1Cnsp;
264600140203             SaveDS1.V1Dtsp  = V1Dtsp;
264700140203             SaveDS1.V1Drsd  = V1Drsd;
264800140203             SaveDS1.V1Dind  = V1Dind;
264900140203             SaveDS1.V1Dlpd  = V1Dlpd;
265000140203             SaveDS1.V1Cncl  = V1Cncl;
265100140203             SaveDS1.V1Cpkb  = V1Cpkb;
265200140203             SaveDS1.V1Dref  = V1Dref;
265300140203           //SaveDS1.V1Igga  = V1Igga;
265400140203             SaveDS1.V1Cgga  = V1Cgga;
265500140203           //SaveDS1.V1Igma  = V1Igma;
265600140203             SaveDS1.V1Cgma  = V1Cgma;
265700140203           //SaveDS1.V1Igva  = V1Igva;
265800140203             SaveDS1.V1Cgva  = V1Cgva;
265900140203             SaveDS1.V1Ccas  = V1Ccas;
266000140203             SaveDS1.V1Cvca  = V1Cvca;
266100140203             SaveDS1.V1Cift  = V1Cift;
266200140203             SaveDS1.V1Cdiv  = V1Cdiv;
266300140203             SaveDS1.V1Dnot8 = V1Dnot8;
266400140203             SaveDS1.V1Dnot9 = V1Dnot9;
266500140203             SaveDS1.V1Cnt1  = V1Cnt1;
266600140203             SaveDS1.V1Cnt2  = V1Cnt2;
266700140203             SaveDS1.V1Cnt3  = V1Cnt3;
266800140203             SaveDS1.V1Cnt4  = V1Cnt4;
266900140203             SaveDS1.V1Nra1  = V1Nra1;
267000140203             SaveDS1.V1Nra2  = V1Nra2;
267100140203
267200140203           When  S1Htor = c_Ritiro;
267300140203             SaveDO1.V1Cpdr    = V1Cpdr;
267400140203             SaveDO1.V1Dpdr    = V1Dpdr;
267500140203             SaveDO1.V1Duco    = V1Duco;
267600140203             SaveDO1.V1Dcmd    = V1Dcmd;
267700140203             SaveDO1.V1Dvar    = V1Dvar;
267800140203             SaveDO1.V1Cpoe    = V1Cpoe;
267900140203             SaveDO1.V1Cnsr    = V1Cnsr;
268000140203             SaveDO1.V1Cnor    = V1Cnor;
268100140203             SaveDO1.V1Cnrv    = V1Cnrv;
268200140203             SaveDO1.V1oraAMda = V1oraAMda;
268300140203             SaveDO1.V1oraAMa  = V1oraAMa;
268400140203           //SaveDO1.V1oraSep  = V1oraSep;
268500140203             SaveDO1.V1oraPMda = V1oraPMda;
268600140203             SaveDO1.V1oraPMa  = V1oraPMa;
268700140203             SaveDO1.V1datN    = V1datN;
268800140203             SaveDO1.V1Drsr    = V1Drsr;
268900140203             SaveDO1.V1Dinr    = V1Dinr;
269000140203             SaveDO1.V1Dlor    = V1Dlor;
269100140203             SaveDO1.V1Dprr    = V1Dprr;
269200140203             SaveDO1.V1Dcar    = V1Dcar;
269300140203             SaveDO1.V1Dnar    = V1Dnar;
269400140203             SaveDO1.V1orr     = V1orr;
269500140203             SaveDO1.V1Dref    = V1Dref;
269600140203             SaveDO1.V1Cncl    = V1Cncl;
269700140203             SaveDO1.V1Cpkg    = V1Cpkg;
269800140203             SaveDO1.V1Cbnc    = V1Cbnc;
269900140203             SaveDO1.V1Cvlm    = V1Cvlm;
270000140203             SaveDO1.V1Cspi    = V1Cspi;
270100140203             SaveDO1.V1Dnot1   = V1Dnot1;
270200140203             SaveDO1.V1Dnot2   = V1Dnot2;
270300140203             SaveDO1.V1Cnt1    = V1Cnt1;
270400140203             SaveDO1.V1Cnt2    = V1Cnt2;
270500140203             SaveDO1.V1nra1    = V1nra1;
270600140203             SaveDO1.V1nra2    = V1nra2;
270700140203
270800140203         EndSl;
270900140203
271000140203       ENDSR;
271100131122
271200131122       //--------------------------------------------------------------
271300140203       //?Gestione tasto funzionale F6=Accetta premuto da DS1 o DO1.   ?
271400131122       //--------------------------------------------------------------
271500140121       BEGSR  sr_F06D01;
271600131205
271700131205         // -?Riaggancio del record del Contatto ad Autotrasportatore?
271800131205         //  ?da aggiornare (allocandolo)?
271900131205         clear  keyFICAU01;
272000140206         //k_CAUfgs    = S1Hfgs;
272100140206         //k_CAUpdr    = C1Cpdr;
272200140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
272300140206         k_CAUpdr    = FNLRX2ds.iX2aut;
272400131205         k_CAUdtOins = S1HdtOins;
272500131205         k_CAUtor    = S1Htor;
272600131205         k_CAUogg    = S1Hogg;
272700131205         chain  %kds( keyFICAU01 )  FICAU000;
272800140207
272900140207         // -?Rec. NON trovato: si torna alla ri-emissione della?
273000140207         //  ?videata (come NON fosse stato premuto F6)?
273100140207         If  Not %found(FICAU01L)  or  CAUatb <> *blank;
273200140207           ErrGenerico = *on;
273300140207           leavesr;
273400140207         EndIf;
273500131122
273600140506         // -?Richiamo del *pgm FIDNA6R = Scrittura R.A. cieca?
273700131205         //  ?(causale " 40") - vedi file FITGD00F+FITGN00F?
273800140506         exsr  sr_CallFIDNA6;
273900131205
274000131205         // -?Aggiornamento Telefonata?
274100131205         exsr  sr_UpdFICAU;
274200140121
274300140121         // -?Aggiornamento flag "ORM scaricato a PDA"?
274400140128         if  CAUtor = c_Ritiro;
274500140128            exsr  sr_UpdFNORG;
274600140128         endif;
274700131205
274800131205         // -?Impostazione parametro di ritorno?
274900131205         FNLRX2ds.oX2upd = *on;
275000131205
275100131205         // -?Ritorno al subfile?
275200131205         $Video = 'S1';
275300131205         $InzS01 = *on;
275400131122
275500131122       ENDSR;
275600140203
275700140203       //--------------------------------------------------------------
275800140203       //?Reperimento (nuovo) ultimo record per l'oggetto in esame.    ?
275900140203       //--------------------------------------------------------------
276000140203       BEGSR  sr_Ricarica_Ogg;
276100140203
276200140203         // -?Ricerca record relativo all'oggetto in esame con Data/Ora?
276300140203         //  ?inserimento aggiornata?
276400140206         //clear  keyFICAU02;      ?(già impostata nella subr. sr_InzDS1/DO1)?
276500140206         //k_CA2fgs    = CAUfgs;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276600140206         //k_CA2nfv    = CAUnfv;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276700140206         //k_CA2pdr    = CAUpdr;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276800140206         //k_CA2tor    = CAUtor;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276900140206         //k_CA2ogg    = CAUogg;   ?(già impostata nella subr. sr_InzDS1/DO1)?
277000140207         ////k_CA2dtOins = *hival; ?(già impostata nella subr. sr_InzDS1/DO1)?
277100140206
277200140206         chain  %kds( keyFICAU02 : 5 )  FICAU002;
277300140203
277400140206         $RecUpdated = ( %found( FICAU02L ) );
277500140203
277600140203         // -?Trovato: si aggiornano i dati a video?
277700140203         //  ?(di cui verrà effettuata la riemissione)?
277800140203         If  $RecUpdated;
277900140206           k_CAUdtOins = CA2dtOins;
278000140207           chain(N)  %kds( keyFICAU01 )  FICAU000;
278100140206           S1HdtOins   = CA2dtOins;
278200140206           S01hmi   = %int( %subst( CA2dtOins : 9 : 4 ) );
278300140203           $InzS01  = *on;
278400140207           $InzD01  = *on;
278500140203         EndIf;
278600140203
278700140203       ENDSR;
278800131205
278900131205       //--------------------------------------------------------------
279000131205       //?Scrittura Richiesta Assistenza cieca (causale " 40")         ?
279100131205       //--------------------------------------------------------------
279200140506       BEGSR  sr_CallFIDNA6;
279300131205
279400140506         clear  FIDNA6ds;
279500140506
279600140506         iDA06mad = c_IDA06mad;
279700131209         select;
279800131209           when  CAUtor = c_Consegna;
279900140506             iDA06tor = c_Spedizione;
280000131209           when  CAUtor = c_Ritiro;
280100140506             iDA06tor = c_ORM;
280200131209         endsl;
280300140506         iDA06ogg = CAUogg;
280400140506         iDA06no1 = V1nra1 + V1nra2;
280500140506         iDA06no2 = 'AUT: '  + %editc( CAUpdr : 'X' ) +
280600140506                    ' DIST: ' + %editc( CAUnfv : 'X' );
280700131205         if  §CMDdes <> *blank;
280800140506           iDA06no2 = %trimr(iDA06no2) + ' ' + %trim(§CMDdes);
280900131205         endif;
281000131205         select;
281100131205           when  CAUdesVA <> *blank;
281200140506             //iDA06no2 = %trimr(iDA06no2) + ' VARIAZ: ' + %trim(CAUdesVA);
281300131205             if  §CMDdes = *blank;
281400140506               iDA06no2 = %trimr(iDA06no2) + ' VARIAZ: ' + %trim(CAUdesVA);
281500131205             else;
281600140506               %subst( iDA06no2 : 46 ) = ' VARIAZ: ' + %trim(CAUdesVA);
281700131205             endif;
281800131205           when  wDesTVA  <> *blank;
281900131205             if  §CMDdes = *blank;
282000140506               iDA06no2 = %trimr(iDA06no2) + ' VARIAZ: ' + %trim(wDesTVA);
282100131205             else;
282200140506               %subst( iDA06no2 : 46 ) = ' VARIAZ: ' + %trim(wDesTVA);
282300131205             endif;
282400131205         endsl;
282500140506         iDA06ute = DUTute;
282600140506         iDA06fil = DUTpou;
282700140506
282800140506         clear  dTGD01;
282900140506         //dTGD01.§TGDarap = *blank;
283000140506         iDA06flo = dTGD01;
283100131205
283200140108         // -?Richiesta Accettata?
283300140506         //%subst( iDA06no2 : %size(iDA06no2) - 10 ) = ' (ESEGUITA)';
283400131205
283500140506         FIDNA6R ( FIDNA6ds );
283600131205
283700131205       ENDSR;
283800131205
283900131205       //--------------------------------------------------------------
284000131205       //?Aggiornamento telefonata (file FICAU00F).                    ?
284100131205       //--------------------------------------------------------------
284200131205       BEGSR  sr_UpdFICAU;
284300131205
284400131205         // -?Reperimento orario corrente?
284500131205         wTimeStamp = %timestamp();
284600131205
284700131205         // -?Impostazione dei campi relativi all'"ESITO"?
284800131205         CAUflt    = 'T';
284900131205         CAUdtoTel = %subst( %char( %dec( wTimeStamp ) ) : 1 : 14 );
285000140110         CAUesito  = c_Eseguita;
285100131205         CAUpruTel = SDSjobUser;
285200131205         CAUterm   = SDSjobName;
285300131205         CAUesec   = 'T';
285400131205
285500131205
285600131205         // -?Aggiornamento rec. FICAU00F?
285700140203         //_______________
285800140203         update  FICAU000;
285900140203         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
286000131205
286100131205       ENDSR;
286200140121
286300140121       //--------------------------------------------------------------
286400140121       //?Aggiornamento O.R.M. (file FNORG00F).                        ?
286500140121       //--------------------------------------------------------------
286600140121       BEGSR  sr_UpdFNORG;
286700140121
286800140121         // -?Aggancia il record dell'Estensione Testata?
286900140121         clear  keyFNORG01;
287000140121         k_ORGpoe = ds_ORM.wPOE;
287100140121         k_ORGnsr = ds_ORM.wNSR;
287200140121         k_ORGnor = ds_ORM.wNOR;
287300140121         k_ORGnrv = ds_ORM.wNRV;
287400140121         chain  %kds( keyFNORG01 )  FNORG000;
287500140121
287600140121         // -?IMPOSSIBILE?che NON venga reperito il rec. da aggiornare?
287700140121         if  Not %found(FNORG01L);
287800140121           //clear  FNORG000;
287900140121           //CAUpoe = k_ORGpoe;
288000140121           //CAUnsr = k_ORGnsr;
288100140121           //CAUnor = k_ORGnor;
288200140121           //CAUnrv = k_ORGnrv;
288300140121           //CAUpor = .....;
288400140121           //--- oppure: ----------
288500140121           //*inH2 = *on;
288600140121           //$Fine = *on;
288700140121           //ErrGenerico = *on;
288800140121           //leavesr;
288900140121         endif;
289000140121
289100140121         // -?Aggiornamento flag "ORM scaricato a PDA"?
289200140123         dORG01 = ORGflo;
289300140121         select;
289400140121           // -?Inserito => Sì?
289500140121           when  CAUcmd = 'I';
289600140121             dORG01.§ORGpda = 'S';
289700140121           // -?Annullato => No?
289800140121           when  CAUcmd = 'A';
289900140121             clear  dORG01.§ORGpda;
290000140121         endsl;
290100140123         ORGflo = dORG01;
290200140121
290300140121         // -?Aggiornamento rec. FNORG00F?
290400140121         //if  %found(FNORG01L);
290500140121           //_______________
290600140121           update  FNORG000;
290700140121           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
290800140121         //endif;
290900140121
291000140121       ENDSR;
291100131030
291200131030       //--------------------------------------------------------------
291300131030       //?Operazioni finali.                                           ?
291400131030       //--------------------------------------------------------------
291500131030       BEGSR  sr_RoutEnd;
291600131120
291700131120         // -?Chiusura funzioni precedentemente aperte?
291800131120         cloTabella ( c_Tntbe );
291900131030
292000131120         // -?Restituzione eventuali parametri?
292100131121         kpjbu = FNLRX2ds;
292200131104
292300131120         // -?Chiusura *pgm?
292400131030         return;
292500131030
292600131030       ENDSR;
292700131030
292800131030      /end-free
292900131030
293000131120** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
293100131120Superato il numero massimo di telefonate visualizzabili                         1
293200131120Opzione errata                                                                  2
293300131205ERRORE: NON risulta più reperibile questa telefonata                            3
293400140206Attenzione: sono stati variati dei dati. VERIFICARE                             4
