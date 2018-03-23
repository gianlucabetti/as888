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
010400131211     d c_IA3mad        c                   const(' 40')
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
034600131211
034700131211       // -?Ds per rcd "O " FNORE - orari apertura?
034800131211     d dOREorari     e ds                  inz   qualified
034900131030
035000131030       //--------------------------------------------------------------
035100131030       //?Definizione variabili globali.                               ?
035200131030       //--------------------------------------------------------------
035300131030
035400131030       // -?Flags booleani?
035500131030     d $Fine           s               n   inz
035600131030     d $EoF            s               n   inz
035700131030     d $InzS01         s               n   inz(*on)
035800140203     d $InzD01         s               n   inz(*on)
035900131128     d $InzW01         s               n   inz(*on)
036000140226     d $InzW02         s               n   inz(*on)
036100131121     d $RecOK          s               n   inz
036200131128     d $UltContPrec    s               n   inz
036300140203     d $RecUpdated     s               n   inz
036400131122
036500131122       // -?Indici di schiera?
036600131122     d xx              s              3  0 inz
036700131125     d yy              s              3  0 inz
036800131030
036900131030       // -?Variabili per la gestione del video?
037000131125     d $Video          s              2    inz('S1')
037100131104     d w_SflPag        s              3  0 inz
037200131030     d wPag            s              4  0 inz
037300131030     d wRig            s              3  0 inz
037400140108     d wOrdSfl         s              1  0 inz
037500131030     d S01nrr          s                   like(C1RcdNbr) inz
037600131030     d SavS01csr       s                   like(C1RcdNbr) inz
037700131212     d SavMAXnrr       s                   like(C1RcdNbr) inz
037800140226     d W2rig           s              3  0 inz(15)
037900140226     d*// W2col           s              3  0 inz(54)
038000131030
038100131030       // -?Variabili di comodo?
038200131121     d wDate_EUR       s               d   datfmt(*eur)  inz(*loval)
038300131205     d wTimeStamp      s               z   inz(*loval)
038400131127     d wTipTVA         s              1    inz
038500131210     d w0112           s             11  2 inz
038600140108     d wDesTVA         s                   like(V1Dvar)   inz
038700131204
038800131204       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
038900131204
039000131204       //--------------------------------------------------------------
039100131204       // -?Constants?
039200131204       //--------------------------------------------------------------
039300131204       // -?MaxKey - Maximum number of key fields allowed by this program?
039400131204     d MaxKey          c                   const(2)
039500131204       // -?Sort order:?
039600131204       //  ?1 = Sort in an ascending sequence?
039700131204       //  ?2 = Sort in a descending sequence?
039800131204     d Ascendente      c                   const(1)
039900131204     d Discendente     c                   const(2)
040000131204       // -?Key data type:?
040100131204       //  ? 0 = Signed binary?
040200131204       //  ? 2 = Signed zoned decimal?
040300131204       //  ? 3 = Signed packed decimal?
040400131204       //  ? 6 = Character with no national language sort sequence applied,?
040500131204       //  ?     if specified?
040600131204       //  ? 7 = Unsigned packed decimal?
040700131204       //  ?     All numbers will have the sign forced positive ('F'X).?
040800131204       //  ? 8 = Unsigned zoned decimal?
040900131204       //  ?     All numbers will have the sign forced positive ('F'X).?
041000131204       //  ? 9 = Unsigned binary?
041100131204       //  ?14 = Date in form of DD/MM/YY?
041200131204       //  ?15 = Date in form of DD.MM.YYYY?
041300131204     d Numero          c                   const(2)
041400131204     d Carattere       c                   const(6)
041500131204       //
041600131204     d Put             c                   const(1)
041700131204     d EndPut          c                   const(2)
041800131204     d Get             c                   const(3)
041900131204
042000131204       //--------------------------------------------------------------
042100131204       // -?Data Structures?
042200131204       //--------------------------------------------------------------
042300131204       //  ?SflRcd     - The entire subfile record to pass to the sort?
042400131204     d SflRcd          ds                  inz
042500131204     d   S1Hfgs                            inz
042600131204     d   S1HdtOins                         inz
042700131204     d   S1Htor                            inz
042800131204     d   S1Hogg                            inz
042900140110     d   S1Hcmd                            inz
043000131204     d   S01opz                            inz
043100140110     d   S01tor                            inz
043200131204     d   S01cmd                            inz
043300131204     d   S01var                            inz
043400131204     d   S01rsc                            inz
043500131206     d   S01hmi                            inz
043600131204     d   S01ogg                            inz
043700131204     d   Selected                     1a   inz
043800131204       //  ?QLGSCB     - The sort request block for the QLGSORT API?
043900131204      /copy QSYSINC/QRPGLESRC,QLGSORT
044000131204       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
044100131204     d QLGKL                         16    dim(MaxKey)
044200131204     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
044300131204     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
044400131204     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
044500131204     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
044600131204       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
044700131204      /copy QSYSINC/QRPGLESRC,QLGSRTIO
044800131204       //  ?QUSEC      - Error structure for the QLGSORT API?
044900131204      /copy QSYSINC/QRPGLESRC,QUSEC
045000131204
045100131204       //--------------------------------------------------------------
045200131204       // -?Standalone fields?
045300131204       //--------------------------------------------------------------
045400131204     d NotUsed         s             16a   inz
045500131204       //  ?ReturnSize - Size of list returned by sort API?
045600131204     d ReturnSize      s              9b 0 inz
045700131204       //  ?SizeList   - Size of list?
045800131204     d SizeList        s              9b 0 inz
045900131204       //  ?Nrr        - Relative record number for subfile?
046000131204     d Nrr             s              5i 0 inz
046100131204     d RrnLast         s              5i 0 inz
046200131030
046300131030       //--------------------------------------------------------------
046400131030       //?Definizione prototipi procedure.                             ?
046500131030       //--------------------------------------------------------------
046600131030
046700131030       // -?Reperimento dati utente?
046800131030     d TIBS34ds      e ds                  inz
046900131030      /copy gaitrasrc/srcProtoPR,TIBS34R
047000131030
047100131120       // -?Reperimento dati tabelle?
047200131120      /copy gaitrasrc/srcProtoPI,TRULTAB
047300131120      /copy gaitrasrc/srcProtoPR,TRULTAB
047400131120
047500131120       // -?Controllo/Decodifica cliente?
047600131120      /copy gaitrasrc/srcProtoPI,TIBS69R
047700131120      /copy gaitrasrc/srcProtoPR,TIBS69R
047800131121
047900131121       // -?Interrogazione Bolle Arrivi?
048000131121      /copy gaitrasrc/srcProtoPI,FNLR36R
048100131121      /copy gaitrasrc/srcProtoPR,FNLR36R
048200131128
048300131128       // -?Interrogazione Particolarità?
048400131128     d DS7PQRS       e ds                  inz   qualified
048500131128     d   DS7ric      e                     inz('S')
048600131128     d   DS7ges      e                     inz('V')
048700131128       // -?       "             "       Giacenza?
048800131128     d trtb69r         pr                  extpgm('TRTB69R')
048900131128     d   kpjba                             likeds(KPJBA)
049000131128       // -?       "             "       Consegna?
049100131128     d trtb70r         pr                  extpgm('TRTB70R')
049200131128     d   kpjba                             likeds(KPJBA)
049300131128       // -?       "             "       Varie?
049400131128     d trtb73r         pr                  extpgm('TRTB73R')
049500131128     d   kpjba                             likeds(KPJBA)
049600131122
049700131122       // -?Interrogazione O.R.M.?
049800131122     d Parm01          ds                  inz   qualified
049900131125     d   ppoe                         3  0 inz
050000131125     d   pnor                         7  0 inz
050100131125     d   pnsr                         2  0 inz
050200131125     d   pnrv                         2  0 inz
050300131125     d   psce                         1    inz
050400131125     d   pfgs                         3  0 inz
050500131125     d   ppor                         3  0 inz
050600131125     d   pdtr                         8  0 inz
050700131125     d   pmdb                        10    inz
050800131125     d   pprb                        10    inz
050900131125     d   pdts                         8  0 inz
051000131125     d   prmp                         1    inz
051100131125     d   pbrc                         1    inz
051200131125     d   pref                         2    inz
051300131125     d   pmio                         1    inz
051400131122     d FIDNA1ds      e ds                  inz   qualified
051500131122     d fior07r         pr                  extpgm('FIOR07R')
051600131122     d   kpjba                             likeds(KPJBA)
051700131122     d   fidnA1ds                          likeds(FIDNA1ds)
051800140130
051900140130       // -?Monitor attività autotrasportatore con PDA?
052000140130     d FNLVP0ds      e ds                  inz
052100140130      /copy gaitrasrc/srcProtoPR,FNLVP0R2
052200140130
052300140130       // -?Dettaglio Interrogazione DISTINTE?
052400140130     d FIDG311ds     e ds                  inz
052500140130      /copy gaitrasrc/srcProtoPR,FIDG311R
052600131205
052700131205       // -?Scrittura Richiesta Assistenza?
052800131205     d FIDNA3ds      e ds                  inz
052900131205     d fidnA3r         pr                  extpgm('FIDNA3R')
053000131205     d   kpjba                             likeds(KPJBA)
053100131205     d   fidnA3ds                          likeds(FIDNA3ds)
053200131204
053300131204       // -?Ordinamento subfile?
053400131204      /copy gaitrasrc/srcProtoPR,QLGSRTIO
053500131030
053600131030       //--------------------------------------------------------------
053700131030       //?Definizione key-list.                                        ?
053800131030       //--------------------------------------------------------------
053900131030
054000131120       // -?File FICAU01L?
054100131120     d keyFICAU01    e ds                  extname(FICAU01L : *key)
054200131126     d                                     prefix(k_)    inz
054300140206       // -?File FICAU02L?
054400140206     d keyFICAU02    e ds                  extname(FICAU02L : *key)
054500140206     d                                     prefix(k_CA2 : 3)   inz
054600131120
054700131120       // -?File FIAPD01L?
054800131120     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
054900131126     d                                     prefix(k_)    inz
055000131121
055100131121       // -?File FIAPD41L?
055200140131     d keyFIAPD41    e ds                  extname(FIAPD41L : *key)
055300140131     d                                     prefix(k_)    inz
055400131030
055500131030       // -?File FNARB01L?
055600131030     d keyFNARB01    e ds                  extname(FNARB01L : *key)
055700131126     d                                     prefix(k_)    inz
055800131030
055900131030       // -?File FIDST01L?
056000131204     d keyFIDST01    e ds                  extname(FIDST01L : *key)
056100131204     d                                     prefix(k_)    inz
056200131126
056300131126       // -?File FIARN12L?
056400131126     d keyFIARN12    e ds                  extname(FIARN12L : *key)
056500131126     d                                     prefix(k_)    inz
056600131030
056700131122       // -?File FIAR401L?
056800131122     d keyFIAR401    e ds                  extname(FIAR401L : *key)
056900131126     d                                     prefix(k_)    inz
057000131210       // -?File FIAR404L?
057100131210     d keyFIAR404    e ds                  extname(FIAR404L : *key)
057200131210     d                                     prefix(k_AR44:3)    inz
057300131122
057400131122       // -?File FIAR501L?
057500131122     d keyFIAR501    e ds                  extname(FIAR501L : *key)
057600131126     d                                     prefix(k_)    inz
057700131122
057800131122       // -?File FIAR601L?
057900131122     d keyFIAR601    e ds                  extname(FIAR601L : *key)
058000131126     d                                     prefix(k_)    inz
058100131122
058200131122       // -?File FIAR901L?
058300131122     d keyFIAR901    e ds                  extname(FIAR901L : *key)
058400131126     d                                     prefix(k_)    inz
058500131120
058600131120       // -?File FNORM01L?
058700131122     d keyFNORM01    e ds                  extname(FNORM01L : *key)
058800131126     d                                     prefix(k_)    inz
058900140121
059000140121       // -?File FNORT01L?
059100140121     d keyFNORT01    e ds                  extname(FNORT01L : *key)
059200140121     d                                     prefix(k_)    inz
059300140121
059400140121       // -?File FNORG01L?
059500140121     d keyFNORG01    e ds                  extname(FNORG01L : *key)
059600140121     d                                     prefix(k_)    inz
059700131211
059800131211       // -?File FNORE01L?
059900131211     d keyFNORE01    e ds                  extname(FNORE01L : *key)
060000131211     d                                     prefix(k_)    inz
060100131030
060200131030       //--------------------------------------------------------------
060300131030       //?Riepilogo indicatori utilizzati.                             ?
060400131030       //--------------------------------------------------------------
060500131030
060600131030
060700131030       //--------------------------------------------------------------
060800131030       //?M A I N - L I N E                                            ?
060900131030       //--------------------------------------------------------------
061000131030
061100131030     c     *Entry        plist
061200131030     c                   parm                    KPJBA
061300131030
061400131030      /free
061500131030
061600131030       // -?Operazioni iniziali?
061700131030       exsr sr_RoutInz;
061800131030
061900131030       // -?Ciclo di gestione del file video?
062000131030       DoW  $Fine = *off;
062100131030
062200131030         select;
062300131030
062400131120           // -?Gestione subfile S01?
062500131030           when $Video = 'S1';
062600131030             exsr sr_GesS01;
062700131120
062800131120           // -?Gestione videata DS1 (singola spedizione)?
062900140207           //  ?- gestita all'interno della subr. sr_GesS01 -?
063000140207           //when  $Video = 'DS';
063100140207           //  exsr sr_GesD01;
063200131120
063300131120           // -?Gestione videata DO1 (singolo O.R.M.)?
063400140207           //  ?- gestita all'interno della subr. sr_GesS01 -?
063500140207           //when  $Video = 'DO';
063600140207           //  exsr sr_GesD01;
063700131030
063800131030           // -?? ? ??
063900131030           other;
064000131030             $Fine = *on;
064100131030
064200131030         endsl;
064300131030
064400131030       EndDo;
064500131030
064600131030       // -?Operazioni finali?
064700131030       exsr sr_RoutEnd;
064800131030
064900131030       //--------------------------------------------------------------
065000131030       //?Operazioni iniziali.                                         ?
065100131030       //--------------------------------------------------------------
065200131030       BEGSR  sr_RoutInz;
065300131030
065400131030         // -?Impostazione chiusura?
065500131030         *inLR = *on;
065600131121
065700131121         // -?Ricezione parametri ed impostazione parametri di output?
065800131121         FNLRX2ds = KPJBU;
065900131122         FNLRX2ds.oX2upd = *off;
066000131125         clear  FNLRX2ds.oX2fxx;
066100131122         FNLRX2ds.oX2err = *off;
066200131125         clear  FNLRX2ds.oX2msg;
066300131030
066400131030         // -?Reperimento dati job?
066500131030         exsr  sr_DatiJob;
066600131104
066700131104         // -?Impostazione nome programma a video?
066800131104         V1Tpgm = SDSpgmName;
066900131030
067000131030         // -?Pulizia / impostazione iniziale dei campi chiave?
067100131120         clear  keyFICAU01;
067200140206         clear  keyFICAU02;
067300131120         clear  keyFIAPD01;
067400140131         clear  keyFIAPD41;
067500131104         clear  keyFNARB01;
067600131125         clear  keyFIAR501;
067700131122         clear  keyFIAR401;
067800131126         clear  keyFIARN12;
067900131204         clear  keyFIDST01;
068000131122         clear  keyFNORM01;
068100131211         clear  keyFNORE01;
068200131210         clear  keyFIAR404;
068300131121         k_APDtip   = 'A';
068400140131         k_APD4tipA = 'A';
068500140131         k_APD4trd  = 'TEL';
068600131121         k_AR5trd   = 'GEN';
068700131126         k_ARNcdn   = 'LDV';
068800131126         k_ARNgst   = 'S';
068900131204         k_DSTnpg   = 4;
069000131211         k_OREtrc   = 'O ';
069100131125
069200131125         // -?Caricamento schiera tab. "TCV" = Tipologiche Causali Variazioni bolle?
069300131125         setll  ('TCV')  TNTBE000;
069400131125         reade  ('TCV')  TNTBE000;
069500131125         DoW  Not  %eof(TNTBE01L);
069600131125           xx += 1;
069700131125           sk_TCV(xx)  = TBEke1;
069800131125           sk_TCVd(xx) = TBEuni;
069900131125           reade  ('TCV')  TNTBE000;
070000131125         EndDo;
070100131030
070200131030       ENDSR;
070300131030
070400131030       //--------------------------------------------------------------
070500131030       //?Reperimento Dati del job (Utente/Operativi).                 ?
070600131030       //--------------------------------------------------------------
070700131030       BEGSR  sr_DatiJob;
070800131030
070900131030         in(e) §AzUte;
071000131030         if NOT %error;
071100131030           in(e) §DatiUte;
071200131030         endif;
071300131030         if %error or RSut = *blank;
071400131030           tibs34r ( tibs34ds );
071500131030           in §AzUte;
071600131030           in §DatiUte;
071700131030         endif;
071800131030
071900131030       ENDSR;
072000131030
072100131030       //--------------------------------------------------------------
072200131030       //?Gestione subfile S01.                                        ?
072300131030       //--------------------------------------------------------------
072400131030       BEGSR  sr_GesS01;
072500131030
072600131030         // -?Inizializzazione videata?
072700131030         if  $InzS01 = *on;
072800131030           exsr  sr_InzS01;
072900131030           $InzS01 = *off;
073000131030         endif;
073100140131
073200140131         // -?(Dis)Abilitazione Tasti Funzionali?
073300140131         //  E' meglio lasciarli qui, perchè F1 ri-(dis)abilitato?
073400140207         //  nella subr. "sr_GesD01": se viene (dis)abilitato là, qui?
073500140131         //  non verrebbe più re-impostato.?
073600140131         F1Attivo  = (S01nrr > 1);
073700140131         F9Attivo  = (S01nrr > *zero);
073800140131         F11Attivo = (S01nrr > 1);
073900131030
074000131030         // -?Emissione Testata & Piede?
074100131120         write  LRX2T01;
074200131120         write  LRX2P01;
074300131030
074400131030         // -?Posizionamento cursore?
074500131030         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
074600131030         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
074700131030         //  ?si visualizza la pagina che vedeva l'utente quando ha?
074800131030         //  ?premuto l'ultimo tasto.?
074900131030         if  C1CsrRrn > *zero;
075000131030           C1RcdNbr = C1CsrRrn;
075100131030         else;
075200131030           C1RcdNbr = 1;
075300131120           write  LRX2S00;             //?(no rec.)?
075400131030         endif;
075500131030
075600131030         // -?Emissione videata?
075700131120         exfmt  LRX2C01;
075800131030
075900131030         reset  ErrGenerico;
076000131030         reset  ErrMessage;
076100131030         clear  V1Dmsg;
076200131030
076300131030         Select;
076400131204
076500131204           // -?F1=Selezione Totale?
076600131204           When  dsp_aid = c_F01;
076700131204             exsr  sr_F01S01;
076800131030
076900131030           // -?F3=Fine?
077000131030           When  dsp_aid = c_F03;
077100131122             FNLRX2ds.oX2fxx = '03';
077200131030             $Fine = *on;
077300140131
077400140131           // -?F4=Distinta?
077500140131           When  dsp_aid = c_F04;
077600140131             exsr  sr_F04S01;
077700131030
077800131030           // -?F5=Rivisualizza?
077900131030           When  dsp_aid = c_F05;
078000131030             $InzS01 = *on;
078100131205
078200131205           // -?F11=Ordinamento Sfl?
078300131205           When  dsp_aid = c_F11;
078400131205             exsr  sr_F11S01;
078500131104
078600131104           // -?F12=Ritorno?
078700131104           When  dsp_aid = c_F12;
078800131122             FNLRX2ds.oX2fxx = '12';
078900131104             $Video = 'D1';
079000131104
079100131104           // -?Roll-Up?
079200131120           When  dsp_aid = c_RollUp;
079300131212             if  SavMAXnrr >= c_MaxRec  and  Not  $EoF;
079400131104               ErrGenerico = *on;
079500131104               ErrMessage  = *on;
079600131120               V1Dmsg = sk_Msg(01);
079700131204             endif;
079800131030
079900131030           // -?SubFile vuoto?
080000131212           When  SavMAXnrr = *zero;
080100131030
080200131030           // -?Invio?
080300131030           Other;
080400131030             // -?Gestione opzioni?
080500131030             exsr  sr_OpzS01;
080600131030             if  ErrGenerico;
080700131030               leavesr;
080800131030             endif;
080900131030
081000131204         EndSl;
081100131030
081200131030       ENDSR;
081300131030
081400131030       //--------------------------------------------------------------
081500131030       //?Inizializzazione subfile S01.                                ?
081600131030       //--------------------------------------------------------------
081700131030       BEGSR  sr_InzS01;
081800131030
081900131030         // -?Spegnimento degli indicatori di errore?
082000131030         %subst(IndDspF : 50) = *off;
082100131125
082200131125         // -?Decodifica autotrasportatore?
082300131205         k_APDpdr = FNLRX2ds.iX2aut;
082400131125         chain  %kds( keyFIAPD01 )  FIAPD000;
082500131125         if  Not %found(FIAPD01L)  or  APDatb <> *blank;
082600131125           clear  APDrsc;
082700131125         endif;
082800131125
082900140131         // -?Reperimento suo n° telefonico?
083000140131         k_APD4pdr = FNLRX2ds.iX2aut;
083100140131         chain  %kds( keyFIAPD41 )  FIAPD400;
083200140131         if  %found(FIAPD41L)  and  APD4atb = *blank;
083300140131           dAPD4tel = APD4uni;
083400140131         endif;
083500131205
083600131205         // -?Reperimento dati della Distinta Consegna?
083700131205         k_DSTfgs = FNLRX2ds.iX2fgs;
083800131205         k_DSTnfv = FNLRX2ds.iX2dst;
083900131205         chain  %kds( keyFIDST01 )  FIDST000;
084000131205         if  Not  %found(FIDST01L);
084100131205           clear  DSTpda;
084200140130           clear  DSTflr;
084300131205         endif;
084400140130
084500140130         dDSTflr = DSTflr;
084600131125
084700131125         // -?Impostazione campi di "testata"?
084800131205         C1Cpdr   = FNLRX2ds.iX2aut;
084900131125         C1Dpdr   = APDrsc;
085000140131         C1TelPdr = dAPD4tel.§APD4tel;
085100140131         //select;
085200140131         //  when  DSTpda = 'O';
085300140131         //    C1Dpda = DSTpda + '=O.R.M.   ';
085400140131         //  when  DSTpda = 'C';
085500140131         //    C1Dpda = DSTpda + '=Consegne ';
085600140131         //  when  DSTpda = 'E';
085700140131         //    C1Dpda = DSTpda + '=ORM+Cons.';
085800140131         //  when  DSTpda = 'N';
085900140131         //    C1Dpda = DSTpda + '=NO       ';
086000140131         //endsl;
086100140131         if  DSTpda = 'N';
086200140131           C1Dpda = 'NO';
086300140131         else;
086400140131           C1Dpda = DSTpda;
086500140131         endif;
086600140131         //select;
086700140131         //  when  dDSTflr.§DSTtstPda = *blank;
086800140131         //  when  dDSTflr.§DSTtstPda = 'O';
086900140131         //    C1Dtst = 'Test O.R.M.   ';
087000140131         //  when  dDSTflr.§DSTtstPda = 'C';
087100140131         //    C1Dtst = 'Test Consegne ';
087200140131         //  when  dDSTflr.§DSTtstPda = 'E';
087300140131         //    C1Dtst = 'Test ORM+Cons.';
087400140131         //endsl;
087500140203         C1Dtst = %xlate( c_Up : c_Lo : dDSTflr.§DSTtstPda );
087600131030
087700131030         // -?Pulizia subfile?
087800131030         SflDsp_N    = *on;
087900131030         SflDspCtl_N = *on;
088000131120         write  LRX2C01;
088100131030         SflDspCtl_N = *off;
088200131030         SflEnd      = *off;
088300131030
088400131104         clear  w_SflPag;
088500131030         clear  C1RcdNbr;
088600131030         clear  C1CsrRrn;
088700131212         clear  SavMAXnrr;
088800131030         clear  S01nrr;
088900131030         clear  V1Dmsg;
089000131030         ErrMessage  = *off;
089100131030         ErrGenerico = *off;
089200131030
089300131104         // -?Caricamento 1ª pagina dei dati nel subfile?
089400131121         clear  keyFICAU01;
089500131121         k_CAUfgs = FNLRX2ds.iX2fgs;
089600131205         k_CAUpdr = FNLRX2ds.iX2aut;
089700131205         //k_CAUnfv = FNLRX2ds.iX2dst;       ?(NON in chiave)?
089800131121         setll  %kds(keyFICAU01 : 2)  FICAU000;
089900131126         exsr  sr_ReadFICAU;
090000131204         exsr  sr_CaricaS01;
090100131030
090200131030         // -?Impaginazione subfile?
090300131030         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
090400131212         if S01nrr > *zero;
090500131030           C1RcdNbr  = 1;
090600131030           C1CsrRrn  = 1;
090700131030         else;
090800131104           clear  C1RcdNbr;
090900131030         endif;
091000131204
091100131204         // -?Ordinamento subfile?
091200131212         if  S01nrr > *zero;
091300131204           exsr  sr_SortSfl;
091400131204         endif;
091500131204
091600131204         // -?Eventuale emissione *msg di superamento limite max di rec.?
091700131204         //  ?visualizzabili nel subfile?
091800131212         if  S01nrr >= c_MaxRec  and  Not $EoF;
091900131204           ErrGenerico = *on;
092000131204           ErrMessage  = *on;
092100131204           V1Dmsg = sk_Msg(01);
092200131204         endif;
092300131030
092400131030       ENDSR;
092500131030
092600131030       //--------------------------------------------------------------
092700131204       //?Caricamento completo del subfile S01                         ?
092800131030       //--------------------------------------------------------------
092900131204       BEGSR  sr_CaricaS01;
093000131104
093100131204         clear  S01nrr;
093200131104         SflNxtChg = *off;
093300131030
093400131204         // -?Ciclo di caricamento completo di dati nel subfile?
093500131204         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
093600131204         //  ?l'eventuale ordinamento)?
093700131204         DoW  S01nrr < c_MaxRec  and  Not $EoF;
093800131122
093900131126           clear  LRX2S01;
094000131126
094100131126           // ·?Reperimento dati Comando da impartire al PDA o all'AUT?
094200131126           If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
094300131127             clear  dCMD;
094400131126             if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
094500131126                              *omit : *omit :
094600131126                              *omit : *omit :
094700131126                              *omit : *omit : *omit : *omit :
094800131126                              *omit : *omit : *omit : *omit :
094900131126                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
095000131126                              = *zero      AND
095100131126                   ds_TNTBE.TBEatb = *blank;
095200131127               dCMD = ds_TNTBE.TBEuni;
095300131126             else;
095400131204               §CMDdes  = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ';
095500131126             endif;
095600131126           Else;
095700131204             §CMDdes  = CAUdesCmd;
095800131126           EndIf;
095900131126
096000131126           // ·?Reperimento dati Tipologiche Causali Variazioni bolle?
096100140131           If  CAUcmd <> c_Cmd_Inserito;
096200140131             If  CAUdesVa = *blank;
096300140131               wTipTVA = 'B';
096400140213               ds_TVA  = CAUtva;
096500140131               exsr  sr_GetDesTCV;
096600140131               S01var  = wDesTVA;
096700140131             Else;
096800140131               S01var  = CAUdesVa;
096900140131             EndIf;
097000140131           EndIf;
097100131126
097200131204
097300131205           Select;
097400131204             // ·?Reperimento dati Spedizione?
097500131205             When  CAUtor = c_Consegna;
097600131126               exsr  sr_GetSped;
097700131127               //S01rsc = ARBrsd + AR4not;
097800131127               S01rsc = ARBrsd;
097900131204             // ·?Reperimento dati O.R.M.?
098000131205             When  CAUtor = c_Ritiro;
098100131126               exsr  sr_GetORM;
098200131126               S01rsc = ORMrsr;
098300131205           EndSl;
098400131204
098500131204           // ·?Campi hidden?
098600131204           S1Hfgs    = CAUfgs;
098700131204           S1HdtOins = CAUdtOins;
098800131204           S1Htor    = CAUtor;
098900131204           S1Hogg    = CAUogg;
099000140206           //S1Hcmd    = §CMDdes;
099100140206           S1Hcmd    = CAUdesCmd;
099200131204           // ·?Campi di Output?
099300131204           S01tor    = CAUtor;
099400140206           //S01cmd    = §CMDdes;
099500140206           S01cmd    = CAUdesCmd;
099600131205           //S01var    = CAUdesVa;    ?(già "impostato" - o costruito)?
099700131206           S01hmi    = %int( %subst( CAUdtOins : 9 : 4 ) );
099800131204           select;
099900131204             when  CAUtor = c_Consegna;
100000131204               S01ogg = 'Spedizione: ' + %editc( ds_Spedizione.wLNP : 'X' ) +
100100131204                                   ' ' + %editc( ds_Spedizione.wNRS : 'Z' ) +
100200131204                                   ' ' + %editc( ds_Spedizione.wNSP : '3' ) +
100300131204                                  '  ' + %editc( ds_Spedizione.wAAS : 'Z' );
100400131204             when  CAUtor = c_Ritiro;
100500131204               S01ogg = 'O.R.M.: ' + %editc( ds_ORM.wPOE : 'X' ) +
100600131204                               ' ' + %editc( ds_ORM.wNSR : 'Z' ) +
100700131204                               ' ' + %editc( ds_ORM.wNOR : '3' ) +
100800131204                               ' ' + %editc( ds_ORM.wNRV : 'Z' );
100900131204           endsl;
101000131126
101100131126           // -?Scrittura record nel subfile?
101200131206           Ord_x_OraIns = (wOrdSfl = 1);
101300131126           S01nrr += 1;
101400131126           write  LRX2S01;
101500131030
101600131104           // -?Lettura record successivo?
101700131126           exsr  sr_ReadFICAU;
101800131030
101900131030         EndDo;
102000131204
102100131030
102200131212         // -?Memorizzazione n° rec. caricati nel subfile?
102300131212         //  ?(servirà all'ordinamento)?
102400131212         SavMAXnrr = S01nrr;
102500131212
102600131030         // -?Visualizzazione del SFL (se ci sono dati)?
102700131030         SflDsp_N = (S01nrr <= *zero);
102800131030
102900131030         // -?Attivazione del SFLEND?
103000131030         SflEnd = ( $EoF );
103100131030
103200131204         // -?Posizionamento cursore al 1° rcd del subfile?
103300131030         if  S01nrr > *zero;
103400131204           C1RcdNbr = 1;
103500131030         else;
103600131030           clear  C1RcdNbr;
103700131030         endif;
103800131030
103900131030         C1CsrRrn = C1RcdNbr;
104000131030
104100131030       ENDSR;
104200131204
104300131204       //--------------------------------------------------------------
104400131204       //?Gestione tasto funzionale F1 da videata C01 / S01            ?
104500131204       //?F1-Selezione Totale                                          ?
104600131204       //--------------------------------------------------------------
104700131204       BEGSR  sr_F01S01;
104800131204
104900131212         RrnLast  = SavMAXnrr;
105000131204
105100131204         For  S01nrr = 1  To  RrnLast;
105200131204
105300131204           chain  S01nrr  LRX2S01;
105400131204           if  Not %found(FNLRX2D);
105500131204             iter;
105600131204           endif;
105700131204
105800131204           S01opz = 1;
105900131204           SflNxtChg = *on;
106000131206           Ord_x_OraIns = (wOrdSfl = 1);
106100131204           update  LRX2S01;
106200131204
106300131204         EndFor;
106400131204
106500131204       ENDSR;
106600140131
106700140131       //--------------------------------------------------------------
106800140131       //?Gestione tasto funzionale F4 da videata C01 / S01            ?
106900140131       //?F4-Spedizioni nella distinta                                ?
107000140131       //--------------------------------------------------------------
107100140131       BEGSR  sr_F04S01;
107200140131
107300140131         // -?Visualizzazione fasi della distinta se PDA?
107400140131         //  ?(questo *pgm dovrebbe "durare" fino ad agosto 2014...)?
107500140131         If  DSTpda = 'E'  and  dDSTflr.§DSTtstPda = *blank;
107600140131
107700140131           clear  KPJBU;
107800140131           clear  FNLVP0ds;
107900140131           LVP0fgs = FNLRX2ds.iX2fgs;
108000140131           LVP0ndc = FNLRX2ds.iX2dst;
108100140131           //LVP0aut = FNLRX2ds.iX2daut;
108200140131           LVP0aut = DSTpdr;
108300140131           FNLVP0R2( KPJBA : FNLVP0ds );
108400140131
108500140131         // -?Visualizzazione Dettaglio Distinta?
108600140131         Else;
108700140131
108800140131           clear  FIDG311ds;
108900140131           F31fgs     = FNLRX2ds.iX2fgs;
109000140131           F31npg     = DSTnpg;
109100140131           F31numDis  = FNLRX2ds.iX2dst;
109200140131           F31DatDis  = DSTdfv;
109300140131           //F31DatiVis = *blank;
109400140131           F31priDati = c_Consegna;
109500140131
109600140131           kpjbu = FIDG311ds;
109700140131           FIDG311R ( KPJBA );
109800140131
109900140131           FIDG311ds = KPJBU;
110000140131           if  F31tFunz = '03';
110100140131             $Fine = *on;
110200140131             FNLRX2ds.oX2fxx = F31tFunz;
110300140131           endif;
110400140131
110500140131         EndIf;
110600140131
110700140131       ENDSR;
110800131204
110900131204       //--------------------------------------------------------------
111000131205       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
111100131205       //?F11-Cambia ordinamento sfl                                   ?
111200131204       //--------------------------------------------------------------
111300131205       BEGSR  sr_F11S01;
111400131204
111500131204         if  wOrdSfl < 1;
111600131204           wOrdSfl += 1;
111700131204         else;
111800131204           clear  wOrdSfl;
111900131204         endif;
112000131204
112100131204         // -?Se subfile vuoto: nessun record da ordinare?
112200131204         //  ?Altrimenti: basta riordinarlo?
112300131212         if  SavMAXnrr > *zero;
112400131204           exsr  sr_SortSfl;
112500131204         endif;
112600131204
112700131204       ENDSR;
112800131204
112900131204       //--------------------------------------------------------------
113000131204       //?Ordinamento subfile                                          ?
113100131204       //?  This subroutine sorts the subfile records.                 ?
113200131204       //--------------------------------------------------------------
113300131204       BEGSR  sr_SortSfl;
113400131204
113500131212         RrnLast  = SavMAXnrr;
113600131204
113700131204         C1RcdNbr = 1;
113800131204         clear  C1CsrRrn;
113900131204         clear  S01nrr;
114000131204         clear  V1Dmsg;
114100131204         ErrMessage  = *off;
114200131204         SflNxtChg   = *off;
114300131204         ErrGenerico = *off;
114400131204         %subst(IndDspF : 50) = *off;
114500131204
114600131204         //?___________________________________________________________?
114700131204         //?Initialize the key fields to sort on.?
114800131204         //?There is one extra field not in the subfile -- Selected --?
114900131204         //?that is added to the record. This field is used to place?
115000131204         //?selected records in front of nonselected records.?
115100131204         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
115200131204
115300131204         clear  QLGKL;
115400131204         clear  QLGSKL;
115500131204         clear  QLGSCB;
115600131204         clear  QLGSCB00;
115700131204
115800131204         // -?Gestione della scelta ordinamento:?
115900131204         Select;
116000131204
116100131204           //  ?Ordinamento per Comando (desc)?
116200131204           When  wOrdSfl = *zero;
116300131204             // -?2 campi chiave?
116400131204             QLGNBRK  = 2;
116500131204             // -?1° campo: Descrizione Comando (S1Hcmd)?
116600131204             //            ?a posizone   39, 20 byte, char, descending.?
116700131204             QLGSP    = 39;
116800131204             QLGSS    = %size(S1Hcmd);
116900131204             QLGDT    = Carattere;
117000131204             QLGSO    = Discendente;
117100131204             QLGKL(1) = QLGSKL;
117200131204             // -?2° campo: Data Inserimento (S1HdtOins)?
117300131204             //            ?a posizone    4, 14 byte, char, ascending.?
117400131204             QLGSP    = 4;
117500131204             QLGSS    = %size(S1HdtOins);
117600131204             QLGDT    = Carattere;
117700131204             QLGSO    = Ascendente;
117800131204             QLGKL(2) = QLGSKL;
117900131204
118000131205           //  ?Ordinamento per Data/Ora Inserimento?
118100131204           When  wOrdSfl = 1;
118200131204             // -?1 campo chiave?
118300131204             QLGNBRK  = 1;
118400131204             // -?1° campo: Data Inserimento (S1HdtOins)?
118500131204             //            ?a posizone    4, 14 byte, char, ascending.?
118600131204             QLGSP    = 4;
118700131204             QLGSS    = %size(S1HdtOins);
118800131204             QLGDT    = Carattere;
118900131204             QLGSO    = Ascendente;
119000131204             QLGKL(1) = QLGSKL;
119100131204
119200131204         EndSl;
119300131204
119400131204         // -?Load other sort parameters.?
119500131204         QLGLB   = 80 + 16 * MaxKey;
119600131204         QLGRL   = %size(SflRcd) - 1;
119700131204         QLGRT   = 8;
119800131204         QLGOKL  = 80;
119900131204         QLGLKE  = 16;
120000131204         QLGLSS  = 290;
120100131204         // -?Initialize Sort I/O API fields.?
120200131204         QLGRL00 = QLGRL;
120300131204         QLGRC00 = 1;
120400131204         clear  QUSEI;
120500131204         QUSBPRV = %size(QUSEC);
120600131204
120700131204         // -?First step - Initialize the sort routine.?
120800131204         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
120900131204                      SizeList : ReturnSize : QUSEC );
121000131204
121100131204         // -?Next step - Write records to I/O routine.?
121200131204         QLGRT00  = Put;
121300131204         For  Nrr = 1  To  RrnLast;
121400131204           chain  Nrr  LRX2S01;
121500131204           // -?Solo le righe con Selected = 'Y' sono riordinate,?
121600131204           //  ?quindi per fare un ordinamento di tutte le righe?
121700131204           //  ?metto 'Y' sempre.?
121800131204           Selected = 'Y';
121900131204           clear  QUSEI;
122000131204           QUSBPRV  = %size(QUSEC);
122100131204           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
122200131204                         SizeList : NotUsed : QUSEC );
122300131204         EndFor;
122400131204
122500131204         // -?Next step - Signal end of input, clear subfile for reload.?
122600131204         QLGRT00 = EndPut;
122700131204         clear  QUSEI;
122800131204         QUSBPRV = %size(QUSEC);
122900131204         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
123000131204                       SizeList : NotUsed : QUSEC );
123100131204
123200131204         // -?Pulizia subfile?
123300131204         SflDsp_N    = *on;
123400131204         SflDspCtl_N = *on;
123500131204         write  LRX2C01;
123600131204         SflDspCtl_N = *off;
123700131204         SflEnd      = *off;
123800131204
123900131204         // -?Final step - Write the records back to the subfile.?
124000131204         QLGRT00  = Get;
124100131204         For  Nrr = 1  To RrnLast;
124200131204           clear  QUSEI;
124300131204           QUSBPRV = %size(QUSEC);
124400131204           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
124500131204                         QLGRL00  : NotUsed : QUSEC );
124600131204           // -?Ripristino indicatori utilizzati nel sfl rec?
124700131206           Ord_x_OraIns = (wOrdSfl = 1);
124800131204           SflNxtChg  = (S01opz <> *zero);
124900131204           S01nrr = Nrr;
125000131204           write  LRX2S01;
125100131204         EndFor;
125200131204
125300131204         C1CsrRrn = 1;
125400131204
125500131204         // -?Visualizzazione del SFL (se ci sono dati)?
125600131204         SflDsp_N = (S01nrr <= *zero);
125700131204
125800131204         // -?Attivazione del SFLEND?
125900131204         SflEnd = $EoF;
126000131204
126100131205         // -?Impostazione F11 nel piede del subfile?
126200131204         select;
126300131204           when  S01nrr  = *zero;
126400131205             clear  V1PF11;
126500131204           when  wOrdSfl = 0;
126600131205             V1PF11 = 'F11=Ord.OraIns';
126700131204           when  wOrdSfl = 1;
126800131205             V1PF11 = 'F11=Ord. x Cmd';
126900131204         endsl;
127000131204
127100131204       ENDSR;
127200131127
127300131127       //--------------------------------------------------------------
127400131127       //?Gestione opzioni del subfile S01.                            ?
127500131127       //--------------------------------------------------------------
127600131127       BEGSR  sr_OpzS01;
127700131127
127800131127         clear  SavS01csr;
127900131127
128000131127         // -?Ciclo di lettura subfile?
128100131127         readc  LRX2S01;
128200131127
128300131129         DoW  Not %eof(FNLRX2D);
128400131127
128500131127           SflNxtChg = *off;
128600131127           %subst(IndDspF : 50) = *off;
128700131127           C1RcdNbr = S01nrr;
128800131127
128900131127           select;
129000131127
129100131127             // -?Nessuna opzione?
129200131127             when  S01opz = *zero;
129300131127
129400131127             // -?Opz. 1=Gestione?
129500131127             when  S01opz = 1;
129600131127               select;
129700131127                 when  S1Htor = c_Consegna;
129800131127                   $Video  = 'DS';
129900140203                   $InzD01 = *on;
130000131127                   doW  $Video = 'DS';
130100140207                     exsr  sr_GesD01;
130200131127                   enddo;
130300131127                 when  S1Htor = c_Ritiro;
130400131127                   $Video  = 'DO';
130500140203                   $InzD01 = *on;
130600131127                   doW  $Video = 'DO';
130700140207                     exsr  sr_GesD01;
130800131127                   enddo;
130900131127                 other;
131000131127                   ErrGenerico = *on;
131100131127                   ErrMessage  = *on;
131200131127                   PosCurOPZ   = *on;
131300131127                   V1Dmsg = sk_Msg(02);
131400131127               endsl;
131500131127
131600131127             // -?Opzione errata?
131700131127             other;
131800131127               ErrGenerico = *on;
131900131127               ErrMessage  = *on;
132000131127               PosCurOPZ   = *on;
132100131127               V1Dmsg = sk_Msg(02);
132200131127
132300131127           endsl;
132400131127
132500131127           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
132600131127           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
132700131127           if  S01opz  <> *zero    and
132800131127              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
132900131127             SavS01csr   = C1RcdNbr;
133000131127           endif;
133100131127
133200131127           // -?Aggiornamento sfl?
133300131127           select;
133400131127             when  ErrMessage;
133500131127               SflNxtChg = *on;
133600131127               C1CsrRrn  = C1RcdNbr;
133700131127             when  ErrGenerico;
133800131127               C1CsrRrn  = C1RcdNbr;
133900131127               clear  S01opz;
134000131127             other;
134100131127               clear  S01opz;
134200131127           endsl;
134300131127
134400131206           Ord_x_OraIns = (wOrdSfl = 1);
134500131206
134600131127           UPDATE  LRX2S01;
134700131127
134800131127           if  ErrGenerico   or   ErrMessage;
134900131127             leave;
135000131127           endif;
135100131127
135200131127           readc  LRX2S01;
135300131127
135400131127         ENDDO;
135500131127
135600131127         // -?Riposizionam. cursore sul record contenente la prima opz.?
135700131127         //  ?(se non sono stati rilevati errori)?
135800131127         if  NOT ErrMessage   and   NOT ErrGenerico   and
135900131127             SavS01csr > *zero;
136000131127           C1CsrRrn = SavS01csr;
136100131127         endif;
136200131127
136300131127       ENDSR;
136400131127
136500131127       //--------------------------------------------------------------
136600131127       //?Lettura file FICAU01L e selezione del record.                ?
136700131127       //--------------------------------------------------------------
136800131127       BEGSR  sr_ReadFICAU;
136900131127
137000131205         reade(N)  %kds(keyFICAU01 : 2)  FICAU000;
137100131127         $EoF = ( %eof(FICAU01L) );
137200131127
137300131127         // -?Selezione singolo record?
137400131212         DoW  Not  $EoF;
137500131127
137600131127           exsr  sr_SelezRec;
137700131127
137800131127           if  $RecOK;
137900131127             leave;
138000131127           endif;
138100131127
138200131205           reade(N)  %kds(keyFICAU01 : 2)  FICAU000;
138300131127           $EoF = ( %eof(FICAU01L) );
138400131127
138500131127         EndDo;
138600131127
138700131127       ENDSR;
138800131121
138900131121       //--------------------------------------------------------------
139000131122       //?Selezione del singolo record in input.                       ?
139100131121       //--------------------------------------------------------------
139200131121       BEGSR  sr_SelezRec;
139300131121
139400131121         $RecOK = *off;
139500131122
139600131122         Select;
139700131121
139800131121           // -?Filiale Gestione diversa?
139900131121           //  ?(IMPOSSIBILE perchè 1ª chiave)?
140000131121           //When  CAUfgs <> FNLRX2ds.iX2fgs;
140100131121           //  setll  (*hival)  FICAU000;
140200131121           //  leavesr;
140300131205
140400131205           // -?Autotrasportatore diverso da quello selezionato?
140500131205           //  ?(IMPOSSIBILE perchè 2ª chiave)?
140600131205           //When  CAUpdr <> FNLRX2ds.iX2aut;
140700131205           //  leavesr;
140800131205
140900131205           // -?Distinta Consegna diversa da quella selezionata?
141000140207           //When  FNLRX2ds.iX2dst <> *zero   and
141100140207           //      FNLRX2ds.iX2dst <> CAUnfv  and
141200140207           //      CAUtor          =  c_Consegna;
141300140207           When  FNLRX2ds.iX2dst <> *zero   and
141400140207                 FNLRX2ds.iX2dst <> CAUnfv;
141500131205             leavesr;
141600131121
141700131121           // -?Record Annullato?
141800131121           When  CAUatb <> *blank;
141900131121             leavesr;
142000131121
142100131121           // -?Ammessi Tipi Oggetto "C" (consegna) e "R" (ritiro)?
142200131121           When  CAUtor <> c_Consegna  and  CAUtor <> c_Ritiro;
142300131121             leavesr;
142400131126
142500131126           // -?Solo Telefonate SENZA risposta?
142600131126           When  CAUflt <> 'T';
142700131126             leavesr;
142800131126           When  CAUdtOtel <> *blank  and  CAUdtOtel <> *zero;
142900131126             leavesr;
143000131121
143100131121         EndSl;
143200131121
143300131121         // -?Telefonata da caricare nel subfile?
143400131121         $RecOK = *on;
143500131121
143600131121       ENDSR;
143700131030
143800131030       //--------------------------------------------------------------
143900140207       //?Gestione videata DS1 (spedizione) o DO1 (O.R.M.)             ?
144000131030       //--------------------------------------------------------------
144100140207       BEGSR  sr_GesD01;
144200140207
144300140207         // -?Inizializzazione videata?
144400140207         //  ?(sempre e comunque, per reperire i dati aggiornati)?
144500140207         clear  $RecUpdated;
144600140207         select;
144700140207           when  $Video = 'DS';
144800140207             exsr  sr_InzDS1;
144900140207             exsr  sr_AttrVisDS1;
145000140207           when  $Video = 'DO';
145100140207             exsr  sr_InzDO1;
145200140207             exsr  sr_AttrVisDO1;
145300140207         endsl;
145400140207
145500140207         // -?(Dis)Abilitazione tasti funzionali a video?
145600140207         exsr  sr_AbilFxxD01;
145700140207
145800140207         // -?Verifica dell'oggetto: confronto con i dati precedentemente?
145900140207         //  ?memorizzati  (qualcuno dei quali potrebbe essere stato?
146000140207         //  ?modificato da un altro utente)?
146100140207         if  Not  $InzD01;
146200140207           exsr  sr_Verifica_Ogg;
146300140207         endif;
146400140207
146500140207         $InzD01 = *off;
146600140207
146700140207         // -?Eventuale avviso di dati variati?
146800140207         if  $RecUpdated;
146900140207           ErrGenerico = *on;
147000140207           ErrMessage  = *on;
147100140207           V1Dmsg = sk_Msg(04);
147200140207         endif;
147300131120
147400131120         // -?Emissione videata?
147500131120         write  LRX2T01;
147600140207         select;
147700140207           when  $Video = 'DS';
147800140207             exfmt  LRX2DS1;
147900140207           when  $Video = 'DO';
148000140207             exfmt  LRX2DO1;
148100140207         endsl;
148200131120
148300131120         clear  V1Dmsg;
148400131120         reset  ErrMessage;
148500140203         reset  ErrGenerico;
148600140203
148700140203         // -?Memorizzazione dati della videata per verificarne variazioni?
148800140203         if  dsp_aid <> c_F03  and  dsp_aid <> c_F12;
148900140203           exsr  sr_Memo_Dati;
149000140203         endif;
149100131120
149200131120         SELECT;
149300131127
149400131127           // -?F1=Ultimo Contatto?
149500131127           WHEN  dsp_aid = c_F01;
149600131128             exsr  sr_GesW01;
149700131122
149800140203           // -?F3=Fine?
149900140203           //  ?(ritorno al subfile S01 interrompendone la lettura)?
150000131122           WHEN  dsp_aid = c_F03;
150100140108             //clear  $Video;
150200140108             //FNLRX2ds.oX2fxx = '03';
150300140108             //ErrGenerico = *on;
150400140108             //$Fine = *on;
150500140108             ErrGenerico = *on;
150600140108             $Video = 'S1';
150700140226
150800140226           // -?F5=Altri Dati?
150900140226           WHEN  dsp_aid = c_F05;
151000140226             exsr  sr_GesW02;
151100131120
151200131120           // -?F7=Interrogazione spedizione?
151300140207           WHEN  dsp_aid = c_F07  and  $Video = 'DS';
151400131121             exsr  sr_VisualSped;
151500140207
151600140207           // -?F8=Interrogazione O.R.M.?
151700140207           WHEN  dsp_aid = c_F08  and  $Video = 'DO';
151800140207             exsr  sr_VisualORM;
151900131120
152000131120           // -?F12=Ritorno?
152100140203           //  ?(ritorno al subfile S01 proseguendone la lettura)?
152200131120           WHEN  dsp_aid = c_F12;
152300140108             //ErrGenerico = *on;
152400131120             $Video = 'S1';
152500131120
152600140108           // -?Invio o F6=Accettato?
152700131120           OTHER;
152800140207             select;
152900140207               when  $Video = 'DS';
153000140207                 exsr  sr_CtrDS1;
153100140207               when  $Video = 'DO';
153200140207                 exsr  sr_CtrDO1;
153300140207             endsl;
153400140207             if  ErrGenerico = *on;
153500131120               leavesr;
153600131120             endif;
153700140108             // -?F6=Accettato (Esegue)?
153800140108             if  dsp_aid = c_F06;
153900140121               exsr  sr_F06D01;
154000131205             endif;
154100131120
154200131120         ENDSL;
154300131030
154400131030       ENDSR;
154500140207
154600140207       //--------------------------------------------------------------
154700140207       //?Abilitazione tasti funzionali video DS1 / DO1.               ?
154800140207       //--------------------------------------------------------------
154900140207       BEGSR  sr_AbilFxxD01;
155000140207
155100140207         // -?F1 = Ultimo Contatto?
155200140207         F1Attivo = ($UltContPrec = *on);
155300140207
155400140207       ENDSR;
155500131121
155600131121       //--------------------------------------------------------------
155700131121       //?Inizializazzione videata DS1 (spedizione).                   ?
155800131121       //--------------------------------------------------------------
155900131121       BEGSR  sr_InzDS1;
156000131121
156100131122         ds_Spedizione = S1Hogg;
156200140206
156300140206         // -?Impostazione chiave di accesso per la v.l. 02L?
156400140206         clear  keyFICAU02;
156500140206         k_CA2fgs    = FNLRX2ds.iX2fgs;
156600140206         k_CA2nfv    = FNLRX2ds.iX2dst;
156700140206         k_CA2pdr    = FNLRX2ds.iX2aut;
156800140206         k_CA2tor    = S1Htor;
156900140206         k_CA2ogg    = S1Hogg;
157000140207         k_CA2dtOins = *hival;
157100131121
157200131121         // -?Pulizia videata?
157300131121         clear  LRX2DS1;
157400131128         clear  $UltContPrec;
157500131122
157600131122         // -?Contatto ad Autotrasportatore?
157700140203         clear  sk_TVA;
157800131122         clear  keyFICAU01;
157900140207         //k_CAUfgs    = S1Hfgs;
158000140207         //k_CAUpdr    = C1Cpdr;
158100140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
158200140206         k_CAUpdr    = FNLRX2ds.iX2aut;
158300131122         k_CAUdtOins = S1HdtOins;
158400131122         k_CAUtor    = S1Htor;
158500131125         k_CAUogg    = S1Hogg;
158600140203         chain(N)  %kds( keyFICAU01 )  FICAU000;
158700140203
158800140203         // -?Rec. NON trovato: ricerca con data/ora inserimento più?
158900140203         //  ?recenti (segnalando errore SE non trovato nuovo rec.)?
159000140203         //  ?e riemissione della videata?
159100140203         if  Not %found(FICAU01L)  or  CAUatb <> *blank;
159200140203
159300140203           exsr  sr_Ricarica_Ogg;
159400140203
159500140203           if  Not $RecUpdated;
159600140203             ErrGenerico = *on;
159700140203             ErrMessage  = *on;
159800140203             V1Dmsg = sk_Msg(03);
159900140203             leavesr;
160000140203           endif;
160100140203
160200140203         endif;
160300131121
160400131121         // -?Decodifica autotrasportatore?
160500140203         exsr  sr_GetAUT;
160600131122
160700131122         // -?Comando da impartire al PDA o all'AUT?
160800140203         exsr  sr_GetCMD;
160900131122
161000131122         // -?Tipologica Causale Variazione bolla?
161100140131         //  ?(SE NON "INSERITO")?
161200140203         exsr  sr_GetTVA;
161300131121
161400131121         // -?Spedizione?
161500131122         exsr  sr_GetSped;
161600131121         if  Not  %found(FNARB01L);
161700131121           leavesr;
161800131121         endif;
161900131122
162000131122         V1Clnp = ds_Spedizione.wLNP;
162100131122         V1Cnrs = ds_Spedizione.wNRS;
162200131122         V1Cnsp = ds_Spedizione.wNSP;
162300131204         //wDate_EUR = %date( (ds_Spedizione.wAAS * 10000) + ARBmgs : *iso );
162400131204         //V1Cdsp = %dec( wDate_EUR );
162500131127
162600131127         // -?Colli?
162700131127         V1Cncl = ARBncl;
162800131127
162900131127         // -?Peso?
163000131127         V1Cpkb = ARBpkb;
163100131127
163200131127         // -?Particolarità?
163300131127         V1Cgga = ARBgga;
163400131127         V1Cgma = ARBgma;
163500131127         V1Cgva = ARBgva;
163600131126
163700131127         // -?Codice Bolla (tab. "3A")?
163800131204         //clear  ds3A;
163900131204         //if  getTabella ( c_Tabel : '3A' : ARBcbo : *omit :
164000131204         //                 *omit : *omit :
164100131204         //                 *omit : *omit :
164200131204         //                 *omit : *omit : *omit : *omit :
164300131204         //                 *omit : *omit : *omit : *omit :
164400131204         //                 ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
164500131204         //                 = *zero      AND
164600131204         //      ds_TNTBE.TBEatb = *blank;
164700131204         //  ds3A = ds_TNTBE.TBEuni;
164800131204         //else;
164900131204         //  §3Ades = *all'? ';
165000131204         //endif;
165100131204         //
165200131204         //V1Ccbo = ARBcbo;
165300131204         //V1Dcbo = §3Ades;
165400131126
165500131126         // -?Tipo Servizio (tab. "5E")?
165600131126         clear  ds5E;
165700131126         if  getTabella ( c_Tabel : '5E' : ARBtsp : *omit :
165800131126                          *omit : *omit :
165900131126                          *omit : *omit :
166000131126                          *omit : *omit : *omit : *omit :
166100131126                          *omit : *omit : *omit : *omit :
166200131126                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
166300131126                          = *zero      AND
166400131126               ds_TNTBE.TBEatb = *blank;
166500131126           ds5E = ds_TNTBE.TBEuni;
166600131126         else;
166700131126           §5Edes = *all'? ';
166800131126         endif;
166900131126
167000131205         evalr V1Dtsp = %trimr(§5Ed08);
167100131122
167200131127         // -?Cliente Mittente?
167300131127         //if  ARBccm <> *zero;
167400131127         //  V1Cccm = ARBccm;
167500131127         //else;
167600131127         //  if  %subst(§3Atb1:1:1) = 'F';
167700131127         //    V1Cccm = ARBksc;
167800131127         //  endif;
167900131127         //endif;
168000131209         //V1Drsm = ARBrsm;
168100131209         //V1Drmo = ARBrmo;
168200131122
168300131127         //// -?Cliente Destinatario?
168400131122         //if  %subst(§3Atb1:1:1) = 'A';
168500131122         //  V1Cccd = ARBksc;
168600131122         //endif;
168700131122         //// -?Se c'è 2ª bolla imposto LNA9999?
168800131122         //if  §3Atb2 <> *blank;
168900131122         //  V1Cccd = (ARBlna * 10000) + 9999;
169000131122         //endif;
169100131209         V1Drsd = ARBrsd + AR4not;
169200131209         V1Dind = ARBind;
169300131209         //V1Dcad = ARBcad;
169400131209         //V1Dlpd = %trim( ARBlod ) + '  ' + ARBprd + ' ' + ARBnzd;
169500131209         V1Dlpd = %trim( ARBcad ) + ' ' + %trim( ARBlod ) + '  ' +
169600131209                  ARBprd + ' ' + ARBnzd;
169700131121
169800131121         // -?Distinta?
169900131205         ////k_DSTfgs = (ARBpdc / 10000);
170000131205         ////k_DSTnfv = ARBndc;
170100131205         //k_DSTfgs = FNLRX2ds.iX2fgs;
170200131205         //k_DSTnfv = FNLRX2ds.iX2dst;
170300131205         //chain  %kds( keyFIDST01 )  FIDST000;
170400131205         //if  Not  %found(FIDST01L);
170500131205         //  clear  DSTpda;
170600131205         //endif;
170700131121
170800131122         // -?Referente Consegna (estensione testata bolla: trk "GEN")?
170900131121         clear  dAR5gen;
171000131121         k_AR5aas = ds_Spedizione.wAAS;
171100131121         k_AR5lnp = ds_Spedizione.wLNP;
171200131121         k_AR5nrs = ds_Spedizione.wNRS;
171300131121         k_AR5nsp = ds_Spedizione.wNSP;
171400131205         //k_AR5trd   = 'GEN';        ?(già così)?
171500131121         chain  %kds(keyFIAR501 : 5)  FIAR5000;
171600131121         if  %found(FIAR501L)  and  AR5atb = *blank;
171700131121           dAR5gen = AR5uni;
171800131121         endif;
171900131122
172000131204         select;
172100131204           when  dAR5gen.§AR5ref  <> *blank  and
172200131204                 dAR5gen.§AR5telD <> *blank;
172300131209             V1Dref = %trimr( dAR5gen.§AR5ref ) + ' - ' +
172400131204                      %triml( dAR5gen.§AR5telD );
172500131204           when  dAR5gen.§AR5ref  <> *blank;
172600131209             V1Dref = dAR5gen.§AR5ref;
172700131204           when  dAR5gen.§AR5telD <> *blank;
172800131209             V1Dref = dAR5gen.§AR5telD;
172900131204         endsl;
173000131121
173100131121         // -?Dati C/Assegno?
173200131121         chain  %kds( keyFNARB01 )  FIAR9000;
173300131122         if  Not %found(FIAR901L);
173400131121           clear  AR9cas;
173500131121           clear  AR9vca;
173600131121         endif;
173700131122
173800131122         V1Ccas = AR9cas;
173900131122         V1Cvca = AR9vca;
174000131121
174100131122         // -?Dati Fattura (Tassazione Bolla - filiale)?
174200131122         clear  keyFIAR601;
174300131121         k_AR6aas = ds_Spedizione.wAAS;
174400131121         k_AR6lnp = ds_Spedizione.wLNP;
174500131121         k_AR6nrs = ds_Spedizione.wNRS;
174600131121         k_AR6nsp = ds_Spedizione.wNSP;
174700131122         // -?C'è 2ª bolla (?)?
174800131122         k_AR6trc = '2';
174900131122         clear  AR6div;
175000131122         clear  AR6ift;
175100131122         chain  %kds(keyFIAR601)  FIAR6000;
175200131122         If  %found(FIAR601L);
175300131122           if  AR6ift = *zero;
175400131122             clear  AR6div;
175500131122           endif;
175600131122         // -?1ª bolla?
175700131122         Else;
175800131122           k_AR6trc = '1';
175900131122           chain  %kds(keyFIAR601)  FIAR6000;
176000131122           if  %found(FIAR601L)  and  AR6ift = *zero;
176100131122             clear  AR6div;
176200131122           endif;
176300131122         EndIf;
176400131122
176500131122         V1Cift = AR6ift;
176600131122         V1Cdiv = AR6div;
176700131121
176800131126         // -?Note LdV?
176900131122         clear  keyFIAR401;
177000131122         k_AR4aas = ds_Spedizione.wAAS;
177100131122         k_AR4lnp = ds_Spedizione.wLNP;
177200131122         k_AR4nrs = ds_Spedizione.wNRS;
177300131122         k_AR4nsp = ds_Spedizione.wNSP;
177400131122         k_AR4trc = '8';
177500131122         chain  %kds(keyFIAR401)  FIAR4000;
177600131122         if  %found(FIAR401L);
177700131122           V1Dnot8 = AR4not;
177800131122         endif;
177900131122         k_AR4trc = '9';
178000131122         chain  %kds(keyFIAR401)  FIAR4000;
178100131122         if  %found(FIAR401L);
178200131122           V1Dnot9 = AR4not;
178300131122         endif;
178400131122
178500131126         // -?Note Bolla?
178600131126         k_ARNaas = ds_Spedizione.wAAS;
178700131126         k_ARNlnp = ds_Spedizione.wLNP;
178800131126         k_ARNnrs = ds_Spedizione.wNRS;
178900131126         k_ARNnsp = ds_Spedizione.wNSP;
179000131205         //k_ARNcdn = 'LDV';          ?(già impostato)?
179100131205         //k_ARNgst = 'S';            ?(già impostato)?
179200131126         setll  %kds(keyFIARN12 : 6)  FIARN000;
179300131126         reade  %kds(keyFIARN12 : 6)  FIARN000;
179400131126         DoW  Not %eof(FIARN12L);
179500131126           select;
179600131126             when  V1Cnt1 = *blank;
179700131126               V1Cnt1 = ARNnob;
179800131126             when  V1Cnt2 = *blank;
179900131126               V1Cnt2 = ARNnob;
180000131126             when  V1Cnt3 = *blank;
180100131126               V1Cnt3 = ARNnob;
180200131126             when  V1Cnt4 = *blank;
180300131126               V1Cnt4 = ARNnob;
180400140121             other;
180500140121               leave;
180600131126           endsl;
180700131126           reade  %kds(keyFIARN12 : 6)  FIARN000;
180800131126         EndDo;
180900131128
181000140108         // -?Note R.A.: solo input?
181100131122
181200131126         // -?Reperimento Ultimo contatto precedente?
181300140203         exsr  sr_GetUltContPrec;
181400131121
181500131121       ENDSR;
181600131127
181700131127       //--------------------------------------------------------------
181800131127       //?Impostazione attributi di visualizzazione video DS1.         ?
181900131127       //--------------------------------------------------------------
182000131127       BEGSR  sr_AttrVisDS1;
182100131127
182200131204         // ·?Ultimo Contatto?
182300131204         RevImgUCO  = (V1Duco <> *blank);
182400131204
182500131127         // ·?Comando?
182600131127         RevImgCMD  = (V1Dcmd <> *blank);
182700131204
182800131204         // ·?Variazioni?
182900131204         RevImgVAR  = (V1Dvar <> *blank);
183000131127
183100131127         // ·?Tipo Servizio?
183200131127         RevImgTSP  = (ARBtsp = 'E'  or  ARBtsp = 'H');
183300131127
183400131127         // ·?Fermo Deposito?
183500131127         VisualFDEP = (ARBffd <> *blank);
183600131127
183700131127         // ·?Contrassegno?
183800131129         VisualCASS = (V1Ccas > *zero);
183900131127
184000131127         // ·?Importo Fattura?
184100131129         VisualIFT  = (V1Cift > *zero);
184200131129
184300131129         // ·?Interrogazione Particolarità Giacenza?
184400131129         VisPartGiac  = (V1Cgga <> *blank);
184500131129
184600131129         // ·?Interrogazione Particolarità Consegna?
184700131129         VisPartCons  = (V1Cgma <> *blank);
184800131129
184900131129         // ·?Interrogazione Particolarità Varia?
185000131129         VisPartVarie = (V1Cgva <> *blank);
185100131127
185200131127       ENDSR;
185300131121
185400131121       //--------------------------------------------------------------
185500131121       //?Controllo videata DS1 (spedizione).                          ?
185600131121       //--------------------------------------------------------------
185700131121       BEGSR  sr_CtrDS1;
185800131121
185900131121         // -?Spegnimento indicatori di posizionamento cursore?
186000131127         %subst(IndDspF : 60) = *off;
186100131127
186200131127         // -?Interrogazione Particolarità Giacenza?
186300131127         if  V1Igga = '?';
186400131128           reset  ds7PQRS;
186500131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
186600131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
186700131128           ds7PQRS.DS7cod = V1Cgga;
186800131127           KPJBU = ds7PQRS;
186900131127           trtb69r ( kpjba );
187000131127           clear  V1Igga;
187100131127           ErrGenerico = *on;
187200131127           leavesr;
187300131127         endif;
187400131127
187500131127         // -?Interrogazione Particolarità Consegna?
187600131127         if  V1Igma = '?';
187700131128           reset  ds7PQRS;
187800131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
187900131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
188000131128           ds7PQRS.DS7cod = V1Cgma;
188100131127           KPJBU = ds7PQRS;
188200131127           trtb70r ( kpjba );
188300131127           clear  V1Igma;
188400131127           ErrGenerico = *on;
188500131127           leavesr;
188600131127         endif;
188700131127
188800131127         // -?Interrogazione Particolarità Varie?
188900131127         if  V1Igva = '?';
189000131128           reset  ds7PQRS;
189100131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
189200131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
189300131128           ds7PQRS.DS7cod = V1Cgva;
189400131127           KPJBU = ds7PQRS;
189500131127           trtb73r ( kpjba );
189600131127           clear  V1Igva;
189700131127           ErrGenerico = *on;
189800131127           leavesr;
189900131127         endif;
190000131128
190100131128         clear  V1Igga;
190200131128         clear  V1Igma;
190300131128         clear  V1Igva;
190400131121
190500131121       ENDSR;
190600131121
190700131121       //--------------------------------------------------------------
190800131122       //?Richiamo *pgm x Interrogazione Spedizione (in filiale).      ?
190900131121       //--------------------------------------------------------------
191000131121       BEGSR  sr_VisualSped;
191100131121
191200131121         clear  FNLR36ds;
191300131121         FNLR36ds.LR36aas = k_ARBaas;
191400131121         FNLR36ds.LR36lnp = k_ARBlnp;
191500131121         FNLR36ds.LR36nrs = k_ARBnrs;
191600131121         FNLR36ds.LR36nsp = k_ARBnsp;
191700131121         FNLR36ds.LR36flg = '1';
191800131121         kpjbu = FNLR36ds;
191900131121
192000131121         fnlr36r ( kpjba );
192100131121
192200131121         FNLR36ds = kpjbu;
192300131121         if  FNLR36ds.LR36f03 = *on;
192400131121           $Video = 'S1';
192500131121           ErrGenerico = *on;
192600131121         endif;
192700131121
192800131121       ENDSR;
192900131209
193000131209       //--------------------------------------------------------------
193100131209       //?Inizializzazione videata DO1 (O.R.M.).                       ?
193200131209       //--------------------------------------------------------------
193300131209       BEGSR  sr_InzDO1;
193400131209
193500131209         ds_ORM = S1Hogg;
193600140207
193700140207         // -?Impostazione chiave di accesso per la v.l. 02L?
193800140207         clear  keyFICAU02;
193900140207         k_CA2fgs    = FNLRX2ds.iX2fgs;
194000140207         k_CA2nfv    = FNLRX2ds.iX2dst;
194100140207         k_CA2pdr    = FNLRX2ds.iX2aut;
194200140207         k_CA2tor    = S1Htor;
194300140207         k_CA2ogg    = S1Hogg;
194400140207         k_CA2dtOins = *hival;
194500131209
194600131209         // -?Pulizia videata?
194700131209         clear  LRX2DO1;
194800131209         clear  $UltContPrec;
194900131209
195000131209         // -?Contatto ad Autotrasportatore?
195100140203         clear  sk_TVA;
195200131209         clear  keyFICAU01;
195300140206         //k_CAUfgs    = S1Hfgs;
195400140206         //k_CAUpdr    = C1Cpdr;
195500140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
195600140206         k_CAUpdr    = FNLRX2ds.iX2aut;
195700131209         k_CAUdtOins = S1HdtOins;
195800131209         k_CAUtor    = S1Htor;
195900131209         k_CAUogg    = S1Hogg;
196000140203         chain(N)  %kds( keyFICAU01 )  FICAU000;
196100140203
196200140203         // -?Rec. NON trovato: ricerca con data/ora inserimento più?
196300140203         //  ?recenti (segnalando errore SE non trovato nuovo rec.)?
196400140203         //  ?e riemissione della videata?
196500131209         if  Not %found(FICAU01L)  or  CAUatb <> *blank;
196600140203
196700140203           exsr  sr_Ricarica_Ogg;
196800140203
196900140203           if  Not $RecUpdated;
197000140203             ErrGenerico = *on;
197100140203             ErrMessage  = *on;
197200140203             V1Dmsg = sk_Msg(03);
197300140203             leavesr;
197400140203           endif;
197500140203
197600131209         endif;
197700131209
197800131209         // -?Decodifica autotrasportatore?
197900140203         exsr  sr_GetAUT;
198000131209
198100131209         // -?Comando da impartire al PDA o all'AUT?
198200140203         exsr  sr_GetCMD;
198300131209
198400131209         // -?Tipologica Causale Variazione bolla?
198500140131         //  ?(SE NON "INSERITO")?
198600140203         exsr  sr_GetTVA;
198700131209
198800131209         // -?O.R.M.?
198900131209         exsr  sr_GetORM;
199000131209         if  Not  %found(FNORM01L);
199100131209           leavesr;
199200131209         endif;
199300131209
199400131209         V1Cpoe = ds_ORM.wPOE;
199500131209         V1Cnsr = ds_ORM.wNSR;
199600131209         V1Cnor = ds_ORM.wNOR;
199700131209         V1Cnrv = ds_ORM.wNRV;
199800131211
199900131211         // -?Orari di Apertura del cliente?
200000131211         //  ?Recupero dati da estensione FNORE?
200100131211         //  ?Rcd "O " => Orari apertura?
200200131211         clear dOREorari;
200300131211         k_OREpoe = ds_ORM.wPOE;
200400131211         k_OREnsr = ds_ORM.wNSR;
200500131211         k_OREnor = ds_ORM.wNOR;
200600131211         k_OREnrv = ds_ORM.wNRV;
200700131211
200800131211         chain  %kds(keyFNORE01)  FNORE000;
200900131211
201000131211         if  %found(FNORE01L);
201100131211           dOREorari = OREdati;
201200131211         endif;
201300131211         V1oraAMda = dOREorari.§OREoramda;
201400131211         V1oraAMa  = dOREorari.§OREorama;
201500131211         V1oraPMda = dOREorari.§OREorapda;
201600131211         V1oraPMa  = dOREorari.§OREorapa;
201700131210
201800131211         // -?Ricevuta di Ritiro?
201900131210         if  dORM01.§ORsrm = 'S';
202000131210           V1Datn = 'Ricevuta di Ritiro';
202100131210         endif;
202200140121
202300140121         // -?Ora merce pronta?
202400140121         V1orr  = ORMorr;
202500131209
202600131209         // -?Colli?
202700131209         V1Cncl = ORMncl;
202800131209
202900131209         // -?Peso?
203000131209         V1Cpkg = ORMpkg;
203100131209
203200131209         // -?Bancali?
203300131209         V1Cbnc = ORMbnc;
203400131209
203500131209         // -?Volume?
203600131209         V1Cvlm = ORMvlm;
203700131209
203800131209         // -?Spunda Idraulica?
203900131209         if  ORMspi = 'S';
204000131209           V1Cspi = 'SI';
204100131209         else;
204200131209           V1Cspi = 'NO';
204300131209         endif;
204400131209
204500131209         // -?Cliente Mittente?
204600131209         V1Drsr = ORMrsr;
204700131209         V1Dinr = ORMinr;
204800131209         V1Dlor = ORMlor;
204900131209         V1Dprr = ORMprr;
205000131209         V1Dcar = ORMcar;
205100131209         V1Dnar = ORMnar;
205200131209         //V1Dlpr = %trim( ORMcar ) + ' ' + %trim( ORMlor ) + '  ' +
205300131209         //         ORMprr + ' ' + ORMnar;
205400131209
205500131209         // -?Referente?
205600131209         select;
205700131209           when  ORMrer <> *blank  and  ORMter <> *blank;
205800131209             V1Dref = %trimr( ORMrer ) + ' - ' + %triml( ORMter );
205900131209           when  ORMrer  <> *blank;
206000131209             V1Dref = ORMrer;
206100131209           when  ORMter <> *blank;
206200131209             V1Dref = ORMter;
206300131209         endsl;
206400131210
206500131210         // -?Importo Prepagato?
206600131210         If  ORMtor = 'P';
206700131210
206800131210           clear  keyFIAR404;
206900131210           k_AR44trc = 'M';
207000131210           k_AR44n14 = ds_ORM;
207100131210           chain  %kds(keyFIAR404)  FIAR4004;
207200131210
207300131210           if  %found(FIAR404L);
207400131210
207500131210             clear  keyFIAR601;
207600131210             k_AR6aas = AR4aas;
207700131210             k_AR6lnp = AR4lnp;
207800131210             k_AR6nrs = AR4nrs;
207900131210             k_AR6nsp = AR4nsp;
208000131210             chain  %kds(keyFIAR601 : 4)  FIAR6000;
208100131210
208200131210             if  %found(FIAR601L);
208300131210               w0112 = AR6ift;
208400131210               V1Datn = 'Prepagato: ' + %triml( %editc( w0112 : 'K' ) )
208500131210                                      + AR6div;
208600131210             endif;
208700131210
208800131210           endif;
208900131210
209000131210         EndIf;
209100131209
209200131209         // -?Note O.R.M.?
209300131209         V1Dnot1 = ORMno1;
209400131209         V1Dnot2 = ORMno2;
209500131209
209600131209         // -?Note AUT?
209700140121         clear  keyFNORT01;
209800140121         k_ORTpoe = ds_ORM.wPOE;
209900140121         k_ORTnsr = ds_ORM.wNSR;
210000140121         k_ORTnor = ds_ORM.wNOR;
210100140121         k_ORTnrv = ds_ORM.wNRV;
210200140121         setll  %kds( keyFNORT01 )  FNORT000;
210300140121         reade  %kds( keyFNORT01 : 4 )  FNORT000;
210400140121         DoW  Not %eof(FNORT01L);
210500140121           select;
210600140123             when  ORTgst <> 'S';
210700140121             when  V1Cnt1 = *blank;
210800140121               V1Cnt1 = ORTnob;
210900140121             when  V1Cnt2 = *blank;
211000140121               V1Cnt2 = ORTnob;
211100140121             //when  V1Cnt3 = *blank;
211200140121             //  V1Cnt3 = ORTnob;
211300140121             //when  V1Cnt4 = *blank;
211400140121             //  V1Cnt4 = ORTnob;
211500140121             other;
211600140121               leave;
211700140121           endsl;
211800140121           reade  %kds( keyFNORT01 : 4 )  FNORT000;
211900140121         EndDo;
212000131209
212100140108         // -?Note R.A.: solo input?
212200131209
212300131209         // -?Reperimento Ultimo contatto precedente?
212400140203         exsr  sr_GetUltContPrec;
212500131209
212600131209       ENDSR;
212700131209
212800131209       //--------------------------------------------------------------
212900131209       //?Impostazione attributi di visualizzazione video DO1.         ?
213000131209       //--------------------------------------------------------------
213100131209       BEGSR  sr_AttrVisDO1;
213200131209
213300131209         // ·?Ultimo Contatto?
213400131209         RevImgUCO   = (V1Duco <> *blank);
213500131209
213600131209         // ·?Comando?
213700131209         RevImgCMD   = (V1Dcmd <> *blank);
213800131209
213900131209         // ·?Variazioni?
214000131209         RevImgVAR   = (V1Dvar <> *blank);
214100131209
214200131209         // ·?O.R.M. PrePagato con importo?
214300131210         VisualORMPP = (ORMtor = 'P');
214400131209
214500131209         // ·?Ricevuta di Ritiro?
214600131210         VisualORMRR = (dORM01.§ORsrm = 'S');
214700131209
214800131209       ENDSR;
214900131209
215000131209       //--------------------------------------------------------------
215100131209       //?Controllo dati nella videata D01.                            ?
215200131209       //--------------------------------------------------------------
215300131209       BEGSR  sr_CtrDO1;
215400131209
215500131209         // -?Spegnimento indicatori di posizionamento cursore?
215600140207         %subst(IndDspF : 60) = *off;
215700131209
215800131209       ENDSR;
215900131209
216000131209       //--------------------------------------------------------------
216100131209       //?Richiamo *pgm x Interrogazione O.R.M. (in filiale).          ?
216200131209       //--------------------------------------------------------------
216300131209       BEGSR  sr_VisualORM;
216400131209
216500131209         clear  Parm01;
216600131209         Parm01.pPOE = ds_ORM.wPOE;
216700131209         Parm01.pNSR = ds_ORM.wNSR;
216800131209         Parm01.pNOR = ds_ORM.wNOR;
216900131209         Parm01.pNRV = ds_ORM.wNRV;
217000131209         Parm01.pSCE = '5';
217100131209         kpjbu = Parm01;
217200131209
217300131209         clear  FIDNA1ds;
217400131209         FIDNA1ds.iA1ins = 'N';
217500131209
217600131209         fior07r ( kpjba : FIDNA1ds );
217700131209
217800131209       ENDSR;
217900131128
218000131128       //--------------------------------------------------------------
218100131128       //?Gestione videata W01 (Visualizzazione Ultimo Contatto).      ?
218200131128       //--------------------------------------------------------------
218300131128       BEGSR  sr_GesW01;
218400131128
218500131128         // -?Inizializzazione videata?
218600140122         //$VideoPrec = $Video;
218700140122         //$Video  = 'W1';
218800131128         if  $InzW01 = *on;
218900131128           exsr  sr_InzW01;
219000131128           $InzW01 = *off;
219100131128         endif;
219200131128
219300131128         // -?Emissione window?
219400131128         DoU  dsp_aid = c_F12;
219500131128           exfmt  LRX2W01;
219600131128         EndDo;
219700131128
219800131128         // -?F12=Ritorno?
219900140122         //if  dsp_aid = c_F12;
220000140122         //  $Video  = $VideoPrec;
220100140122         //endif;
220200131128
220300131128       ENDSR;
220400131128
220500131128       //--------------------------------------------------------------
220600131128       //?Inizializzazione window W01.                                 ?
220700131128       //--------------------------------------------------------------
220800131128       BEGSR  sr_InzW01;
220900131128
221000131128         // -?Reperimento Ultimo contatto precedente?
221100140203         //  ?GIÀ effettuato nella subr. "sr_InzDS1/DO1"?
221200131128
221300131128
221400131128         clear  LRX2W01;
221500131128
221600131128         // -?Profilo Utente Telefonata Precedente?
221700140213         W1pruTel   = CA2pruTel;
221800131128
221900131128         // -?Data/Ora Inserimento Telefonata Precedente?
222000140213         ds_DataOra = CA2dtOins;
222100131128         wDate_EUR  = %date( ds_DataOra.wDate );
222200131128         W1dtTel    = %dec( wDate_EUR );
222300131128         W1orTel    = ds_DataOra.wTime;
222400131128
222500131128         // -?Descrizione Comando Telefonata precedente?
222600131128         clear  dCMD;
222700140213         If  CA2desCmd = *blank  and  CA2cmd <> *blank;
222800131128           if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
222900131128                            *omit : *omit :
223000131128                            *omit : *omit :
223100131128                            *omit : *omit : *omit : *omit :
223200131128                            *omit : *omit : *omit : *omit :
223300131128                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
223400131128                            = *zero      AND
223500131128                 ds_TNTBE.TBEatb = *blank;
223600131128             dCMD = ds_TNTBE.TBEuni;
223700131128           else;
223800140213             §CMDdes = CA2cmd + '-' + '? ? ? ? ? ? ? ? ? ?';
223900131128           endif;
224000131128         Else;
224100140213           §CMDdes = %triml( CA2desCmd );
224200131128         EndIf;
224300131128         W1desCmd = §CMDdes;
224400131128
224500131128         // -?Descrizione Variazione Telefonata precedente?
224600140213         If  CA2desVA = *blank;
224700140213           wTipTVA = 'B';
224800140213           ds_TVA  = CA2tva;
224900140213           exsr  sr_GetDesTCV;
225000140213           W1desVa1 = wDesTVA;
225100140213         Else;
225200140213           W1desVa1 = %subst( CA2desVA : 1 : %size(W1desVa1) );
225300140213           W1desVa2 = %subst( CA2desVA : %size(W1desVa1) + 1 );
225400140213         EndIf;
225500131128
225600131128       ENDSR;
225700140226
225800140226       //--------------------------------------------------------------
225900140226       //?Gestione videata W02 (Visualizzazione Altri Dati).           ?
226000140226       //--------------------------------------------------------------
226100140226       BEGSR  sr_GesW02;
226200140226
226300140226         // -?Inizializzazione videata?
226400140226         //$VideoPrec = $Video;
226500140226         //$Video  = 'W2';
226600140226         if  $InzW02 = *on;
226700140226           exsr  sr_InzW02;
226800140226           $InzW02 = *off;
226900140226         endif;
227000140226
227100140226         // -?Emissione window?
227200140226         DoU  dsp_aid = c_F12;
227300140226           exfmt  LRX2W02;
227400140226         EndDo;
227500140226
227600140226         // -?F12=Ritorno?
227700140226         //if  dsp_aid = c_F12;
227800140226         //  $Video  = $VideoPrec;
227900140226         //endif;
228000140226
228100140226       ENDSR;
228200140226
228300140226       //--------------------------------------------------------------
228400140226       //?Inizializzazione window W02.                                 ?
228500140226       //--------------------------------------------------------------
228600140226       BEGSR  sr_InzW02;
228700140226
228800140226         clear  LRX2W02;
228900140226
229000140226         // -?Profilo Utente Inserimento Telefonata?
229100140226         W2pruTel   = CAUpruIns;
229200140226
229300140226         // -?Data/Ora Inserimento Telefonata?
229400140226         ds_DataOra = CAUdtOins;
229500140226         wDate_EUR  = %date( ds_DataOra.wDate );
229600140226         W2dtTel    = %dec( wDate_EUR );
229700140226         W2orTel    = ds_DataOra.wTime;
229800140226
229900140226         // -?Posizionamento (verticale) Window?
230000140226         select;
230100140226           when  $Video  = 'DS';
230200140226             W2rig = 16;
230300140226           when  $Video  = 'DO';
230400140226             W2rig = 14;
230500140226           //other;
230600140226           //  W2rig = 2;
230700140226         endsl;
230800140226
230900140226       ENDSR;
231000140207
231100140207       //--------------------------------------------------------------
231200140207       //?Reperimento dati della Spedizione (Consegna).                ?
231300140207       //--------------------------------------------------------------
231400140207       BEGSR  sr_GetSped;
231500140207
231600140207         ds_Spedizione = CAUogg;
231700140207
231800140207         // -?Reperimento dati da Bolle Arrivi: Testate (FNARB)?
231900140207         clear  keyFNARB01;
232000140207         k_ARBaas = ds_Spedizione.wAAS;
232100140207         k_ARBlnp = ds_Spedizione.wLNP;
232200140207         k_ARBnrs = ds_Spedizione.wNRS;
232300140207         k_ARBnsp = ds_Spedizione.wNSP;
232400140207
232500140207         chain  %kds( keyFNARB01 )  FNARB000;
232600140207
232700140207         if  Not  %found(FNARB01L);
232800140207           ARBrsd = *all'? ';
232900140207         endif;
233000140207
233100140207         // -?Estensione ragione sociale destinatario da FIAR4 rec. "D"?
233200140207         clear  keyFIAR401;
233300140207         k_AR4aas = ds_Spedizione.wAAS;
233400140207         k_AR4lnp = ds_Spedizione.wLNP;
233500140207         k_AR4nrs = ds_Spedizione.wNRS;
233600140207         k_AR4nsp = ds_Spedizione.wNSP;
233700140207         k_AR4trc = 'D';
233800140207
233900140207         chain  %kds(keyFIAR401)  FIAR4000;
234000140207
234100140207         if  Not  %found(FIAR401L);
234200140207           clear  AR4not;
234300140207         endif;
234400140207
234500140207       ENDSR;
234600140207
234700140207       //--------------------------------------------------------------
234800140207       //?Reperimento dati dell'Ordine Ritiro Merce (Ritiro).          ?
234900140207       //--------------------------------------------------------------
235000140207       BEGSR  sr_GetORM;
235100140207
235200140207         ds_ORM = CAUogg;
235300140207
235400140207         clear  keyFNORM01;
235500140207         k_ORMpoe = ds_ORM.wPOE;
235600140207         k_ORMnsr = ds_ORM.wNSR;
235700140207         k_ORMnor = ds_ORM.wNOR;
235800140207         k_ORMnrv = ds_ORM.wNRV;
235900140207
236000140207         chain  %kds( keyFNORM01 )  FNORM000;
236100140207
236200140207         if  Not  %found(FNORM01L);
236300140207           ORMrsr = *all'? ';
236400140207           clear  dORM01;
236500140207         else;
236600140207           dORM01 = ORMflo;
236700140207         endif;
236800140207
236900140207       ENDSR;
237000140203
237100140203       //--------------------------------------------------------------
237200140203       //?Reperimento ed Impostazione a video delle informazioni AUT.  ?
237300140203       //--------------------------------------------------------------
237400140203       BEGSR  sr_GetAUT;
237500140203
237600140203         // -?Decodifica autotrasportatore?
237700140203         k_APDpdr = CAUpdr;
237800140203         chain  %kds( keyFIAPD01 )  FIAPD000;
237900140203         if  Not %found(FIAPD01L)  or  APDatb <> *blank;
238000140203           clear  APDrsc;
238100140203         endif;
238200140203
238300140203         V1Cpdr = CAUpdr;
238400140203         V1Dpdr = APDrsc;
238500140203
238600140203         // -?Num. Telefono Autotrasportatore?
238700140203         //k_APD4pdr = CAUpdr;
238800140203         //chain  %kds( keyFIAPD41 )  FIAPD400;
238900140203         //if  %found(FIAPD41L)  and  APD4atb = *blank;
239000140203         //  dAPD4tel = APD4uni;
239100140203         //endif;
239200140203         //
239300140203         //V1Dtel = dAPD4tel.§APD4tel;
239400140203
239500140203       ENDSR;
239600140203
239700140203       //--------------------------------------------------------------
239800140203       //?Reperimento ed Impostazione a video delle informazioni CMD.  ?
239900140203       //--------------------------------------------------------------
240000140203       BEGSR  sr_GetCMD;
240100140203
240200140203         // -?Comando da impartire al PDA o all'AUT?
240300140203         clear  dCMD;
240400140203         If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
240500140203           if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
240600140203                            *omit : *omit :
240700140203                            *omit : *omit :
240800140203                            *omit : *omit : *omit : *omit :
240900140203                            *omit : *omit : *omit : *omit :
241000140203                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
241100140203                            = *zero      AND
241200140203                 ds_TNTBE.TBEatb = *blank;
241300140203             dCMD = ds_TNTBE.TBEuni;
241400140203           else;
241500140203             §CMDdes = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ?';
241600140203           endif;
241700140203         Else;
241800140203           §CMDdes = %triml( CAUdesCmd );
241900140203         EndIf;
242000140203
242100140203         if  §CMDdes <> *blank;
242200140203           xx = ( %size(V1Dcmd) - %len( %trim(§CMDdes) ) ) / 2;
242300140203           %subst( V1Dcmd : xx + 1 ) = §CMDdes;
242400140203         endif;
242500140203
242600140203       ENDSR;
242700140203
242800140203       //--------------------------------------------------------------
242900140203       //?Reperimento ed Impostazione a video delle informazioni TVA.  ?
243000140203       //--------------------------------------------------------------
243100140203       BEGSR  sr_GetTVA;
243200140203
243300140203         // -?Tipologica Causale Variazione bolla?
243400140203         //  ?(SE NON "INSERITO")?
243500140203         If  CAUcmd <> c_Cmd_Inserito;
243600140203           If  CAUdesVA = *blank;
243700140203             wTipTVA = *blank;
243800140213             ds_TVA  = CAUtva;
243900140203             exsr  sr_GetDesTCV;
244000140203             V1Dvar  = wDesTVA;
244100140203             //if  %subst( wDesTVA : %size( V1Dvar ) + 1 ) <> *blank;
244200140203             //  %subst( V1Dvar : %size( V1Dvar ) - 3 ) = ' (+)';
244300140203             //endif;
244400140203           Else;
244500140203             V1Dvar = CAUdesVa;
244600140203             //if  %subst( CAUdesVA : %size( V1Dvar ) + 1 ) <> *blank;
244700140203             //  %subst( V1Dvar : %size( V1Dvar ) - 3 ) = ' (+)';
244800140203             //endif;
244900140203           EndIf;
245000140203         EndIf;
245100140203
245200140203       ENDSR;
245300131205
245400131205       //--------------------------------------------------------------
245500131205       //?Reperimento dati della Spedizione (Consegna).                ?
245600131205       //--------------------------------------------------------------
245700131205       BEGSR  sr_GetDesTCV;
245800131205
245900131205         clear  wDesTVA;
246000131205
246100131205         For  xx = 1  To  %elem(sk_TVA);
246200131205
246300131205           clear  dTCV;
246400131205
246500131205           If  sk_TVA(xx) <> *blank;
246600131205
246700131205             yy = %lookup( sk_TVA(xx) : sk_TCV );
246800131205
246900131205             If  yy > *zero;
247000131205
247100131205               dTCV = sk_TCVd(yy);
247200131205               If  wDesTVA = *blank;
247300131205                 select;
247400131205                   when  wTipTVA = 'B';
247500131205                     wDesTVA = %triml( §TCVdesB );
247600131205                   when  wTipTVA = 'V';
247700131205                     wDesTVA = %triml( §TCVdesV );
247800131205                   other;
247900131205                     wDesTVA = %triml( §TCVdes );
248000131205                 endsl;
248100131205               Else;
248200131205                 select;
248300131205                   when  wTipTVA = 'B';
248400131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
248500131205                               %triml( §TCVdesB );
248600131205                   when  wTipTVA = 'V';
248700131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
248800131205                               %triml( §TCVdesV );
248900131205                   other;
249000131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
249100131205                               %triml( §TCVdes );
249200131205                 endsl;
249300131205               EndIf;
249400131205
249500131205             Else;
249600131205
249700131205               §TCVdesB  = '???' + sk_TVA(xx) + '???';
249800131205               if  wDesTVA = *blank;
249900131205                 wDesTVA = %triml( §TCVdesB );
250000131205               else;
250100131205                 wDesTVA = %trimr( wDesTVA ) + '-' +
250200131205                           %triml( §TCVdesB );
250300131205               endif;
250400131205
250500131205             EndIf;
250600131205
250700131205           EndIf;
250800131205
250900131205         EndFor;
251000131205
251100131205       ENDSR;
251200140203
251300140203       //--------------------------------------------------------------
251400140203       //?Reperimento ed Impostazione a video dell'eventuale           ?
251500140203       //?Ultimo Contatto Precedente.                                  ?
251600140203       //--------------------------------------------------------------
251700140203       BEGSR  sr_GetUltContPrec;
251800140203
251900140203         // -?Reperimento Ultimo contatto precedente?
252000140206         //clear  keyFICAU02;      ?(già impostata nella subr. sr_InzDS1/DO1)?
252100140206         //k_CA2fgs    = CAUfgs;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252200140206         //k_CA2nfv    = CAUnfv;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252300140206         //k_CA2pdr    = CAUpdr;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252400140206         //k_CA2tor    = CAUtor;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252500140206         //k_CA2ogg    = CAUogg;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252600140206         k_CA2dtOins = CAUdtOins;
252700140206
252800140206         chain  %kds( keyFICAU02 )  FICAU002;
252900140206         reade  %kds( keyFICAU02 : 5)  FICAU002;
253000140206
253100140203         // -?NON?trovato alcun contatto precedente?
253200140206         if  %eof(FICAU02L);
253300140203           leavesr;
253400140203         endif;
253500140206
253600140203         // -?Trovato l'ultimo contatto precedente?
253700140203         $UltContPrec = *on;
253800140206         ds_DataOra = CA2dtOins;
253900140203         V1Duco     = 'Ultimo Contatto alle ' +
254000140203                      %editw( ds_DataOra.wTime4 : '  :  ' );
254100140203         // *...+....1....+....2....+.
254200140203         // Ultimo Contatto alle 12:34
254300140203
254400140203       ENDSR;
254500140203
254600140203       //--------------------------------------------------------------
254700140203       //?Reperimento (nuovo) ultimo record per l'oggetto in esame.    ?
254800140203       //--------------------------------------------------------------
254900140203       BEGSR  sr_Verifica_Ogg;
255000140203
255100140203         select;
255200140203
255300140203           when  S1Htor = c_Consegna;
255400140203             $RecUpdated = ( V1Cpdr  <> SaveDS1.V1Cpdr   or
255500140203                             V1Dpdr  <> SaveDS1.V1Dpdr   or
255600140203                             V1Duco  <> SaveDS1.V1Duco   or
255700140203                             V1Dcmd  <> SaveDS1.V1Dcmd   or
255800140203                             V1Dvar  <> SaveDS1.V1Dvar   or
255900140203                             V1Clnp  <> SaveDS1.V1Clnp   or
256000140203                             V1Cnrs  <> SaveDS1.V1Cnrs   or
256100140203                             V1Cnsp  <> SaveDS1.V1Cnsp   or
256200140203                             V1Dtsp  <> SaveDS1.V1Dtsp   or
256300140203                             V1Drsd  <> SaveDS1.V1Drsd   or
256400140203                             V1Dind  <> SaveDS1.V1Dind   or
256500140203                             V1Dlpd  <> SaveDS1.V1Dlpd   or
256600140203                             V1Cncl  <> SaveDS1.V1Cncl   or
256700140203                             V1Cpkb  <> SaveDS1.V1Cpkb   or
256800140203                             V1Dref  <> SaveDS1.V1Dref   or
256900140203                           //V1Igga  <> SaveDS1.V1Igga   or
257000140203                             V1Cgga  <> SaveDS1.V1Cgga   or
257100140203                           //V1Igma  <> SaveDS1.V1Igma   or
257200140203                             V1Cgma  <> SaveDS1.V1Cgma   or
257300140203                           //V1Igva  <> SaveDS1.V1Igva   or
257400140203                             V1Cgva  <> SaveDS1.V1Cgva   or
257500140203                             V1Ccas  <> SaveDS1.V1Ccas   or
257600140203                             V1Cvca  <> SaveDS1.V1Cvca   or
257700140203                             V1Cift  <> SaveDS1.V1Cift   or
257800140203                             V1Cdiv  <> SaveDS1.V1Cdiv   or
257900140203                             V1Dnot8 <> SaveDS1.V1Dnot8  or
258000140203                             V1Dnot9 <> SaveDS1.V1Dnot9  or
258100140203                             V1Cnt1  <> SaveDS1.V1Cnt1   or
258200140203                             V1Cnt2  <> SaveDS1.V1Cnt2   or
258300140203                             V1Cnt3  <> SaveDS1.V1Cnt3   or
258400140203                             V1Cnt4  <> SaveDS1.V1Cnt4   or
258500140203                             V1Nra1  <> SaveDS1.V1Nra1   or
258600140203                             V1Nra2  <> SaveDS1.V1Nra2 );
258700140203
258800140203           when  S1Htor = c_Ritiro;
258900140203             $RecUpdated = ( V1Cpdr    <> SaveDO1.V1Cpdr     or
259000140203                             V1Dpdr    <> SaveDO1.V1Dpdr     or
259100140203                             V1Duco    <> SaveDO1.V1Duco     or
259200140203                             V1Dcmd    <> SaveDO1.V1Dcmd     or
259300140203                             V1Dvar    <> SaveDO1.V1Dvar     or
259400140203                             V1Cpoe    <> SaveDO1.V1Cpoe     or
259500140203                             V1Cnsr    <> SaveDO1.V1Cnsr     or
259600140203                             V1Cnor    <> SaveDO1.V1Cnor     or
259700140203                             V1Cnrv    <> SaveDO1.V1Cnrv     or
259800140203                             V1oraAMda <> SaveDO1.V1oraAMda  or
259900140203                             V1oraAMa  <> SaveDO1.V1oraAMa   or
260000140203                           //V1oraSep  <> SaveDO1.V1oraSep   or
260100140203                             V1oraPMda <> SaveDO1.V1oraPMda  or
260200140203                             V1oraPMa  <> SaveDO1.V1oraPMa   or
260300140203                             V1datN    <> SaveDO1.V1datN     or
260400140203                             V1Drsr    <> SaveDO1.V1Drsr     or
260500140203                             V1Dinr    <> SaveDO1.V1Dinr     or
260600140203                             V1Dlor    <> SaveDO1.V1Dlor     or
260700140203                             V1Dprr    <> SaveDO1.V1Dprr     or
260800140203                             V1Dcar    <> SaveDO1.V1Dcar     or
260900140203                             V1Dnar    <> SaveDO1.V1Dnar     or
261000140203                             V1orr     <> SaveDO1.V1orr      or
261100140203                             V1Dref    <> SaveDO1.V1Dref     or
261200140203                             V1Cncl    <> SaveDO1.V1Cncl     or
261300140203                             V1Cpkg    <> SaveDO1.V1Cpkg     or
261400140203                             V1Cbnc    <> SaveDO1.V1Cbnc     or
261500140203                             V1Cvlm    <> SaveDO1.V1Cvlm     or
261600140203                             V1Cspi    <> SaveDO1.V1Cspi     or
261700140203                             V1Dnot1   <> SaveDO1.V1Dnot1    or
261800140203                             V1Dnot2   <> SaveDO1.V1Dnot2    or
261900140203                             V1Cnt1    <> SaveDO1.V1Cnt1     or
262000140203                             V1Cnt2    <> SaveDO1.V1Cnt2     or
262100140203                             V1nra1    <> SaveDO1.V1nra1     or
262200140203                             V1nra2    <> SaveDO1.V1nra2 );
262300140203
262400140203         endsl;
262500140203
262600140203       ENDSR;
262700140203
262800140203       //--------------------------------------------------------------
262900140203       //?Memorizzazione dati da confrontare per rilevare variazioni.  ?
263000140203       //--------------------------------------------------------------
263100140203       BEGSR  sr_Memo_Dati;
263200140203
263300140203         Select;
263400140203
263500140203           When  S1Htor = c_Consegna;
263600140203             SaveDS1.V1Cpdr  = V1Cpdr;
263700140203             SaveDS1.V1Dpdr  = V1Dpdr;
263800140203             SaveDS1.V1Duco  = V1Duco;
263900140203             SaveDS1.V1Dcmd  = V1Dcmd;
264000140203             SaveDS1.V1Dvar  = V1Dvar;
264100140203             SaveDS1.V1Clnp  = V1Clnp;
264200140203             SaveDS1.V1Cnrs  = V1Cnrs;
264300140203             SaveDS1.V1Cnsp  = V1Cnsp;
264400140203             SaveDS1.V1Dtsp  = V1Dtsp;
264500140203             SaveDS1.V1Drsd  = V1Drsd;
264600140203             SaveDS1.V1Dind  = V1Dind;
264700140203             SaveDS1.V1Dlpd  = V1Dlpd;
264800140203             SaveDS1.V1Cncl  = V1Cncl;
264900140203             SaveDS1.V1Cpkb  = V1Cpkb;
265000140203             SaveDS1.V1Dref  = V1Dref;
265100140203           //SaveDS1.V1Igga  = V1Igga;
265200140203             SaveDS1.V1Cgga  = V1Cgga;
265300140203           //SaveDS1.V1Igma  = V1Igma;
265400140203             SaveDS1.V1Cgma  = V1Cgma;
265500140203           //SaveDS1.V1Igva  = V1Igva;
265600140203             SaveDS1.V1Cgva  = V1Cgva;
265700140203             SaveDS1.V1Ccas  = V1Ccas;
265800140203             SaveDS1.V1Cvca  = V1Cvca;
265900140203             SaveDS1.V1Cift  = V1Cift;
266000140203             SaveDS1.V1Cdiv  = V1Cdiv;
266100140203             SaveDS1.V1Dnot8 = V1Dnot8;
266200140203             SaveDS1.V1Dnot9 = V1Dnot9;
266300140203             SaveDS1.V1Cnt1  = V1Cnt1;
266400140203             SaveDS1.V1Cnt2  = V1Cnt2;
266500140203             SaveDS1.V1Cnt3  = V1Cnt3;
266600140203             SaveDS1.V1Cnt4  = V1Cnt4;
266700140203             SaveDS1.V1Nra1  = V1Nra1;
266800140203             SaveDS1.V1Nra2  = V1Nra2;
266900140203
267000140203           When  S1Htor = c_Ritiro;
267100140203             SaveDO1.V1Cpdr    = V1Cpdr;
267200140203             SaveDO1.V1Dpdr    = V1Dpdr;
267300140203             SaveDO1.V1Duco    = V1Duco;
267400140203             SaveDO1.V1Dcmd    = V1Dcmd;
267500140203             SaveDO1.V1Dvar    = V1Dvar;
267600140203             SaveDO1.V1Cpoe    = V1Cpoe;
267700140203             SaveDO1.V1Cnsr    = V1Cnsr;
267800140203             SaveDO1.V1Cnor    = V1Cnor;
267900140203             SaveDO1.V1Cnrv    = V1Cnrv;
268000140203             SaveDO1.V1oraAMda = V1oraAMda;
268100140203             SaveDO1.V1oraAMa  = V1oraAMa;
268200140203           //SaveDO1.V1oraSep  = V1oraSep;
268300140203             SaveDO1.V1oraPMda = V1oraPMda;
268400140203             SaveDO1.V1oraPMa  = V1oraPMa;
268500140203             SaveDO1.V1datN    = V1datN;
268600140203             SaveDO1.V1Drsr    = V1Drsr;
268700140203             SaveDO1.V1Dinr    = V1Dinr;
268800140203             SaveDO1.V1Dlor    = V1Dlor;
268900140203             SaveDO1.V1Dprr    = V1Dprr;
269000140203             SaveDO1.V1Dcar    = V1Dcar;
269100140203             SaveDO1.V1Dnar    = V1Dnar;
269200140203             SaveDO1.V1orr     = V1orr;
269300140203             SaveDO1.V1Dref    = V1Dref;
269400140203             SaveDO1.V1Cncl    = V1Cncl;
269500140203             SaveDO1.V1Cpkg    = V1Cpkg;
269600140203             SaveDO1.V1Cbnc    = V1Cbnc;
269700140203             SaveDO1.V1Cvlm    = V1Cvlm;
269800140203             SaveDO1.V1Cspi    = V1Cspi;
269900140203             SaveDO1.V1Dnot1   = V1Dnot1;
270000140203             SaveDO1.V1Dnot2   = V1Dnot2;
270100140203             SaveDO1.V1Cnt1    = V1Cnt1;
270200140203             SaveDO1.V1Cnt2    = V1Cnt2;
270300140203             SaveDO1.V1nra1    = V1nra1;
270400140203             SaveDO1.V1nra2    = V1nra2;
270500140203
270600140203         EndSl;
270700140203
270800140203       ENDSR;
270900131122
271000131122       //--------------------------------------------------------------
271100140203       //?Gestione tasto funzionale F6=Accetta premuto da DS1 o DO1.   ?
271200131122       //--------------------------------------------------------------
271300140121       BEGSR  sr_F06D01;
271400131205
271500131205         // -?Riaggancio del record del Contatto ad Autotrasportatore?
271600131205         //  ?da aggiornare (allocandolo)?
271700131205         clear  keyFICAU01;
271800140206         //k_CAUfgs    = S1Hfgs;
271900140206         //k_CAUpdr    = C1Cpdr;
272000140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
272100140206         k_CAUpdr    = FNLRX2ds.iX2aut;
272200131205         k_CAUdtOins = S1HdtOins;
272300131205         k_CAUtor    = S1Htor;
272400131205         k_CAUogg    = S1Hogg;
272500131205         chain  %kds( keyFICAU01 )  FICAU000;
272600140207
272700140207         // -?Rec. NON trovato: si torna alla ri-emissione della?
272800140207         //  ?videata (come NON fosse stato premuto F6)?
272900140207         If  Not %found(FICAU01L)  or  CAUatb <> *blank;
273000140207           ErrGenerico = *on;
273100140207           leavesr;
273200140207         EndIf;
273300131122
273400131205         // -?Richiamo del *pgm FIDNA3R = Scrittura R.A. cieca?
273500131205         //  ?(causale " 40") - vedi file FITGD00F+FITGN00F?
273600131205         exsr  sr_CallFIDNA3;
273700131205
273800131205         // -?Aggiornamento Telefonata?
273900131205         exsr  sr_UpdFICAU;
274000140121
274100140121         // -?Aggiornamento flag "ORM scaricato a PDA"?
274200140128         if  CAUtor = c_Ritiro;
274300140128            exsr  sr_UpdFNORG;
274400140128         endif;
274500131205
274600131205         // -?Impostazione parametro di ritorno?
274700131205         FNLRX2ds.oX2upd = *on;
274800131205
274900131205         // -?Ritorno al subfile?
275000131205         $Video = 'S1';
275100131205         $InzS01 = *on;
275200131122
275300131122       ENDSR;
275400140203
275500140203       //--------------------------------------------------------------
275600140203       //?Reperimento (nuovo) ultimo record per l'oggetto in esame.    ?
275700140203       //--------------------------------------------------------------
275800140203       BEGSR  sr_Ricarica_Ogg;
275900140203
276000140203         // -?Ricerca record relativo all'oggetto in esame con Data/Ora?
276100140203         //  ?inserimento aggiornata?
276200140206         //clear  keyFICAU02;      ?(già impostata nella subr. sr_InzDS1/DO1)?
276300140206         //k_CA2fgs    = CAUfgs;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276400140206         //k_CA2nfv    = CAUnfv;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276500140206         //k_CA2pdr    = CAUpdr;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276600140206         //k_CA2tor    = CAUtor;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276700140206         //k_CA2ogg    = CAUogg;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276800140207         ////k_CA2dtOins = *hival; ?(già impostata nella subr. sr_InzDS1/DO1)?
276900140206
277000140206         chain  %kds( keyFICAU02 : 5 )  FICAU002;
277100140203
277200140206         $RecUpdated = ( %found( FICAU02L ) );
277300140203
277400140203         // -?Trovato: si aggiornano i dati a video?
277500140203         //  ?(di cui verrà effettuata la riemissione)?
277600140203         If  $RecUpdated;
277700140206           k_CAUdtOins = CA2dtOins;
277800140207           chain(N)  %kds( keyFICAU01 )  FICAU000;
277900140206           S1HdtOins   = CA2dtOins;
278000140206           S01hmi   = %int( %subst( CA2dtOins : 9 : 4 ) );
278100140203           $InzS01  = *on;
278200140207           $InzD01  = *on;
278300140203         EndIf;
278400140203
278500140203       ENDSR;
278600131205
278700131205       //--------------------------------------------------------------
278800131205       //?Scrittura Richiesta Assistenza cieca (causale " 40")         ?
278900131205       //--------------------------------------------------------------
279000131205       BEGSR  sr_CallFIDNA3;
279100131205
279200131205         clear  FIDNA3ds;
279300131205         iA3mad = c_IA3mad;
279400131209         select;
279500131209           when  CAUtor = c_Consegna;
279600131209             iA3tor = c_Spedizione;
279700131209           when  CAUtor = c_Ritiro;
279800131209             iA3tor = c_ORM;
279900131209         endsl;
280000131205         iA3ogg = CAUogg;
280100140108         iA3no1 = V1nra1 + V1nra2;
280200131205         iA3no2 = 'AUT: '  + %editc( CAUpdr : 'X' ) +
280300131205                 ' DIST: ' + %editc( CAUnfv : 'X' );
280400131205         if  §CMDdes <> *blank;
280500131205           iA3no2 = %trimr(iA3no2) + ' ' + %trim(§CMDdes);
280600131205         endif;
280700131205         select;
280800131205           when  CAUdesVA <> *blank;
280900131205             //iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(CAUdesVA);
281000131205             if  §CMDdes = *blank;
281100131205               iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(CAUdesVA);
281200131205             else;
281300131205               %subst( iA3no2 : 46 ) = ' VARIAZ: ' + %trim(CAUdesVA);
281400131205             endif;
281500131205           when  wDesTVA  <> *blank;
281600131205             if  §CMDdes = *blank;
281700131205               iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(wDesTVA);
281800131205             else;
281900131205               %subst( iA3no2 : 46 ) = ' VARIAZ: ' + %trim(wDesTVA);
282000131205             endif;
282100131205         endsl;
282200131205         //iA3ara = *blank;
282300131205
282400140108         // -?Richiesta Accettata?
282500140121         //%subst( iA3no2 : %size(iA3no2) - 10 ) = ' (ESEGUITA)';
282600131205
282700131205         FIDNA3R ( kpjba : FIDNA3ds );
282800131205
282900131205       ENDSR;
283000131205
283100131205       //--------------------------------------------------------------
283200131205       //?Aggiornamento telefonata (file FICAU00F).                    ?
283300131205       //--------------------------------------------------------------
283400131205       BEGSR  sr_UpdFICAU;
283500131205
283600131205         // -?Reperimento orario corrente?
283700131205         wTimeStamp = %timestamp();
283800131205
283900131205         // -?Impostazione dei campi relativi all'"ESITO"?
284000131205         CAUflt    = 'T';
284100131205         CAUdtoTel = %subst( %char( %dec( wTimeStamp ) ) : 1 : 14 );
284200140110         CAUesito  = c_Eseguita;
284300131205         CAUpruTel = SDSjobUser;
284400131205         CAUterm   = SDSjobName;
284500131205         CAUesec   = 'T';
284600131205
284700131205
284800131205         // -?Aggiornamento rec. FICAU00F?
284900140203         //_______________
285000140203         update  FICAU000;
285100140203         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
285200131205
285300131205       ENDSR;
285400140121
285500140121       //--------------------------------------------------------------
285600140121       //?Aggiornamento O.R.M. (file FNORG00F).                        ?
285700140121       //--------------------------------------------------------------
285800140121       BEGSR  sr_UpdFNORG;
285900140121
286000140121         // -?Aggancia il record dell'Estensione Testata?
286100140121         clear  keyFNORG01;
286200140121         k_ORGpoe = ds_ORM.wPOE;
286300140121         k_ORGnsr = ds_ORM.wNSR;
286400140121         k_ORGnor = ds_ORM.wNOR;
286500140121         k_ORGnrv = ds_ORM.wNRV;
286600140121         chain  %kds( keyFNORG01 )  FNORG000;
286700140121
286800140121         // -?IMPOSSIBILE?che NON venga reperito il rec. da aggiornare?
286900140121         if  Not %found(FNORG01L);
287000140121           //clear  FNORG000;
287100140121           //CAUpoe = k_ORGpoe;
287200140121           //CAUnsr = k_ORGnsr;
287300140121           //CAUnor = k_ORGnor;
287400140121           //CAUnrv = k_ORGnrv;
287500140121           //CAUpor = .....;
287600140121           //--- oppure: ----------
287700140121           //*inH2 = *on;
287800140121           //$Fine = *on;
287900140121           //ErrGenerico = *on;
288000140121           //leavesr;
288100140121         endif;
288200140121
288300140121         // -?Aggiornamento flag "ORM scaricato a PDA"?
288400140123         dORG01 = ORGflo;
288500140121         select;
288600140121           // -?Inserito => Sì?
288700140121           when  CAUcmd = 'I';
288800140121             dORG01.§ORGpda = 'S';
288900140121           // -?Annullato => No?
289000140121           when  CAUcmd = 'A';
289100140121             clear  dORG01.§ORGpda;
289200140121         endsl;
289300140123         ORGflo = dORG01;
289400140121
289500140121         // -?Aggiornamento rec. FNORG00F?
289600140121         //if  %found(FNORG01L);
289700140121           //_______________
289800140121           update  FNORG000;
289900140121           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
290000140121         //endif;
290100140121
290200140121       ENDSR;
290300131030
290400131030       //--------------------------------------------------------------
290500131030       //?Operazioni finali.                                           ?
290600131030       //--------------------------------------------------------------
290700131030       BEGSR  sr_RoutEnd;
290800131120
290900131120         // -?Chiusura funzioni precedentemente aperte?
291000131120         cloTabella ( c_Tntbe );
291100131030
291200131120         // -?Restituzione eventuali parametri?
291300131121         kpjbu = FNLRX2ds;
291400131104
291500131120         // -?Chiusura *pgm?
291600131030         return;
291700131030
291800131030       ENDSR;
291900131030
292000131030      /end-free
292100131030
292200131120** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
292300131120Superato il numero massimo di telefonate visualizzabili                         1
292400131120Opzione errata                                                                  2
292500131205ERRORE: NON risulta più reperibile questa telefonata                            3
292600140206Attenzione: sono stati variati dei dati. VERIFICARE                             4
