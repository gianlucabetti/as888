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
036000131121     d $RecOK          s               n   inz
036100131128     d $UltContPrec    s               n   inz
036200140203     d $RecUpdated     s               n   inz
036300131122
036400131122       // -?Indici di schiera?
036500131122     d xx              s              3  0 inz
036600131125     d yy              s              3  0 inz
036700131030
036800131030       // -?Variabili per la gestione del video?
036900131125     d $Video          s              2    inz('S1')
037000131104     d w_SflPag        s              3  0 inz
037100131030     d wPag            s              4  0 inz
037200131030     d wRig            s              3  0 inz
037300140108     d wOrdSfl         s              1  0 inz
037400131030     d S01nrr          s                   like(C1RcdNbr) inz
037500131030     d SavS01csr       s                   like(C1RcdNbr) inz
037600131212     d SavMAXnrr       s                   like(C1RcdNbr) inz
037700131030
037800131030       // -?Variabili di comodo?
037900131121     d wDate_EUR       s               d   datfmt(*eur)  inz(*loval)
038000131205     d wTimeStamp      s               z   inz(*loval)
038100131127     d wTipTVA         s              1    inz
038200131210     d w0112           s             11  2 inz
038300140108     d wDesTVA         s                   like(V1Dvar)   inz
038400131204
038500131204       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
038600131204
038700131204       //--------------------------------------------------------------
038800131204       // -?Constants?
038900131204       //--------------------------------------------------------------
039000131204       // -?MaxKey - Maximum number of key fields allowed by this program?
039100131204     d MaxKey          c                   const(2)
039200131204       // -?Sort order:?
039300131204       //  ?1 = Sort in an ascending sequence?
039400131204       //  ?2 = Sort in a descending sequence?
039500131204     d Ascendente      c                   const(1)
039600131204     d Discendente     c                   const(2)
039700131204       // -?Key data type:?
039800131204       //  ? 0 = Signed binary?
039900131204       //  ? 2 = Signed zoned decimal?
040000131204       //  ? 3 = Signed packed decimal?
040100131204       //  ? 6 = Character with no national language sort sequence applied,?
040200131204       //  ?     if specified?
040300131204       //  ? 7 = Unsigned packed decimal?
040400131204       //  ?     All numbers will have the sign forced positive ('F'X).?
040500131204       //  ? 8 = Unsigned zoned decimal?
040600131204       //  ?     All numbers will have the sign forced positive ('F'X).?
040700131204       //  ? 9 = Unsigned binary?
040800131204       //  ?14 = Date in form of DD/MM/YY?
040900131204       //  ?15 = Date in form of DD.MM.YYYY?
041000131204     d Numero          c                   const(2)
041100131204     d Carattere       c                   const(6)
041200131204       //
041300131204     d Put             c                   const(1)
041400131204     d EndPut          c                   const(2)
041500131204     d Get             c                   const(3)
041600131204
041700131204       //--------------------------------------------------------------
041800131204       // -?Data Structures?
041900131204       //--------------------------------------------------------------
042000131204       //  ?SflRcd     - The entire subfile record to pass to the sort?
042100131204     d SflRcd          ds                  inz
042200131204     d   S1Hfgs                            inz
042300131204     d   S1HdtOins                         inz
042400131204     d   S1Htor                            inz
042500131204     d   S1Hogg                            inz
042600140110     d   S1Hcmd                            inz
042700131204     d   S01opz                            inz
042800140110     d   S01tor                            inz
042900131204     d   S01cmd                            inz
043000131204     d   S01var                            inz
043100131204     d   S01rsc                            inz
043200131206     d   S01hmi                            inz
043300131204     d   S01ogg                            inz
043400131204     d   Selected                     1a   inz
043500131204       //  ?QLGSCB     - The sort request block for the QLGSORT API?
043600131204      /copy QSYSINC/QRPGLESRC,QLGSORT
043700131204       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
043800131204     d QLGKL                         16    dim(MaxKey)
043900131204     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
044000131204     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
044100131204     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
044200131204     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
044300131204       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
044400131204      /copy QSYSINC/QRPGLESRC,QLGSRTIO
044500131204       //  ?QUSEC      - Error structure for the QLGSORT API?
044600131204      /copy QSYSINC/QRPGLESRC,QUSEC
044700131204
044800131204       //--------------------------------------------------------------
044900131204       // -?Standalone fields?
045000131204       //--------------------------------------------------------------
045100131204     d NotUsed         s             16a   inz
045200131204       //  ?ReturnSize - Size of list returned by sort API?
045300131204     d ReturnSize      s              9b 0 inz
045400131204       //  ?SizeList   - Size of list?
045500131204     d SizeList        s              9b 0 inz
045600131204       //  ?Nrr        - Relative record number for subfile?
045700131204     d Nrr             s              5i 0 inz
045800131204     d RrnLast         s              5i 0 inz
045900131030
046000131030       //--------------------------------------------------------------
046100131030       //?Definizione prototipi procedure.                             ?
046200131030       //--------------------------------------------------------------
046300131030
046400131030       // -?Reperimento dati utente?
046500131030     d TIBS34ds      e ds                  inz
046600131030      /copy gaitrasrc/srcProtoPR,TIBS34R
046700131030
046800131120       // -?Reperimento dati tabelle?
046900131120      /copy gaitrasrc/srcProtoPI,TRULTAB
047000131120      /copy gaitrasrc/srcProtoPR,TRULTAB
047100131120
047200131120       // -?Controllo/Decodifica cliente?
047300131120      /copy gaitrasrc/srcProtoPI,TIBS69R
047400131120      /copy gaitrasrc/srcProtoPR,TIBS69R
047500131121
047600131121       // -?Interrogazione Bolle Arrivi?
047700131121      /copy gaitrasrc/srcProtoPI,FNLR36R
047800131121      /copy gaitrasrc/srcProtoPR,FNLR36R
047900131128
048000131128       // -?Interrogazione Particolarità?
048100131128     d DS7PQRS       e ds                  inz   qualified
048200131128     d   DS7ric      e                     inz('S')
048300131128     d   DS7ges      e                     inz('V')
048400131128       // -?       "             "       Giacenza?
048500131128     d trtb69r         pr                  extpgm('TRTB69R')
048600131128     d   kpjba                             likeds(KPJBA)
048700131128       // -?       "             "       Consegna?
048800131128     d trtb70r         pr                  extpgm('TRTB70R')
048900131128     d   kpjba                             likeds(KPJBA)
049000131128       // -?       "             "       Varie?
049100131128     d trtb73r         pr                  extpgm('TRTB73R')
049200131128     d   kpjba                             likeds(KPJBA)
049300131122
049400131122       // -?Interrogazione O.R.M.?
049500131122     d Parm01          ds                  inz   qualified
049600131125     d   ppoe                         3  0 inz
049700131125     d   pnor                         7  0 inz
049800131125     d   pnsr                         2  0 inz
049900131125     d   pnrv                         2  0 inz
050000131125     d   psce                         1    inz
050100131125     d   pfgs                         3  0 inz
050200131125     d   ppor                         3  0 inz
050300131125     d   pdtr                         8  0 inz
050400131125     d   pmdb                        10    inz
050500131125     d   pprb                        10    inz
050600131125     d   pdts                         8  0 inz
050700131125     d   prmp                         1    inz
050800131125     d   pbrc                         1    inz
050900131125     d   pref                         2    inz
051000131125     d   pmio                         1    inz
051100131122     d FIDNA1ds      e ds                  inz   qualified
051200131122     d fior07r         pr                  extpgm('FIOR07R')
051300131122     d   kpjba                             likeds(KPJBA)
051400131122     d   fidnA1ds                          likeds(FIDNA1ds)
051500140130
051600140130       // -?Monitor attività autotrasportatore con PDA?
051700140130     d FNLVP0ds      e ds                  inz
051800140130      /copy gaitrasrc/srcProtoPR,FNLVP0R2
051900140130
052000140130       // -?Dettaglio Interrogazione DISTINTE?
052100140130     d FIDG311ds     e ds                  inz
052200140130      /copy gaitrasrc/srcProtoPR,FIDG311R
052300131205
052400131205       // -?Scrittura Richiesta Assistenza?
052500131205     d FIDNA3ds      e ds                  inz
052600131205     d fidnA3r         pr                  extpgm('FIDNA3R')
052700131205     d   kpjba                             likeds(KPJBA)
052800131205     d   fidnA3ds                          likeds(FIDNA3ds)
052900131204
053000131204       // -?Ordinamento subfile?
053100131204      /copy gaitrasrc/srcProtoPR,QLGSRTIO
053200131030
053300131030       //--------------------------------------------------------------
053400131030       //?Definizione key-list.                                        ?
053500131030       //--------------------------------------------------------------
053600131030
053700131120       // -?File FICAU01L?
053800131120     d keyFICAU01    e ds                  extname(FICAU01L : *key)
053900131126     d                                     prefix(k_)    inz
054000140206       // -?File FICAU02L?
054100140206     d keyFICAU02    e ds                  extname(FICAU02L : *key)
054200140206     d                                     prefix(k_CA2 : 3)   inz
054300131120
054400131120       // -?File FIAPD01L?
054500131120     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
054600131126     d                                     prefix(k_)    inz
054700131121
054800131121       // -?File FIAPD41L?
054900140131     d keyFIAPD41    e ds                  extname(FIAPD41L : *key)
055000140131     d                                     prefix(k_)    inz
055100131030
055200131030       // -?File FNARB01L?
055300131030     d keyFNARB01    e ds                  extname(FNARB01L : *key)
055400131126     d                                     prefix(k_)    inz
055500131030
055600131030       // -?File FIDST01L?
055700131204     d keyFIDST01    e ds                  extname(FIDST01L : *key)
055800131204     d                                     prefix(k_)    inz
055900131126
056000131126       // -?File FIARN12L?
056100131126     d keyFIARN12    e ds                  extname(FIARN12L : *key)
056200131126     d                                     prefix(k_)    inz
056300131030
056400131122       // -?File FIAR401L?
056500131122     d keyFIAR401    e ds                  extname(FIAR401L : *key)
056600131126     d                                     prefix(k_)    inz
056700131210       // -?File FIAR404L?
056800131210     d keyFIAR404    e ds                  extname(FIAR404L : *key)
056900131210     d                                     prefix(k_AR44:3)    inz
057000131122
057100131122       // -?File FIAR501L?
057200131122     d keyFIAR501    e ds                  extname(FIAR501L : *key)
057300131126     d                                     prefix(k_)    inz
057400131122
057500131122       // -?File FIAR601L?
057600131122     d keyFIAR601    e ds                  extname(FIAR601L : *key)
057700131126     d                                     prefix(k_)    inz
057800131122
057900131122       // -?File FIAR901L?
058000131122     d keyFIAR901    e ds                  extname(FIAR901L : *key)
058100131126     d                                     prefix(k_)    inz
058200131120
058300131120       // -?File FNORM01L?
058400131122     d keyFNORM01    e ds                  extname(FNORM01L : *key)
058500131126     d                                     prefix(k_)    inz
058600140121
058700140121       // -?File FNORT01L?
058800140121     d keyFNORT01    e ds                  extname(FNORT01L : *key)
058900140121     d                                     prefix(k_)    inz
059000140121
059100140121       // -?File FNORG01L?
059200140121     d keyFNORG01    e ds                  extname(FNORG01L : *key)
059300140121     d                                     prefix(k_)    inz
059400131211
059500131211       // -?File FNORE01L?
059600131211     d keyFNORE01    e ds                  extname(FNORE01L : *key)
059700131211     d                                     prefix(k_)    inz
059800131030
059900131030       //--------------------------------------------------------------
060000131030       //?Riepilogo indicatori utilizzati.                             ?
060100131030       //--------------------------------------------------------------
060200131030
060300131030
060400131030       //--------------------------------------------------------------
060500131030       //?M A I N - L I N E                                            ?
060600131030       //--------------------------------------------------------------
060700131030
060800131030     c     *Entry        plist
060900131030     c                   parm                    KPJBA
061000131030
061100131030      /free
061200131030
061300131030       // -?Operazioni iniziali?
061400131030       exsr sr_RoutInz;
061500131030
061600131030       // -?Ciclo di gestione del file video?
061700131030       DoW  $Fine = *off;
061800131030
061900131030         select;
062000131030
062100131120           // -?Gestione subfile S01?
062200131030           when $Video = 'S1';
062300131030             exsr sr_GesS01;
062400131120
062500131120           // -?Gestione videata DS1 (singola spedizione)?
062600140207           //  ?- gestita all'interno della subr. sr_GesS01 -?
062700140207           //when  $Video = 'DS';
062800140207           //  exsr sr_GesD01;
062900131120
063000131120           // -?Gestione videata DO1 (singolo O.R.M.)?
063100140207           //  ?- gestita all'interno della subr. sr_GesS01 -?
063200140207           //when  $Video = 'DO';
063300140207           //  exsr sr_GesD01;
063400131030
063500131030           // -?? ? ??
063600131030           other;
063700131030             $Fine = *on;
063800131030
063900131030         endsl;
064000131030
064100131030       EndDo;
064200131030
064300131030       // -?Operazioni finali?
064400131030       exsr sr_RoutEnd;
064500131030
064600131030       //--------------------------------------------------------------
064700131030       //?Operazioni iniziali.                                         ?
064800131030       //--------------------------------------------------------------
064900131030       BEGSR  sr_RoutInz;
065000131030
065100131030         // -?Impostazione chiusura?
065200131030         *inLR = *on;
065300131121
065400131121         // -?Ricezione parametri ed impostazione parametri di output?
065500131121         FNLRX2ds = KPJBU;
065600131122         FNLRX2ds.oX2upd = *off;
065700131125         clear  FNLRX2ds.oX2fxx;
065800131122         FNLRX2ds.oX2err = *off;
065900131125         clear  FNLRX2ds.oX2msg;
066000131030
066100131030         // -?Reperimento dati job?
066200131030         exsr  sr_DatiJob;
066300131104
066400131104         // -?Impostazione nome programma a video?
066500131104         V1Tpgm = SDSpgmName;
066600131030
066700131030         // -?Pulizia / impostazione iniziale dei campi chiave?
066800131120         clear  keyFICAU01;
066900140206         clear  keyFICAU02;
067000131120         clear  keyFIAPD01;
067100140131         clear  keyFIAPD41;
067200131104         clear  keyFNARB01;
067300131125         clear  keyFIAR501;
067400131122         clear  keyFIAR401;
067500131126         clear  keyFIARN12;
067600131204         clear  keyFIDST01;
067700131122         clear  keyFNORM01;
067800131211         clear  keyFNORE01;
067900131210         clear  keyFIAR404;
068000131121         k_APDtip   = 'A';
068100140131         k_APD4tipA = 'A';
068200140131         k_APD4trd  = 'TEL';
068300131121         k_AR5trd   = 'GEN';
068400131126         k_ARNcdn   = 'LDV';
068500131126         k_ARNgst   = 'S';
068600131204         k_DSTnpg   = 4;
068700131211         k_OREtrc   = 'O ';
068800131125
068900131125         // -?Caricamento schiera tab. "TCV" = Tipologiche Causali Variazioni bolle?
069000131125         setll  ('TCV')  TNTBE000;
069100131125         reade  ('TCV')  TNTBE000;
069200131125         DoW  Not  %eof(TNTBE01L);
069300131125           xx += 1;
069400131125           sk_TCV(xx)  = TBEke1;
069500131125           sk_TCVd(xx) = TBEuni;
069600131125           reade  ('TCV')  TNTBE000;
069700131125         EndDo;
069800131030
069900131030       ENDSR;
070000131030
070100131030       //--------------------------------------------------------------
070200131030       //?Reperimento Dati del job (Utente/Operativi).                 ?
070300131030       //--------------------------------------------------------------
070400131030       BEGSR  sr_DatiJob;
070500131030
070600131030         in(e) §AzUte;
070700131030         if NOT %error;
070800131030           in(e) §DatiUte;
070900131030         endif;
071000131030         if %error or RSut = *blank;
071100131030           tibs34r ( tibs34ds );
071200131030           in §AzUte;
071300131030           in §DatiUte;
071400131030         endif;
071500131030
071600131030       ENDSR;
071700131030
071800131030       //--------------------------------------------------------------
071900131030       //?Gestione subfile S01.                                        ?
072000131030       //--------------------------------------------------------------
072100131030       BEGSR  sr_GesS01;
072200131030
072300131030         // -?Inizializzazione videata?
072400131030         if  $InzS01 = *on;
072500131030           exsr  sr_InzS01;
072600131030           $InzS01 = *off;
072700131030         endif;
072800140131
072900140131         // -?(Dis)Abilitazione Tasti Funzionali?
073000140131         //  E' meglio lasciarli qui, perchè F1 ri-(dis)abilitato?
073100140207         //  nella subr. "sr_GesD01": se viene (dis)abilitato là, qui?
073200140131         //  non verrebbe più re-impostato.?
073300140131         F1Attivo  = (S01nrr > 1);
073400140131         F9Attivo  = (S01nrr > *zero);
073500140131         F11Attivo = (S01nrr > 1);
073600131030
073700131030         // -?Emissione Testata & Piede?
073800131120         write  LRX2T01;
073900131120         write  LRX2P01;
074000131030
074100131030         // -?Posizionamento cursore?
074200131030         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
074300131030         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
074400131030         //  ?si visualizza la pagina che vedeva l'utente quando ha?
074500131030         //  ?premuto l'ultimo tasto.?
074600131030         if  C1CsrRrn > *zero;
074700131030           C1RcdNbr = C1CsrRrn;
074800131030         else;
074900131030           C1RcdNbr = 1;
075000131120           write  LRX2S00;             //?(no rec.)?
075100131030         endif;
075200131030
075300131030         // -?Emissione videata?
075400131120         exfmt  LRX2C01;
075500131030
075600131030         reset  ErrGenerico;
075700131030         reset  ErrMessage;
075800131030         clear  V1Dmsg;
075900131030
076000131030         Select;
076100131204
076200131204           // -?F1=Selezione Totale?
076300131204           When  dsp_aid = c_F01;
076400131204             exsr  sr_F01S01;
076500131030
076600131030           // -?F3=Fine?
076700131030           When  dsp_aid = c_F03;
076800131122             FNLRX2ds.oX2fxx = '03';
076900131030             $Fine = *on;
077000140131
077100140131           // -?F4=Distinta?
077200140131           When  dsp_aid = c_F04;
077300140131             exsr  sr_F04S01;
077400131030
077500131030           // -?F5=Rivisualizza?
077600131030           When  dsp_aid = c_F05;
077700131030             $InzS01 = *on;
077800131205
077900131205           // -?F11=Ordinamento Sfl?
078000131205           When  dsp_aid = c_F11;
078100131205             exsr  sr_F11S01;
078200131104
078300131104           // -?F12=Ritorno?
078400131104           When  dsp_aid = c_F12;
078500131122             FNLRX2ds.oX2fxx = '12';
078600131104             $Video = 'D1';
078700131104
078800131104           // -?Roll-Up?
078900131120           When  dsp_aid = c_RollUp;
079000131212             if  SavMAXnrr >= c_MaxRec  and  Not  $EoF;
079100131104               ErrGenerico = *on;
079200131104               ErrMessage  = *on;
079300131120               V1Dmsg = sk_Msg(01);
079400131204             endif;
079500131030
079600131030           // -?SubFile vuoto?
079700131212           When  SavMAXnrr = *zero;
079800131030
079900131030           // -?Invio?
080000131030           Other;
080100131030             // -?Gestione opzioni?
080200131030             exsr  sr_OpzS01;
080300131030             if  ErrGenerico;
080400131030               leavesr;
080500131030             endif;
080600131030
080700131204         EndSl;
080800131030
080900131030       ENDSR;
081000131030
081100131030       //--------------------------------------------------------------
081200131030       //?Inizializzazione subfile S01.                                ?
081300131030       //--------------------------------------------------------------
081400131030       BEGSR  sr_InzS01;
081500131030
081600131030         // -?Spegnimento degli indicatori di errore?
081700131030         %subst(IndDspF : 50) = *off;
081800131125
081900131125         // -?Decodifica autotrasportatore?
082000131205         k_APDpdr = FNLRX2ds.iX2aut;
082100131125         chain  %kds( keyFIAPD01 )  FIAPD000;
082200131125         if  Not %found(FIAPD01L)  or  APDatb <> *blank;
082300131125           clear  APDrsc;
082400131125         endif;
082500131125
082600140131         // -?Reperimento suo n° telefonico?
082700140131         k_APD4pdr = FNLRX2ds.iX2aut;
082800140131         chain  %kds( keyFIAPD41 )  FIAPD400;
082900140131         if  %found(FIAPD41L)  and  APD4atb = *blank;
083000140131           dAPD4tel = APD4uni;
083100140131         endif;
083200131205
083300131205         // -?Reperimento dati della Distinta Consegna?
083400131205         k_DSTfgs = FNLRX2ds.iX2fgs;
083500131205         k_DSTnfv = FNLRX2ds.iX2dst;
083600131205         chain  %kds( keyFIDST01 )  FIDST000;
083700131205         if  Not  %found(FIDST01L);
083800131205           clear  DSTpda;
083900140130           clear  DSTflr;
084000131205         endif;
084100140130
084200140130         dDSTflr = DSTflr;
084300131125
084400131125         // -?Impostazione campi di "testata"?
084500131205         C1Cpdr   = FNLRX2ds.iX2aut;
084600131125         C1Dpdr   = APDrsc;
084700140131         C1TelPdr = dAPD4tel.§APD4tel;
084800140131         //select;
084900140131         //  when  DSTpda = 'O';
085000140131         //    C1Dpda = DSTpda + '=O.R.M.   ';
085100140131         //  when  DSTpda = 'C';
085200140131         //    C1Dpda = DSTpda + '=Consegne ';
085300140131         //  when  DSTpda = 'E';
085400140131         //    C1Dpda = DSTpda + '=ORM+Cons.';
085500140131         //  when  DSTpda = 'N';
085600140131         //    C1Dpda = DSTpda + '=NO       ';
085700140131         //endsl;
085800140131         if  DSTpda = 'N';
085900140131           C1Dpda = 'NO';
086000140131         else;
086100140131           C1Dpda = DSTpda;
086200140131         endif;
086300140131         //select;
086400140131         //  when  dDSTflr.§DSTtstPda = *blank;
086500140131         //  when  dDSTflr.§DSTtstPda = 'O';
086600140131         //    C1Dtst = 'Test O.R.M.   ';
086700140131         //  when  dDSTflr.§DSTtstPda = 'C';
086800140131         //    C1Dtst = 'Test Consegne ';
086900140131         //  when  dDSTflr.§DSTtstPda = 'E';
087000140131         //    C1Dtst = 'Test ORM+Cons.';
087100140131         //endsl;
087200140203         C1Dtst = %xlate( c_Up : c_Lo : dDSTflr.§DSTtstPda );
087300131030
087400131030         // -?Pulizia subfile?
087500131030         SflDsp_N    = *on;
087600131030         SflDspCtl_N = *on;
087700131120         write  LRX2C01;
087800131030         SflDspCtl_N = *off;
087900131030         SflEnd      = *off;
088000131030
088100131104         clear  w_SflPag;
088200131030         clear  C1RcdNbr;
088300131030         clear  C1CsrRrn;
088400131212         clear  SavMAXnrr;
088500131030         clear  S01nrr;
088600131030         clear  V1Dmsg;
088700131030         ErrMessage  = *off;
088800131030         ErrGenerico = *off;
088900131030
089000131104         // -?Caricamento 1ª pagina dei dati nel subfile?
089100131121         clear  keyFICAU01;
089200131121         k_CAUfgs = FNLRX2ds.iX2fgs;
089300131205         k_CAUpdr = FNLRX2ds.iX2aut;
089400131205         //k_CAUnfv = FNLRX2ds.iX2dst;       ?(NON in chiave)?
089500131121         setll  %kds(keyFICAU01 : 2)  FICAU000;
089600131126         exsr  sr_ReadFICAU;
089700131204         exsr  sr_CaricaS01;
089800131030
089900131030         // -?Impaginazione subfile?
090000131030         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
090100131212         if S01nrr > *zero;
090200131030           C1RcdNbr  = 1;
090300131030           C1CsrRrn  = 1;
090400131030         else;
090500131104           clear  C1RcdNbr;
090600131030         endif;
090700131204
090800131204         // -?Ordinamento subfile?
090900131212         if  S01nrr > *zero;
091000131204           exsr  sr_SortSfl;
091100131204         endif;
091200131204
091300131204         // -?Eventuale emissione *msg di superamento limite max di rec.?
091400131204         //  ?visualizzabili nel subfile?
091500131212         if  S01nrr >= c_MaxRec  and  Not $EoF;
091600131204           ErrGenerico = *on;
091700131204           ErrMessage  = *on;
091800131204           V1Dmsg = sk_Msg(01);
091900131204         endif;
092000131030
092100131030       ENDSR;
092200131030
092300131030       //--------------------------------------------------------------
092400131204       //?Caricamento completo del subfile S01                         ?
092500131030       //--------------------------------------------------------------
092600131204       BEGSR  sr_CaricaS01;
092700131104
092800131204         clear  S01nrr;
092900131104         SflNxtChg = *off;
093000131030
093100131204         // -?Ciclo di caricamento completo di dati nel subfile?
093200131204         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
093300131204         //  ?l'eventuale ordinamento)?
093400131204         DoW  S01nrr < c_MaxRec  and  Not $EoF;
093500131122
093600131126           clear  LRX2S01;
093700131126
093800131126           // ·?Reperimento dati Comando da impartire al PDA o all'AUT?
093900131126           If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
094000131127             clear  dCMD;
094100131126             if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
094200131126                              *omit : *omit :
094300131126                              *omit : *omit :
094400131126                              *omit : *omit : *omit : *omit :
094500131126                              *omit : *omit : *omit : *omit :
094600131126                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
094700131126                              = *zero      AND
094800131126                   ds_TNTBE.TBEatb = *blank;
094900131127               dCMD = ds_TNTBE.TBEuni;
095000131126             else;
095100131204               §CMDdes  = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ';
095200131126             endif;
095300131126           Else;
095400131204             §CMDdes  = CAUdesCmd;
095500131126           EndIf;
095600131126
095700131126           // ·?Reperimento dati Tipologiche Causali Variazioni bolle?
095800140131           If  CAUcmd <> c_Cmd_Inserito;
095900140131             If  CAUdesVa = *blank;
096000140131               wTipTVA = 'B';
096100140213               ds_TVA  = CAUtva;
096200140131               exsr  sr_GetDesTCV;
096300140131               S01var  = wDesTVA;
096400140131             Else;
096500140131               S01var  = CAUdesVa;
096600140131             EndIf;
096700140131           EndIf;
096800131126
096900131204
097000131205           Select;
097100131204             // ·?Reperimento dati Spedizione?
097200131205             When  CAUtor = c_Consegna;
097300131126               exsr  sr_GetSped;
097400131127               //S01rsc = ARBrsd + AR4not;
097500131127               S01rsc = ARBrsd;
097600131204             // ·?Reperimento dati O.R.M.?
097700131205             When  CAUtor = c_Ritiro;
097800131126               exsr  sr_GetORM;
097900131126               S01rsc = ORMrsr;
098000131205           EndSl;
098100131204
098200131204           // ·?Campi hidden?
098300131204           S1Hfgs    = CAUfgs;
098400131204           S1HdtOins = CAUdtOins;
098500131204           S1Htor    = CAUtor;
098600131204           S1Hogg    = CAUogg;
098700140206           //S1Hcmd    = §CMDdes;
098800140206           S1Hcmd    = CAUdesCmd;
098900131204           // ·?Campi di Output?
099000131204           S01tor    = CAUtor;
099100140206           //S01cmd    = §CMDdes;
099200140206           S01cmd    = CAUdesCmd;
099300131205           //S01var    = CAUdesVa;    ?(già "impostato" - o costruito)?
099400131206           S01hmi    = %int( %subst( CAUdtOins : 9 : 4 ) );
099500131204           select;
099600131204             when  CAUtor = c_Consegna;
099700131204               S01ogg = 'Spedizione: ' + %editc( ds_Spedizione.wLNP : 'X' ) +
099800131204                                   ' ' + %editc( ds_Spedizione.wNRS : 'Z' ) +
099900131204                                   ' ' + %editc( ds_Spedizione.wNSP : '3' ) +
100000131204                                  '  ' + %editc( ds_Spedizione.wAAS : 'Z' );
100100131204             when  CAUtor = c_Ritiro;
100200131204               S01ogg = 'O.R.M.: ' + %editc( ds_ORM.wPOE : 'X' ) +
100300131204                               ' ' + %editc( ds_ORM.wNSR : 'Z' ) +
100400131204                               ' ' + %editc( ds_ORM.wNOR : '3' ) +
100500131204                               ' ' + %editc( ds_ORM.wNRV : 'Z' );
100600131204           endsl;
100700131126
100800131126           // -?Scrittura record nel subfile?
100900131206           Ord_x_OraIns = (wOrdSfl = 1);
101000131126           S01nrr += 1;
101100131126           write  LRX2S01;
101200131030
101300131104           // -?Lettura record successivo?
101400131126           exsr  sr_ReadFICAU;
101500131030
101600131030         EndDo;
101700131204
101800131030
101900131212         // -?Memorizzazione n° rec. caricati nel subfile?
102000131212         //  ?(servirà all'ordinamento)?
102100131212         SavMAXnrr = S01nrr;
102200131212
102300131030         // -?Visualizzazione del SFL (se ci sono dati)?
102400131030         SflDsp_N = (S01nrr <= *zero);
102500131030
102600131030         // -?Attivazione del SFLEND?
102700131030         SflEnd = ( $EoF );
102800131030
102900131204         // -?Posizionamento cursore al 1° rcd del subfile?
103000131030         if  S01nrr > *zero;
103100131204           C1RcdNbr = 1;
103200131030         else;
103300131030           clear  C1RcdNbr;
103400131030         endif;
103500131030
103600131030         C1CsrRrn = C1RcdNbr;
103700131030
103800131030       ENDSR;
103900131204
104000131204       //--------------------------------------------------------------
104100131204       //?Gestione tasto funzionale F1 da videata C01 / S01            ?
104200131204       //?F1-Selezione Totale                                          ?
104300131204       //--------------------------------------------------------------
104400131204       BEGSR  sr_F01S01;
104500131204
104600131212         RrnLast  = SavMAXnrr;
104700131204
104800131204         For  S01nrr = 1  To  RrnLast;
104900131204
105000131204           chain  S01nrr  LRX2S01;
105100131204           if  Not %found(FNLRX2D);
105200131204             iter;
105300131204           endif;
105400131204
105500131204           S01opz = 1;
105600131204           SflNxtChg = *on;
105700131206           Ord_x_OraIns = (wOrdSfl = 1);
105800131204           update  LRX2S01;
105900131204
106000131204         EndFor;
106100131204
106200131204       ENDSR;
106300140131
106400140131       //--------------------------------------------------------------
106500140131       //?Gestione tasto funzionale F4 da videata C01 / S01            ?
106600140131       //?F4-Spedizioni nella distinta                                ?
106700140131       //--------------------------------------------------------------
106800140131       BEGSR  sr_F04S01;
106900140131
107000140131         // -?Visualizzazione fasi della distinta se PDA?
107100140131         //  ?(questo *pgm dovrebbe "durare" fino ad agosto 2014...)?
107200140131         If  DSTpda = 'E'  and  dDSTflr.§DSTtstPda = *blank;
107300140131
107400140131           clear  KPJBU;
107500140131           clear  FNLVP0ds;
107600140131           LVP0fgs = FNLRX2ds.iX2fgs;
107700140131           LVP0ndc = FNLRX2ds.iX2dst;
107800140131           //LVP0aut = FNLRX2ds.iX2daut;
107900140131           LVP0aut = DSTpdr;
108000140131           FNLVP0R2( KPJBA : FNLVP0ds );
108100140131
108200140131         // -?Visualizzazione Dettaglio Distinta?
108300140131         Else;
108400140131
108500140131           clear  FIDG311ds;
108600140131           F31fgs     = FNLRX2ds.iX2fgs;
108700140131           F31npg     = DSTnpg;
108800140131           F31numDis  = FNLRX2ds.iX2dst;
108900140131           F31DatDis  = DSTdfv;
109000140131           //F31DatiVis = *blank;
109100140131           F31priDati = c_Consegna;
109200140131
109300140131           kpjbu = FIDG311ds;
109400140131           FIDG311R ( KPJBA );
109500140131
109600140131           FIDG311ds = KPJBU;
109700140131           if  F31tFunz = '03';
109800140131             $Fine = *on;
109900140131             FNLRX2ds.oX2fxx = F31tFunz;
110000140131           endif;
110100140131
110200140131         EndIf;
110300140131
110400140131       ENDSR;
110500131204
110600131204       //--------------------------------------------------------------
110700131205       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
110800131205       //?F11-Cambia ordinamento sfl                                   ?
110900131204       //--------------------------------------------------------------
111000131205       BEGSR  sr_F11S01;
111100131204
111200131204         if  wOrdSfl < 1;
111300131204           wOrdSfl += 1;
111400131204         else;
111500131204           clear  wOrdSfl;
111600131204         endif;
111700131204
111800131204         // -?Se subfile vuoto: nessun record da ordinare?
111900131204         //  ?Altrimenti: basta riordinarlo?
112000131212         if  SavMAXnrr > *zero;
112100131204           exsr  sr_SortSfl;
112200131204         endif;
112300131204
112400131204       ENDSR;
112500131204
112600131204       //--------------------------------------------------------------
112700131204       //?Ordinamento subfile                                          ?
112800131204       //?  This subroutine sorts the subfile records.                 ?
112900131204       //--------------------------------------------------------------
113000131204       BEGSR  sr_SortSfl;
113100131204
113200131212         RrnLast  = SavMAXnrr;
113300131204
113400131204         C1RcdNbr = 1;
113500131204         clear  C1CsrRrn;
113600131204         clear  S01nrr;
113700131204         clear  V1Dmsg;
113800131204         ErrMessage  = *off;
113900131204         SflNxtChg   = *off;
114000131204         ErrGenerico = *off;
114100131204         %subst(IndDspF : 50) = *off;
114200131204
114300131204         //?___________________________________________________________?
114400131204         //?Initialize the key fields to sort on.?
114500131204         //?There is one extra field not in the subfile -- Selected --?
114600131204         //?that is added to the record. This field is used to place?
114700131204         //?selected records in front of nonselected records.?
114800131204         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
114900131204
115000131204         clear  QLGKL;
115100131204         clear  QLGSKL;
115200131204         clear  QLGSCB;
115300131204         clear  QLGSCB00;
115400131204
115500131204         // -?Gestione della scelta ordinamento:?
115600131204         Select;
115700131204
115800131204           //  ?Ordinamento per Comando (desc)?
115900131204           When  wOrdSfl = *zero;
116000131204             // -?2 campi chiave?
116100131204             QLGNBRK  = 2;
116200131204             // -?1° campo: Descrizione Comando (S1Hcmd)?
116300131204             //            ?a posizone   39, 20 byte, char, descending.?
116400131204             QLGSP    = 39;
116500131204             QLGSS    = %size(S1Hcmd);
116600131204             QLGDT    = Carattere;
116700131204             QLGSO    = Discendente;
116800131204             QLGKL(1) = QLGSKL;
116900131204             // -?2° campo: Data Inserimento (S1HdtOins)?
117000131204             //            ?a posizone    4, 14 byte, char, ascending.?
117100131204             QLGSP    = 4;
117200131204             QLGSS    = %size(S1HdtOins);
117300131204             QLGDT    = Carattere;
117400131204             QLGSO    = Ascendente;
117500131204             QLGKL(2) = QLGSKL;
117600131204
117700131205           //  ?Ordinamento per Data/Ora Inserimento?
117800131204           When  wOrdSfl = 1;
117900131204             // -?1 campo chiave?
118000131204             QLGNBRK  = 1;
118100131204             // -?1° campo: Data Inserimento (S1HdtOins)?
118200131204             //            ?a posizone    4, 14 byte, char, ascending.?
118300131204             QLGSP    = 4;
118400131204             QLGSS    = %size(S1HdtOins);
118500131204             QLGDT    = Carattere;
118600131204             QLGSO    = Ascendente;
118700131204             QLGKL(1) = QLGSKL;
118800131204
118900131204         EndSl;
119000131204
119100131204         // -?Load other sort parameters.?
119200131204         QLGLB   = 80 + 16 * MaxKey;
119300131204         QLGRL   = %size(SflRcd) - 1;
119400131204         QLGRT   = 8;
119500131204         QLGOKL  = 80;
119600131204         QLGLKE  = 16;
119700131204         QLGLSS  = 290;
119800131204         // -?Initialize Sort I/O API fields.?
119900131204         QLGRL00 = QLGRL;
120000131204         QLGRC00 = 1;
120100131204         clear  QUSEI;
120200131204         QUSBPRV = %size(QUSEC);
120300131204
120400131204         // -?First step - Initialize the sort routine.?
120500131204         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
120600131204                      SizeList : ReturnSize : QUSEC );
120700131204
120800131204         // -?Next step - Write records to I/O routine.?
120900131204         QLGRT00  = Put;
121000131204         For  Nrr = 1  To  RrnLast;
121100131204           chain  Nrr  LRX2S01;
121200131204           // -?Solo le righe con Selected = 'Y' sono riordinate,?
121300131204           //  ?quindi per fare un ordinamento di tutte le righe?
121400131204           //  ?metto 'Y' sempre.?
121500131204           Selected = 'Y';
121600131204           clear  QUSEI;
121700131204           QUSBPRV  = %size(QUSEC);
121800131204           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
121900131204                         SizeList : NotUsed : QUSEC );
122000131204         EndFor;
122100131204
122200131204         // -?Next step - Signal end of input, clear subfile for reload.?
122300131204         QLGRT00 = EndPut;
122400131204         clear  QUSEI;
122500131204         QUSBPRV = %size(QUSEC);
122600131204         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
122700131204                       SizeList : NotUsed : QUSEC );
122800131204
122900131204         // -?Pulizia subfile?
123000131204         SflDsp_N    = *on;
123100131204         SflDspCtl_N = *on;
123200131204         write  LRX2C01;
123300131204         SflDspCtl_N = *off;
123400131204         SflEnd      = *off;
123500131204
123600131204         // -?Final step - Write the records back to the subfile.?
123700131204         QLGRT00  = Get;
123800131204         For  Nrr = 1  To RrnLast;
123900131204           clear  QUSEI;
124000131204           QUSBPRV = %size(QUSEC);
124100131204           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
124200131204                         QLGRL00  : NotUsed : QUSEC );
124300131204           // -?Ripristino indicatori utilizzati nel sfl rec?
124400131206           Ord_x_OraIns = (wOrdSfl = 1);
124500131204           SflNxtChg  = (S01opz <> *zero);
124600131204           S01nrr = Nrr;
124700131204           write  LRX2S01;
124800131204         EndFor;
124900131204
125000131204         C1CsrRrn = 1;
125100131204
125200131204         // -?Visualizzazione del SFL (se ci sono dati)?
125300131204         SflDsp_N = (S01nrr <= *zero);
125400131204
125500131204         // -?Attivazione del SFLEND?
125600131204         SflEnd = $EoF;
125700131204
125800131205         // -?Impostazione F11 nel piede del subfile?
125900131204         select;
126000131204           when  S01nrr  = *zero;
126100131205             clear  V1PF11;
126200131204           when  wOrdSfl = 0;
126300131205             V1PF11 = 'F11=Ord.OraIns';
126400131204           when  wOrdSfl = 1;
126500131205             V1PF11 = 'F11=Ord. x Cmd';
126600131204         endsl;
126700131204
126800131204       ENDSR;
126900131127
127000131127       //--------------------------------------------------------------
127100131127       //?Gestione opzioni del subfile S01.                            ?
127200131127       //--------------------------------------------------------------
127300131127       BEGSR  sr_OpzS01;
127400131127
127500131127         clear  SavS01csr;
127600131127
127700131127         // -?Ciclo di lettura subfile?
127800131127         readc  LRX2S01;
127900131127
128000131129         DoW  Not %eof(FNLRX2D);
128100131127
128200131127           SflNxtChg = *off;
128300131127           %subst(IndDspF : 50) = *off;
128400131127           C1RcdNbr = S01nrr;
128500131127
128600131127           select;
128700131127
128800131127             // -?Nessuna opzione?
128900131127             when  S01opz = *zero;
129000131127
129100131127             // -?Opz. 1=Gestione?
129200131127             when  S01opz = 1;
129300131127               select;
129400131127                 when  S1Htor = c_Consegna;
129500131127                   $Video  = 'DS';
129600140203                   $InzD01 = *on;
129700131127                   doW  $Video = 'DS';
129800140207                     exsr  sr_GesD01;
129900131127                   enddo;
130000131127                 when  S1Htor = c_Ritiro;
130100131127                   $Video  = 'DO';
130200140203                   $InzD01 = *on;
130300131127                   doW  $Video = 'DO';
130400140207                     exsr  sr_GesD01;
130500131127                   enddo;
130600131127                 other;
130700131127                   ErrGenerico = *on;
130800131127                   ErrMessage  = *on;
130900131127                   PosCurOPZ   = *on;
131000131127                   V1Dmsg = sk_Msg(02);
131100131127               endsl;
131200131127
131300131127             // -?Opzione errata?
131400131127             other;
131500131127               ErrGenerico = *on;
131600131127               ErrMessage  = *on;
131700131127               PosCurOPZ   = *on;
131800131127               V1Dmsg = sk_Msg(02);
131900131127
132000131127           endsl;
132100131127
132200131127           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
132300131127           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
132400131127           if  S01opz  <> *zero    and
132500131127              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
132600131127             SavS01csr   = C1RcdNbr;
132700131127           endif;
132800131127
132900131127           // -?Aggiornamento sfl?
133000131127           select;
133100131127             when  ErrMessage;
133200131127               SflNxtChg = *on;
133300131127               C1CsrRrn  = C1RcdNbr;
133400131127             when  ErrGenerico;
133500131127               C1CsrRrn  = C1RcdNbr;
133600131127               clear  S01opz;
133700131127             other;
133800131127               clear  S01opz;
133900131127           endsl;
134000131127
134100131206           Ord_x_OraIns = (wOrdSfl = 1);
134200131206
134300131127           UPDATE  LRX2S01;
134400131127
134500131127           if  ErrGenerico   or   ErrMessage;
134600131127             leave;
134700131127           endif;
134800131127
134900131127           readc  LRX2S01;
135000131127
135100131127         ENDDO;
135200131127
135300131127         // -?Riposizionam. cursore sul record contenente la prima opz.?
135400131127         //  ?(se non sono stati rilevati errori)?
135500131127         if  NOT ErrMessage   and   NOT ErrGenerico   and
135600131127             SavS01csr > *zero;
135700131127           C1CsrRrn = SavS01csr;
135800131127         endif;
135900131127
136000131127       ENDSR;
136100131127
136200131127       //--------------------------------------------------------------
136300131127       //?Lettura file FICAU01L e selezione del record.                ?
136400131127       //--------------------------------------------------------------
136500131127       BEGSR  sr_ReadFICAU;
136600131127
136700131205         reade(N)  %kds(keyFICAU01 : 2)  FICAU000;
136800131127         $EoF = ( %eof(FICAU01L) );
136900131127
137000131127         // -?Selezione singolo record?
137100131212         DoW  Not  $EoF;
137200131127
137300131127           exsr  sr_SelezRec;
137400131127
137500131127           if  $RecOK;
137600131127             leave;
137700131127           endif;
137800131127
137900131205           reade(N)  %kds(keyFICAU01 : 2)  FICAU000;
138000131127           $EoF = ( %eof(FICAU01L) );
138100131127
138200131127         EndDo;
138300131127
138400131127       ENDSR;
138500131121
138600131121       //--------------------------------------------------------------
138700131122       //?Selezione del singolo record in input.                       ?
138800131121       //--------------------------------------------------------------
138900131121       BEGSR  sr_SelezRec;
139000131121
139100131121         $RecOK = *off;
139200131122
139300131122         Select;
139400131121
139500131121           // -?Filiale Gestione diversa?
139600131121           //  ?(IMPOSSIBILE perchè 1ª chiave)?
139700131121           //When  CAUfgs <> FNLRX2ds.iX2fgs;
139800131121           //  setll  (*hival)  FICAU000;
139900131121           //  leavesr;
140000131205
140100131205           // -?Autotrasportatore diverso da quello selezionato?
140200131205           //  ?(IMPOSSIBILE perchè 2ª chiave)?
140300131205           //When  CAUpdr <> FNLRX2ds.iX2aut;
140400131205           //  leavesr;
140500131205
140600131205           // -?Distinta Consegna diversa da quella selezionata?
140700140207           //When  FNLRX2ds.iX2dst <> *zero   and
140800140207           //      FNLRX2ds.iX2dst <> CAUnfv  and
140900140207           //      CAUtor          =  c_Consegna;
141000140207           When  FNLRX2ds.iX2dst <> *zero   and
141100140207                 FNLRX2ds.iX2dst <> CAUnfv;
141200131205             leavesr;
141300131121
141400131121           // -?Record Annullato?
141500131121           When  CAUatb <> *blank;
141600131121             leavesr;
141700131121
141800131121           // -?Ammessi Tipi Oggetto "C" (consegna) e "R" (ritiro)?
141900131121           When  CAUtor <> c_Consegna  and  CAUtor <> c_Ritiro;
142000131121             leavesr;
142100131126
142200131126           // -?Solo Telefonate SENZA risposta?
142300131126           When  CAUflt <> 'T';
142400131126             leavesr;
142500131126           When  CAUdtOtel <> *blank  and  CAUdtOtel <> *zero;
142600131126             leavesr;
142700131121
142800131121         EndSl;
142900131121
143000131121         // -?Telefonata da caricare nel subfile?
143100131121         $RecOK = *on;
143200131121
143300131121       ENDSR;
143400131030
143500131030       //--------------------------------------------------------------
143600140207       //?Gestione videata DS1 (spedizione) o DO1 (O.R.M.)             ?
143700131030       //--------------------------------------------------------------
143800140207       BEGSR  sr_GesD01;
143900140207
144000140207         // -?Inizializzazione videata?
144100140207         //  ?(sempre e comunque, per reperire i dati aggiornati)?
144200140207         clear  $RecUpdated;
144300140207         select;
144400140207           when  $Video = 'DS';
144500140207             exsr  sr_InzDS1;
144600140207             exsr  sr_AttrVisDS1;
144700140207           when  $Video = 'DO';
144800140207             exsr  sr_InzDO1;
144900140207             exsr  sr_AttrVisDO1;
145000140207         endsl;
145100140207
145200140207         // -?(Dis)Abilitazione tasti funzionali a video?
145300140207         exsr  sr_AbilFxxD01;
145400140207
145500140207         // -?Verifica dell'oggetto: confronto con i dati precedentemente?
145600140207         //  ?memorizzati  (qualcuno dei quali potrebbe essere stato?
145700140207         //  ?modificato da un altro utente)?
145800140207         if  Not  $InzD01;
145900140207           exsr  sr_Verifica_Ogg;
146000140207         endif;
146100140207
146200140207         $InzD01 = *off;
146300140207
146400140207         // -?Eventuale avviso di dati variati?
146500140207         if  $RecUpdated;
146600140207           ErrGenerico = *on;
146700140207           ErrMessage  = *on;
146800140207           V1Dmsg = sk_Msg(04);
146900140207         endif;
147000131120
147100131120         // -?Emissione videata?
147200131120         write  LRX2T01;
147300140207         select;
147400140207           when  $Video = 'DS';
147500140207             exfmt  LRX2DS1;
147600140207           when  $Video = 'DO';
147700140207             exfmt  LRX2DO1;
147800140207         endsl;
147900131120
148000131120         clear  V1Dmsg;
148100131120         reset  ErrMessage;
148200140203         reset  ErrGenerico;
148300140203
148400140203         // -?Memorizzazione dati della videata per verificarne variazioni?
148500140203         if  dsp_aid <> c_F03  and  dsp_aid <> c_F12;
148600140203           exsr  sr_Memo_Dati;
148700140203         endif;
148800131120
148900131120         SELECT;
149000131127
149100131127           // -?F1=Ultimo Contatto?
149200131127           WHEN  dsp_aid = c_F01;
149300131128             exsr  sr_GesW01;
149400131122
149500140203           // -?F3=Fine?
149600140203           //  ?(ritorno al subfile S01 interrompendone la lettura)?
149700131122           WHEN  dsp_aid = c_F03;
149800140108             //clear  $Video;
149900140108             //FNLRX2ds.oX2fxx = '03';
150000140108             //ErrGenerico = *on;
150100140108             //$Fine = *on;
150200140108             ErrGenerico = *on;
150300140108             $Video = 'S1';
150400131120
150500131120           // -?F7=Interrogazione spedizione?
150600140207           WHEN  dsp_aid = c_F07  and  $Video = 'DS';
150700131121             exsr  sr_VisualSped;
150800140207
150900140207           // -?F8=Interrogazione O.R.M.?
151000140207           WHEN  dsp_aid = c_F08  and  $Video = 'DO';
151100140207             exsr  sr_VisualORM;
151200131120
151300131120           // -?F12=Ritorno?
151400140203           //  ?(ritorno al subfile S01 proseguendone la lettura)?
151500131120           WHEN  dsp_aid = c_F12;
151600140108             //ErrGenerico = *on;
151700131120             $Video = 'S1';
151800131120
151900140108           // -?Invio o F6=Accettato?
152000131120           OTHER;
152100140207             select;
152200140207               when  $Video = 'DS';
152300140207                 exsr  sr_CtrDS1;
152400140207               when  $Video = 'DO';
152500140207                 exsr  sr_CtrDO1;
152600140207             endsl;
152700140207             if  ErrGenerico = *on;
152800131120               leavesr;
152900131120             endif;
153000140108             // -?F6=Accettato (Esegue)?
153100140108             if  dsp_aid = c_F06;
153200140121               exsr  sr_F06D01;
153300131205             endif;
153400131120
153500131120         ENDSL;
153600131030
153700131030       ENDSR;
153800140207
153900140207       //--------------------------------------------------------------
154000140207       //?Abilitazione tasti funzionali video DS1 / DO1.               ?
154100140207       //--------------------------------------------------------------
154200140207       BEGSR  sr_AbilFxxD01;
154300140207
154400140207         // -?F1 = Ultimo Contatto?
154500140207         F1Attivo = ($UltContPrec = *on);
154600140207
154700140207       ENDSR;
154800131121
154900131121       //--------------------------------------------------------------
155000131121       //?Inizializazzione videata DS1 (spedizione).                   ?
155100131121       //--------------------------------------------------------------
155200131121       BEGSR  sr_InzDS1;
155300131121
155400131122         ds_Spedizione = S1Hogg;
155500140206
155600140206         // -?Impostazione chiave di accesso per la v.l. 02L?
155700140206         clear  keyFICAU02;
155800140206         k_CA2fgs    = FNLRX2ds.iX2fgs;
155900140206         k_CA2nfv    = FNLRX2ds.iX2dst;
156000140206         k_CA2pdr    = FNLRX2ds.iX2aut;
156100140206         k_CA2tor    = S1Htor;
156200140206         k_CA2ogg    = S1Hogg;
156300140207         k_CA2dtOins = *hival;
156400131121
156500131121         // -?Pulizia videata?
156600131121         clear  LRX2DS1;
156700131128         clear  $UltContPrec;
156800131122
156900131122         // -?Contatto ad Autotrasportatore?
157000140203         clear  sk_TVA;
157100131122         clear  keyFICAU01;
157200140207         //k_CAUfgs    = S1Hfgs;
157300140207         //k_CAUpdr    = C1Cpdr;
157400140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
157500140206         k_CAUpdr    = FNLRX2ds.iX2aut;
157600131122         k_CAUdtOins = S1HdtOins;
157700131122         k_CAUtor    = S1Htor;
157800131125         k_CAUogg    = S1Hogg;
157900140203         chain(N)  %kds( keyFICAU01 )  FICAU000;
158000140203
158100140203         // -?Rec. NON trovato: ricerca con data/ora inserimento più?
158200140203         //  ?recenti (segnalando errore SE non trovato nuovo rec.)?
158300140203         //  ?e riemissione della videata?
158400140203         if  Not %found(FICAU01L)  or  CAUatb <> *blank;
158500140203
158600140203           exsr  sr_Ricarica_Ogg;
158700140203
158800140203           if  Not $RecUpdated;
158900140203             ErrGenerico = *on;
159000140203             ErrMessage  = *on;
159100140203             V1Dmsg = sk_Msg(03);
159200140203             leavesr;
159300140203           endif;
159400140203
159500140203         endif;
159600131121
159700131121         // -?Decodifica autotrasportatore?
159800140203         exsr  sr_GetAUT;
159900131122
160000131122         // -?Comando da impartire al PDA o all'AUT?
160100140203         exsr  sr_GetCMD;
160200131122
160300131122         // -?Tipologica Causale Variazione bolla?
160400140131         //  ?(SE NON "INSERITO")?
160500140203         exsr  sr_GetTVA;
160600131121
160700131121         // -?Spedizione?
160800131122         exsr  sr_GetSped;
160900131121         if  Not  %found(FNARB01L);
161000131121           leavesr;
161100131121         endif;
161200131122
161300131122         V1Clnp = ds_Spedizione.wLNP;
161400131122         V1Cnrs = ds_Spedizione.wNRS;
161500131122         V1Cnsp = ds_Spedizione.wNSP;
161600131204         //wDate_EUR = %date( (ds_Spedizione.wAAS * 10000) + ARBmgs : *iso );
161700131204         //V1Cdsp = %dec( wDate_EUR );
161800131127
161900131127         // -?Colli?
162000131127         V1Cncl = ARBncl;
162100131127
162200131127         // -?Peso?
162300131127         V1Cpkb = ARBpkb;
162400131127
162500131127         // -?Particolarità?
162600131127         V1Cgga = ARBgga;
162700131127         V1Cgma = ARBgma;
162800131127         V1Cgva = ARBgva;
162900131126
163000131127         // -?Codice Bolla (tab. "3A")?
163100131204         //clear  ds3A;
163200131204         //if  getTabella ( c_Tabel : '3A' : ARBcbo : *omit :
163300131204         //                 *omit : *omit :
163400131204         //                 *omit : *omit :
163500131204         //                 *omit : *omit : *omit : *omit :
163600131204         //                 *omit : *omit : *omit : *omit :
163700131204         //                 ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
163800131204         //                 = *zero      AND
163900131204         //      ds_TNTBE.TBEatb = *blank;
164000131204         //  ds3A = ds_TNTBE.TBEuni;
164100131204         //else;
164200131204         //  §3Ades = *all'? ';
164300131204         //endif;
164400131204         //
164500131204         //V1Ccbo = ARBcbo;
164600131204         //V1Dcbo = §3Ades;
164700131126
164800131126         // -?Tipo Servizio (tab. "5E")?
164900131126         clear  ds5E;
165000131126         if  getTabella ( c_Tabel : '5E' : ARBtsp : *omit :
165100131126                          *omit : *omit :
165200131126                          *omit : *omit :
165300131126                          *omit : *omit : *omit : *omit :
165400131126                          *omit : *omit : *omit : *omit :
165500131126                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
165600131126                          = *zero      AND
165700131126               ds_TNTBE.TBEatb = *blank;
165800131126           ds5E = ds_TNTBE.TBEuni;
165900131126         else;
166000131126           §5Edes = *all'? ';
166100131126         endif;
166200131126
166300131205         evalr V1Dtsp = %trimr(§5Ed08);
166400131122
166500131127         // -?Cliente Mittente?
166600131127         //if  ARBccm <> *zero;
166700131127         //  V1Cccm = ARBccm;
166800131127         //else;
166900131127         //  if  %subst(§3Atb1:1:1) = 'F';
167000131127         //    V1Cccm = ARBksc;
167100131127         //  endif;
167200131127         //endif;
167300131209         //V1Drsm = ARBrsm;
167400131209         //V1Drmo = ARBrmo;
167500131122
167600131127         //// -?Cliente Destinatario?
167700131122         //if  %subst(§3Atb1:1:1) = 'A';
167800131122         //  V1Cccd = ARBksc;
167900131122         //endif;
168000131122         //// -?Se c'è 2ª bolla imposto LNA9999?
168100131122         //if  §3Atb2 <> *blank;
168200131122         //  V1Cccd = (ARBlna * 10000) + 9999;
168300131122         //endif;
168400131209         V1Drsd = ARBrsd + AR4not;
168500131209         V1Dind = ARBind;
168600131209         //V1Dcad = ARBcad;
168700131209         //V1Dlpd = %trim( ARBlod ) + '  ' + ARBprd + ' ' + ARBnzd;
168800131209         V1Dlpd = %trim( ARBcad ) + ' ' + %trim( ARBlod ) + '  ' +
168900131209                  ARBprd + ' ' + ARBnzd;
169000131121
169100131121         // -?Distinta?
169200131205         ////k_DSTfgs = (ARBpdc / 10000);
169300131205         ////k_DSTnfv = ARBndc;
169400131205         //k_DSTfgs = FNLRX2ds.iX2fgs;
169500131205         //k_DSTnfv = FNLRX2ds.iX2dst;
169600131205         //chain  %kds( keyFIDST01 )  FIDST000;
169700131205         //if  Not  %found(FIDST01L);
169800131205         //  clear  DSTpda;
169900131205         //endif;
170000131121
170100131122         // -?Referente Consegna (estensione testata bolla: trk "GEN")?
170200131121         clear  dAR5gen;
170300131121         k_AR5aas = ds_Spedizione.wAAS;
170400131121         k_AR5lnp = ds_Spedizione.wLNP;
170500131121         k_AR5nrs = ds_Spedizione.wNRS;
170600131121         k_AR5nsp = ds_Spedizione.wNSP;
170700131205         //k_AR5trd   = 'GEN';        ?(già così)?
170800131121         chain  %kds(keyFIAR501 : 5)  FIAR5000;
170900131121         if  %found(FIAR501L)  and  AR5atb = *blank;
171000131121           dAR5gen = AR5uni;
171100131121         endif;
171200131122
171300131204         select;
171400131204           when  dAR5gen.§AR5ref  <> *blank  and
171500131204                 dAR5gen.§AR5telD <> *blank;
171600131209             V1Dref = %trimr( dAR5gen.§AR5ref ) + ' - ' +
171700131204                      %triml( dAR5gen.§AR5telD );
171800131204           when  dAR5gen.§AR5ref  <> *blank;
171900131209             V1Dref = dAR5gen.§AR5ref;
172000131204           when  dAR5gen.§AR5telD <> *blank;
172100131209             V1Dref = dAR5gen.§AR5telD;
172200131204         endsl;
172300131121
172400131121         // -?Dati C/Assegno?
172500131121         chain  %kds( keyFNARB01 )  FIAR9000;
172600131122         if  Not %found(FIAR901L);
172700131121           clear  AR9cas;
172800131121           clear  AR9vca;
172900131121         endif;
173000131122
173100131122         V1Ccas = AR9cas;
173200131122         V1Cvca = AR9vca;
173300131121
173400131122         // -?Dati Fattura (Tassazione Bolla - filiale)?
173500131122         clear  keyFIAR601;
173600131121         k_AR6aas = ds_Spedizione.wAAS;
173700131121         k_AR6lnp = ds_Spedizione.wLNP;
173800131121         k_AR6nrs = ds_Spedizione.wNRS;
173900131121         k_AR6nsp = ds_Spedizione.wNSP;
174000131122         // -?C'è 2ª bolla (?)?
174100131122         k_AR6trc = '2';
174200131122         clear  AR6div;
174300131122         clear  AR6ift;
174400131122         chain  %kds(keyFIAR601)  FIAR6000;
174500131122         If  %found(FIAR601L);
174600131122           if  AR6ift = *zero;
174700131122             clear  AR6div;
174800131122           endif;
174900131122         // -?1ª bolla?
175000131122         Else;
175100131122           k_AR6trc = '1';
175200131122           chain  %kds(keyFIAR601)  FIAR6000;
175300131122           if  %found(FIAR601L)  and  AR6ift = *zero;
175400131122             clear  AR6div;
175500131122           endif;
175600131122         EndIf;
175700131122
175800131122         V1Cift = AR6ift;
175900131122         V1Cdiv = AR6div;
176000131121
176100131126         // -?Note LdV?
176200131122         clear  keyFIAR401;
176300131122         k_AR4aas = ds_Spedizione.wAAS;
176400131122         k_AR4lnp = ds_Spedizione.wLNP;
176500131122         k_AR4nrs = ds_Spedizione.wNRS;
176600131122         k_AR4nsp = ds_Spedizione.wNSP;
176700131122         k_AR4trc = '8';
176800131122         chain  %kds(keyFIAR401)  FIAR4000;
176900131122         if  %found(FIAR401L);
177000131122           V1Dnot8 = AR4not;
177100131122         endif;
177200131122         k_AR4trc = '9';
177300131122         chain  %kds(keyFIAR401)  FIAR4000;
177400131122         if  %found(FIAR401L);
177500131122           V1Dnot9 = AR4not;
177600131122         endif;
177700131122
177800131126         // -?Note Bolla?
177900131126         k_ARNaas = ds_Spedizione.wAAS;
178000131126         k_ARNlnp = ds_Spedizione.wLNP;
178100131126         k_ARNnrs = ds_Spedizione.wNRS;
178200131126         k_ARNnsp = ds_Spedizione.wNSP;
178300131205         //k_ARNcdn = 'LDV';          ?(già impostato)?
178400131205         //k_ARNgst = 'S';            ?(già impostato)?
178500131126         setll  %kds(keyFIARN12 : 6)  FIARN000;
178600131126         reade  %kds(keyFIARN12 : 6)  FIARN000;
178700131126         DoW  Not %eof(FIARN12L);
178800131126           select;
178900131126             when  V1Cnt1 = *blank;
179000131126               V1Cnt1 = ARNnob;
179100131126             when  V1Cnt2 = *blank;
179200131126               V1Cnt2 = ARNnob;
179300131126             when  V1Cnt3 = *blank;
179400131126               V1Cnt3 = ARNnob;
179500131126             when  V1Cnt4 = *blank;
179600131126               V1Cnt4 = ARNnob;
179700140121             other;
179800140121               leave;
179900131126           endsl;
180000131126           reade  %kds(keyFIARN12 : 6)  FIARN000;
180100131126         EndDo;
180200131128
180300140108         // -?Note R.A.: solo input?
180400131122
180500131126         // -?Reperimento Ultimo contatto precedente?
180600140203         exsr  sr_GetUltContPrec;
180700131121
180800131121       ENDSR;
180900131127
181000131127       //--------------------------------------------------------------
181100131127       //?Impostazione attributi di visualizzazione video DS1.         ?
181200131127       //--------------------------------------------------------------
181300131127       BEGSR  sr_AttrVisDS1;
181400131127
181500131204         // ·?Ultimo Contatto?
181600131204         RevImgUCO  = (V1Duco <> *blank);
181700131204
181800131127         // ·?Comando?
181900131127         RevImgCMD  = (V1Dcmd <> *blank);
182000131204
182100131204         // ·?Variazioni?
182200131204         RevImgVAR  = (V1Dvar <> *blank);
182300131127
182400131127         // ·?Tipo Servizio?
182500131127         RevImgTSP  = (ARBtsp = 'E'  or  ARBtsp = 'H');
182600131127
182700131127         // ·?Fermo Deposito?
182800131127         VisualFDEP = (ARBffd <> *blank);
182900131127
183000131127         // ·?Contrassegno?
183100131129         VisualCASS = (V1Ccas > *zero);
183200131127
183300131127         // ·?Importo Fattura?
183400131129         VisualIFT  = (V1Cift > *zero);
183500131129
183600131129         // ·?Interrogazione Particolarità Giacenza?
183700131129         VisPartGiac  = (V1Cgga <> *blank);
183800131129
183900131129         // ·?Interrogazione Particolarità Consegna?
184000131129         VisPartCons  = (V1Cgma <> *blank);
184100131129
184200131129         // ·?Interrogazione Particolarità Varia?
184300131129         VisPartVarie = (V1Cgva <> *blank);
184400131127
184500131127       ENDSR;
184600131121
184700131121       //--------------------------------------------------------------
184800131121       //?Controllo videata DS1 (spedizione).                          ?
184900131121       //--------------------------------------------------------------
185000131121       BEGSR  sr_CtrDS1;
185100131121
185200131121         // -?Spegnimento indicatori di posizionamento cursore?
185300131127         %subst(IndDspF : 60) = *off;
185400131127
185500131127         // -?Interrogazione Particolarità Giacenza?
185600131127         if  V1Igga = '?';
185700131128           reset  ds7PQRS;
185800131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
185900131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
186000131128           ds7PQRS.DS7cod = V1Cgga;
186100131127           KPJBU = ds7PQRS;
186200131127           trtb69r ( kpjba );
186300131127           clear  V1Igga;
186400131127           ErrGenerico = *on;
186500131127           leavesr;
186600131127         endif;
186700131127
186800131127         // -?Interrogazione Particolarità Consegna?
186900131127         if  V1Igma = '?';
187000131128           reset  ds7PQRS;
187100131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
187200131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
187300131128           ds7PQRS.DS7cod = V1Cgma;
187400131127           KPJBU = ds7PQRS;
187500131127           trtb70r ( kpjba );
187600131127           clear  V1Igma;
187700131127           ErrGenerico = *on;
187800131127           leavesr;
187900131127         endif;
188000131127
188100131127         // -?Interrogazione Particolarità Varie?
188200131127         if  V1Igva = '?';
188300131128           reset  ds7PQRS;
188400131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
188500131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
188600131128           ds7PQRS.DS7cod = V1Cgva;
188700131127           KPJBU = ds7PQRS;
188800131127           trtb73r ( kpjba );
188900131127           clear  V1Igva;
189000131127           ErrGenerico = *on;
189100131127           leavesr;
189200131127         endif;
189300131128
189400131128         clear  V1Igga;
189500131128         clear  V1Igma;
189600131128         clear  V1Igva;
189700131121
189800131121       ENDSR;
189900131121
190000131121       //--------------------------------------------------------------
190100131122       //?Richiamo *pgm x Interrogazione Spedizione (in filiale).      ?
190200131121       //--------------------------------------------------------------
190300131121       BEGSR  sr_VisualSped;
190400131121
190500131121         clear  FNLR36ds;
190600131121         FNLR36ds.LR36aas = k_ARBaas;
190700131121         FNLR36ds.LR36lnp = k_ARBlnp;
190800131121         FNLR36ds.LR36nrs = k_ARBnrs;
190900131121         FNLR36ds.LR36nsp = k_ARBnsp;
191000131121         FNLR36ds.LR36flg = '1';
191100131121         kpjbu = FNLR36ds;
191200131121
191300131121         fnlr36r ( kpjba );
191400131121
191500131121         FNLR36ds = kpjbu;
191600131121         if  FNLR36ds.LR36f03 = *on;
191700131121           $Video = 'S1';
191800131121           ErrGenerico = *on;
191900131121         endif;
192000131121
192100131121       ENDSR;
192200131209
192300131209       //--------------------------------------------------------------
192400131209       //?Inizializzazione videata DO1 (O.R.M.).                       ?
192500131209       //--------------------------------------------------------------
192600131209       BEGSR  sr_InzDO1;
192700131209
192800131209         ds_ORM = S1Hogg;
192900140207
193000140207         // -?Impostazione chiave di accesso per la v.l. 02L?
193100140207         clear  keyFICAU02;
193200140207         k_CA2fgs    = FNLRX2ds.iX2fgs;
193300140207         k_CA2nfv    = FNLRX2ds.iX2dst;
193400140207         k_CA2pdr    = FNLRX2ds.iX2aut;
193500140207         k_CA2tor    = S1Htor;
193600140207         k_CA2ogg    = S1Hogg;
193700140207         k_CA2dtOins = *hival;
193800131209
193900131209         // -?Pulizia videata?
194000131209         clear  LRX2DO1;
194100131209         clear  $UltContPrec;
194200131209
194300131209         // -?Contatto ad Autotrasportatore?
194400140203         clear  sk_TVA;
194500131209         clear  keyFICAU01;
194600140206         //k_CAUfgs    = S1Hfgs;
194700140206         //k_CAUpdr    = C1Cpdr;
194800140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
194900140206         k_CAUpdr    = FNLRX2ds.iX2aut;
195000131209         k_CAUdtOins = S1HdtOins;
195100131209         k_CAUtor    = S1Htor;
195200131209         k_CAUogg    = S1Hogg;
195300140203         chain(N)  %kds( keyFICAU01 )  FICAU000;
195400140203
195500140203         // -?Rec. NON trovato: ricerca con data/ora inserimento più?
195600140203         //  ?recenti (segnalando errore SE non trovato nuovo rec.)?
195700140203         //  ?e riemissione della videata?
195800131209         if  Not %found(FICAU01L)  or  CAUatb <> *blank;
195900140203
196000140203           exsr  sr_Ricarica_Ogg;
196100140203
196200140203           if  Not $RecUpdated;
196300140203             ErrGenerico = *on;
196400140203             ErrMessage  = *on;
196500140203             V1Dmsg = sk_Msg(03);
196600140203             leavesr;
196700140203           endif;
196800140203
196900131209         endif;
197000131209
197100131209         // -?Decodifica autotrasportatore?
197200140203         exsr  sr_GetAUT;
197300131209
197400131209         // -?Comando da impartire al PDA o all'AUT?
197500140203         exsr  sr_GetCMD;
197600131209
197700131209         // -?Tipologica Causale Variazione bolla?
197800140131         //  ?(SE NON "INSERITO")?
197900140203         exsr  sr_GetTVA;
198000131209
198100131209         // -?O.R.M.?
198200131209         exsr  sr_GetORM;
198300131209         if  Not  %found(FNORM01L);
198400131209           leavesr;
198500131209         endif;
198600131209
198700131209         V1Cpoe = ds_ORM.wPOE;
198800131209         V1Cnsr = ds_ORM.wNSR;
198900131209         V1Cnor = ds_ORM.wNOR;
199000131209         V1Cnrv = ds_ORM.wNRV;
199100131211
199200131211         // -?Orari di Apertura del cliente?
199300131211         //  ?Recupero dati da estensione FNORE?
199400131211         //  ?Rcd "O " => Orari apertura?
199500131211         clear dOREorari;
199600131211         k_OREpoe = ds_ORM.wPOE;
199700131211         k_OREnsr = ds_ORM.wNSR;
199800131211         k_OREnor = ds_ORM.wNOR;
199900131211         k_OREnrv = ds_ORM.wNRV;
200000131211
200100131211         chain  %kds(keyFNORE01)  FNORE000;
200200131211
200300131211         if  %found(FNORE01L);
200400131211           dOREorari = OREdati;
200500131211         endif;
200600131211         V1oraAMda = dOREorari.§OREoramda;
200700131211         V1oraAMa  = dOREorari.§OREorama;
200800131211         V1oraPMda = dOREorari.§OREorapda;
200900131211         V1oraPMa  = dOREorari.§OREorapa;
201000131210
201100131211         // -?Ricevuta di Ritiro?
201200131210         if  dORM01.§ORsrm = 'S';
201300131210           V1Datn = 'Ricevuta di Ritiro';
201400131210         endif;
201500140121
201600140121         // -?Ora merce pronta?
201700140121         V1orr  = ORMorr;
201800131209
201900131209         // -?Colli?
202000131209         V1Cncl = ORMncl;
202100131209
202200131209         // -?Peso?
202300131209         V1Cpkg = ORMpkg;
202400131209
202500131209         // -?Bancali?
202600131209         V1Cbnc = ORMbnc;
202700131209
202800131209         // -?Volume?
202900131209         V1Cvlm = ORMvlm;
203000131209
203100131209         // -?Spunda Idraulica?
203200131209         if  ORMspi = 'S';
203300131209           V1Cspi = 'SI';
203400131209         else;
203500131209           V1Cspi = 'NO';
203600131209         endif;
203700131209
203800131209         // -?Cliente Mittente?
203900131209         V1Drsr = ORMrsr;
204000131209         V1Dinr = ORMinr;
204100131209         V1Dlor = ORMlor;
204200131209         V1Dprr = ORMprr;
204300131209         V1Dcar = ORMcar;
204400131209         V1Dnar = ORMnar;
204500131209         //V1Dlpr = %trim( ORMcar ) + ' ' + %trim( ORMlor ) + '  ' +
204600131209         //         ORMprr + ' ' + ORMnar;
204700131209
204800131209         // -?Referente?
204900131209         select;
205000131209           when  ORMrer <> *blank  and  ORMter <> *blank;
205100131209             V1Dref = %trimr( ORMrer ) + ' - ' + %triml( ORMter );
205200131209           when  ORMrer  <> *blank;
205300131209             V1Dref = ORMrer;
205400131209           when  ORMter <> *blank;
205500131209             V1Dref = ORMter;
205600131209         endsl;
205700131210
205800131210         // -?Importo Prepagato?
205900131210         If  ORMtor = 'P';
206000131210
206100131210           clear  keyFIAR404;
206200131210           k_AR44trc = 'M';
206300131210           k_AR44n14 = ds_ORM;
206400131210           chain  %kds(keyFIAR404)  FIAR4004;
206500131210
206600131210           if  %found(FIAR404L);
206700131210
206800131210             clear  keyFIAR601;
206900131210             k_AR6aas = AR4aas;
207000131210             k_AR6lnp = AR4lnp;
207100131210             k_AR6nrs = AR4nrs;
207200131210             k_AR6nsp = AR4nsp;
207300131210             chain  %kds(keyFIAR601 : 4)  FIAR6000;
207400131210
207500131210             if  %found(FIAR601L);
207600131210               w0112 = AR6ift;
207700131210               V1Datn = 'Prepagato: ' + %triml( %editc( w0112 : 'K' ) )
207800131210                                      + AR6div;
207900131210             endif;
208000131210
208100131210           endif;
208200131210
208300131210         EndIf;
208400131209
208500131209         // -?Note O.R.M.?
208600131209         V1Dnot1 = ORMno1;
208700131209         V1Dnot2 = ORMno2;
208800131209
208900131209         // -?Note AUT?
209000140121         clear  keyFNORT01;
209100140121         k_ORTpoe = ds_ORM.wPOE;
209200140121         k_ORTnsr = ds_ORM.wNSR;
209300140121         k_ORTnor = ds_ORM.wNOR;
209400140121         k_ORTnrv = ds_ORM.wNRV;
209500140121         setll  %kds( keyFNORT01 )  FNORT000;
209600140121         reade  %kds( keyFNORT01 : 4 )  FNORT000;
209700140121         DoW  Not %eof(FNORT01L);
209800140121           select;
209900140123             when  ORTgst <> 'S';
210000140121             when  V1Cnt1 = *blank;
210100140121               V1Cnt1 = ORTnob;
210200140121             when  V1Cnt2 = *blank;
210300140121               V1Cnt2 = ORTnob;
210400140121             //when  V1Cnt3 = *blank;
210500140121             //  V1Cnt3 = ORTnob;
210600140121             //when  V1Cnt4 = *blank;
210700140121             //  V1Cnt4 = ORTnob;
210800140121             other;
210900140121               leave;
211000140121           endsl;
211100140121           reade  %kds( keyFNORT01 : 4 )  FNORT000;
211200140121         EndDo;
211300131209
211400140108         // -?Note R.A.: solo input?
211500131209
211600131209         // -?Reperimento Ultimo contatto precedente?
211700140203         exsr  sr_GetUltContPrec;
211800131209
211900131209       ENDSR;
212000131209
212100131209       //--------------------------------------------------------------
212200131209       //?Impostazione attributi di visualizzazione video DO1.         ?
212300131209       //--------------------------------------------------------------
212400131209       BEGSR  sr_AttrVisDO1;
212500131209
212600131209         // ·?Ultimo Contatto?
212700131209         RevImgUCO   = (V1Duco <> *blank);
212800131209
212900131209         // ·?Comando?
213000131209         RevImgCMD   = (V1Dcmd <> *blank);
213100131209
213200131209         // ·?Variazioni?
213300131209         RevImgVAR   = (V1Dvar <> *blank);
213400131209
213500131209         // ·?O.R.M. PrePagato con importo?
213600131210         VisualORMPP = (ORMtor = 'P');
213700131209
213800131209         // ·?Ricevuta di Ritiro?
213900131210         VisualORMRR = (dORM01.§ORsrm = 'S');
214000131209
214100131209       ENDSR;
214200131209
214300131209       //--------------------------------------------------------------
214400131209       //?Controllo dati nella videata D01.                            ?
214500131209       //--------------------------------------------------------------
214600131209       BEGSR  sr_CtrDO1;
214700131209
214800131209         // -?Spegnimento indicatori di posizionamento cursore?
214900140207         %subst(IndDspF : 60) = *off;
215000131209
215100131209       ENDSR;
215200131209
215300131209       //--------------------------------------------------------------
215400131209       //?Richiamo *pgm x Interrogazione O.R.M. (in filiale).          ?
215500131209       //--------------------------------------------------------------
215600131209       BEGSR  sr_VisualORM;
215700131209
215800131209         clear  Parm01;
215900131209         Parm01.pPOE = ds_ORM.wPOE;
216000131209         Parm01.pNSR = ds_ORM.wNSR;
216100131209         Parm01.pNOR = ds_ORM.wNOR;
216200131209         Parm01.pNRV = ds_ORM.wNRV;
216300131209         Parm01.pSCE = '5';
216400131209         kpjbu = Parm01;
216500131209
216600131209         clear  FIDNA1ds;
216700131209         FIDNA1ds.iA1ins = 'N';
216800131209
216900131209         fior07r ( kpjba : FIDNA1ds );
217000131209
217100131209       ENDSR;
217200131128
217300131128       //--------------------------------------------------------------
217400131128       //?Gestione videata W01 (Visualizzazione Ultimo Contatto).      ?
217500131128       //--------------------------------------------------------------
217600131128       BEGSR  sr_GesW01;
217700131128
217800131128         // -?Inizializzazione videata?
217900140122         //$VideoPrec = $Video;
218000140122         //$Video  = 'W1';
218100131128         if  $InzW01 = *on;
218200131128           exsr  sr_InzW01;
218300131128           $InzW01 = *off;
218400131128         endif;
218500131128
218600131128         // -?Emissione window?
218700131128         DoU  dsp_aid = c_F12;
218800131128           exfmt  LRX2W01;
218900131128         EndDo;
219000131128
219100131128         // -?F12=Ritorno?
219200140122         //if  dsp_aid = c_F12;
219300140122         //  $Video  = $VideoPrec;
219400140122         //endif;
219500131128
219600131128       ENDSR;
219700131128
219800131128       //--------------------------------------------------------------
219900131128       //?Inizializzazione window W01.                                 ?
220000131128       //--------------------------------------------------------------
220100131128       BEGSR  sr_InzW01;
220200131128
220300131128         // -?Reperimento Ultimo contatto precedente?
220400140203         //  ?GIÀ effettuato nella subr. "sr_InzDS1/DO1"?
220500131128
220600131128
220700131128         clear  LRX2W01;
220800131128
220900131128         // -?Profilo Utente Telefonata Precedente?
221000140213         W1pruTel   = CA2pruTel;
221100131128
221200131128         // -?Data/Ora Inserimento Telefonata Precedente?
221300140213         ds_DataOra = CA2dtOins;
221400131128         wDate_EUR  = %date( ds_DataOra.wDate );
221500131128         W1dtTel    = %dec( wDate_EUR );
221600131128         W1orTel    = ds_DataOra.wTime;
221700131128
221800131128         // -?Descrizione Comando Telefonata precedente?
221900131128         clear  dCMD;
222000140213         If  CA2desCmd = *blank  and  CA2cmd <> *blank;
222100131128           if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
222200131128                            *omit : *omit :
222300131128                            *omit : *omit :
222400131128                            *omit : *omit : *omit : *omit :
222500131128                            *omit : *omit : *omit : *omit :
222600131128                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
222700131128                            = *zero      AND
222800131128                 ds_TNTBE.TBEatb = *blank;
222900131128             dCMD = ds_TNTBE.TBEuni;
223000131128           else;
223100140213             §CMDdes = CA2cmd + '-' + '? ? ? ? ? ? ? ? ? ?';
223200131128           endif;
223300131128         Else;
223400140213           §CMDdes = %triml( CA2desCmd );
223500131128         EndIf;
223600131128         W1desCmd = §CMDdes;
223700131128
223800131128         // -?Descrizione Variazione Telefonata precedente?
223900140213         If  CA2desVA = *blank;
224000140213           wTipTVA = 'B';
224100140213           ds_TVA  = CA2tva;
224200140213           exsr  sr_GetDesTCV;
224300140213           W1desVa1 = wDesTVA;
224400140213         Else;
224500140213           W1desVa1 = %subst( CA2desVA : 1 : %size(W1desVa1) );
224600140213           W1desVa2 = %subst( CA2desVA : %size(W1desVa1) + 1 );
224700140213         EndIf;
224800131128
224900131128       ENDSR;
225000140207
225100140207       //--------------------------------------------------------------
225200140207       //?Reperimento dati della Spedizione (Consegna).                ?
225300140207       //--------------------------------------------------------------
225400140207       BEGSR  sr_GetSped;
225500140207
225600140207         ds_Spedizione = CAUogg;
225700140207
225800140207         // -?Reperimento dati da Bolle Arrivi: Testate (FNARB)?
225900140207         clear  keyFNARB01;
226000140207         k_ARBaas = ds_Spedizione.wAAS;
226100140207         k_ARBlnp = ds_Spedizione.wLNP;
226200140207         k_ARBnrs = ds_Spedizione.wNRS;
226300140207         k_ARBnsp = ds_Spedizione.wNSP;
226400140207
226500140207         chain  %kds( keyFNARB01 )  FNARB000;
226600140207
226700140207         if  Not  %found(FNARB01L);
226800140207           ARBrsd = *all'? ';
226900140207         endif;
227000140207
227100140207         // -?Estensione ragione sociale destinatario da FIAR4 rec. "D"?
227200140207         clear  keyFIAR401;
227300140207         k_AR4aas = ds_Spedizione.wAAS;
227400140207         k_AR4lnp = ds_Spedizione.wLNP;
227500140207         k_AR4nrs = ds_Spedizione.wNRS;
227600140207         k_AR4nsp = ds_Spedizione.wNSP;
227700140207         k_AR4trc = 'D';
227800140207
227900140207         chain  %kds(keyFIAR401)  FIAR4000;
228000140207
228100140207         if  Not  %found(FIAR401L);
228200140207           clear  AR4not;
228300140207         endif;
228400140207
228500140207       ENDSR;
228600140207
228700140207       //--------------------------------------------------------------
228800140207       //?Reperimento dati dell'Ordine Ritiro Merce (Ritiro).          ?
228900140207       //--------------------------------------------------------------
229000140207       BEGSR  sr_GetORM;
229100140207
229200140207         ds_ORM = CAUogg;
229300140207
229400140207         clear  keyFNORM01;
229500140207         k_ORMpoe = ds_ORM.wPOE;
229600140207         k_ORMnsr = ds_ORM.wNSR;
229700140207         k_ORMnor = ds_ORM.wNOR;
229800140207         k_ORMnrv = ds_ORM.wNRV;
229900140207
230000140207         chain  %kds( keyFNORM01 )  FNORM000;
230100140207
230200140207         if  Not  %found(FNORM01L);
230300140207           ORMrsr = *all'? ';
230400140207           clear  dORM01;
230500140207         else;
230600140207           dORM01 = ORMflo;
230700140207         endif;
230800140207
230900140207       ENDSR;
231000140203
231100140203       //--------------------------------------------------------------
231200140203       //?Reperimento ed Impostazione a video delle informazioni AUT.  ?
231300140203       //--------------------------------------------------------------
231400140203       BEGSR  sr_GetAUT;
231500140203
231600140203         // -?Decodifica autotrasportatore?
231700140203         k_APDpdr = CAUpdr;
231800140203         chain  %kds( keyFIAPD01 )  FIAPD000;
231900140203         if  Not %found(FIAPD01L)  or  APDatb <> *blank;
232000140203           clear  APDrsc;
232100140203         endif;
232200140203
232300140203         V1Cpdr = CAUpdr;
232400140203         V1Dpdr = APDrsc;
232500140203
232600140203         // -?Num. Telefono Autotrasportatore?
232700140203         //k_APD4pdr = CAUpdr;
232800140203         //chain  %kds( keyFIAPD41 )  FIAPD400;
232900140203         //if  %found(FIAPD41L)  and  APD4atb = *blank;
233000140203         //  dAPD4tel = APD4uni;
233100140203         //endif;
233200140203         //
233300140203         //V1Dtel = dAPD4tel.§APD4tel;
233400140203
233500140203       ENDSR;
233600140203
233700140203       //--------------------------------------------------------------
233800140203       //?Reperimento ed Impostazione a video delle informazioni CMD.  ?
233900140203       //--------------------------------------------------------------
234000140203       BEGSR  sr_GetCMD;
234100140203
234200140203         // -?Comando da impartire al PDA o all'AUT?
234300140203         clear  dCMD;
234400140203         If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
234500140203           if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
234600140203                            *omit : *omit :
234700140203                            *omit : *omit :
234800140203                            *omit : *omit : *omit : *omit :
234900140203                            *omit : *omit : *omit : *omit :
235000140203                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
235100140203                            = *zero      AND
235200140203                 ds_TNTBE.TBEatb = *blank;
235300140203             dCMD = ds_TNTBE.TBEuni;
235400140203           else;
235500140203             §CMDdes = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ?';
235600140203           endif;
235700140203         Else;
235800140203           §CMDdes = %triml( CAUdesCmd );
235900140203         EndIf;
236000140203
236100140203         if  §CMDdes <> *blank;
236200140203           xx = ( %size(V1Dcmd) - %len( %trim(§CMDdes) ) ) / 2;
236300140203           %subst( V1Dcmd : xx + 1 ) = §CMDdes;
236400140203         endif;
236500140203
236600140203       ENDSR;
236700140203
236800140203       //--------------------------------------------------------------
236900140203       //?Reperimento ed Impostazione a video delle informazioni TVA.  ?
237000140203       //--------------------------------------------------------------
237100140203       BEGSR  sr_GetTVA;
237200140203
237300140203         // -?Tipologica Causale Variazione bolla?
237400140203         //  ?(SE NON "INSERITO")?
237500140203         If  CAUcmd <> c_Cmd_Inserito;
237600140203           If  CAUdesVA = *blank;
237700140203             wTipTVA = *blank;
237800140213             ds_TVA  = CAUtva;
237900140203             exsr  sr_GetDesTCV;
238000140203             V1Dvar  = wDesTVA;
238100140203             //if  %subst( wDesTVA : %size( V1Dvar ) + 1 ) <> *blank;
238200140203             //  %subst( V1Dvar : %size( V1Dvar ) - 3 ) = ' (+)';
238300140203             //endif;
238400140203           Else;
238500140203             V1Dvar = CAUdesVa;
238600140203             //if  %subst( CAUdesVA : %size( V1Dvar ) + 1 ) <> *blank;
238700140203             //  %subst( V1Dvar : %size( V1Dvar ) - 3 ) = ' (+)';
238800140203             //endif;
238900140203           EndIf;
239000140203         EndIf;
239100140203
239200140203       ENDSR;
239300131205
239400131205       //--------------------------------------------------------------
239500131205       //?Reperimento dati della Spedizione (Consegna).                ?
239600131205       //--------------------------------------------------------------
239700131205       BEGSR  sr_GetDesTCV;
239800131205
239900131205         clear  wDesTVA;
240000131205
240100131205         For  xx = 1  To  %elem(sk_TVA);
240200131205
240300131205           clear  dTCV;
240400131205
240500131205           If  sk_TVA(xx) <> *blank;
240600131205
240700131205             yy = %lookup( sk_TVA(xx) : sk_TCV );
240800131205
240900131205             If  yy > *zero;
241000131205
241100131205               dTCV = sk_TCVd(yy);
241200131205               If  wDesTVA = *blank;
241300131205                 select;
241400131205                   when  wTipTVA = 'B';
241500131205                     wDesTVA = %triml( §TCVdesB );
241600131205                   when  wTipTVA = 'V';
241700131205                     wDesTVA = %triml( §TCVdesV );
241800131205                   other;
241900131205                     wDesTVA = %triml( §TCVdes );
242000131205                 endsl;
242100131205               Else;
242200131205                 select;
242300131205                   when  wTipTVA = 'B';
242400131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
242500131205                               %triml( §TCVdesB );
242600131205                   when  wTipTVA = 'V';
242700131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
242800131205                               %triml( §TCVdesV );
242900131205                   other;
243000131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
243100131205                               %triml( §TCVdes );
243200131205                 endsl;
243300131205               EndIf;
243400131205
243500131205             Else;
243600131205
243700131205               §TCVdesB  = '???' + sk_TVA(xx) + '???';
243800131205               if  wDesTVA = *blank;
243900131205                 wDesTVA = %triml( §TCVdesB );
244000131205               else;
244100131205                 wDesTVA = %trimr( wDesTVA ) + '-' +
244200131205                           %triml( §TCVdesB );
244300131205               endif;
244400131205
244500131205             EndIf;
244600131205
244700131205           EndIf;
244800131205
244900131205         EndFor;
245000131205
245100131205       ENDSR;
245200140203
245300140203       //--------------------------------------------------------------
245400140203       //?Reperimento ed Impostazione a video dell'eventuale           ?
245500140203       //?Ultimo Contatto Precedente.                                  ?
245600140203       //--------------------------------------------------------------
245700140203       BEGSR  sr_GetUltContPrec;
245800140203
245900140203         // -?Reperimento Ultimo contatto precedente?
246000140206         //clear  keyFICAU02;      ?(già impostata nella subr. sr_InzDS1/DO1)?
246100140206         //k_CA2fgs    = CAUfgs;   ?(già impostata nella subr. sr_InzDS1/DO1)?
246200140206         //k_CA2nfv    = CAUnfv;   ?(già impostata nella subr. sr_InzDS1/DO1)?
246300140206         //k_CA2pdr    = CAUpdr;   ?(già impostata nella subr. sr_InzDS1/DO1)?
246400140206         //k_CA2tor    = CAUtor;   ?(già impostata nella subr. sr_InzDS1/DO1)?
246500140206         //k_CA2ogg    = CAUogg;   ?(già impostata nella subr. sr_InzDS1/DO1)?
246600140206         k_CA2dtOins = CAUdtOins;
246700140206
246800140206         chain  %kds( keyFICAU02 )  FICAU002;
246900140206         reade  %kds( keyFICAU02 : 5)  FICAU002;
247000140206
247100140203         // -?NON?trovato alcun contatto precedente?
247200140206         if  %eof(FICAU02L);
247300140203           leavesr;
247400140203         endif;
247500140206
247600140203         // -?Trovato l'ultimo contatto precedente?
247700140203         $UltContPrec = *on;
247800140206         ds_DataOra = CA2dtOins;
247900140203         V1Duco     = 'Ultimo Contatto alle ' +
248000140203                      %editw( ds_DataOra.wTime4 : '  :  ' );
248100140203         // *...+....1....+....2....+.
248200140203         // Ultimo Contatto alle 12:34
248300140203
248400140203       ENDSR;
248500140203
248600140203       //--------------------------------------------------------------
248700140203       //?Reperimento (nuovo) ultimo record per l'oggetto in esame.    ?
248800140203       //--------------------------------------------------------------
248900140203       BEGSR  sr_Verifica_Ogg;
249000140203
249100140203         select;
249200140203
249300140203           when  S1Htor = c_Consegna;
249400140203             $RecUpdated = ( V1Cpdr  <> SaveDS1.V1Cpdr   or
249500140203                             V1Dpdr  <> SaveDS1.V1Dpdr   or
249600140203                             V1Duco  <> SaveDS1.V1Duco   or
249700140203                             V1Dcmd  <> SaveDS1.V1Dcmd   or
249800140203                             V1Dvar  <> SaveDS1.V1Dvar   or
249900140203                             V1Clnp  <> SaveDS1.V1Clnp   or
250000140203                             V1Cnrs  <> SaveDS1.V1Cnrs   or
250100140203                             V1Cnsp  <> SaveDS1.V1Cnsp   or
250200140203                             V1Dtsp  <> SaveDS1.V1Dtsp   or
250300140203                             V1Drsd  <> SaveDS1.V1Drsd   or
250400140203                             V1Dind  <> SaveDS1.V1Dind   or
250500140203                             V1Dlpd  <> SaveDS1.V1Dlpd   or
250600140203                             V1Cncl  <> SaveDS1.V1Cncl   or
250700140203                             V1Cpkb  <> SaveDS1.V1Cpkb   or
250800140203                             V1Dref  <> SaveDS1.V1Dref   or
250900140203                           //V1Igga  <> SaveDS1.V1Igga   or
251000140203                             V1Cgga  <> SaveDS1.V1Cgga   or
251100140203                           //V1Igma  <> SaveDS1.V1Igma   or
251200140203                             V1Cgma  <> SaveDS1.V1Cgma   or
251300140203                           //V1Igva  <> SaveDS1.V1Igva   or
251400140203                             V1Cgva  <> SaveDS1.V1Cgva   or
251500140203                             V1Ccas  <> SaveDS1.V1Ccas   or
251600140203                             V1Cvca  <> SaveDS1.V1Cvca   or
251700140203                             V1Cift  <> SaveDS1.V1Cift   or
251800140203                             V1Cdiv  <> SaveDS1.V1Cdiv   or
251900140203                             V1Dnot8 <> SaveDS1.V1Dnot8  or
252000140203                             V1Dnot9 <> SaveDS1.V1Dnot9  or
252100140203                             V1Cnt1  <> SaveDS1.V1Cnt1   or
252200140203                             V1Cnt2  <> SaveDS1.V1Cnt2   or
252300140203                             V1Cnt3  <> SaveDS1.V1Cnt3   or
252400140203                             V1Cnt4  <> SaveDS1.V1Cnt4   or
252500140203                             V1Nra1  <> SaveDS1.V1Nra1   or
252600140203                             V1Nra2  <> SaveDS1.V1Nra2 );
252700140203
252800140203           when  S1Htor = c_Ritiro;
252900140203             $RecUpdated = ( V1Cpdr    <> SaveDO1.V1Cpdr     or
253000140203                             V1Dpdr    <> SaveDO1.V1Dpdr     or
253100140203                             V1Duco    <> SaveDO1.V1Duco     or
253200140203                             V1Dcmd    <> SaveDO1.V1Dcmd     or
253300140203                             V1Dvar    <> SaveDO1.V1Dvar     or
253400140203                             V1Cpoe    <> SaveDO1.V1Cpoe     or
253500140203                             V1Cnsr    <> SaveDO1.V1Cnsr     or
253600140203                             V1Cnor    <> SaveDO1.V1Cnor     or
253700140203                             V1Cnrv    <> SaveDO1.V1Cnrv     or
253800140203                             V1oraAMda <> SaveDO1.V1oraAMda  or
253900140203                             V1oraAMa  <> SaveDO1.V1oraAMa   or
254000140203                           //V1oraSep  <> SaveDO1.V1oraSep   or
254100140203                             V1oraPMda <> SaveDO1.V1oraPMda  or
254200140203                             V1oraPMa  <> SaveDO1.V1oraPMa   or
254300140203                             V1datN    <> SaveDO1.V1datN     or
254400140203                             V1Drsr    <> SaveDO1.V1Drsr     or
254500140203                             V1Dinr    <> SaveDO1.V1Dinr     or
254600140203                             V1Dlor    <> SaveDO1.V1Dlor     or
254700140203                             V1Dprr    <> SaveDO1.V1Dprr     or
254800140203                             V1Dcar    <> SaveDO1.V1Dcar     or
254900140203                             V1Dnar    <> SaveDO1.V1Dnar     or
255000140203                             V1orr     <> SaveDO1.V1orr      or
255100140203                             V1Dref    <> SaveDO1.V1Dref     or
255200140203                             V1Cncl    <> SaveDO1.V1Cncl     or
255300140203                             V1Cpkg    <> SaveDO1.V1Cpkg     or
255400140203                             V1Cbnc    <> SaveDO1.V1Cbnc     or
255500140203                             V1Cvlm    <> SaveDO1.V1Cvlm     or
255600140203                             V1Cspi    <> SaveDO1.V1Cspi     or
255700140203                             V1Dnot1   <> SaveDO1.V1Dnot1    or
255800140203                             V1Dnot2   <> SaveDO1.V1Dnot2    or
255900140203                             V1Cnt1    <> SaveDO1.V1Cnt1     or
256000140203                             V1Cnt2    <> SaveDO1.V1Cnt2     or
256100140203                             V1nra1    <> SaveDO1.V1nra1     or
256200140203                             V1nra2    <> SaveDO1.V1nra2 );
256300140203
256400140203         endsl;
256500140203
256600140203       ENDSR;
256700140203
256800140203       //--------------------------------------------------------------
256900140203       //?Memorizzazione dati da confrontare per rilevare variazioni.  ?
257000140203       //--------------------------------------------------------------
257100140203       BEGSR  sr_Memo_Dati;
257200140203
257300140203         Select;
257400140203
257500140203           When  S1Htor = c_Consegna;
257600140203             SaveDS1.V1Cpdr  = V1Cpdr;
257700140203             SaveDS1.V1Dpdr  = V1Dpdr;
257800140203             SaveDS1.V1Duco  = V1Duco;
257900140203             SaveDS1.V1Dcmd  = V1Dcmd;
258000140203             SaveDS1.V1Dvar  = V1Dvar;
258100140203             SaveDS1.V1Clnp  = V1Clnp;
258200140203             SaveDS1.V1Cnrs  = V1Cnrs;
258300140203             SaveDS1.V1Cnsp  = V1Cnsp;
258400140203             SaveDS1.V1Dtsp  = V1Dtsp;
258500140203             SaveDS1.V1Drsd  = V1Drsd;
258600140203             SaveDS1.V1Dind  = V1Dind;
258700140203             SaveDS1.V1Dlpd  = V1Dlpd;
258800140203             SaveDS1.V1Cncl  = V1Cncl;
258900140203             SaveDS1.V1Cpkb  = V1Cpkb;
259000140203             SaveDS1.V1Dref  = V1Dref;
259100140203           //SaveDS1.V1Igga  = V1Igga;
259200140203             SaveDS1.V1Cgga  = V1Cgga;
259300140203           //SaveDS1.V1Igma  = V1Igma;
259400140203             SaveDS1.V1Cgma  = V1Cgma;
259500140203           //SaveDS1.V1Igva  = V1Igva;
259600140203             SaveDS1.V1Cgva  = V1Cgva;
259700140203             SaveDS1.V1Ccas  = V1Ccas;
259800140203             SaveDS1.V1Cvca  = V1Cvca;
259900140203             SaveDS1.V1Cift  = V1Cift;
260000140203             SaveDS1.V1Cdiv  = V1Cdiv;
260100140203             SaveDS1.V1Dnot8 = V1Dnot8;
260200140203             SaveDS1.V1Dnot9 = V1Dnot9;
260300140203             SaveDS1.V1Cnt1  = V1Cnt1;
260400140203             SaveDS1.V1Cnt2  = V1Cnt2;
260500140203             SaveDS1.V1Cnt3  = V1Cnt3;
260600140203             SaveDS1.V1Cnt4  = V1Cnt4;
260700140203             SaveDS1.V1Nra1  = V1Nra1;
260800140203             SaveDS1.V1Nra2  = V1Nra2;
260900140203
261000140203           When  S1Htor = c_Ritiro;
261100140203             SaveDO1.V1Cpdr    = V1Cpdr;
261200140203             SaveDO1.V1Dpdr    = V1Dpdr;
261300140203             SaveDO1.V1Duco    = V1Duco;
261400140203             SaveDO1.V1Dcmd    = V1Dcmd;
261500140203             SaveDO1.V1Dvar    = V1Dvar;
261600140203             SaveDO1.V1Cpoe    = V1Cpoe;
261700140203             SaveDO1.V1Cnsr    = V1Cnsr;
261800140203             SaveDO1.V1Cnor    = V1Cnor;
261900140203             SaveDO1.V1Cnrv    = V1Cnrv;
262000140203             SaveDO1.V1oraAMda = V1oraAMda;
262100140203             SaveDO1.V1oraAMa  = V1oraAMa;
262200140203           //SaveDO1.V1oraSep  = V1oraSep;
262300140203             SaveDO1.V1oraPMda = V1oraPMda;
262400140203             SaveDO1.V1oraPMa  = V1oraPMa;
262500140203             SaveDO1.V1datN    = V1datN;
262600140203             SaveDO1.V1Drsr    = V1Drsr;
262700140203             SaveDO1.V1Dinr    = V1Dinr;
262800140203             SaveDO1.V1Dlor    = V1Dlor;
262900140203             SaveDO1.V1Dprr    = V1Dprr;
263000140203             SaveDO1.V1Dcar    = V1Dcar;
263100140203             SaveDO1.V1Dnar    = V1Dnar;
263200140203             SaveDO1.V1orr     = V1orr;
263300140203             SaveDO1.V1Dref    = V1Dref;
263400140203             SaveDO1.V1Cncl    = V1Cncl;
263500140203             SaveDO1.V1Cpkg    = V1Cpkg;
263600140203             SaveDO1.V1Cbnc    = V1Cbnc;
263700140203             SaveDO1.V1Cvlm    = V1Cvlm;
263800140203             SaveDO1.V1Cspi    = V1Cspi;
263900140203             SaveDO1.V1Dnot1   = V1Dnot1;
264000140203             SaveDO1.V1Dnot2   = V1Dnot2;
264100140203             SaveDO1.V1Cnt1    = V1Cnt1;
264200140203             SaveDO1.V1Cnt2    = V1Cnt2;
264300140203             SaveDO1.V1nra1    = V1nra1;
264400140203             SaveDO1.V1nra2    = V1nra2;
264500140203
264600140203         EndSl;
264700140203
264800140203       ENDSR;
264900131122
265000131122       //--------------------------------------------------------------
265100140203       //?Gestione tasto funzionale F6=Accetta premuto da DS1 o DO1.   ?
265200131122       //--------------------------------------------------------------
265300140121       BEGSR  sr_F06D01;
265400131205
265500131205         // -?Riaggancio del record del Contatto ad Autotrasportatore?
265600131205         //  ?da aggiornare (allocandolo)?
265700131205         clear  keyFICAU01;
265800140206         //k_CAUfgs    = S1Hfgs;
265900140206         //k_CAUpdr    = C1Cpdr;
266000140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
266100140206         k_CAUpdr    = FNLRX2ds.iX2aut;
266200131205         k_CAUdtOins = S1HdtOins;
266300131205         k_CAUtor    = S1Htor;
266400131205         k_CAUogg    = S1Hogg;
266500131205         chain  %kds( keyFICAU01 )  FICAU000;
266600140207
266700140207         // -?Rec. NON trovato: si torna alla ri-emissione della?
266800140207         //  ?videata (come NON fosse stato premuto F6)?
266900140207         If  Not %found(FICAU01L)  or  CAUatb <> *blank;
267000140207           ErrGenerico = *on;
267100140207           leavesr;
267200140207         EndIf;
267300131122
267400131205         // -?Richiamo del *pgm FIDNA3R = Scrittura R.A. cieca?
267500131205         //  ?(causale " 40") - vedi file FITGD00F+FITGN00F?
267600131205         exsr  sr_CallFIDNA3;
267700131205
267800131205         // -?Aggiornamento Telefonata?
267900131205         exsr  sr_UpdFICAU;
268000140121
268100140121         // -?Aggiornamento flag "ORM scaricato a PDA"?
268200140128         if  CAUtor = c_Ritiro;
268300140128            exsr  sr_UpdFNORG;
268400140128         endif;
268500131205
268600131205         // -?Impostazione parametro di ritorno?
268700131205         FNLRX2ds.oX2upd = *on;
268800131205
268900131205         // -?Ritorno al subfile?
269000131205         $Video = 'S1';
269100131205         $InzS01 = *on;
269200131122
269300131122       ENDSR;
269400140203
269500140203       //--------------------------------------------------------------
269600140203       //?Reperimento (nuovo) ultimo record per l'oggetto in esame.    ?
269700140203       //--------------------------------------------------------------
269800140203       BEGSR  sr_Ricarica_Ogg;
269900140203
270000140203         // -?Ricerca record relativo all'oggetto in esame con Data/Ora?
270100140203         //  ?inserimento aggiornata?
270200140206         //clear  keyFICAU02;      ?(già impostata nella subr. sr_InzDS1/DO1)?
270300140206         //k_CA2fgs    = CAUfgs;   ?(già impostata nella subr. sr_InzDS1/DO1)?
270400140206         //k_CA2nfv    = CAUnfv;   ?(già impostata nella subr. sr_InzDS1/DO1)?
270500140206         //k_CA2pdr    = CAUpdr;   ?(già impostata nella subr. sr_InzDS1/DO1)?
270600140206         //k_CA2tor    = CAUtor;   ?(già impostata nella subr. sr_InzDS1/DO1)?
270700140206         //k_CA2ogg    = CAUogg;   ?(già impostata nella subr. sr_InzDS1/DO1)?
270800140207         ////k_CA2dtOins = *hival; ?(già impostata nella subr. sr_InzDS1/DO1)?
270900140206
271000140206         chain  %kds( keyFICAU02 : 5 )  FICAU002;
271100140203
271200140206         $RecUpdated = ( %found( FICAU02L ) );
271300140203
271400140203         // -?Trovato: si aggiornano i dati a video?
271500140203         //  ?(di cui verrà effettuata la riemissione)?
271600140203         If  $RecUpdated;
271700140206           k_CAUdtOins = CA2dtOins;
271800140207           chain(N)  %kds( keyFICAU01 )  FICAU000;
271900140206           S1HdtOins   = CA2dtOins;
272000140206           S01hmi   = %int( %subst( CA2dtOins : 9 : 4 ) );
272100140203           $InzS01  = *on;
272200140207           $InzD01  = *on;
272300140203         EndIf;
272400140203
272500140203       ENDSR;
272600131205
272700131205       //--------------------------------------------------------------
272800131205       //?Scrittura Richiesta Assistenza cieca (causale " 40")         ?
272900131205       //--------------------------------------------------------------
273000131205       BEGSR  sr_CallFIDNA3;
273100131205
273200131205         clear  FIDNA3ds;
273300131205         iA3mad = c_IA3mad;
273400131209         select;
273500131209           when  CAUtor = c_Consegna;
273600131209             iA3tor = c_Spedizione;
273700131209           when  CAUtor = c_Ritiro;
273800131209             iA3tor = c_ORM;
273900131209         endsl;
274000131205         iA3ogg = CAUogg;
274100140108         iA3no1 = V1nra1 + V1nra2;
274200131205         iA3no2 = 'AUT: '  + %editc( CAUpdr : 'X' ) +
274300131205                 ' DIST: ' + %editc( CAUnfv : 'X' );
274400131205         if  §CMDdes <> *blank;
274500131205           iA3no2 = %trimr(iA3no2) + ' ' + %trim(§CMDdes);
274600131205         endif;
274700131205         select;
274800131205           when  CAUdesVA <> *blank;
274900131205             //iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(CAUdesVA);
275000131205             if  §CMDdes = *blank;
275100131205               iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(CAUdesVA);
275200131205             else;
275300131205               %subst( iA3no2 : 46 ) = ' VARIAZ: ' + %trim(CAUdesVA);
275400131205             endif;
275500131205           when  wDesTVA  <> *blank;
275600131205             if  §CMDdes = *blank;
275700131205               iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(wDesTVA);
275800131205             else;
275900131205               %subst( iA3no2 : 46 ) = ' VARIAZ: ' + %trim(wDesTVA);
276000131205             endif;
276100131205         endsl;
276200131205         //iA3ara = *blank;
276300131205
276400140108         // -?Richiesta Accettata?
276500140121         //%subst( iA3no2 : %size(iA3no2) - 10 ) = ' (ESEGUITA)';
276600131205
276700131205         FIDNA3R ( kpjba : FIDNA3ds );
276800131205
276900131205       ENDSR;
277000131205
277100131205       //--------------------------------------------------------------
277200131205       //?Aggiornamento telefonata (file FICAU00F).                    ?
277300131205       //--------------------------------------------------------------
277400131205       BEGSR  sr_UpdFICAU;
277500131205
277600131205         // -?Reperimento orario corrente?
277700131205         wTimeStamp = %timestamp();
277800131205
277900131205         // -?Impostazione dei campi relativi all'"ESITO"?
278000131205         CAUflt    = 'T';
278100131205         CAUdtoTel = %subst( %char( %dec( wTimeStamp ) ) : 1 : 14 );
278200140110         CAUesito  = c_Eseguita;
278300131205         CAUpruTel = SDSjobUser;
278400131205         CAUterm   = SDSjobName;
278500131205         CAUesec   = 'T';
278600131205
278700131205
278800131205         // -?Aggiornamento rec. FICAU00F?
278900140203         //_______________
279000140203         update  FICAU000;
279100140203         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
279200131205
279300131205       ENDSR;
279400140121
279500140121       //--------------------------------------------------------------
279600140121       //?Aggiornamento O.R.M. (file FNORG00F).                        ?
279700140121       //--------------------------------------------------------------
279800140121       BEGSR  sr_UpdFNORG;
279900140121
280000140121         // -?Aggancia il record dell'Estensione Testata?
280100140121         clear  keyFNORG01;
280200140121         k_ORGpoe = ds_ORM.wPOE;
280300140121         k_ORGnsr = ds_ORM.wNSR;
280400140121         k_ORGnor = ds_ORM.wNOR;
280500140121         k_ORGnrv = ds_ORM.wNRV;
280600140121         chain  %kds( keyFNORG01 )  FNORG000;
280700140121
280800140121         // -?IMPOSSIBILE?che NON venga reperito il rec. da aggiornare?
280900140121         if  Not %found(FNORG01L);
281000140121           //clear  FNORG000;
281100140121           //CAUpoe = k_ORGpoe;
281200140121           //CAUnsr = k_ORGnsr;
281300140121           //CAUnor = k_ORGnor;
281400140121           //CAUnrv = k_ORGnrv;
281500140121           //CAUpor = .....;
281600140121           //--- oppure: ----------
281700140121           //*inH2 = *on;
281800140121           //$Fine = *on;
281900140121           //ErrGenerico = *on;
282000140121           //leavesr;
282100140121         endif;
282200140121
282300140121         // -?Aggiornamento flag "ORM scaricato a PDA"?
282400140123         dORG01 = ORGflo;
282500140121         select;
282600140121           // -?Inserito => Sì?
282700140121           when  CAUcmd = 'I';
282800140121             dORG01.§ORGpda = 'S';
282900140121           // -?Annullato => No?
283000140121           when  CAUcmd = 'A';
283100140121             clear  dORG01.§ORGpda;
283200140121         endsl;
283300140123         ORGflo = dORG01;
283400140121
283500140121         // -?Aggiornamento rec. FNORG00F?
283600140121         //if  %found(FNORG01L);
283700140121           //_______________
283800140121           update  FNORG000;
283900140121           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
284000140121         //endif;
284100140121
284200140121       ENDSR;
284300131030
284400131030       //--------------------------------------------------------------
284500131030       //?Operazioni finali.                                           ?
284600131030       //--------------------------------------------------------------
284700131030       BEGSR  sr_RoutEnd;
284800131120
284900131120         // -?Chiusura funzioni precedentemente aperte?
285000131120         cloTabella ( c_Tntbe );
285100131030
285200131120         // -?Restituzione eventuali parametri?
285300131121         kpjbu = FNLRX2ds;
285400131104
285500131120         // -?Chiusura *pgm?
285600131030         return;
285700131030
285800131030       ENDSR;
285900131030
286000131030      /end-free
286100131030
286200131120** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
286300131120Superato il numero massimo di telefonate visualizzabili                         1
286400131120Opzione errata                                                                  2
286500131205ERRORE: NON risulta più reperibile questa telefonata                            3
286600140206Attenzione: sono stati variati dei dati. VERIFICARE                             4
