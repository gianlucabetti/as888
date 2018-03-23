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
031600131125     d   CAUtva                            inz
031700131125     d     sk_TVA                     1    inz  overlay(ds_TVA : 1)
031800131125     d                                     dim(10)
031900131030
032000131030       // -?Parametri ricevuti?
032100131030     d KPJBA         e ds
032200131125     d FNLRX2ds      e ds                  inz   qualified
032300131120
032400131120       // -?Tabella CMD = Comandi da impartire al PDA o all'AUT?
032500131127     d dCMD          e ds                  inz
032600131120       // -?Tabella TCV = Tipologie Causali Variazioni bolle?
032700131127     d dTCV          e ds                  inz
032800131121       // -?Tabella 3A = Codici Bolla?
032900131204     d*// ds3A          e ds                  inz
033000131121       // -?Tabella 5E = Tipi Servizio?
033100131121     d ds5E          e ds                  inz
033200140130
033300140130       // -?DS campo DSTFLR di FIDST00F?
033400140130     d dDSTflr       e ds                  inz   qualified
033500131121
033600131121       // -?DS Tipo record "TEL" di FIAPD40F?
033700140131     d dAPD4tel      e ds                  inz   qualified
033800131122
033900131122       // -?DS Tipo record "GEN" di FIAR500F (Estensione)?
034000131125     d dAR5gen       e ds                  inz   qualified
034100131210
034200131210       // -?Flag operativi di FNORM00F?
034300131210     d dORM01        e ds                  inz   qualified
034400140121
034500140121       // -?Flag operativi di FNORG00F?
034600140121     d dORG01        e ds                  inz   qualified
034700131211
034800131211       // -?Ds per rcd "O " FNORE - orari apertura?
034900131211     d dOREorari     e ds                  inz   qualified
035000131030
035100131030       //--------------------------------------------------------------
035200131030       //?Definizione variabili globali.                               ?
035300131030       //--------------------------------------------------------------
035400131030
035500131030       // -?Flags booleani?
035600131030     d $Fine           s               n   inz
035700131030     d $EoF            s               n   inz
035800131030     d $InzS01         s               n   inz(*on)
035900140203     d $InzD01         s               n   inz(*on)
036000131128     d $InzW01         s               n   inz(*on)
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
037800131030
037900131030       // -?Variabili di comodo?
038000131121     d wDate_EUR       s               d   datfmt(*eur)  inz(*loval)
038100131205     d wTimeStamp      s               z   inz(*loval)
038200131127     d wTipTVA         s              1    inz
038300131210     d w0112           s             11  2 inz
038400140108     d wDesTVA         s                   like(V1Dvar)   inz
038500131204
038600131204       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
038700131204
038800131204       //--------------------------------------------------------------
038900131204       // -?Constants?
039000131204       //--------------------------------------------------------------
039100131204       // -?MaxKey - Maximum number of key fields allowed by this program?
039200131204     d MaxKey          c                   const(2)
039300131204       // -?Sort order:?
039400131204       //  ?1 = Sort in an ascending sequence?
039500131204       //  ?2 = Sort in a descending sequence?
039600131204     d Ascendente      c                   const(1)
039700131204     d Discendente     c                   const(2)
039800131204       // -?Key data type:?
039900131204       //  ? 0 = Signed binary?
040000131204       //  ? 2 = Signed zoned decimal?
040100131204       //  ? 3 = Signed packed decimal?
040200131204       //  ? 6 = Character with no national language sort sequence applied,?
040300131204       //  ?     if specified?
040400131204       //  ? 7 = Unsigned packed decimal?
040500131204       //  ?     All numbers will have the sign forced positive ('F'X).?
040600131204       //  ? 8 = Unsigned zoned decimal?
040700131204       //  ?     All numbers will have the sign forced positive ('F'X).?
040800131204       //  ? 9 = Unsigned binary?
040900131204       //  ?14 = Date in form of DD/MM/YY?
041000131204       //  ?15 = Date in form of DD.MM.YYYY?
041100131204     d Numero          c                   const(2)
041200131204     d Carattere       c                   const(6)
041300131204       //
041400131204     d Put             c                   const(1)
041500131204     d EndPut          c                   const(2)
041600131204     d Get             c                   const(3)
041700131204
041800131204       //--------------------------------------------------------------
041900131204       // -?Data Structures?
042000131204       //--------------------------------------------------------------
042100131204       //  ?SflRcd     - The entire subfile record to pass to the sort?
042200131204     d SflRcd          ds                  inz
042300131204     d   S1Hfgs                            inz
042400131204     d   S1HdtOins                         inz
042500131204     d   S1Htor                            inz
042600131204     d   S1Hogg                            inz
042700140110     d   S1Hcmd                            inz
042800131204     d   S01opz                            inz
042900140110     d   S01tor                            inz
043000131204     d   S01cmd                            inz
043100131204     d   S01var                            inz
043200131204     d   S01rsc                            inz
043300131206     d   S01hmi                            inz
043400131204     d   S01ogg                            inz
043500131204     d   Selected                     1a   inz
043600131204       //  ?QLGSCB     - The sort request block for the QLGSORT API?
043700131204      /copy QSYSINC/QRPGLESRC,QLGSORT
043800131204       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
043900131204     d QLGKL                         16    dim(MaxKey)
044000131204     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
044100131204     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
044200131204     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
044300131204     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
044400131204       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
044500131204      /copy QSYSINC/QRPGLESRC,QLGSRTIO
044600131204       //  ?QUSEC      - Error structure for the QLGSORT API?
044700131204      /copy QSYSINC/QRPGLESRC,QUSEC
044800131204
044900131204       //--------------------------------------------------------------
045000131204       // -?Standalone fields?
045100131204       //--------------------------------------------------------------
045200131204     d NotUsed         s             16a   inz
045300131204       //  ?ReturnSize - Size of list returned by sort API?
045400131204     d ReturnSize      s              9b 0 inz
045500131204       //  ?SizeList   - Size of list?
045600131204     d SizeList        s              9b 0 inz
045700131204       //  ?Nrr        - Relative record number for subfile?
045800131204     d Nrr             s              5i 0 inz
045900131204     d RrnLast         s              5i 0 inz
046000131030
046100131030       //--------------------------------------------------------------
046200131030       //?Definizione prototipi procedure.                             ?
046300131030       //--------------------------------------------------------------
046400131030
046500131030       // -?Reperimento dati utente?
046600131030     d TIBS34ds      e ds                  inz
046700131030      /copy gaitrasrc/srcProtoPR,TIBS34R
046800131030
046900131120       // -?Reperimento dati tabelle?
047000131120      /copy gaitrasrc/srcProtoPI,TRULTAB
047100131120      /copy gaitrasrc/srcProtoPR,TRULTAB
047200131120
047300131120       // -?Controllo/Decodifica cliente?
047400131120      /copy gaitrasrc/srcProtoPI,TIBS69R
047500131120      /copy gaitrasrc/srcProtoPR,TIBS69R
047600131121
047700131121       // -?Interrogazione Bolle Arrivi?
047800131121      /copy gaitrasrc/srcProtoPI,FNLR36R
047900131121      /copy gaitrasrc/srcProtoPR,FNLR36R
048000131128
048100131128       // -?Interrogazione Particolarità?
048200131128     d DS7PQRS       e ds                  inz   qualified
048300131128     d   DS7ric      e                     inz('S')
048400131128     d   DS7ges      e                     inz('V')
048500131128       // -?       "             "       Giacenza?
048600131128     d trtb69r         pr                  extpgm('TRTB69R')
048700131128     d   kpjba                             likeds(KPJBA)
048800131128       // -?       "             "       Consegna?
048900131128     d trtb70r         pr                  extpgm('TRTB70R')
049000131128     d   kpjba                             likeds(KPJBA)
049100131128       // -?       "             "       Varie?
049200131128     d trtb73r         pr                  extpgm('TRTB73R')
049300131128     d   kpjba                             likeds(KPJBA)
049400131122
049500131122       // -?Interrogazione O.R.M.?
049600131122     d Parm01          ds                  inz   qualified
049700131125     d   ppoe                         3  0 inz
049800131125     d   pnor                         7  0 inz
049900131125     d   pnsr                         2  0 inz
050000131125     d   pnrv                         2  0 inz
050100131125     d   psce                         1    inz
050200131125     d   pfgs                         3  0 inz
050300131125     d   ppor                         3  0 inz
050400131125     d   pdtr                         8  0 inz
050500131125     d   pmdb                        10    inz
050600131125     d   pprb                        10    inz
050700131125     d   pdts                         8  0 inz
050800131125     d   prmp                         1    inz
050900131125     d   pbrc                         1    inz
051000131125     d   pref                         2    inz
051100131125     d   pmio                         1    inz
051200131122     d FIDNA1ds      e ds                  inz   qualified
051300131122     d fior07r         pr                  extpgm('FIOR07R')
051400131122     d   kpjba                             likeds(KPJBA)
051500131122     d   fidnA1ds                          likeds(FIDNA1ds)
051600140130
051700140130       // -?Monitor attività autotrasportatore con PDA?
051800140130     d FNLVP0ds      e ds                  inz
051900140130      /copy gaitrasrc/srcProtoPR,FNLVP0R2
052000140130
052100140130       // -?Dettaglio Interrogazione DISTINTE?
052200140130     d FIDG311ds     e ds                  inz
052300140130      /copy gaitrasrc/srcProtoPR,FIDG311R
052400131205
052500131205       // -?Scrittura Richiesta Assistenza?
052600131205     d FIDNA3ds      e ds                  inz
052700131205     d fidnA3r         pr                  extpgm('FIDNA3R')
052800131205     d   kpjba                             likeds(KPJBA)
052900131205     d   fidnA3ds                          likeds(FIDNA3ds)
053000131204
053100131204       // -?Ordinamento subfile?
053200131204      /copy gaitrasrc/srcProtoPR,QLGSRTIO
053300131030
053400131030       //--------------------------------------------------------------
053500131030       //?Definizione key-list.                                        ?
053600131030       //--------------------------------------------------------------
053700131030
053800131120       // -?File FICAU01L?
053900131120     d keyFICAU01    e ds                  extname(FICAU01L : *key)
054000131126     d                                     prefix(k_)    inz
054100140206       // -?File FICAU02L?
054200140206     d keyFICAU02    e ds                  extname(FICAU02L : *key)
054300140206     d                                     prefix(k_CA2 : 3)   inz
054400131120
054500131120       // -?File FIAPD01L?
054600131120     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
054700131126     d                                     prefix(k_)    inz
054800131121
054900131121       // -?File FIAPD41L?
055000140131     d keyFIAPD41    e ds                  extname(FIAPD41L : *key)
055100140131     d                                     prefix(k_)    inz
055200131030
055300131030       // -?File FNARB01L?
055400131030     d keyFNARB01    e ds                  extname(FNARB01L : *key)
055500131126     d                                     prefix(k_)    inz
055600131030
055700131030       // -?File FIDST01L?
055800131204     d keyFIDST01    e ds                  extname(FIDST01L : *key)
055900131204     d                                     prefix(k_)    inz
056000131126
056100131126       // -?File FIARN12L?
056200131126     d keyFIARN12    e ds                  extname(FIARN12L : *key)
056300131126     d                                     prefix(k_)    inz
056400131030
056500131122       // -?File FIAR401L?
056600131122     d keyFIAR401    e ds                  extname(FIAR401L : *key)
056700131126     d                                     prefix(k_)    inz
056800131210       // -?File FIAR404L?
056900131210     d keyFIAR404    e ds                  extname(FIAR404L : *key)
057000131210     d                                     prefix(k_AR44:3)    inz
057100131122
057200131122       // -?File FIAR501L?
057300131122     d keyFIAR501    e ds                  extname(FIAR501L : *key)
057400131126     d                                     prefix(k_)    inz
057500131122
057600131122       // -?File FIAR601L?
057700131122     d keyFIAR601    e ds                  extname(FIAR601L : *key)
057800131126     d                                     prefix(k_)    inz
057900131122
058000131122       // -?File FIAR901L?
058100131122     d keyFIAR901    e ds                  extname(FIAR901L : *key)
058200131126     d                                     prefix(k_)    inz
058300131120
058400131120       // -?File FNORM01L?
058500131122     d keyFNORM01    e ds                  extname(FNORM01L : *key)
058600131126     d                                     prefix(k_)    inz
058700140121
058800140121       // -?File FNORT01L?
058900140121     d keyFNORT01    e ds                  extname(FNORT01L : *key)
059000140121     d                                     prefix(k_)    inz
059100140121
059200140121       // -?File FNORG01L?
059300140121     d keyFNORG01    e ds                  extname(FNORG01L : *key)
059400140121     d                                     prefix(k_)    inz
059500131211
059600131211       // -?File FNORE01L?
059700131211     d keyFNORE01    e ds                  extname(FNORE01L : *key)
059800131211     d                                     prefix(k_)    inz
059900131030
060000131030       //--------------------------------------------------------------
060100131030       //?Riepilogo indicatori utilizzati.                             ?
060200131030       //--------------------------------------------------------------
060300131030
060400131030
060500131030       //--------------------------------------------------------------
060600131030       //?M A I N - L I N E                                            ?
060700131030       //--------------------------------------------------------------
060800131030
060900131030     c     *Entry        plist
061000131030     c                   parm                    KPJBA
061100131030
061200131030      /free
061300131030
061400131030       // -?Operazioni iniziali?
061500131030       exsr sr_RoutInz;
061600131030
061700131030       // -?Ciclo di gestione del file video?
061800131030       DoW  $Fine = *off;
061900131030
062000131030         select;
062100131030
062200131120           // -?Gestione subfile S01?
062300131030           when $Video = 'S1';
062400131030             exsr sr_GesS01;
062500131120
062600131120           // -?Gestione videata DS1 (singola spedizione)?
062700131125           when  $Video = 'DS';
062800131125             exsr sr_GesDS1;
062900131120
063000131120           // -?Gestione videata DO1 (singolo O.R.M.)?
063100131125           when  $Video = 'DO';
063200131125             exsr sr_GesDO1;
063300131030
063400131030           // -?? ? ??
063500131030           other;
063600131030             $Fine = *on;
063700131030
063800131030         endsl;
063900131030
064000131030       EndDo;
064100131030
064200131030       // -?Operazioni finali?
064300131030       exsr sr_RoutEnd;
064400131030
064500131030       //--------------------------------------------------------------
064600131030       //?Operazioni iniziali.                                         ?
064700131030       //--------------------------------------------------------------
064800131030       BEGSR  sr_RoutInz;
064900131030
065000131030         // -?Impostazione chiusura?
065100131030         *inLR = *on;
065200131121
065300131121         // -?Ricezione parametri ed impostazione parametri di output?
065400131121         FNLRX2ds = KPJBU;
065500131122         FNLRX2ds.oX2upd = *off;
065600131125         clear  FNLRX2ds.oX2fxx;
065700131122         FNLRX2ds.oX2err = *off;
065800131125         clear  FNLRX2ds.oX2msg;
065900131030
066000131030         // -?Reperimento dati job?
066100131030         exsr  sr_DatiJob;
066200131104
066300131104         // -?Impostazione nome programma a video?
066400131104         V1Tpgm = SDSpgmName;
066500131030
066600131030         // -?Pulizia / impostazione iniziale dei campi chiave?
066700131120         clear  keyFICAU01;
066800140206         clear  keyFICAU02;
066900131120         clear  keyFIAPD01;
067000140131         clear  keyFIAPD41;
067100131104         clear  keyFNARB01;
067200131125         clear  keyFIAR501;
067300131122         clear  keyFIAR401;
067400131126         clear  keyFIARN12;
067500131204         clear  keyFIDST01;
067600131122         clear  keyFNORM01;
067700131211         clear  keyFNORE01;
067800131210         clear  keyFIAR404;
067900131121         k_APDtip   = 'A';
068000140131         k_APD4tipA = 'A';
068100140131         k_APD4trd  = 'TEL';
068200131121         k_AR5trd   = 'GEN';
068300131126         k_ARNcdn   = 'LDV';
068400131126         k_ARNgst   = 'S';
068500131204         k_DSTnpg   = 4;
068600131211         k_OREtrc   = 'O ';
068700131125
068800131125         // -?Caricamento schiera tab. "TCV" = Tipologiche Causali Variazioni bolle?
068900131125         setll  ('TCV')  TNTBE000;
069000131125         reade  ('TCV')  TNTBE000;
069100131125         DoW  Not  %eof(TNTBE01L);
069200131125           xx += 1;
069300131125           sk_TCV(xx)  = TBEke1;
069400131125           sk_TCVd(xx) = TBEuni;
069500131125           reade  ('TCV')  TNTBE000;
069600131125         EndDo;
069700131030
069800131030       ENDSR;
069900131030
070000131030       //--------------------------------------------------------------
070100131030       //?Reperimento Dati del job (Utente/Operativi).                 ?
070200131030       //--------------------------------------------------------------
070300131030       BEGSR  sr_DatiJob;
070400131030
070500131030         in(e) §AzUte;
070600131030         if NOT %error;
070700131030           in(e) §DatiUte;
070800131030         endif;
070900131030         if %error or RSut = *blank;
071000131030           tibs34r ( tibs34ds );
071100131030           in §AzUte;
071200131030           in §DatiUte;
071300131030         endif;
071400131030
071500131030       ENDSR;
071600131030
071700131030       //--------------------------------------------------------------
071800131030       //?Gestione subfile S01.                                        ?
071900131030       //--------------------------------------------------------------
072000131030       BEGSR  sr_GesS01;
072100131030
072200131030         // -?Inizializzazione videata?
072300131030         if  $InzS01 = *on;
072400131030           exsr  sr_InzS01;
072500131030           $InzS01 = *off;
072600131030         endif;
072700140131
072800140131         // -?(Dis)Abilitazione Tasti Funzionali?
072900140131         //  E' meglio lasciarli qui, perchè F1 ri-(dis)abilitato?
073000140131         //  nella subr. "sr_GesDS1": se viene (dis)abilitato là, qui?
073100140131         //  non verrebbe più re-impostato.?
073200140131         F1Attivo  = (S01nrr > 1);
073300140131         F9Attivo  = (S01nrr > *zero);
073400140131         F11Attivo = (S01nrr > 1);
073500131030
073600131030         // -?Emissione Testata & Piede?
073700131120         write  LRX2T01;
073800131120         write  LRX2P01;
073900131030
074000131030         // -?Posizionamento cursore?
074100131030         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
074200131030         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
074300131030         //  ?si visualizza la pagina che vedeva l'utente quando ha?
074400131030         //  ?premuto l'ultimo tasto.?
074500131030         if  C1CsrRrn > *zero;
074600131030           C1RcdNbr = C1CsrRrn;
074700131030         else;
074800131030           C1RcdNbr = 1;
074900131120           write  LRX2S00;             //?(no rec.)?
075000131030         endif;
075100131030
075200131030         // -?Emissione videata?
075300131120         exfmt  LRX2C01;
075400131030
075500131030         reset  ErrGenerico;
075600131030         reset  ErrMessage;
075700131030         clear  V1Dmsg;
075800131030
075900131030         Select;
076000131204
076100131204           // -?F1=Selezione Totale?
076200131204           When  dsp_aid = c_F01;
076300131204             exsr  sr_F01S01;
076400131030
076500131030           // -?F3=Fine?
076600131030           When  dsp_aid = c_F03;
076700131122             FNLRX2ds.oX2fxx = '03';
076800131030             $Fine = *on;
076900140131
077000140131           // -?F4=Distinta?
077100140131           When  dsp_aid = c_F04;
077200140131             exsr  sr_F04S01;
077300131030
077400131030           // -?F5=Rivisualizza?
077500131030           When  dsp_aid = c_F05;
077600131030             $InzS01 = *on;
077700131205
077800131205           // -?F11=Ordinamento Sfl?
077900131205           When  dsp_aid = c_F11;
078000131205             exsr  sr_F11S01;
078100131104
078200131104           // -?F12=Ritorno?
078300131104           When  dsp_aid = c_F12;
078400131122             FNLRX2ds.oX2fxx = '12';
078500131104             $Video = 'D1';
078600131104
078700131104           // -?Roll-Up?
078800131120           When  dsp_aid = c_RollUp;
078900131212             if  SavMAXnrr >= c_MaxRec  and  Not  $EoF;
079000131104               ErrGenerico = *on;
079100131104               ErrMessage  = *on;
079200131120               V1Dmsg = sk_Msg(01);
079300131204             endif;
079400131030
079500131030           // -?SubFile vuoto?
079600131212           When  SavMAXnrr = *zero;
079700131030
079800131030           // -?Invio?
079900131030           Other;
080000131030             // -?Gestione opzioni?
080100131030             exsr  sr_OpzS01;
080200131030             if  ErrGenerico;
080300131030               leavesr;
080400131030             endif;
080500131030
080600131204         EndSl;
080700131030
080800131030       ENDSR;
080900131030
081000131030       //--------------------------------------------------------------
081100131030       //?Inizializzazione subfile S01.                                ?
081200131030       //--------------------------------------------------------------
081300131030       BEGSR  sr_InzS01;
081400131030
081500131030         // -?Spegnimento degli indicatori di errore?
081600131030         %subst(IndDspF : 50) = *off;
081700131125
081800131125         // -?Decodifica autotrasportatore?
081900131205         k_APDpdr = FNLRX2ds.iX2aut;
082000131125         chain  %kds( keyFIAPD01 )  FIAPD000;
082100131125         if  Not %found(FIAPD01L)  or  APDatb <> *blank;
082200131125           clear  APDrsc;
082300131125         endif;
082400131125
082500140131         // -?Reperimento suo n° telefonico?
082600140131         k_APD4pdr = FNLRX2ds.iX2aut;
082700140131         chain  %kds( keyFIAPD41 )  FIAPD400;
082800140131         if  %found(FIAPD41L)  and  APD4atb = *blank;
082900140131           dAPD4tel = APD4uni;
083000140131         endif;
083100131205
083200131205         // -?Reperimento dati della Distinta Consegna?
083300131205         k_DSTfgs = FNLRX2ds.iX2fgs;
083400131205         k_DSTnfv = FNLRX2ds.iX2dst;
083500131205         chain  %kds( keyFIDST01 )  FIDST000;
083600131205         if  Not  %found(FIDST01L);
083700131205           clear  DSTpda;
083800140130           clear  DSTflr;
083900131205         endif;
084000140130
084100140130         dDSTflr = DSTflr;
084200131125
084300131125         // -?Impostazione campi di "testata"?
084400131205         C1Cpdr   = FNLRX2ds.iX2aut;
084500131125         C1Dpdr   = APDrsc;
084600140131         C1TelPdr = dAPD4tel.§APD4tel;
084700140131         //select;
084800140131         //  when  DSTpda = 'O';
084900140131         //    C1Dpda = DSTpda + '=O.R.M.   ';
085000140131         //  when  DSTpda = 'C';
085100140131         //    C1Dpda = DSTpda + '=Consegne ';
085200140131         //  when  DSTpda = 'E';
085300140131         //    C1Dpda = DSTpda + '=ORM+Cons.';
085400140131         //  when  DSTpda = 'N';
085500140131         //    C1Dpda = DSTpda + '=NO       ';
085600140131         //endsl;
085700140131         if  DSTpda = 'N';
085800140131           C1Dpda = 'NO';
085900140131         else;
086000140131           C1Dpda = DSTpda;
086100140131         endif;
086200140131         //select;
086300140131         //  when  dDSTflr.§DSTtstPda = *blank;
086400140131         //  when  dDSTflr.§DSTtstPda = 'O';
086500140131         //    C1Dtst = 'Test O.R.M.   ';
086600140131         //  when  dDSTflr.§DSTtstPda = 'C';
086700140131         //    C1Dtst = 'Test Consegne ';
086800140131         //  when  dDSTflr.§DSTtstPda = 'E';
086900140131         //    C1Dtst = 'Test ORM+Cons.';
087000140131         //endsl;
087100140203         C1Dtst = %xlate( c_Up : c_Lo : dDSTflr.§DSTtstPda );
087200131030
087300131030         // -?Pulizia subfile?
087400131030         SflDsp_N    = *on;
087500131030         SflDspCtl_N = *on;
087600131120         write  LRX2C01;
087700131030         SflDspCtl_N = *off;
087800131030         SflEnd      = *off;
087900131030
088000131104         clear  w_SflPag;
088100131030         clear  C1RcdNbr;
088200131030         clear  C1CsrRrn;
088300131212         clear  SavMAXnrr;
088400131030         clear  S01nrr;
088500131030         clear  V1Dmsg;
088600131030         ErrMessage  = *off;
088700131030         ErrGenerico = *off;
088800131030
088900131104         // -?Caricamento 1ª pagina dei dati nel subfile?
089000131121         clear  keyFICAU01;
089100131121         k_CAUfgs = FNLRX2ds.iX2fgs;
089200131205         k_CAUpdr = FNLRX2ds.iX2aut;
089300131205         //k_CAUnfv = FNLRX2ds.iX2dst;       ?(NON in chiave)?
089400131121         setll  %kds(keyFICAU01 : 2)  FICAU000;
089500131126         exsr  sr_ReadFICAU;
089600131204         exsr  sr_CaricaS01;
089700131030
089800131030         // -?Impaginazione subfile?
089900131030         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
090000131212         if S01nrr > *zero;
090100131030           C1RcdNbr  = 1;
090200131030           C1CsrRrn  = 1;
090300131030         else;
090400131104           clear  C1RcdNbr;
090500131030         endif;
090600131204
090700131204         // -?Ordinamento subfile?
090800131212         if  S01nrr > *zero;
090900131204           exsr  sr_SortSfl;
091000131204         endif;
091100131204
091200131204         // -?Eventuale emissione *msg di superamento limite max di rec.?
091300131204         //  ?visualizzabili nel subfile?
091400131212         if  S01nrr >= c_MaxRec  and  Not $EoF;
091500131204           ErrGenerico = *on;
091600131204           ErrMessage  = *on;
091700131204           V1Dmsg = sk_Msg(01);
091800131204         endif;
091900131030
092000131030       ENDSR;
092100131030
092200131030       //--------------------------------------------------------------
092300131204       //?Caricamento completo del subfile S01                         ?
092400131030       //--------------------------------------------------------------
092500131204       BEGSR  sr_CaricaS01;
092600131104
092700131204         clear  S01nrr;
092800131104         SflNxtChg = *off;
092900131030
093000131204         // -?Ciclo di caricamento completo di dati nel subfile?
093100131204         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
093200131204         //  ?l'eventuale ordinamento)?
093300131204         DoW  S01nrr < c_MaxRec  and  Not $EoF;
093400131122
093500131126           clear  LRX2S01;
093600131126
093700131126           // ·?Reperimento dati Comando da impartire al PDA o all'AUT?
093800131126           If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
093900131127             clear  dCMD;
094000131126             if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
094100131126                              *omit : *omit :
094200131126                              *omit : *omit :
094300131126                              *omit : *omit : *omit : *omit :
094400131126                              *omit : *omit : *omit : *omit :
094500131126                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
094600131126                              = *zero      AND
094700131126                   ds_TNTBE.TBEatb = *blank;
094800131127               dCMD = ds_TNTBE.TBEuni;
094900131126             else;
095000131204               §CMDdes  = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ';
095100131126             endif;
095200131126           Else;
095300131204             §CMDdes  = CAUdesCmd;
095400131126           EndIf;
095500131126
095600131126           // ·?Reperimento dati Tipologiche Causali Variazioni bolle?
095700140131           If  CAUcmd <> c_Cmd_Inserito;
095800140131             If  CAUdesVa = *blank;
095900140131               wTipTVA = 'B';
096000140131               exsr  sr_GetDesTCV;
096100140131               S01var  = wDesTVA;
096200140131             Else;
096300140131               S01var  = CAUdesVa;
096400140131             EndIf;
096500140131           EndIf;
096600131126
096700131204
096800131205           Select;
096900131204             // ·?Reperimento dati Spedizione?
097000131205             When  CAUtor = c_Consegna;
097100131126               exsr  sr_GetSped;
097200131127               //S01rsc = ARBrsd + AR4not;
097300131127               S01rsc = ARBrsd;
097400131204             // ·?Reperimento dati O.R.M.?
097500131205             When  CAUtor = c_Ritiro;
097600131126               exsr  sr_GetORM;
097700131126               S01rsc = ORMrsr;
097800131205           EndSl;
097900131204
098000131204           // ·?Campi hidden?
098100131204           S1Hfgs    = CAUfgs;
098200131204           S1HdtOins = CAUdtOins;
098300131204           S1Htor    = CAUtor;
098400131204           S1Hogg    = CAUogg;
098500140206           //S1Hcmd    = §CMDdes;
098600140206           S1Hcmd    = CAUdesCmd;
098700131204           // ·?Campi di Output?
098800131204           S01tor    = CAUtor;
098900140206           //S01cmd    = §CMDdes;
099000140206           S01cmd    = CAUdesCmd;
099100131205           //S01var    = CAUdesVa;    ?(già "impostato" - o costruito)?
099200131206           S01hmi    = %int( %subst( CAUdtOins : 9 : 4 ) );
099300131204           select;
099400131204             when  CAUtor = c_Consegna;
099500131204               S01ogg = 'Spedizione: ' + %editc( ds_Spedizione.wLNP : 'X' ) +
099600131204                                   ' ' + %editc( ds_Spedizione.wNRS : 'Z' ) +
099700131204                                   ' ' + %editc( ds_Spedizione.wNSP : '3' ) +
099800131204                                  '  ' + %editc( ds_Spedizione.wAAS : 'Z' );
099900131204             when  CAUtor = c_Ritiro;
100000131204               S01ogg = 'O.R.M.: ' + %editc( ds_ORM.wPOE : 'X' ) +
100100131204                               ' ' + %editc( ds_ORM.wNSR : 'Z' ) +
100200131204                               ' ' + %editc( ds_ORM.wNOR : '3' ) +
100300131204                               ' ' + %editc( ds_ORM.wNRV : 'Z' );
100400131204           endsl;
100500131126
100600131126           // -?Scrittura record nel subfile?
100700131206           Ord_x_OraIns = (wOrdSfl = 1);
100800131126           S01nrr += 1;
100900131126           write  LRX2S01;
101000131030
101100131104           // -?Lettura record successivo?
101200131126           exsr  sr_ReadFICAU;
101300131030
101400131030         EndDo;
101500131204
101600131030
101700131212         // -?Memorizzazione n° rec. caricati nel subfile?
101800131212         //  ?(servirà all'ordinamento)?
101900131212         SavMAXnrr = S01nrr;
102000131212
102100131030         // -?Visualizzazione del SFL (se ci sono dati)?
102200131030         SflDsp_N = (S01nrr <= *zero);
102300131030
102400131030         // -?Attivazione del SFLEND?
102500131030         SflEnd = ( $EoF );
102600131030
102700131204         // -?Posizionamento cursore al 1° rcd del subfile?
102800131030         if  S01nrr > *zero;
102900131204           C1RcdNbr = 1;
103000131030         else;
103100131030           clear  C1RcdNbr;
103200131030         endif;
103300131030
103400131030         C1CsrRrn = C1RcdNbr;
103500131030
103600131030       ENDSR;
103700131204
103800131204       //--------------------------------------------------------------
103900131204       //?Gestione tasto funzionale F1 da videata C01 / S01            ?
104000131204       //?F1-Selezione Totale                                          ?
104100131204       //--------------------------------------------------------------
104200131204       BEGSR  sr_F01S01;
104300131204
104400131212         RrnLast  = SavMAXnrr;
104500131204
104600131204         For  S01nrr = 1  To  RrnLast;
104700131204
104800131204           chain  S01nrr  LRX2S01;
104900131204           if  Not %found(FNLRX2D);
105000131204             iter;
105100131204           endif;
105200131204
105300131204           S01opz = 1;
105400131204           SflNxtChg = *on;
105500131206           Ord_x_OraIns = (wOrdSfl = 1);
105600131204           update  LRX2S01;
105700131204
105800131204         EndFor;
105900131204
106000131204       ENDSR;
106100140131
106200140131       //--------------------------------------------------------------
106300140131       //?Gestione tasto funzionale F4 da videata C01 / S01            ?
106400140131       //?F4-Spedizioni nella distinta                                ?
106500140131       //--------------------------------------------------------------
106600140131       BEGSR  sr_F04S01;
106700140131
106800140131         // -?Visualizzazione fasi della distinta se PDA?
106900140131         //  ?(questo *pgm dovrebbe "durare" fino ad agosto 2014...)?
107000140131         If  DSTpda = 'E'  and  dDSTflr.§DSTtstPda = *blank;
107100140131
107200140131           clear  KPJBU;
107300140131           clear  FNLVP0ds;
107400140131           LVP0fgs = FNLRX2ds.iX2fgs;
107500140131           LVP0ndc = FNLRX2ds.iX2dst;
107600140131           //LVP0aut = FNLRX2ds.iX2daut;
107700140131           LVP0aut = DSTpdr;
107800140131           FNLVP0R2( KPJBA : FNLVP0ds );
107900140131
108000140131         // -?Visualizzazione Dettaglio Distinta?
108100140131         Else;
108200140131
108300140131           clear  FIDG311ds;
108400140131           F31fgs     = FNLRX2ds.iX2fgs;
108500140131           F31npg     = DSTnpg;
108600140131           F31numDis  = FNLRX2ds.iX2dst;
108700140131           F31DatDis  = DSTdfv;
108800140131           //F31DatiVis = *blank;
108900140131           F31priDati = c_Consegna;
109000140131
109100140131           kpjbu = FIDG311ds;
109200140131           FIDG311R ( KPJBA );
109300140131
109400140131           FIDG311ds = KPJBU;
109500140131           if  F31tFunz = '03';
109600140131             $Fine = *on;
109700140131             FNLRX2ds.oX2fxx = F31tFunz;
109800140131           endif;
109900140131
110000140131         EndIf;
110100140131
110200140131       ENDSR;
110300131204
110400131204       //--------------------------------------------------------------
110500131205       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
110600131205       //?F11-Cambia ordinamento sfl                                   ?
110700131204       //--------------------------------------------------------------
110800131205       BEGSR  sr_F11S01;
110900131204
111000131204         if  wOrdSfl < 1;
111100131204           wOrdSfl += 1;
111200131204         else;
111300131204           clear  wOrdSfl;
111400131204         endif;
111500131204
111600131204         // -?Se subfile vuoto: nessun record da ordinare?
111700131204         //  ?Altrimenti: basta riordinarlo?
111800131212         if  SavMAXnrr > *zero;
111900131204           exsr  sr_SortSfl;
112000131204         endif;
112100131204
112200131204       ENDSR;
112300131204
112400131204       //--------------------------------------------------------------
112500131204       //?Ordinamento subfile                                          ?
112600131204       //?  This subroutine sorts the subfile records.                 ?
112700131204       //--------------------------------------------------------------
112800131204       BEGSR  sr_SortSfl;
112900131204
113000131212         RrnLast  = SavMAXnrr;
113100131204
113200131204         C1RcdNbr = 1;
113300131204         clear  C1CsrRrn;
113400131204         clear  S01nrr;
113500131204         clear  V1Dmsg;
113600131204         ErrMessage  = *off;
113700131204         SflNxtChg   = *off;
113800131204         ErrGenerico = *off;
113900131204         %subst(IndDspF : 50) = *off;
114000131204
114100131204         //?___________________________________________________________?
114200131204         //?Initialize the key fields to sort on.?
114300131204         //?There is one extra field not in the subfile -- Selected --?
114400131204         //?that is added to the record. This field is used to place?
114500131204         //?selected records in front of nonselected records.?
114600131204         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
114700131204
114800131204         clear  QLGKL;
114900131204         clear  QLGSKL;
115000131204         clear  QLGSCB;
115100131204         clear  QLGSCB00;
115200131204
115300131204         // -?Gestione della scelta ordinamento:?
115400131204         Select;
115500131204
115600131204           //  ?Ordinamento per Comando (desc)?
115700131204           When  wOrdSfl = *zero;
115800131204             // -?2 campi chiave?
115900131204             QLGNBRK  = 2;
116000131204             // -?1° campo: Descrizione Comando (S1Hcmd)?
116100131204             //            ?a posizone   39, 20 byte, char, descending.?
116200131204             QLGSP    = 39;
116300131204             QLGSS    = %size(S1Hcmd);
116400131204             QLGDT    = Carattere;
116500131204             QLGSO    = Discendente;
116600131204             QLGKL(1) = QLGSKL;
116700131204             // -?2° campo: Data Inserimento (S1HdtOins)?
116800131204             //            ?a posizone    4, 14 byte, char, ascending.?
116900131204             QLGSP    = 4;
117000131204             QLGSS    = %size(S1HdtOins);
117100131204             QLGDT    = Carattere;
117200131204             QLGSO    = Ascendente;
117300131204             QLGKL(2) = QLGSKL;
117400131204
117500131205           //  ?Ordinamento per Data/Ora Inserimento?
117600131204           When  wOrdSfl = 1;
117700131204             // -?1 campo chiave?
117800131204             QLGNBRK  = 1;
117900131204             // -?1° campo: Data Inserimento (S1HdtOins)?
118000131204             //            ?a posizone    4, 14 byte, char, ascending.?
118100131204             QLGSP    = 4;
118200131204             QLGSS    = %size(S1HdtOins);
118300131204             QLGDT    = Carattere;
118400131204             QLGSO    = Ascendente;
118500131204             QLGKL(1) = QLGSKL;
118600131204
118700131204         EndSl;
118800131204
118900131204         // -?Load other sort parameters.?
119000131204         QLGLB   = 80 + 16 * MaxKey;
119100131204         QLGRL   = %size(SflRcd) - 1;
119200131204         QLGRT   = 8;
119300131204         QLGOKL  = 80;
119400131204         QLGLKE  = 16;
119500131204         QLGLSS  = 290;
119600131204         // -?Initialize Sort I/O API fields.?
119700131204         QLGRL00 = QLGRL;
119800131204         QLGRC00 = 1;
119900131204         clear  QUSEI;
120000131204         QUSBPRV = %size(QUSEC);
120100131204
120200131204         // -?First step - Initialize the sort routine.?
120300131204         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
120400131204                      SizeList : ReturnSize : QUSEC );
120500131204
120600131204         // -?Next step - Write records to I/O routine.?
120700131204         QLGRT00  = Put;
120800131204         For  Nrr = 1  To  RrnLast;
120900131204           chain  Nrr  LRX2S01;
121000131204           // -?Solo le righe con Selected = 'Y' sono riordinate,?
121100131204           //  ?quindi per fare un ordinamento di tutte le righe?
121200131204           //  ?metto 'Y' sempre.?
121300131204           Selected = 'Y';
121400131204           clear  QUSEI;
121500131204           QUSBPRV  = %size(QUSEC);
121600131204           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
121700131204                         SizeList : NotUsed : QUSEC );
121800131204         EndFor;
121900131204
122000131204         // -?Next step - Signal end of input, clear subfile for reload.?
122100131204         QLGRT00 = EndPut;
122200131204         clear  QUSEI;
122300131204         QUSBPRV = %size(QUSEC);
122400131204         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
122500131204                       SizeList : NotUsed : QUSEC );
122600131204
122700131204         // -?Pulizia subfile?
122800131204         SflDsp_N    = *on;
122900131204         SflDspCtl_N = *on;
123000131204         write  LRX2C01;
123100131204         SflDspCtl_N = *off;
123200131204         SflEnd      = *off;
123300131204
123400131204         // -?Final step - Write the records back to the subfile.?
123500131204         QLGRT00  = Get;
123600131204         For  Nrr = 1  To RrnLast;
123700131204           clear  QUSEI;
123800131204           QUSBPRV = %size(QUSEC);
123900131204           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
124000131204                         QLGRL00  : NotUsed : QUSEC );
124100131204           // -?Ripristino indicatori utilizzati nel sfl rec?
124200131206           Ord_x_OraIns = (wOrdSfl = 1);
124300131204           SflNxtChg  = (S01opz <> *zero);
124400131204           S01nrr = Nrr;
124500131204           write  LRX2S01;
124600131204         EndFor;
124700131204
124800131204         C1CsrRrn = 1;
124900131204
125000131204         // -?Visualizzazione del SFL (se ci sono dati)?
125100131204         SflDsp_N = (S01nrr <= *zero);
125200131204
125300131204         // -?Attivazione del SFLEND?
125400131204         SflEnd = $EoF;
125500131204
125600131205         // -?Impostazione F11 nel piede del subfile?
125700131204         select;
125800131204           when  S01nrr  = *zero;
125900131205             clear  V1PF11;
126000131204           when  wOrdSfl = 0;
126100131205             V1PF11 = 'F11=Ord.OraIns';
126200131204           when  wOrdSfl = 1;
126300131205             V1PF11 = 'F11=Ord. x Cmd';
126400131204         endsl;
126500131204
126600131204       ENDSR;
126700131127
126800131127       //--------------------------------------------------------------
126900131127       //?Gestione opzioni del subfile S01.                            ?
127000131127       //--------------------------------------------------------------
127100131127       BEGSR  sr_OpzS01;
127200131127
127300131127         clear  SavS01csr;
127400131127
127500131127         // -?Ciclo di lettura subfile?
127600131127         readc  LRX2S01;
127700131127
127800131129         DoW  Not %eof(FNLRX2D);
127900131127
128000131127           SflNxtChg = *off;
128100131127           %subst(IndDspF : 50) = *off;
128200131127           C1RcdNbr = S01nrr;
128300131127
128400131127           select;
128500131127
128600131127             // -?Nessuna opzione?
128700131127             when  S01opz = *zero;
128800131127
128900131127             // -?Opz. 1=Gestione?
129000131127             when  S01opz = 1;
129100131127               select;
129200131127                 when  S1Htor = c_Consegna;
129300131127                   $Video  = 'DS';
129400140203                   $InzD01 = *on;
129500131127                   doW  $Video = 'DS';
129600131127                     exsr  sr_GesDS1;
129700131127                   enddo;
129800131127                 when  S1Htor = c_Ritiro;
129900131127                   $Video  = 'DO';
130000140203                   $InzD01 = *on;
130100131127                   doW  $Video = 'DO';
130200131127                     exsr  sr_GesDO1;
130300131127                   enddo;
130400131127                 other;
130500131127                   ErrGenerico = *on;
130600131127                   ErrMessage  = *on;
130700131127                   PosCurOPZ   = *on;
130800131127                   V1Dmsg = sk_Msg(02);
130900131127               endsl;
131000131127
131100131127             // -?Opzione errata?
131200131127             other;
131300131127               ErrGenerico = *on;
131400131127               ErrMessage  = *on;
131500131127               PosCurOPZ   = *on;
131600131127               V1Dmsg = sk_Msg(02);
131700131127
131800131127           endsl;
131900131127
132000131127           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
132100131127           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
132200131127           if  S01opz  <> *zero    and
132300131127              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
132400131127             SavS01csr   = C1RcdNbr;
132500131127           endif;
132600131127
132700131127           // -?Aggiornamento sfl?
132800131127           select;
132900131127             when  ErrMessage;
133000131127               SflNxtChg = *on;
133100131127               C1CsrRrn  = C1RcdNbr;
133200131127             when  ErrGenerico;
133300131127               C1CsrRrn  = C1RcdNbr;
133400131127               clear  S01opz;
133500131127             other;
133600131127               clear  S01opz;
133700131127           endsl;
133800131127
133900131206           Ord_x_OraIns = (wOrdSfl = 1);
134000131206
134100131127           UPDATE  LRX2S01;
134200131127
134300131127           if  ErrGenerico   or   ErrMessage;
134400131127             leave;
134500131127           endif;
134600131127
134700131127           readc  LRX2S01;
134800131127
134900131127         ENDDO;
135000131127
135100131127         // -?Riposizionam. cursore sul record contenente la prima opz.?
135200131127         //  ?(se non sono stati rilevati errori)?
135300131127         if  NOT ErrMessage   and   NOT ErrGenerico   and
135400131127             SavS01csr > *zero;
135500131127           C1CsrRrn = SavS01csr;
135600131127         endif;
135700131127
135800131127       ENDSR;
135900131127
136000131127       //--------------------------------------------------------------
136100131127       //?Lettura file FICAU01L e selezione del record.                ?
136200131127       //--------------------------------------------------------------
136300131127       BEGSR  sr_ReadFICAU;
136400131127
136500131205         reade(N)  %kds(keyFICAU01 : 2)  FICAU000;
136600131127         $EoF = ( %eof(FICAU01L) );
136700131127
136800131127         // -?Selezione singolo record?
136900131212         DoW  Not  $EoF;
137000131127
137100131127           exsr  sr_SelezRec;
137200131127
137300131127           if  $RecOK;
137400131127             leave;
137500131127           endif;
137600131127
137700131205           reade(N)  %kds(keyFICAU01 : 2)  FICAU000;
137800131127           $EoF = ( %eof(FICAU01L) );
137900131127
138000131127         EndDo;
138100131127
138200131127       ENDSR;
138300131121
138400131121       //--------------------------------------------------------------
138500131122       //?Selezione del singolo record in input.                       ?
138600131121       //--------------------------------------------------------------
138700131121       BEGSR  sr_SelezRec;
138800131121
138900131121         $RecOK = *off;
139000131122
139100131122         Select;
139200131121
139300131121           // -?Filiale Gestione diversa?
139400131121           //  ?(IMPOSSIBILE perchè 1ª chiave)?
139500131121           //When  CAUfgs <> FNLRX2ds.iX2fgs;
139600131121           //  setll  (*hival)  FICAU000;
139700131121           //  leavesr;
139800131205
139900131205           // -?Autotrasportatore diverso da quello selezionato?
140000131205           //  ?(IMPOSSIBILE perchè 2ª chiave)?
140100131205           //When  CAUpdr <> FNLRX2ds.iX2aut;
140200131205           //  leavesr;
140300131205
140400131205           // -?Distinta Consegna diversa da quella selezionata?
140500131205           When  FNLRX2ds.iX2dst <> *zero   and
140600131205                 FNLRX2ds.iX2dst <> CAUnfv  and
140700131205                 CAUtor          =  c_Consegna;
140800131205             leavesr;
140900131121
141000131121           // -?Record Annullato?
141100131121           When  CAUatb <> *blank;
141200131121             leavesr;
141300131121
141400131121           // -?Ammessi Tipi Oggetto "C" (consegna) e "R" (ritiro)?
141500131121           When  CAUtor <> c_Consegna  and  CAUtor <> c_Ritiro;
141600131121             leavesr;
141700131126
141800131126           // -?Solo Telefonate SENZA risposta?
141900131126           When  CAUflt <> 'T';
142000131126             leavesr;
142100131126           When  CAUdtOtel <> *blank  and  CAUdtOtel <> *zero;
142200131126             leavesr;
142300131121
142400131121         EndSl;
142500131121
142600131121         // -?Telefonata da caricare nel subfile?
142700131121         $RecOK = *on;
142800131121
142900131121       ENDSR;
143000131121
143100131121       //--------------------------------------------------------------
143200131122       //?Reperimento dati della Spedizione (Consegna).                ?
143300131121       //--------------------------------------------------------------
143400131121       BEGSR  sr_GetSped;
143500131121
143600131121         ds_Spedizione = CAUogg;
143700131121
143800131127         // -?Reperimento dati da Bolle Arrivi: Testate (FNARB)?
143900131121         clear  keyFNARB01;
144000131121         k_ARBaas = ds_Spedizione.wAAS;
144100131121         k_ARBlnp = ds_Spedizione.wLNP;
144200131121         k_ARBnrs = ds_Spedizione.wNRS;
144300131121         k_ARBnsp = ds_Spedizione.wNSP;
144400131121
144500131121         chain  %kds( keyFNARB01 )  FNARB000;
144600131121
144700131121         if  Not  %found(FNARB01L);
144800131121           ARBrsd = *all'? ';
144900131121         endif;
145000131127
145100131127         // -?Estensione ragione sociale destinatario da FIAR4 rec. "D"?
145200131127         clear  keyFIAR401;
145300131127         k_AR4aas = ds_Spedizione.wAAS;
145400131127         k_AR4lnp = ds_Spedizione.wLNP;
145500131127         k_AR4nrs = ds_Spedizione.wNRS;
145600131127         k_AR4nsp = ds_Spedizione.wNSP;
145700131127         k_AR4trc = 'D';
145800131211
145900131127         chain  %kds(keyFIAR401)  FIAR4000;
146000131211
146100131211         if  Not  %found(FIAR401L);
146200131211           clear  AR4not;
146300131211         endif;
146400131121
146500131121       ENDSR;
146600131121
146700131121       //--------------------------------------------------------------
146800131122       //?Reperimento dati dell'Ordine Ritiro Merce (Ritiro).          ?
146900131121       //--------------------------------------------------------------
147000131121       BEGSR  sr_GetORM;
147100131121
147200131121         ds_ORM = CAUogg;
147300131121
147400131121         clear  keyFNORM01;
147500131122         k_ORMpoe = ds_ORM.wPOE;
147600131122         k_ORMnsr = ds_ORM.wNSR;
147700131122         k_ORMnor = ds_ORM.wNOR;
147800131122         k_ORMnrv = ds_ORM.wNRV;
147900131121
148000131121         chain  %kds( keyFNORM01 )  FNORM000;
148100131121
148200131121         if  Not  %found(FNORM01L);
148300131121           ORMrsr = *all'? ';
148400131210           clear  dORM01;
148500131210         else;
148600131210           dORM01 = ORMflo;
148700131121         endif;
148800131121
148900131121       ENDSR;
149000131030
149100131030       //--------------------------------------------------------------
149200131120       //?Gestione videata DS1 (spedizione).                           ?
149300131030       //--------------------------------------------------------------
149400140203       BEGSR  sr_GesDS1;
149500140207
149600140207         // -?Inizializzazione videata?
149700140207         //  ?(sempre e comunque)?
149800140207         clear  $RecUpdated;
149900140207         exsr  sr_InzDS1;
150000140207         exsr  sr_AttrVisDS1;
150100140207         exsr  sr_AbilFxxDS1;
150200140207
150300140207         // -?Verifica dell'oggetto: confronto con i dati precedentemente?
150400140207         //  ?memorizzati  (qualcuno dei quali potrebbe essere stato?
150500140207         //  ?modificato da un altro utente)?
150600140207         if  Not  $InzD01;
150700140207           exsr  sr_Verifica_Ogg;
150800140207         endif;
150900140207
151000140207         $InzD01 = *off;
151100140207
151200140207         if  $RecUpdated;
151300140207           ErrGenerico = *on;
151400140207           ErrMessage  = *on;
151500140207           V1Dmsg = sk_Msg(04);
151600140207         endif;
151700131120
151800131120         // -?Emissione videata?
151900131120         write  LRX2T01;
152000131120         exfmt  LRX2DS1;
152100131120
152200131120         clear  V1Dmsg;
152300131120         reset  ErrMessage;
152400140203         reset  ErrGenerico;
152500140203         reset  $RecUpdated;
152600140203
152700140203         // -?Memorizzazione dati della videata per verificarne variazioni?
152800140203         if  dsp_aid <> c_F03  and  dsp_aid <> c_F12;
152900140203           exsr  sr_Memo_Dati;
153000140203         endif;
153100131120
153200131120         SELECT;
153300131127
153400131127           // -?F1=Ultimo Contatto?
153500131127           WHEN  dsp_aid = c_F01;
153600131128             exsr  sr_GesW01;
153700131122
153800140203           // -?F3=Fine?
153900140203           //  ?(ritorno al subfile S01 interrompendone la lettura)?
154000131122           WHEN  dsp_aid = c_F03;
154100140108             //clear  $Video;
154200140108             //FNLRX2ds.oX2fxx = '03';
154300140108             //ErrGenerico = *on;
154400140108             //$Fine = *on;
154500140108             ErrGenerico = *on;
154600140108             $Video = 'S1';
154700131120
154800131120           // -?F7=Interrogazione spedizione?
154900131120           WHEN  dsp_aid = c_F07;
155000131121             exsr  sr_VisualSped;
155100131120
155200131120           // -?F12=Ritorno?
155300140203           //  ?(ritorno al subfile S01 proseguendone la lettura)?
155400131120           WHEN  dsp_aid = c_F12;
155500140108             //ErrGenerico = *on;
155600131120             $Video = 'S1';
155700131120
155800140108           // -?Invio o F6=Accettato?
155900131120           OTHER;
156000131120             exsr  sr_CtrDS1;
156100131122             if  ErrGenerico = *on;
156200131120               leavesr;
156300131120             endif;
156400140108             // -?F6=Accettato (Esegue)?
156500140108             if  dsp_aid = c_F06;
156600140121               exsr  sr_F06D01;
156700131205             endif;
156800131120
156900131120         ENDSL;
157000131030
157100131030       ENDSR;
157200131121
157300131121       //--------------------------------------------------------------
157400131121       //?Inizializazzione videata DS1 (spedizione).                   ?
157500131121       //--------------------------------------------------------------
157600131121       BEGSR  sr_InzDS1;
157700131121
157800131122         ds_Spedizione = S1Hogg;
157900140206
158000140206         // -?Impostazione chiave di accesso per la v.l. 02L?
158100140206         clear  keyFICAU02;
158200140206         k_CA2fgs    = FNLRX2ds.iX2fgs;
158300140206         k_CA2nfv    = FNLRX2ds.iX2dst;
158400140206         k_CA2pdr    = FNLRX2ds.iX2aut;
158500140206         k_CA2tor    = S1Htor;
158600140206         k_CA2ogg    = S1Hogg;
158700140207         k_CA2dtOins = *hival;
158800131121
158900131121         // -?Pulizia videata?
159000131121         clear  LRX2DS1;
159100131128         clear  $UltContPrec;
159200131122
159300131122         // -?Contatto ad Autotrasportatore?
159400140203         clear  sk_TVA;
159500131122         clear  keyFICAU01;
159600140207         //k_CAUfgs    = S1Hfgs;
159700140207         //k_CAUpdr    = C1Cpdr;
159800140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
159900140206         k_CAUpdr    = FNLRX2ds.iX2aut;
160000131122         k_CAUdtOins = S1HdtOins;
160100131122         k_CAUtor    = S1Htor;
160200131125         k_CAUogg    = S1Hogg;
160300140203         chain(N)  %kds( keyFICAU01 )  FICAU000;
160400140203
160500140203         // -?Rec. NON trovato: ricerca con data/ora inserimento più?
160600140203         //  ?recenti (segnalando errore SE non trovato nuovo rec.)?
160700140203         //  ?e riemissione della videata?
160800140203         if  Not %found(FICAU01L)  or  CAUatb <> *blank;
160900140203
161000140203           exsr  sr_Ricarica_Ogg;
161100140203
161200140203           if  Not $RecUpdated;
161300140203             ErrGenerico = *on;
161400140203             ErrMessage  = *on;
161500140203             V1Dmsg = sk_Msg(03);
161600140203             leavesr;
161700140203           endif;
161800140203
161900140203         endif;
162000131121
162100131121         // -?Decodifica autotrasportatore?
162200140203         exsr  sr_GetAUT;
162300131122
162400131122         // -?Comando da impartire al PDA o all'AUT?
162500140203         exsr  sr_GetCMD;
162600131122
162700131122         // -?Tipologica Causale Variazione bolla?
162800140131         //  ?(SE NON "INSERITO")?
162900140203         exsr  sr_GetTVA;
163000131121
163100131121         // -?Spedizione?
163200131122         exsr  sr_GetSped;
163300131121         if  Not  %found(FNARB01L);
163400131121           leavesr;
163500131121         endif;
163600131122
163700131122         V1Clnp = ds_Spedizione.wLNP;
163800131122         V1Cnrs = ds_Spedizione.wNRS;
163900131122         V1Cnsp = ds_Spedizione.wNSP;
164000131204         //wDate_EUR = %date( (ds_Spedizione.wAAS * 10000) + ARBmgs : *iso );
164100131204         //V1Cdsp = %dec( wDate_EUR );
164200131127
164300131127         // -?Colli?
164400131127         V1Cncl = ARBncl;
164500131127
164600131127         // -?Peso?
164700131127         V1Cpkb = ARBpkb;
164800131127
164900131127         // -?Particolarità?
165000131127         V1Cgga = ARBgga;
165100131127         V1Cgma = ARBgma;
165200131127         V1Cgva = ARBgva;
165300131126
165400131127         // -?Codice Bolla (tab. "3A")?
165500131204         //clear  ds3A;
165600131204         //if  getTabella ( c_Tabel : '3A' : ARBcbo : *omit :
165700131204         //                 *omit : *omit :
165800131204         //                 *omit : *omit :
165900131204         //                 *omit : *omit : *omit : *omit :
166000131204         //                 *omit : *omit : *omit : *omit :
166100131204         //                 ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
166200131204         //                 = *zero      AND
166300131204         //      ds_TNTBE.TBEatb = *blank;
166400131204         //  ds3A = ds_TNTBE.TBEuni;
166500131204         //else;
166600131204         //  §3Ades = *all'? ';
166700131204         //endif;
166800131204         //
166900131204         //V1Ccbo = ARBcbo;
167000131204         //V1Dcbo = §3Ades;
167100131126
167200131126         // -?Tipo Servizio (tab. "5E")?
167300131126         clear  ds5E;
167400131126         if  getTabella ( c_Tabel : '5E' : ARBtsp : *omit :
167500131126                          *omit : *omit :
167600131126                          *omit : *omit :
167700131126                          *omit : *omit : *omit : *omit :
167800131126                          *omit : *omit : *omit : *omit :
167900131126                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
168000131126                          = *zero      AND
168100131126               ds_TNTBE.TBEatb = *blank;
168200131126           ds5E = ds_TNTBE.TBEuni;
168300131126         else;
168400131126           §5Edes = *all'? ';
168500131126         endif;
168600131126
168700131205         evalr V1Dtsp = %trimr(§5Ed08);
168800131122
168900131127         // -?Cliente Mittente?
169000131127         //if  ARBccm <> *zero;
169100131127         //  V1Cccm = ARBccm;
169200131127         //else;
169300131127         //  if  %subst(§3Atb1:1:1) = 'F';
169400131127         //    V1Cccm = ARBksc;
169500131127         //  endif;
169600131127         //endif;
169700131209         //V1Drsm = ARBrsm;
169800131209         //V1Drmo = ARBrmo;
169900131122
170000131127         //// -?Cliente Destinatario?
170100131122         //if  %subst(§3Atb1:1:1) = 'A';
170200131122         //  V1Cccd = ARBksc;
170300131122         //endif;
170400131122         //// -?Se c'è 2ª bolla imposto LNA9999?
170500131122         //if  §3Atb2 <> *blank;
170600131122         //  V1Cccd = (ARBlna * 10000) + 9999;
170700131122         //endif;
170800131209         V1Drsd = ARBrsd + AR4not;
170900131209         V1Dind = ARBind;
171000131209         //V1Dcad = ARBcad;
171100131209         //V1Dlpd = %trim( ARBlod ) + '  ' + ARBprd + ' ' + ARBnzd;
171200131209         V1Dlpd = %trim( ARBcad ) + ' ' + %trim( ARBlod ) + '  ' +
171300131209                  ARBprd + ' ' + ARBnzd;
171400131121
171500131121         // -?Distinta?
171600131205         ////k_DSTfgs = (ARBpdc / 10000);
171700131205         ////k_DSTnfv = ARBndc;
171800131205         //k_DSTfgs = FNLRX2ds.iX2fgs;
171900131205         //k_DSTnfv = FNLRX2ds.iX2dst;
172000131205         //chain  %kds( keyFIDST01 )  FIDST000;
172100131205         //if  Not  %found(FIDST01L);
172200131205         //  clear  DSTpda;
172300131205         //endif;
172400131121
172500131122         // -?Referente Consegna (estensione testata bolla: trk "GEN")?
172600131121         clear  dAR5gen;
172700131121         k_AR5aas = ds_Spedizione.wAAS;
172800131121         k_AR5lnp = ds_Spedizione.wLNP;
172900131121         k_AR5nrs = ds_Spedizione.wNRS;
173000131121         k_AR5nsp = ds_Spedizione.wNSP;
173100131205         //k_AR5trd   = 'GEN';        ?(già così)?
173200131121         chain  %kds(keyFIAR501 : 5)  FIAR5000;
173300131121         if  %found(FIAR501L)  and  AR5atb = *blank;
173400131121           dAR5gen = AR5uni;
173500131121         endif;
173600131122
173700131204         select;
173800131204           when  dAR5gen.§AR5ref  <> *blank  and
173900131204                 dAR5gen.§AR5telD <> *blank;
174000131209             V1Dref = %trimr( dAR5gen.§AR5ref ) + ' - ' +
174100131204                      %triml( dAR5gen.§AR5telD );
174200131204           when  dAR5gen.§AR5ref  <> *blank;
174300131209             V1Dref = dAR5gen.§AR5ref;
174400131204           when  dAR5gen.§AR5telD <> *blank;
174500131209             V1Dref = dAR5gen.§AR5telD;
174600131204         endsl;
174700131121
174800131121         // -?Dati C/Assegno?
174900131121         chain  %kds( keyFNARB01 )  FIAR9000;
175000131122         if  Not %found(FIAR901L);
175100131121           clear  AR9cas;
175200131121           clear  AR9vca;
175300131121         endif;
175400131122
175500131122         V1Ccas = AR9cas;
175600131122         V1Cvca = AR9vca;
175700131121
175800131122         // -?Dati Fattura (Tassazione Bolla - filiale)?
175900131122         clear  keyFIAR601;
176000131121         k_AR6aas = ds_Spedizione.wAAS;
176100131121         k_AR6lnp = ds_Spedizione.wLNP;
176200131121         k_AR6nrs = ds_Spedizione.wNRS;
176300131121         k_AR6nsp = ds_Spedizione.wNSP;
176400131122         // -?C'è 2ª bolla (?)?
176500131122         k_AR6trc = '2';
176600131122         clear  AR6div;
176700131122         clear  AR6ift;
176800131122         chain  %kds(keyFIAR601)  FIAR6000;
176900131122         If  %found(FIAR601L);
177000131122           if  AR6ift = *zero;
177100131122             clear  AR6div;
177200131122           endif;
177300131122         // -?1ª bolla?
177400131122         Else;
177500131122           k_AR6trc = '1';
177600131122           chain  %kds(keyFIAR601)  FIAR6000;
177700131122           if  %found(FIAR601L)  and  AR6ift = *zero;
177800131122             clear  AR6div;
177900131122           endif;
178000131122         EndIf;
178100131122
178200131122         V1Cift = AR6ift;
178300131122         V1Cdiv = AR6div;
178400131121
178500131126         // -?Note LdV?
178600131122         clear  keyFIAR401;
178700131122         k_AR4aas = ds_Spedizione.wAAS;
178800131122         k_AR4lnp = ds_Spedizione.wLNP;
178900131122         k_AR4nrs = ds_Spedizione.wNRS;
179000131122         k_AR4nsp = ds_Spedizione.wNSP;
179100131122         k_AR4trc = '8';
179200131122         chain  %kds(keyFIAR401)  FIAR4000;
179300131122         if  %found(FIAR401L);
179400131122           V1Dnot8 = AR4not;
179500131122         endif;
179600131122         k_AR4trc = '9';
179700131122         chain  %kds(keyFIAR401)  FIAR4000;
179800131122         if  %found(FIAR401L);
179900131122           V1Dnot9 = AR4not;
180000131122         endif;
180100131122
180200131126         // -?Note Bolla?
180300131126         k_ARNaas = ds_Spedizione.wAAS;
180400131126         k_ARNlnp = ds_Spedizione.wLNP;
180500131126         k_ARNnrs = ds_Spedizione.wNRS;
180600131126         k_ARNnsp = ds_Spedizione.wNSP;
180700131205         //k_ARNcdn = 'LDV';          ?(già impostato)?
180800131205         //k_ARNgst = 'S';            ?(già impostato)?
180900131126         setll  %kds(keyFIARN12 : 6)  FIARN000;
181000131126         reade  %kds(keyFIARN12 : 6)  FIARN000;
181100131126         DoW  Not %eof(FIARN12L);
181200131126           select;
181300131126             when  V1Cnt1 = *blank;
181400131126               V1Cnt1 = ARNnob;
181500131126             when  V1Cnt2 = *blank;
181600131126               V1Cnt2 = ARNnob;
181700131126             when  V1Cnt3 = *blank;
181800131126               V1Cnt3 = ARNnob;
181900131126             when  V1Cnt4 = *blank;
182000131126               V1Cnt4 = ARNnob;
182100140121             other;
182200140121               leave;
182300131126           endsl;
182400131126           reade  %kds(keyFIARN12 : 6)  FIARN000;
182500131126         EndDo;
182600131128
182700140108         // -?Note R.A.: solo input?
182800131122
182900131126         // -?Reperimento Ultimo contatto precedente?
183000140203         exsr  sr_GetUltContPrec;
183100131121
183200131121       ENDSR;
183300131127
183400131127       //--------------------------------------------------------------
183500131127       //?Impostazione attributi di visualizzazione video DS1.         ?
183600131127       //--------------------------------------------------------------
183700131127       BEGSR  sr_AttrVisDS1;
183800131127
183900131204         // ·?Ultimo Contatto?
184000131204         RevImgUCO  = (V1Duco <> *blank);
184100131204
184200131127         // ·?Comando?
184300131127         RevImgCMD  = (V1Dcmd <> *blank);
184400131204
184500131204         // ·?Variazioni?
184600131204         RevImgVAR  = (V1Dvar <> *blank);
184700131127
184800131127         // ·?Tipo Servizio?
184900131127         RevImgTSP  = (ARBtsp = 'E'  or  ARBtsp = 'H');
185000131127
185100131127         // ·?Fermo Deposito?
185200131127         VisualFDEP = (ARBffd <> *blank);
185300131127
185400131127         // ·?Contrassegno?
185500131129         VisualCASS = (V1Ccas > *zero);
185600131127
185700131127         // ·?Importo Fattura?
185800131129         VisualIFT  = (V1Cift > *zero);
185900131129
186000131129         // ·?Interrogazione Particolarità Giacenza?
186100131129         VisPartGiac  = (V1Cgga <> *blank);
186200131129
186300131129         // ·?Interrogazione Particolarità Consegna?
186400131129         VisPartCons  = (V1Cgma <> *blank);
186500131129
186600131129         // ·?Interrogazione Particolarità Varia?
186700131129         VisPartVarie = (V1Cgva <> *blank);
186800131127
186900131127       ENDSR;
187000131128
187100131128       //--------------------------------------------------------------
187200131128       //?Abilitazione tasti funzionali video DS1.                     ?
187300131128       //--------------------------------------------------------------
187400131128       BEGSR  sr_AbilFxxDS1;
187500131128
187600131128         // -?F1 = Ultimo Contatto?
187700131128         F1Attivo = ($UltContPrec = *on);
187800131128
187900131128       ENDSR;
188000131121
188100131121       //--------------------------------------------------------------
188200131121       //?Controllo videata DS1 (spedizione).                          ?
188300131121       //--------------------------------------------------------------
188400131121       BEGSR  sr_CtrDS1;
188500131121
188600131121         // -?Spegnimento indicatori di posizionamento cursore?
188700131127         %subst(IndDspF : 60) = *off;
188800131127
188900131127         // -?Interrogazione Particolarità Giacenza?
189000131127         if  V1Igga = '?';
189100131128           reset  ds7PQRS;
189200131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
189300131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
189400131128           ds7PQRS.DS7cod = V1Cgga;
189500131127           KPJBU = ds7PQRS;
189600131127           trtb69r ( kpjba );
189700131127           clear  V1Igga;
189800131127           ErrGenerico = *on;
189900131127           leavesr;
190000131127         endif;
190100131127
190200131127         // -?Interrogazione Particolarità Consegna?
190300131127         if  V1Igma = '?';
190400131128           reset  ds7PQRS;
190500131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
190600131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
190700131128           ds7PQRS.DS7cod = V1Cgma;
190800131127           KPJBU = ds7PQRS;
190900131127           trtb70r ( kpjba );
191000131127           clear  V1Igma;
191100131127           ErrGenerico = *on;
191200131127           leavesr;
191300131127         endif;
191400131127
191500131127         // -?Interrogazione Particolarità Varie?
191600131127         if  V1Igva = '?';
191700131128           reset  ds7PQRS;
191800131205           //ds7PQRS.DS7ric = 'S';    ?(già così)?
191900131205           //ds7PQRS.DS7ges = 'V';    ?(già così)?
192000131128           ds7PQRS.DS7cod = V1Cgva;
192100131127           KPJBU = ds7PQRS;
192200131127           trtb73r ( kpjba );
192300131127           clear  V1Igva;
192400131127           ErrGenerico = *on;
192500131127           leavesr;
192600131127         endif;
192700131128
192800131128         clear  V1Igga;
192900131128         clear  V1Igma;
193000131128         clear  V1Igva;
193100131121
193200131121       ENDSR;
193300131121
193400131121       //--------------------------------------------------------------
193500131122       //?Richiamo *pgm x Interrogazione Spedizione (in filiale).      ?
193600131121       //--------------------------------------------------------------
193700131121       BEGSR  sr_VisualSped;
193800131121
193900131121         clear  FNLR36ds;
194000131121         FNLR36ds.LR36aas = k_ARBaas;
194100131121         FNLR36ds.LR36lnp = k_ARBlnp;
194200131121         FNLR36ds.LR36nrs = k_ARBnrs;
194300131121         FNLR36ds.LR36nsp = k_ARBnsp;
194400131121         FNLR36ds.LR36flg = '1';
194500131121         kpjbu = FNLR36ds;
194600131121
194700131121         fnlr36r ( kpjba );
194800131121
194900131121         FNLR36ds = kpjbu;
195000131121         if  FNLR36ds.LR36f03 = *on;
195100131121           $Video = 'S1';
195200131121           ErrGenerico = *on;
195300131121         endif;
195400131121
195500131121       ENDSR;
195600131209
195700131209       //--------------------------------------------------------------
195800131209       //?Gestione videata DO1 (O.R.M.).                               ?
195900131209       //--------------------------------------------------------------
196000131209       BEGSR  sr_GesDO1;
196100131209
196200140207         // -?Inizializzazione videata?
196300140207         clear  $RecUpdated;
196400140207         exsr  sr_InzDO1;
196500140207         exsr  sr_AttrVisDO1;
196600140207         exsr  sr_AbilFxxDO1;
196700140207
196800140207         // -?Verifica dell'oggetto: confronto con i dati precedentemente?
196900140207         //  ?memorizzati  (qualcuno dei quali potrebbe essere stato?
197000140207         //  ?modificato da un altro utente)?
197100140207         if  Not  $InzD01;
197200140207           exsr  sr_Verifica_Ogg;
197300140207         endif;
197400140207
197500140207         $InzD01 = *off;
197600140207
197700140207         if  $RecUpdated;
197800140207           ErrGenerico = *on;
197900140207           ErrMessage  = *on;
198000140207           V1Dmsg = sk_Msg(04);
198100140207         endif;
198200131209
198300131209         // -?Emissione videata?
198400131209         write  LRX2T01;
198500131209         exfmt  LRX2DO1;
198600131209
198700131209         clear  V1Dmsg;
198800131209         reset  ErrMessage;
198900131209         reset  ErrGenerico;
199000140203         reset  $RecUpdated;
199100140203
199200140203         // -?Memorizzazione dati della videata per verificarne variazioni?
199300140203         if  dsp_aid <> c_F03  and  dsp_aid <> c_F12;
199400140203           exsr  sr_Memo_Dati;
199500140203         endif;
199600131209
199700131209         SELECT;
199800131209
199900131209           // -?F1=Ultimo Contatto?
200000131209           WHEN  dsp_aid = c_F01;
200100131209             exsr  sr_GesW01;
200200131209
200300131209           // -?F3=Fine?
200400140203           //  ?(ritorno al subfile S01 interrompendone la lettura)?
200500131209           WHEN  dsp_aid = c_F03;
200600140108             //clear  $Video;
200700140108             //FNLRX2ds.oX2fxx = '03';
200800140108             //ErrGenerico = *on;
200900140108             //$Fine = *on;
201000140108             ErrGenerico = *on;
201100140108             $Video = 'S1';
201200131209
201300131211           // -?F8=Interrogazione O.R.M.?
201400131211           WHEN  dsp_aid = c_F08;
201500131209             exsr  sr_VisualORM;
201600131209
201700131209           // -?F12=Ritorno?
201800140203           //  ?(ritorno al subfile S01 proseguendone la lettura)?
201900131209           WHEN  dsp_aid = c_F12;
202000140108             //ErrGenerico = *on;
202100131209             $Video = 'S1';
202200131209
202300140108           // -?Invio o F6=Accettato?
202400131209           OTHER;
202500131209             exsr  sr_CtrDO1;
202600131209             if  ErrGenerico = *on;
202700131209               leavesr;
202800131209             endif;
202900140108             // -?F6=Accettato (Esegue)?
203000140108             if  dsp_aid = c_F06;
203100140121               exsr  sr_F06D01;
203200131209             endif;
203300131209
203400131209         ENDSL;
203500131209
203600131209       ENDSR;
203700131209
203800131209       //--------------------------------------------------------------
203900131209       //?Inizializzazione videata DO1 (O.R.M.).                       ?
204000131209       //--------------------------------------------------------------
204100131209       BEGSR  sr_InzDO1;
204200131209
204300131209         ds_ORM = S1Hogg;
204400140207
204500140207         // -?Impostazione chiave di accesso per la v.l. 02L?
204600140207         clear  keyFICAU02;
204700140207         k_CA2fgs    = FNLRX2ds.iX2fgs;
204800140207         k_CA2nfv    = FNLRX2ds.iX2dst;
204900140207         k_CA2pdr    = FNLRX2ds.iX2aut;
205000140207         k_CA2tor    = S1Htor;
205100140207         k_CA2ogg    = S1Hogg;
205200140207         k_CA2dtOins = *hival;
205300131209
205400131209         // -?Pulizia videata?
205500131209         clear  LRX2DO1;
205600131209         clear  $UltContPrec;
205700131209
205800131209         // -?Contatto ad Autotrasportatore?
205900140203         clear  sk_TVA;
206000131209         clear  keyFICAU01;
206100140206         //k_CAUfgs    = S1Hfgs;
206200140206         //k_CAUpdr    = C1Cpdr;
206300140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
206400140206         k_CAUpdr    = FNLRX2ds.iX2aut;
206500131209         k_CAUdtOins = S1HdtOins;
206600131209         k_CAUtor    = S1Htor;
206700131209         k_CAUogg    = S1Hogg;
206800140203         chain(N)  %kds( keyFICAU01 )  FICAU000;
206900140203
207000140203         // -?Rec. NON trovato: ricerca con data/ora inserimento più?
207100140203         //  ?recenti (segnalando errore SE non trovato nuovo rec.)?
207200140203         //  ?e riemissione della videata?
207300131209         if  Not %found(FICAU01L)  or  CAUatb <> *blank;
207400140203
207500140203           exsr  sr_Ricarica_Ogg;
207600140203
207700140203           if  Not $RecUpdated;
207800140203             ErrGenerico = *on;
207900140203             ErrMessage  = *on;
208000140203             V1Dmsg = sk_Msg(03);
208100140203             leavesr;
208200140203           endif;
208300140203
208400131209         endif;
208500131209
208600131209         // -?Decodifica autotrasportatore?
208700140203         exsr  sr_GetAUT;
208800131209
208900131209         // -?Comando da impartire al PDA o all'AUT?
209000140203         exsr  sr_GetCMD;
209100131209
209200131209         // -?Tipologica Causale Variazione bolla?
209300140131         //  ?(SE NON "INSERITO")?
209400140203         exsr  sr_GetTVA;
209500131209
209600131209         // -?O.R.M.?
209700131209         exsr  sr_GetORM;
209800131209         if  Not  %found(FNORM01L);
209900131209           leavesr;
210000131209         endif;
210100131209
210200131209         V1Cpoe = ds_ORM.wPOE;
210300131209         V1Cnsr = ds_ORM.wNSR;
210400131209         V1Cnor = ds_ORM.wNOR;
210500131209         V1Cnrv = ds_ORM.wNRV;
210600131211
210700131211         // -?Orari di Apertura del cliente?
210800131211         //  ?Recupero dati da estensione FNORE?
210900131211         //  ?Rcd "O " => Orari apertura?
211000131211         clear dOREorari;
211100131211         k_OREpoe = ds_ORM.wPOE;
211200131211         k_OREnsr = ds_ORM.wNSR;
211300131211         k_OREnor = ds_ORM.wNOR;
211400131211         k_OREnrv = ds_ORM.wNRV;
211500131211
211600131211         chain  %kds(keyFNORE01)  FNORE000;
211700131211
211800131211         if  %found(FNORE01L);
211900131211           dOREorari = OREdati;
212000131211         endif;
212100131211         V1oraAMda = dOREorari.§OREoramda;
212200131211         V1oraAMa  = dOREorari.§OREorama;
212300131211         V1oraPMda = dOREorari.§OREorapda;
212400131211         V1oraPMa  = dOREorari.§OREorapa;
212500131210
212600131211         // -?Ricevuta di Ritiro?
212700131210         if  dORM01.§ORsrm = 'S';
212800131210           V1Datn = 'Ricevuta di Ritiro';
212900131210         endif;
213000140121
213100140121         // -?Ora merce pronta?
213200140121         V1orr  = ORMorr;
213300131209
213400131209         // -?Colli?
213500131209         V1Cncl = ORMncl;
213600131209
213700131209         // -?Peso?
213800131209         V1Cpkg = ORMpkg;
213900131209
214000131209         // -?Bancali?
214100131209         V1Cbnc = ORMbnc;
214200131209
214300131209         // -?Volume?
214400131209         V1Cvlm = ORMvlm;
214500131209
214600131209         // -?Spunda Idraulica?
214700131209         if  ORMspi = 'S';
214800131209           V1Cspi = 'SI';
214900131209         else;
215000131209           V1Cspi = 'NO';
215100131209         endif;
215200131209
215300131209         // -?Cliente Mittente?
215400131209         V1Drsr = ORMrsr;
215500131209         V1Dinr = ORMinr;
215600131209         V1Dlor = ORMlor;
215700131209         V1Dprr = ORMprr;
215800131209         V1Dcar = ORMcar;
215900131209         V1Dnar = ORMnar;
216000131209         //V1Dlpr = %trim( ORMcar ) + ' ' + %trim( ORMlor ) + '  ' +
216100131209         //         ORMprr + ' ' + ORMnar;
216200131209
216300131209         // -?Referente?
216400131209         select;
216500131209           when  ORMrer <> *blank  and  ORMter <> *blank;
216600131209             V1Dref = %trimr( ORMrer ) + ' - ' + %triml( ORMter );
216700131209           when  ORMrer  <> *blank;
216800131209             V1Dref = ORMrer;
216900131209           when  ORMter <> *blank;
217000131209             V1Dref = ORMter;
217100131209         endsl;
217200131210
217300131210         // -?Importo Prepagato?
217400131210         If  ORMtor = 'P';
217500131210
217600131210           clear  keyFIAR404;
217700131210           k_AR44trc = 'M';
217800131210           k_AR44n14 = ds_ORM;
217900131210           chain  %kds(keyFIAR404)  FIAR4004;
218000131210
218100131210           if  %found(FIAR404L);
218200131210
218300131210             clear  keyFIAR601;
218400131210             k_AR6aas = AR4aas;
218500131210             k_AR6lnp = AR4lnp;
218600131210             k_AR6nrs = AR4nrs;
218700131210             k_AR6nsp = AR4nsp;
218800131210             chain  %kds(keyFIAR601 : 4)  FIAR6000;
218900131210
219000131210             if  %found(FIAR601L);
219100131210               w0112 = AR6ift;
219200131210               V1Datn = 'Prepagato: ' + %triml( %editc( w0112 : 'K' ) )
219300131210                                      + AR6div;
219400131210             endif;
219500131210
219600131210           endif;
219700131210
219800131210         EndIf;
219900131209
220000131209         // -?Note O.R.M.?
220100131209         V1Dnot1 = ORMno1;
220200131209         V1Dnot2 = ORMno2;
220300131209
220400131209         // -?Note AUT?
220500140121         clear  keyFNORT01;
220600140121         k_ORTpoe = ds_ORM.wPOE;
220700140121         k_ORTnsr = ds_ORM.wNSR;
220800140121         k_ORTnor = ds_ORM.wNOR;
220900140121         k_ORTnrv = ds_ORM.wNRV;
221000140121         setll  %kds( keyFNORT01 )  FNORT000;
221100140121         reade  %kds( keyFNORT01 : 4 )  FNORT000;
221200140121         DoW  Not %eof(FNORT01L);
221300140121           select;
221400140123             when  ORTgst <> 'S';
221500140121             when  V1Cnt1 = *blank;
221600140121               V1Cnt1 = ORTnob;
221700140121             when  V1Cnt2 = *blank;
221800140121               V1Cnt2 = ORTnob;
221900140121             //when  V1Cnt3 = *blank;
222000140121             //  V1Cnt3 = ORTnob;
222100140121             //when  V1Cnt4 = *blank;
222200140121             //  V1Cnt4 = ORTnob;
222300140121             other;
222400140121               leave;
222500140121           endsl;
222600140121           reade  %kds( keyFNORT01 : 4 )  FNORT000;
222700140121         EndDo;
222800131209
222900140108         // -?Note R.A.: solo input?
223000131209
223100131209         // -?Reperimento Ultimo contatto precedente?
223200140203         exsr  sr_GetUltContPrec;
223300131209
223400131209       ENDSR;
223500131209
223600131209       //--------------------------------------------------------------
223700131209       //?Impostazione attributi di visualizzazione video DO1.         ?
223800131209       //--------------------------------------------------------------
223900131209       BEGSR  sr_AttrVisDO1;
224000131209
224100131209         // ·?Ultimo Contatto?
224200131209         RevImgUCO   = (V1Duco <> *blank);
224300131209
224400131209         // ·?Comando?
224500131209         RevImgCMD   = (V1Dcmd <> *blank);
224600131209
224700131209         // ·?Variazioni?
224800131209         RevImgVAR   = (V1Dvar <> *blank);
224900131209
225000131209         // ·?O.R.M. PrePagato con importo?
225100131210         VisualORMPP = (ORMtor = 'P');
225200131209
225300131209         // ·?Ricevuta di Ritiro?
225400131210         VisualORMRR = (dORM01.§ORsrm = 'S');
225500131209
225600131209       ENDSR;
225700131209
225800131209       //--------------------------------------------------------------
225900131209       //?Abilitazione tasti funzionali video DO1.                     ?
226000131209       //--------------------------------------------------------------
226100131209       BEGSR  sr_AbilFxxDO1;
226200131209
226300131209         // -?F1 = Ultimo Contatto?
226400131209         F1Attivo = ($UltContPrec = *on);
226500131209
226600131209       ENDSR;
226700131209
226800131209       //--------------------------------------------------------------
226900131209       //?Controllo dati nella videata D01.                            ?
227000131209       //--------------------------------------------------------------
227100131209       BEGSR  sr_CtrDO1;
227200131209
227300131209         // -?Spegnimento indicatori di posizionamento cursore?
227400131209         %subst(IndDspF : 70) = *off;
227500131209
227600131209       ENDSR;
227700131209
227800131209       //--------------------------------------------------------------
227900131209       //?Richiamo *pgm x Interrogazione O.R.M. (in filiale).          ?
228000131209       //--------------------------------------------------------------
228100131209       BEGSR  sr_VisualORM;
228200131209
228300131209         clear  Parm01;
228400131209         Parm01.pPOE = ds_ORM.wPOE;
228500131209         Parm01.pNSR = ds_ORM.wNSR;
228600131209         Parm01.pNOR = ds_ORM.wNOR;
228700131209         Parm01.pNRV = ds_ORM.wNRV;
228800131209         Parm01.pSCE = '5';
228900131209         kpjbu = Parm01;
229000131209
229100131209         clear  FIDNA1ds;
229200131209         FIDNA1ds.iA1ins = 'N';
229300131209
229400131209         fior07r ( kpjba : FIDNA1ds );
229500131209
229600131209       ENDSR;
229700131128
229800131128       //--------------------------------------------------------------
229900131128       //?Gestione videata W01 (Visualizzazione Ultimo Contatto).      ?
230000131128       //--------------------------------------------------------------
230100131128       BEGSR  sr_GesW01;
230200131128
230300131128         // -?Inizializzazione videata?
230400140122         //$VideoPrec = $Video;
230500140122         //$Video  = 'W1';
230600131128         if  $InzW01 = *on;
230700131128           exsr  sr_InzW01;
230800131128           $InzW01 = *off;
230900131128         endif;
231000131128
231100131128         // -?Emissione window?
231200131128         DoU  dsp_aid = c_F12;
231300131128           exfmt  LRX2W01;
231400131128         EndDo;
231500131128
231600131128         // -?F12=Ritorno?
231700140122         //if  dsp_aid = c_F12;
231800140122         //  $Video  = $VideoPrec;
231900140122         //endif;
232000131128
232100131128       ENDSR;
232200131128
232300131128       //--------------------------------------------------------------
232400131128       //?Inizializzazione window W01.                                 ?
232500131128       //--------------------------------------------------------------
232600131128       BEGSR  sr_InzW01;
232700131128
232800131128         // -?Reperimento Ultimo contatto precedente?
232900140203         //  ?GIÀ effettuato nella subr. "sr_InzDS1/DO1"?
233000131128
233100131128
233200131128         clear  LRX2W01;
233300131128
233400131128         // -?Profilo Utente Telefonata Precedente?
233500131128         W1pruTel   = CAUpruTel;
233600131128
233700131128         // -?Data/Ora Inserimento Telefonata Precedente?
233800131128         ds_DataOra = CAUdtOins;
233900131128         wDate_EUR  = %date( ds_DataOra.wDate );
234000131128         W1dtTel    = %dec( wDate_EUR );
234100131128         W1orTel    = ds_DataOra.wTime;
234200131128
234300131128         // -?Descrizione Comando Telefonata precedente?
234400131128         clear  dCMD;
234500131128         If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
234600131128           if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
234700131128                            *omit : *omit :
234800131128                            *omit : *omit :
234900131128                            *omit : *omit : *omit : *omit :
235000131128                            *omit : *omit : *omit : *omit :
235100131128                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
235200131128                            = *zero      AND
235300131128                 ds_TNTBE.TBEatb = *blank;
235400131128             dCMD = ds_TNTBE.TBEuni;
235500131128           else;
235600131128             §CMDdes = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ?';
235700131128           endif;
235800131128         Else;
235900131128           §CMDdes = %triml( CAUdesCmd );
236000131128         EndIf;
236100131128         W1desCmd = §CMDdes;
236200131128
236300131128         // -?Descrizione Variazione Telefonata precedente?
236400131128         If  CAUdesVA = *blank;
236500131128           wTipTVA = 'B';
236600131128           exsr  sr_GetDesTCV;
236700140108           W1desVa1 = wDesTVA;
236800131128         Else;
236900131128           W1desVa1 = %subst( CAUdesVA : 1 : %size(W1desVa1) );
237000131128           W1desVa2 = %subst( CAUdesVA : %size(W1desVa1) + 1 );
237100131128         EndIf;
237200131128
237300131128       ENDSR;
237400140203
237500140203       //--------------------------------------------------------------
237600140203       //?Reperimento ed Impostazione a video delle informazioni AUT.  ?
237700140203       //--------------------------------------------------------------
237800140203       BEGSR  sr_GetAUT;
237900140203
238000140203         // -?Decodifica autotrasportatore?
238100140203         k_APDpdr = CAUpdr;
238200140203         chain  %kds( keyFIAPD01 )  FIAPD000;
238300140203         if  Not %found(FIAPD01L)  or  APDatb <> *blank;
238400140203           clear  APDrsc;
238500140203         endif;
238600140203
238700140203         V1Cpdr = CAUpdr;
238800140203         V1Dpdr = APDrsc;
238900140203
239000140203         // -?Num. Telefono Autotrasportatore?
239100140203         //k_APD4pdr = CAUpdr;
239200140203         //chain  %kds( keyFIAPD41 )  FIAPD400;
239300140203         //if  %found(FIAPD41L)  and  APD4atb = *blank;
239400140203         //  dAPD4tel = APD4uni;
239500140203         //endif;
239600140203         //
239700140203         //V1Dtel = dAPD4tel.§APD4tel;
239800140203
239900140203       ENDSR;
240000140203
240100140203       //--------------------------------------------------------------
240200140203       //?Reperimento ed Impostazione a video delle informazioni CMD.  ?
240300140203       //--------------------------------------------------------------
240400140203       BEGSR  sr_GetCMD;
240500140203
240600140203         // -?Comando da impartire al PDA o all'AUT?
240700140203         clear  dCMD;
240800140203         If  CAUdesCmd = *blank  and  CAUcmd <> *blank;
240900140203           if  getTabella ( c_Tntbe : 'CMD' : CAUcmd : *blank :
241000140203                            *omit : *omit :
241100140203                            *omit : *omit :
241200140203                            *omit : *omit : *omit : *omit :
241300140203                            *omit : *omit : *omit : *omit :
241400140203                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
241500140203                            = *zero      AND
241600140203                 ds_TNTBE.TBEatb = *blank;
241700140203             dCMD = ds_TNTBE.TBEuni;
241800140203           else;
241900140203             §CMDdes = CAUcmd + '-' + '? ? ? ? ? ? ? ? ? ?';
242000140203           endif;
242100140203         Else;
242200140203           §CMDdes = %triml( CAUdesCmd );
242300140203         EndIf;
242400140203
242500140203         if  §CMDdes <> *blank;
242600140203           xx = ( %size(V1Dcmd) - %len( %trim(§CMDdes) ) ) / 2;
242700140203           %subst( V1Dcmd : xx + 1 ) = §CMDdes;
242800140203         endif;
242900140203
243000140203       ENDSR;
243100140203
243200140203       //--------------------------------------------------------------
243300140203       //?Reperimento ed Impostazione a video delle informazioni TVA.  ?
243400140203       //--------------------------------------------------------------
243500140203       BEGSR  sr_GetTVA;
243600140203
243700140203         // -?Tipologica Causale Variazione bolla?
243800140203         //  ?(SE NON "INSERITO")?
243900140203         If  CAUcmd <> c_Cmd_Inserito;
244000140203           If  CAUdesVA = *blank;
244100140203             wTipTVA = *blank;
244200140203             exsr  sr_GetDesTCV;
244300140203             V1Dvar  = wDesTVA;
244400140203             //if  %subst( wDesTVA : %size( V1Dvar ) + 1 ) <> *blank;
244500140203             //  %subst( V1Dvar : %size( V1Dvar ) - 3 ) = ' (+)';
244600140203             //endif;
244700140203           Else;
244800140203             V1Dvar = CAUdesVa;
244900140203             //if  %subst( CAUdesVA : %size( V1Dvar ) + 1 ) <> *blank;
245000140203             //  %subst( V1Dvar : %size( V1Dvar ) - 3 ) = ' (+)';
245100140203             //endif;
245200140203           EndIf;
245300140203         EndIf;
245400140203
245500140203       ENDSR;
245600131205
245700131205       //--------------------------------------------------------------
245800131205       //?Reperimento dati della Spedizione (Consegna).                ?
245900131205       //--------------------------------------------------------------
246000131205       BEGSR  sr_GetDesTCV;
246100131205
246200131205         clear  wDesTVA;
246300131205
246400131205         For  xx = 1  To  %elem(sk_TVA);
246500131205
246600131205           clear  dTCV;
246700131205
246800131205           If  sk_TVA(xx) <> *blank;
246900131205
247000131205             yy = %lookup( sk_TVA(xx) : sk_TCV );
247100131205
247200131205             If  yy > *zero;
247300131205
247400131205               dTCV = sk_TCVd(yy);
247500131205               If  wDesTVA = *blank;
247600131205                 select;
247700131205                   when  wTipTVA = 'B';
247800131205                     wDesTVA = %triml( §TCVdesB );
247900131205                   when  wTipTVA = 'V';
248000131205                     wDesTVA = %triml( §TCVdesV );
248100131205                   other;
248200131205                     wDesTVA = %triml( §TCVdes );
248300131205                 endsl;
248400131205               Else;
248500131205                 select;
248600131205                   when  wTipTVA = 'B';
248700131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
248800131205                               %triml( §TCVdesB );
248900131205                   when  wTipTVA = 'V';
249000131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
249100131205                               %triml( §TCVdesV );
249200131205                   other;
249300131205                     wDesTVA = %trimr( wDesTVA ) + '-' +
249400131205                               %triml( §TCVdes );
249500131205                 endsl;
249600131205               EndIf;
249700131205
249800131205             Else;
249900131205
250000131205               §TCVdesB  = '???' + sk_TVA(xx) + '???';
250100131205               if  wDesTVA = *blank;
250200131205                 wDesTVA = %triml( §TCVdesB );
250300131205               else;
250400131205                 wDesTVA = %trimr( wDesTVA ) + '-' +
250500131205                           %triml( §TCVdesB );
250600131205               endif;
250700131205
250800131205             EndIf;
250900131205
251000131205           EndIf;
251100131205
251200131205         EndFor;
251300131205
251400131205       ENDSR;
251500140203
251600140203       //--------------------------------------------------------------
251700140203       //?Reperimento ed Impostazione a video dell'eventuale           ?
251800140203       //?Ultimo Contatto Precedente.                                  ?
251900140203       //--------------------------------------------------------------
252000140203       BEGSR  sr_GetUltContPrec;
252100140203
252200140203         // -?Reperimento Ultimo contatto precedente?
252300140206         //clear  keyFICAU02;      ?(già impostata nella subr. sr_InzDS1/DO1)?
252400140206         //k_CA2fgs    = CAUfgs;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252500140206         //k_CA2nfv    = CAUnfv;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252600140206         //k_CA2pdr    = CAUpdr;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252700140206         //k_CA2tor    = CAUtor;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252800140206         //k_CA2ogg    = CAUogg;   ?(già impostata nella subr. sr_InzDS1/DO1)?
252900140206         k_CA2dtOins = CAUdtOins;
253000140206
253100140206         chain  %kds( keyFICAU02 )  FICAU002;
253200140206         reade  %kds( keyFICAU02 : 5)  FICAU002;
253300140206
253400140203         // -?NON?trovato alcun contatto precedente?
253500140206         if  %eof(FICAU02L);
253600140203           leavesr;
253700140203         endif;
253800140206
253900140203         // -?Trovato l'ultimo contatto precedente?
254000140203         $UltContPrec = *on;
254100140206         ds_DataOra = CA2dtOins;
254200140203         V1Duco     = 'Ultimo Contatto alle ' +
254300140203                      %editw( ds_DataOra.wTime4 : '  :  ' );
254400140203         // *...+....1....+....2....+.
254500140203         // Ultimo Contatto alle 12:34
254600140203
254700140203       ENDSR;
254800140203
254900140203       //--------------------------------------------------------------
255000140203       //?Reperimento (nuovo) ultimo record per l'oggetto in esame.    ?
255100140203       //--------------------------------------------------------------
255200140203       BEGSR  sr_Verifica_Ogg;
255300140203
255400140203         select;
255500140203
255600140203           when  S1Htor = c_Consegna;
255700140203             $RecUpdated = ( V1Cpdr  <> SaveDS1.V1Cpdr   or
255800140203                             V1Dpdr  <> SaveDS1.V1Dpdr   or
255900140203                             V1Duco  <> SaveDS1.V1Duco   or
256000140203                             V1Dcmd  <> SaveDS1.V1Dcmd   or
256100140203                             V1Dvar  <> SaveDS1.V1Dvar   or
256200140203                             V1Clnp  <> SaveDS1.V1Clnp   or
256300140203                             V1Cnrs  <> SaveDS1.V1Cnrs   or
256400140203                             V1Cnsp  <> SaveDS1.V1Cnsp   or
256500140203                             V1Dtsp  <> SaveDS1.V1Dtsp   or
256600140203                             V1Drsd  <> SaveDS1.V1Drsd   or
256700140203                             V1Dind  <> SaveDS1.V1Dind   or
256800140203                             V1Dlpd  <> SaveDS1.V1Dlpd   or
256900140203                             V1Cncl  <> SaveDS1.V1Cncl   or
257000140203                             V1Cpkb  <> SaveDS1.V1Cpkb   or
257100140203                             V1Dref  <> SaveDS1.V1Dref   or
257200140203                           //V1Igga  <> SaveDS1.V1Igga   or
257300140203                             V1Cgga  <> SaveDS1.V1Cgga   or
257400140203                           //V1Igma  <> SaveDS1.V1Igma   or
257500140203                             V1Cgma  <> SaveDS1.V1Cgma   or
257600140203                           //V1Igva  <> SaveDS1.V1Igva   or
257700140203                             V1Cgva  <> SaveDS1.V1Cgva   or
257800140203                             V1Ccas  <> SaveDS1.V1Ccas   or
257900140203                             V1Cvca  <> SaveDS1.V1Cvca   or
258000140203                             V1Cift  <> SaveDS1.V1Cift   or
258100140203                             V1Cdiv  <> SaveDS1.V1Cdiv   or
258200140203                             V1Dnot8 <> SaveDS1.V1Dnot8  or
258300140203                             V1Dnot9 <> SaveDS1.V1Dnot9  or
258400140203                             V1Cnt1  <> SaveDS1.V1Cnt1   or
258500140203                             V1Cnt2  <> SaveDS1.V1Cnt2   or
258600140203                             V1Cnt3  <> SaveDS1.V1Cnt3   or
258700140203                             V1Cnt4  <> SaveDS1.V1Cnt4   or
258800140203                             V1Nra1  <> SaveDS1.V1Nra1   or
258900140203                             V1Nra2  <> SaveDS1.V1Nra2 );
259000140203
259100140203           when  S1Htor = c_Ritiro;
259200140203             $RecUpdated = ( V1Cpdr    <> SaveDO1.V1Cpdr     or
259300140203                             V1Dpdr    <> SaveDO1.V1Dpdr     or
259400140203                             V1Duco    <> SaveDO1.V1Duco     or
259500140203                             V1Dcmd    <> SaveDO1.V1Dcmd     or
259600140203                             V1Dvar    <> SaveDO1.V1Dvar     or
259700140203                             V1Cpoe    <> SaveDO1.V1Cpoe     or
259800140203                             V1Cnsr    <> SaveDO1.V1Cnsr     or
259900140203                             V1Cnor    <> SaveDO1.V1Cnor     or
260000140203                             V1Cnrv    <> SaveDO1.V1Cnrv     or
260100140203                             V1oraAMda <> SaveDO1.V1oraAMda  or
260200140203                             V1oraAMa  <> SaveDO1.V1oraAMa   or
260300140203                           //V1oraSep  <> SaveDO1.V1oraSep   or
260400140203                             V1oraPMda <> SaveDO1.V1oraPMda  or
260500140203                             V1oraPMa  <> SaveDO1.V1oraPMa   or
260600140203                             V1datN    <> SaveDO1.V1datN     or
260700140203                             V1Drsr    <> SaveDO1.V1Drsr     or
260800140203                             V1Dinr    <> SaveDO1.V1Dinr     or
260900140203                             V1Dlor    <> SaveDO1.V1Dlor     or
261000140203                             V1Dprr    <> SaveDO1.V1Dprr     or
261100140203                             V1Dcar    <> SaveDO1.V1Dcar     or
261200140203                             V1Dnar    <> SaveDO1.V1Dnar     or
261300140203                             V1orr     <> SaveDO1.V1orr      or
261400140203                             V1Dref    <> SaveDO1.V1Dref     or
261500140203                             V1Cncl    <> SaveDO1.V1Cncl     or
261600140203                             V1Cpkg    <> SaveDO1.V1Cpkg     or
261700140203                             V1Cbnc    <> SaveDO1.V1Cbnc     or
261800140203                             V1Cvlm    <> SaveDO1.V1Cvlm     or
261900140203                             V1Cspi    <> SaveDO1.V1Cspi     or
262000140203                             V1Dnot1   <> SaveDO1.V1Dnot1    or
262100140203                             V1Dnot2   <> SaveDO1.V1Dnot2    or
262200140203                             V1Cnt1    <> SaveDO1.V1Cnt1     or
262300140203                             V1Cnt2    <> SaveDO1.V1Cnt2     or
262400140203                             V1nra1    <> SaveDO1.V1nra1     or
262500140203                             V1nra2    <> SaveDO1.V1nra2 );
262600140203
262700140203         endsl;
262800140203
262900140203       ENDSR;
263000140203
263100140203       //--------------------------------------------------------------
263200140203       //?Memorizzazione dati da confrontare per rilevare variazioni.  ?
263300140203       //--------------------------------------------------------------
263400140203       BEGSR  sr_Memo_Dati;
263500140203
263600140203         Select;
263700140203
263800140203           When  S1Htor = c_Consegna;
263900140203             SaveDS1.V1Cpdr  = V1Cpdr;
264000140203             SaveDS1.V1Dpdr  = V1Dpdr;
264100140203             SaveDS1.V1Duco  = V1Duco;
264200140203             SaveDS1.V1Dcmd  = V1Dcmd;
264300140203             SaveDS1.V1Dvar  = V1Dvar;
264400140203             SaveDS1.V1Clnp  = V1Clnp;
264500140203             SaveDS1.V1Cnrs  = V1Cnrs;
264600140203             SaveDS1.V1Cnsp  = V1Cnsp;
264700140203             SaveDS1.V1Dtsp  = V1Dtsp;
264800140203             SaveDS1.V1Drsd  = V1Drsd;
264900140203             SaveDS1.V1Dind  = V1Dind;
265000140203             SaveDS1.V1Dlpd  = V1Dlpd;
265100140203             SaveDS1.V1Cncl  = V1Cncl;
265200140203             SaveDS1.V1Cpkb  = V1Cpkb;
265300140203             SaveDS1.V1Dref  = V1Dref;
265400140203           //SaveDS1.V1Igga  = V1Igga;
265500140203             SaveDS1.V1Cgga  = V1Cgga;
265600140203           //SaveDS1.V1Igma  = V1Igma;
265700140203             SaveDS1.V1Cgma  = V1Cgma;
265800140203           //SaveDS1.V1Igva  = V1Igva;
265900140203             SaveDS1.V1Cgva  = V1Cgva;
266000140203             SaveDS1.V1Ccas  = V1Ccas;
266100140203             SaveDS1.V1Cvca  = V1Cvca;
266200140203             SaveDS1.V1Cift  = V1Cift;
266300140203             SaveDS1.V1Cdiv  = V1Cdiv;
266400140203             SaveDS1.V1Dnot8 = V1Dnot8;
266500140203             SaveDS1.V1Dnot9 = V1Dnot9;
266600140203             SaveDS1.V1Cnt1  = V1Cnt1;
266700140203             SaveDS1.V1Cnt2  = V1Cnt2;
266800140203             SaveDS1.V1Cnt3  = V1Cnt3;
266900140203             SaveDS1.V1Cnt4  = V1Cnt4;
267000140203             SaveDS1.V1Nra1  = V1Nra1;
267100140203             SaveDS1.V1Nra2  = V1Nra2;
267200140203
267300140203           When  S1Htor = c_Ritiro;
267400140203             SaveDO1.V1Cpdr    = V1Cpdr;
267500140203             SaveDO1.V1Dpdr    = V1Dpdr;
267600140203             SaveDO1.V1Duco    = V1Duco;
267700140203             SaveDO1.V1Dcmd    = V1Dcmd;
267800140203             SaveDO1.V1Dvar    = V1Dvar;
267900140203             SaveDO1.V1Cpoe    = V1Cpoe;
268000140203             SaveDO1.V1Cnsr    = V1Cnsr;
268100140203             SaveDO1.V1Cnor    = V1Cnor;
268200140203             SaveDO1.V1Cnrv    = V1Cnrv;
268300140203             SaveDO1.V1oraAMda = V1oraAMda;
268400140203             SaveDO1.V1oraAMa  = V1oraAMa;
268500140203           //SaveDO1.V1oraSep  = V1oraSep;
268600140203             SaveDO1.V1oraPMda = V1oraPMda;
268700140203             SaveDO1.V1oraPMa  = V1oraPMa;
268800140203             SaveDO1.V1datN    = V1datN;
268900140203             SaveDO1.V1Drsr    = V1Drsr;
269000140203             SaveDO1.V1Dinr    = V1Dinr;
269100140203             SaveDO1.V1Dlor    = V1Dlor;
269200140203             SaveDO1.V1Dprr    = V1Dprr;
269300140203             SaveDO1.V1Dcar    = V1Dcar;
269400140203             SaveDO1.V1Dnar    = V1Dnar;
269500140203             SaveDO1.V1orr     = V1orr;
269600140203             SaveDO1.V1Dref    = V1Dref;
269700140203             SaveDO1.V1Cncl    = V1Cncl;
269800140203             SaveDO1.V1Cpkg    = V1Cpkg;
269900140203             SaveDO1.V1Cbnc    = V1Cbnc;
270000140203             SaveDO1.V1Cvlm    = V1Cvlm;
270100140203             SaveDO1.V1Cspi    = V1Cspi;
270200140203             SaveDO1.V1Dnot1   = V1Dnot1;
270300140203             SaveDO1.V1Dnot2   = V1Dnot2;
270400140203             SaveDO1.V1Cnt1    = V1Cnt1;
270500140203             SaveDO1.V1Cnt2    = V1Cnt2;
270600140203             SaveDO1.V1nra1    = V1nra1;
270700140203             SaveDO1.V1nra2    = V1nra2;
270800140203
270900140203         EndSl;
271000140203
271100140203       ENDSR;
271200131122
271300131122       //--------------------------------------------------------------
271400140203       //?Gestione tasto funzionale F6=Accetta premuto da DS1 o DO1.   ?
271500131122       //--------------------------------------------------------------
271600140121       BEGSR  sr_F06D01;
271700131205
271800131205         // -?Riaggancio del record del Contatto ad Autotrasportatore?
271900131205         //  ?da aggiornare (allocandolo)?
272000131205         clear  keyFICAU01;
272100140206         //k_CAUfgs    = S1Hfgs;
272200140206         //k_CAUpdr    = C1Cpdr;
272300140206         k_CAUfgs    = FNLRX2ds.iX2fgs;
272400140206         k_CAUpdr    = FNLRX2ds.iX2aut;
272500131205         k_CAUdtOins = S1HdtOins;
272600131205         k_CAUtor    = S1Htor;
272700131205         k_CAUogg    = S1Hogg;
272800131205         chain  %kds( keyFICAU01 )  FICAU000;
272900140207
273000140207         // -?Rec. NON trovato: si torna alla ri-emissione della?
273100140207         //  ?videata (come NON fosse stato premuto F6)?
273200140207         If  Not %found(FICAU01L)  or  CAUatb <> *blank;
273300140207           ErrGenerico = *on;
273400140207           leavesr;
273500140207         EndIf;
273600131122
273700131205         // -?Richiamo del *pgm FIDNA3R = Scrittura R.A. cieca?
273800131205         //  ?(causale " 40") - vedi file FITGD00F+FITGN00F?
273900131205         exsr  sr_CallFIDNA3;
274000131205
274100131205         // -?Aggiornamento Telefonata?
274200131205         exsr  sr_UpdFICAU;
274300140121
274400140121         // -?Aggiornamento flag "ORM scaricato a PDA"?
274500140128         if  CAUtor = c_Ritiro;
274600140128            exsr  sr_UpdFNORG;
274700140128         endif;
274800131205
274900131205         // -?Impostazione parametro di ritorno?
275000131205         FNLRX2ds.oX2upd = *on;
275100131205
275200131205         // -?Ritorno al subfile?
275300131205         $Video = 'S1';
275400131205         $InzS01 = *on;
275500131122
275600131122       ENDSR;
275700140203
275800140203       //--------------------------------------------------------------
275900140203       //?Reperimento (nuovo) ultimo record per l'oggetto in esame.    ?
276000140203       //--------------------------------------------------------------
276100140203       BEGSR  sr_Ricarica_Ogg;
276200140203
276300140203         // -?Ricerca record relativo all'oggetto in esame con Data/Ora?
276400140203         //  ?inserimento aggiornata?
276500140203         $RecUpdated = *off;
276600140206         //clear  keyFICAU02;      ?(già impostata nella subr. sr_InzDS1/DO1)?
276700140206         //k_CA2fgs    = CAUfgs;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276800140206         //k_CA2nfv    = CAUnfv;   ?(già impostata nella subr. sr_InzDS1/DO1)?
276900140206         //k_CA2pdr    = CAUpdr;   ?(già impostata nella subr. sr_InzDS1/DO1)?
277000140206         //k_CA2tor    = CAUtor;   ?(già impostata nella subr. sr_InzDS1/DO1)?
277100140206         //k_CA2ogg    = CAUogg;   ?(già impostata nella subr. sr_InzDS1/DO1)?
277200140207         ////k_CA2dtOins = *hival; ?(già impostata nella subr. sr_InzDS1/DO1)?
277300140206
277400140206         chain  %kds( keyFICAU02 : 5 )  FICAU002;
277500140203
277600140206         $RecUpdated = ( %found( FICAU02L ) );
277700140203
277800140203         // -?Trovato: si aggiornano i dati a video?
277900140203         //  ?(di cui verrà effettuata la riemissione)?
278000140203         If  $RecUpdated;
278100140206           k_CAUdtOins = CA2dtOins;
278200140206           S1HdtOins   = CA2dtOins;
278300140206           S01hmi   = %int( %subst( CA2dtOins : 9 : 4 ) );
278400140203           $InzS01  = *on;
278500140203           $InzD01  = *on;
278600140203         EndIf;
278700140203
278800140203       ENDSR;
278900131205
279000131205       //--------------------------------------------------------------
279100131205       //?Scrittura Richiesta Assistenza cieca (causale " 40")         ?
279200131205       //--------------------------------------------------------------
279300131205       BEGSR  sr_CallFIDNA3;
279400131205
279500131205         clear  FIDNA3ds;
279600131205         iA3mad = c_IA3mad;
279700131209         select;
279800131209           when  CAUtor = c_Consegna;
279900131209             iA3tor = c_Spedizione;
280000131209           when  CAUtor = c_Ritiro;
280100131209             iA3tor = c_ORM;
280200131209         endsl;
280300131205         iA3ogg = CAUogg;
280400140108         iA3no1 = V1nra1 + V1nra2;
280500131205         iA3no2 = 'AUT: '  + %editc( CAUpdr : 'X' ) +
280600131205                 ' DIST: ' + %editc( CAUnfv : 'X' );
280700131205         if  §CMDdes <> *blank;
280800131205           iA3no2 = %trimr(iA3no2) + ' ' + %trim(§CMDdes);
280900131205         endif;
281000131205         select;
281100131205           when  CAUdesVA <> *blank;
281200131205             //iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(CAUdesVA);
281300131205             if  §CMDdes = *blank;
281400131205               iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(CAUdesVA);
281500131205             else;
281600131205               %subst( iA3no2 : 46 ) = ' VARIAZ: ' + %trim(CAUdesVA);
281700131205             endif;
281800131205           when  wDesTVA  <> *blank;
281900131205             if  §CMDdes = *blank;
282000131205               iA3no2 = %trimr(iA3no2) + ' VARIAZ: ' + %trim(wDesTVA);
282100131205             else;
282200131205               %subst( iA3no2 : 46 ) = ' VARIAZ: ' + %trim(wDesTVA);
282300131205             endif;
282400131205         endsl;
282500131205         //iA3ara = *blank;
282600131205
282700140108         // -?Richiesta Accettata?
282800140121         //%subst( iA3no2 : %size(iA3no2) - 10 ) = ' (ESEGUITA)';
282900131205
283000131205         FIDNA3R ( kpjba : FIDNA3ds );
283100131205
283200131205       ENDSR;
283300131205
283400131205       //--------------------------------------------------------------
283500131205       //?Aggiornamento telefonata (file FICAU00F).                    ?
283600131205       //--------------------------------------------------------------
283700131205       BEGSR  sr_UpdFICAU;
283800131205
283900131205         // -?Reperimento orario corrente?
284000131205         wTimeStamp = %timestamp();
284100131205
284200131205         // -?Impostazione dei campi relativi all'"ESITO"?
284300131205         CAUflt    = 'T';
284400131205         CAUdtoTel = %subst( %char( %dec( wTimeStamp ) ) : 1 : 14 );
284500140110         CAUesito  = c_Eseguita;
284600131205         CAUpruTel = SDSjobUser;
284700131205         CAUterm   = SDSjobName;
284800131205         CAUesec   = 'T';
284900131205
285000131205
285100131205         // -?Aggiornamento rec. FICAU00F?
285200140203         //_______________
285300140203         update  FICAU000;
285400140203         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
285500131205
285600131205       ENDSR;
285700140121
285800140121       //--------------------------------------------------------------
285900140121       //?Aggiornamento O.R.M. (file FNORG00F).                        ?
286000140121       //--------------------------------------------------------------
286100140121       BEGSR  sr_UpdFNORG;
286200140121
286300140121         // -?Aggancia il record dell'Estensione Testata?
286400140121         clear  keyFNORG01;
286500140121         k_ORGpoe = ds_ORM.wPOE;
286600140121         k_ORGnsr = ds_ORM.wNSR;
286700140121         k_ORGnor = ds_ORM.wNOR;
286800140121         k_ORGnrv = ds_ORM.wNRV;
286900140121         chain  %kds( keyFNORG01 )  FNORG000;
287000140121
287100140121         // -?IMPOSSIBILE?che NON venga reperito il rec. da aggiornare?
287200140121         if  Not %found(FNORG01L);
287300140121           //clear  FNORG000;
287400140121           //CAUpoe = k_ORGpoe;
287500140121           //CAUnsr = k_ORGnsr;
287600140121           //CAUnor = k_ORGnor;
287700140121           //CAUnrv = k_ORGnrv;
287800140121           //CAUpor = .....;
287900140121           //--- oppure: ----------
288000140121           //*inH2 = *on;
288100140121           //$Fine = *on;
288200140121           //ErrGenerico = *on;
288300140121           //leavesr;
288400140121         endif;
288500140121
288600140121         // -?Aggiornamento flag "ORM scaricato a PDA"?
288700140123         dORG01 = ORGflo;
288800140121         select;
288900140121           // -?Inserito => Sì?
289000140121           when  CAUcmd = 'I';
289100140121             dORG01.§ORGpda = 'S';
289200140121           // -?Annullato => No?
289300140121           when  CAUcmd = 'A';
289400140121             clear  dORG01.§ORGpda;
289500140121         endsl;
289600140123         ORGflo = dORG01;
289700140121
289800140121         // -?Aggiornamento rec. FNORG00F?
289900140121         //if  %found(FNORG01L);
290000140121           //_______________
290100140121           update  FNORG000;
290200140121           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
290300140121         //endif;
290400140121
290500140121       ENDSR;
290600131030
290700131030       //--------------------------------------------------------------
290800131030       //?Operazioni finali.                                           ?
290900131030       //--------------------------------------------------------------
291000131030       BEGSR  sr_RoutEnd;
291100131120
291200131120         // -?Chiusura funzioni precedentemente aperte?
291300131120         cloTabella ( c_Tntbe );
291400131030
291500131120         // -?Restituzione eventuali parametri?
291600131121         kpjbu = FNLRX2ds;
291700131104
291800131120         // -?Chiusura *pgm?
291900131030         return;
292000131030
292100131030       ENDSR;
292200131030
292300131030      /end-free
292400131030
292500131120** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
292600131120Superato il numero massimo di telefonate visualizzabili                         1
292700131120Opzione errata                                                                  2
292800131205ERRORE: NON risulta più reperibile questa telefonata                            3
292900140206Attenzione: sono stati variati dei dati. VERIFICARE                             4
