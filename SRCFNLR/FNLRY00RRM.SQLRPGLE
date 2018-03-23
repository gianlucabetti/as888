000100131007       //==============================================================
000200131007       //
000300140519       //?FNLRY00R - Disposizioni di Consegna da Internet
000400140519       //
000500131007       //
000600131007       //==============================================================
000700131009
000800131009       //--------------------------------------------------------------
000900131009       //?Specifiche di controllo.                                     ?
001000131009       //--------------------------------------------------------------
001100140528     H dftactgrp(*no) actgrp(*caller) BNDDIR('TIS':'UBBNDDIR')
001200131009
001300131009       //--------------------------------------------------------------
001400131009       //?Dichiarazione file.                                          ?
001500131009       //--------------------------------------------------------------
001600131010
001700131010       // -?File organigramma
001800131010     fAZORG01L  if   e           k disk
001900131009
002000131009       // -?File Tabelle
002100131009     fTABEL00F  if   e           k disk
002200131010     fTNTBE01L  if   e           k disk
002300140603       // -?File Disposizioni di consegna da Web
002400140610     ftiidc01l  if a e           k disk
002500141013     ftiidc02l  if   e           k disk
002600140604     f                                     rename(tiidc000:tiidc2)
002700140807     ftivpi00f  o  a e             disk    usropn
002800140521       // -?
002900140521     FFnarb01l  IF   E           K DISK    extfile(wlibfil) usropn
002902161128     FFnblp01l  IF   E           K DISK    extfile(wlibfil) usropn
002903161128     f                                     prefix(ARB:3)
003000140522     FFiar501l  IF   E           K DISK    extfile(wlibfil) usropn
003100140528     FFiar401l  IF   E           K DISK    extfile(wlibfil) usropn
003200140521     FFipct02l  IF   E           K DISK    extfile(wlibfil) usropn
003300140521     FTIgcp21l  IF   E           K DISK
003400140605     Fazcln01l  IF   E           K DISK
003500140620     Ffnlbl02l  IF   E           K DISK
003600131008
003700131007       //--------------------------------------------------------------
003800131007       //?Definizione costanti.                                        ?
003900131007       //--------------------------------------------------------------
004000140520      /copy gaitrasrc/srcconst,FNLRY00R
004100140520
004200140520     D digits          c                   '0123456789'
004300140617
004400140617     D MSGID_LUNEDI...
004500140617     D                 C                   'TIS0351'
004600140617     D MSGID_MARTEDI...
004700140617     D                 C                   'TIS0354'
004800140617     D MSGID_MERCOLEDI...
004900140617     D                 C                   'TIS0356'
005000140617     D MSGID_GIOVEDI...
005100140617     D                 C                   'TIS0203'
005200140617     D MSGID_VENERDI...
005300140617     D                 C                   'TIS0607'
005400140617     D MSGID_SABATO...
005500140617     D                 C                   'TIS0533'
005600140617     D MSGID_DOMENICA...
005700140617     D                 C                   'TIS0159'
005800140114
005900131007       //--------------------------------------------------------------
006000131007       //?Definizione strutture dati.                                  ?
006100131007       //--------------------------------------------------------------
006200131007       // -?Dati INPUT
006300140519     d Fnlry00i0     e ds                  QUALIFIED INZ(*EXTDFT)
006400140528     d Fnlry00i1     e ds                  QUALIFIED INZ(*EXTDFT)
006500141006     d TIIDCds       e ds                  extname(tiidc00f)
006600131007
006700131007       // -?Dati OUTPUT
006800140519     d Fnlry00o0     e ds                  QUALIFIED INZ(*EXTDFT)
006900140605     d  schdcr                       10a   dim(9)  overlay(dateconst)
007000140923     d  schdcri                      10a   dim(9)  overlay(dateconti)
007100140521     d Fnlry00o1     e ds                  QUALIFIED INZ(*EXTDFT)
007200131007
007300131007       // -?Messaggi
007400140522     d Fnlry00m0     e ds                  QUALIFIED INZ(*EXTDFT)
007500140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
007600140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
007700140522     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
007800140522     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
007900131007
008000131007       // -?Forzature
008100140522     d Fnlry00f0     e ds                  QUALIFIED INZ(*EXTDFT)
008200140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
008300140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
008400140522     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
008500140520       // -?Legami clienti
008600140520     d TIBS10DS      e ds                  INZ(*EXTDFT)
008700140520     d  skc                          11    dim(500) overlay(d10skc)
008800140521       // -?Controllo BRTCODE
008900140521     d TRUL28DS0     e ds                  INZ(*EXTDFT)
009000140528       // -?Controllo indirizzo mail
009100140528     d dsemail       e ds                  INZ
009200140521       // -?Calcola orari di servizio
009300140521     d TRULORSDS     e ds                  INZ
009400140521     d TRULOR2ds     e ds                  INZ
009500150729     d diore01       e ds                  INZ
009600140604
009602161129       // -?File bolle arrivi
009603161129     d FNARBds       e ds                  extname(FNARB00F)
009700140522     D tnsd99ds      e ds
009800140604     D TRUL33DS      E DS
009900140923     D Tisio9ds      E DS
010000140521
010100140521     d fidn12ds      e ds
010200140603     d tibs02ds      e ds
010300140603     d fnlv55ds      e ds
010400140604     d KPJBA         e ds
010401161128
010402161128     d TRULVPODS     e ds
010403161128     D  skFilOk               16    765    dim(250)
010500140521
010501161202     d didcal        e ds
010600140521     d fipctcetds    e ds
010700140521     d og143         e ds
010800140522     d dar5emd       e ds
010900140523     d dar5gen       e ds
011000140522     d ds7r          e ds
011100140522     d ds3a          e ds
011200140613     d ds2a          e ds
011300140528     d ds2g          e ds
011400140528     d ds7u          e ds
011500140619     d ds7w          e ds
011600140603     d divp          e ds
011700140924     d dvpooraris    e ds
011800140528
011900140528     D WDAT8           DS
012000140528     D  DATADA                 1      8  0
012100140528     D  DATAA                  9     16  0
012200140528     D  GGL                   17     21  0
012300140610     D tfptfa          DS
012400140610     D  p_tfp                  1      3  0
012500140610     D  p_tfa                  4      6  0
012600140606
012700140606     D clnmat          DS
012800140606     D  mat                    1     31    dim(31)
012900140606     D clnpom          DS
013000140606     D  pom                    1     31    dim(31)
013100140617     D dayOfWeek       DS                  QUALIFIED
013200140617     D                                7A   INZ(MSGID_LUNEDI)
013300140617     D                                7A   INZ(MSGID_MARTEDI)
013400140617     D                                7A   INZ(MSGID_MERCOLEDI)
013500140617     D                                7A   INZ(MSGID_GIOVEDI)
013600140617     D                                7A   INZ(MSGID_VENERDI)
013700140617     D                                7A   INZ(MSGID_SABATO)
013800140617     D                                7A   INZ(MSGID_DOMENICA)
013900140617     D  msgId                         7A   DIM(7) OVERLAY(dayOfWeek)
014000140814
014100140814     D chklib          S             35    DIM(1) CTDATA PERRCD(1)
014200140312
014300131009       //--------------------------------------------------------------
014400131009       //?Definizione schiere.
014500131009       //--------------------------------------------------------------
014600140210
014700140210     d skFIdMsg        s              7a   dim(99)
014800140210     d skFIdCampo      s             10a   dim(99)
014900140210     d skFGiaForza     s              1a   dim(99)
015000131007
015100131007       //--------------------------------------------------------------
015200131007       //?Definizione campi.
015300131007       //--------------------------------------------------------------
015400140527     d wIdMsg          s              7a
015500140527     d wIdCampo        s             10a
015600140527     d wcampo          s             10a
015700140519     d wksu            s                   like(fnlry00i0.ksu)
015800140520     d widbolla        s                   like(fnlry00i0.idbolla)
015900140521     d wbolla          s                   like(widbolla)
016000140520     d wbrtcode        s                   like(fnlry00i0.brtcode)
016100140527     d wIdlingua       s                   like(fnlry00i0.liniso2)
016200140527     d wdcreur         s                   like(fnlry00i1.olddtcrich)
016300140529     d wrbhcr          s                   like(fnlry00i1.OLDORCRICH)
016400140528     d wtprich         s              1
016500140530     d wtentacons      s              1
016600140528     d dataoggi        s              8s 0
016601161129     d dataPrvC        s              8s 0
016700141218     d wdataconfr      s              8s 0
016800140604     d annocur         s              4s 0
016900140528     d wdcr            s              8s 0
017000140605     d wdata           s              8s 0
017100140528     d wora            s              4s 0
017200140527     d wrsd2           s             35
017300140522     d wchi            s              1
017400140521     d kaas            s                   like(arbaas)
017500140521     d klnp            s                   like(arblnp)
017600140521     d knrs            s                   like(arbnrs)
017700140521     d knsp            s                   like(arbnsp)
017800140522     d kkey            s                   like(tblkey)
017900140523     d wrefbolla       s                   like(§ar5ref)
018000140523     d wtelbolla       s                   like(§ar5teld)
018100140528     d wlimitdcr       s                   like(§2gldr)
018200140613     d wlimitFD        s                   like(§2agla)
018300140521     d wcliente        s             11
018400140521     D Wlibfil         S             21
018500140521     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
018600140521     D Wlibfil201      S             21    INZ('FILTRA201/        ')
018700140521     d dataeur         s               d   datfmt(*eur)
018800140528     d dataiso         s               d   datfmt(*iso)
018900140528     d wParm           s            512a   inz
019000140528     d w008a           s              8a   inz
019100140908     d w0140           s             14  0 inz
019200140603     d wfgslna         s                   like(d55tfa)
019300140616     d wfgslnad        s             20a
019400140604     D Esito           s              1a
019500141031     d w0090           s              9  0
019600140604     d kprg            s                   like(idcprg)
019700140604     d WDCINSDATA      s             20  0
019800140605     d I               s              2  0
019900140606     d Ix              s              2  0
020000140923     d Iy              s              2  0
020100140606     d wfes            s              1
020200140606     d matpom          s             10
020300140611     d wTextMsg        s            255
020400140604     d $Finenum        s               n   inz(*off)
020500140617     d dw              s              1s 0
020600140617     d wdesgio         s             20a
020700140718     d totale          s              2  0
020800140801     d wkeyspa         s             16
020900140924     D wtempoh         S              2  0 inz
021000140924     D wtempom         S              2  0 inz
021100140925     D waltroind       S              1a   inz
021200140930     D wconsric        S                   like(fnlry00i1.prconsric)
021300140930     D w_dalle         S               t
021400140930     D w_alle          S               t
021500140930     D w_dalleoser     S               t
021600140930     D w_alleoser      S               t
021700140930     D w_dallemsg      S              5a
021800140930     D w_allemsg       S              5a
021900140930     D w_orcric        S                   like(fnlry00i1.neworcrich)
022000141006     D wolna           S                   like(olna)
022100141007     d wtxt            S           2048
022200141007     d TxtInOut        S           2048
022300141007     d ElencoChar      S            256
022400141007     d TipoElenco      S              1
022500141007     d CharSost        S              1
022600141007     d UpperCase       S              1
022700141007     d ChkNull         S              1    Inz('1')
022800141007     d CharNull        S              1
022900141007     d posl            s              2  0
023000141007     d posr            s              2  0
023100141007     d lung            s              2  0
023200141007     d* Lunghezza di CHARNOOK deve essere:  lunghezza di tbeuni + 1
023300141007     d* Il +1 è necessario per assicurarmi che nella stringa sia sempre
023400141007     d* presente un blank
023500141007     d charnook        s            257
023600141007     d charnookf       s                   like(charnook)
023601161130     d w_tbo           s              1    inz
023602161129     d wDecofiAll      s               n   inz
023700140611       //?- Parametri per FISPE02R: EasySpedWeb: S.P.  TISIT5R+TISI95R?
023800140611     d iTYPE           s              1    inz
023900140611     d iLANG           s              3    inz
024000140611     d iDATA           s              8s 0 inz
024100140611     d iNTW            s              1    inz
024200140611     d iLNP            s              3s 0 inz
024300140611     d iNZD            s              3    inz
024400140611     d iPRD            s              2    inz
024500140611     d iCAD            s              9    inz
024600140611     d iLOD            s             35    inz
024700140611     d iIND            s             35    inz
024800140611     d iRSD            s             35    inz
024900140611     d iNCL            s              5s 0 inz
025000140611     d iPKB            s              7s 1 inz
025100140611     d iVLB            s              5s 3 inz
025200140611     d iFFD            s              1    inz
025300140611     d iTSP            s              1    inz
025400140611     d iTC1            s              1    inz
025500140611     d iTC2            s              1    inz
025600140611     d iCBO            s              2    inz
025700140611      *
025800140611     d oTFA            s              3s 0 inz
025900140611     d oLNA            s              3s 0 inz
026000140611     d oZNC            s              2s 0 inz
026100140611     d oLNPD           s             20    inz
026200140611     d oLNAD           s             20    inz
026300140611     d oERR            s              1    inz
026400140611     d oMSG            s            256    inz
026500140611     d oTAG            s             30    inz
026600140611      *
026700140611     d RTNesito        s             10I 0 inz
026800140611     d RTNopcode       s             10    inz
026900140611     d RTNstatus       s             10I 0 inz
027000131008
027100131008       //--------------------------------------------------------------
027200131008       //?Definizione procedure.
027300131008       //--------------------------------------------------------------
027400140603     d tibs02r         pr                  extpgm('TIBS02R')
027500140604     d  kpjba                              likeds(kpjba)
027600140603     d  tibs02ds                           likeds(tibs02ds)
027700140604     D trul33R         pr                  extpgm('TRUL33R')
027800140604     D  kpjba                              likeds(kpjba)
027900140923     D tisio9r         pr                  extpgm('TISIO9R')
028000140923     D  kpjba                              likeds(kpjba)
028100140923     D  tisio9ds                           likeds(tisio9ds)
028200140611       //?- Stored Procedure per EasySpedWeb?
028300140611     d fispE02r        pr                  extpgm('FISPE02R')
028400140611     d   i_TYPE                            like(iTYPE)
028500140611     d   i_LANG                            like(iLANG)
028600140611     d   i_DATA                            like(iDATA)
028700140611     d   i_NTW                             like(iNTW)
028800140611     d   i_LNP                             like(iLNP)
028900140611     d   i_NZD                             like(iNZD)
029000140611     d   i_PRD                             like(iPRD)
029100140611     d   i_CAD                             like(iCAD)
029200140611     d   i_LOD                             like(iLOD)
029300140611     d   i_IND                             like(iIND)
029400140611     d   i_RSD                             like(iRSD)
029500140611     d   i_NCL                             like(iNCL)
029600140611     d   i_PKB                             like(iPKB)
029700140611     d   i_VLB                             like(iVLB)
029800140611     d   i_FFD                             like(iFFD)
029900140611     d   i_TSP                             like(iTSP)
030000140611     d   i_TC1                             like(iTC1)
030100140611     d   i_TC2                             like(iTC2)
030200140611     d   i_CBO                             like(iCBO)
030300140611     d   o_TFA                             like(oTFA)
030400140611     d   o_LNA                             like(oLNA)
030500140611     d   o_ZNC                             like(oZNC)
030600140611     d   o_LNPD                            like(oLNPD)
030700140611     d   o_LNAD                            like(oLNAD)
030800140611     d   o_ERR                             like(oERR)
030900140611     d   o_MSG                             like(oMSG)
031000140611     d   o_TAG                             like(oTAG)
031100140611     d   RTN_esito                         like(RTNesito)
031200140611     d   RTN_opcode                        like(RTNopcode)
031300140611     d   RTN_status                        like(RTNstatus)
031400131008
031500131008       //--------------------------------------------------------------
031600131008       //?Definizione prototipi.
031700131008       //--------------------------------------------------------------
031800140519      /copy gaitrasrc/srcprotopr,Fnlry00r
031900140520      /copy gaitrasrc/srcprotopr,TIBS10R
032000140521      /copy gaitrasrc/srcprotopr,TRUL28R0
032100140521      /copy gaitrasrc/srcprotopr,FIDN12R
032200140522      /copy gaitrasrc/srcprotopr,tnsd99r
032300131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
032400131230      /copy gaitrasrc/srcprotopr,TIBS0800R
032500140311      /copy gaitrasrc/srcprotopr,UBCHKCEL
032600140311      /copy gaitrasrc/srcprotopi,UBCHKCEL
032700140311      /copy gaitrasrc/srcprotopr,XEMAIL
032800140528      /copy gaitrasrc/srcprotopr,xsrlav8
032900140603      /copy gaitrasrc/srcprotopr,fnlv55r
033000131219
033100131219       //--------------------------------------------------------------
033200131219       //?Definizione parametri programma.
033300131219       //--------------------------------------------------------------
033400140519     D Fnlry00_Immetti...
033500140519     D                 PI
033600140519     D prmRqsOpCode...
033700140519     D                               10I 0 CONST
033800140519     D prmRpyOpCode...
033900140519     D                               10I 0
034000140519     D prmRpyIdMsg...
034100140519     D                               10I 0
034200140519     D prmRqsFormato...
034300140519     D                               10A   CONST
034400140519     D prmRqsData...
034500140519     D                            32767A   OPTIONS(*VARSIZE)
034600140519     D prmRqsDataSize...
034700140519     D                               10I 0 CONST
034800140519     D prmRpyFormato...
034900140520     D                               10A   CONST
035000140519     D prmRpyData...
035100140519     D                            32767A   OPTIONS(*VARSIZE)
035200140519     D prmRpyDataSize...
035300140519     D                               10I 0 CONST
035400140519     D prmRpyFormMsg...
035500140519     D                               10A   CONST OPTIONS(*NOPASS)
035600140519     D prmRpyMessage...
035700140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
035800140519     D prmRpyMsgSize...
035900140519     D                               10I 0 CONST OPTIONS(*NOPASS)
036000140519     D prmRpyFormForz...
036100140519     D                               10A   CONST OPTIONS(*NOPASS)
036200140519     D prmRpyForzatu...
036300140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
036400140519     D prmRpyForSize...
036500140519     D                               10I 0 CONST OPTIONS(*NOPASS)
036600131007
036700131007       //--------------------------------------------------------------
036800131007       //?MAIN.
036900131007       //--------------------------------------------------------------
037000131008
037100131007      /free
037200131008
037300131008       //?Operazioni iniziali
037400131008       exsr RoutInz;
037500131008
037600140523       //?Controllo formale dei dati di accesso
037700131008       exsr Controlla;
037800140523       //?Controllo stato bolla: deve essere in uno stato che renda possibile
037900140523       //?l'immissione delle disposizioni di consegna
038000140523       exsr StatoBolla;
038100140529
038200140523       //?Controllo che l'immagine della bolla da Web sia la stessa della bolla su
038300140523       //?AS/400
038400140529       //IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
038500140529       //   exsr ImmagineB;
038600140529       //endif;
038700140529
038800140529       //?Chaino estensioni bolla
038900140529       exsr sr_chainEstBolla;
039000140523       //?Controllo dettaglio dei campi ricevuti
039100140521       exsr ControllaDett;
039200140603       //?Per PUTISTRUCO in modalità Controllo + Inserimento scrivo file TIIDC
039300140611       IF  prmRqsOpCode=FNLRY00_RQSOPCODE_PUT_ISTRUCO and fnlry00i1.tipoela='2'
039400140611                                                      and fnlry00m0.nrmsg=0;
039500140604          exsr sr_Tiidc;
039600140613       // Restituzione messaggio di conferma
039700140613          exsr sr_MsgConf;
039800140603       endif;
039900131010
040000131010       //?Operazioni finali
040100131010       exsr RoutEnd;
040200131008
040300140613       //--------------------------------------------------------------
040400140613       //?Messaggio di avvenuta registrazione delle Indicazioni di Cons.
040500140613       //--------------------------------------------------------------
040600140613       BEGSR  sr_MsgConf;
040700140613
040800140929       // MESSAGGIO GENERICO
040900140617       wIdMsg   = 'TIS0771';
041000140617       fnlry00o1.MSGCONFERM=rtvMsgLang(wIdMsg:widlingua);
041100141007       fnlry00o1.MSGCONFERM=%trim(fnlry00o1.MSGCONFERM)+' del '
041200141007                                                  + %char(%date():*eur)
041300141007                                                  + ' '+  %char(%time()) ;
041400140613
041500140929       // MESSAGGIO SPECIFICO per Fermo Deposito e Consegna richiesta:
041600140929       // FERMO DEPOSITO
041700140929       select;
041800140929       when fnlry00i1.tipodispo='2';
041801161130        if w_tbo='A'    ;
041900140930          exsr sr_wdesgio;
042000140924          wIdMsg   = 'TIS0772';
042100140924          wparm=wdesgio + %editc(WlimitFD:'X') ;
042200140929          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
042300140617          fnlry00o1.fdidfil=wfgslna;
042301161130        else ;
042302161130        endif;
042400140929       // APPUNTAMENTO
042500140929       when fnlry00i1.tipodispo='1';
042600140930          wIdMsg   = 'TIS0787';
042700140930          wparm=fnlry00i1.newdtcrich ;
042800140929       // presente orario
042900140929          if fnlry00i1.neworcrich>*zeros;
043000140930             wIdMsg   = 'TIS0785';
043100140930             w_orcric=fnlry00i1.neworcrich;
043200140930             exsr sr_dallealle;
043300140930            wparm=fnlry00i1.newdtcrich + w_dALLEmsg + W_ALLEMSG;
043400140929          endif;
043500140929       // Presente preferenza matt/pomeriggio
043600140929          if fnlry00i1.prconsric<>*blanks;
043700140929             wconsric=fnlry00i1.prconsric;
043800140929             exsr sr_matpom;
043900140930             wIdMsg   = 'TIS0789';
044000140929             wparm=fnlry00i1.newdtcrich + matpom ;
044100140929          endif;
044200140930          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
044300140929       // ALTRO INDIRIZZO
044400140930       when fnlry00i1.tipodispo='3';
044500140930       // presente orario --> determino il "Dalle - Alle"
044600140930          if fnlry00i1.neworcrici>*zeros;
044700140930             w_orcric=fnlry00i1.neworcrici;
044800140930             exsr sr_dallealle;
044900140930          endif;
045000140930       // Presente preferenza matt/pomeriggio --> Traduco il flag nella descrizione
045100140930          if fnlry00i1.prconsrici<>*blanks;
045200140930             wconsric=fnlry00i1.prconsrici;
045300140930             exsr sr_matpom;
045400140930          endif;
045500141001       // Messaggi in presenza della data
045600140930          if fnlry00i1.newdtcrici>*zeros;
045700140930             wIdMsg   = 'TIS0788';
045800140930             wparm=fnlry00i1.newdtcrici ;
045900140930             if fnlry00i1.neworcrici>*zeros;
046000140930                wIdMsg   = 'TIS0786';
046100140930                wparm=fnlry00i1.newdtcrici + w_dALLEmsg + W_ALLEMSG;
046200140930             endif;
046300140930       // Presente preferenza matt/pomeriggio
046400140930             if fnlry00i1.prconsrici<>*blanks;
046500140930                wIdMsg   = 'TIS0790';
046600140930                wparm=fnlry00i1.newdtcrici + matpom ;
046700140930             endif;
046800141001             fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
046900140930          else;
047000141001       // Messaggi in assenza della data (caso che non si verifica perchè se presente
047100141001       //   ora o preferenza devo avere anche la data)
047200140930             if fnlry00i1.neworcrici>*zeros;
047300140930                wIdMsg   = 'TIS0791';
047400140930                wparm=w_dALLEmsg + W_ALLEMSG;
047500141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
047600140930             endif;
047700140930       // Presente preferenza matt/pomeriggio
047800140930             if fnlry00i1.prconsrici<>*blanks;
047900140930                wIdMsg   = 'TIS0792';
048000140930                wparm=matpom ;
048100141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
048200140930             endif;
048300140930          endif;
048400140929
048500140929       endsl;
048600140613
048700140613       endsr;
048800140930       //--------------------------------------------------------------
048900140930       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
049000140930       //--------------------------------------------------------------
049100140930       BEGSR  sr_dallealle;
049200140930
049300140930       //   determino il Dalle/alle da mettere nel messaggio
049400140930            w_dalle=%time((%dec(w_orcric:4:0)*100):*hms)-%minutes(§VPORST_MW);
049500140930            w_alle=%time((%dec(w_orcric:4:0)*100):*hms)+%minutes(§VPORST_PW);
049600140930       //   determino orari servizio comprensivi della tolleranza
049700140930            if §ivptipo='S';
049800140930               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
049900140930               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
050000140930            else;
050100140930               w_dalleoser=%time((oor2miis *100):*hms)-%minutes(§VPORST_MT);
050200140930               w_alleoser=%time((oor2mxfs *100):*hms)+%minutes(§VPORST_MT);
050300140930            endif;
050400140930       //   Confronto dalle/alle calcolato con dalle/alle orari ser
050500140930       //      Dalle < del "dalle" orario ser --> prendo quest'ultimo e l'alle
050600140930       //      lo ricalcolo aggiungendo i minuti da tabella vpo
050700140930            if w_dalle<w_dalleoser;
050800140930               w_dalle=w_dalleoser;
050900140930               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
051000140930            endif;
051100140930       //      Alle  > del "alle" orario ser --> prendo quest'ultimo e il dalle
051200140930       //      lo ricalcolo sottraendo i minuti da tabella vpo purchè non diventi
051300140930       //      minore del "dalle" orario ser
051400140930            if w_alle>w_alleoser;
051500140930               w_alle=w_alleoser;
051600140930               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
051700140930                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
051800140930               endif;
051900140930            endif;
052000140930            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
052100140930            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
052200140930
052300140930       endsr;
052400131008       //--------------------------------------------------------------
052500131008       //?Operazioni iniziali.                                         ?
052600131008       //--------------------------------------------------------------
052700131008       BEGSR  RoutInz;
052800140211
052900140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
053000131008
053100140519         prmRpyOpCode = FNLRY00_RPYOPCODE_DONE;
053200140519         prmRpyIdMsg  = FNLRY00_ESITO_OK;
053300131009
053400131009       //?Data del giorno
053500131009         dataoggi = %dec(%date());
053600140604         annocur  = %subdt(%date():*years);
053700131009
053800131009       //?Pulisco ds di output
053900140519         reset Fnlry00o0;
054000140519         reset fnlry00o1;
054100140522         reset fnlry00m0;
054200140521
054300140521       //?Apertura file
054400140521         wlibfil=wlibfil201;
054500140521         %subst(wlibfil:11:8)='FNARB01L';
054600140521         open(e) fnarb01l;
054700140521         if not %open(fnarb01l);
054800140521            wlibfil=wlibfilprd;
054900140521            %subst(wlibfil:11:8)='FNARB01L';
055000140521            open fnarb01l;
055100140521         endif;
055101161128         %subst(wlibfil:11:8)='FNBLP01L';
055102161128         open(e) fnblp01l;
055103161128         if not %open(fnblp01l);
055104161128            wlibfil=wlibfilprd;
055105161128            %subst(wlibfil:11:8)='FNBLP01L';
055106161128            open fnblp01l;
055107161128         endif;
055200140521         wlibfil=wlibfil201;
055300140521         %subst(wlibfil:11:8)='FIPCT02L';
055400140521         open(e) fipct02l;
055500140521         if not %open(fipct02l);
055600140521            wlibfil=wlibfilprd;
055700140521            %subst(wlibfil:11:8)='FIPCT02L';
055800140521            open fipct02l;
055900140521         endif;
056000140522         wlibfil=wlibfil201;
056100140522         %subst(wlibfil:11:8)='FIAR501L';
056200140522         open(e) fiar501l;
056300140522         if not %open(fiar501l);
056400140522            wlibfil=wlibfilprd;
056500140522            %subst(wlibfil:11:8)='FIAR501L';
056600140522            open fiar501l;
056700140522         endif;
056800140528         wlibfil=wlibfil201;
056900140528         %subst(wlibfil:11:8)='FIAR401L';
057000140528         open(e) fiar401l;
057100140528         if not %open(fiar401l);
057200140528            wlibfil=wlibfilprd;
057300140528            %subst(wlibfil:11:8)='FIAR401L';
057400140528            open fiar401l;
057500140528         endif;
057600140807         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
057700140807            open tivpi00f ;
057800140807         endif ;
057900140528
058000140528       // Reperimento G.LIMITE X GIAC.CON DATA RIC
058100140528       clear wlimitdcr;
058200140528       chain (1:'2G':'1       ') tabel00f;
058300140528       if %found(tabel00f);
058400140528          ds2g=tbluni;
058500140528          wlimitdcr=§2gldr;
058600140528       endif;
058700140603       // Reperimento dati tabella IVP - Variabili programma per INTERNET
058800140603       clear tibs02ds;
058900140603       clear divp;
059000140603       t02mod='C';
059100140603       t02cod='IVP';
059200140603       t02ke1='1  ';
059300140603       t02tla='L  ';
059400140603       tibs02r(kpjba:tibs02ds);
059500140603       if t02err=*blanks;
059600140603          divp=t02uni;
059700140603       endif;
059800140613       // Reperimento Giorni lavorativi entro cui ritirare le sped. in FD
059900140613       clear wlimitFD ;
060000140613       chain (1:'2A':'F       ') tabel00f;
060100140613       if %found(tabel00f);
060200140613          ds2a=tbluni;
060300140613          wlimitFD =§2agla;
060400140613       endif;
060500140924       // Reperimento dati tabella VPO - ORARISER
060600140924       clear tibs02ds;
060700140925       clear dvpooraris;
060800140924       t02mod='C';
060900140924       t02cod='VPO';
061000140925       t02ke1='ORARISER';
061100140924       t02tla='L  ';
061200140924       tibs02r(kpjba:tibs02ds);
061300140924       if t02err=*blanks;
061400140924          dvpooraris=t02uni;
061500140924       endif;
061600141007       // Reperimento dati tabella CSP - Caratteri speciali per eliminazione
061700141007      *                           caratteri di punteggiatura da loc.dest.
061800141007       clear tibs02ds;
061900141007       clear charnook  ;
062000141007       t02mod='C';
062100141007       t02cod='CSP';
062200141007       t02ke1='LOC';
062300141007       t02tla='L  ';
062400141007       tibs02r(kpjba:tibs02ds);
062500141007       if t02err=*blanks;
062600141007          charnook=t02uni;
062700141007       endif;
062800141007     c* imposto stringa contenente caratteri speciali per il controllo
062900141007     c* della parte finale (uguale alla charnook maa senza l'eventuale '.')
063000141007     c                   eval      charnookf=%xlate('.':' ':charnook)
063001161128     c* carico le filiali abilitate all'immissione dispo di consegna su bolle in partenza
063002161128     c                   clear                   trulvpods
063003161128     c                   eval      ivpoke1 = 'DECOFIIDC'
063004161128     c                   call      'TRULVPOR'
063005161128     c                   parm                    trulvpods
063006161129      *
063007161129     c                   eval      wDecofiAll=*off
063008161129     c                   if        %lookup('999':skFilOk)>0
063009161129     c                   eval      wDecofiAll=*on
063010161129     c                   endif
063011161128     c
063100140521      /end-free
063200140521     C     Kcet          KLIST
063300140521     C                   KFLD                    pctfgs
063400140521     C                   KFLD                    pctNDC
063500140521     C                   KFLD                    arbpdc
063600140521     C                   KFLD                    TTRD              3
063700140521     C                   KFLD                    arbAAS
063800140521     C                   KFLD                    arbLNP
063900140521     C                   KFLD                    arbNRS
064000140521     C                   KFLD                    arbNSP
064100140521      /free
064200131008
064300131008         *inLR = *on;
064400131008
064500131008       ENDSR;
064600131008
064700131008       //--------------------------------------------------------------
064800131008       //?Controllo formale dei dati passati.
064900131008       //--------------------------------------------------------------
065000131008       BEGSR  Controlla;
065100131008
065200140519         clear wksu;
065300140520         clear widbolla;
065400140520         clear wbrtcode;
065500140522         clear wchi    ;
065600140527         clear widlingua;
065700140527         clear wcampo;
065800140519
065900131008       //?OpCode
066000140519         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_GET_ISTRUCO  and
066100140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_PUT_ISTRUCO  and
066200140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
066300140519           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
066400140519           prmRpyIdMsg  = FNLRY00_ESITO_RQSOPCODE_SCONOSCIUTO;
066500140210           exsr RoutEnd;
066600131008         ENDIF;
066700140408
066800140519       //?per GET_ISTRUCO
066900140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
067000140408       //?Formato e size RQS
067100140519           IF  prmRqsFormato = fnlry00I0.formato;
067200140519             fnlry00I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
067300140519             wksu=fnlry00i0.ksu;
067400140520             widbolla=fnlry00i0.idbolla;
067500140520             wbrtcode=fnlry00i0.brtcode;
067600140527             widlingua=fnlry00i0.liniso2;
067700140408           ELSE;
067800140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
067900140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
068000140408             exsr RoutEnd;
068100140408           ENDIF;
068200140408           IF  prmRqsDataSize > 0;
068300140408           ELSE;
068400140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
068500140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
068600140408             exsr RoutEnd;
068700140408           ENDIF;
068800140408         //?Formato e size RPY
068900140519           IF  prmRpyFormato = fnlry00O0.formato;
069000140408           ELSE;
069100140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
069200140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
069300140408             exsr RoutEnd;
069400140408           ENDIF;
069500140408           IF  prmRpyDataSize > 0;
069600140408           ELSE;
069700140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
069800140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
069900140408             exsr RoutEnd;
070000140408           ENDIF;
070100140408         ENDIF;
070200140210
070300140522       //?per PUT_ISTRUCO
070400140522         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
070500140522       //?Formato e size RQS
070600140522           IF  prmRqsFormato = fnlry00I1.formato;
070700140522             fnlry00I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
070800140522             wksu=fnlry00i1.ksu;
070900140522             widbolla=fnlry00i1.idbolla;
071000140522             wbrtcode=fnlry00i1.brtcode;
071100140527             widlingua=fnlry00i1.liniso2;
071200140522           ELSE;
071300140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
071400140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
071500140522             exsr RoutEnd;
071600140522           ENDIF;
071700140522           IF  prmRqsDataSize > 0;
071800140522           ELSE;
071900140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
072000140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
072100140522             exsr RoutEnd;
072200140522           ENDIF;
072300140522         //?Formato e size RPY
072400140522           IF  prmRpyFormato = fnlry00O1.formato;
072500140522           ELSE;
072600140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
072700140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
072800140522             exsr RoutEnd;
072900140522           ENDIF;
073000140522           IF  prmRpyDataSize > 0;
073100140522           ELSE;
073200140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
073300140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
073400140522             exsr RoutEnd;
073500140522           ENDIF;
073600140522       //?Formato e size MSG
073700140522         IF  prmRpyFormMsg = FNLRY00M0.formato;
073800140522         ELSE;
073900140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
074000140522           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
074100140522           exsr RoutEnd;
074200140522         ENDIF;
074300140522         IF  prmRpyMsgSize > 0;
074400140522         ELSE;
074500140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
074600140522           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
074700140522           exsr RoutEnd;
074800140522         ENDIF;
074900140523       //?Tipo Elaborazione
075000140523         if fnlry00i1.tipoela<> '1' and fnlry00i1.tipoela<> '2';
075100140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
075200140523           prmRpyIdMsg = FNLRY00_ESITO_TIPOELA_ERRATO;
075300140523           exsr RoutEnd;
075400140523         endif;
075500140523       //?Tipo Disposizione
075600140523         if fnlry00i1.tipodispo <> '1' and
075700140523            fnlry00i1.tipodispo <> '2' and
075800140523            fnlry00i1.tipodispo <> '3' and
075900151112            fnlry00i1.tipodispo <> '4' and
076000151112            fnlry00i1.tipodispo <> '5'     ;
076100140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
076200140523           prmRpyIdMsg = FNLRY00_ESITO_TIPODISPO_ERRATO;
076300140523           exsr RoutEnd;
076400140523         endif;
076500140522         ENDIF;
076600140624
076700140624       //?per CHK_ISTRUCO
076800140624         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
076900140624       //?Formato e size RQS
077000140624           IF  prmRqsFormato = FNLRY00_RQSFORMATO_CHK_ISTRUCO;
077100140624             TIIDCDS = %SUBST(prmRqsData:1:prmRqsDataSize);
077200140624             wksu=idcidcli     ;
077300141223             widbolla=%subst(%editc(idcwaas:'X'):3:2) + %editc(idcwlnp:'X') +
077400141223                             %editc(idcwnrs:'X') + %editc(idcwnsp:'X');
077500140627             if wksu=*blanks;
077600140627             wbrtcode=widbolla;
077700140627             exsr sr_trul28;
077800140627             wbrtcode=o280cod ;
077900140627             else;
078000140624             clear wbrtcode;
078100140627             endif;
078200140624             widlingua='it';
078300140624           ELSE;
078400140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
078500140624             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
078600140624             exsr RoutEnd;
078700140624           ENDIF;
078800140624           IF  prmRqsDataSize > 0;
078900140624           ELSE;
079000140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
079100140624             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
079200140624             exsr RoutEnd;
079300140624           ENDIF;
079400140624       // per comodità valorizzo campi FNLRY00I1 prendendoli dal record del TIIDC
079500140624           fnlry00i1.TIPODISPO = IDCTIPODIS;
079600140624           fnlry00i1.NEWREFCONS= IDCREF;
079700140624           fnlry00i1.NEWTELCONS= IDCTELD;
079800140624           fnlry00i1.NEWDESMAIL= IDCMAILAVV;
079900140624           fnlry00i1.NEWDESCELL= IDCTELAVV ;
080000140923       // Se no dispo di indirizzo alternativo
080100140923           if fnlry00i1.tipodispo<>'3';
080200140923              if idcdcr>0;
080300140923                 fnlry00i1.NEWDTCRICH= %char(%date(IDCDCR:*iso):*eur);
080400140923              endif;
080500140923              fnlry00i1.NEWORCRICH= %editc(IDCHCR:'X') ;
080600140923              fnlry00i1.NEWTPCRICH= IDCTCR    ;
080700140923              fnlry00i1.PRCONSRIC = IDCPRCR   ;
080800140923           endif;
080900140624           fnlry00i1.NEWRSD    = IDCRSD    ;
081000140624           fnlry00i1.NEWIND    = IDCIND    ;
081100140624           fnlry00i1.NEWCAD    = IDCCAD    ;
081200140624           fnlry00i1.NEWLOD    = IDCLOD    ;
081300140624           fnlry00i1.NEWPRD    = IDCPRD    ;
081400140624           fnlry00i1.NEWNZD    = IDCNAZ    ;
081500140624           fnlry00i1.ALTRO     = IDCNOTE   ;
081600140923       // Se indirizzo alternativo
081700140923           if fnlry00i1.tipodispo='3';
081800140923              if idcdcr>0;
081900140923                 fnlry00i1.NEWDTCRICI=%char(%date(IDCDCR:*iso):*eur);
082000140923              endif;
082100140923                 fnlry00i1.NEWORCRICi= %editc(IDCHCR:'X') ;
082200140923                 fnlry00i1.NEWTPCRICi= IDCTCR    ;
082300140923                 fnlry00i1.PRCONSRICi= IDCPRCR   ;
082400140923           endif;
082500140624
082600140624         //?Formato e size RPY
082700140624       //?Formato e size MSG
082800140624         IF  prmRpyFormMsg = FNLRY00M0.formato;
082900140624         ELSE;
083000140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
083100140624           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
083200140624           exsr RoutEnd;
083300140624         ENDIF;
083400140624         IF  prmRpyMsgSize > 0;
083500140624         ELSE;
083600140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
083700140624           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
083800140624           exsr RoutEnd;
083900140624         ENDIF;
084000140624         ENDIF;
084100140522
084200140210
084300140210       //?Formato e size Forzature
084400140522       //IF  prmRpyFormForz = fNLRY00F0.formato;
084500140522       //ELSE;
084600140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
084700140522       //  prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
084800140522       //  exsr RoutEnd;
084900140522       //ENDIF;
085000140522       //IF  prmRpyForSize > 0;
085100140522       //ELSE;
085200140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
085300140522       //  prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
085400140522       //  exsr RoutEnd;
085500140522       //ENDIF;
085600140519
085700140520       //?devo ricevere ID cliente unificante o BRTCODE
085800140520         if wksu=*blanks and wbrtcode=*blanks;
085900140520           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
086000140520           prmRpyIdMsg = FNLRY00_ESITO_ID_RICHIAMO_ERRATO;
086100140520           exsr RoutEnd;
086200140520         endif;
086300140520       //?Accesso per cliente unificante e ID Bolla
086400140519         if wksu <>*blanks;
086500140520       // ID Bolla mancante
086600140520           if widbolla=*blanks;
086700140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
086800140527              wIdMsg   = 'TIS0734';
086900140527              wIdCampo = 'IDBOLLA   ';
087000140527              clear wParm;
087100140527              exsr Messaggi;
087200140520              exsr RoutEnd;
087300140520           endif;
087400141003       // KSU può contenere solo numeri
087500141003           if %check(digits:wksu)>0;
087600141003                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
087700141003                      wIdMsg   = 'TIS0736';
087800141003                      wIdCampo = 'KSU       ';
087900141003                      wParm = widbolla;
088000141003                      exsr Messaggi;
088100141003                      exsr RoutEnd;
088200141003           endif;
088300140520       // ID cliente unificante
088400140520           clear tibs10ds ;
088500140520           d10tle='WW'  ;
088600140520           d10paf='F'    ;     // verifico se è padre
088700140520           d10cod=%dec(wksu:8:0);
088800140520           TIBS10R (tibs10ds)   ;
088900140520  1        if d10err<>*blanks   ;
089000140520           // Se errore verifico se è figlio
089100140520              clear tibs10ds ;
089200140520              d10tle='WW'        ;
089300140520              d10paf='P'    ;
089400140520              d10cod=%dec(wksu:8:0);
089500140520              TIBS10R (tibs10ds)   ;
089600140520           endif;
089700140520
089800140520  2        if d10err<>*blanks   ;
089900140520             // Imposto nella skiera e come padre solo se stesso
090000140520             skc(1)=%editc(d10cod:'X');
090100140520             d10cop=d10cod      ;
090200140520  2        endif                ;
090300140520       // Verifico correttezza dell'ID bolla
090400140520       // Deve contenere solo numeri
090500140520         if %check(digits:widbolla)>0;
090600140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
090700140527              wIdMsg   = 'TIS0734';
090800140527              wIdCampo = 'IDBOLLA   ';
090900140527              clear wParm;
091000140527              exsr Messaggi;
091100140520              exsr RoutEnd;
091200140520         endif;
091300140529       // Se ricevuto anche il BRTCODE lo controllo
091400140529         if wbrtcode<>*blanks;
091500140529            exsr sr_trul28;
091600140529       //   Id bolla e brtcode devono riferirsi alla stessa spedizione
091700140529            if widbolla<>%subst(wbrtcode:1:14) ;
091800140529              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
091900140529              wIdMsg   = 'TIS0751';
092000140529              wIdCampo = 'IDBOLLA   ';
092100140530              wparm = wbrtcode + widbolla;
092200140529              exsr Messaggi;
092300140529              wIdCampo = 'BRTCODE   ';
092400140529              exsr Messaggi;
092500140529              exsr RoutEnd;
092600140529            endif;
092700140529         endif;
092800161128       // La bolla deve esistere su archivio bolle:
092900140521          wbolla=widbolla;
093000140527          wcampo='IDBOLLA';
093100140521          exsr sr_chainbolla;
093200140520       // Verifico che il cliente sia il mittente o il destinatario della bolla
093300140522             if arbccm>0;
093400140522                eval wcliente='0000'+%editc(arbccm:'X');
093500140522                if %lookup(wcliente:skc)=0;
093600140522                   eval wcliente='0000'+%editc(arbksc:'X');
093700140522                   if %lookup(wcliente:skc)=0;
093800140522                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
093900140527                      wIdMsg   = 'TIS0736';
094000140527                      wIdCampo = 'KSU       ';
094100140530                      wParm = widbolla;
094200140527                      exsr Messaggi;
094300140522                      exsr RoutEnd;
094400140522                   else;
094500140522                      wchi='D';
094600140522                   endif;
094700140522                else;
094800140522                   wchi='M';
094900140522                endif;
095000140522             else;
095100140522                eval wcliente='0000'+%editc(arbksc:'X');
095200140522                if %lookup(wcliente:skc)=0;
095300140522                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
095400140527                   wIdMsg   = 'TIS0736';
095500140527                   wIdCampo = 'KSU       ';
095600140530                   wParm = widbolla;
095700140527                   exsr Messaggi;
095800140522                   exsr RoutEnd;
095900140522                else;
096000140610                   exsr sr_ds3A;
096100140528                      if %subst(§3atb1:1:1)='F';
096200140522       //                se franco    --> mittente
096300140522                         wchi='M';
096400140522                      else;
096500140522       //                se Assegnato --> Destinatario
096600140522                         wchi='D';
096700140522                      endif;
096800140522                endif;
096900140522             endif;
097000140519         else;
097100140520       //?Accesso per BRTCode
097200140529          exsr sr_trul28;
097300161128       // La bolla deve esistere su archivio bolle:
097400140521          wbolla=%subst(wbrtcode:1:14);
097500140527          wcampo='BRTCODE';
097600140521          exsr sr_chainbolla;
097700140522          wchi='D';
097800140519         endif;
097900140620       //?Verifico se presenti legami bolla
098000140620         exsr sr_lbl;
098100131008
098200131008       ENDSR;
098300140529       //--------------------------------------------------------------
098400140529       //?Controllo correttezza BRTCODE
098500140529       //--------------------------------------------------------------
098600140529       BEGSR  sr_trul28 ;
098700140529          clear trul28ds0;
098800140529          i280cod=wbrtcode;
098900140529          trul28r0(trul28ds0);
099000140529          if o280err='1';
099100140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
099200140529             wIdMsg   = 'TIS0737';
099300140529             wIdCampo = 'BRTCODE   ';
099400140530             wparm = wbrtcode ;
099500140529             exsr Messaggi;
099600140529             exsr RoutEnd;
099700140529          endif;
099800140529       ENDSR;
099900140523       //--------------------------------------------------------------
100000140523       //?Controlli stato della bolla per abilitare le istruzioni di consegna
100100161202       //--------------------------------------------------------------
100200140523       BEGSR  statobolla;
100300140530          clear wtentacons;
100400140620          clear wdcreur;
100500140620          clear wrsd2  ;
100600140620
100700140620       // DETErMINO IL GESTORE DELLA LNA
100800140620         clear fnlv55ds;
100900140620         d55lin=arblna;
101000140620         d55tpt='6';
101100140620         d55drf=dataoggi;
101200140620         fnlv55r(fnlv55ds);
101300140620         wfgslna=d55tfA;
101400140620         clear wfgslnad;
101500140620         chain (wfgslna) azorg01l;
101600140620         if %found(azorg01l);
101700140620            wfgslnad=orgdes;
101800140620         endif;
101900140620
102000140620         clear dataeur;
102100140620         if arbdcr>0 ;
102200140620            dataeur=%date(arbdcr:*iso);
102300140620            wdcreur=%char(dataeur);
102400140620         endif;
102500140620
102600140620         clear wrbhcr ;
102700140620         if arbhcr>0 ;
102800140620            wrbhcr=%editc(arbhcr:'X');
102900140620         endif;
103000140620
103100140620       //wtprich=%subst(arbgc1:2:1);
103200140620       //if wtprich=*blanks;
103300140620       //    wtprich=%subst(arbgc2:2:1);
103400140620       //endif;
103500140620
103600140620            chain (arbaas:arblnp:arbnrs:arbnsp:'D') fiar401l ;
103700140620            if %found(fiar401l);
103800140620               eval wrsd2=%subst(ar4not:1:35);
103900140620            endif;
104000140620
104100140530       //?deve essere diretta in Italia
104200140530         clear og143;
104300140530         chain (arblna) azorg01l;
104400140530         if %found(azorg01l);
104500140530            og143=orgde3;
104600140530            if §ogntw = 'EEX' or §ogntw= 'FED' or §ogntw= 'DPD';
104700140530               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
104800140530               wIdMsg   = 'TIS0739';
104900140530               wIdCampo = '*BOLLA    ';
105000140530               wParm = wbolla;
105100140530               exsr Messaggi;
105200140530               exsr routend;
105300140530            endif;
105400140530         endif;
105401161129        if w_tbo='A';
105500140523       //?Spedizione non deve essere consegnata
105600140523          if arbdcm>0;
105700141007            IF  prmRqsOpCode =  FNLRY00_RQSOPCODE_CHK_ISTRUCO;
105800141006                wIdMsg   = 'TISA738';
105900141006                wIdCampo = '*BOLLA    ';
106000141006                clear wParm ;
106100141006            else;
106200140527                wIdMsg   = 'TIS0738';
106300140527                wIdCampo = '*BOLLA    ';
106400140530                wParm = wbolla ;
106500141006            endif;
106600141006                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
106700140527                exsr Messaggi;
106800141006                exsr routend;
106900140523          else;
107000140523      /end-free
107100140523     c* prima vedo se c'e' da PDA
107200140708     c                   exsr      sr_RepPct
107300140523      /free
107400140523          endif;
107500140523       //?non deve avere giacenze o c.a. aperte
107600140620         chain (Arbaas:arblnp:arbnrs:arbnsp) tigcp21l;
107700140523         if %found(tigcp21l) and gcpatb=' ' and gcpdcg=0;
107800140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
107900141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
108000140527            wIdMsg   = 'TIS0740';
108100140527            wIdCampo = '*BOLLA    ';
108200140619            wParm = wbolla + wfgslnad;
108300141006           else;
108400141006            wIdMsg   = 'TISA740';
108500141006            wIdCampo = '*BOLLA    ';
108600141006            clear wParm ;
108700141006           endif;
108800140527            exsr Messaggi;
108900140523            exsr routend;
109000140523         endif;
109100140523         clear fidn12ds;
109200140620         i12aas=arbaas;
109300140620         i12lnp=arblnp;
109400140620         i12nrs=arbnrs;
109500140620         i12nsp=arbnsp;
109600140523         i12fel='E';
109700140523         i12fan='E';
109800140523         i12fch='E';
109900140523         fidn12r(fidn12ds);
110000140523         if o12nca>0;
110100140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
110200141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
110300140527            wIdMsg   = 'TIS0741';
110400140527            wIdCampo = '*BOLLA    ';
110500140530            wParm = wbolla;
110600141006           else;
110700141006            wIdMsg   = 'TISA741';
110800141006            wIdCampo = '*BOLLA    ';
110900141006            clear wParm ;
111000141006           endif;
111100140527            exsr Messaggi;
111200140523            exsr routend;
111300140523         endif;
111400140619       //?non deve essere bloccata
111500140917         if arbfbc<>*blanks and arbfbc<>'A';
111600140619            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
111700141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
111800140619            wIdMsg   = 'TIS0740';
111900140619            wIdCampo = '*BOLLA    ';
112000140619            wParm = wbolla + wfgslnad;
112100141006           else;
112200141006            wIdMsg   = 'TISA740';
112300141006            wIdCampo = '*BOLLA    ';
112400141006            clear wParm ;
112500141006           endif;
112600140619            exsr Messaggi;
112700140619            exsr routend;
112800140619         endif;
112801161129        endif;
112900140910       //?non deve avere un codice mancata consegna di tipo giacenza
113000140911       //if arbcmc <> *blanks;
113100140911       //  clear ds2a;
113200140911       //  w008a = arbcmc;
113300140911       //  chain (1:'2A':w008a) tabel00f;
113400140911       //  if %found(tabel00f);
113500140911       //     ds2a=tbluni;
113600140911       //  endif;
113700140911       //  if §2aftc='G';
113800140911       //     prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
113900140911       //     wIdMsg   = 'TIS0740';
114000140911       //     wIdCampo = '*BOLLA    ';
114100140911       //     wParm = wbolla + wfgslnad;
114200140911       //     exsr Messaggi;
114300140911       //     exsr routend;
114400140911       //  endif;
114500140911       //endif;
114600140530       //?deve essere già stato fatto almeno un tentativo di consegna
114601161129        if wDecofiAll=*on or %lookup(%editc(arblna:'X'):skFilOk)=0 ;
114700161202         if §IVPTENC='S' and (W_tbo='P' or (arbntc=0 and wtentacons=*blanks))  ;
114800140530             wIdMsg   = 'TIS0752';
114900140530             wIdCampo = '*NOTENTAT ';
115000140530             wParm = wbolla;
115100140530             exsr Messaggi;
115200140530             exsr routend;
115300140530         endif;
115301161129        endif;
115400141016       //?non deve essere una spedizione per i supermercati
115500141016         if arbtc1='S' or arbtc2='S'                          ;
115600141016             wIdMsg   = 'TIS0812';
115700141016             wIdCampo = '*BOLLA    ';
115800141016             clear wparm;
115900141016             exsr Messaggi;
116000141016             exsr routend;
116100141016         endif;
116200140604       //?Per la spedizione non devono esserci disposizioni ancora da elaborare
116300140624         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
116400140604          exsr sr_ChkDispo;
116500140624         endif;
116600140530       //?NON TROVATI ERRORI:
116700140530       //?Per GET_ISTRUCO --> attivo il bottone e restituisco i dati della bolla
116800140527         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
116900140527            fnlry00o0.BUTISTRCON='1';
117000140527       // restituisco anche dati relativi alla bolla
117100140527            exsr sr_GetDatiBolla;
117200140527            endif;
117300140523
117400140527       endsr;
117500140523       //--------------------------------------------------------------
117600140523       //?Controlli immagine bolla da WEB con bolla su ARB
117700140523       //--------------------------------------------------------------
117800140523       BEGSR  immagineB;
117900140523       // Chaino estensioni bolla
118000140529       // exsr sr_chainEstBolla;
118100140523       // Errore se il referente di consegna sulla bolla è diverso dall'immagine bolla
118200140523       // che ricevo
118300140523          if wrefbolla<>fnlry00i1.oldrefcons or
118400140523             wtelbolla<>fnlry00i1.oldtelcons or
118500140523             §ar5eml<>fnlry00i1.olddesmail or
118600140528             §ar5tel<>fnlry00i1.olddescell           ;
118700140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
118800140527               wIdMsg   = 'TIS0742';
118900140528               wIdCampo = '*CHGDATA  ';
119000140527               clear wParm;
119100140527               exsr Messaggi;
119200140523               exsr routend;
119300140523          endif;
119400140527         select;
119500140527       // Disposizioni di Appuntamento
119600140527         when fnlry00i1.tipodispo = '1';
119700140527          if wdcreur<>fnlry00i1.olddtcrich or
119800140529             wrbhcr<>fnlry00i1.oldorcrich  or
119900140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
120000140527               wIdMsg   = 'TIS0742';
120100140527               wIdCampo = '*CHGDATA  ';
120200140527               clear wParm;
120300140527               exsr Messaggi;
120400140527               exsr routend;
120500140527          endif;
120600140527       // Disposizioni di consegna ad altro indirizzo
120700140527         when fnlry00i1.tipodispo = '3';
120800140527          if arbrsd+wrsd2 <>fnlry00i1.oldrsd or
120900140527             arbind<>fnlry00i1.oldind or
121000140527             arbcad<>fnlry00i1.oldcad or
121100140527             arblod<>fnlry00i1.oldlod or
121200140527             arbprd<>fnlry00i1.oldprd or
121300140527             arbnzd<>fnlry00i1.oldnzd ;
121400140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
121500140527               wIdMsg   = 'TIS0742';
121600140527               wIdCampo = '*CHGDATA  ';
121700140527               clear wParm;
121800140527               exsr Messaggi;
121900140527               exsr routend;
122000140527          endif;
122100140527         endsl;
122200140523       ENDSR;
122300140523       //--------------------------------------------------------------
122400140523       //?Controllo dettaglio
122500140523       //--------------------------------------------------------------
122600140523       BEGSR  ControllaDett;
122700140523
122800140617       //?Appuntamento
122900140617       // (se non caricate date da proporre per GetIstruCo il bottone non deve
123000140617       //  essere attivato)
123100140609       if (prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO and fnlry00o0.nrdate>0)
123200140624                                                     or fnlry00i1.tipodispo='1';
123300140523          exsr sr_appuntam;
123400140523       endif;
123500140617       //?Fermo Deposito
123600140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='2';
123700140523          exsr sr_FD;
123800140523       endif;
123900140617       //?Altro Indirizzo
124000140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='3';
124100140523          exsr sr_AltroInd;
124200140523       endif;
124300140617       //?Altre Istruzioni
124400140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='4';
124500140523          exsr sr_AltreIstruz;
124600140523       endif;
124700140617       //?Per GETISTRUCO -->  se bottoni tutti in OFF metto in OFF anche bottone
124800140617       //?per le istruzioni di consegna
124900140616         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
125000140616            and fnlry00o0.BUTAPPUNT='0'
125100140616            and fnlry00o0.BUTFERDEP='0'
125200140616            and fnlry00o0.BUTindiriz='0'
125300140616            and fnlry00o0.BUTaltro='0';
125400140616            fnlry00o0.BUTISTRCON='0';
125500140617             wIdMsg   = 'TIS0770';
125600140617             wIdCampo =  '*BOLLA    ';
125700140617             wParm = wbolla;
125800140617             exsr Messaggi;
125900140617             exsr routend;
126000140616         endif;
126100140924       //?Per GETISTRUCO -->  se in ON  bottone per appuntam o altro indirizzo
126200140924       //?valorizzo campo messaggio per la preferenza di orario
126300140924         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
126400140924            and (fnlry00o0.BUTAPPUNT='1'  or fnlry00o0.BUTindiriz='1');
126500140930               wtempoh=%div((§VPORST_MW + §VPORST_PW):60);
126600140930               wtempom=%rem((§VPORST_MW + §VPORST_PW):60);
126700140924               wIdMsg   = 'TIS0781';
126800140929               wparm=      %editc(wtempoh:'Z');
126900140924               if wtempom>0;
127000140929                  wIdMsg   = 'TIS0784';
127100140929                  wparm=      wparm + %editc(wtempom:'Z') ;
127200140924               endif;
127300140924               fnlry00o0.MSGPRFORA=rtvMsgLang(wIdMsg:widlingua:wparm);
127400140924         endif;
127500140523
127600140523       endsr;
127700140408
127800140521       //--------------------------------------------------------------
127900140521       //?Chain FNARB
128000140521       //--------------------------------------------------------------
128100140521       BEGSR  sr_chainbolla;
128200140527
128201161128          clear w_tbo ;
128202161129          clear fnarbds;
128203161129          exsr  sr_inzsoloblp;
128300140521       // imposto la chiave di accesso
128400140521          kaas=2000+%dec(%subst(wbolla:1:2):2:0);
128500140521          klnp=%dec(%subst(wbolla:3:3):3:0);
128600140521          knrs=%dec(%subst(wbolla:6:2):2:0);
128700140521          knsp=%dec(%subst(wbolla:8:7):7:0);
128800140521          chain (kaas:klnp:knrs:knsp) fnarb01l;
128900140521          if not %found(fnarb01l);
128901161128       //  NON TROVATA LA BOLLA IN ARRIVO -> la cerco su blp
128902161128           chain (kaas:klnp:knrs:knsp) fnblp01l;
128903161128       //    ERRORE SE:
128904161128       //    Non trovata neanche su blp oppure
128905161128       //    trovata su blp ma flaggata oppure
128906161129       //    filiale non ancora partita con la nuova procedura
128907161128             if not %found(fnblp01l) or
128908161128                  arbft1<>*blanks  or
128909161129                  (%lookup(%editc(arblna:'X'):skFilOk)=0  and
128910161128                   %lookup('999':skFilOk)=0 );
129000140521              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
129100140527              wIdMsg   = 'TIS0735';
129200140527              wIdCampo = wcampo      ;
129300140530              wParm=wbolla;
129400140527              exsr Messaggi;
129500140521              exsr RoutEnd;
129501161128             else;
129502161128             eval w_tbo='P';
129503161128             endif    ;
129504161128          else;
129505161128             eval w_tbo='A';
129600140521          endif;
129700141211       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
129800141211         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
129900141211           fnlry00i1.oldCAD    = arbCAD    ;
130000141211           fnlry00i1.oldLOD    = arbLOD    ;
130100141211           fnlry00i1.oldPRD    = arbPRD    ;
130200141211           fnlry00i1.oldNZD    = arbNzd    ;
130300141211         endif;
130400140521       endsr;
130500140620       //--------------------------------------------------------------
130600140620       //?Operazioni per bolle legate
130700140620       //--------------------------------------------------------------
130800140620       BEGSR  sr_lbl          ;
130900140620          chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl02l;
131000140620          if %found(fnlbl02l);
131100140624       // Per CHK_ISTRUCO --> errore se la bolla da elaborare ha una figlia
131200141223       //    IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
131300141223       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
131400141223       //             wIdMsg   = 'TIS0777';
131500141223       //             wIdCampo = '*NODISPO  ';
131600141223       //             wParm = wbolla;
131700141223       //             exsr Messaggi;
131800141223       //             exsr RoutEnd;
131900141223       //    endif;
132000140620       //    BOLLA CONSEGNATA
132100140620             if arbdcm>0;
132200140620       //       E' LA BOLLA ORIGINALE
132300140620                if LBLAAO=arbaas and LBLLPO=arblnp and
132400140620                   LBLNRO=arbnrs and LBLNSO=arbnsp;
132500140620       //       Non è il Mittente --> Errore: utente non abilitato a dare disposizioni
132600140620                   if wchi <>'M';
132700140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
132800140620                      wIdMsg   = 'TIS0736';
132900140620                      wIdCampo = '*NOAUT    ';
133000140620                      wParm = wbolla;
133100140620                      exsr Messaggi;
133200140620                      exsr RoutEnd;
133300140620                   endif;
133400140620                else;
133500140620       //       NON E' LA BOLLA ORIGINALE
133600140620       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
133700140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
133800140627                      wIdMsg   = 'TIS0770';
133900140620                      wIdCampo = '*NODISPO  ';
134000140620                      wParm = wbolla;
134100140620                      exsr Messaggi;
134200140620                      exsr RoutEnd;
134300140620                endif;
134400140620             endif;
134500140620       //    Recupero i dati dell'ultima figlia
134600140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
134700140620             dow %found(fnlbl02l);
134800140620                chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
134900140620             enddo;
134901161129             clear fnarbds;
134902161129             exsr  sr_inzsoloblp;
135000140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnarb01l;
135100140620             if not %found(fnarb01l);
135200140627       //    Se non trovo l'ultima figlia su arb errore
135300140627       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
135400140627                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
135500140627                      wIdMsg   = 'TIS0777';
135600140627                      wIdCampo = '*NODISPO  ';
135700140627                      wParm = wbolla;
135800140627                      exsr Messaggi;
135900140627                      exsr RoutEnd;
135901161128             else;
135902161128                eval w_tbo='A';
136000140620             endif;
136100141223       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
136200141223         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
136300141223           fnlry00i1.oldCAD    = arbCAD    ;
136400141223           fnlry00i1.oldLOD    = arbLOD    ;
136500141223           fnlry00i1.oldPRD    = arbPRD    ;
136600141223           fnlry00i1.oldNZD    = arbNzd    ;
136700141223         endif;
136800140620          endif;
136900140620       endsr;
137000140523       //--------------------------------------------------------------
137100140523       //?Chain Estensioni bolla
137200140523       //--------------------------------------------------------------
137300140523       BEGSR  sr_chainEstBolla;
137400140523          clear dar5gen;
137500140523          clear wrefbolla;
137600140523          clear wtelbolla;
137700140620          chain (arbaas:arblnp:arbnrs:arbnsp:'GEN') fiar501l ;
137800140523          if %found(fiar501l);
137900140523             dar5gen=ar5uni;
138000140523       // Se presente # nel referente e nel telefono
138100140523       // lo devo togliere
138200140523             if %subst(§ar5ref:1:1)='#';
138300140523                eval wrefbolla=%subst(§ar5ref:2);
138400140523             else;
138500140523                eval wrefbolla=§ar5ref;
138600140523             endif;
138700140523             if %subst(§ar5teld:1:1)='#';
138800140523                eval wtelbolla=%subst(§ar5teld:2);
138900140523             else;
139000140523                eval wtelbolla=§ar5teld;
139100140523             endif;
139200140523          endif;
139300140523          clear dar5emd;
139400140620          chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
139500140523          if %found(fiar501l);
139600140523              dar5emd=ar5uni;
139700140523         endif;
139800140523       ENDSR;
139900140522       //--------------------------------------------------------------
140000140522       //?Recupera dati bolla da passare a chiamante
140100140522       //--------------------------------------------------------------
140200140522       BEGSR  sr_GetDatiBolla;
140300140603       // - orari da passare nella tendina: standard o minimo/massimo in base
140400140603       //   a quanto memorizzato in tabella
140500140923          clear waltroind;
140600140522          exsr sr_orarisr;
140700140604          if §ivptipo='M';
140800140604             fnlry00o0.ORARITNDI=%editc(OOR2MIIS:'X');
140900140604             fnlry00o0.ORARITNDF=%editc(OOR2MXFS:'X');
141000140604          else;
141100140604             fnlry00o0.ORARITNDI=%editc(OOR2STIS:'X');
141200140604             fnlry00o0.ORARITNDF=%editc(OOR2STFS:'X');
141300140604          endif;
141400140923       // - per la tendina con orari di filiale -->
141500140923         clear tisio9ds;
141600140923         do0lna=arblna;
141700140923         do0tric='S';
141800140923         do0tser='C';
141900140923         tisio9r (kpjba:tisio9ds);
142000140923          if §ivptipo='M';
142100140923             fnlry00o0.ORARITNFI=%editc(DO0OMIIS:'X');
142200140923             fnlry00o0.ORARITNFF=%editc(DO0OMXFS:'X');
142300140923          else;
142400140923             fnlry00o0.ORARITNFI=%editc(DO0OSTIS:'X');
142500140923             fnlry00o0.ORARITNFF=%editc(DO0OSTFS:'X');
142600140923          endif;
142700140603          fnlry00o0.MMINTERVC=%editc(§ivpmini:'X');
142800140613       // DATE DI CONSEGNA DA PROPORRE  -------------------
142900140605       // a partire da oggi vengono proposte le date fino al giorno limite
143000140605       // previsto per apertura giacenza
143100140605       // 1) verifico se previsti giorni personalizzati per cliente
143200140605         exsr sr_PersGGiac;
143300140709       // 2) Verifico se effetuato un tentativo di consegna ai fini dell'abilitazione
143400140709       //    della riconsegna
143401161202       //    e anche per decidere da che data iniziare a proporre le dcr
143501161202         if w_tbo='A' and (arbntc=0 or (§IVPARIC='S'AND OOR2FRIC='S'));
143600140709            exsr sr_RepPct   ;
143700140709         ENDIF;
143800140709       // 3) Ciclo da oggi per numero di giorni per caricare campo delle date
143900140605       //    consegna richiesta da proporre
143901161202       if w_tbo='A' and (arbntc>0 or wtentacons='1');
144000140605         wdata=dataoggi;
144001161129       else;
144002161129       // Se bolla ancora da partire non parto da Oggi ma dalla Data Prevista Consegna
144003161202       // Idem se bolla in arrivo ma non ancora fatti tentativi di consegna
144004161129         wdata=dataPrvC;
144005161129       endif;
144100140605         clear ix;
144200140923         clear iy;
144300140605         for I=1 to wlimitdcr;
144400140606       //   se cade in un giorno festivo incremento la data di un giorno
144500140606            exsr sr_datafes;
144600161202       //   Propongo la data di oggi solo se:
144601161202       //   non fatto nemmeno un tentativo di consegna e la bolla non è in distinta
144602161202       //   già fatto un tentativo di consegna ed è possibile la riconsegna in giornata
144603161202            if wdata<>dataoggi or (arbntc=0 and wtentacons=' ' and arbndc=0) or
144604161202                             (oor2fric='S' and §ivparic='S' and wtentacons='1');
144700161202       //   if (wdata=dataoggi and oor2fric='S'
144800161202       //       and §ivparic='S' and wtentacons='1') or wdata<>dataoggi;
144900140609       //   Memorizzo la data, purchè, in presenza di data cons.rich "Dopo il",
145000140609       //   la stessa non sia <= della data "Dopo Il"
145100140710             if arbtcr<>'D' or (arbtcr='D' and wdata>arbdcr);
145200140606               ix+=1;
145300140606               fnlry00o0.schdcr(ix)=%char(%date(wdata:*iso):*eur);
145400140923       //      la data per l'indirizzo alternativo non deve mai essere "oggi"
145401161202       //      ma può andare bene se la spedizione non è ancora in distinta...
145500161202               if wdata<>dataoggi or (arbntc=0 and wtentacons=' ' and arbndc=0);
145600140923                  iy+=1;
145700140923                  fnlry00o0.schdcri(iy)=%char(%date(wdata:*iso):*eur);
145800140923                  fnlry00o0.nrdatei+=1;
145900140923               endif;
146000140606       //      Data da Consigliare --> quella successiva a oggi oppure
146100140606       //      quella della bolla se maggiore
146200161202               if fnlry00o0.dtconsric=*blanks  and (wdata<>dataoggi
146201161202                 or (arbntc=0 and wtentacons=' ' and arbndc=0));
146300140606                   fnlry00o0.dtconsric=%char(%date(wdata:*iso):*eur);
146400140606                   if arbdcr>0 and arbtcr=*blanks and arbdcr>wdata;
146500140606                      fnlry00o0.dtconsric=%char(%date(arbdcr:*iso):*eur);
146600140606                   endif;
146700140606               endif;
146800140606       //   Incremento contatore date da passare a chiamante
146900140606               fnlry00o0.nrdate+=1;
147000140609             endif;
147100140606            endif;
147200140606       //   Incremento la data di un giorno
147300140606            exsr sr_IncreData;
147400140605         endfor;
147500140522       // - consegna richiesta: data/ora/tipo
147600140606       //if arbdcr>0 ;
147700140606       //   fnlry00o0.DTCONSRIC=wdcreur;
147800140606       //endif;
147900140606       //if arbhcr>0;
148000140606       //   fnlry00o0.ORCONSRIC=%editc(arbhcr:'X');
148100140606       //endif;
148200140606       //fnlry00o0.TPCONSRIC=arbtcr;
148300140613       // indirizzo e-mail e n. telefono per SMS -----------
148400140620         chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
148500140522         if %found(fiar501l);
148600140522             dar5emd=ar5uni;
148700140522             fnlry00o0.destmail=§ar5eml;
148800140522             fnlry00o0.destcell=§ar5tel;
148900140522         endif;
149000140613       // Messaggio di avviso per indicare che il FD deve essere autorizzato dal mitt. ------------
149100140522         if arbffd=*blanks and arbgma<>*blanks;
149200140529            exsr sr_tab7r;
149300140522            if §7rfd='S' and wchi='D';
149400140522               wIdMsg   = 'TIS0733';
149500140617               fnlry00o0.MSGFDMITT=rtvMsgLang(wIdMsg:widlingua);
149600140522            endif;
149700140522         endif;
149800140522       endsr;
149900140606       //--------------------------------------------------------------
150000140606       //?Controllo data se festiva + restituzione data prima data non festiva
150100140606       //--------------------------------------------------------------
150200140606       BEGSR  sr_DataFes;
150300140606       wfes='1';
150400140606       dow wfes='1';
150500140606       chain (0:wfgslna:%subdt(%date(wdata):*years):
150600140606                        %subdt(%date(wdata):*MONTHS)) azcln01l;
150700140606          if %found(azcln01l) and mat(%subdt(%date(wdata):*days)) = 'F'
150800140606                               or pom(%subdt(%date(wdata):*days)) = 'F';
150900140606       // data festiva: la incremento di un giorno
151000140606             exsr sr_incredata;
151100140606          else;
151200140606            clear wfes;
151300140606          endif;
151400140606       enddo;
151500140606       endsr;
151600140606       //--------------------------------------------------------------
151700140606       //?Incremento data di un giorno
151800140606       //--------------------------------------------------------------
151900140606       BEGSR  sr_incredata;
152000140606             dataiso=%date(wdata);
152100140606             dataiso=dataiso+%days(1);
152200140606             wdata=%dec(dataiso);
152300140606       endsr;
152400140521       //--------------------------------------------------------------
152500140521       //?Controlli per abilitare la richiesta di appuntamento
152600140521       //--------------------------------------------------------------
152700140521       BEGSR  sr_appuntam;
152800140528
152900140528       // Non ammesso per spedizioni in fermo deposito
153000140528         if arbffd= 'S';
153100140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
153200140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
153300140528               leavesr;
153400140528            else;
153500140528       //?PUT_ISTRUCO --> uscita da pgm con errore
153600140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
153700141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
153800140528              wIdMsg   = 'TIS0743';
153900140528              wIdCampo = '*NOAPPUNT ';
154000140530              wParm = wbolla ;
154100141006         else;
154200141006              wIdMsg   = 'TISA743';
154300141006              wIdCampo = '*NOAPPUNT ';
154400141006              clear wParm  ;
154500141006         endif;
154600140528              exsr Messaggi;
154700140528              exsr routend;
154800140528            endif;
154900140528         endif;
155000140528
155100140528       //?GET_ISTRUCO --> attivazione bottone per appuntamento
155200140528         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
155300140528            fnlry00o0.BUTAPPUNT='1';
155400140528         else;
155500140528       //?PUT_ISTRUCO --> controllo dati ricevuti
155600140528            exsr sr_ctrApp;
155700140528         endif;
155800140528
155900140521       endsr;
156000140522       //--------------------------------------------------------------
156100140522       //?Controlli per abilitare la richiesta di Fermo Deposito
156200140522       //--------------------------------------------------------------
156300140522       BEGSR  sr_FD      ;
156400140522
156500140528       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
156600140528          if arbffd = 'S';
156700140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
156800140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
156900140528               leavesr;
157000140528            else;
157100140528       //?PUT_ISTRUCO --> uscita da pgm con errore
157200141223            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
157300140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
157400141223       //if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
157500140528              wIdMsg   = 'TIS0743';
157600140528              wIdCampo = '*NOFD     ';
157700140530              wParm = wbolla;
157800141223       //else;
157900141223       //     wIdMsg   = 'TISA743';
158000141223       //     wIdCampo = '*NOFD     ';
158100141223       //     clear wParm;
158200141223       //endif;
158300140528              exsr Messaggi;
158400140528              exsr routend;
158500141223            endif;
158600140528            endif;
158700140528          else;
158800140530       //?GET_ISTRUCO -->
158900141008            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
159000141008       //   and fnlry00o0.MSGFDMITT=*blanks;
159100140616       //?GET_ISTRUCO -->> Restituisco, insieme al bottone attivato anche messaggio
159200140616       //?                 riguardante le codizioni di ritiro in FD
159300140616       //?                 Solo se msgfdmit=*blanks
159400140616             if fnlry00o0.msgfdmitt=*blanks;
159500141008                fnlry00o0.BUTFERDEP='1';
159600140924                exsr sr_wdesgio;
159700140617               wparm=wdesgio + %editc(WlimitFD:'X') + wfgslnad;
159800140616               wIdMsg   = 'TIS0768';
159900140617               fnlry00o0.MSGFDCOND=rtvMsgLang(wIdMsg:widlingua:wparm);
160000141008             else;
160100141008                fnlry00o0.BUTFERDEP='2';
160200140616             endif;
160300140616            endif;
160400140606       //?PUT_ISTRUCO --> Controllo dati ricevuti
160500140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
160600140624               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
160700140616       //?                Errore al destinatario se è richiesta autorizzaz.
160800140616       //?                da parte del mittente
160900140616             if arbgma<>*blanks;
161000140616                exsr sr_tab7r;
161100140616                if §7rfd='S' and wchi='D';
161200140616                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
161300140616                   wIdMsg   = 'TIS0733';
161400140616                   wIdCampo = '*NOFD     ';
161500140616                   clear wParm;
161600140616                   exsr Messaggi;
161700140616                   exsr routend;
161800140616                endif;
161900140616             endif;
162000140606             exsr sr_ctrUNI;
162100140606            endif;
162200140530          endif;
162300140522       endsr;
162400140522       //--------------------------------------------------------------
162500140522       //?Controlli per abilitare la richiesta di Altro Indirizzo
162600140522       //--------------------------------------------------------------
162700140522       BEGSR  sr_altroind;
162800140522
162900140616       if §ivpdind='S';
163000140617       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
163100140617          if arbffd = 'S';
163200140617       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
163300140617            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
163400140617               leavesr;
163500140617            else;
163600140617       //?PUT_ISTRUCO --> uscita da pgm con errore
163700140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
163800141007           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
163900140617              wIdMsg   = 'TIS0743';
164000140617              wIdCampo = '*NOALTRIND';
164100140617              wParm = wbolla;
164200141007           else;
164300141007              wIdMsg   = 'TISA743';
164400141007              wIdCampo = '*NOALTRIND';
164500141007              clear wparm ;
164600141007           endif;
164700140617              exsr Messaggi;
164800140617              exsr routend;
164900140617            endif;
165000140617          endif;
165100140619       //NON AMMESSO AL DESTINATARIO SE NON ABILITATO MEDIANTE TABELLA 7W
165200140619         clear ds7W;
165300140619         if wchi='D' and arbgga<>*blanks;
165400140619            w008a=ARBGGA;
165500140619            chain (1 : '7W' : W008A) tabel00f;
165600140619            if %found(tabel00f);
165700140619               ds7w=tbluni;
165800140619            endif;
165900140619         endif;
166000140619         if wchi='D' and §7wdes<>'S';
166100140619       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
166200140918       //?                18/09/2014 --> attivo il bottone ma poi verrà dato errore
166300140918       //?                Il concetto è: il cambio di indirizzo è gestito (e quindi il
166400140918       //?                bottone deve essere attivo) ma non può essere richiesto da
166500140918       //?                destinatario se non autorizzato (e quindi poi viene dato errore)
166600140918            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
166700141007               fnlry00o0.BUTindiriz='2';
166800141007               wIdMsg   = 'TIS0796';
166900141007               fnlry00o0.MSGAUTIND=rtvMsgLang(wIdMsg:widlingua);
167000141007               leavesr;
167100140619            else;
167200140619       //?PUT_ISTRUCO --> uscita da pgm con errore
167300140619              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
167400140619              wIdMsg   = 'TIS0776';
167500140619              wIdCampo = '*NOALTRIND';
167600140924              clear wparm;
167700140924       //     wParm = wbolla;
167800140619              exsr Messaggi;
167900140619              exsr routend;
168000140619            endif;
168100140619         endif;
168200140522         fnlry00o0.BUTindiriz='1';
168300140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
168400140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
168500140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
168600140611       //?Controllo dati comuni a tutti tipi di disposizione
168700140611             exsr sr_ctrUNI;
168800140611
168900140609             exsr sr_ctrind;
169000140609            endif;
169100140616       else;
169200140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
169300140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
169400141016               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
169500141016                 prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
169600141016                 prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
169700141016                 exsr RoutEnd;
169800140616            endif;
169900140616       endif;
170000140522       endsr;
170100140522       //--------------------------------------------------------------
170200140522       //?Controlli per abilitare la richiesta di Altre Istruzioni
170300140522       //--------------------------------------------------------------
170400140522       BEGSR  sr_AltreIstruz;
170500140522
170600140616       if §ivpdalr='S';
170700140522         fnlry00o0.BUTaltro='1';
170800140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
170900140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
171000140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
171100140609             exsr sr_ctrUNI;
171200140609            endif;
171300140616       else;
171400140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
171500140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
171600140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
171700140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
171800140617              prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
171900140617              exsr RoutEnd;
172000140616            endif;
172100140616       endif;
172200140522       endsr;
172300140522
172400140523       //--------------------------------------------------------------
172500140523       //?Controllo Disposizioni di Appuntamento
172600140523       //--------------------------------------------------------------
172700140523       BEGSR  sr_ctrApp     ;
172800140611       //?Controllo dati comuni a tutti tipi di disposizione
172900140611          exsr sr_ctrUNI;
173000140528       //?Consegna richiesta
173100140528          exsr sr_ctrConsRich;
173200140523       endsr;
173300140528       //--------------------------------------------------------------
173400140528       //?Controllo Data/ora/tipo consegna richiesta
173500140528       //--------------------------------------------------------------
173600140528       BEGSR  sr_CtrConsRich;
173700140923          clear waltroind;
173800140606          exsr sr_Orarisr;
173900140528       // DATA CONSEGNA RICHIESTA:
174000140528          exsr sr_ctrDCR;
174100140528       // ORA  CONSEGNA RICHIESTA:
174200140528       if fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
174300140528       // Deve contenere solo numeri
174400140528         if %check(digits:fnlry00i1.neworcrich)>0;
174500140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174600140528              wIdMsg   = 'TIS0746';
174700140528              wIdCampo = 'NEWORCRICH';
174800140530              wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
174900140530                      %subst(fnlry00i1.neworcrich:3:2);
175000140528              exsr Messaggi;
175100140528         else;
175200140709       // Se data consegna richiesta diversa da oggi (no riconsegna)
175300140709       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
175400140528              wora = %dec(fnlry00i1.neworcrich:4:0);
175500141216              if wdcr<>wdataconfr;
175600140709                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
175700140709                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
175800140709                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
175900140709                       wIdMsg   = 'TIS0747';
176000140709                       wIdCampo = 'NEWORCRICH';
176100140709                       wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
176200140709                       %subst(fnlry00i1.neworcrich:3:2);
176300140709                       exsr Messaggi;
176400140709                 endif;
176500140709              else;
176600140709       //  Se Data Consegna Richiesta=oggi (riconsegna) l'orario deve essere incluso negli orari
176700140610       //  inizio/fine previsti per la riconsegna
176800140709                 if  wora < OOR2RIDS  or
176900140709                     wora > OOR2RIAS ;
177000140709                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
177100140709                        wIdMsg   = 'TIS0778';
177200140709                        wIdCampo = 'NEWORCRICH';
177300140709                        wParm = %editw(oor2rids:'  :  ') +
177400140709                                %editW(oor2rias:'  :  ')  ;
177500140709                        exsr Messaggi;
177600140709                 endif;
177700140709              endif;
177800140528         endif;
177900140528       endif;
178000140529       // PREFERENZA MATTINO/POMERIGGIO
178100140529       if fnlry00i1.prconsric<>*blanks and
178200140529          fnlry00i1.prconsric<>'M' and
178300140529          fnlry00i1.prconsric<>'P'               ;
178400140528                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
178500140528                    wIdMsg   = 'TIS0749';
178600140529                    wIdCampo = 'PRCONSRIC ';
178700140528                    clear wParm;
178800140528                    exsr Messaggi;
178900140528       endif;
179000140710       // NO RICONSEGNA:
179100140710       // Verifico preferenza m/p con gli orari standard o min/max in base
179200140710       // all'ora limite del mattino
179300141216       if wdcr<>wdataconfr;
179400140710         if (fnlry00i1.prconsric='P' and §ivptipo='S' and §ivporlm>=OOR2STFS) or
179500140710            (fnlry00i1.prconsric='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
179600140710            (fnlry00i1.prconsric='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS) or
179700140710            (fnlry00i1.prconsric='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
179800140710            If fnlry00i1.prconsric='P';
179900140710                widmsg='TIS0676';
180000140710            else;
180100140710                widmsg='TIS0667';
180200140710            endif;
180300140710            matpom=rtvMsgLang(widmsg : widlingua);
180400140710                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
180500141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
180600140710                      wIdMsg   = 'TIS0760';
180700140710                      wIdCampo = 'PRCONSRIC ';
180800140710                      wParm = MatPom + wbolla;
180900141006         else;
181000141006                      wIdMsg   = 'TISA760';
181100141006                      wIdCampo = 'PRCONSRIC ';
181200141006                      wParm = MatPom ;
181300141006         endif;
181400140710                      exsr Messaggi;
181500140710         ENDIF;
181600140710       ELSE;
181700140710       // SI RICONSEGNA:
181800140710       // Verifico preferenza m/p con gli orari previsti per la riconsegna
181900140710       // in base all'ora limite del mattino
182000140710          if (fnlry00i1.prconsric='P' and §ivporlm>=OOR2rias) or
182100140710             (fnlry00i1.prconsric='M' and §ivporlm<OOR2rids);
182200140710             If fnlry00i1.prconsric='P';
182300140710                 widmsg='TIS0676';
182400140710             else;
182500140710                 widmsg='TIS0667';
182600140710             endif;
182700140710             matpom=rtvMsgLang(widmsg : widlingua);
182800140710                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
182900140710                       wIdMsg   = 'TIS0779';
183000140710                       wIdCampo = 'PRCONSRIC ';
183100140710                       wParm = MatPom ;
183200140710                       exsr Messaggi;
183300140710          ENDIF;
183400140606       endif;
183500140529       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
183600140529       if fnlry00i1.prconsric<>*blanks and
183700140529          fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
183800140529                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
183900140530                    wIdMsg   = 'TIS0753';
184000140530                    wIdCampo = 'NEWORCRICH';
184100140530                    clear wParm;
184200140529                    exsr Messaggi;
184300140530                    wIdCampo = 'PRCONSRIC ';
184400140529                    exsr Messaggi;
184500140529       endif;
184600140528       //  Se Campi tutti vuoti errore
184700140529       //if (fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros) and
184800140529       //   (fnlry00i1.neworcrich=*blanks or fnlry00i1.neworcrich=*zeros) and
184900140529       //    fnlry00i1.prconsric=*blanks;
185000140529       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
185100140529       //             wIdMsg   = 'TIS0748';
185200140529       //             wIdCampo = '*NOSELEZ';
185300140529       //             clear wParm;
185400140529       //             exsr Messaggi;
185500140529       //endif;
185600140528       endsr;
185700140528       //--------------------------------------------------------------
185800140528       //?Controllo Data Consegna richiesta
185900140528       //--------------------------------------------------------------
186000140528       BEGSR  sr_ctrDCR     ;
186100140528       // DATA CONSEGNA RICHIESTA:
186200140529       // Obbligatoria
186300140529       if fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros;
186400140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
186500140529             wIdMsg   = 'TIS0130';
186600140529             wIdCampo = 'NEWDTCRICH';
186700140529             clear wParm;
186800140529             exsr Messaggi;
186900140529             leavesr;
187000140529       endif;
187100140528       //  Controllo correttezza formale
1872001405282        monitor  ;
187300140528         dataiso=%date(fnlry00i1.newdtcrich:  *eur)   ;
187400140528         on-error ;
187500140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
187600140528             wIdMsg   = 'TIS0121';
187700140528             wIdCampo = 'NEWDTCRICH';
187800140528             clear wParm;
187900140528             exsr Messaggi;
188000140528             leavesr;
188100140528 2       endmon  ;
188200140528
188300140528       wdcr=%dec(dataiso);
188400140528
188500140528       //  Non inferiore a oggi
188600140528       if wdcr < dataoggi;
188700140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
188800140528             wIdMsg   = 'TIS0744';
188900140528             wIdCampo = 'NEWDTCRICH';
189000140611             wParm=%char(%date(wdcr:*iso):*eur);
189100140528             exsr Messaggi;
189200140528             leavesr;
189300140528       endif;
189302161202       if w_tbo='A' and (arbntc=0 or (§IVPARIC='S'AND OOR2FRIC='S'));
189303161202        exsr sr_reppct;
189304161202       endif;
189305161130       //  Non inferiore a data prevista consegna se spedizione da partire o senza
189306161129       //  Tentativi di consegna
189307161202       if (w_tbo='P' or (arbntc=0 and wtentacons=' ')) and wdcr < dataPrvC;
189308161129             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
189309161129             wIdMsg   = 'TIS0xxx';
189310161129             wIdCampo = 'NEWDTCRICH';
189311161129             wParm=%char(%date(wdcr:*iso):*eur);
189312161129             exsr Messaggi;
189313161129             leavesr;
189314161129       endif;
189400140709       //  Uguale ad oggi solo se possibile la riconsegna in giornata
189500141215       //  Per chkistruco considero "data oggi" la data in cui è stata
189600141215       //  fatta la  richiesta
189700141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
189800141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
189900141215         else;
190000141218              wdataconfr=dataoggi;
190100141215         endif;
190200141215       if wdcr=wdataconfr;
190300161202       //if oor2fric='S' and §ivparic='S';
190400161202       // exsr sr_reppct;
190500161202       //endif;
190501161202       // Non devo dare questo errore se la bolla non è in distinta
190502161202        if arbndc>0 ;
190600140709         if oor2fric='N' or §ivparic<>'S' or wtentacons=' ';
190700140606             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
190800140606             wIdMsg   = 'TIS0759';
190900140606             wIdCampo = 'NEWDTCRICH';
191000140606             clear wparm;
191100140606             exsr Messaggi;
191200140606             leavesr;
191300140709         endif;
191301161202        endif;
191400140606       endif;
191500140528       //  Non superiore ai Giorni limite giac. con data cons.rich.
191600140528       //      Verifico se presenti limiti personalizzati per il cliente
191700140528
191800140605       exsr sr_PersGGiac;
191900140528       //Calcolo giorni lavorativi fra data odierna e data consegna richiesta
192000141215       datada=wdataconfr;
192001161129       // Se spedizione ancora da partire o non ancora fatti tentativi di cons.
192002161129       // il calcolo dei giorni lavorativi non è con la data odierna ma con la
192003161129       // data consegna richiesta
192004161202       if (w_tbo='P' or (arbntc=0 and wtentacons=' '))         ;
192005161129          datada=DataprvC;
192006161129       endif;
192100140528       dataa=wdcr;
192200140606       clear tfptfa;
192300140610       p_tfa =wfgslna;
192400140606       xsrlav8(wdat8:tfptfa);
192500140528       if ggl > wlimitdcr;
192600140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
192700140528             wIdMsg   = 'TIS0745';
192800140528             wIdCampo = 'NEWDTCRICH';
192900140611             wParm=%char(%date(wdcr:*iso):*eur);
193000140528             exsr Messaggi;
193100140528             leavesr;
193200140528       endif;
193300140606       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
193400140528       //  Controllo anche con la data consegna richiesta originale
193500140609       if (arbtcr='D' and wdcr<=arbdcr) or
193600140609         (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
193700140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
193800140529             wIdMsg   = 'TIS0750';
193900140528             wIdCampo = 'NEWDTCRICH';
194000140606             if arbtcr='D' ;
194100140606             dataeur=%date(%dec(arbdcr:8:0):*iso);
194200140606             else;
194300140606             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
194400140606             endif;
194500140611             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
194600140528             exsr Messaggi;
194700140528             leavesr;
194800140528       endif;
194900140605       // Non deve cadere in giorno festivo
195000140605       chain (0:wfgslna:%subdt(%date(wdcr):*years):
195100140605                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
195200140605       if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
195300140605                            or pom(%subdt(%date(wdcr):*days)) = 'F';
195400140605             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
195500140605             wIdMsg   = 'TIS0758';
195600140605             wIdCampo = 'NEWDTCRICH';
195700140611             wParm=%char(%date(wdcr:*iso):*eur);
195800140605             exsr Messaggi;
195900140605             leavesr;
196000140605       endif;
196100140528
196200140528       endsr;
196300140605       //--------------------------------------------------------------
196400140605       //?Rperimento eventuale personalizzazione giorni attesa prima di apert.giac
196500140605       //--------------------------------------------------------------
196600140605       BEGSR  sr_PersGGiac  ;
196700140606       // Faccio come fa fnlg80r --> prima prova on arbksc
196800140606       // e se non trova con questo prova poi con arbccm
196900140606       clear ds7u;
197000140606       w008a=%editc(arbksc:'X')+'Q';
197100140606       chain (1:'7U':w008a) tabel00f;
197200140606       if %found (tabel00f);
197300140606          ds7u=tbluni;
197400140606       else;
197500140606          w008a=%editc(arbCCM:'X')+'Q';
197600140606          chain (1:'7U':w008a) tabel00f;
197700140606          if %found (tabel00f);
197800140606             ds7u=tbluni;
197900140606          endif;
198000140606       endif;
198100140606       if §7uldcr>'00';
198200140606          wlimitdcr=%dec(§7uldcr:2:0);
198300140606       endif;
198400140605       // Cerco con il ccm se presente altrimenti con KSC
198500140606       // clear ds7u;
198600140606       //if arbccm > 0;
198700140606       //   w008a=%editc(arbCCM:'X')+'Q';
198800140606       //   chain (1:'7U':w008a) tabel00f;
198900140606       //   if %found (tabel00f);
199000140606       //      ds7u=tbluni;
199100140606       //   endif;
199200140606       //else;
199300140606       //   w008a=%editc(arbKSC:'X')+'Q';
199400140606       //   chain (1:'7U':w008a) tabel00f;
199500140606       //   if %found (tabel00f);
199600140606       //      ds7u=tbluni;
199700140606       //   endif;
199800140606       //endif;
199900140606       //if §7uldcr>'00';
200000140606       //   wlimitdcr=%dec(§7uldcr:2:0);
200100140606       //endif;
200200140605       endsr;
200300140609       //--------------------------------------------------------------
200400140609       //?Controlli Disposizioni di consegna ad altro indirizzo
200500140609       //--------------------------------------------------------------
200600140609       BEGSR  sr_ctrind;
200700141006          clear wolna;
200800140610       // Per il controllo dell'indirizzo (CAP escluso) non viene richiamato
200900140610       // fnlv14r (quest'ultimo, se cap vuoto richiama l'interrogazione
201000140609       // del cappario e controlla la nazione che qui non serve)
201100140609       //?Ragione sociale, Indirizzo, Località e Provincia --> obbligatori
201200140610          if fnlry00i1.newrsd = *blanks;
201300140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
201400140610             wIdMsg   = 'TIS0761';
201500140610             wIdCampo = 'NEWRSD    ';
201600140610             clear wParm;
201700140610             exsr Messaggi;
201800141006             leavesr;
201900140610          endif;
202000140610          if fnlry00i1.newind = *blanks;
202100140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
202200140610             wIdMsg   = 'TIS0762';
202300140610             wIdCampo = 'NEWIND    ';
202400140610             clear wParm;
202500140610             exsr Messaggi;
202600141006             leavesr;
202700140610          endif;
202800140610          if fnlry00i1.newcad = *blanks;
202900140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
203000140610             wIdMsg   = 'TIS0763';
203100140610             wIdCampo = 'NEWCAD    ';
203200140610             clear wParm;
203300140610             exsr Messaggi;
203400141006             leavesr;
203500140610          endif;
203600140610          if fnlry00i1.newlod = *blanks;
203700140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
203800140610             wIdMsg   = 'TIS0764';
203900140610             wIdCampo = 'NEWLOD    ';
204000140610             clear wParm;
204100140610             exsr Messaggi;
204200141006             leavesr;
204300140610          endif;
204400140610          if fnlry00i1.newprd = *blanks;
204500140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
204600140610             wIdMsg   = 'TIS0765';
204700140610             wIdCampo = 'NEWPRD    ';
204800140610             clear wParm;
204900140610             exsr Messaggi;
205000141006             leavesr;
205100140609          endif;
205200140610       //?La nazione deve essere vuota
205300140610          if fnlry00i1.newnzd <> *blanks;
205400140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
205500140610             wIdMsg   = 'TIS0384';
205600140610             wIdCampo = 'NEWNZD    ';
205700140610             clear wParm;
205800140610             exsr Messaggi;
205900141006             leavesr;
206000140610          endif;
206100140611       //?Richiamo pgm per controllo CAP e calcolo instradamento
206200140610          if fnlry00i1.newcad<>fnlry00i1.oldcad or
206300140610             fnlry00i1.newlod<>fnlry00i1.oldlod or
206400140610             fnlry00i1.newprd<>fnlry00i1.oldprd or
206500140610             fnlry00i1.newnzd<>fnlry00i1.oldnzd         ;
206600140611             exsr sr_call_fispe02r;
206700141003          else;
206800141003             if fnlry00i1.newrsd=fnlry00i1.oldrsd and
206900141003                fnlry00i1.newind=fnlry00i1.oldind;
207000141003       // Errore se non eseguita alcuna variazione di indirizzo
207100141003                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
207200141003                wIdMsg   = 'TIS0794';
207300141003                wIdCampo = '*NOCHGIND ';
207400141003                clear wParm;
207500141003                exsr Messaggi;
207600141006                leavesr;
207700141003             endif;
207800140610       endif;
207900140930
208000140930         waltroind='1';
208100140930         exsr sr_orarisr ;
208200140930
208300141016       //?SE NO DIROTTAMENTO
208400141016       //if wolna = 0 ;
208500140923       //?DATA/ORA CONSEGNA RICHIESTA
208600140923       if fnlry00i1.newdtcrici<>*blanks and fnlry00i1.newdtcrici<>*zeros;
208700141006       // Se richiesto dirottamento non si può inserire la consegna richiesta
208800141016         if  wolna>0;
208900141016                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
209000141016                wIdMsg   = 'TIS0795';
209100141016                wIdCampo = 'NEWDTCRICI';
209200141016                clear wParm;
209300141016                exsr Messaggi;
209400141016                leavesr;
209500141016         endif;
209600140923       // DATA CONSEGNA RICHIESTA:
209700140923       //  Controllo correttezza formale
209800140923         monitor  ;
209900140923         dataiso=%date(fnlry00i1.newdtcrici:  *eur)   ;
210000140923         on-error ;
210100140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
210200140923             wIdMsg   = 'TIS0121';
210300140923             wIdCampo = 'NEWDTCRICI';
210400140923             clear wParm;
210500140923             exsr Messaggi;
210600140923             leavesr;
210700140923         endmon  ;
210800140923
210900140923         wdcr=%dec(dataiso);
211000141215       //  Per chkistruco considero "data oggi" la data in cui è stata
211100141215       //  fatta la  richiesta
211200141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
211300141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
211400141215         else;
211500141218              wdataconfr=dataoggi;
211600141215         endif;
211700140923
211800140923       //  Non inferiore o uguale a oggi
211900161202       //if wdcr <= wdataconfr;
211901161202         if wdcr <  wdataconfr or (wdcr=wdataconfr and arbndc>0);
212000140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
212100140929             wIdMsg   = 'TIS0759';
212200140923             wIdCampo = 'NEWDTCRICI';
212300140923             clear wparm;
212400140923             exsr Messaggi;
212500140923             leavesr;
212600140923         endif;
212601161130       //  Non inferiore a data prevista consegna se spedizione da partire o senza
212602161129       //  Tentativi di consegna
212603161202       if w_tbo='A' and arbntc=0;
212604161202          exsr sr_RepPct;
212605161202       endif;
212606161202       if (w_tbo='P' or (arbntc=0 and wtentacons=' ')) and wdcr < dataPrvC;
212607161129             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
212608161129             wIdMsg   = 'TIS0xxx';
212609161129             wIdCampo = 'NEWDTCRICI';
212610161129             wParm=%char(%date(wdcr:*iso):*eur);
212611161129             exsr Messaggi;
212612161129             leavesr;
212613161129       endif;
212700140923       //  Non superiore ai Giorni limite giac. con data cons.rich.
212800140923       //      Verifico se presenti limiti personalizzati per il cliente
212900140923
213000140923         exsr sr_PersGGiac;
213100140923       //Calcolo giorni lavorativi fra data odierna e data consegna richiesta
213200141215         datada=wdataconfr;
213201161129       // Se spedizione ancora da partire o non ancora fatti tentativi di cons.
213202161129       // il calcolo dei giorni lavorativi non è con la data odierna ma con la
213203161202       // data prevista consegna
213204161202       if (w_tbo='P' or (arbntc=0 and wtentacons=' '))         ;
213205161129          datada=DataprvC;
213206161129       endif;
213300140923         dataa=wdcr;
213400140923         clear tfptfa;
213500140923         p_tfa =wfgslna;
213600140923         xsrlav8(wdat8:tfptfa);
213700140923         if ggl > wlimitdcr;
213800140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
213900140923             wIdMsg   = 'TIS0745';
214000140923             wIdCampo = 'NEWDTCRICI';
214100140923             wParm=%char(%date(wdcr:*iso):*eur);
214200140923             exsr Messaggi;
214300140923             leavesr;
214400140923         endif;
214500140923       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
214600140923       //  Controllo anche con la data consegna richiesta originale
214700140923         if (arbtcr='D' and wdcr<=arbdcr) or
214800140923           (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
214900140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
215000140923             wIdMsg   = 'TIS0750';
215100140923             wIdCampo = 'NEWDTCRICI';
215200140923             if arbtcr='D' ;
215300140923             dataeur=%date(%dec(arbdcr:8:0):*iso);
215400140923             else;
215500140923             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
215600140923             endif;
215700140923             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
215800140923             exsr Messaggi;
215900140923             leavesr;
216000140923         endif;
216100140923       // Non deve cadere in giorno festivo
216200140923         chain (0:wfgslna:%subdt(%date(wdcr):*years):
216300140923                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
216400140923         if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
216500140923                            or pom(%subdt(%date(wdcr):*days)) = 'F';
216600140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
216700140923             wIdMsg   = 'TIS0758';
216800140923             wIdCampo = 'NEWDTCRICI';
216900140923             wParm=%char(%date(wdcr:*iso):*eur);
217000140923             exsr Messaggi;
217100140923             leavesr;
217200140923         endif;
217300140923       // ORA CONSEGNA RICHIESTA:
217400140923         if fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
217500140923       // Deve contenere solo numeri
217600140923           if %check(digits:fnlry00i1.neworcrici)>0;
217700140923                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
217800140923                wIdMsg   = 'TIS0746';
217900140923                wIdCampo = 'NEWORCRICH';
218000140923                wParm = %subst(fnlry00i1.neworcrici:1:2) + ':' +
218100140923                        %subst(fnlry00i1.neworcrici:3:2);
218200140923                exsr Messaggi;
218300140923           else;
218400140923       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
218500140923                wora = %dec(fnlry00i1.neworcrici:4:0);
218600140923                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
218700140923                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
218800140923                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
218900140923                       wIdMsg   = 'TIS0780';
219000140923                       wIdCampo = 'NEWORCRICI';
219100140925       //              wParm = fnlry00i1.newlod;
219200140923                       if §ivptipo='S';
219300140925                          wParm = fnlry00i1.newlod +
219400140925                                  %editw(oor2stis:'  :  ') +
219500140925                                  %editw(oor2stfs:'  :  ') ;
219600140923                       else;
219700140925                          wParm = fnlry00i1.newlod +
219800140925                                  %editw(oor2miis:'  :  ') +
219900140925                                 %editw(oor2mxfs:'  :  ') ;
220000140923                       endif;
220100140923                       exsr Messaggi;
220200140923                 endif;
220300140923           endif;
220400140923         endif;
220500140923       // PREFERENZA MATTINO/POMERIGGIO
220600140923         if fnlry00i1.prconsrici<>*blanks and
220700140923            fnlry00i1.prconsrici<>'M' and
220800140923            fnlry00i1.prconsrici<>'P'               ;
220900140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
221000140923                    wIdMsg   = 'TIS0749';
221100140923                    wIdCampo = 'PRCONSRICI';
221200140923                    clear wParm;
221300140923                    exsr Messaggi;
221400140923         endif;
221500140923       // Verifico preferenza m/p con gli orari standard o min/max in base
221600140923       // all'ora limite del mattino
221700140923         if (fnlry00i1.prconsrici='P' and §ivptipo='S' and §ivporlm>=OOR2STFS)or
221800140923            (fnlry00i1.prconsrici='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
221900140923            (fnlry00i1.prconsrici='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS)or
222000140923            (fnlry00i1.prconsrici='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
222100140923            If fnlry00i1.prconsricI='P';
222200140923                widmsg='TIS0676';
222300140923            else;
222400140923                widmsg='TIS0667';
222500140923            endif;
222600140923            matpom=rtvMsgLang(widmsg : widlingua);
222700140923                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
222800141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
222900140923                      wIdMsg   = 'TIS0760';
223000140923                      wIdCampo = 'PRCONSRICI';
223100140923                      wParm = MatPom + wbolla;
223200141006         else;
223300141006                      wIdMsg   = 'TISA760';
223400141006                      wIdCampo = 'PRCONSRICI';
223500141006                      wParm = MatPom ;
223600141006         endif;
223700140923                      exsr Messaggi;
223800140923         ENDIF;
223900140923       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
224000140923         if fnlry00i1.prconsrici<>*blanks and
224100140923            fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
224200140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
224300140923                    wIdMsg   = 'TIS0753';
224400140923                    wIdCampo = 'NEWORCRICI';
224500140923                    clear wParm;
224600140923                    exsr Messaggi;
224700140923                    wIdCampo = 'PRCONSRICI';
224800140923                    exsr Messaggi;
224900140923         endif;
225000141001         else;
225100141001       // se non immessa la data consegna richiesta errore se immessa l'ora o la preferenza M/P
225200141001           if fnlry00i1.prconsrici<>*blanks ;
225300141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
225400141016              if  wolna>0;
225500141016                        wIdMsg   = 'TIS0795';
225600141016              else;
225700141001                        wIdMsg   = 'TIS0782';
225800141016              endif;
225900141001                        wIdCampo = 'PRCONSRICI';
226000141001                        clear wParm;
226100141001                        exsr Messaggi;
226200141001           endif;
226300141001           if  fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros ;
226400141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
226500141016              if  wolna>0;
226600141016                        wIdMsg   = 'TIS0795';
226700141016              else;
226800141001                        wIdMsg   = 'TIS0782';
226900141016              endif;
227000141001                        wIdCampo = 'NEWORCRICI';
227100141001                        clear wParm;
227200141001                        exsr Messaggi;
227300141001           endif;
227400141001         endif;
227500141016       //else;
227600141016       //?SE DIROTTAMENTO pulisco data/ora preferenza
227700141016       //   clear fnlry00i1.newdtcrici ;
227800141016       //   clear fnlry00i1.neworcrici ;
227900141016       //   clear fnlry00i1.prconsrici ;
228000141016       //endif;
228100140609       endsr;
228200140611       //--------------------------------------------------------------
228300140611       //?Richiamo stored procedure per controllo e calcolo instradamento
228400140611       //--------------------------------------------------------------
228500140611       BEGSR sr_Call_FISPE02R;
228600141006
228700141006
228800140611         clear  oTFA;
228900140611         clear  oLNA;
229000140611         clear  oZNC;
229100140611         clear  oLNPD;
229200140611         clear  oLNAD;
229300140611         clear  oERR;
229400140611         clear  oMSG;
229500140611         clear  oTAG;
229600140611         clear  RTNesito;
229700140611         clear  RTNopcode;
229800140611         clear  RTNstatus;
229900140611         iTYPE='C';
230000140611         iLANG=Widlingua;
230100140611         iDATA=dataoggi;
230200140611         clear iNTW ;
230300140611         iLNP=arblnp;
230400140611         iNZD=fnlry00i1.newnzd;
230500140611         iPRD=fnlry00i1.newprd;
230600140611         iCAD=fnlry00i1.newcad;
230700140611         iLOD=fnlry00i1.newlod;
230800140611         iIND=fnlry00i1.newind;
230900140611         iRSD=fnlry00i1.newrsd;
231000140611         iNCL=arbncl;
231100140611         iPKB=arbpkb;
231200140611         iVLB=arbvlb;
231300140611         iFFD=arbffd;
231400140611         iTSP=arbtsp;
231500140611         iTC1=arbtc1;
231600140611         iTC2=arbtc2;
231700140611         iCBO=arbcbo;
231800140611         fispE02r ( iTYPE :
231900140611                    iLANG :
232000140611                    iDATA :
232100140611                    iNTW :
232200140611                    iLNP :
232300140611                    iNZD :
232400140611                    iPRD :
232500140611                    iCAD :
232600140611                    iLOD :
232700140611                    iIND :
232800140611                    iRSD :
232900140611                    iNCL :
233000140611                    iPKB :
233100140611                    iVLB :
233200140611                    iFFD :
233300140611                    iTSP :
233400140611                    iTC1 :
233500140611                    iTC2 :
233600140611                    iCBO :
233700140611                    oTFA :
233800140611                    oLNA :
233900140611                    oZNC :
234000140611                    oLNPD :
234100140611                    oLNAD :
234200140611                    oERR :
234300140611                    oMSG :
234400140611                    oTAG :
234500140611                    RTNesito :
234600140611                    RTNopcode :
234700140611                    RTNstatus );
234800140925       //if RTNesito<0;
234900140925       //ELSe;
235000140611          if oERR<>*blanks;
235100140611             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
235200140611             clear wIdMsg;
235300140611             wTextMsg=oMSG;
235400140611             select;
235500140611             when  %scan('Provincia':otag)>0;
235600140611                wIdCampo = 'NEWPRD    ';
235700140611             when  %scan('Localita':otag)>0;
235800140611                wIdCampo = 'NEWLOD    ';
235900140611             other;
236000140611                wIdCampo = 'NEWCAD    ';
236100140611             endsl;
236200140611             clear wParm;
236300140611             exsr Messaggi;
236400140611       // Se non ci sono errori verifico che la linea di arrivo calcolata sia la
236500141006       // stessa della bolla --> se dirottamento non possono selezionare la data/ora
236600140611          else;
236700140611             if arblna<>olna;
236800141006       //       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
236900141006       //       wIdMsg   = 'TIS0767';
237000141006       //       wIdCampo = '*CHGLNA   ';
237100141006       //       clear wParm;
237200141006       //       exsr Messaggi;
237300141006                Wolna=olna;
237400140611             endif;
237500140611          endif;
237600140925       //endif;
237700140611       endsr;
237800140610       //--------------------------------------------------------------
237900140610       //?Controllo Dati comuni a tutti i tipi di disposizioni
238000140610       //--------------------------------------------------------------
238100140610       BEGSR  sr_ds3A       ;
238200140610                   clear kkey;
238300140610                   clear ds3a;
238400140610                   %subst(kkey:1:2)=arbcbo;
238500140610                   chain (1:'3A':kkey) tabel00f ;
238600140610                   if %found(tabel00f);
238700140610                      ds3a=tbluni;
238800140610                   endif;
238900140610       endsr;
239000140523       //--------------------------------------------------------------
239100140523       //?Controllo Dati comuni a tutti i tipi di disposizioni
239200140523       //--------------------------------------------------------------
239300140523       BEGSR  sr_ctrUNI     ;
239400140609       //?Referente Consegna - obbligatorio
239500140530       if fnlry00i1.newrefcons=  *blanks;
239600140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
239700140530             wIdMsg   = 'TIS0754';
239800140530             wIdCampo = 'NEWREFCONS';
239900140530             clear wParm;
240000140530             exsr Messaggi;
240100140530       endif;
240200140530       //?Telefono Destinatario - obbligatorio
240300140530       if fnlry00i1.newtelcons=  *blanks;
240400140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
240500140530             wIdMsg   = 'TIS0755';
240600140530             wIdCampo = 'NEWTELCONS';
240700140530             clear wParm;
240800140530             exsr Messaggi;
240900140530       endif;
241000140523       //?Indirizzo e-mail
241100140528       if fnlry00i1.newdesmail<> *blanks;
241200140528          clear dsemail                        ;
241300140528          §emlindi = fnlry00i1.newdesmail;
241400140528          xemail (dsemail);
241500140528          if §emlerro <> '0';
241600140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
241700140528             wIdMsg   = 'TIS0732';
241800140528             wIdCampo = 'NEWDESMAIL';
241900140528             clear wParm;
242000140528             exsr Messaggi;
242100140528          endif;
242200140528       endif;
242300140523       //?N.Telefono per SMS
242400140528       if fnlry00i1.newdescell<> *blanks;
242500140528          pInCell = %trim(fnlry00i1.newdescell);
242600140528          pOutErr = *blanks;
242700140528          if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
242800140528           // Numero di cellulare KO
242900140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
243000140528             wIdMsg   = 'TIS0725';
243100140528             wIdCampo = 'NEWDESCELL';
243200140528             clear wParm;
243300140528             exsr Messaggi;
243400140528          endif;
243500140528       endif;
243600140523       endsr;
243700140529       //--------------------------------------------------------------
243800140529       //?Routine per recuperare flag di autorizzazione al Fermo Deposito
243900140529       //--------------------------------------------------------------
244000140529       BEGSR  sr_tab7r;
244100140529            clear kkey;
244200140529            clear ds7r;
244300140529            %subst(kkey:1:2)=arbgma;
244400140529            chain (1:'7R':kkey) tabel00f ;
244500140529            if %found(tabel00f);
244600140529               ds7r=tbluni;
244700140529            endif;
244800140529       endsr;
244900140528      /end-free
245000140521      **************************************************************************
245100140528     c     sr_OrariSr    BEGSR
245200140521
245300140521     c                   clear                   trulorSds
245301161129     c                   clear                   dataPRVC
245400140522      /free
245500140522              // reperisco gli orari di consegna previsti
245600140522              clear tnsd99ds;
245700161130              d98tbo=w_tbo;
245800140522              d98aas=arbaas;
245900140522              d98lnp=arblnp;
246000140522              d98nrs=arbnrs;
246100140522              d98nsp=arbnsp;
246101161130       // Per la distribuzione forzo tipo servizio "C":
246102161130       // - SEMPRE se la bolla è già in arrivo perchè in questo caso il collo
246103161130       //   o è in IMP oppure è partito ma comunque entrato
246104161130       // - Solo se entrato almeno un collo se la bolla è ancora "Da partire"
246200161130              if arbtsp='D' and (w_tbo='A' or arbdse>0 );
246300140522                 I98TSP_FOR='C';
246400140522              endif;
246500140522              tnsd99r(tnsd99ds);
246600140522              if d98err='0';
246700140522                IOREDTA = d98dci;
246701161130               if d98dee>0;
246702161129                DataPrvC= d98dEE;
246703161130               else;
246704161130                DataPrvC= %dec(d98dtctd:8:0);
246705161130               endif;
246800140522              endif;
246900140522      /end-free
247000140521     c                   eval       IOREtfp = ARBtfp
247100140521     c                   eval       IOREtfa = ARBtfa
247200150107     c                   if         arbdti>0
247300140521     c                   eval       IOREdti = ARBdti
247400150107     c                   else
247500150107     c* Caso di consegna parziale:
247600150107     c* Visto che vogliamo prendere gli orari "tutto il giorno", se dti vuoto non lo
247700150107     c* prenderebbe mai e qunidi metto ARBDAM
247800150107     c                   eval       IOREdti = ARBdam
247900150107     c                   endif
248000140528     c* imposto a 0 hti per recuperare sempre gli orari ' '=Tutto il giorno
248100140522     c****               eval       IOREhti = ARBhti
248200140521     c                   eval       IOREdcr = ARBdcr
248300140521     c                   eval       IOREhcr = ARBhcr
248400140521     c                   eval       IOREtcr = ARBtcr
248500140522     c                   eval       IOREDEI = d98dei
248600140522     c                   eval       IOREOEI = d98oei
248700150729
248800150729     c                   clear                   diore01
248900150729     c                   eval       §IORETRAZC=%editc(d98ttc:'X')
249000150729     c                   eval       §IORECONSC=%editc(d98tcc:'X')
249100150729     c                   eval       ioreflo=diore01
249200150729
249300140521     c                   eval       IOREfil = ARBlna
249400140923     c                   if         waltroind=' '
249500140521     c                   eval       IOREcap = ARBcad
249600140521     c                   eval       IOREloc = ARBlod
249700140521     c                   eval       IOREnar = ARBnzd
249800140923     c                   else
249900140925     c                   eval       IOREcap = fnlry00i1.newcad
250000140925     c                   eval       IOREloc = fnlry00i1.newlod
250100140925     c                   eval       IOREnar = fnlry00i1.newnzd
250200140923     c                   endif
250300140521     c
250400140521     c                   eval       IOREtser = 'C'
250500140521     c                   eval       IOREDDC=arbddc
250600140521     c                   eval       IOREnDC=arbndc
250700140521     c
250800140521     c
250900140521     c                   eval       IOREtsp = ARBtsp
251000140618     c                   if          ARBfbc = *blanks and
251100140618     c                               ARBddc > 0 and
251200140618     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
251300140618     c                               arbica='R' or arbicc=' ' or arbicc='R')
251400140618     c                   eval       IOREcons = 'S'
251500140618     c                   endif
251600140521     c
251700140521
251800140521     c                   call      'TRULORSR'
251900140521     c                   parm                    kpjba
252000140521     c                   parm                    trulorSds
252100140521     c                   parm                    trulor2ds
252200140521     c
252300140521     c                   ENDSR
252400140521      /free
252500140924       //--------------------------------------------------------------
252600140924       //?Routine per determinare il giorno della settimana di dataoggi + 1gg lav.
252700140924       //--------------------------------------------------------------
252800140924       BEGSR  sr_wdesgio ;
252900140924       //       Determino il giorno della settimana di dataoggi+1giorno lavorativo
253000140924       //       da restituire nel messaggio
253100140924               wdata=dataoggi;
253200140924       //       Incremento la data di un giorno
253300140924               exsr sr_incredata;
253400140924       //       Verifico se data festiva e in tal caso incremento finchè non trovo giorno
253500140924       //       lavorativo
253600140924               exsr sr_datafes;
253700140924       //       Determino il giorno della settimana della data calcolata
253800140924               dataiso=%date(wdata:*iso);
253900140924               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
254000140924               wdesgio=rtvMsgLang(dayOfWeek.msgId(dw) : widlingua);
254100140924       endsr;
254200140603       //--------------------------------------------------------------
254300140603       //?Routine per scrittura disposizioni di consegna
254400140603       //--------------------------------------------------------------
254500140924       BEGSR  SR_Tiidc;
254600140603       // Per la spedizione non devono esserci disposizioni ancora da elaborare
254700140603       exsr sr_ChkDispo;
254800140603       clear tiidc000;
254900141013       clear tiidcds ;
255000140718       clear tivpi000;
255100140604       exsr repnum;
255200140604       if esito = *blanks;
255300140604          IDCPRG=kprg                         ;
255400140604       else;
255500140619       // prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
255600140619  //   // wIdMsg   = 'TIS0757';
255700140619  //   // wIdCampo = '*REPNUM   ';
255800140619  //   // clear wParm;
255900140619  //   // exsr Messaggi;
256000140616          prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
256100140616          prmRpyIdMsg  = FNLRY00_ESITO_NUMERATORE_DISPO_NONREPERITO;
256200140604          exsr RoutEnd;
256300140604       endif;
256400141009       //WDCINSDATA = %dec(fnlry00i1.ksutimest);
256500141009       //IDCINSDATA = %dec(%subst(%editc(wdcinsdata:'X'):1:14):14:0);
256600141009       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
256700141009                                    : 1 : 14 ) );
256800141009       IDCtimestn   = fnlry00i1.ksutimestn;
256900140603       //IDCMODACC                           ;
257000140610       if wksu<>*blanks;
257100140610       IDCMODACC='L'                       ;
257200140610       else;
257300140610       endif;
257400140603       IDCIP = fnlry00I1.cltipaddr;
257500140603       IDCIDCLI = fnlry00i1.ksu;
257600140603       IDCTIPOCLI= Wchi;
257700140603       IDCWAAS   = kaas;
257800140603       IDCWLNP   = klnp;
257900140603       IDCWNRS   = knrs;
258000140603       IDCWNSP   = knsp;
258100140620       IDCAAS    = arbaas;
258200140620       IDCLNP    = arblnp;
258300140620       IDCNRS    = arbnrs;
258400140620       IDCNSP    = arbnsp;
258500140603       IDCFGS    = wfgslna;
258600140603       IDCTIPODIS = fnlry00i1.tipodispo;
258700140604       IDCMAILAVV = fnlry00i1.newdesmail;
258800140604       IDCTELAVV  = fnlry00i1.newdescell;
258900161202       //IDCALERT Metto nell'ultimo byte informazione del momento in cui
258901161202       //         è stata immessa la dispo rispetto allo stato di avanzamento
258902161202       //         della bolla: P=Bolla da Partire,
258903161202       //                      A=Arrivata ma non ancora fatto il primo tentativo di consegna
258904161202       //                  blank=Arrivata e già fatto il primo tentativo di consegna
258905161202       clear  didcal;
258906161202       select;
258907161202       when w_tbo='P';
258908161202          eval §IDCSTA='P'         ;
258909161202       when w_tbo='A' and arbntc=0 and wtentacons=*blanks;
258910161202          eval §IDCSTA='A'         ;
258913161202       endsl;
258914161202       eval idcalert=didcal;
258915161202
259000140604       if fnlry00i1.newdtcrich <> *blanks and fnlry00i1.tipodispo='1';
259100140604           IDCDCR =  %dec(%date(fnlry00i1.newdtcrich:*eur):*iso);
259200140604       endif;
259300140604       if fnlry00i1.neworcrich <> *blanks and fnlry00i1.tipodispo='1';
259400140604           IDCHCR =  %dec(fnlry00i1.neworcrich:4:0);
259500140604       endif;
259600140924       if fnlry00i1.newdtcrici <> *blanks and fnlry00i1.tipodispo='3';
259700140924           IDCDCR =  %dec(%date(fnlry00i1.newdtcrici:*eur):*iso);
259800140924       endif;
259900140924       if fnlry00i1.neworcrici <> *blanks and fnlry00i1.tipodispo='3';
260000140924           IDCHCR =  %dec(fnlry00i1.neworcrici:4:0);
260100140924       endif;
260200140603       //IDCTCR                         ;
260300140924       if fnlry00i1.tipodispo='1';
260400140924          IDCPRCR=  fnlry00i1.prconsric  ;
260500140924       endif;
260600140924       if fnlry00i1.tipodispo='3';
260700140924          IDCPRCR=  fnlry00i1.prconsrici ;
260800140924       endif;
260900140603       IDCREF  = fnlry00i1.newrefcons ;
261000140603       IDCTELD = fnlry00i1.newtelcons ;
261100140603       if fnlry00i1.tipodispo='2';
261200140603          IDCFFD  = 'S';
261300140603       endif;
261400140611       if fnlry00i1.tipodispo='3';
261500141007          wtxt     = fnlry00i1.newrsd;
261600141007          uppercase='1';
261700141007          exsr sr_chk;
261800141007          IDCRSD   = wtxt;
261900141007          wtxt     = fnlry00i1.newind;
262000141007          uppercase='1';
262100141007          exsr sr_chk;
262200141007          IDCIND   = wtxt            ;
262300140611          IDCCAD   = fnlry00i1.newcad;
262400141007          wtxt     = fnlry00i1.newlod;
262500141007          uppercase='1';
262600141007          exsr sr_chk;
262700141007          exsr chkstringa;
262800141007          IDCLOD   = wtxt            ;
262900140611          IDCPRD   = fnlry00i1.newprd;
263000140611          IDCNAZ   = fnlry00i1.newnzd;
263100141006       // bolla da dirottare
263200141006          if wolna>0;
263300141006               IDCNEWLNA=wolna;
263400141006               IDCFLA   ='S';
263500141006          endif;
263600140611       endif;
263700140611       if fnlry00i1.tipodispo='4';
263800140611          IDCNOTE  = fnlry00i1.altro;
263900140611       endif;
264000140603       IDCELAFLG='S';
264100140603
264200140718       VPITIP='TI';
264300140718       VPIISV='TT';
264400140718       // indicazioni ricevute da cliente loggato
264500140718       if wksu<>*blanks;
264600140801          VPISUN=%editc(fnlry00i1.sun:'X');
264700140718          VPIKSU=wksu    ;
264800140718       else;
264900140718       // indicazioni ricevute mediante BRTCODE
265000140718          VPISUN='8IDC00000';
265100140718          VPIKSU='0IDC0000';
265200140718       endif;
265300140718       VPIDTA=tiidcds ;
265400140718       write tivpi000;
265500140814       // se AS888 scrivo anche tiidc
265600140814
265700140814      /end-free
265800140814     c* controllo se sono in AS888
265900141006     c                   eval      comman=chklib(1)
266000141006     c                   eval      lengh=35
266100141006     c                   call      'QCMDEXC'                            99
266200141006     c                   parm                    comman           80
266300141006     c                   parm                    lengh            15 5
266400140814     c
266500141006     c                   if        not *in99
266600140814      /free
266700140814
266800141013         write tiidc000;
266900141006         endif  ;
267000140603
267100140603       endsr;
267200140603       //--------------------------------------------------------------
267300140603       //?Verifica presenza di diposizioni ancora da elaborare
267400140603       //--------------------------------------------------------------
267500140603       BEGSR  SR_ChkDispo;
267600140908       clear w0140;
267700140908       setll (arbaas:arblnp:arbnrs:arbnsp:w0140) tiidc01l;
267800140603       if %equal(tiidc01l);
267900140603              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
268000140603              wIdMsg   = 'TIS0756';
268100140603              wIdCampo = '*TIIDC    ';
268200140611              wParm = wbolla;
268300140603              exsr Messaggi;
268400140603              exsr Routend;
268500140603       endif;
268600140718       // verifico anche che non siano ancora su tivpi
268700140801       eval wkeyspa=%editc(arbaas:'X')+%editc(arblnp:'X')+
268800140801                    %editc(arbnrs:'X')+%editc(arbnsp:'X')  ;
268900140801      /end-free
269000140718     C/EXEC SQL
269100140718     C+ SELECT count(*) INTO :totale FROM tivpi00f where
269200140718     c+ VPITIP='TI' and VPIISV='TT' and
269300141002     C+ substr(vpidta, 67, 16) = :wkeyspa
269400140718     C/END-EXEC
269500140801      /free
269600140718          if totale>0 ;
269700140718              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
269800140718              wIdMsg   = 'TIS0756';
269900140718              wIdCampo = '*TIVPI    ';
270000140718              wParm = wbolla;
270100140718              exsr Messaggi;
270200140718              exsr Routend;
270300140718       endif;
270400140603       endsr;
270500140604        //-----------------------------------------------------------------------*
270600140604        // Reperimento progressivo richiesta da numeratore 650
270700140604        //-----------------------------------------------------------------------*
270800140604        begsr RepNum;
270900140604
271000140604          clear esito;
271100140604          $finenum=*off;
271200140604          clear trul33ds;
271300140604
271400140604          I33TLa = 'L';
271500140604          I33Ope = *ZEROS;
271600140604          I33CNu = 650;
271700140604          I33Num = 1;
271800140604
271900140604          KPJBU = TRUL33DS;
272000140604
272100140604          dow $finenum=*off   ;
272200140604             Trul33R(kpjba);
272300140604
272400140604             TRUL33DS = KPJBU;
272500140604
272600140604             if O33Err <> *zeros;
272700140604               Esito = '2';
272800140604               $finenum=*on;
272900140604             else;
273000140908               w0090=o33nri;
273100140908               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
273200140604               setll kprg tiidc02l;
273300140604        // Se numero già presente nel file TIIDC ne cerco un altro
273400140604               if not %equal;
273500140604                  $finenum=*on;
273600140604               endif;
273700140604             endif;
273800140604          enddo;
273900140604
274000140604          endsr;
274100140604
274200140708       //--------------------------------------------------------------
274300140708       //?Routine per schierare i messaggi nella ds FNLRY00M0
274400140708       //--------------------------------------------------------------
274500140708       BEGSR  sr_RepPct;
274600140708
274700140708       clear wtentacons;
274800140708
274900140708      /end-free
274901161202     c                   if        arbndc>0
275000140708     c                   move      arbndc        pctndc
275100140708     c                   move      arbifp        pctfgs
275200140708     c                   move      'CET'         ttrd
275300140708     c     kcet          setgt     fipct02l
275400140708     c     kcet          readpe    fipct02l
275500140708     c                   if        not %eof(fipct02l)
275600140708     c                   movel     pctdati       fipctcetds
275700140708     c* consegnata
275800140708     c                   if        §pctcmc = ' '
275900140708     c                   eval      prmRpyOpCode=FNLRY00_RPYOPCODE_ERROR
276000141006     c                   if        prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO
276100141006     c                   eval      wIdMsg   = 'TISA738'
276200141006     c                   eval      wIdCampo = '*BOLLA    '
276300141006     c                   clear                   wbolla
276400141006     c                   else
276500140708     c                   eval      wIdMsg   = 'TIS0738'
276600140708     c                   eval      wIdCampo = '*BOLLA    '
276700140708     c                   eval      wParm = wbolla
276800141006     c                   endif
276900140708     c                   exsr      Messaggi
277000140708     c                   exsr      routend
277100140708     c                   else
277200140708       //?deve essere già stato fatto almeno un tentativo di consegna
277300140708     c                   eval      wtentacons='1'
277400140708     c                   endif
277500140708     c                   endif
277501161202     c                   endif
277600140708      /free
277700140708       endsr;
277800140929       //--------------------------------------------------------------
277900140929       //?Routine per tradurre mattino/pomeriggio
278000140929       //--------------------------------------------------------------
278100140929       BEGSR  sr_matpom;
278200140929
278300140929            If wconsric='P';
278400140929                widmsg='TIS0676';
278500140929            else;
278600140929                widmsg='TIS0667';
278700140929            endif;
278800140929            matpom=rtvMsgLang(widmsg : widlingua);
278900140929       endsr;
279000140604
279100140527       //--------------------------------------------------------------
279200140527       //?Routine per schierare i messaggi nella ds FNLRY00M0
279300140527       //--------------------------------------------------------------
279400140527       BEGSR  Messaggi;
279500140527
279600140527       //?Se ho già caricato 99 messaggi esco
279700140527         IF  fnlry00M0.nrmsg = 99;
279800140527           exsr RoutEnd;
279900140527           clear fnlry00M0.nrmsg;
280000140527         ENDIF;
280100140527
280200140527         fnlry00M0.nrmsg += 1;
280300140527         fnlry00M0.skIdMsg(fnlry00M0.nrmsg) = wIdMsg;
280400140527         fnlry00M0.skIdCampo(fnlry00M0.nrmsg) = wIdCampo;
280500140527         fnlry00M0.skErrWarn(fnlry00M0.nrmsg) = FNLRY00_MSG_ERRORE;
280600140527         IF  wParm <> *blanks;
280700140527           fnlry00M0.skTextMsg (fnlry00M0.nrmsg) =
280800140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
280900140527         ELSE;
281000140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
281100140611           if wIdMsg=*blanks;
281200140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=wTextMsg;
281300140611           else;
281400140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=
281500140611             rtvMsgLang(wIdMsg:wIdlingua);
281600140611           endif;
281700140527         ENDIF;
281800140527
281900140527       ENDSR;
282000140527
282100140521       //--------------------------------------------------------------
282200140408       //?Carico tabelle.
282300140408       //--------------------------------------------------------------
282400140408       BEGSR  CaricaTab;
282500140408
282600140408
282700140408       ENDSR;
282800131010
282900131010       //--------------------------------------------------------------
283000131010       //?Operazioni finali.
283100131010       //--------------------------------------------------------------
283200131010       BEGSR  RoutEnd;
283300140408
283400140519       //?Se richiamato per GET_ISTRUCO
283500140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
283600140519           %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o0;
283700140408         ELSE;
283800131010
283900140522       //?Se richiamato per PUT_ISTRUCO
284000140522          %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o1;
284100140521       //%subst(prmRpyForzatu:1:prmRpyForSize) = fnlry0F0;
284200140408
284300140408         ENDIF;
284400140530
284500140530          %subst(prmRpyMessage:1:prmRpyMsgSize) = fnlrY00M0;
284600131010
284700131010         return;
284800131010
284900131010       ENDSR;
285000131009
285100131007      /end-free
285200141007      *---------------------------------------------------------
285300141007      * Controllo i caratteri dei campi alfanumerici
285400141007      *---------------------------------------------------------
285500141007     c     Sr_Chk        BegSr
285600141007
285700141007     c                   clear                   esito
285800141007     c                   Eval      TxtInOut = wtxt
285900141007     c                   Call      'XCHKCHAR'
286000141007     c                   Parm                    TxtInOut
286100141007     c                   Parm                    ElencoChar
286200141007     c                   Parm                    TipoElenco
286300141007     c                   Parm                    CharSost
286400141007     c                   Parm                    UpperCase
286500141007     c                   Parm                    ChkNull
286600141007     c                   Parm                    CharNull
286700141007     c                   Parm                    Esito
286800141007     c                   If        Esito = '1'
286900141007     c                   Move      TxtInOut      wtxt
287000141007     c                   EndIf
287100141007
287200141007     c                   EndSr
287300141007      *---------------------------------------------------------
287400141007      * Controllo caratteri di punteggiatura precedenti e seguenti
287500141007      *---------------------------------------------------------
287600141007     c     chkstringa    BegSr
287700141007
287800141007     c* trovo la prima posizione "buona" da utilizzare
287900141007     c                   eval      posl=%check(charnook:wtxt)
288000141007     c* trovo l'ultima posizione "buona" da utilizzare (escludo il ".")
288100141007     c                   eval      posr=%checkr(charnookf:wtxt)
288200141007     c                   if        posl=0
288300141007     c                   clear                   wtxt
288400141007     c                   else
288500141007     c* determino la lunghezza della stringa "buona"
288600141007     c                   eval      lung=(posr-posl)+1
288700141007     c                   eval      wtxt=%subst(wtxt:posl:lung)
288800141007     c                   endif
288900141007
289000141007     c                   EndSr
289001161129       //--------------------------------------------------------------
289002161129       //?Pulizia campi di blp non presenti su arb
289003161129       //--------------------------------------------------------------
289004161129       BEGSR sr_inzsoloblp;
289005161129          clear arbflp;
289006161129          clear arbdim;
289007161129          clear arbsop;
289008161129          clear arbfst;
289009161129          clear arbctm;
289010161129          clear arbdrt;
289011161129          clear arbnrt;
289012161129          clear arbfpp;
289013161129          clear arbpdr;
289014161129          clear arbdse;
289015161129       ENDSR;
289100140814**
289200141006CHKOBJ OBJ(FILTRAPRD) OBJTYPE(*LIB)
