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
009700140522     D tnsd99ds      e ds
009800140604     D TRUL33DS      E DS
009900140923     D Tisio9ds      E DS
010000140521
010100140521     d fidn12ds      e ds
010200140603     d tibs02ds      e ds
010300140603     d fnlv55ds      e ds
010400140604     d KPJBA         e ds
010500140521
010600140521     d xcfivads      e ds                  INZ
010700140521     d fipctcetds    e ds
010800140521     d og143         e ds
010900140522     d dar5emd       e ds
011000140523     d dar5gen       e ds
011100140522     d ds7r          e ds
011200140522     d ds3a          e ds
011300140613     d ds2a          e ds
011400140528     d ds2g          e ds
011500140528     d ds7u          e ds
011600140619     d ds7w          e ds
011700140603     d divp          e ds
011800140924     d dvpooraris    e ds
011900140528
012000140528     D WDAT8           DS
012100140528     D  DATADA                 1      8  0
012200140528     D  DATAA                  9     16  0
012300140528     D  GGL                   17     21  0
012400140610     D tfptfa          DS
012500140610     D  p_tfp                  1      3  0
012600140610     D  p_tfa                  4      6  0
012700140606
012800140606     D clnmat          DS
012900140606     D  mat                    1     31    dim(31)
013000140606     D clnpom          DS
013100140606     D  pom                    1     31    dim(31)
013200140617     D dayOfWeek       DS                  QUALIFIED
013300140617     D                                7A   INZ(MSGID_LUNEDI)
013400140617     D                                7A   INZ(MSGID_MARTEDI)
013500140617     D                                7A   INZ(MSGID_MERCOLEDI)
013600140617     D                                7A   INZ(MSGID_GIOVEDI)
013700140617     D                                7A   INZ(MSGID_VENERDI)
013800140617     D                                7A   INZ(MSGID_SABATO)
013900140617     D                                7A   INZ(MSGID_DOMENICA)
014000140617     D  msgId                         7A   DIM(7) OVERLAY(dayOfWeek)
014100140814
014200140814     D chklib          S             35    DIM(1) CTDATA PERRCD(1)
014300140312
014400131009       //--------------------------------------------------------------
014500131009       //?Definizione schiere.
014600131009       //--------------------------------------------------------------
014700140210
014800140210     d skFIdMsg        s              7a   dim(99)
014900140210     d skFIdCampo      s             10a   dim(99)
015000140210     d skFGiaForza     s              1a   dim(99)
015100131007
015200131007       //--------------------------------------------------------------
015300131007       //?Definizione campi.
015400131007       //--------------------------------------------------------------
015500140527     d wIdMsg          s              7a
015600140527     d wIdCampo        s             10a
015700140527     d wcampo          s             10a
015800140519     d wksu            s                   like(fnlry00i0.ksu)
015900140520     d widbolla        s                   like(fnlry00i0.idbolla)
016000140521     d wbolla          s                   like(widbolla)
016100140520     d wbrtcode        s                   like(fnlry00i0.brtcode)
016200140527     d wIdlingua       s                   like(fnlry00i0.liniso2)
016300140527     d wdcreur         s                   like(fnlry00i1.olddtcrich)
016400140529     d wrbhcr          s                   like(fnlry00i1.OLDORCRICH)
016500140528     d wtprich         s              1
016600140530     d wtentacons      s              1
016700140528     d dataoggi        s              8s 0
016800141218     d wdataconfr      s              8s 0
016900140604     d annocur         s              4s 0
017000140528     d wdcr            s              8s 0
017100140605     d wdata           s              8s 0
017200140528     d wora            s              4s 0
017300140527     d wrsd2           s             35
017400140522     d wchi            s              1
017500140521     d kaas            s                   like(arbaas)
017600140521     d klnp            s                   like(arblnp)
017700140521     d knrs            s                   like(arbnrs)
017800140521     d knsp            s                   like(arbnsp)
017900140522     d kkey            s                   like(tblkey)
018000140523     d wrefbolla       s                   like(§ar5ref)
018100140523     d wtelbolla       s                   like(§ar5teld)
018200140528     d wlimitdcr       s                   like(§2gldr)
018300140613     d wlimitFD        s                   like(§2agla)
018400140521     d wcliente        s             11
018500140521     D Wlibfil         S             21
018600140521     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
018700140521     D Wlibfil201      S             21    INZ('FILTRA201/        ')
018800140521     d dataeur         s               d   datfmt(*eur)
018900140528     d dataiso         s               d   datfmt(*iso)
019000140528     d wParm           s            512a   inz
019100140528     d w008a           s              8a   inz
019200140908     d w0140           s             14  0 inz
019300140603     d wfgslna         s                   like(d55tfa)
019400140616     d wfgslnad        s             20a
019500140604     D Esito           s              1a
019600141031     d w0090           s              9  0
019700140604     d kprg            s                   like(idcprg)
019800140604     d WDCINSDATA      s             20  0
019900140605     d I               s              2  0
020000140606     d Ix              s              2  0
020100140923     d Iy              s              2  0
020200140606     d wfes            s              1
020300140606     d matpom          s             10
020400140611     d wTextMsg        s            255
020500140604     d $Finenum        s               n   inz(*off)
020600140617     d dw              s              1s 0
020700140617     d wdesgio         s             20a
020800140718     d totale          s              2  0
020900140801     d wkeyspa         s             16
021000140924     D wtempoh         S              2  0 inz
021100140924     D wtempom         S              2  0 inz
021200140925     D waltroind       S              1a   inz
021300140930     D wconsric        S                   like(fnlry00i1.prconsric)
021400140930     D w_dalle         S               t
021500140930     D w_alle          S               t
021600140930     D w_dalleoser     S               t
021700140930     D w_alleoser      S               t
021800140930     D w_dallemsg      S              5a
021900140930     D w_allemsg       S              5a
022000140930     D w_orcric        S                   like(fnlry00i1.neworcrich)
022100141006     D wolna           S                   like(olna)
022200141007     d wtxt            S           2048
022300141007     d TxtInOut        S           2048
022400141007     d ElencoChar      S            256
022500141007     d TipoElenco      S              1
022600141007     d CharSost        S              1
022700141007     d UpperCase       S              1
022800141007     d ChkNull         S              1    Inz('1')
022900141007     d CharNull        S              1
023000141007     d posl            s              2  0
023100141007     d posr            s              2  0
023200141007     d lung            s              2  0
023300141007     d* Lunghezza di CHARNOOK deve essere:  lunghezza di tbeuni + 1
023400141007     d* Il +1 è necessario per assicurarmi che nella stringa sia sempre
023500141007     d* presente un blank
023600141007     d charnook        s            257
023700141007     d charnookf       s                   like(charnook)
023800140611       //?- Parametri per FISPE02R: EasySpedWeb: S.P.  TISIT5R+TISI95R?
023900140611     d iTYPE           s              1    inz
024000140611     d iLANG           s              3    inz
024100140611     d iDATA           s              8s 0 inz
024200140611     d iNTW            s              1    inz
024300140611     d iLNP            s              3s 0 inz
024400140611     d iNZD            s              3    inz
024500140611     d iPRD            s              2    inz
024600140611     d iCAD            s              9    inz
024700140611     d iLOD            s             35    inz
024800140611     d iIND            s             35    inz
024900140611     d iRSD            s             35    inz
025000140611     d iNCL            s              5s 0 inz
025100140611     d iPKB            s              7s 1 inz
025200140611     d iVLB            s              5s 3 inz
025300140611     d iFFD            s              1    inz
025400140611     d iTSP            s              1    inz
025500140611     d iTC1            s              1    inz
025600140611     d iTC2            s              1    inz
025700140611     d iCBO            s              2    inz
025800140611      *
025900140611     d oTFA            s              3s 0 inz
026000140611     d oLNA            s              3s 0 inz
026100140611     d oZNC            s              2s 0 inz
026200140611     d oLNPD           s             20    inz
026300140611     d oLNAD           s             20    inz
026400140611     d oERR            s              1    inz
026500140611     d oMSG            s            256    inz
026600140611     d oTAG            s             30    inz
026700140611      *
026800140611     d RTNesito        s             10I 0 inz
026900140611     d RTNopcode       s             10    inz
027000140611     d RTNstatus       s             10I 0 inz
027100131008
027200131008       //--------------------------------------------------------------
027300131008       //?Definizione procedure.
027400131008       //--------------------------------------------------------------
027500140603     d tibs02r         pr                  extpgm('TIBS02R')
027600140604     d  kpjba                              likeds(kpjba)
027700140603     d  tibs02ds                           likeds(tibs02ds)
027800140604     D trul33R         pr                  extpgm('TRUL33R')
027900140604     D  kpjba                              likeds(kpjba)
028000140923     D tisio9r         pr                  extpgm('TISIO9R')
028100140923     D  kpjba                              likeds(kpjba)
028200140923     D  tisio9ds                           likeds(tisio9ds)
028300140611       //?- Stored Procedure per EasySpedWeb?
028400140611     d fispE02r        pr                  extpgm('FISPE02R')
028500140611     d   i_TYPE                            like(iTYPE)
028600140611     d   i_LANG                            like(iLANG)
028700140611     d   i_DATA                            like(iDATA)
028800140611     d   i_NTW                             like(iNTW)
028900140611     d   i_LNP                             like(iLNP)
029000140611     d   i_NZD                             like(iNZD)
029100140611     d   i_PRD                             like(iPRD)
029200140611     d   i_CAD                             like(iCAD)
029300140611     d   i_LOD                             like(iLOD)
029400140611     d   i_IND                             like(iIND)
029500140611     d   i_RSD                             like(iRSD)
029600140611     d   i_NCL                             like(iNCL)
029700140611     d   i_PKB                             like(iPKB)
029800140611     d   i_VLB                             like(iVLB)
029900140611     d   i_FFD                             like(iFFD)
030000140611     d   i_TSP                             like(iTSP)
030100140611     d   i_TC1                             like(iTC1)
030200140611     d   i_TC2                             like(iTC2)
030300140611     d   i_CBO                             like(iCBO)
030400140611     d   o_TFA                             like(oTFA)
030500140611     d   o_LNA                             like(oLNA)
030600140611     d   o_ZNC                             like(oZNC)
030700140611     d   o_LNPD                            like(oLNPD)
030800140611     d   o_LNAD                            like(oLNAD)
030900140611     d   o_ERR                             like(oERR)
031000140611     d   o_MSG                             like(oMSG)
031100140611     d   o_TAG                             like(oTAG)
031200140611     d   RTN_esito                         like(RTNesito)
031300140611     d   RTN_opcode                        like(RTNopcode)
031400140611     d   RTN_status                        like(RTNstatus)
031500131008
031600131008       //--------------------------------------------------------------
031700131008       //?Definizione prototipi.
031800131008       //--------------------------------------------------------------
031900140519      /copy gaitrasrc/srcprotopr,Fnlry00r
032000140520      /copy gaitrasrc/srcprotopr,TIBS10R
032100140521      /copy gaitrasrc/srcprotopr,TRUL28R0
032200140521      /copy gaitrasrc/srcprotopr,FIDN12R
032300140522      /copy gaitrasrc/srcprotopr,tnsd99r
032400131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
032500131230      /copy gaitrasrc/srcprotopr,TIBS0800R
032600140311      /copy gaitrasrc/srcprotopr,UBCHKCEL
032700140311      /copy gaitrasrc/srcprotopi,UBCHKCEL
032800131008      /copy gaitrasrc/srcprotopr,XCFIVAR
032900140311      /copy gaitrasrc/srcprotopr,XEMAIL
033000140528      /copy gaitrasrc/srcprotopr,xsrlav8
033100140603      /copy gaitrasrc/srcprotopr,fnlv55r
033200131219
033300131219       //--------------------------------------------------------------
033400131219       //?Definizione parametri programma.
033500131219       //--------------------------------------------------------------
033600140519     D Fnlry00_Immetti...
033700140519     D                 PI
033800140519     D prmRqsOpCode...
033900140519     D                               10I 0 CONST
034000140519     D prmRpyOpCode...
034100140519     D                               10I 0
034200140519     D prmRpyIdMsg...
034300140519     D                               10I 0
034400140519     D prmRqsFormato...
034500140519     D                               10A   CONST
034600140519     D prmRqsData...
034700140519     D                            32767A   OPTIONS(*VARSIZE)
034800140519     D prmRqsDataSize...
034900140519     D                               10I 0 CONST
035000140519     D prmRpyFormato...
035100140520     D                               10A   CONST
035200140519     D prmRpyData...
035300140519     D                            32767A   OPTIONS(*VARSIZE)
035400140519     D prmRpyDataSize...
035500140519     D                               10I 0 CONST
035600140519     D prmRpyFormMsg...
035700140519     D                               10A   CONST OPTIONS(*NOPASS)
035800140519     D prmRpyMessage...
035900140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
036000140519     D prmRpyMsgSize...
036100140519     D                               10I 0 CONST OPTIONS(*NOPASS)
036200140519     D prmRpyFormForz...
036300140519     D                               10A   CONST OPTIONS(*NOPASS)
036400140519     D prmRpyForzatu...
036500140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
036600140519     D prmRpyForSize...
036700140519     D                               10I 0 CONST OPTIONS(*NOPASS)
036800131007
036900131007       //--------------------------------------------------------------
037000131007       //?MAIN.
037100131007       //--------------------------------------------------------------
037200131008
037300131007      /free
037400131008
037500131008       //?Operazioni iniziali
037600131008       exsr RoutInz;
037700131008
037800140523       //?Controllo formale dei dati di accesso
037900131008       exsr Controlla;
038000140523       //?Controllo stato bolla: deve essere in uno stato che renda possibile
038100140523       //?l'immissione delle disposizioni di consegna
038200140523       exsr StatoBolla;
038300140529
038400140523       //?Controllo che l'immagine della bolla da Web sia la stessa della bolla su
038500140523       //?AS/400
038600140529       //IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
038700140529       //   exsr ImmagineB;
038800140529       //endif;
038900140529
039000140529       //?Chaino estensioni bolla
039100140529       exsr sr_chainEstBolla;
039200140523       //?Controllo dettaglio dei campi ricevuti
039300140521       exsr ControllaDett;
039400140603       //?Per PUTISTRUCO in modalità Controllo + Inserimento scrivo file TIIDC
039500140611       IF  prmRqsOpCode=FNLRY00_RQSOPCODE_PUT_ISTRUCO and fnlry00i1.tipoela='2'
039600140611                                                      and fnlry00m0.nrmsg=0;
039700140604          exsr sr_Tiidc;
039800140613       // Restituzione messaggio di conferma
039900140613          exsr sr_MsgConf;
040000140603       endif;
040100131010
040200131010       //?Operazioni finali
040300131010       exsr RoutEnd;
040400131008
040500140613       //--------------------------------------------------------------
040600140613       //?Messaggio di avvenuta registrazione delle Indicazioni di Cons.
040700140613       //--------------------------------------------------------------
040800140613       BEGSR  sr_MsgConf;
040900140613
041000140929       // MESSAGGIO GENERICO
041100140617       wIdMsg   = 'TIS0771';
041200140617       fnlry00o1.MSGCONFERM=rtvMsgLang(wIdMsg:widlingua);
041300141007       fnlry00o1.MSGCONFERM=%trim(fnlry00o1.MSGCONFERM)+' del '
041400141007                                                  + %char(%date():*eur)
041500141007                                                  + ' '+  %char(%time()) ;
041600140613
041700140929       // MESSAGGIO SPECIFICO per Fermo Deposito e Consegna richiesta:
041800140929       // FERMO DEPOSITO
041900140929       select;
042000140929       when fnlry00i1.tipodispo='2';
042100140930          exsr sr_wdesgio;
042200140924          wIdMsg   = 'TIS0772';
042300140924          wparm=wdesgio + %editc(WlimitFD:'X') ;
042400140929          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
042500140617          fnlry00o1.fdidfil=wfgslna;
042600140929       // APPUNTAMENTO
042700140929       when fnlry00i1.tipodispo='1';
042800140930          wIdMsg   = 'TIS0787';
042900140930          wparm=fnlry00i1.newdtcrich ;
043000140929       // presente orario
043100140929          if fnlry00i1.neworcrich>*zeros;
043200140930             wIdMsg   = 'TIS0785';
043300140930             w_orcric=fnlry00i1.neworcrich;
043400140930             exsr sr_dallealle;
043500140930            wparm=fnlry00i1.newdtcrich + w_dALLEmsg + W_ALLEMSG;
043600140929          endif;
043700140929       // Presente preferenza matt/pomeriggio
043800140929          if fnlry00i1.prconsric<>*blanks;
043900140929             wconsric=fnlry00i1.prconsric;
044000140929             exsr sr_matpom;
044100140930             wIdMsg   = 'TIS0789';
044200140929             wparm=fnlry00i1.newdtcrich + matpom ;
044300140929          endif;
044400140930          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
044500140929       // ALTRO INDIRIZZO
044600140930       when fnlry00i1.tipodispo='3';
044700140930       // presente orario --> determino il "Dalle - Alle"
044800140930          if fnlry00i1.neworcrici>*zeros;
044900140930             w_orcric=fnlry00i1.neworcrici;
045000140930             exsr sr_dallealle;
045100140930          endif;
045200140930       // Presente preferenza matt/pomeriggio --> Traduco il flag nella descrizione
045300140930          if fnlry00i1.prconsrici<>*blanks;
045400140930             wconsric=fnlry00i1.prconsrici;
045500140930             exsr sr_matpom;
045600140930          endif;
045700141001       // Messaggi in presenza della data
045800140930          if fnlry00i1.newdtcrici>*zeros;
045900140930             wIdMsg   = 'TIS0788';
046000140930             wparm=fnlry00i1.newdtcrici ;
046100140930             if fnlry00i1.neworcrici>*zeros;
046200140930                wIdMsg   = 'TIS0786';
046300140930                wparm=fnlry00i1.newdtcrici + w_dALLEmsg + W_ALLEMSG;
046400140930             endif;
046500140930       // Presente preferenza matt/pomeriggio
046600140930             if fnlry00i1.prconsrici<>*blanks;
046700140930                wIdMsg   = 'TIS0790';
046800140930                wparm=fnlry00i1.newdtcrici + matpom ;
046900140930             endif;
047000141001             fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
047100140930          else;
047200141001       // Messaggi in assenza della data (caso che non si verifica perchè se presente
047300141001       //   ora o preferenza devo avere anche la data)
047400140930             if fnlry00i1.neworcrici>*zeros;
047500140930                wIdMsg   = 'TIS0791';
047600140930                wparm=w_dALLEmsg + W_ALLEMSG;
047700141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
047800140930             endif;
047900140930       // Presente preferenza matt/pomeriggio
048000140930             if fnlry00i1.prconsrici<>*blanks;
048100140930                wIdMsg   = 'TIS0792';
048200140930                wparm=matpom ;
048300141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
048400140930             endif;
048500140930          endif;
048600140929
048700140929       endsl;
048800140613
048900140613       endsr;
049000140930       //--------------------------------------------------------------
049100140930       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
049200140930       //--------------------------------------------------------------
049300140930       BEGSR  sr_dallealle;
049400140930
049500140930       //   determino il Dalle/alle da mettere nel messaggio
049600140930            w_dalle=%time((%dec(w_orcric:4:0)*100):*hms)-%minutes(§VPORST_MW);
049700140930            w_alle=%time((%dec(w_orcric:4:0)*100):*hms)+%minutes(§VPORST_PW);
049800140930       //   determino orari servizio comprensivi della tolleranza
049900140930            if §ivptipo='S';
050000140930               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
050100140930               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
050200140930            else;
050300140930               w_dalleoser=%time((oor2miis *100):*hms)-%minutes(§VPORST_MT);
050400140930               w_alleoser=%time((oor2mxfs *100):*hms)+%minutes(§VPORST_MT);
050500140930            endif;
050600140930       //   Confronto dalle/alle calcolato con dalle/alle orari ser
050700140930       //      Dalle < del "dalle" orario ser --> prendo quest'ultimo e l'alle
050800140930       //      lo ricalcolo aggiungendo i minuti da tabella vpo
050900140930            if w_dalle<w_dalleoser;
051000140930               w_dalle=w_dalleoser;
051100140930               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
051200140930            endif;
051300140930       //      Alle  > del "alle" orario ser --> prendo quest'ultimo e il dalle
051400140930       //      lo ricalcolo sottraendo i minuti da tabella vpo purchè non diventi
051500140930       //      minore del "dalle" orario ser
051600140930            if w_alle>w_alleoser;
051700140930               w_alle=w_alleoser;
051800140930               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
051900140930                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
052000140930               endif;
052100140930            endif;
052200140930            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
052300140930            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
052400140930
052500140930       endsr;
052600131008       //--------------------------------------------------------------
052700131008       //?Operazioni iniziali.                                         ?
052800131008       //--------------------------------------------------------------
052900131008       BEGSR  RoutInz;
053000140211
053100140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
053200131008
053300140519         prmRpyOpCode = FNLRY00_RPYOPCODE_DONE;
053400140519         prmRpyIdMsg  = FNLRY00_ESITO_OK;
053500131009
053600131009       //?Data del giorno
053700131009         dataoggi = %dec(%date());
053800140604         annocur  = %subdt(%date():*years);
053900131009
054000131009       //?Pulisco ds di output
054100140519         reset Fnlry00o0;
054200140519         reset fnlry00o1;
054300140522         reset fnlry00m0;
054400140521
054500140521       //?Apertura file
054600140521         wlibfil=wlibfil201;
054700140521         %subst(wlibfil:11:8)='FNARB01L';
054800140521         open(e) fnarb01l;
054900140521         if not %open(fnarb01l);
055000140521            wlibfil=wlibfilprd;
055100140521            %subst(wlibfil:11:8)='FNARB01L';
055200140521            open fnarb01l;
055300140521         endif;
055400140521         wlibfil=wlibfil201;
055500140521         %subst(wlibfil:11:8)='FIPCT02L';
055600140521         open(e) fipct02l;
055700140521         if not %open(fipct02l);
055800140521            wlibfil=wlibfilprd;
055900140521            %subst(wlibfil:11:8)='FIPCT02L';
056000140521            open fipct02l;
056100140521         endif;
056200140522         wlibfil=wlibfil201;
056300140522         %subst(wlibfil:11:8)='FIAR501L';
056400140522         open(e) fiar501l;
056500140522         if not %open(fiar501l);
056600140522            wlibfil=wlibfilprd;
056700140522            %subst(wlibfil:11:8)='FIAR501L';
056800140522            open fiar501l;
056900140522         endif;
057000140528         wlibfil=wlibfil201;
057100140528         %subst(wlibfil:11:8)='FIAR401L';
057200140528         open(e) fiar401l;
057300140528         if not %open(fiar401l);
057400140528            wlibfil=wlibfilprd;
057500140528            %subst(wlibfil:11:8)='FIAR401L';
057600140528            open fiar401l;
057700140528         endif;
057800140807         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
057900140807            open tivpi00f ;
058000140807         endif ;
058100140528
058200140528       // Reperimento G.LIMITE X GIAC.CON DATA RIC
058300140528       clear wlimitdcr;
058400140528       chain (1:'2G':'1       ') tabel00f;
058500140528       if %found(tabel00f);
058600140528          ds2g=tbluni;
058700140528          wlimitdcr=§2gldr;
058800140528       endif;
058900140603       // Reperimento dati tabella IVP - Variabili programma per INTERNET
059000140603       clear tibs02ds;
059100140603       clear divp;
059200140603       t02mod='C';
059300140603       t02cod='IVP';
059400140603       t02ke1='1  ';
059500140603       t02tla='L  ';
059600140603       tibs02r(kpjba:tibs02ds);
059700140603       if t02err=*blanks;
059800140603          divp=t02uni;
059900140603       endif;
060000140613       // Reperimento Giorni lavorativi entro cui ritirare le sped. in FD
060100140613       clear wlimitFD ;
060200140613       chain (1:'2A':'F       ') tabel00f;
060300140613       if %found(tabel00f);
060400140613          ds2a=tbluni;
060500140613          wlimitFD =§2agla;
060600140613       endif;
060700140924       // Reperimento dati tabella VPO - ORARISER
060800140924       clear tibs02ds;
060900140925       clear dvpooraris;
061000140924       t02mod='C';
061100140924       t02cod='VPO';
061200140925       t02ke1='ORARISER';
061300140924       t02tla='L  ';
061400140924       tibs02r(kpjba:tibs02ds);
061500140924       if t02err=*blanks;
061600140924          dvpooraris=t02uni;
061700140924       endif;
061800141007       // Reperimento dati tabella CSP - Caratteri speciali per eliminazione
061900141007      *                           caratteri di punteggiatura da loc.dest.
062000141007       clear tibs02ds;
062100141007       clear charnook  ;
062200141007       t02mod='C';
062300141007       t02cod='CSP';
062400141007       t02ke1='LOC';
062500141007       t02tla='L  ';
062600141007       tibs02r(kpjba:tibs02ds);
062700141007       if t02err=*blanks;
062800141007          charnook=t02uni;
062900141007       endif;
063000141007     c* imposto stringa contenente caratteri speciali per il controllo
063100141007     c* della parte finale (uguale alla charnook maa senza l'eventuale '.')
063200141007     c                   eval      charnookf=%xlate('.':' ':charnook)
063300140521      /end-free
063400140521     C     Kcet          KLIST
063500140521     C                   KFLD                    pctfgs
063600140521     C                   KFLD                    pctNDC
063700140521     C                   KFLD                    arbpdc
063800140521     C                   KFLD                    TTRD              3
063900140521     C                   KFLD                    arbAAS
064000140521     C                   KFLD                    arbLNP
064100140521     C                   KFLD                    arbNRS
064200140521     C                   KFLD                    arbNSP
064300140521      /free
064400131008
064500131008         *inLR = *on;
064600131008
064700131008       ENDSR;
064800131008
064900131008       //--------------------------------------------------------------
065000131008       //?Controllo formale dei dati passati.
065100131008       //--------------------------------------------------------------
065200131008       BEGSR  Controlla;
065300131008
065400140519         clear wksu;
065500140520         clear widbolla;
065600140520         clear wbrtcode;
065700140522         clear wchi    ;
065800140527         clear widlingua;
065900140527         clear wcampo;
066000140519
066100131008       //?OpCode
066200140519         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_GET_ISTRUCO  and
066300140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_PUT_ISTRUCO  and
066400140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
066500140519           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
066600140519           prmRpyIdMsg  = FNLRY00_ESITO_RQSOPCODE_SCONOSCIUTO;
066700140210           exsr RoutEnd;
066800131008         ENDIF;
066900140408
067000140519       //?per GET_ISTRUCO
067100140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
067200140408       //?Formato e size RQS
067300140519           IF  prmRqsFormato = fnlry00I0.formato;
067400140519             fnlry00I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
067500140519             wksu=fnlry00i0.ksu;
067600140520             widbolla=fnlry00i0.idbolla;
067700140520             wbrtcode=fnlry00i0.brtcode;
067800140527             widlingua=fnlry00i0.liniso2;
067900140408           ELSE;
068000140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
068100140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
068200140408             exsr RoutEnd;
068300140408           ENDIF;
068400140408           IF  prmRqsDataSize > 0;
068500140408           ELSE;
068600140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
068700140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
068800140408             exsr RoutEnd;
068900140408           ENDIF;
069000140408         //?Formato e size RPY
069100140519           IF  prmRpyFormato = fnlry00O0.formato;
069200140408           ELSE;
069300140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
069400140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
069500140408             exsr RoutEnd;
069600140408           ENDIF;
069700140408           IF  prmRpyDataSize > 0;
069800140408           ELSE;
069900140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
070000140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
070100140408             exsr RoutEnd;
070200140408           ENDIF;
070300140408         ENDIF;
070400140210
070500140522       //?per PUT_ISTRUCO
070600140522         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
070700140522       //?Formato e size RQS
070800140522           IF  prmRqsFormato = fnlry00I1.formato;
070900140522             fnlry00I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
071000140522             wksu=fnlry00i1.ksu;
071100140522             widbolla=fnlry00i1.idbolla;
071200140522             wbrtcode=fnlry00i1.brtcode;
071300140527             widlingua=fnlry00i1.liniso2;
071400140522           ELSE;
071500140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
071600140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
071700140522             exsr RoutEnd;
071800140522           ENDIF;
071900140522           IF  prmRqsDataSize > 0;
072000140522           ELSE;
072100140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
072200140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
072300140522             exsr RoutEnd;
072400140522           ENDIF;
072500140522         //?Formato e size RPY
072600140522           IF  prmRpyFormato = fnlry00O1.formato;
072700140522           ELSE;
072800140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
072900140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
073000140522             exsr RoutEnd;
073100140522           ENDIF;
073200140522           IF  prmRpyDataSize > 0;
073300140522           ELSE;
073400140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
073500140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
073600140522             exsr RoutEnd;
073700140522           ENDIF;
073800140522       //?Formato e size MSG
073900140522         IF  prmRpyFormMsg = FNLRY00M0.formato;
074000140522         ELSE;
074100140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
074200140522           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
074300140522           exsr RoutEnd;
074400140522         ENDIF;
074500140522         IF  prmRpyMsgSize > 0;
074600140522         ELSE;
074700140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
074800140522           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
074900140522           exsr RoutEnd;
075000140522         ENDIF;
075100140523       //?Tipo Elaborazione
075200140523         if fnlry00i1.tipoela<> '1' and fnlry00i1.tipoela<> '2';
075300140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
075400140523           prmRpyIdMsg = FNLRY00_ESITO_TIPOELA_ERRATO;
075500140523           exsr RoutEnd;
075600140523         endif;
075700140523       //?Tipo Disposizione
075800140523         if fnlry00i1.tipodispo <> '1' and
075900140523            fnlry00i1.tipodispo <> '2' and
076000140523            fnlry00i1.tipodispo <> '3' and
076100151112            fnlry00i1.tipodispo <> '4' and
076200151112            fnlry00i1.tipodispo <> '5'     ;
076300140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
076400140523           prmRpyIdMsg = FNLRY00_ESITO_TIPODISPO_ERRATO;
076500140523           exsr RoutEnd;
076600140523         endif;
076700140522         ENDIF;
076800140624
076900140624       //?per CHK_ISTRUCO
077000140624         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
077100140624       //?Formato e size RQS
077200140624           IF  prmRqsFormato = FNLRY00_RQSFORMATO_CHK_ISTRUCO;
077300140624             TIIDCDS = %SUBST(prmRqsData:1:prmRqsDataSize);
077400140624             wksu=idcidcli     ;
077500141223             widbolla=%subst(%editc(idcwaas:'X'):3:2) + %editc(idcwlnp:'X') +
077600141223                             %editc(idcwnrs:'X') + %editc(idcwnsp:'X');
077700140627             if wksu=*blanks;
077800140627             wbrtcode=widbolla;
077900140627             exsr sr_trul28;
078000140627             wbrtcode=o280cod ;
078100140627             else;
078200140624             clear wbrtcode;
078300140627             endif;
078400140624             widlingua='it';
078500140624           ELSE;
078600140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
078700140624             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
078800140624             exsr RoutEnd;
078900140624           ENDIF;
079000140624           IF  prmRqsDataSize > 0;
079100140624           ELSE;
079200140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
079300140624             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
079400140624             exsr RoutEnd;
079500140624           ENDIF;
079600140624       // per comodità valorizzo campi FNLRY00I1 prendendoli dal record del TIIDC
079700140624           fnlry00i1.TIPODISPO = IDCTIPODIS;
079800140624           fnlry00i1.NEWREFCONS= IDCREF;
079900140624           fnlry00i1.NEWTELCONS= IDCTELD;
080000140624           fnlry00i1.NEWDESMAIL= IDCMAILAVV;
080100140624           fnlry00i1.NEWDESCELL= IDCTELAVV ;
080200140923       // Se no dispo di indirizzo alternativo
080300140923           if fnlry00i1.tipodispo<>'3';
080400140923              if idcdcr>0;
080500140923                 fnlry00i1.NEWDTCRICH= %char(%date(IDCDCR:*iso):*eur);
080600140923              endif;
080700140923              fnlry00i1.NEWORCRICH= %editc(IDCHCR:'X') ;
080800140923              fnlry00i1.NEWTPCRICH= IDCTCR    ;
080900140923              fnlry00i1.PRCONSRIC = IDCPRCR   ;
081000140923           endif;
081100140624           fnlry00i1.NEWRSD    = IDCRSD    ;
081200140624           fnlry00i1.NEWIND    = IDCIND    ;
081300140624           fnlry00i1.NEWCAD    = IDCCAD    ;
081400140624           fnlry00i1.NEWLOD    = IDCLOD    ;
081500140624           fnlry00i1.NEWPRD    = IDCPRD    ;
081600140624           fnlry00i1.NEWNZD    = IDCNAZ    ;
081700140624           fnlry00i1.ALTRO     = IDCNOTE   ;
081800140923       // Se indirizzo alternativo
081900140923           if fnlry00i1.tipodispo='3';
082000140923              if idcdcr>0;
082100140923                 fnlry00i1.NEWDTCRICI=%char(%date(IDCDCR:*iso):*eur);
082200140923              endif;
082300140923                 fnlry00i1.NEWORCRICi= %editc(IDCHCR:'X') ;
082400140923                 fnlry00i1.NEWTPCRICi= IDCTCR    ;
082500140923                 fnlry00i1.PRCONSRICi= IDCPRCR   ;
082600140923           endif;
082700140624
082800140624         //?Formato e size RPY
082900140624       //?Formato e size MSG
083000140624         IF  prmRpyFormMsg = FNLRY00M0.formato;
083100140624         ELSE;
083200140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
083300140624           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
083400140624           exsr RoutEnd;
083500140624         ENDIF;
083600140624         IF  prmRpyMsgSize > 0;
083700140624         ELSE;
083800140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
083900140624           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
084000140624           exsr RoutEnd;
084100140624         ENDIF;
084200140624         ENDIF;
084300140522
084400140210
084500140210       //?Formato e size Forzature
084600140522       //IF  prmRpyFormForz = fNLRY00F0.formato;
084700140522       //ELSE;
084800140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
084900140522       //  prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
085000140522       //  exsr RoutEnd;
085100140522       //ENDIF;
085200140522       //IF  prmRpyForSize > 0;
085300140522       //ELSE;
085400140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
085500140522       //  prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
085600140522       //  exsr RoutEnd;
085700140522       //ENDIF;
085800140519
085900140520       //?devo ricevere ID cliente unificante o BRTCODE
086000140520         if wksu=*blanks and wbrtcode=*blanks;
086100140520           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
086200140520           prmRpyIdMsg = FNLRY00_ESITO_ID_RICHIAMO_ERRATO;
086300140520           exsr RoutEnd;
086400140520         endif;
086500140520       //?Accesso per cliente unificante e ID Bolla
086600140519         if wksu <>*blanks;
086700140520       // ID Bolla mancante
086800140520           if widbolla=*blanks;
086900140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
087000140527              wIdMsg   = 'TIS0734';
087100140527              wIdCampo = 'IDBOLLA   ';
087200140527              clear wParm;
087300140527              exsr Messaggi;
087400140520              exsr RoutEnd;
087500140520           endif;
087600141003       // KSU può contenere solo numeri
087700141003           if %check(digits:wksu)>0;
087800141003                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
087900141003                      wIdMsg   = 'TIS0736';
088000141003                      wIdCampo = 'KSU       ';
088100141003                      wParm = widbolla;
088200141003                      exsr Messaggi;
088300141003                      exsr RoutEnd;
088400141003           endif;
088500140520       // ID cliente unificante
088600140520           clear tibs10ds ;
088700140520           d10tle='WW'  ;
088800140520           d10paf='F'    ;     // verifico se è padre
088900140520           d10cod=%dec(wksu:8:0);
089000140520           TIBS10R (tibs10ds)   ;
089100140520  1        if d10err<>*blanks   ;
089200140520           // Se errore verifico se è figlio
089300140520              clear tibs10ds ;
089400140520              d10tle='WW'        ;
089500140520              d10paf='P'    ;
089600140520              d10cod=%dec(wksu:8:0);
089700140520              TIBS10R (tibs10ds)   ;
089800140520           endif;
089900140520
090000140520  2        if d10err<>*blanks   ;
090100140520             // Imposto nella skiera e come padre solo se stesso
090200140520             skc(1)=%editc(d10cod:'X');
090300140520             d10cop=d10cod      ;
090400140520  2        endif                ;
090500140520       // Verifico correttezza dell'ID bolla
090600140520       // Deve contenere solo numeri
090700140520         if %check(digits:widbolla)>0;
090800140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
090900140527              wIdMsg   = 'TIS0734';
091000140527              wIdCampo = 'IDBOLLA   ';
091100140527              clear wParm;
091200140527              exsr Messaggi;
091300140520              exsr RoutEnd;
091400140520         endif;
091500140529       // Se ricevuto anche il BRTCODE lo controllo
091600140529         if wbrtcode<>*blanks;
091700140529            exsr sr_trul28;
091800140529       //   Id bolla e brtcode devono riferirsi alla stessa spedizione
091900140529            if widbolla<>%subst(wbrtcode:1:14) ;
092000140529              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
092100140529              wIdMsg   = 'TIS0751';
092200140529              wIdCampo = 'IDBOLLA   ';
092300140530              wparm = wbrtcode + widbolla;
092400140529              exsr Messaggi;
092500140529              wIdCampo = 'BRTCODE   ';
092600140529              exsr Messaggi;
092700140529              exsr RoutEnd;
092800140529            endif;
092900140529         endif;
093000140521       // La bolla deve esistere su archivio bolle arrivi:
093100140521          wbolla=widbolla;
093200140527          wcampo='IDBOLLA';
093300140521          exsr sr_chainbolla;
093400140520       // Verifico che il cliente sia il mittente o il destinatario della bolla
093500140522             if arbccm>0;
093600140522                eval wcliente='0000'+%editc(arbccm:'X');
093700140522                if %lookup(wcliente:skc)=0;
093800140522                   eval wcliente='0000'+%editc(arbksc:'X');
093900140522                   if %lookup(wcliente:skc)=0;
094000140522                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
094100140527                      wIdMsg   = 'TIS0736';
094200140527                      wIdCampo = 'KSU       ';
094300140530                      wParm = widbolla;
094400140527                      exsr Messaggi;
094500140522                      exsr RoutEnd;
094600140522                   else;
094700140522                      wchi='D';
094800140522                   endif;
094900140522                else;
095000140522                   wchi='M';
095100140522                endif;
095200140522             else;
095300140522                eval wcliente='0000'+%editc(arbksc:'X');
095400140522                if %lookup(wcliente:skc)=0;
095500140522                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
095600140527                   wIdMsg   = 'TIS0736';
095700140527                   wIdCampo = 'KSU       ';
095800140530                   wParm = widbolla;
095900140527                   exsr Messaggi;
096000140522                   exsr RoutEnd;
096100140522                else;
096200140610                   exsr sr_ds3A;
096300140528                      if %subst(§3atb1:1:1)='F';
096400140522       //                se franco    --> mittente
096500140522                         wchi='M';
096600140522                      else;
096700140522       //                se Assegnato --> Destinatario
096800140522                         wchi='D';
096900140522                      endif;
097000140522                endif;
097100140522             endif;
097200140519         else;
097300140520       //?Accesso per BRTCode
097400140529          exsr sr_trul28;
097500140521       // La bolla deve esistere su archivio bolle arrivi:
097600140521          wbolla=%subst(wbrtcode:1:14);
097700140527          wcampo='BRTCODE';
097800140521          exsr sr_chainbolla;
097900140522          wchi='D';
098000140519         endif;
098100140620       //?Verifico se presenti legami bolla
098200140620         exsr sr_lbl;
098300131008
098400131008       ENDSR;
098500140529       //--------------------------------------------------------------
098600140529       //?Controllo correttezza BRTCODE
098700140529       //--------------------------------------------------------------
098800140529       BEGSR  sr_trul28 ;
098900140529          clear trul28ds0;
099000140529          i280cod=wbrtcode;
099100140529          trul28r0(trul28ds0);
099200140529          if o280err='1';
099300140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
099400140529             wIdMsg   = 'TIS0737';
099500140529             wIdCampo = 'BRTCODE   ';
099600140530             wparm = wbrtcode ;
099700140529             exsr Messaggi;
099800140529             exsr RoutEnd;
099900140529          endif;
100000140529       ENDSR;
100100140523       //--------------------------------------------------------------
100200140523       //?Controlli stato della bolla per abilitare le istruzioni di consegna
100300140523       //--------------------------------------------------------------
100400140523       BEGSR  statobolla;
100500140530          clear wtentacons;
100600140620          clear wdcreur;
100700140620          clear wrsd2  ;
100800140620
100900140620       // DETErMINO IL GESTORE DELLA LNA
101000140620         clear fnlv55ds;
101100140620         d55lin=arblna;
101200140620         d55tpt='6';
101300140620         d55drf=dataoggi;
101400140620         fnlv55r(fnlv55ds);
101500140620         wfgslna=d55tfA;
101600140620         clear wfgslnad;
101700140620         chain (wfgslna) azorg01l;
101800140620         if %found(azorg01l);
101900140620            wfgslnad=orgdes;
102000140620         endif;
102100140620
102200140620         clear dataeur;
102300140620         if arbdcr>0 ;
102400140620            dataeur=%date(arbdcr:*iso);
102500140620            wdcreur=%char(dataeur);
102600140620         endif;
102700140620
102800140620         clear wrbhcr ;
102900140620         if arbhcr>0 ;
103000140620            wrbhcr=%editc(arbhcr:'X');
103100140620         endif;
103200140620
103300140620       //wtprich=%subst(arbgc1:2:1);
103400140620       //if wtprich=*blanks;
103500140620       //    wtprich=%subst(arbgc2:2:1);
103600140620       //endif;
103700140620
103800140620            chain (arbaas:arblnp:arbnrs:arbnsp:'D') fiar401l ;
103900140620            if %found(fiar401l);
104000140620               eval wrsd2=%subst(ar4not:1:35);
104100140620            endif;
104200140620
104300140530       //?deve essere diretta in Italia
104400140530         clear og143;
104500140530         chain (arblna) azorg01l;
104600140530         if %found(azorg01l);
104700140530            og143=orgde3;
104800140530            if §ogntw = 'EEX' or §ogntw= 'FED' or §ogntw= 'DPD';
104900140530               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
105000140530               wIdMsg   = 'TIS0739';
105100140530               wIdCampo = '*BOLLA    ';
105200140530               wParm = wbolla;
105300140530               exsr Messaggi;
105400140530               exsr routend;
105500140530            endif;
105600140530         endif;
105700140523       //?Spedizione non deve essere consegnata
105800140523          if arbdcm>0;
105900141007            IF  prmRqsOpCode =  FNLRY00_RQSOPCODE_CHK_ISTRUCO;
106000141006                wIdMsg   = 'TISA738';
106100141006                wIdCampo = '*BOLLA    ';
106200141006                clear wParm ;
106300141006            else;
106400140527                wIdMsg   = 'TIS0738';
106500140527                wIdCampo = '*BOLLA    ';
106600140530                wParm = wbolla ;
106700141006            endif;
106800141006                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
106900140527                exsr Messaggi;
107000141006                exsr routend;
107100140523          else;
107200140523      /end-free
107300140523     c* prima vedo se c'e' da PDA
107400140708     c                   exsr      sr_RepPct
107500140523      /free
107600140523          endif;
107700140523       //?non deve avere giacenze o c.a. aperte
107800140620         chain (Arbaas:arblnp:arbnrs:arbnsp) tigcp21l;
107900140523         if %found(tigcp21l) and gcpatb=' ' and gcpdcg=0;
108000140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
108100141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
108200140527            wIdMsg   = 'TIS0740';
108300140527            wIdCampo = '*BOLLA    ';
108400140619            wParm = wbolla + wfgslnad;
108500141006           else;
108600141006            wIdMsg   = 'TISA740';
108700141006            wIdCampo = '*BOLLA    ';
108800141006            clear wParm ;
108900141006           endif;
109000140527            exsr Messaggi;
109100140523            exsr routend;
109200140523         endif;
109300140523         clear fidn12ds;
109400140620         i12aas=arbaas;
109500140620         i12lnp=arblnp;
109600140620         i12nrs=arbnrs;
109700140620         i12nsp=arbnsp;
109800140523         i12fel='E';
109900140523         i12fan='E';
110000140523         i12fch='E';
110100140523         fidn12r(fidn12ds);
110200140523         if o12nca>0;
110300140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
110400141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
110500140527            wIdMsg   = 'TIS0741';
110600140527            wIdCampo = '*BOLLA    ';
110700140530            wParm = wbolla;
110800141006           else;
110900141006            wIdMsg   = 'TISA741';
111000141006            wIdCampo = '*BOLLA    ';
111100141006            clear wParm ;
111200141006           endif;
111300140527            exsr Messaggi;
111400140523            exsr routend;
111500140523         endif;
111600140619       //?non deve essere bloccata
111700140917         if arbfbc<>*blanks and arbfbc<>'A';
111800140619            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
111900141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
112000140619            wIdMsg   = 'TIS0740';
112100140619            wIdCampo = '*BOLLA    ';
112200140619            wParm = wbolla + wfgslnad;
112300141006           else;
112400141006            wIdMsg   = 'TISA740';
112500141006            wIdCampo = '*BOLLA    ';
112600141006            clear wParm ;
112700141006           endif;
112800140619            exsr Messaggi;
112900140619            exsr routend;
113000140619         endif;
113100140910       //?non deve avere un codice mancata consegna di tipo giacenza
113200140911       //if arbcmc <> *blanks;
113300140911       //  clear ds2a;
113400140911       //  w008a = arbcmc;
113500140911       //  chain (1:'2A':w008a) tabel00f;
113600140911       //  if %found(tabel00f);
113700140911       //     ds2a=tbluni;
113800140911       //  endif;
113900140911       //  if §2aftc='G';
114000140911       //     prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
114100140911       //     wIdMsg   = 'TIS0740';
114200140911       //     wIdCampo = '*BOLLA    ';
114300140911       //     wParm = wbolla + wfgslnad;
114400140911       //     exsr Messaggi;
114500140911       //     exsr routend;
114600140911       //  endif;
114700140911       //endif;
114800140530       //?deve essere già stato fatto almeno un tentativo di consegna
114900140603         if §IVPTENC='S' and arbntc=0 and wtentacons=*blanks  ;
115000140530             wIdMsg   = 'TIS0752';
115100140530             wIdCampo = '*NOTENTAT ';
115200140530             wParm = wbolla;
115300140530             exsr Messaggi;
115400140530             exsr routend;
115500140530         endif;
115600141016       //?non deve essere una spedizione per i supermercati
115700141016         if arbtc1='S' or arbtc2='S'                          ;
115800141016             wIdMsg   = 'TIS0812';
115900141016             wIdCampo = '*BOLLA    ';
116000141016             clear wparm;
116100141016             exsr Messaggi;
116200141016             exsr routend;
116300141016         endif;
116400140604       //?Per la spedizione non devono esserci disposizioni ancora da elaborare
116500140624         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
116600140604          exsr sr_ChkDispo;
116700140624         endif;
116800140530       //?NON TROVATI ERRORI:
116900140530       //?Per GET_ISTRUCO --> attivo il bottone e restituisco i dati della bolla
117000140527         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
117100140527            fnlry00o0.BUTISTRCON='1';
117200140527       // restituisco anche dati relativi alla bolla
117300140527            exsr sr_GetDatiBolla;
117400140527            endif;
117500140523
117600140527       endsr;
117700140523       //--------------------------------------------------------------
117800140523       //?Controlli immagine bolla da WEB con bolla su ARB
117900140523       //--------------------------------------------------------------
118000140523       BEGSR  immagineB;
118100140523       // Chaino estensioni bolla
118200140529       // exsr sr_chainEstBolla;
118300140523       // Errore se il referente di consegna sulla bolla è diverso dall'immagine bolla
118400140523       // che ricevo
118500140523          if wrefbolla<>fnlry00i1.oldrefcons or
118600140523             wtelbolla<>fnlry00i1.oldtelcons or
118700140523             §ar5eml<>fnlry00i1.olddesmail or
118800140528             §ar5tel<>fnlry00i1.olddescell           ;
118900140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
119000140527               wIdMsg   = 'TIS0742';
119100140528               wIdCampo = '*CHGDATA  ';
119200140527               clear wParm;
119300140527               exsr Messaggi;
119400140523               exsr routend;
119500140523          endif;
119600140527         select;
119700140527       // Disposizioni di Appuntamento
119800140527         when fnlry00i1.tipodispo = '1';
119900140527          if wdcreur<>fnlry00i1.olddtcrich or
120000140529             wrbhcr<>fnlry00i1.oldorcrich  or
120100140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
120200140527               wIdMsg   = 'TIS0742';
120300140527               wIdCampo = '*CHGDATA  ';
120400140527               clear wParm;
120500140527               exsr Messaggi;
120600140527               exsr routend;
120700140527          endif;
120800140527       // Disposizioni di consegna ad altro indirizzo
120900140527         when fnlry00i1.tipodispo = '3';
121000140527          if arbrsd+wrsd2 <>fnlry00i1.oldrsd or
121100140527             arbind<>fnlry00i1.oldind or
121200140527             arbcad<>fnlry00i1.oldcad or
121300140527             arblod<>fnlry00i1.oldlod or
121400140527             arbprd<>fnlry00i1.oldprd or
121500140527             arbnzd<>fnlry00i1.oldnzd ;
121600140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
121700140527               wIdMsg   = 'TIS0742';
121800140527               wIdCampo = '*CHGDATA  ';
121900140527               clear wParm;
122000140527               exsr Messaggi;
122100140527               exsr routend;
122200140527          endif;
122300140527         endsl;
122400140523       ENDSR;
122500140523       //--------------------------------------------------------------
122600140523       //?Controllo dettaglio
122700140523       //--------------------------------------------------------------
122800140523       BEGSR  ControllaDett;
122900140523
123000140617       //?Appuntamento
123100140617       // (se non caricate date da proporre per GetIstruCo il bottone non deve
123200140617       //  essere attivato)
123300140609       if (prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO and fnlry00o0.nrdate>0)
123400140624                                                     or fnlry00i1.tipodispo='1';
123500140523          exsr sr_appuntam;
123600140523       endif;
123700140617       //?Fermo Deposito
123800140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='2';
123900140523          exsr sr_FD;
124000140523       endif;
124100140617       //?Altro Indirizzo
124200140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='3';
124300140523          exsr sr_AltroInd;
124400140523       endif;
124500140617       //?Altre Istruzioni
124600140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='4';
124700140523          exsr sr_AltreIstruz;
124800140523       endif;
124900140617       //?Per GETISTRUCO -->  se bottoni tutti in OFF metto in OFF anche bottone
125000140617       //?per le istruzioni di consegna
125100140616         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
125200140616            and fnlry00o0.BUTAPPUNT='0'
125300140616            and fnlry00o0.BUTFERDEP='0'
125400140616            and fnlry00o0.BUTindiriz='0'
125500140616            and fnlry00o0.BUTaltro='0';
125600140616            fnlry00o0.BUTISTRCON='0';
125700140617             wIdMsg   = 'TIS0770';
125800140617             wIdCampo =  '*BOLLA    ';
125900140617             wParm = wbolla;
126000140617             exsr Messaggi;
126100140617             exsr routend;
126200140616         endif;
126300140924       //?Per GETISTRUCO -->  se in ON  bottone per appuntam o altro indirizzo
126400140924       //?valorizzo campo messaggio per la preferenza di orario
126500140924         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
126600140924            and (fnlry00o0.BUTAPPUNT='1'  or fnlry00o0.BUTindiriz='1');
126700140930               wtempoh=%div((§VPORST_MW + §VPORST_PW):60);
126800140930               wtempom=%rem((§VPORST_MW + §VPORST_PW):60);
126900140924               wIdMsg   = 'TIS0781';
127000140929               wparm=      %editc(wtempoh:'Z');
127100140924               if wtempom>0;
127200140929                  wIdMsg   = 'TIS0784';
127300140929                  wparm=      wparm + %editc(wtempom:'Z') ;
127400140924               endif;
127500140924               fnlry00o0.MSGPRFORA=rtvMsgLang(wIdMsg:widlingua:wparm);
127600140924         endif;
127700140523
127800140523       endsr;
127900140408
128000140521       //--------------------------------------------------------------
128100140521       //?Chain FNARB
128200140521       //--------------------------------------------------------------
128300140521       BEGSR  sr_chainbolla;
128400140527
128500140521       // imposto la chiave di accesso
128600140521          kaas=2000+%dec(%subst(wbolla:1:2):2:0);
128700140521          klnp=%dec(%subst(wbolla:3:3):3:0);
128800140521          knrs=%dec(%subst(wbolla:6:2):2:0);
128900140521          knsp=%dec(%subst(wbolla:8:7):7:0);
129000140521          chain (kaas:klnp:knrs:knsp) fnarb01l;
129100140521          if not %found(fnarb01l);
129200140521              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
129300140527              wIdMsg   = 'TIS0735';
129400140527              wIdCampo = wcampo      ;
129500140530              wParm=wbolla;
129600140527              exsr Messaggi;
129700140521              exsr RoutEnd;
129800140521          endif;
129900141211       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
130000141211         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
130100141211           fnlry00i1.oldCAD    = arbCAD    ;
130200141211           fnlry00i1.oldLOD    = arbLOD    ;
130300141211           fnlry00i1.oldPRD    = arbPRD    ;
130400141211           fnlry00i1.oldNZD    = arbNzd    ;
130500141211         endif;
130600140521       endsr;
130700140620       //--------------------------------------------------------------
130800140620       //?Operazioni per bolle legate
130900140620       //--------------------------------------------------------------
131000140620       BEGSR  sr_lbl          ;
131100140620          chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl02l;
131200140620          if %found(fnlbl02l);
131300140624       // Per CHK_ISTRUCO --> errore se la bolla da elaborare ha una figlia
131400141223       //    IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
131500141223       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
131600141223       //             wIdMsg   = 'TIS0777';
131700141223       //             wIdCampo = '*NODISPO  ';
131800141223       //             wParm = wbolla;
131900141223       //             exsr Messaggi;
132000141223       //             exsr RoutEnd;
132100141223       //    endif;
132200140620       //    BOLLA CONSEGNATA
132300140620             if arbdcm>0;
132400140620       //       E' LA BOLLA ORIGINALE
132500140620                if LBLAAO=arbaas and LBLLPO=arblnp and
132600140620                   LBLNRO=arbnrs and LBLNSO=arbnsp;
132700140620       //       Non è il Mittente --> Errore: utente non abilitato a dare disposizioni
132800140620                   if wchi <>'M';
132900140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
133000140620                      wIdMsg   = 'TIS0736';
133100140620                      wIdCampo = '*NOAUT    ';
133200140620                      wParm = wbolla;
133300140620                      exsr Messaggi;
133400140620                      exsr RoutEnd;
133500140620                   endif;
133600140620                else;
133700140620       //       NON E' LA BOLLA ORIGINALE
133800140620       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
133900140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
134000140627                      wIdMsg   = 'TIS0770';
134100140620                      wIdCampo = '*NODISPO  ';
134200140620                      wParm = wbolla;
134300140620                      exsr Messaggi;
134400140620                      exsr RoutEnd;
134500140620                endif;
134600140620             endif;
134700140620       //    Recupero i dati dell'ultima figlia
134800140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
134900140620             dow %found(fnlbl02l);
135000140620                chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
135100140620             enddo;
135200140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnarb01l;
135300140620             if not %found(fnarb01l);
135400140627       //    Se non trovo l'ultima figlia su arb errore
135500140627       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
135600140627                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
135700140627                      wIdMsg   = 'TIS0777';
135800140627                      wIdCampo = '*NODISPO  ';
135900140627                      wParm = wbolla;
136000140627                      exsr Messaggi;
136100140627                      exsr RoutEnd;
136200140620             endif;
136300141223       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
136400141223         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
136500141223           fnlry00i1.oldCAD    = arbCAD    ;
136600141223           fnlry00i1.oldLOD    = arbLOD    ;
136700141223           fnlry00i1.oldPRD    = arbPRD    ;
136800141223           fnlry00i1.oldNZD    = arbNzd    ;
136900141223         endif;
137000140620          endif;
137100140620       endsr;
137200140523       //--------------------------------------------------------------
137300140523       //?Chain Estensioni bolla
137400140523       //--------------------------------------------------------------
137500140523       BEGSR  sr_chainEstBolla;
137600140523          clear dar5gen;
137700140523          clear wrefbolla;
137800140523          clear wtelbolla;
137900140620          chain (arbaas:arblnp:arbnrs:arbnsp:'GEN') fiar501l ;
138000140523          if %found(fiar501l);
138100140523             dar5gen=ar5uni;
138200140523       // Se presente # nel referente e nel telefono
138300140523       // lo devo togliere
138400140523             if %subst(§ar5ref:1:1)='#';
138500140523                eval wrefbolla=%subst(§ar5ref:2);
138600140523             else;
138700140523                eval wrefbolla=§ar5ref;
138800140523             endif;
138900140523             if %subst(§ar5teld:1:1)='#';
139000140523                eval wtelbolla=%subst(§ar5teld:2);
139100140523             else;
139200140523                eval wtelbolla=§ar5teld;
139300140523             endif;
139400140523          endif;
139500140523          clear dar5emd;
139600140620          chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
139700140523          if %found(fiar501l);
139800140523              dar5emd=ar5uni;
139900140523         endif;
140000140523       ENDSR;
140100140522       //--------------------------------------------------------------
140200140522       //?Recupera dati bolla da passare a chiamante
140300140522       //--------------------------------------------------------------
140400140522       BEGSR  sr_GetDatiBolla;
140500140603       // - orari da passare nella tendina: standard o minimo/massimo in base
140600140603       //   a quanto memorizzato in tabella
140700140923          clear waltroind;
140800140522          exsr sr_orarisr;
140900140604          if §ivptipo='M';
141000140604             fnlry00o0.ORARITNDI=%editc(OOR2MIIS:'X');
141100140604             fnlry00o0.ORARITNDF=%editc(OOR2MXFS:'X');
141200140604          else;
141300140604             fnlry00o0.ORARITNDI=%editc(OOR2STIS:'X');
141400140604             fnlry00o0.ORARITNDF=%editc(OOR2STFS:'X');
141500140604          endif;
141600140923       // - per la tendina con orari di filiale -->
141700140923         clear tisio9ds;
141800140923         do0lna=arblna;
141900140923         do0tric='S';
142000140923         do0tser='C';
142100140923         tisio9r (kpjba:tisio9ds);
142200140923          if §ivptipo='M';
142300140923             fnlry00o0.ORARITNFI=%editc(DO0OMIIS:'X');
142400140923             fnlry00o0.ORARITNFF=%editc(DO0OMXFS:'X');
142500140923          else;
142600140923             fnlry00o0.ORARITNFI=%editc(DO0OSTIS:'X');
142700140923             fnlry00o0.ORARITNFF=%editc(DO0OSTFS:'X');
142800140923          endif;
142900140603          fnlry00o0.MMINTERVC=%editc(§ivpmini:'X');
143000140613       // DATE DI CONSEGNA DA PROPORRE  -------------------
143100140605       // a partire da oggi vengono proposte le date fino al giorno limite
143200140605       // previsto per apertura giacenza
143300140605       // 1) verifico se previsti giorni personalizzati per cliente
143400140605         exsr sr_PersGGiac;
143500140709       // 2) Verifico se effetuato un tentativo di consegna ai fini dell'abilitazione
143600140709       //    della riconsegna
143700140709         IF §IVPARIC='S'AND OOR2FRIC='S';
143800140709            exsr sr_RepPct   ;
143900140709         ENDIF;
144000140709       // 3) Ciclo da oggi per numero di giorni per caricare campo delle date
144100140605       //    consegna richiesta da proporre
144200140605         wdata=dataoggi;
144300140605         clear ix;
144400140923         clear iy;
144500140605         for I=1 to wlimitdcr;
144600140606       //   se cade in un giorno festivo incremento la data di un giorno
144700140606            exsr sr_datafes;
144800140606       //   Propongo la data di oggi solo se possibile la riconsegna in giornata
144900140709            if (wdata=dataoggi and oor2fric='S'
145000140709                and §ivparic='S' and wtentacons='1') or wdata<>dataoggi;
145100140609       //   Memorizzo la data, purchè, in presenza di data cons.rich "Dopo il",
145200140609       //   la stessa non sia <= della data "Dopo Il"
145300140710             if arbtcr<>'D' or (arbtcr='D' and wdata>arbdcr);
145400140606               ix+=1;
145500140606               fnlry00o0.schdcr(ix)=%char(%date(wdata:*iso):*eur);
145600140923       //      la data per l'indirizzo alternativo non deve mai essere "oggi"
145700140923               if wdata<>dataoggi;
145800140923                  iy+=1;
145900140923                  fnlry00o0.schdcri(iy)=%char(%date(wdata:*iso):*eur);
146000140923                  fnlry00o0.nrdatei+=1;
146100140923               endif;
146200140606       //      Data da Consigliare --> quella successiva a oggi oppure
146300140606       //      quella della bolla se maggiore
146400140606               if fnlry00o0.dtconsric=*blanks  and wdata<>dataoggi ;
146500140606                   fnlry00o0.dtconsric=%char(%date(wdata:*iso):*eur);
146600140606                   if arbdcr>0 and arbtcr=*blanks and arbdcr>wdata;
146700140606                      fnlry00o0.dtconsric=%char(%date(arbdcr:*iso):*eur);
146800140606                   endif;
146900140606               endif;
147000140606       //   Incremento contatore date da passare a chiamante
147100140606               fnlry00o0.nrdate+=1;
147200140609             endif;
147300140606            endif;
147400140606       //   Incremento la data di un giorno
147500140606            exsr sr_IncreData;
147600140605         endfor;
147700140522       // - consegna richiesta: data/ora/tipo
147800140606       //if arbdcr>0 ;
147900140606       //   fnlry00o0.DTCONSRIC=wdcreur;
148000140606       //endif;
148100140606       //if arbhcr>0;
148200140606       //   fnlry00o0.ORCONSRIC=%editc(arbhcr:'X');
148300140606       //endif;
148400140606       //fnlry00o0.TPCONSRIC=arbtcr;
148500140613       // indirizzo e-mail e n. telefono per SMS -----------
148600140620         chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
148700140522         if %found(fiar501l);
148800140522             dar5emd=ar5uni;
148900140522             fnlry00o0.destmail=§ar5eml;
149000140522             fnlry00o0.destcell=§ar5tel;
149100140522         endif;
149200140613       // Messaggio di avviso per indicare che il FD deve essere autorizzato dal mitt. ------------
149300140522         if arbffd=*blanks and arbgma<>*blanks;
149400140529            exsr sr_tab7r;
149500140522            if §7rfd='S' and wchi='D';
149600140522               wIdMsg   = 'TIS0733';
149700140617               fnlry00o0.MSGFDMITT=rtvMsgLang(wIdMsg:widlingua);
149800140522            endif;
149900140522         endif;
150000140522       endsr;
150100140606       //--------------------------------------------------------------
150200140606       //?Controllo data se festiva + restituzione data prima data non festiva
150300140606       //--------------------------------------------------------------
150400140606       BEGSR  sr_DataFes;
150500140606       wfes='1';
150600140606       dow wfes='1';
150700140606       chain (0:wfgslna:%subdt(%date(wdata):*years):
150800140606                        %subdt(%date(wdata):*MONTHS)) azcln01l;
150900140606          if %found(azcln01l) and mat(%subdt(%date(wdata):*days)) = 'F'
151000140606                               or pom(%subdt(%date(wdata):*days)) = 'F';
151100140606       // data festiva: la incremento di un giorno
151200140606             exsr sr_incredata;
151300140606          else;
151400140606            clear wfes;
151500140606          endif;
151600140606       enddo;
151700140606       endsr;
151800140606       //--------------------------------------------------------------
151900140606       //?Incremento data di un giorno
152000140606       //--------------------------------------------------------------
152100140606       BEGSR  sr_incredata;
152200140606             dataiso=%date(wdata);
152300140606             dataiso=dataiso+%days(1);
152400140606             wdata=%dec(dataiso);
152500140606       endsr;
152600140521       //--------------------------------------------------------------
152700140521       //?Controlli per abilitare la richiesta di appuntamento
152800140521       //--------------------------------------------------------------
152900140521       BEGSR  sr_appuntam;
153000140528
153100140528       // Non ammesso per spedizioni in fermo deposito
153200140528         if arbffd= 'S';
153300140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
153400140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
153500140528               leavesr;
153600140528            else;
153700140528       //?PUT_ISTRUCO --> uscita da pgm con errore
153800140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
153900141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
154000140528              wIdMsg   = 'TIS0743';
154100140528              wIdCampo = '*NOAPPUNT ';
154200140530              wParm = wbolla ;
154300141006         else;
154400141006              wIdMsg   = 'TISA743';
154500141006              wIdCampo = '*NOAPPUNT ';
154600141006              clear wParm  ;
154700141006         endif;
154800140528              exsr Messaggi;
154900140528              exsr routend;
155000140528            endif;
155100140528         endif;
155200140528
155300140528       //?GET_ISTRUCO --> attivazione bottone per appuntamento
155400140528         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
155500140528            fnlry00o0.BUTAPPUNT='1';
155600140528         else;
155700140528       //?PUT_ISTRUCO --> controllo dati ricevuti
155800140528            exsr sr_ctrApp;
155900140528         endif;
156000140528
156100140521       endsr;
156200140522       //--------------------------------------------------------------
156300140522       //?Controlli per abilitare la richiesta di Fermo Deposito
156400140522       //--------------------------------------------------------------
156500140522       BEGSR  sr_FD      ;
156600140522
156700140528       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
156800140528          if arbffd = 'S';
156900140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
157000140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
157100140528               leavesr;
157200140528            else;
157300140528       //?PUT_ISTRUCO --> uscita da pgm con errore
157400141223            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
157500140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
157600141223       //if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
157700140528              wIdMsg   = 'TIS0743';
157800140528              wIdCampo = '*NOFD     ';
157900140530              wParm = wbolla;
158000141223       //else;
158100141223       //     wIdMsg   = 'TISA743';
158200141223       //     wIdCampo = '*NOFD     ';
158300141223       //     clear wParm;
158400141223       //endif;
158500140528              exsr Messaggi;
158600140528              exsr routend;
158700141223            endif;
158800140528            endif;
158900140528          else;
159000140530       //?GET_ISTRUCO -->
159100141008            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
159200141008       //   and fnlry00o0.MSGFDMITT=*blanks;
159300140616       //?GET_ISTRUCO -->> Restituisco, insieme al bottone attivato anche messaggio
159400140616       //?                 riguardante le codizioni di ritiro in FD
159500140616       //?                 Solo se msgfdmit=*blanks
159600140616             if fnlry00o0.msgfdmitt=*blanks;
159700141008                fnlry00o0.BUTFERDEP='1';
159800140924                exsr sr_wdesgio;
159900140617               wparm=wdesgio + %editc(WlimitFD:'X') + wfgslnad;
160000140616               wIdMsg   = 'TIS0768';
160100140617               fnlry00o0.MSGFDCOND=rtvMsgLang(wIdMsg:widlingua:wparm);
160200141008             else;
160300141008                fnlry00o0.BUTFERDEP='2';
160400140616             endif;
160500140616            endif;
160600140606       //?PUT_ISTRUCO --> Controllo dati ricevuti
160700140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
160800140624               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
160900140616       //?                Errore al destinatario se è richiesta autorizzaz.
161000140616       //?                da parte del mittente
161100140616             if arbgma<>*blanks;
161200140616                exsr sr_tab7r;
161300140616                if §7rfd='S' and wchi='D';
161400140616                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
161500140616                   wIdMsg   = 'TIS0733';
161600140616                   wIdCampo = '*NOFD     ';
161700140616                   clear wParm;
161800140616                   exsr Messaggi;
161900140616                   exsr routend;
162000140616                endif;
162100140616             endif;
162200140606             exsr sr_ctrUNI;
162300140606            endif;
162400140530          endif;
162500140522       endsr;
162600140522       //--------------------------------------------------------------
162700140522       //?Controlli per abilitare la richiesta di Altro Indirizzo
162800140522       //--------------------------------------------------------------
162900140522       BEGSR  sr_altroind;
163000140522
163100140616       if §ivpdind='S';
163200140617       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
163300140617          if arbffd = 'S';
163400140617       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
163500140617            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
163600140617               leavesr;
163700140617            else;
163800140617       //?PUT_ISTRUCO --> uscita da pgm con errore
163900140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
164000141007           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
164100140617              wIdMsg   = 'TIS0743';
164200140617              wIdCampo = '*NOALTRIND';
164300140617              wParm = wbolla;
164400141007           else;
164500141007              wIdMsg   = 'TISA743';
164600141007              wIdCampo = '*NOALTRIND';
164700141007              clear wparm ;
164800141007           endif;
164900140617              exsr Messaggi;
165000140617              exsr routend;
165100140617            endif;
165200140617          endif;
165300140619       //NON AMMESSO AL DESTINATARIO SE NON ABILITATO MEDIANTE TABELLA 7W
165400140619         clear ds7W;
165500140619         if wchi='D' and arbgga<>*blanks;
165600140619            w008a=ARBGGA;
165700140619            chain (1 : '7W' : W008A) tabel00f;
165800140619            if %found(tabel00f);
165900140619               ds7w=tbluni;
166000140619            endif;
166100140619         endif;
166200140619         if wchi='D' and §7wdes<>'S';
166300140619       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
166400140918       //?                18/09/2014 --> attivo il bottone ma poi verrà dato errore
166500140918       //?                Il concetto è: il cambio di indirizzo è gestito (e quindi il
166600140918       //?                bottone deve essere attivo) ma non può essere richiesto da
166700140918       //?                destinatario se non autorizzato (e quindi poi viene dato errore)
166800140918            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
166900141007               fnlry00o0.BUTindiriz='2';
167000141007               wIdMsg   = 'TIS0796';
167100141007               fnlry00o0.MSGAUTIND=rtvMsgLang(wIdMsg:widlingua);
167200141007               leavesr;
167300140619            else;
167400140619       //?PUT_ISTRUCO --> uscita da pgm con errore
167500140619              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
167600140619              wIdMsg   = 'TIS0776';
167700140619              wIdCampo = '*NOALTRIND';
167800140924              clear wparm;
167900140924       //     wParm = wbolla;
168000140619              exsr Messaggi;
168100140619              exsr routend;
168200140619            endif;
168300140619         endif;
168400140522         fnlry00o0.BUTindiriz='1';
168500140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
168600140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
168700140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
168800140611       //?Controllo dati comuni a tutti tipi di disposizione
168900140611             exsr sr_ctrUNI;
169000140611
169100140609             exsr sr_ctrind;
169200140609            endif;
169300140616       else;
169400140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
169500140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
169600141016               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
169700141016                 prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
169800141016                 prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
169900141016                 exsr RoutEnd;
170000140616            endif;
170100140616       endif;
170200140522       endsr;
170300140522       //--------------------------------------------------------------
170400140522       //?Controlli per abilitare la richiesta di Altre Istruzioni
170500140522       //--------------------------------------------------------------
170600140522       BEGSR  sr_AltreIstruz;
170700140522
170800140616       if §ivpdalr='S';
170900140522         fnlry00o0.BUTaltro='1';
171000140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
171100140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
171200140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
171300140609             exsr sr_ctrUNI;
171400140609            endif;
171500140616       else;
171600140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
171700140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
171800140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
171900140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
172000140617              prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
172100140617              exsr RoutEnd;
172200140616            endif;
172300140616       endif;
172400140522       endsr;
172500140522
172600140523       //--------------------------------------------------------------
172700140523       //?Controllo Disposizioni di Appuntamento
172800140523       //--------------------------------------------------------------
172900140523       BEGSR  sr_ctrApp     ;
173000140611       //?Controllo dati comuni a tutti tipi di disposizione
173100140611          exsr sr_ctrUNI;
173200140528       //?Consegna richiesta
173300140528          exsr sr_ctrConsRich;
173400140523       endsr;
173500140528       //--------------------------------------------------------------
173600140528       //?Controllo Data/ora/tipo consegna richiesta
173700140528       //--------------------------------------------------------------
173800140528       BEGSR  sr_CtrConsRich;
173900140923          clear waltroind;
174000140606          exsr sr_Orarisr;
174100140528       // DATA CONSEGNA RICHIESTA:
174200140528          exsr sr_ctrDCR;
174300140528       // ORA  CONSEGNA RICHIESTA:
174400140528       if fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
174500140528       // Deve contenere solo numeri
174600140528         if %check(digits:fnlry00i1.neworcrich)>0;
174700140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174800140528              wIdMsg   = 'TIS0746';
174900140528              wIdCampo = 'NEWORCRICH';
175000140530              wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
175100140530                      %subst(fnlry00i1.neworcrich:3:2);
175200140528              exsr Messaggi;
175300140528         else;
175400140709       // Se data consegna richiesta diversa da oggi (no riconsegna)
175500140709       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
175600140528              wora = %dec(fnlry00i1.neworcrich:4:0);
175700141216              if wdcr<>wdataconfr;
175800140709                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
175900140709                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
176000140709                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
176100140709                       wIdMsg   = 'TIS0747';
176200140709                       wIdCampo = 'NEWORCRICH';
176300140709                       wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
176400140709                       %subst(fnlry00i1.neworcrich:3:2);
176500140709                       exsr Messaggi;
176600140709                 endif;
176700140709              else;
176800140709       //  Se Data Consegna Richiesta=oggi (riconsegna) l'orario deve essere incluso negli orari
176900140610       //  inizio/fine previsti per la riconsegna
177000140709                 if  wora < OOR2RIDS  or
177100140709                     wora > OOR2RIAS ;
177200140709                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
177300140709                        wIdMsg   = 'TIS0778';
177400140709                        wIdCampo = 'NEWORCRICH';
177500140709                        wParm = %editw(oor2rids:'  :  ') +
177600140709                                %editW(oor2rias:'  :  ')  ;
177700140709                        exsr Messaggi;
177800140709                 endif;
177900140709              endif;
178000140528         endif;
178100140528       endif;
178200140529       // PREFERENZA MATTINO/POMERIGGIO
178300140529       if fnlry00i1.prconsric<>*blanks and
178400140529          fnlry00i1.prconsric<>'M' and
178500140529          fnlry00i1.prconsric<>'P'               ;
178600140528                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
178700140528                    wIdMsg   = 'TIS0749';
178800140529                    wIdCampo = 'PRCONSRIC ';
178900140528                    clear wParm;
179000140528                    exsr Messaggi;
179100140528       endif;
179200140710       // NO RICONSEGNA:
179300140710       // Verifico preferenza m/p con gli orari standard o min/max in base
179400140710       // all'ora limite del mattino
179500141216       if wdcr<>wdataconfr;
179600140710         if (fnlry00i1.prconsric='P' and §ivptipo='S' and §ivporlm>=OOR2STFS) or
179700140710            (fnlry00i1.prconsric='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
179800140710            (fnlry00i1.prconsric='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS) or
179900140710            (fnlry00i1.prconsric='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
180000140710            If fnlry00i1.prconsric='P';
180100140710                widmsg='TIS0676';
180200140710            else;
180300140710                widmsg='TIS0667';
180400140710            endif;
180500140710            matpom=rtvMsgLang(widmsg : widlingua);
180600140710                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
180700141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
180800140710                      wIdMsg   = 'TIS0760';
180900140710                      wIdCampo = 'PRCONSRIC ';
181000140710                      wParm = MatPom + wbolla;
181100141006         else;
181200141006                      wIdMsg   = 'TISA760';
181300141006                      wIdCampo = 'PRCONSRIC ';
181400141006                      wParm = MatPom ;
181500141006         endif;
181600140710                      exsr Messaggi;
181700140710         ENDIF;
181800140710       ELSE;
181900140710       // SI RICONSEGNA:
182000140710       // Verifico preferenza m/p con gli orari previsti per la riconsegna
182100140710       // in base all'ora limite del mattino
182200140710          if (fnlry00i1.prconsric='P' and §ivporlm>=OOR2rias) or
182300140710             (fnlry00i1.prconsric='M' and §ivporlm<OOR2rids);
182400140710             If fnlry00i1.prconsric='P';
182500140710                 widmsg='TIS0676';
182600140710             else;
182700140710                 widmsg='TIS0667';
182800140710             endif;
182900140710             matpom=rtvMsgLang(widmsg : widlingua);
183000140710                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
183100140710                       wIdMsg   = 'TIS0779';
183200140710                       wIdCampo = 'PRCONSRIC ';
183300140710                       wParm = MatPom ;
183400140710                       exsr Messaggi;
183500140710          ENDIF;
183600140606       endif;
183700140529       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
183800140529       if fnlry00i1.prconsric<>*blanks and
183900140529          fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
184000140529                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
184100140530                    wIdMsg   = 'TIS0753';
184200140530                    wIdCampo = 'NEWORCRICH';
184300140530                    clear wParm;
184400140529                    exsr Messaggi;
184500140530                    wIdCampo = 'PRCONSRIC ';
184600140529                    exsr Messaggi;
184700140529       endif;
184800140528       //  Se Campi tutti vuoti errore
184900140529       //if (fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros) and
185000140529       //   (fnlry00i1.neworcrich=*blanks or fnlry00i1.neworcrich=*zeros) and
185100140529       //    fnlry00i1.prconsric=*blanks;
185200140529       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
185300140529       //             wIdMsg   = 'TIS0748';
185400140529       //             wIdCampo = '*NOSELEZ';
185500140529       //             clear wParm;
185600140529       //             exsr Messaggi;
185700140529       //endif;
185800140528       endsr;
185900140528       //--------------------------------------------------------------
186000140528       //?Controllo Data Consegna richiesta
186100140528       //--------------------------------------------------------------
186200140528       BEGSR  sr_ctrDCR     ;
186300140528       // DATA CONSEGNA RICHIESTA:
186400140529       // Obbligatoria
186500140529       if fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros;
186600140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
186700140529             wIdMsg   = 'TIS0130';
186800140529             wIdCampo = 'NEWDTCRICH';
186900140529             clear wParm;
187000140529             exsr Messaggi;
187100140529             leavesr;
187200140529       endif;
187300140528       //  Controllo correttezza formale
1874001405282        monitor  ;
187500140528         dataiso=%date(fnlry00i1.newdtcrich:  *eur)   ;
187600140528         on-error ;
187700140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
187800140528             wIdMsg   = 'TIS0121';
187900140528             wIdCampo = 'NEWDTCRICH';
188000140528             clear wParm;
188100140528             exsr Messaggi;
188200140528             leavesr;
188300140528 2       endmon  ;
188400140528
188500140528       wdcr=%dec(dataiso);
188600140528
188700140528       //  Non inferiore a oggi
188800140528       if wdcr < dataoggi;
188900140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
189000140528             wIdMsg   = 'TIS0744';
189100140528             wIdCampo = 'NEWDTCRICH';
189200140611             wParm=%char(%date(wdcr:*iso):*eur);
189300140528             exsr Messaggi;
189400140528             leavesr;
189500140528       endif;
189600140709       //  Uguale ad oggi solo se possibile la riconsegna in giornata
189700141215       //  Per chkistruco considero "data oggi" la data in cui è stata
189800141215       //  fatta la  richiesta
189900141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
190000141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
190100141215         else;
190200141218              wdataconfr=dataoggi;
190300141215         endif;
190400141215       if wdcr=wdataconfr;
190500140709         if oor2fric='S' and §ivparic='S';
190600140709          exsr sr_reppct;
190700140709         endif;
190800140709         if oor2fric='N' or §ivparic<>'S' or wtentacons=' ';
190900140606             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
191000140606             wIdMsg   = 'TIS0759';
191100140606             wIdCampo = 'NEWDTCRICH';
191200140606             clear wparm;
191300140606             exsr Messaggi;
191400140606             leavesr;
191500140709         endif;
191600140606       endif;
191700140528       //  Non superiore ai Giorni limite giac. con data cons.rich.
191800140528       //      Verifico se presenti limiti personalizzati per il cliente
191900140528
192000140605       exsr sr_PersGGiac;
192100140528       //Calcolo giorni lavorativi fra data odierna e data consegna richiesta
192200141215       datada=wdataconfr;
192300140528       dataa=wdcr;
192400140606       clear tfptfa;
192500140610       p_tfa =wfgslna;
192600140606       xsrlav8(wdat8:tfptfa);
192700140528       if ggl > wlimitdcr;
192800140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
192900140528             wIdMsg   = 'TIS0745';
193000140528             wIdCampo = 'NEWDTCRICH';
193100140611             wParm=%char(%date(wdcr:*iso):*eur);
193200140528             exsr Messaggi;
193300140528             leavesr;
193400140528       endif;
193500140606       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
193600140528       //  Controllo anche con la data consegna richiesta originale
193700140609       if (arbtcr='D' and wdcr<=arbdcr) or
193800140609         (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
193900140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
194000140529             wIdMsg   = 'TIS0750';
194100140528             wIdCampo = 'NEWDTCRICH';
194200140606             if arbtcr='D' ;
194300140606             dataeur=%date(%dec(arbdcr:8:0):*iso);
194400140606             else;
194500140606             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
194600140606             endif;
194700140611             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
194800140528             exsr Messaggi;
194900140528             leavesr;
195000140528       endif;
195100140605       // Non deve cadere in giorno festivo
195200140605       chain (0:wfgslna:%subdt(%date(wdcr):*years):
195300140605                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
195400140605       if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
195500140605                            or pom(%subdt(%date(wdcr):*days)) = 'F';
195600140605             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
195700140605             wIdMsg   = 'TIS0758';
195800140605             wIdCampo = 'NEWDTCRICH';
195900140611             wParm=%char(%date(wdcr:*iso):*eur);
196000140605             exsr Messaggi;
196100140605             leavesr;
196200140605       endif;
196300140528
196400140528       endsr;
196500140605       //--------------------------------------------------------------
196600140605       //?Rperimento eventuale personalizzazione giorni attesa prima di apert.giac
196700140605       //--------------------------------------------------------------
196800140605       BEGSR  sr_PersGGiac  ;
196900140606       // Faccio come fa fnlg80r --> prima prova on arbksc
197000140606       // e se non trova con questo prova poi con arbccm
197100140606       clear ds7u;
197200140606       w008a=%editc(arbksc:'X')+'Q';
197300140606       chain (1:'7U':w008a) tabel00f;
197400140606       if %found (tabel00f);
197500140606          ds7u=tbluni;
197600140606       else;
197700140606          w008a=%editc(arbCCM:'X')+'Q';
197800140606          chain (1:'7U':w008a) tabel00f;
197900140606          if %found (tabel00f);
198000140606             ds7u=tbluni;
198100140606          endif;
198200140606       endif;
198300140606       if §7uldcr>'00';
198400140606          wlimitdcr=%dec(§7uldcr:2:0);
198500140606       endif;
198600140605       // Cerco con il ccm se presente altrimenti con KSC
198700140606       // clear ds7u;
198800140606       //if arbccm > 0;
198900140606       //   w008a=%editc(arbCCM:'X')+'Q';
199000140606       //   chain (1:'7U':w008a) tabel00f;
199100140606       //   if %found (tabel00f);
199200140606       //      ds7u=tbluni;
199300140606       //   endif;
199400140606       //else;
199500140606       //   w008a=%editc(arbKSC:'X')+'Q';
199600140606       //   chain (1:'7U':w008a) tabel00f;
199700140606       //   if %found (tabel00f);
199800140606       //      ds7u=tbluni;
199900140606       //   endif;
200000140606       //endif;
200100140606       //if §7uldcr>'00';
200200140606       //   wlimitdcr=%dec(§7uldcr:2:0);
200300140606       //endif;
200400140605       endsr;
200500140609       //--------------------------------------------------------------
200600140609       //?Controlli Disposizioni di consegna ad altro indirizzo
200700140609       //--------------------------------------------------------------
200800140609       BEGSR  sr_ctrind;
200900141006          clear wolna;
201000140610       // Per il controllo dell'indirizzo (CAP escluso) non viene richiamato
201100140610       // fnlv14r (quest'ultimo, se cap vuoto richiama l'interrogazione
201200140609       // del cappario e controlla la nazione che qui non serve)
201300140609       //?Ragione sociale, Indirizzo, Località e Provincia --> obbligatori
201400140610          if fnlry00i1.newrsd = *blanks;
201500140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
201600140610             wIdMsg   = 'TIS0761';
201700140610             wIdCampo = 'NEWRSD    ';
201800140610             clear wParm;
201900140610             exsr Messaggi;
202000141006             leavesr;
202100140610          endif;
202200140610          if fnlry00i1.newind = *blanks;
202300140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
202400140610             wIdMsg   = 'TIS0762';
202500140610             wIdCampo = 'NEWIND    ';
202600140610             clear wParm;
202700140610             exsr Messaggi;
202800141006             leavesr;
202900140610          endif;
203000140610          if fnlry00i1.newcad = *blanks;
203100140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
203200140610             wIdMsg   = 'TIS0763';
203300140610             wIdCampo = 'NEWCAD    ';
203400140610             clear wParm;
203500140610             exsr Messaggi;
203600141006             leavesr;
203700140610          endif;
203800140610          if fnlry00i1.newlod = *blanks;
203900140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
204000140610             wIdMsg   = 'TIS0764';
204100140610             wIdCampo = 'NEWLOD    ';
204200140610             clear wParm;
204300140610             exsr Messaggi;
204400141006             leavesr;
204500140610          endif;
204600140610          if fnlry00i1.newprd = *blanks;
204700140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
204800140610             wIdMsg   = 'TIS0765';
204900140610             wIdCampo = 'NEWPRD    ';
205000140610             clear wParm;
205100140610             exsr Messaggi;
205200141006             leavesr;
205300140609          endif;
205400140610       //?La nazione deve essere vuota
205500140610          if fnlry00i1.newnzd <> *blanks;
205600140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
205700140610             wIdMsg   = 'TIS0384';
205800140610             wIdCampo = 'NEWNZD    ';
205900140610             clear wParm;
206000140610             exsr Messaggi;
206100141006             leavesr;
206200140610          endif;
206300140611       //?Richiamo pgm per controllo CAP e calcolo instradamento
206400140610          if fnlry00i1.newcad<>fnlry00i1.oldcad or
206500140610             fnlry00i1.newlod<>fnlry00i1.oldlod or
206600140610             fnlry00i1.newprd<>fnlry00i1.oldprd or
206700140610             fnlry00i1.newnzd<>fnlry00i1.oldnzd         ;
206800140611             exsr sr_call_fispe02r;
206900141003          else;
207000141003             if fnlry00i1.newrsd=fnlry00i1.oldrsd and
207100141003                fnlry00i1.newind=fnlry00i1.oldind;
207200141003       // Errore se non eseguita alcuna variazione di indirizzo
207300141003                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
207400141003                wIdMsg   = 'TIS0794';
207500141003                wIdCampo = '*NOCHGIND ';
207600141003                clear wParm;
207700141003                exsr Messaggi;
207800141006                leavesr;
207900141003             endif;
208000140610       endif;
208100140930
208200140930         waltroind='1';
208300140930         exsr sr_orarisr ;
208400140930
208500141016       //?SE NO DIROTTAMENTO
208600141016       //if wolna = 0 ;
208700140923       //?DATA/ORA CONSEGNA RICHIESTA
208800140923       if fnlry00i1.newdtcrici<>*blanks and fnlry00i1.newdtcrici<>*zeros;
208900141006       // Se richiesto dirottamento non si può inserire la consegna richiesta
209000141016         if  wolna>0;
209100141016                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
209200141016                wIdMsg   = 'TIS0795';
209300141016                wIdCampo = 'NEWDTCRICI';
209400141016                clear wParm;
209500141016                exsr Messaggi;
209600141016                leavesr;
209700141016         endif;
209800140923       // DATA CONSEGNA RICHIESTA:
209900140923       //  Controllo correttezza formale
210000140923         monitor  ;
210100140923         dataiso=%date(fnlry00i1.newdtcrici:  *eur)   ;
210200140923         on-error ;
210300140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
210400140923             wIdMsg   = 'TIS0121';
210500140923             wIdCampo = 'NEWDTCRICI';
210600140923             clear wParm;
210700140923             exsr Messaggi;
210800140923             leavesr;
210900140923         endmon  ;
211000140923
211100140923         wdcr=%dec(dataiso);
211200141215       //  Per chkistruco considero "data oggi" la data in cui è stata
211300141215       //  fatta la  richiesta
211400141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
211500141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
211600141215         else;
211700141218              wdataconfr=dataoggi;
211800141215         endif;
211900140923
212000140923       //  Non inferiore o uguale a oggi
212100141215         if wdcr <= wdataconfr;
212200140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
212300140929             wIdMsg   = 'TIS0759';
212400140923             wIdCampo = 'NEWDTCRICI';
212500140923             clear wparm;
212600140923             exsr Messaggi;
212700140923             leavesr;
212800140923         endif;
212900140923       //  Non superiore ai Giorni limite giac. con data cons.rich.
213000140923       //      Verifico se presenti limiti personalizzati per il cliente
213100140923
213200140923         exsr sr_PersGGiac;
213300140923       //Calcolo giorni lavorativi fra data odierna e data consegna richiesta
213400141215         datada=wdataconfr;
213500140923         dataa=wdcr;
213600140923         clear tfptfa;
213700140923         p_tfa =wfgslna;
213800140923         xsrlav8(wdat8:tfptfa);
213900140923         if ggl > wlimitdcr;
214000140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
214100140923             wIdMsg   = 'TIS0745';
214200140923             wIdCampo = 'NEWDTCRICI';
214300140923             wParm=%char(%date(wdcr:*iso):*eur);
214400140923             exsr Messaggi;
214500140923             leavesr;
214600140923         endif;
214700140923       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
214800140923       //  Controllo anche con la data consegna richiesta originale
214900140923         if (arbtcr='D' and wdcr<=arbdcr) or
215000140923           (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
215100140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
215200140923             wIdMsg   = 'TIS0750';
215300140923             wIdCampo = 'NEWDTCRICI';
215400140923             if arbtcr='D' ;
215500140923             dataeur=%date(%dec(arbdcr:8:0):*iso);
215600140923             else;
215700140923             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
215800140923             endif;
215900140923             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
216000140923             exsr Messaggi;
216100140923             leavesr;
216200140923         endif;
216300140923       // Non deve cadere in giorno festivo
216400140923         chain (0:wfgslna:%subdt(%date(wdcr):*years):
216500140923                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
216600140923         if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
216700140923                            or pom(%subdt(%date(wdcr):*days)) = 'F';
216800140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
216900140923             wIdMsg   = 'TIS0758';
217000140923             wIdCampo = 'NEWDTCRICI';
217100140923             wParm=%char(%date(wdcr:*iso):*eur);
217200140923             exsr Messaggi;
217300140923             leavesr;
217400140923         endif;
217500140923       // ORA CONSEGNA RICHIESTA:
217600140923         if fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
217700140923       // Deve contenere solo numeri
217800140923           if %check(digits:fnlry00i1.neworcrici)>0;
217900140923                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
218000140923                wIdMsg   = 'TIS0746';
218100140923                wIdCampo = 'NEWORCRICH';
218200140923                wParm = %subst(fnlry00i1.neworcrici:1:2) + ':' +
218300140923                        %subst(fnlry00i1.neworcrici:3:2);
218400140923                exsr Messaggi;
218500140923           else;
218600140923       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
218700140923                wora = %dec(fnlry00i1.neworcrici:4:0);
218800140923                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
218900140923                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
219000140923                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
219100140923                       wIdMsg   = 'TIS0780';
219200140923                       wIdCampo = 'NEWORCRICI';
219300140925       //              wParm = fnlry00i1.newlod;
219400140923                       if §ivptipo='S';
219500140925                          wParm = fnlry00i1.newlod +
219600140925                                  %editw(oor2stis:'  :  ') +
219700140925                                  %editw(oor2stfs:'  :  ') ;
219800140923                       else;
219900140925                          wParm = fnlry00i1.newlod +
220000140925                                  %editw(oor2miis:'  :  ') +
220100140925                                 %editw(oor2mxfs:'  :  ') ;
220200140923                       endif;
220300140923                       exsr Messaggi;
220400140923                 endif;
220500140923           endif;
220600140923         endif;
220700140923       // PREFERENZA MATTINO/POMERIGGIO
220800140923         if fnlry00i1.prconsrici<>*blanks and
220900140923            fnlry00i1.prconsrici<>'M' and
221000140923            fnlry00i1.prconsrici<>'P'               ;
221100140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
221200140923                    wIdMsg   = 'TIS0749';
221300140923                    wIdCampo = 'PRCONSRICI';
221400140923                    clear wParm;
221500140923                    exsr Messaggi;
221600140923         endif;
221700140923       // Verifico preferenza m/p con gli orari standard o min/max in base
221800140923       // all'ora limite del mattino
221900140923         if (fnlry00i1.prconsrici='P' and §ivptipo='S' and §ivporlm>=OOR2STFS)or
222000140923            (fnlry00i1.prconsrici='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
222100140923            (fnlry00i1.prconsrici='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS)or
222200140923            (fnlry00i1.prconsrici='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
222300140923            If fnlry00i1.prconsricI='P';
222400140923                widmsg='TIS0676';
222500140923            else;
222600140923                widmsg='TIS0667';
222700140923            endif;
222800140923            matpom=rtvMsgLang(widmsg : widlingua);
222900140923                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
223000141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
223100140923                      wIdMsg   = 'TIS0760';
223200140923                      wIdCampo = 'PRCONSRICI';
223300140923                      wParm = MatPom + wbolla;
223400141006         else;
223500141006                      wIdMsg   = 'TISA760';
223600141006                      wIdCampo = 'PRCONSRICI';
223700141006                      wParm = MatPom ;
223800141006         endif;
223900140923                      exsr Messaggi;
224000140923         ENDIF;
224100140923       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
224200140923         if fnlry00i1.prconsrici<>*blanks and
224300140923            fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
224400140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
224500140923                    wIdMsg   = 'TIS0753';
224600140923                    wIdCampo = 'NEWORCRICI';
224700140923                    clear wParm;
224800140923                    exsr Messaggi;
224900140923                    wIdCampo = 'PRCONSRICI';
225000140923                    exsr Messaggi;
225100140923         endif;
225200141001         else;
225300141001       // se non immessa la data consegna richiesta errore se immessa l'ora o la preferenza M/P
225400141001           if fnlry00i1.prconsrici<>*blanks ;
225500141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
225600141016              if  wolna>0;
225700141016                        wIdMsg   = 'TIS0795';
225800141016              else;
225900141001                        wIdMsg   = 'TIS0782';
226000141016              endif;
226100141001                        wIdCampo = 'PRCONSRICI';
226200141001                        clear wParm;
226300141001                        exsr Messaggi;
226400141001           endif;
226500141001           if  fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros ;
226600141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
226700141016              if  wolna>0;
226800141016                        wIdMsg   = 'TIS0795';
226900141016              else;
227000141001                        wIdMsg   = 'TIS0782';
227100141016              endif;
227200141001                        wIdCampo = 'NEWORCRICI';
227300141001                        clear wParm;
227400141001                        exsr Messaggi;
227500141001           endif;
227600141001         endif;
227700141016       //else;
227800141016       //?SE DIROTTAMENTO pulisco data/ora preferenza
227900141016       //   clear fnlry00i1.newdtcrici ;
228000141016       //   clear fnlry00i1.neworcrici ;
228100141016       //   clear fnlry00i1.prconsrici ;
228200141016       //endif;
228300140609       endsr;
228400140611       //--------------------------------------------------------------
228500140611       //?Richiamo stored procedure per controllo e calcolo instradamento
228600140611       //--------------------------------------------------------------
228700140611       BEGSR sr_Call_FISPE02R;
228800141006
228900141006
229000140611         clear  oTFA;
229100140611         clear  oLNA;
229200140611         clear  oZNC;
229300140611         clear  oLNPD;
229400140611         clear  oLNAD;
229500140611         clear  oERR;
229600140611         clear  oMSG;
229700140611         clear  oTAG;
229800140611         clear  RTNesito;
229900140611         clear  RTNopcode;
230000140611         clear  RTNstatus;
230100140611         iTYPE='C';
230200140611         iLANG=Widlingua;
230300140611         iDATA=dataoggi;
230400140611         clear iNTW ;
230500140611         iLNP=arblnp;
230600140611         iNZD=fnlry00i1.newnzd;
230700140611         iPRD=fnlry00i1.newprd;
230800140611         iCAD=fnlry00i1.newcad;
230900140611         iLOD=fnlry00i1.newlod;
231000140611         iIND=fnlry00i1.newind;
231100140611         iRSD=fnlry00i1.newrsd;
231200140611         iNCL=arbncl;
231300140611         iPKB=arbpkb;
231400140611         iVLB=arbvlb;
231500140611         iFFD=arbffd;
231600140611         iTSP=arbtsp;
231700140611         iTC1=arbtc1;
231800140611         iTC2=arbtc2;
231900140611         iCBO=arbcbo;
232000140611         fispE02r ( iTYPE :
232100140611                    iLANG :
232200140611                    iDATA :
232300140611                    iNTW :
232400140611                    iLNP :
232500140611                    iNZD :
232600140611                    iPRD :
232700140611                    iCAD :
232800140611                    iLOD :
232900140611                    iIND :
233000140611                    iRSD :
233100140611                    iNCL :
233200140611                    iPKB :
233300140611                    iVLB :
233400140611                    iFFD :
233500140611                    iTSP :
233600140611                    iTC1 :
233700140611                    iTC2 :
233800140611                    iCBO :
233900140611                    oTFA :
234000140611                    oLNA :
234100140611                    oZNC :
234200140611                    oLNPD :
234300140611                    oLNAD :
234400140611                    oERR :
234500140611                    oMSG :
234600140611                    oTAG :
234700140611                    RTNesito :
234800140611                    RTNopcode :
234900140611                    RTNstatus );
235000140925       //if RTNesito<0;
235100140925       //ELSe;
235200140611          if oERR<>*blanks;
235300140611             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
235400140611             clear wIdMsg;
235500140611             wTextMsg=oMSG;
235600140611             select;
235700140611             when  %scan('Provincia':otag)>0;
235800140611                wIdCampo = 'NEWPRD    ';
235900140611             when  %scan('Localita':otag)>0;
236000140611                wIdCampo = 'NEWLOD    ';
236100140611             other;
236200140611                wIdCampo = 'NEWCAD    ';
236300140611             endsl;
236400140611             clear wParm;
236500140611             exsr Messaggi;
236600140611       // Se non ci sono errori verifico che la linea di arrivo calcolata sia la
236700141006       // stessa della bolla --> se dirottamento non possono selezionare la data/ora
236800140611          else;
236900140611             if arblna<>olna;
237000141006       //       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
237100141006       //       wIdMsg   = 'TIS0767';
237200141006       //       wIdCampo = '*CHGLNA   ';
237300141006       //       clear wParm;
237400141006       //       exsr Messaggi;
237500141006                Wolna=olna;
237600140611             endif;
237700140611          endif;
237800140925       //endif;
237900140611       endsr;
238000140610       //--------------------------------------------------------------
238100140610       //?Controllo Dati comuni a tutti i tipi di disposizioni
238200140610       //--------------------------------------------------------------
238300140610       BEGSR  sr_ds3A       ;
238400140610                   clear kkey;
238500140610                   clear ds3a;
238600140610                   %subst(kkey:1:2)=arbcbo;
238700140610                   chain (1:'3A':kkey) tabel00f ;
238800140610                   if %found(tabel00f);
238900140610                      ds3a=tbluni;
239000140610                   endif;
239100140610       endsr;
239200140523       //--------------------------------------------------------------
239300140523       //?Controllo Dati comuni a tutti i tipi di disposizioni
239400140523       //--------------------------------------------------------------
239500140523       BEGSR  sr_ctrUNI     ;
239600140609       //?Referente Consegna - obbligatorio
239700140530       if fnlry00i1.newrefcons=  *blanks;
239800140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
239900140530             wIdMsg   = 'TIS0754';
240000140530             wIdCampo = 'NEWREFCONS';
240100140530             clear wParm;
240200140530             exsr Messaggi;
240300140530       endif;
240400140530       //?Telefono Destinatario - obbligatorio
240500140530       if fnlry00i1.newtelcons=  *blanks;
240600140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
240700140530             wIdMsg   = 'TIS0755';
240800140530             wIdCampo = 'NEWTELCONS';
240900140530             clear wParm;
241000140530             exsr Messaggi;
241100140530       endif;
241200140523       //?Indirizzo e-mail
241300140528       if fnlry00i1.newdesmail<> *blanks;
241400140528          clear dsemail                        ;
241500140528          §emlindi = fnlry00i1.newdesmail;
241600140528          xemail (dsemail);
241700140528          if §emlerro <> '0';
241800140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
241900140528             wIdMsg   = 'TIS0732';
242000140528             wIdCampo = 'NEWDESMAIL';
242100140528             clear wParm;
242200140528             exsr Messaggi;
242300140528          endif;
242400140528       endif;
242500140523       //?N.Telefono per SMS
242600140528       if fnlry00i1.newdescell<> *blanks;
242700140528          pInCell = %trim(fnlry00i1.newdescell);
242800140528          pOutErr = *blanks;
242900140528          if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
243000140528           // Numero di cellulare KO
243100140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
243200140528             wIdMsg   = 'TIS0725';
243300140528             wIdCampo = 'NEWDESCELL';
243400140528             clear wParm;
243500140528             exsr Messaggi;
243600140528          endif;
243700140528       endif;
243800140523       endsr;
243900140529       //--------------------------------------------------------------
244000140529       //?Routine per recuperare flag di autorizzazione al Fermo Deposito
244100140529       //--------------------------------------------------------------
244200140529       BEGSR  sr_tab7r;
244300140529            clear kkey;
244400140529            clear ds7r;
244500140529            %subst(kkey:1:2)=arbgma;
244600140529            chain (1:'7R':kkey) tabel00f ;
244700140529            if %found(tabel00f);
244800140529               ds7r=tbluni;
244900140529            endif;
245000140529       endsr;
245100140528      /end-free
245200140521      **************************************************************************
245300140528     c     sr_OrariSr    BEGSR
245400140521
245500140521     c                   clear                   trulorSds
245600140522      /free
245700140522              // reperisco gli orari di consegna previsti
245800140522              clear tnsd99ds;
245900140522              d98tbo='A';
246000140522              d98aas=arbaas;
246100140522              d98lnp=arblnp;
246200140522              d98nrs=arbnrs;
246300140522              d98nsp=arbnsp;
246400140522              if arbtsp='D';
246500140522                 I98TSP_FOR='C';
246600140522              endif;
246700140522              tnsd99r(tnsd99ds);
246800140522              if d98err='0';
246900140522                IOREDTA = d98dci;
247000140522              endif;
247100140522      /end-free
247200140521     c                   eval       IOREtfp = ARBtfp
247300140521     c                   eval       IOREtfa = ARBtfa
247400150107     c                   if         arbdti>0
247500140521     c                   eval       IOREdti = ARBdti
247600150107     c                   else
247700150107     c* Caso di consegna parziale:
247800150107     c* Visto che vogliamo prendere gli orari "tutto il giorno", se dti vuoto non lo
247900150107     c* prenderebbe mai e qunidi metto ARBDAM
248000150107     c                   eval       IOREdti = ARBdam
248100150107     c                   endif
248200140528     c* imposto a 0 hti per recuperare sempre gli orari ' '=Tutto il giorno
248300140522     c****               eval       IOREhti = ARBhti
248400140521     c                   eval       IOREdcr = ARBdcr
248500140521     c                   eval       IOREhcr = ARBhcr
248600140521     c                   eval       IOREtcr = ARBtcr
248700140522     c                   eval       IOREDEI = d98dei
248800140522     c                   eval       IOREOEI = d98oei
248900150729
249000150729     c                   clear                   diore01
249100150729     c                   eval       §IORETRAZC=%editc(d98ttc:'X')
249200150729     c                   eval       §IORECONSC=%editc(d98tcc:'X')
249300150729     c                   eval       ioreflo=diore01
249400150729
249500140521     c                   eval       IOREfil = ARBlna
249600140923     c                   if         waltroind=' '
249700140521     c                   eval       IOREcap = ARBcad
249800140521     c                   eval       IOREloc = ARBlod
249900140521     c                   eval       IOREnar = ARBnzd
250000140923     c                   else
250100140925     c                   eval       IOREcap = fnlry00i1.newcad
250200140925     c                   eval       IOREloc = fnlry00i1.newlod
250300140925     c                   eval       IOREnar = fnlry00i1.newnzd
250400140923     c                   endif
250500140521     c
250600140521     c                   eval       IOREtser = 'C'
250700140521     c                   eval       IOREDDC=arbddc
250800140521     c                   eval       IOREnDC=arbndc
250900140521     c
251000140521     c
251100140521     c                   eval       IOREtsp = ARBtsp
251200140618     c                   if          ARBfbc = *blanks and
251300140618     c                               ARBddc > 0 and
251400140618     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
251500140618     c                               arbica='R' or arbicc=' ' or arbicc='R')
251600140618     c                   eval       IOREcons = 'S'
251700140618     c                   endif
251800140521     c
251900140521
252000140521     c                   call      'TRULORSR'
252100140521     c                   parm                    kpjba
252200140521     c                   parm                    trulorSds
252300140521     c                   parm                    trulor2ds
252400140521     c
252500140521     c                   ENDSR
252600140521      /free
252700140924       //--------------------------------------------------------------
252800140924       //?Routine per determinare il giorno della settimana di dataoggi + 1gg lav.
252900140924       //--------------------------------------------------------------
253000140924       BEGSR  sr_wdesgio ;
253100140924       //       Determino il giorno della settimana di dataoggi+1giorno lavorativo
253200140924       //       da restituire nel messaggio
253300140924               wdata=dataoggi;
253400140924       //       Incremento la data di un giorno
253500140924               exsr sr_incredata;
253600140924       //       Verifico se data festiva e in tal caso incremento finchè non trovo giorno
253700140924       //       lavorativo
253800140924               exsr sr_datafes;
253900140924       //       Determino il giorno della settimana della data calcolata
254000140924               dataiso=%date(wdata:*iso);
254100140924               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
254200140924               wdesgio=rtvMsgLang(dayOfWeek.msgId(dw) : widlingua);
254300140924       endsr;
254400140603       //--------------------------------------------------------------
254500140603       //?Routine per scrittura disposizioni di consegna
254600140603       //--------------------------------------------------------------
254700140924       BEGSR  SR_Tiidc;
254800140603       // Per la spedizione non devono esserci disposizioni ancora da elaborare
254900140603       exsr sr_ChkDispo;
255000140603       clear tiidc000;
255100141013       clear tiidcds ;
255200140718       clear tivpi000;
255300140604       exsr repnum;
255400140604       if esito = *blanks;
255500140604          IDCPRG=kprg                         ;
255600140604       else;
255700140619       // prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
255800140619  //   // wIdMsg   = 'TIS0757';
255900140619  //   // wIdCampo = '*REPNUM   ';
256000140619  //   // clear wParm;
256100140619  //   // exsr Messaggi;
256200140616          prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
256300140616          prmRpyIdMsg  = FNLRY00_ESITO_NUMERATORE_DISPO_NONREPERITO;
256400140604          exsr RoutEnd;
256500140604       endif;
256600141009       //WDCINSDATA = %dec(fnlry00i1.ksutimest);
256700141009       //IDCINSDATA = %dec(%subst(%editc(wdcinsdata:'X'):1:14):14:0);
256800141009       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
256900141009                                    : 1 : 14 ) );
257000141009       IDCtimestn   = fnlry00i1.ksutimestn;
257100140603       //IDCMODACC                           ;
257200140610       if wksu<>*blanks;
257300140610       IDCMODACC='L'                       ;
257400140610       else;
257500140610       endif;
257600140603       IDCIP = fnlry00I1.cltipaddr;
257700140603       IDCIDCLI = fnlry00i1.ksu;
257800140603       IDCTIPOCLI= Wchi;
257900140603       IDCWAAS   = kaas;
258000140603       IDCWLNP   = klnp;
258100140603       IDCWNRS   = knrs;
258200140603       IDCWNSP   = knsp;
258300140620       IDCAAS    = arbaas;
258400140620       IDCLNP    = arblnp;
258500140620       IDCNRS    = arbnrs;
258600140620       IDCNSP    = arbnsp;
258700140603       IDCFGS    = wfgslna;
258800140603       IDCTIPODIS = fnlry00i1.tipodispo;
258900140604       IDCMAILAVV = fnlry00i1.newdesmail;
259000140604       IDCTELAVV  = fnlry00i1.newdescell;
259100140603       //IDCALERT
259200140604       if fnlry00i1.newdtcrich <> *blanks and fnlry00i1.tipodispo='1';
259300140604           IDCDCR =  %dec(%date(fnlry00i1.newdtcrich:*eur):*iso);
259400140604       endif;
259500140604       if fnlry00i1.neworcrich <> *blanks and fnlry00i1.tipodispo='1';
259600140604           IDCHCR =  %dec(fnlry00i1.neworcrich:4:0);
259700140604       endif;
259800140924       if fnlry00i1.newdtcrici <> *blanks and fnlry00i1.tipodispo='3';
259900140924           IDCDCR =  %dec(%date(fnlry00i1.newdtcrici:*eur):*iso);
260000140924       endif;
260100140924       if fnlry00i1.neworcrici <> *blanks and fnlry00i1.tipodispo='3';
260200140924           IDCHCR =  %dec(fnlry00i1.neworcrici:4:0);
260300140924       endif;
260400140603       //IDCTCR                         ;
260500140924       if fnlry00i1.tipodispo='1';
260600140924          IDCPRCR=  fnlry00i1.prconsric  ;
260700140924       endif;
260800140924       if fnlry00i1.tipodispo='3';
260900140924          IDCPRCR=  fnlry00i1.prconsrici ;
261000140924       endif;
261100140603       IDCREF  = fnlry00i1.newrefcons ;
261200140603       IDCTELD = fnlry00i1.newtelcons ;
261300140603       if fnlry00i1.tipodispo='2';
261400140603          IDCFFD  = 'S';
261500140603       endif;
261600140611       if fnlry00i1.tipodispo='3';
261700141007          wtxt     = fnlry00i1.newrsd;
261800141007          uppercase='1';
261900141007          exsr sr_chk;
262000141007          IDCRSD   = wtxt;
262100141007          wtxt     = fnlry00i1.newind;
262200141007          uppercase='1';
262300141007          exsr sr_chk;
262400141007          IDCIND   = wtxt            ;
262500140611          IDCCAD   = fnlry00i1.newcad;
262600141007          wtxt     = fnlry00i1.newlod;
262700141007          uppercase='1';
262800141007          exsr sr_chk;
262900141007          exsr chkstringa;
263000141007          IDCLOD   = wtxt            ;
263100140611          IDCPRD   = fnlry00i1.newprd;
263200140611          IDCNAZ   = fnlry00i1.newnzd;
263300141006       // bolla da dirottare
263400141006          if wolna>0;
263500141006               IDCNEWLNA=wolna;
263600141006               IDCFLA   ='S';
263700141006          endif;
263800140611       endif;
263900140611       if fnlry00i1.tipodispo='4';
264000140611          IDCNOTE  = fnlry00i1.altro;
264100140611       endif;
264200140603       IDCELAFLG='S';
264300140603
264400140718       VPITIP='TI';
264500140718       VPIISV='TT';
264600140718       // indicazioni ricevute da cliente loggato
264700140718       if wksu<>*blanks;
264800140801          VPISUN=%editc(fnlry00i1.sun:'X');
264900140718          VPIKSU=wksu    ;
265000140718       else;
265100140718       // indicazioni ricevute mediante BRTCODE
265200140718          VPISUN='8IDC00000';
265300140718          VPIKSU='0IDC0000';
265400140718       endif;
265500140718       VPIDTA=tiidcds ;
265600140718       write tivpi000;
265700140814       // se AS888 scrivo anche tiidc
265800140814
265900140814      /end-free
266000140814     c* controllo se sono in AS888
266100141006     c                   eval      comman=chklib(1)
266200141006     c                   eval      lengh=35
266300141006     c                   call      'QCMDEXC'                            99
266400141006     c                   parm                    comman           80
266500141006     c                   parm                    lengh            15 5
266600140814     c
266700141006     c                   if        not *in99
266800140814      /free
266900140814
267000141013         write tiidc000;
267100141006         endif  ;
267200140603
267300140603       endsr;
267400140603       //--------------------------------------------------------------
267500140603       //?Verifica presenza di diposizioni ancora da elaborare
267600140603       //--------------------------------------------------------------
267700140603       BEGSR  SR_ChkDispo;
267800140908       clear w0140;
267900140908       setll (arbaas:arblnp:arbnrs:arbnsp:w0140) tiidc01l;
268000140603       if %equal(tiidc01l);
268100140603              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
268200140603              wIdMsg   = 'TIS0756';
268300140603              wIdCampo = '*TIIDC    ';
268400140611              wParm = wbolla;
268500140603              exsr Messaggi;
268600140603              exsr Routend;
268700140603       endif;
268800140718       // verifico anche che non siano ancora su tivpi
268900140801       eval wkeyspa=%editc(arbaas:'X')+%editc(arblnp:'X')+
269000140801                    %editc(arbnrs:'X')+%editc(arbnsp:'X')  ;
269100140801      /end-free
269200140718     C/EXEC SQL
269300140718     C+ SELECT count(*) INTO :totale FROM tivpi00f where
269400140718     c+ VPITIP='TI' and VPIISV='TT' and
269500141002     C+ substr(vpidta, 67, 16) = :wkeyspa
269600140718     C/END-EXEC
269700140801      /free
269800140718          if totale>0 ;
269900140718              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
270000140718              wIdMsg   = 'TIS0756';
270100140718              wIdCampo = '*TIVPI    ';
270200140718              wParm = wbolla;
270300140718              exsr Messaggi;
270400140718              exsr Routend;
270500140718       endif;
270600140603       endsr;
270700140604        //-----------------------------------------------------------------------*
270800140604        // Reperimento progressivo richiesta da numeratore 650
270900140604        //-----------------------------------------------------------------------*
271000140604        begsr RepNum;
271100140604
271200140604          clear esito;
271300140604          $finenum=*off;
271400140604          clear trul33ds;
271500140604
271600140604          I33TLa = 'L';
271700140604          I33Ope = *ZEROS;
271800140604          I33CNu = 650;
271900140604          I33Num = 1;
272000140604
272100140604          KPJBU = TRUL33DS;
272200140604
272300140604          dow $finenum=*off   ;
272400140604             Trul33R(kpjba);
272500140604
272600140604             TRUL33DS = KPJBU;
272700140604
272800140604             if O33Err <> *zeros;
272900140604               Esito = '2';
273000140604               $finenum=*on;
273100140604             else;
273200140908               w0090=o33nri;
273300140908               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
273400140604               setll kprg tiidc02l;
273500140604        // Se numero già presente nel file TIIDC ne cerco un altro
273600140604               if not %equal;
273700140604                  $finenum=*on;
273800140604               endif;
273900140604             endif;
274000140604          enddo;
274100140604
274200140604          endsr;
274300140604
274400140708       //--------------------------------------------------------------
274500140708       //?Routine per schierare i messaggi nella ds FNLRY00M0
274600140708       //--------------------------------------------------------------
274700140708       BEGSR  sr_RepPct;
274800140708
274900140708       clear wtentacons;
275000140708
275100140708      /end-free
275200140708     c                   move      arbndc        pctndc
275300140708     c                   move      arbifp        pctfgs
275400140708     c                   move      'CET'         ttrd
275500140708     c     kcet          setgt     fipct02l
275600140708     c     kcet          readpe    fipct02l
275700140708     c                   if        not %eof(fipct02l)
275800140708     c                   movel     pctdati       fipctcetds
275900140708     c* consegnata
276000140708     c                   if        §pctcmc = ' '
276100140708     c                   eval      prmRpyOpCode=FNLRY00_RPYOPCODE_ERROR
276200141006     c                   if        prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO
276300141006     c                   eval      wIdMsg   = 'TISA738'
276400141006     c                   eval      wIdCampo = '*BOLLA    '
276500141006     c                   clear                   wbolla
276600141006     c                   else
276700140708     c                   eval      wIdMsg   = 'TIS0738'
276800140708     c                   eval      wIdCampo = '*BOLLA    '
276900140708     c                   eval      wParm = wbolla
277000141006     c                   endif
277100140708     c                   exsr      Messaggi
277200140708     c                   exsr      routend
277300140708     c                   else
277400140708       //?deve essere già stato fatto almeno un tentativo di consegna
277500140708     c                   eval      wtentacons='1'
277600140708     c                   endif
277700140708     c                   endif
277800140708      /free
277900140708       endsr;
278000140929       //--------------------------------------------------------------
278100140929       //?Routine per tradurre mattino/pomeriggio
278200140929       //--------------------------------------------------------------
278300140929       BEGSR  sr_matpom;
278400140929
278500140929            If wconsric='P';
278600140929                widmsg='TIS0676';
278700140929            else;
278800140929                widmsg='TIS0667';
278900140929            endif;
279000140929            matpom=rtvMsgLang(widmsg : widlingua);
279100140929       endsr;
279200140604
279300140527       //--------------------------------------------------------------
279400140527       //?Routine per schierare i messaggi nella ds FNLRY00M0
279500140527       //--------------------------------------------------------------
279600140527       BEGSR  Messaggi;
279700140527
279800140527       //?Se ho già caricato 99 messaggi esco
279900140527         IF  fnlry00M0.nrmsg = 99;
280000140527           exsr RoutEnd;
280100140527           clear fnlry00M0.nrmsg;
280200140527         ENDIF;
280300140527
280400140527         fnlry00M0.nrmsg += 1;
280500140527         fnlry00M0.skIdMsg(fnlry00M0.nrmsg) = wIdMsg;
280600140527         fnlry00M0.skIdCampo(fnlry00M0.nrmsg) = wIdCampo;
280700140527         fnlry00M0.skErrWarn(fnlry00M0.nrmsg) = FNLRY00_MSG_ERRORE;
280800140527         IF  wParm <> *blanks;
280900140527           fnlry00M0.skTextMsg (fnlry00M0.nrmsg) =
281000140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
281100140527         ELSE;
281200140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
281300140611           if wIdMsg=*blanks;
281400140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=wTextMsg;
281500140611           else;
281600140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=
281700140611             rtvMsgLang(wIdMsg:wIdlingua);
281800140611           endif;
281900140527         ENDIF;
282000140527
282100140527       ENDSR;
282200140527
282300140521       //--------------------------------------------------------------
282400140408       //?Carico tabelle.
282500140408       //--------------------------------------------------------------
282600140408       BEGSR  CaricaTab;
282700140408
282800140408
282900140408       ENDSR;
283000131010
283100131010       //--------------------------------------------------------------
283200131010       //?Operazioni finali.
283300131010       //--------------------------------------------------------------
283400131010       BEGSR  RoutEnd;
283500140408
283600140519       //?Se richiamato per GET_ISTRUCO
283700140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
283800140519           %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o0;
283900140408         ELSE;
284000131010
284100140522       //?Se richiamato per PUT_ISTRUCO
284200140522          %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o1;
284300140521       //%subst(prmRpyForzatu:1:prmRpyForSize) = fnlry0F0;
284400140408
284500140408         ENDIF;
284600140530
284700140530          %subst(prmRpyMessage:1:prmRpyMsgSize) = fnlrY00M0;
284800131010
284900131010         return;
285000131010
285100131010       ENDSR;
285200131009
285300131007      /end-free
285400141007      *---------------------------------------------------------
285500141007      * Controllo i caratteri dei campi alfanumerici
285600141007      *---------------------------------------------------------
285700141007     c     Sr_Chk        BegSr
285800141007
285900141007     c                   clear                   esito
286000141007     c                   Eval      TxtInOut = wtxt
286100141007     c                   Call      'XCHKCHAR'
286200141007     c                   Parm                    TxtInOut
286300141007     c                   Parm                    ElencoChar
286400141007     c                   Parm                    TipoElenco
286500141007     c                   Parm                    CharSost
286600141007     c                   Parm                    UpperCase
286700141007     c                   Parm                    ChkNull
286800141007     c                   Parm                    CharNull
286900141007     c                   Parm                    Esito
287000141007     c                   If        Esito = '1'
287100141007     c                   Move      TxtInOut      wtxt
287200141007     c                   EndIf
287300141007
287400141007     c                   EndSr
287500141007      *---------------------------------------------------------
287600141007      * Controllo caratteri di punteggiatura precedenti e seguenti
287700141007      *---------------------------------------------------------
287800141007     c     chkstringa    BegSr
287900141007
288000141007     c* trovo la prima posizione "buona" da utilizzare
288100141007     c                   eval      posl=%check(charnook:wtxt)
288200141007     c* trovo l'ultima posizione "buona" da utilizzare (escludo il ".")
288300141007     c                   eval      posr=%checkr(charnookf:wtxt)
288400141007     c                   if        posl=0
288500141007     c                   clear                   wtxt
288600141007     c                   else
288700141007     c* determino la lunghezza della stringa "buona"
288800141007     c                   eval      lung=(posr-posl)+1
288900141007     c                   eval      wtxt=%subst(wtxt:posl:lung)
289000141007     c                   endif
289100141007
289200141007     c                   EndSr
289300140814**
289400141006CHKOBJ OBJ(FILTRAPRD) OBJTYPE(*LIB)
