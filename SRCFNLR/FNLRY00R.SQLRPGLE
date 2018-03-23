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
003000161128     FFnblp01l  IF   E           K DISK    extfile(wlibfil) usropn
003100161128     f                                     prefix(ARB:3)
003200140522     FFiar501l  IF   E           K DISK    extfile(wlibfil) usropn
003300170421     FFiar901l  IF   E           K DISK    extfile(wlibfil) usropn
003400140528     FFiar401l  IF   E           K DISK    extfile(wlibfil) usropn
003500140521     FFipct02l  IF   E           K DISK    extfile(wlibfil) usropn
003600161215     FFnevb01l  IF   E           K DISK
003700140521     FTIgcp21l  IF   E           K DISK
003800140605     Fazcln01l  IF   E           K DISK
003900140620     Ffnlbl02l  IF   E           K DISK
004000131008
004100131007       //--------------------------------------------------------------
004200131007       //?Definizione costanti.                                        ?
004300131007       //--------------------------------------------------------------
004400140520      /copy gaitrasrc/srcconst,FNLRY00R
004500140520
004600140520     D digits          c                   '0123456789'
004700140617
004800140617     D MSGID_LUNEDI...
004900140617     D                 C                   'TIS0351'
005000140617     D MSGID_MARTEDI...
005100140617     D                 C                   'TIS0354'
005200140617     D MSGID_MERCOLEDI...
005300140617     D                 C                   'TIS0356'
005400140617     D MSGID_GIOVEDI...
005500140617     D                 C                   'TIS0203'
005600140617     D MSGID_VENERDI...
005700140617     D                 C                   'TIS0607'
005800140617     D MSGID_SABATO...
005900140617     D                 C                   'TIS0533'
006000140617     D MSGID_DOMENICA...
006100140617     D                 C                   'TIS0159'
006200140114
006300131007       //--------------------------------------------------------------
006400131007       //?Definizione strutture dati.                                  ?
006500131007       //--------------------------------------------------------------
006600131007       // -?Dati INPUT
006700140519     d Fnlry00i0     e ds                  QUALIFIED INZ(*EXTDFT)
006800140528     d Fnlry00i1     e ds                  QUALIFIED INZ(*EXTDFT)
006900141006     d TIIDCds       e ds                  extname(tiidc00f)
007000131007
007100131007       // -?Dati OUTPUT
007200140519     d Fnlry00o0     e ds                  QUALIFIED INZ(*EXTDFT)
007300140605     d  schdcr                       10a   dim(9)  overlay(dateconst)
007400140923     d  schdcri                      10a   dim(9)  overlay(dateconti)
007500140521     d Fnlry00o1     e ds                  QUALIFIED INZ(*EXTDFT)
007600131007
007700131007       // -?Messaggi
007800140522     d Fnlry00m0     e ds                  QUALIFIED INZ(*EXTDFT)
007900140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
008000140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
008100140522     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
008200140522     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
008300131007
008400131007       // -?Forzature
008500140522     d Fnlry00f0     e ds                  QUALIFIED INZ(*EXTDFT)
008600140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
008700140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
008800140522     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
008900140520       // -?Legami clienti
009000140520     d TIBS10DS      e ds                  INZ(*EXTDFT)
009100140520     d  skc                          11    dim(500) overlay(d10skc)
009200140521       // -?Controllo BRTCODE
009300180109     d TRULa8DS0     e ds                  INZ(*EXTDFT)
009400140528       // -?Controllo indirizzo mail
009500140528     d dsemail       e ds                  INZ
009600140521       // -?Calcola orari di servizio
009700140521     d TRULORSDS     e ds                  INZ
009800140521     d TRULOR2ds     e ds                  INZ
009900150729     d diore01       e ds                  INZ
010000140604
010100161129       // -?File bolle arrivi
010200161129     d FNARBds       e ds                  extname(FNARB00F)
010300140522     D tnsd99ds      e ds
010400140604     D TRUL33DS      E DS
010500170421     D TRUL21ds      E DS
010600140923     D Tisio9ds      E DS
010700140521
010800140521     d fidn12ds      e ds
010900140603     d tibs02ds      e ds
011000140603     d fnlv55ds      e ds
011100140604     d KPJBA         e ds
011200161128
011300161128     d TRULVPODS     e ds
011400161128     D  skFilOk               16    765    dim(250)
011500140521
011600161202     d didcal        e ds
011700140521     d fipctcetds    e ds
011800140521     d og143         e ds
011900140522     d dar5emd       e ds
012000140523     d dar5gen       e ds
012100140522     d ds7r          e ds
012200140522     d ds3a          e ds
012300140613     d ds2a          e ds
012400140528     d ds2g          e ds
012500140528     d ds7u          e ds
012600140619     d ds7w          e ds
012700140603     d divp          e ds
012800140924     d dvpooraris    e ds
012900140528
013000140528     D WDAT8           DS
013100140528     D  DATADA                 1      8  0
013200140528     D  DATAA                  9     16  0
013300140528     D  GGL                   17     21  0
013400140610     D tfptfa          DS
013500140610     D  p_tfp                  1      3  0
013600140610     D  p_tfa                  4      6  0
013700140606
013800140606     D clnmat          DS
013900140606     D  mat                    1     31    dim(31)
014000140606     D clnpom          DS
014100140606     D  pom                    1     31    dim(31)
014200140617     D dayOfWeek       DS                  QUALIFIED
014300140617     D                                7A   INZ(MSGID_LUNEDI)
014400140617     D                                7A   INZ(MSGID_MARTEDI)
014500140617     D                                7A   INZ(MSGID_MERCOLEDI)
014600140617     D                                7A   INZ(MSGID_GIOVEDI)
014700140617     D                                7A   INZ(MSGID_VENERDI)
014800140617     D                                7A   INZ(MSGID_SABATO)
014900140617     D                                7A   INZ(MSGID_DOMENICA)
015000140617     D  msgId                         7A   DIM(7) OVERLAY(dayOfWeek)
015100140814
015200140814     D chklib          S             35    DIM(1) CTDATA PERRCD(1)
015300140312
015400170421     d                sds
015500170421     d  nompgm                 1     10
015600131009       //--------------------------------------------------------------
015700131009       //?Definizione schiere.
015800131009       //--------------------------------------------------------------
015900140210
016000140210     d skFIdMsg        s              7a   dim(99)
016100140210     d skFIdCampo      s             10a   dim(99)
016200140210     d skFGiaForza     s              1a   dim(99)
016300131007
016400131007       //--------------------------------------------------------------
016500131007       //?Definizione campi.
016600131007       //--------------------------------------------------------------
016700140527     d wIdMsg          s              7a
016800140527     d wIdCampo        s             10a
016900140527     d wcampo          s             10a
017000140519     d wksu            s                   like(fnlry00i0.ksu)
017100140520     d widbolla        s                   like(fnlry00i0.idbolla)
017200140521     d wbolla          s                   like(widbolla)
017300140520     d wbrtcode        s                   like(fnlry00i0.brtcode)
017400140527     d wIdlingua       s                   like(fnlry00i0.liniso2)
017500140527     d wdcreur         s                   like(fnlry00i1.olddtcrich)
017600140529     d wrbhcr          s                   like(fnlry00i1.OLDORCRICH)
017700140528     d wtprich         s              1
017800140530     d wtentacons      s              1
017900140528     d dataoggi        s              8s 0
018000161129     d dataPrvC        s              8s 0
018100141218     d wdataconfr      s              8s 0
018200140604     d annocur         s              4s 0
018300140528     d wdcr            s              8s 0
018400140605     d wdata           s              8s 0
018500161206     d wdatam          s              8s 0
018600161206     d wdatai          s              8s 0
018700140528     d wora            s              4s 0
018800140527     d wrsd2           s             35
018900140522     d wchi            s              1
019000180109     d wtric           s              1
019100140521     d kaas            s                   like(arbaas)
019200140521     d klnp            s                   like(arblnp)
019300140521     d knrs            s                   like(arbnrs)
019400140521     d knsp            s                   like(arbnsp)
019500140522     d kkey            s                   like(tblkey)
019600140523     d wrefbolla       s                   like(§ar5ref)
019700140523     d wtelbolla       s                   like(§ar5teld)
019800140528     d wlimitdcr       s                   like(§2gldr)
019900170217     d wlimitdcr_s     s                   like(§2gldr)
020000140613     d wlimitFD        s                   like(§2agla)
020100171128       // -?Lunghezza del codice ricevuto in Input?
020200171128     d wLen            s              3  0 inz
020300140521     d wcliente        s             11
020400140521     D Wlibfil         S             21
020500140521     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
020600140521     D Wlibfil201      S             21    INZ('FILTRA201/        ')
020700140521     d dataeur         s               d   datfmt(*eur)
020800140528     d dataiso         s               d   datfmt(*iso)
020900140528     d wParm           s            512a   inz
021000140528     d w008a           s              8a   inz
021100140908     d w0140           s             14  0 inz
021200140603     d wfgslna         s                   like(d55tfa)
021300140616     d wfgslnad        s             20a
021400140604     D Esito           s              1a
021500141031     d w0090           s              9  0
021600140604     d kprg            s                   like(idcprg)
021700140604     d WDCINSDATA      s             20  0
021800140605     d I               s              2  0
021900140606     d Ix              s              2  0
022000140923     d Iy              s              2  0
022100140606     d wfes            s              1
022200140606     d matpom          s             10
022300140611     d wTextMsg        s            255
022400140604     d $Finenum        s               n   inz(*off)
022500140617     d dw              s              1s 0
022600140617     d wdesgio         s             20a
022700140718     d totale          s              2  0
022800170331     d totdispo        s              2  0
022900140801     d wkeyspa         s             16
023000140924     D wtempoh         S              2  0 inz
023100140924     D wtempom         S              2  0 inz
023200140925     D waltroind       S              1a   inz
023300140930     D wconsric        S                   like(fnlry00i1.prconsric)
023400140930     D w_dalle         S               t
023500140930     D w_alle          S               t
023600140930     D w_dalleoser     S               t
023700140930     D w_alleoser      S               t
023800140930     D w_dallemsg      S              5a
023900140930     D w_allemsg       S              5a
024000140930     D w_orcric        S                   like(fnlry00i1.neworcrich)
024100141006     D wolna           S                   like(olna)
024200141007     d wtxt            S           2048
024300141007     d TxtInOut        S           2048
024400141007     d ElencoChar      S            256
024500141007     d TipoElenco      S              1
024600141007     d CharSost        S              1
024700141007     d UpperCase       S              1
024800141007     d ChkNull         S              1    Inz('1')
024900141007     d CharNull        S              1
025000141007     d posl            s              2  0
025100141007     d posr            s              2  0
025200141007     d lung            s              2  0
025300141007     d* Lunghezza di CHARNOOK deve essere:  lunghezza di tbeuni + 1
025400141007     d* Il +1 è necessario per assicurarmi che nella stringa sia sempre
025500141007     d* presente un blank
025600141007     d charnook        s            257
025700141007     d charnookf       s                   like(charnook)
025800170404     d charnooki       s                   like(charnook)
025900161130     d w_tbo           s              1    inz
026000161129     d wDecofiAll      s               n   inz
026100140611       //?- Parametri per FISPE02R: EasySpedWeb: S.P.  TISIT5R+TISI95R?
026200140611     d iTYPE           s              1    inz
026300140611     d iLANG           s              3    inz
026400140611     d iDATA           s              8s 0 inz
026500140611     d iNTW            s              1    inz
026600140611     d iLNP            s              3s 0 inz
026700140611     d iNZD            s              3    inz
026800140611     d iPRD            s              2    inz
026900140611     d iCAD            s              9    inz
027000140611     d iLOD            s             35    inz
027100140611     d iIND            s             35    inz
027200140611     d iRSD            s             35    inz
027300140611     d iNCL            s              5s 0 inz
027400140611     d iPKB            s              7s 1 inz
027500140611     d iVLB            s              5s 3 inz
027600140611     d iFFD            s              1    inz
027700140611     d iTSP            s              1    inz
027800140611     d iTC1            s              1    inz
027900140611     d iTC2            s              1    inz
028000140611     d iCBO            s              2    inz
028100140611      *
028200140611     d oTFA            s              3s 0 inz
028300140611     d oLNA            s              3s 0 inz
028400140611     d oZNC            s              2s 0 inz
028500140611     d oLNPD           s             20    inz
028600140611     d oLNAD           s             20    inz
028700140611     d oERR            s              1    inz
028800140611     d oMSG            s            256    inz
028900140611     d oTAG            s             30    inz
029000140611      *
029100140611     d RTNesito        s             10I 0 inz
029200140611     d RTNopcode       s             10    inz
029300140611     d RTNstatus       s             10I 0 inz
029400131008
029500131008       //--------------------------------------------------------------
029600131008       //?Definizione procedure.
029700131008       //--------------------------------------------------------------
029800140603     d tibs02r         pr                  extpgm('TIBS02R')
029900140604     d  kpjba                              likeds(kpjba)
030000140603     d  tibs02ds                           likeds(tibs02ds)
030100140604     D trul33R         pr                  extpgm('TRUL33R')
030200140604     D  kpjba                              likeds(kpjba)
030300140923     D tisio9r         pr                  extpgm('TISIO9R')
030400140923     D  kpjba                              likeds(kpjba)
030500140923     D  tisio9ds                           likeds(tisio9ds)
030600140611       //?- Stored Procedure per EasySpedWeb?
030700140611     d fispE02r        pr                  extpgm('FISPE02R')
030800140611     d   i_TYPE                            like(iTYPE)
030900140611     d   i_LANG                            like(iLANG)
031000140611     d   i_DATA                            like(iDATA)
031100140611     d   i_NTW                             like(iNTW)
031200140611     d   i_LNP                             like(iLNP)
031300140611     d   i_NZD                             like(iNZD)
031400140611     d   i_PRD                             like(iPRD)
031500140611     d   i_CAD                             like(iCAD)
031600140611     d   i_LOD                             like(iLOD)
031700140611     d   i_IND                             like(iIND)
031800140611     d   i_RSD                             like(iRSD)
031900140611     d   i_NCL                             like(iNCL)
032000140611     d   i_PKB                             like(iPKB)
032100140611     d   i_VLB                             like(iVLB)
032200140611     d   i_FFD                             like(iFFD)
032300140611     d   i_TSP                             like(iTSP)
032400140611     d   i_TC1                             like(iTC1)
032500140611     d   i_TC2                             like(iTC2)
032600140611     d   i_CBO                             like(iCBO)
032700140611     d   o_TFA                             like(oTFA)
032800140611     d   o_LNA                             like(oLNA)
032900140611     d   o_ZNC                             like(oZNC)
033000140611     d   o_LNPD                            like(oLNPD)
033100140611     d   o_LNAD                            like(oLNAD)
033200140611     d   o_ERR                             like(oERR)
033300140611     d   o_MSG                             like(oMSG)
033400140611     d   o_TAG                             like(oTAG)
033500140611     d   RTN_esito                         like(RTNesito)
033600140611     d   RTN_opcode                        like(RTNopcode)
033700140611     d   RTN_status                        like(RTNstatus)
033800131008
033900131008       //--------------------------------------------------------------
034000131008       //?Definizione prototipi.
034100131008       //--------------------------------------------------------------
034200140519      /copy gaitrasrc/srcprotopr,Fnlry00r
034300140520      /copy gaitrasrc/srcprotopr,TIBS10R
034400180109      /copy gaitrasrc/srcprotopr,TRULa8R0
034500140521      /copy gaitrasrc/srcprotopr,FIDN12R
034600140522      /copy gaitrasrc/srcprotopr,tnsd99r
034700131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
034800131230      /copy gaitrasrc/srcprotopr,TIBS0800R
034900140311      /copy gaitrasrc/srcprotopr,UBCHKCEL
035000140311      /copy gaitrasrc/srcprotopi,UBCHKCEL
035100140311      /copy gaitrasrc/srcprotopr,XEMAIL
035200140528      /copy gaitrasrc/srcprotopr,xsrlav8
035300140603      /copy gaitrasrc/srcprotopr,fnlv55r
035400131219
035500131219       //--------------------------------------------------------------
035600131219       //?Definizione parametri programma.
035700131219       //--------------------------------------------------------------
035800140519     D Fnlry00_Immetti...
035900140519     D                 PI
036000140519     D prmRqsOpCode...
036100140519     D                               10I 0 CONST
036200140519     D prmRpyOpCode...
036300140519     D                               10I 0
036400140519     D prmRpyIdMsg...
036500140519     D                               10I 0
036600140519     D prmRqsFormato...
036700140519     D                               10A   CONST
036800140519     D prmRqsData...
036900140519     D                            32767A   OPTIONS(*VARSIZE)
037000140519     D prmRqsDataSize...
037100140519     D                               10I 0 CONST
037200140519     D prmRpyFormato...
037300140520     D                               10A   CONST
037400140519     D prmRpyData...
037500140519     D                            32767A   OPTIONS(*VARSIZE)
037600140519     D prmRpyDataSize...
037700140519     D                               10I 0 CONST
037800140519     D prmRpyFormMsg...
037900140519     D                               10A   CONST OPTIONS(*NOPASS)
038000140519     D prmRpyMessage...
038100140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
038200140519     D prmRpyMsgSize...
038300140519     D                               10I 0 CONST OPTIONS(*NOPASS)
038400140519     D prmRpyFormForz...
038500140519     D                               10A   CONST OPTIONS(*NOPASS)
038600140519     D prmRpyForzatu...
038700140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
038800140519     D prmRpyForSize...
038900140519     D                               10I 0 CONST OPTIONS(*NOPASS)
039000131007
039100131007       //--------------------------------------------------------------
039200131007       //?MAIN.
039300131007       //--------------------------------------------------------------
039400131008
039500131007      /free
039600131008
039700131008       //?Operazioni iniziali
039800131008       exsr RoutInz;
039900131008
040000140523       //?Controllo formale dei dati di accesso
040100131008       exsr Controlla;
040200140523       //?Controllo stato bolla: deve essere in uno stato che renda possibile
040300140523       //?l'immissione delle disposizioni di consegna
040400140523       exsr StatoBolla;
040500140529
040600140523       //?Controllo che l'immagine della bolla da Web sia la stessa della bolla su
040700140523       //?AS/400
040800140529       //IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
040900140529       //   exsr ImmagineB;
041000140529       //endif;
041100140529
041200140529       //?Chaino estensioni bolla
041300140529       exsr sr_chainEstBolla;
041400140523       //?Controllo dettaglio dei campi ricevuti
041500140521       exsr ControllaDett;
041600140603       //?Per PUTISTRUCO in modalità Controllo + Inserimento scrivo file TIIDC
041700140611       IF  prmRqsOpCode=FNLRY00_RQSOPCODE_PUT_ISTRUCO and fnlry00i1.tipoela='2'
041800140611                                                      and fnlry00m0.nrmsg=0;
041900140604          exsr sr_Tiidc;
042000140613       // Restituzione messaggio di conferma
042100140613          exsr sr_MsgConf;
042200140603       endif;
042300131010
042400131010       //?Operazioni finali
042500131010       exsr RoutEnd;
042600131008
042700140613       //--------------------------------------------------------------
042800140613       //?Messaggio di avvenuta registrazione delle Indicazioni di Cons.
042900140613       //--------------------------------------------------------------
043000140613       BEGSR  sr_MsgConf;
043100140613
043200140929       // MESSAGGIO GENERICO
043300140617       wIdMsg   = 'TIS0771';
043400140617       fnlry00o1.MSGCONFERM=rtvMsgLang(wIdMsg:widlingua);
043500141007       fnlry00o1.MSGCONFERM=%trim(fnlry00o1.MSGCONFERM)+' del '
043600141007                                                  + %char(%date():*eur)
043700141007                                                  + ' '+  %char(%time()) ;
043800140613
043900140929       // MESSAGGIO SPECIFICO per Fermo Deposito e Consegna richiesta:
044000140929       // FERMO DEPOSITO
044100140929       select;
044200140929       when fnlry00i1.tipodispo='2';
044300161216        if w_tbo='A' and arbdam>0 ;
044400140930          exsr sr_wdesgio;
044500140924          wIdMsg   = 'TIS0772';
044600140924          wparm=wdesgio + %editc(WlimitFD:'X') ;
044700140929          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
044800140617          fnlry00o1.fdidfil=wfgslna;
044900161130        else ;
045000161216          clear wParm;
045100161216       // !!!!!!!!!!!!!!!!!!!!
045200161216       // Nuovo messaggio da inserire nel file messaggi:
045300161216       // "Il destinatario verrà contattato per il ritiro della merce "
045400161216          wIdMsg   = 'TIS0857';
045500161216          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
045600161216          fnlry00o1.fdidfil=wfgslna;
045700161130        endif;
045800140929       // APPUNTAMENTO
045900140929       when fnlry00i1.tipodispo='1';
046000140930          wIdMsg   = 'TIS0787';
046100140930          wparm=fnlry00i1.newdtcrich ;
046200140929       // presente orario
046300140929          if fnlry00i1.neworcrich>*zeros;
046400140930             wIdMsg   = 'TIS0785';
046500140930             w_orcric=fnlry00i1.neworcrich;
046600140930             exsr sr_dallealle;
046700140930            wparm=fnlry00i1.newdtcrich + w_dALLEmsg + W_ALLEMSG;
046800140929          endif;
046900140929       // Presente preferenza matt/pomeriggio
047000140929          if fnlry00i1.prconsric<>*blanks;
047100140929             wconsric=fnlry00i1.prconsric;
047200140929             exsr sr_matpom;
047300140930             wIdMsg   = 'TIS0789';
047400140929             wparm=fnlry00i1.newdtcrich + matpom ;
047500140929          endif;
047600140930          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
047700140929       // ALTRO INDIRIZZO
047800140930       when fnlry00i1.tipodispo='3';
047900140930       // presente orario --> determino il "Dalle - Alle"
048000140930          if fnlry00i1.neworcrici>*zeros;
048100140930             w_orcric=fnlry00i1.neworcrici;
048200140930             exsr sr_dallealle;
048300140930          endif;
048400140930       // Presente preferenza matt/pomeriggio --> Traduco il flag nella descrizione
048500140930          if fnlry00i1.prconsrici<>*blanks;
048600140930             wconsric=fnlry00i1.prconsrici;
048700140930             exsr sr_matpom;
048800140930          endif;
048900141001       // Messaggi in presenza della data
049000140930          if fnlry00i1.newdtcrici>*zeros;
049100140930             wIdMsg   = 'TIS0788';
049200140930             wparm=fnlry00i1.newdtcrici ;
049300140930             if fnlry00i1.neworcrici>*zeros;
049400140930                wIdMsg   = 'TIS0786';
049500140930                wparm=fnlry00i1.newdtcrici + w_dALLEmsg + W_ALLEMSG;
049600140930             endif;
049700140930       // Presente preferenza matt/pomeriggio
049800140930             if fnlry00i1.prconsrici<>*blanks;
049900140930                wIdMsg   = 'TIS0790';
050000140930                wparm=fnlry00i1.newdtcrici + matpom ;
050100140930             endif;
050200141001             fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
050300140930          else;
050400141001       // Messaggi in assenza della data (caso che non si verifica perchè se presente
050500141001       //   ora o preferenza devo avere anche la data)
050600140930             if fnlry00i1.neworcrici>*zeros;
050700140930                wIdMsg   = 'TIS0791';
050800140930                wparm=w_dALLEmsg + W_ALLEMSG;
050900141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
051000140930             endif;
051100140930       // Presente preferenza matt/pomeriggio
051200140930             if fnlry00i1.prconsrici<>*blanks;
051300140930                wIdMsg   = 'TIS0792';
051400140930                wparm=matpom ;
051500141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
051600140930             endif;
051700140930          endif;
051800140929
051900140929       endsl;
052000140613
052100140613       endsr;
052200140930       //--------------------------------------------------------------
052300140930       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
052400140930       //--------------------------------------------------------------
052500140930       BEGSR  sr_dallealle;
052600140930
052700140930       //   determino il Dalle/alle da mettere nel messaggio
052800140930            w_dalle=%time((%dec(w_orcric:4:0)*100):*hms)-%minutes(§VPORST_MW);
052900140930            w_alle=%time((%dec(w_orcric:4:0)*100):*hms)+%minutes(§VPORST_PW);
053000140930       //   determino orari servizio comprensivi della tolleranza
053100140930            if §ivptipo='S';
053200140930               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
053300140930               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
053400140930            else;
053500140930               w_dalleoser=%time((oor2miis *100):*hms)-%minutes(§VPORST_MT);
053600140930               w_alleoser=%time((oor2mxfs *100):*hms)+%minutes(§VPORST_MT);
053700140930            endif;
053800140930       //   Confronto dalle/alle calcolato con dalle/alle orari ser
053900140930       //      Dalle < del "dalle" orario ser --> prendo quest'ultimo e l'alle
054000140930       //      lo ricalcolo aggiungendo i minuti da tabella vpo
054100140930            if w_dalle<w_dalleoser;
054200140930               w_dalle=w_dalleoser;
054300140930               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
054400140930            endif;
054500140930       //      Alle  > del "alle" orario ser --> prendo quest'ultimo e il dalle
054600140930       //      lo ricalcolo sottraendo i minuti da tabella vpo purchè non diventi
054700140930       //      minore del "dalle" orario ser
054800140930            if w_alle>w_alleoser;
054900140930               w_alle=w_alleoser;
055000140930               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
055100140930                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
055200140930               endif;
055300140930            endif;
055400140930            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
055500140930            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
055600140930
055700140930       endsr;
055800131008       //--------------------------------------------------------------
055900131008       //?Operazioni iniziali.                                         ?
056000131008       //--------------------------------------------------------------
056100131008       BEGSR  RoutInz;
056200140211
056300140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
056400131008
056500140519         prmRpyOpCode = FNLRY00_RPYOPCODE_DONE;
056600140519         prmRpyIdMsg  = FNLRY00_ESITO_OK;
056700131009
056800131009       //?Data del giorno
056900131009         dataoggi = %dec(%date());
057000140604         annocur  = %subdt(%date():*years);
057100131009
057200131009       //?Pulisco ds di output
057300140519         reset Fnlry00o0;
057400140519         reset fnlry00o1;
057500140522         reset fnlry00m0;
057600140521
057700140521       //?Apertura file
057800140521         wlibfil=wlibfil201;
057900140521         %subst(wlibfil:11:8)='FNARB01L';
058000140521         open(e) fnarb01l;
058100140521         if not %open(fnarb01l);
058200140521            wlibfil=wlibfilprd;
058300140521            %subst(wlibfil:11:8)='FNARB01L';
058400140521            open fnarb01l;
058500140521         endif;
058600161128         %subst(wlibfil:11:8)='FNBLP01L';
058700161128         open(e) fnblp01l;
058800161128         if not %open(fnblp01l);
058900161128            wlibfil=wlibfilprd;
059000161128            %subst(wlibfil:11:8)='FNBLP01L';
059100161128            open fnblp01l;
059200161128         endif;
059300140521         wlibfil=wlibfil201;
059400140521         %subst(wlibfil:11:8)='FIPCT02L';
059500140521         open(e) fipct02l;
059600140521         if not %open(fipct02l);
059700140521            wlibfil=wlibfilprd;
059800140521            %subst(wlibfil:11:8)='FIPCT02L';
059900140521            open fipct02l;
060000140521         endif;
060100140522         wlibfil=wlibfil201;
060200140522         %subst(wlibfil:11:8)='FIAR501L';
060300140522         open(e) fiar501l;
060400140522         if not %open(fiar501l);
060500140522            wlibfil=wlibfilprd;
060600140522            %subst(wlibfil:11:8)='FIAR501L';
060700140522            open fiar501l;
060800140522         endif;
060900170421         wlibfil=wlibfil201;
061000170421         %subst(wlibfil:11:8)='FIAR901L';
061100170421         open(e) fiar901l;
061200170421         if not %open(fiar901l);
061300170421            wlibfil=wlibfilprd;
061400170421            %subst(wlibfil:11:8)='FIAR901L';
061500170421            open fiar901l;
061600170421         endif;
061700140528         wlibfil=wlibfil201;
061800140528         %subst(wlibfil:11:8)='FIAR401L';
061900140528         open(e) fiar401l;
062000140528         if not %open(fiar401l);
062100140528            wlibfil=wlibfilprd;
062200140528            %subst(wlibfil:11:8)='FIAR401L';
062300140528            open fiar401l;
062400140528         endif;
062500140807         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
062600140807            open tivpi00f ;
062700140807         endif ;
062800140528
062900140528       // Reperimento G.LIMITE X GIAC.CON DATA RIC
063000140528       clear wlimitdcr;
063100170217       clear wlimitdcr_s;
063200140528       chain (1:'2G':'1       ') tabel00f;
063300140528       if %found(tabel00f);
063400140528          ds2g=tbluni;
063500140528          wlimitdcr=§2gldr;
063600140528       endif;
063700170217       wlimitdcr_s=wlimitdcr;
063800140603       // Reperimento dati tabella IVP - Variabili programma per INTERNET
063900140603       clear tibs02ds;
064000140603       clear divp;
064100140603       t02mod='C';
064200140603       t02cod='IVP';
064300140603       t02ke1='1  ';
064400140603       t02tla='L  ';
064500140603       tibs02r(kpjba:tibs02ds);
064600140603       if t02err=*blanks;
064700140603          divp=t02uni;
064800140603       endif;
064900140613       // Reperimento Giorni lavorativi entro cui ritirare le sped. in FD
065000140613       clear wlimitFD ;
065100140613       chain (1:'2A':'F       ') tabel00f;
065200140613       if %found(tabel00f);
065300140613          ds2a=tbluni;
065400140613          wlimitFD =§2agla;
065500140613       endif;
065600140924       // Reperimento dati tabella VPO - ORARISER
065700140924       clear tibs02ds;
065800140925       clear dvpooraris;
065900140924       t02mod='C';
066000140924       t02cod='VPO';
066100140925       t02ke1='ORARISER';
066200140924       t02tla='L  ';
066300140924       tibs02r(kpjba:tibs02ds);
066400140924       if t02err=*blanks;
066500140924          dvpooraris=t02uni;
066600140924       endif;
066700141007       // Reperimento dati tabella CSP - Caratteri speciali per eliminazione
066800141007      *                           caratteri di punteggiatura da loc.dest.
066900170404       //Caratteri da togliere validi per TUTTO il campo  tab "CSP" "LOC_ALL"
067000141007       clear tibs02ds;
067100141007       clear charnook  ;
067200141007       t02mod='C';
067300141007       t02cod='CSP';
067400170404       t02ke1='LOC_ALL';
067500141007       t02tla='L  ';
067600141007       tibs02r(kpjba:tibs02ds);
067700141007       if t02err=*blanks;
067800141007          charnook=t02uni;
067900141007       endif;
068000170404       //Caratteri da togliere validi per la FINE del campo  tab "CSP" "LOC_F"
068100170404       clear tibs02ds;
068200170404       clear charnookf ;
068300170404       t02mod='C';
068400170404       t02cod='CSP';
068500170404       t02ke1='LOC_F';
068600170404       t02tla='L  ';
068700170404       tibs02r(kpjba:tibs02ds);
068800170404       if t02err=*blanks;
068900170404          charnookf=t02uni;
069000170404       endif;
069100170404       //Caratteri da togliere validi per l'INIZIO del campo  tab "CSP" "LOC_I"
069200170404       clear tibs02ds;
069300170404       clear charnooki ;
069400170404       t02mod='C';
069500170404       t02cod='CSP';
069600170404       t02ke1='LOC_I';
069700170404       t02tla='L  ';
069800170404       tibs02r(kpjba:tibs02ds);
069900170404       if t02err=*blanks;
070000170404          charnooki=t02uni;
070100170404       endif;
070200141007     c* imposto stringa contenente caratteri speciali per il controllo
070300141007     c* della parte finale (uguale alla charnook maa senza l'eventuale '.')
070400170404     c*****              eval      charnookf=%xlate('.':' ':charnook)
070500161128     c* carico le filiali abilitate all'immissione dispo di consegna su bolle in partenza
070600161128     c                   clear                   trulvpods
070700161128     c                   eval      ivpoke1 = 'DECOFIIDC'
070800161128     c                   call      'TRULVPOR'
070900161128     c                   parm                    trulvpods
071000161129      *
071100161129     c                   eval      wDecofiAll=*off
071200161129     c                   if        %lookup('999':skFilOk)>0
071300161129     c                   eval      wDecofiAll=*on
071400161129     c                   endif
071500161128     c
071600140521      /end-free
071700140521     C     Kcet          KLIST
071800140521     C                   KFLD                    pctfgs
071900140521     C                   KFLD                    pctNDC
072000140521     C                   KFLD                    arbpdc
072100140521     C                   KFLD                    TTRD              3
072200140521     C                   KFLD                    arbAAS
072300140521     C                   KFLD                    arbLNP
072400140521     C                   KFLD                    arbNRS
072500140521     C                   KFLD                    arbNSP
072600140521      /free
072700131008
072800131008         *inLR = *on;
072900131008
073000131008       ENDSR;
073100131008
073200131008       //--------------------------------------------------------------
073300131008       //?Controllo formale dei dati passati.
073400131008       //--------------------------------------------------------------
073500131008       BEGSR  Controlla;
073600131008
073700140519         clear wksu;
073800140520         clear widbolla;
073900140520         clear wbrtcode;
074000140522         clear wchi    ;
074100140527         clear widlingua;
074200140527         clear wcampo;
074300140519
074400131008       //?OpCode
074500140519         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_GET_ISTRUCO  and
074600140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_PUT_ISTRUCO  and
074700140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
074800140519           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
074900140519           prmRpyIdMsg  = FNLRY00_ESITO_RQSOPCODE_SCONOSCIUTO;
075000140210           exsr RoutEnd;
075100131008         ENDIF;
075200140408
075300140519       //?per GET_ISTRUCO
075400140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
075500140408       //?Formato e size RQS
075600140519           IF  prmRqsFormato = fnlry00I0.formato;
075700140519             fnlry00I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
075800140519             wksu=fnlry00i0.ksu;
075900140520             widbolla=fnlry00i0.idbolla;
076000140520             wbrtcode=fnlry00i0.brtcode;
076100140527             widlingua=fnlry00i0.liniso2;
076200140408           ELSE;
076300140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
076400140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
076500140408             exsr RoutEnd;
076600140408           ENDIF;
076700140408           IF  prmRqsDataSize > 0;
076800140408           ELSE;
076900140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
077000140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
077100140408             exsr RoutEnd;
077200140408           ENDIF;
077300140408         //?Formato e size RPY
077400140519           IF  prmRpyFormato = fnlry00O0.formato;
077500140408           ELSE;
077600140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
077700140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
077800140408             exsr RoutEnd;
077900140408           ENDIF;
078000140408           IF  prmRpyDataSize > 0;
078100140408           ELSE;
078200140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
078300140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
078400140408             exsr RoutEnd;
078500140408           ENDIF;
078600140408         ENDIF;
078700140210
078800140522       //?per PUT_ISTRUCO
078900140522         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
079000140522       //?Formato e size RQS
079100140522           IF  prmRqsFormato = fnlry00I1.formato;
079200140522             fnlry00I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
079300140522             wksu=fnlry00i1.ksu;
079400140522             widbolla=fnlry00i1.idbolla;
079500140522             wbrtcode=fnlry00i1.brtcode;
079600140527             widlingua=fnlry00i1.liniso2;
079700140522           ELSE;
079800140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
079900140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
080000140522             exsr RoutEnd;
080100140522           ENDIF;
080200140522           IF  prmRqsDataSize > 0;
080300140522           ELSE;
080400140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
080500140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
080600140522             exsr RoutEnd;
080700140522           ENDIF;
080800140522         //?Formato e size RPY
080900140522           IF  prmRpyFormato = fnlry00O1.formato;
081000140522           ELSE;
081100140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
081200140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
081300140522             exsr RoutEnd;
081400140522           ENDIF;
081500140522           IF  prmRpyDataSize > 0;
081600140522           ELSE;
081700140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
081800140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
081900140522             exsr RoutEnd;
082000140522           ENDIF;
082100140522       //?Formato e size MSG
082200140522         IF  prmRpyFormMsg = FNLRY00M0.formato;
082300140522         ELSE;
082400140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
082500140522           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
082600140522           exsr RoutEnd;
082700140522         ENDIF;
082800140522         IF  prmRpyMsgSize > 0;
082900140522         ELSE;
083000140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
083100140522           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
083200140522           exsr RoutEnd;
083300140522         ENDIF;
083400140523       //?Tipo Elaborazione
083500140523         if fnlry00i1.tipoela<> '1' and fnlry00i1.tipoela<> '2';
083600140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
083700140523           prmRpyIdMsg = FNLRY00_ESITO_TIPOELA_ERRATO;
083800140523           exsr RoutEnd;
083900140523         endif;
084000140523       //?Tipo Disposizione
084100140523         if fnlry00i1.tipodispo <> '1' and
084200140523            fnlry00i1.tipodispo <> '2' and
084300140523            fnlry00i1.tipodispo <> '3' and
084400151112            fnlry00i1.tipodispo <> '4' and
084500151112            fnlry00i1.tipodispo <> '5'     ;
084600140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
084700140523           prmRpyIdMsg = FNLRY00_ESITO_TIPODISPO_ERRATO;
084800140523           exsr RoutEnd;
084900140523         endif;
085000170404         IF FNLRY00I1.TIPODISPO='3'  ;
085100170404             wtxt=fnlry00i1.newlod ;
085200170404             exsr sr_chklod ;
085300170404             fnlry00i1.newlod=wtxt;
085400170404         ENDIF ;
085500140522         ENDIF;
085600140624
085700140624       //?per CHK_ISTRUCO
085800140624         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
085900140624       //?Formato e size RQS
086000140624           IF  prmRqsFormato = FNLRY00_RQSFORMATO_CHK_ISTRUCO;
086100140624             TIIDCDS = %SUBST(prmRqsData:1:prmRqsDataSize);
086200140624             wksu=idcidcli     ;
086300141223             widbolla=%subst(%editc(idcwaas:'X'):3:2) + %editc(idcwlnp:'X') +
086400141223                             %editc(idcwnrs:'X') + %editc(idcwnsp:'X');
086500140627             if wksu=*blanks;
086600140627             wbrtcode=widbolla;
086700180109             // in questo caso devo calcolare il brtcode dall'idbolla
086800180109             wtric='K'  ;
086900140627             exsr sr_trul28;
087000180109             wbrtcode=oa80cod ;
087100140627             else;
087200140624             clear wbrtcode;
087300140627             endif;
087400140624             widlingua='it';
087500140624           ELSE;
087600140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
087700140624             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
087800140624             exsr RoutEnd;
087900140624           ENDIF;
088000140624           IF  prmRqsDataSize > 0;
088100140624           ELSE;
088200140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
088300140624             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
088400140624             exsr RoutEnd;
088500140624           ENDIF;
088600140624       // per comodità valorizzo campi FNLRY00I1 prendendoli dal record del TIIDC
088700140624           fnlry00i1.TIPODISPO = IDCTIPODIS;
088800140624           fnlry00i1.NEWREFCONS= IDCREF;
088900140624           fnlry00i1.NEWTELCONS= IDCTELD;
089000140624           fnlry00i1.NEWDESMAIL= IDCMAILAVV;
089100140624           fnlry00i1.NEWDESCELL= IDCTELAVV ;
089200140923       // Se no dispo di indirizzo alternativo
089300140923           if fnlry00i1.tipodispo<>'3';
089400140923              if idcdcr>0;
089500140923                 fnlry00i1.NEWDTCRICH= %char(%date(IDCDCR:*iso):*eur);
089600140923              endif;
089700140923              fnlry00i1.NEWORCRICH= %editc(IDCHCR:'X') ;
089800140923              fnlry00i1.NEWTPCRICH= IDCTCR    ;
089900140923              fnlry00i1.PRCONSRIC = IDCPRCR   ;
090000140923           endif;
090100140624           fnlry00i1.NEWRSD    = IDCRSD    ;
090200140624           fnlry00i1.NEWIND    = IDCIND    ;
090300140624           fnlry00i1.NEWCAD    = IDCCAD    ;
090400140624           fnlry00i1.NEWLOD    = IDCLOD    ;
090500140624           fnlry00i1.NEWPRD    = IDCPRD    ;
090600140624           fnlry00i1.NEWNZD    = IDCNAZ    ;
090700140624           fnlry00i1.ALTRO     = IDCNOTE   ;
090800140923       // Se indirizzo alternativo
090900140923           if fnlry00i1.tipodispo='3';
091000140923              if idcdcr>0;
091100140923                 fnlry00i1.NEWDTCRICI=%char(%date(IDCDCR:*iso):*eur);
091200140923              endif;
091300140923                 fnlry00i1.NEWORCRICi= %editc(IDCHCR:'X') ;
091400140923                 fnlry00i1.NEWTPCRICi= IDCTCR    ;
091500140923                 fnlry00i1.PRCONSRICi= IDCPRCR   ;
091600140923           endif;
091700140624
091800140624         //?Formato e size RPY
091900140624       //?Formato e size MSG
092000140624         IF  prmRpyFormMsg = FNLRY00M0.formato;
092100140624         ELSE;
092200140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
092300140624           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
092400140624           exsr RoutEnd;
092500140624         ENDIF;
092600140624         IF  prmRpyMsgSize > 0;
092700140624         ELSE;
092800140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
092900140624           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
093000140624           exsr RoutEnd;
093100140624         ENDIF;
093200140624         ENDIF;
093300140522
093400140210
093500140210       //?Formato e size Forzature
093600140522       //IF  prmRpyFormForz = fNLRY00F0.formato;
093700140522       //ELSE;
093800140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
093900140522       //  prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
094000140522       //  exsr RoutEnd;
094100140522       //ENDIF;
094200140522       //IF  prmRpyForSize > 0;
094300140522       //ELSE;
094400140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
094500140522       //  prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
094600140522       //  exsr RoutEnd;
094700140522       //ENDIF;
094800140519
094900140520       //?devo ricevere ID cliente unificante o BRTCODE
095000140520         if wksu=*blanks and wbrtcode=*blanks;
095100140520           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
095200140520           prmRpyIdMsg = FNLRY00_ESITO_ID_RICHIAMO_ERRATO;
095300140520           exsr RoutEnd;
095400140520         endif;
095500140520       //?Accesso per cliente unificante e ID Bolla
095600140519         if wksu <>*blanks;
095700140520       // ID Bolla mancante
095800140520           if widbolla=*blanks;
095900140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
096000140527              wIdMsg   = 'TIS0734';
096100140527              wIdCampo = 'IDBOLLA   ';
096200140527              clear wParm;
096300140527              exsr Messaggi;
096400140520              exsr RoutEnd;
096500140520           endif;
096600141003       // KSU può contenere solo numeri
096700141003           if %check(digits:wksu)>0;
096800141003                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
096900141003                      wIdMsg   = 'TIS0736';
097000141003                      wIdCampo = 'KSU       ';
097100141003                      wParm = widbolla;
097200141003                      exsr Messaggi;
097300141003                      exsr RoutEnd;
097400141003           endif;
097500140520       // ID cliente unificante
097600140520           clear tibs10ds ;
097700140520           d10tle='WW'  ;
097800140520           d10paf='F'    ;     // verifico se è padre
097900140520           d10cod=%dec(wksu:8:0);
098000140520           TIBS10R (tibs10ds)   ;
098100140520  1        if d10err<>*blanks   ;
098200140520           // Se errore verifico se è figlio
098300140520              clear tibs10ds ;
098400140520              d10tle='WW'        ;
098500140520              d10paf='P'    ;
098600140520              d10cod=%dec(wksu:8:0);
098700140520              TIBS10R (tibs10ds)   ;
098800140520           endif;
098900140520
099000140520  2        if d10err<>*blanks   ;
099100140520             // Imposto nella skiera e come padre solo se stesso
099200140520             skc(1)=%editc(d10cod:'X');
099300140520             d10cop=d10cod      ;
099400140520  2        endif                ;
099500140520       // Verifico correttezza dell'ID bolla
099600140520       // Deve contenere solo numeri
099700140520         if %check(digits:widbolla)>0;
099800140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
099900140527              wIdMsg   = 'TIS0734';
100000140527              wIdCampo = 'IDBOLLA   ';
100100140527              clear wParm;
100200140527              exsr Messaggi;
100300140520              exsr RoutEnd;
100400140520         endif;
100500140529       // Se ricevuto anche il BRTCODE lo controllo
100600140529         if wbrtcode<>*blanks;
100700180109             wtric='C'  ;
100800140529            exsr sr_trul28;
100900140529       //   Id bolla e brtcode devono riferirsi alla stessa spedizione
101000140529            if widbolla<>%subst(wbrtcode:1:14) ;
101100140529              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
101200140529              wIdMsg   = 'TIS0751';
101300140529              wIdCampo = 'IDBOLLA   ';
101400140530              wparm = wbrtcode + widbolla;
101500140529              exsr Messaggi;
101600140529              wIdCampo = 'BRTCODE   ';
101700140529              exsr Messaggi;
101800140529              exsr RoutEnd;
101900140529            endif;
102000140529         endif;
102100161128       // La bolla deve esistere su archivio bolle:
102200140521          wbolla=widbolla;
102300140527          wcampo='IDBOLLA';
102400140521          exsr sr_chainbolla;
102500140520       // Verifico che il cliente sia il mittente o il destinatario della bolla
102600140522             if arbccm>0;
102700140522                eval wcliente='0000'+%editc(arbccm:'X');
102800140522                if %lookup(wcliente:skc)=0;
102900140522                   eval wcliente='0000'+%editc(arbksc:'X');
103000140522                   if %lookup(wcliente:skc)=0;
103100140522                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
103200140527                      wIdMsg   = 'TIS0736';
103300140527                      wIdCampo = 'KSU       ';
103400140530                      wParm = widbolla;
103500140527                      exsr Messaggi;
103600140522                      exsr RoutEnd;
103700140522                   else;
103800140522                      wchi='D';
103900140522                   endif;
104000140522                else;
104100140522                   wchi='M';
104200140522                endif;
104300140522             else;
104400140522                eval wcliente='0000'+%editc(arbksc:'X');
104500140522                if %lookup(wcliente:skc)=0;
104600140522                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
104700140527                   wIdMsg   = 'TIS0736';
104800140527                   wIdCampo = 'KSU       ';
104900140530                   wParm = widbolla;
105000140527                   exsr Messaggi;
105100140522                   exsr RoutEnd;
105200140522                else;
105300140610                   exsr sr_ds3A;
105400140528                      if %subst(§3atb1:1:1)='F';
105500140522       //                se franco    --> mittente
105600140522                         wchi='M';
105700140522                      else;
105800140522       //                se Assegnato --> Destinatario
105900140522                         wchi='D';
106000140522                      endif;
106100140522                endif;
106200140522             endif;
106300140519         else;
106400140520       //?Accesso per BRTCode
106500180109          wtric='C'  ;
106600140529          exsr sr_trul28;
106700161128       // La bolla deve esistere su archivio bolle:
106800140521          wbolla=%subst(wbrtcode:1:14);
106900140527          wcampo='BRTCODE';
107000140521          exsr sr_chainbolla;
107100140522          wchi='D';
107200140519         endif;
107300140620       //?Verifico se presenti legami bolla
107400140620         exsr sr_lbl;
107500131008
107600131008       ENDSR;
107700140529       //--------------------------------------------------------------
107800140529       //?Controllo correttezza BRTCODE
107900140529       //--------------------------------------------------------------
108000140529       BEGSR  sr_trul28 ;
108100171128
108200180109          clear trula8ds0;
108300180109          ia80tric=wtric  ;
108400180109          ia80cod=wbrtcode;
108500180109          trula8r0(trula8ds0);
108600180109          if oa80err='1';
108700140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
108800140529             wIdMsg   = 'TIS0737';
108900140529             wIdCampo = 'BRTCODE   ';
109000140530             wparm = wbrtcode ;
109100140529             exsr Messaggi;
109200140529             exsr RoutEnd;
109300140529          endif;
109400180109
109500140529       ENDSR;
109600140523       //--------------------------------------------------------------
109700140523       //?Controlli stato della bolla per abilitare le istruzioni di consegna
109800161202       //--------------------------------------------------------------
109900140523       BEGSR  statobolla;
110000140530          clear wtentacons;
110100140620          clear wdcreur;
110200140620          clear wrsd2  ;
110300140620
110400140620       // DETErMINO IL GESTORE DELLA LNA
110500140620         clear fnlv55ds;
110600140620         d55lin=arblna;
110700140620         d55tpt='6';
110800140620         d55drf=dataoggi;
110900140620         fnlv55r(fnlv55ds);
111000140620         wfgslna=d55tfA;
111100140620         clear wfgslnad;
111200140620         chain (wfgslna) azorg01l;
111300140620         if %found(azorg01l);
111400140620            wfgslnad=orgdes;
111500140620         endif;
111600140620
111700140620         clear dataeur;
111800140620         if arbdcr>0 ;
111900140620            dataeur=%date(arbdcr:*iso);
112000140620            wdcreur=%char(dataeur);
112100140620         endif;
112200140620
112300140620         clear wrbhcr ;
112400140620         if arbhcr>0 ;
112500140620            wrbhcr=%editc(arbhcr:'X');
112600140620         endif;
112700140620
112800140620       //wtprich=%subst(arbgc1:2:1);
112900140620       //if wtprich=*blanks;
113000140620       //    wtprich=%subst(arbgc2:2:1);
113100140620       //endif;
113200140620
113300140620            chain (arbaas:arblnp:arbnrs:arbnsp:'D') fiar401l ;
113400140620            if %found(fiar401l);
113500140620               eval wrsd2=%subst(ar4not:1:35);
113600140620            endif;
113700140620
113800140530       //?deve essere diretta in Italia
113900140530         clear og143;
114000140530         chain (arblna) azorg01l;
114100140530         if %found(azorg01l);
114200140530            og143=orgde3;
114300140530            if §ogntw = 'EEX' or §ogntw= 'FED' or §ogntw= 'DPD';
114400140530               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
114500140530               wIdMsg   = 'TIS0739';
114600140530               wIdCampo = '*BOLLA    ';
114700140530               wParm = wbolla;
114800140530               exsr Messaggi;
114900140530               exsr routend;
115000140530            endif;
115100140530         endif;
115200161129        if w_tbo='A';
115300140523       //?Spedizione non deve essere consegnata
115400140523          if arbdcm>0;
115500141007            IF  prmRqsOpCode =  FNLRY00_RQSOPCODE_CHK_ISTRUCO;
115600141006                wIdMsg   = 'TISA738';
115700141006                wIdCampo = '*BOLLA    ';
115800141006                clear wParm ;
115900141006            else;
116000140527                wIdMsg   = 'TIS0738';
116100140527                wIdCampo = '*BOLLA    ';
116200140530                wParm = wbolla ;
116300141006            endif;
116400141006                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
116500140527                exsr Messaggi;
116600141006                exsr routend;
116700140523          else;
116800140523      /end-free
116900140523     c* prima vedo se c'e' da PDA
117000140708     c                   exsr      sr_RepPct
117100140523      /free
117200140523          endif;
117300140523       //?non deve avere giacenze o c.a. aperte
117400140620         chain (Arbaas:arblnp:arbnrs:arbnsp) tigcp21l;
117500140523         if %found(tigcp21l) and gcpatb=' ' and gcpdcg=0;
117600140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
117700141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
117800140527            wIdMsg   = 'TIS0740';
117900140527            wIdCampo = '*BOLLA    ';
118000140619            wParm = wbolla + wfgslnad;
118100141006           else;
118200141006            wIdMsg   = 'TISA740';
118300141006            wIdCampo = '*BOLLA    ';
118400141006            clear wParm ;
118500141006           endif;
118600140527            exsr Messaggi;
118700140523            exsr routend;
118800140523         endif;
118900140523         clear fidn12ds;
119000140620         i12aas=arbaas;
119100140620         i12lnp=arblnp;
119200140620         i12nrs=arbnrs;
119300140620         i12nsp=arbnsp;
119400140523         i12fel='E';
119500140523         i12fan='E';
119600140523         i12fch='E';
119700140523         fidn12r(fidn12ds);
119800140523         if o12nca>0;
119900140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
120000141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
120100140527            wIdMsg   = 'TIS0741';
120200140527            wIdCampo = '*BOLLA    ';
120300140530            wParm = wbolla;
120400141006           else;
120500141006            wIdMsg   = 'TISA741';
120600141006            wIdCampo = '*BOLLA    ';
120700141006            clear wParm ;
120800141006           endif;
120900140527            exsr Messaggi;
121000140523            exsr routend;
121100140523         endif;
121200140619       //?non deve essere bloccata
121300140917         if arbfbc<>*blanks and arbfbc<>'A';
121400140619            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
121500141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
121600140619            wIdMsg   = 'TIS0740';
121700140619            wIdCampo = '*BOLLA    ';
121800140619            wParm = wbolla + wfgslnad;
121900141006           else;
122000141006            wIdMsg   = 'TISA740';
122100141006            wIdCampo = '*BOLLA    ';
122200141006            clear wParm ;
122300141006           endif;
122400140619            exsr Messaggi;
122500140619            exsr routend;
122600140619         endif;
122700161129        endif;
122800140910       //?non deve avere un codice mancata consegna di tipo giacenza
122900140911       //if arbcmc <> *blanks;
123000140911       //  clear ds2a;
123100140911       //  w008a = arbcmc;
123200140911       //  chain (1:'2A':w008a) tabel00f;
123300140911       //  if %found(tabel00f);
123400140911       //     ds2a=tbluni;
123500140911       //  endif;
123600140911       //  if §2aftc='G';
123700140911       //     prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
123800140911       //     wIdMsg   = 'TIS0740';
123900140911       //     wIdCampo = '*BOLLA    ';
124000140911       //     wParm = wbolla + wfgslnad;
124100140911       //     exsr Messaggi;
124200140911       //     exsr routend;
124300140911       //  endif;
124400140911       //endif;
124500140530       //?deve essere già stato fatto almeno un tentativo di consegna
124600161129        if wDecofiAll=*on or %lookup(%editc(arblna:'X'):skFilOk)=0 ;
124700161202         if §IVPTENC='S' and (W_tbo='P' or (arbntc=0 and wtentacons=*blanks))  ;
124800140530             wIdMsg   = 'TIS0752';
124900140530             wIdCampo = '*NOTENTAT ';
125000140530             wParm = wbolla;
125100140530             exsr Messaggi;
125200140530             exsr routend;
125300140530         endif;
125400161129        endif;
125500141016       //?non deve essere una spedizione per i supermercati
125600141016         if arbtc1='S' or arbtc2='S'                          ;
125700141016             wIdMsg   = 'TIS0812';
125800141016             wIdCampo = '*BOLLA    ';
125900141016             clear wparm;
126000141016             exsr Messaggi;
126100141016             exsr routend;
126200141016         endif;
126300170421       //?non deve esserci il contrassegno ancora da abilitare
126400170421         if  w_tbo ='P' ;
126500170421          chain (arbaas:arblnp:arbnrs:arbnsp) fiar901l ;
126600170421          if %found(fiar901l) and ar9cas>0 ;
126700170421
126800170421             exsr sr_ds3A;
126900170421
127000170421     c                   clear                   trul21ds
127100170421     c                   eval      i21tla = 'L'
127200170421     c                   eval      i21cbo = arbcbo
127300170421     c                   eval      i21tsp = arbtsp
127400170421     c                   eval      i21lnp = arblnp
127500170421     c                   eval      i21nzm = arbnzm
127600170421     c                   eval      i21lna = arblna
127700170421     c                   eval      i21nzd = arbnzd
127800170421     c                   eval      i21tic = ar9tic
127900170421     c                   eval      i21imp = ar9cas
128000170421     c                   eval      i21div = ar9vca
128100170421     c                   eval      i21ute = 'INTERNET'
128200170421     c                   eval      i21pgm = nompgm
128300170421     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
128400170421     c                   if        %subst(§3atb1:1:1)='F'
128500170421     c                   z-add     arbksc        i21ksc
128600170421     c                   else
128700170421     c                   z-add     arbccm        i21ksc
128800170421     c                   if        i21ksc=0
128900170421     c                   z-add     8888          i21ksc
129000170421     c                   movel     arblnp        i21ksc
129100170421     c                   endif
129200170421     c                   endif
129300170421     c
129400170421     c                   call      'TRUL21R'
129500170421     c                   parm                    trul21ds
129600170421
129700170421     c                   if        o21fca <> *blanks
129800170421                                 wIdMsg   = 'TIS0740';
129900170421                                 wIdCampo = '*BOLLA    ';
130000170421                                 wParm = wbolla + wfgslnad;
130100170421                                 exsr Messaggi;
130200170421                                 exsr routend;
130300170421     c                   endif
130400170421          endif;
130500170421         else;
130600170421            if arbacc = ' '  ;
130700170421             wIdMsg   = 'TIS0740';
130800170421             wIdCampo = '*BOLLA    ';
130900170421             wParm = wbolla + wfgslnad;
131000170421             exsr Messaggi;
131100170421             exsr routend;
131200170421            endif;
131300170421         endif;
131400140604       //?Per la spedizione non devono esserci disposizioni ancora da elaborare
131500140624         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
131600140604          exsr sr_ChkDispo;
131700140624         endif;
131800140530       //?NON TROVATI ERRORI:
131900140530       //?Per GET_ISTRUCO --> attivo il bottone e restituisco i dati della bolla
132000140527         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
132100140527            fnlry00o0.BUTISTRCON='1';
132200170112            if   w_tbo='P' or (arbntc=0 and wtentacons=*blanks);
132300170113            wIdMsg  = 'TIS0859';
132400170112            else;
132500170113            wIdMsg  = 'TIS0858';
132600170112            endif;
132700170113            fnlry00o0.DESISTRCON=rtvMsgLang(wIdMsg:widlingua);
132800140527       // restituisco anche dati relativi alla bolla
132900140527            exsr sr_GetDatiBolla;
133000140527            endif;
133100140523
133200140527       endsr;
133300140523       //--------------------------------------------------------------
133400140523       //?Controlli immagine bolla da WEB con bolla su ARB
133500140523       //--------------------------------------------------------------
133600140523       BEGSR  immagineB;
133700140523       // Chaino estensioni bolla
133800140529       // exsr sr_chainEstBolla;
133900140523       // Errore se il referente di consegna sulla bolla è diverso dall'immagine bolla
134000140523       // che ricevo
134100140523          if wrefbolla<>fnlry00i1.oldrefcons or
134200140523             wtelbolla<>fnlry00i1.oldtelcons or
134300140523             §ar5eml<>fnlry00i1.olddesmail or
134400140528             §ar5tel<>fnlry00i1.olddescell           ;
134500140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
134600140527               wIdMsg   = 'TIS0742';
134700140528               wIdCampo = '*CHGDATA  ';
134800140527               clear wParm;
134900140527               exsr Messaggi;
135000140523               exsr routend;
135100140523          endif;
135200140527         select;
135300140527       // Disposizioni di Appuntamento
135400140527         when fnlry00i1.tipodispo = '1';
135500140527          if wdcreur<>fnlry00i1.olddtcrich or
135600140529             wrbhcr<>fnlry00i1.oldorcrich  or
135700140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
135800140527               wIdMsg   = 'TIS0742';
135900140527               wIdCampo = '*CHGDATA  ';
136000140527               clear wParm;
136100140527               exsr Messaggi;
136200140527               exsr routend;
136300140527          endif;
136400140527       // Disposizioni di consegna ad altro indirizzo
136500140527         when fnlry00i1.tipodispo = '3';
136600140527          if arbrsd+wrsd2 <>fnlry00i1.oldrsd or
136700140527             arbind<>fnlry00i1.oldind or
136800140527             arbcad<>fnlry00i1.oldcad or
136900140527             arblod<>fnlry00i1.oldlod or
137000140527             arbprd<>fnlry00i1.oldprd or
137100140527             arbnzd<>fnlry00i1.oldnzd ;
137200140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
137300140527               wIdMsg   = 'TIS0742';
137400140527               wIdCampo = '*CHGDATA  ';
137500140527               clear wParm;
137600140527               exsr Messaggi;
137700140527               exsr routend;
137800140527          endif;
137900140527         endsl;
138000140523       ENDSR;
138100140523       //--------------------------------------------------------------
138200140523       //?Controllo dettaglio
138300140523       //--------------------------------------------------------------
138400140523       BEGSR  ControllaDett;
138500140523
138600140617       //?Appuntamento
138700140617       // (se non caricate date da proporre per GetIstruCo il bottone non deve
138800140617       //  essere attivato)
138900140609       if (prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO and fnlry00o0.nrdate>0)
139000140624                                                     or fnlry00i1.tipodispo='1';
139100140523          exsr sr_appuntam;
139200140523       endif;
139300140617       //?Fermo Deposito
139400140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='2';
139500140523          exsr sr_FD;
139600140523       endif;
139700140617       //?Altro Indirizzo
139800140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='3';
139900140523          exsr sr_AltroInd;
140000140523       endif;
140100140617       //?Altre Istruzioni
140200140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='4';
140300140523          exsr sr_AltreIstruz;
140400140523       endif;
140500140617       //?Per GETISTRUCO -->  se bottoni tutti in OFF metto in OFF anche bottone
140600140617       //?per le istruzioni di consegna
140700140616         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
140800140616            and fnlry00o0.BUTAPPUNT='0'
140900140616            and fnlry00o0.BUTFERDEP='0'
141000140616            and fnlry00o0.BUTindiriz='0'
141100140616            and fnlry00o0.BUTaltro='0';
141200140616            fnlry00o0.BUTISTRCON='0';
141300140617             wIdMsg   = 'TIS0770';
141400140617             wIdCampo =  '*BOLLA    ';
141500140617             wParm = wbolla;
141600140617             exsr Messaggi;
141700140617             exsr routend;
141800140616         endif;
141900170331       //?Per la spedizione non devono essere già state inserite da utente Internet
142000170331       //?più di tot disposizioni  - (Valido solo per utente non loggato)
142100170331       //?IN Questo caso disattivo tutti i bottoni
142200170331         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO and wksu=*blanks;
142300170331     C/EXEC SQL
142400170331     C+ SELECT count(*) INTO :totdispo FROM tiidc00f where
142500170331     c+ IDCAAS=:arbaas and IDCLNP=:arblnp and IDCNRS=:arbnrs and IDCNSP=:arbnsp
142600170331     C+ and IDCMODACC=' '
142700170331     C/END-EXEC
142800170331          if totdispo>=§ivpmaxd;
142900170331            fnlry00o0.BUTAPPUNT='0'    ;
143000170331            fnlry00o0.BUTFERDEP='0'    ;
143100170331            fnlry00o0.BUTindiriz='0'   ;
143200170331            fnlry00o0.BUTaltro='0';
143300170331            fnlry00o0.BUTISTRCON='0';
143400170331             wIdMsg   = 'TIS0770';
143500170331             wIdCampo =  '*BOLLA    ';
143600170331             wParm = wbolla;
143700170331             exsr Messaggi;
143800170331             exsr routend;
143900170331          endif;
144000170331         endif;
144100140924       //?Per GETISTRUCO -->  se in ON  bottone per appuntam o altro indirizzo
144200140924       //?valorizzo campo messaggio per la preferenza di orario
144300140924         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
144400140924            and (fnlry00o0.BUTAPPUNT='1'  or fnlry00o0.BUTindiriz='1');
144500140930               wtempoh=%div((§VPORST_MW + §VPORST_PW):60);
144600140930               wtempom=%rem((§VPORST_MW + §VPORST_PW):60);
144700140924               wIdMsg   = 'TIS0781';
144800140929               wparm=      %editc(wtempoh:'Z');
144900140924               if wtempom>0;
145000140929                  wIdMsg   = 'TIS0784';
145100140929                  wparm=      wparm + %editc(wtempom:'Z') ;
145200140924               endif;
145300140924               fnlry00o0.MSGPRFORA=rtvMsgLang(wIdMsg:widlingua:wparm);
145400140924         endif;
145500140523
145600140523       endsr;
145700140408
145800140521       //--------------------------------------------------------------
145900140521       //?Chain FNARB
146000140521       //--------------------------------------------------------------
146100140521       BEGSR  sr_chainbolla;
146200140527
146300161128          clear w_tbo ;
146400161129          clear fnarbds;
146500161129          exsr  sr_inzsoloblp;
146600140521       // imposto la chiave di accesso
146700140521          kaas=2000+%dec(%subst(wbolla:1:2):2:0);
146800140521          klnp=%dec(%subst(wbolla:3:3):3:0);
146900140521          knrs=%dec(%subst(wbolla:6:2):2:0);
147000140521          knsp=%dec(%subst(wbolla:8:7):7:0);
147100140521          chain (kaas:klnp:knrs:knsp) fnarb01l;
147200140521          if not %found(fnarb01l);
147300161128       //  NON TROVATA LA BOLLA IN ARRIVO -> la cerco su blp
147400161128           chain (kaas:klnp:knrs:knsp) fnblp01l;
147500161128       //    ERRORE SE:
147600161128       //    Non trovata neanche su blp oppure
147700161128       //    trovata su blp ma flaggata oppure
147800161129       //    filiale non ancora partita con la nuova procedura
147900161128             if not %found(fnblp01l) or
148000161128                  arbft1<>*blanks  or
148100161129                  (%lookup(%editc(arblna:'X'):skFilOk)=0  and
148200161128                   %lookup('999':skFilOk)=0 );
148300140521              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
148400140527              wIdMsg   = 'TIS0735';
148500140527              wIdCampo = wcampo      ;
148600140530              wParm=wbolla;
148700140527              exsr Messaggi;
148800140521              exsr RoutEnd;
148900161128             else;
149000161128             eval w_tbo='P';
149100161128             endif    ;
149200161128          else;
149300161128             eval w_tbo='A';
149400140521          endif;
149500141211       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
149600141211         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
149700141211           fnlry00i1.oldCAD    = arbCAD    ;
149800141211           fnlry00i1.oldLOD    = arbLOD    ;
149900141211           fnlry00i1.oldPRD    = arbPRD    ;
150000141211           fnlry00i1.oldNZD    = arbNzd    ;
150100141211         endif;
150200140521       endsr;
150300140620       //--------------------------------------------------------------
150400140620       //?Operazioni per bolle legate
150500140620       //--------------------------------------------------------------
150600140620       BEGSR  sr_lbl          ;
150700140620          chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl02l;
150800140620          if %found(fnlbl02l);
150900140624       // Per CHK_ISTRUCO --> errore se la bolla da elaborare ha una figlia
151000141223       //    IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
151100141223       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
151200141223       //             wIdMsg   = 'TIS0777';
151300141223       //             wIdCampo = '*NODISPO  ';
151400141223       //             wParm = wbolla;
151500141223       //             exsr Messaggi;
151600141223       //             exsr RoutEnd;
151700141223       //    endif;
151800140620       //    BOLLA CONSEGNATA
151900140620             if arbdcm>0;
152000140620       //       E' LA BOLLA ORIGINALE
152100140620                if LBLAAO=arbaas and LBLLPO=arblnp and
152200140620                   LBLNRO=arbnrs and LBLNSO=arbnsp;
152300140620       //       Non è il Mittente --> Errore: utente non abilitato a dare disposizioni
152400140620                   if wchi <>'M';
152500140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
152600140620                      wIdMsg   = 'TIS0736';
152700140620                      wIdCampo = '*NOAUT    ';
152800140620                      wParm = wbolla;
152900140620                      exsr Messaggi;
153000140620                      exsr RoutEnd;
153100140620                   endif;
153200140620                else;
153300140620       //       NON E' LA BOLLA ORIGINALE
153400140620       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
153500140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
153600140627                      wIdMsg   = 'TIS0770';
153700140620                      wIdCampo = '*NODISPO  ';
153800140620                      wParm = wbolla;
153900140620                      exsr Messaggi;
154000140620                      exsr RoutEnd;
154100140620                endif;
154200140620             endif;
154300140620       //    Recupero i dati dell'ultima figlia
154400140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
154500140620             dow %found(fnlbl02l);
154600140620                chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
154700140620             enddo;
154800161129             clear fnarbds;
154900161129             exsr  sr_inzsoloblp;
155000140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnarb01l;
155100140620             if not %found(fnarb01l);
155200140627       //    Se non trovo l'ultima figlia su arb errore
155300140627       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
155400140627                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
155500140627                      wIdMsg   = 'TIS0777';
155600140627                      wIdCampo = '*NODISPO  ';
155700140627                      wParm = wbolla;
155800140627                      exsr Messaggi;
155900140627                      exsr RoutEnd;
156000161128             else;
156100161128                eval w_tbo='A';
156200140620             endif;
156300141223       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
156400141223         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
156500141223           fnlry00i1.oldCAD    = arbCAD    ;
156600141223           fnlry00i1.oldLOD    = arbLOD    ;
156700141223           fnlry00i1.oldPRD    = arbPRD    ;
156800141223           fnlry00i1.oldNZD    = arbNzd    ;
156900141223         endif;
157000140620          endif;
157100140620       endsr;
157200140523       //--------------------------------------------------------------
157300140523       //?Chain Estensioni bolla
157400140523       //--------------------------------------------------------------
157500140523       BEGSR  sr_chainEstBolla;
157600140523          clear dar5gen;
157700140523          clear wrefbolla;
157800140523          clear wtelbolla;
157900140620          chain (arbaas:arblnp:arbnrs:arbnsp:'GEN') fiar501l ;
158000140523          if %found(fiar501l);
158100140523             dar5gen=ar5uni;
158200140523       // Se presente # nel referente e nel telefono
158300140523       // lo devo togliere
158400140523             if %subst(§ar5ref:1:1)='#';
158500140523                eval wrefbolla=%subst(§ar5ref:2);
158600140523             else;
158700140523                eval wrefbolla=§ar5ref;
158800140523             endif;
158900140523             if %subst(§ar5teld:1:1)='#';
159000140523                eval wtelbolla=%subst(§ar5teld:2);
159100140523             else;
159200140523                eval wtelbolla=§ar5teld;
159300140523             endif;
159400140523          endif;
159500140523          clear dar5emd;
159600140620          chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
159700140523          if %found(fiar501l);
159800140523              dar5emd=ar5uni;
159900140523         endif;
160000140523       ENDSR;
160100140522       //--------------------------------------------------------------
160200140522       //?Recupera dati bolla da passare a chiamante
160300140522       //--------------------------------------------------------------
160400140522       BEGSR  sr_GetDatiBolla;
160500140603       // - orari da passare nella tendina: standard o minimo/massimo in base
160600140603       //   a quanto memorizzato in tabella
160700140923          clear waltroind;
160800140522          exsr sr_orarisr;
160900140604          if §ivptipo='M';
161000140604             fnlry00o0.ORARITNDI=%editc(OOR2MIIS:'X');
161100140604             fnlry00o0.ORARITNDF=%editc(OOR2MXFS:'X');
161200140604          else;
161300140604             fnlry00o0.ORARITNDI=%editc(OOR2STIS:'X');
161400140604             fnlry00o0.ORARITNDF=%editc(OOR2STFS:'X');
161500140604          endif;
161600140923       // - per la tendina con orari di filiale -->
161700140923         clear tisio9ds;
161800140923         do0lna=arblna;
161900140923         do0tric='S';
162000140923         do0tser='C';
162100140923         tisio9r (kpjba:tisio9ds);
162200140923          if §ivptipo='M';
162300140923             fnlry00o0.ORARITNFI=%editc(DO0OMIIS:'X');
162400140923             fnlry00o0.ORARITNFF=%editc(DO0OMXFS:'X');
162500140923          else;
162600140923             fnlry00o0.ORARITNFI=%editc(DO0OSTIS:'X');
162700140923             fnlry00o0.ORARITNFF=%editc(DO0OSTFS:'X');
162800140923          endif;
162900140603          fnlry00o0.MMINTERVC=%editc(§ivpmini:'X');
163000161206       //?DATE DI CONSEGNA DA PROPORRE: -------------------
163100161205       // 1) Giorni di attesa prima di aprire giacenza:
163200161205       //    verifico se previsti giorni personalizzati per cliente
163300140605         exsr sr_PersGGiac;
163400161214       // 2) Verifico se effetuato almeno un tentativo di consegna
163500161214         if w_tbo='A' ;
163600140709            exsr sr_RepPct   ;
163700140709         ENDIF;
163800161206       //?Data Minima . . . . . .:
163900161206       //?Data Da cui inizia il conteggio del giorno limite per l'apertura giacenza:
164000161205       // vengono proposte le date fino al giorno limite previsto per apertura giacenza
164100161216       exsr sr_date;
164200170217       exsr sr_limitdcrMod;
164300161206
164400161206       // 3) Ciclo per caricare campo delle date consegna richiesta da proporre
164500161206       //    Parto dalla data da cui iniza il conteggio per l'apertura giac (WDATA) e
164600161206       //    scarto le date inferiori alla data minima calcolata (WDATAM)
164700140605         clear ix;
164800140923         clear iy;
164900140605         for I=1 to wlimitdcr;
165000140606       //   se cade in un giorno festivo incremento la data di un giorno
165100140606            exsr sr_datafes;
165200140606       //   Propongo la data di oggi solo se possibile la riconsegna in giornata
165300140709            if (wdata=dataoggi and oor2fric='S'
165400161206                and §ivparic='S' and wtentacons='1') or
165500161206                (wdata<>dataoggi and wdata>=wdatam);
165600140609       //   Memorizzo la data, purchè, in presenza di data cons.rich "Dopo il",
165700140609       //   la stessa non sia <= della data "Dopo Il"
165800140710             if arbtcr<>'D' or (arbtcr='D' and wdata>arbdcr);
165900140606               ix+=1;
166000140606               fnlry00o0.schdcr(ix)=%char(%date(wdata:*iso):*eur);
166100140923       //      la data per l'indirizzo alternativo non deve mai essere "oggi"
166200140923               if wdata<>dataoggi;
166300140923                  iy+=1;
166400140923                  fnlry00o0.schdcri(iy)=%char(%date(wdata:*iso):*eur);
166500140923                  fnlry00o0.nrdatei+=1;
166600140923               endif;
166700140606       //      Data da Consigliare --> quella successiva a oggi oppure
166800140606       //      quella della bolla se maggiore
166900140606               if fnlry00o0.dtconsric=*blanks  and wdata<>dataoggi ;
167000140606                   fnlry00o0.dtconsric=%char(%date(wdata:*iso):*eur);
167100140606                   if arbdcr>0 and arbtcr=*blanks and arbdcr>wdata;
167200140606                      fnlry00o0.dtconsric=%char(%date(arbdcr:*iso):*eur);
167300140606                   endif;
167400140606               endif;
167500140606       //   Incremento contatore date da passare a chiamante
167600140606               fnlry00o0.nrdate+=1;
167700140609             endif;
167800140606            endif;
167900140606       //   Incremento la data di un giorno
168000140606            exsr sr_IncreData;
168100140605         endfor;
168200140522       // - consegna richiesta: data/ora/tipo
168300140606       //if arbdcr>0 ;
168400140606       //   fnlry00o0.DTCONSRIC=wdcreur;
168500140606       //endif;
168600140606       //if arbhcr>0;
168700140606       //   fnlry00o0.ORCONSRIC=%editc(arbhcr:'X');
168800140606       //endif;
168900140606       //fnlry00o0.TPCONSRIC=arbtcr;
169000140613       // indirizzo e-mail e n. telefono per SMS -----------
169100140620         chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
169200140522         if %found(fiar501l);
169300140522             dar5emd=ar5uni;
169400140522             fnlry00o0.destmail=§ar5eml;
169500140522             fnlry00o0.destcell=§ar5tel;
169600140522         endif;
169700140613       // Messaggio di avviso per indicare che il FD deve essere autorizzato dal mitt. ------------
169800140522         if arbffd=*blanks and arbgma<>*blanks;
169900140529            exsr sr_tab7r;
170000140522            if §7rfd='S' and wchi='D';
170100140522               wIdMsg   = 'TIS0733';
170200140617               fnlry00o0.MSGFDMITT=rtvMsgLang(wIdMsg:widlingua);
170300140522            endif;
170400140522         endif;
170500140522       endsr;
170600140606       //--------------------------------------------------------------
170700140606       //?Controllo data se festiva + restituzione data prima data non festiva
170800140606       //--------------------------------------------------------------
170900140606       BEGSR  sr_DataFes;
171000140606       wfes='1';
171100140606       dow wfes='1';
171200161206       chain (0:wfgslna:%subdt(%date(wdata):*years):
171300161206                        %subdt(%date(wdata):*MONTHS)) azcln01l;
171400161206          if %found(azcln01l) and mat(%subdt(%date(wdata):*days)) = 'F'
171500161206                               or pom(%subdt(%date(wdata):*days)) = 'F';
171600140606       // data festiva: la incremento di un giorno
171700140606             exsr sr_incredata;
171800140606          else;
171900140606            clear wfes;
172000140606          endif;
172100140606       enddo;
172200140606       endsr;
172300140606       //--------------------------------------------------------------
172400140606       //?Incremento data di un giorno
172500140606       //--------------------------------------------------------------
172600140606       BEGSR  sr_incredata;
172700161206             dataiso=%date(wdata);
172800140606             dataiso=dataiso+%days(1);
172900161206             wdata=%dec(dataiso);
173000140606       endsr;
173100140521       //--------------------------------------------------------------
173200140521       //?Controlli per abilitare la richiesta di appuntamento
173300140521       //--------------------------------------------------------------
173400140521       BEGSR  sr_appuntam;
173500140528
173600140528       // Non ammesso per spedizioni in fermo deposito
173700140528         if arbffd= 'S';
173800140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
173900140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
174000140528               leavesr;
174100140528            else;
174200140528       //?PUT_ISTRUCO --> uscita da pgm con errore
174300140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174400141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
174500140528              wIdMsg   = 'TIS0743';
174600140528              wIdCampo = '*NOAPPUNT ';
174700140530              wParm = wbolla ;
174800141006         else;
174900141006              wIdMsg   = 'TISA743';
175000141006              wIdCampo = '*NOAPPUNT ';
175100141006              clear wParm  ;
175200141006         endif;
175300140528              exsr Messaggi;
175400140528              exsr routend;
175500140528            endif;
175600140528         endif;
175700140528
175800140528       //?GET_ISTRUCO --> attivazione bottone per appuntamento
175900140528         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
176000140528            fnlry00o0.BUTAPPUNT='1';
176100140528         else;
176200140528       //?PUT_ISTRUCO --> controllo dati ricevuti
176300140528            exsr sr_ctrApp;
176400140528         endif;
176500140528
176600140521       endsr;
176700140522       //--------------------------------------------------------------
176800140522       //?Controlli per abilitare la richiesta di Fermo Deposito
176900140522       //--------------------------------------------------------------
177000140522       BEGSR  sr_FD      ;
177100140522
177200140528       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
177300140528          if arbffd = 'S';
177400140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
177500140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
177600140528               leavesr;
177700140528            else;
177800140528       //?PUT_ISTRUCO --> uscita da pgm con errore
177900141223            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
178000140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
178100141223       //if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
178200140528              wIdMsg   = 'TIS0743';
178300140528              wIdCampo = '*NOFD     ';
178400140530              wParm = wbolla;
178500141223       //else;
178600141223       //     wIdMsg   = 'TISA743';
178700141223       //     wIdCampo = '*NOFD     ';
178800141223       //     clear wParm;
178900141223       //endif;
179000140528              exsr Messaggi;
179100140528              exsr routend;
179200141223            endif;
179300140528            endif;
179400140528          else;
179500140530       //?GET_ISTRUCO -->
179600141008            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
179700141008       //   and fnlry00o0.MSGFDMITT=*blanks;
179800140616       //?GET_ISTRUCO -->> Restituisco, insieme al bottone attivato anche messaggio
179900140616       //?                 riguardante le codizioni di ritiro in FD
180000140616       //?                 Solo se msgfdmit=*blanks
180100140616             if fnlry00o0.msgfdmitt=*blanks;
180200141008                fnlry00o0.BUTFERDEP='1';
180300161216              if w_tbo='A' and arbdam>0;
180400140924                exsr sr_wdesgio;
180500140617               wparm=wdesgio + %editc(WlimitFD:'X') + wfgslnad;
180600140616               wIdMsg   = 'TIS0768';
180700161216              else;
180800161216               clear wparm;
180900161216               wIdMsg   = 'TIS0857';
181000161216              endif;
181100140617               fnlry00o0.MSGFDCOND=rtvMsgLang(wIdMsg:widlingua:wparm);
181200141008             else;
181300141008                fnlry00o0.BUTFERDEP='2';
181400140616             endif;
181500140616            endif;
181600140606       //?PUT_ISTRUCO --> Controllo dati ricevuti
181700140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
181800140624               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
181900140616       //?                Errore al destinatario se è richiesta autorizzaz.
182000140616       //?                da parte del mittente
182100140616             if arbgma<>*blanks;
182200140616                exsr sr_tab7r;
182300140616                if §7rfd='S' and wchi='D';
182400140616                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
182500140616                   wIdMsg   = 'TIS0733';
182600140616                   wIdCampo = '*NOFD     ';
182700140616                   clear wParm;
182800140616                   exsr Messaggi;
182900140616                   exsr routend;
183000140616                endif;
183100140616             endif;
183200140606             exsr sr_ctrUNI;
183300140606            endif;
183400140530          endif;
183500140522       endsr;
183600140522       //--------------------------------------------------------------
183700140522       //?Controlli per abilitare la richiesta di Altro Indirizzo
183800140522       //--------------------------------------------------------------
183900140522       BEGSR  sr_altroind;
184000140522
184100140616       if §ivpdind='S';
184200140617       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
184300140617          if arbffd = 'S';
184400140617       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
184500140617            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
184600140617               leavesr;
184700140617            else;
184800140617       //?PUT_ISTRUCO --> uscita da pgm con errore
184900140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
185000141007           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
185100140617              wIdMsg   = 'TIS0743';
185200140617              wIdCampo = '*NOALTRIND';
185300140617              wParm = wbolla;
185400141007           else;
185500141007              wIdMsg   = 'TISA743';
185600141007              wIdCampo = '*NOALTRIND';
185700141007              clear wparm ;
185800141007           endif;
185900140617              exsr Messaggi;
186000140617              exsr routend;
186100140617            endif;
186200140617          endif;
186300140619       //NON AMMESSO AL DESTINATARIO SE NON ABILITATO MEDIANTE TABELLA 7W
186400140619         clear ds7W;
186500140619         if wchi='D' and arbgga<>*blanks;
186600140619            w008a=ARBGGA;
186700140619            chain (1 : '7W' : W008A) tabel00f;
186800140619            if %found(tabel00f);
186900140619               ds7w=tbluni;
187000140619            endif;
187100140619         endif;
187200140619         if wchi='D' and §7wdes<>'S';
187300140619       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
187400140918       //?                18/09/2014 --> attivo il bottone ma poi verrà dato errore
187500140918       //?                Il concetto è: il cambio di indirizzo è gestito (e quindi il
187600140918       //?                bottone deve essere attivo) ma non può essere richiesto da
187700140918       //?                destinatario se non autorizzato (e quindi poi viene dato errore)
187800140918            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
187900141007               fnlry00o0.BUTindiriz='2';
188000141007               wIdMsg   = 'TIS0796';
188100141007               fnlry00o0.MSGAUTIND=rtvMsgLang(wIdMsg:widlingua);
188200141007               leavesr;
188300140619            else;
188400140619       //?PUT_ISTRUCO --> uscita da pgm con errore
188500140619              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
188600140619              wIdMsg   = 'TIS0776';
188700140619              wIdCampo = '*NOALTRIND';
188800140924              clear wparm;
188900140924       //     wParm = wbolla;
189000140619              exsr Messaggi;
189100140619              exsr routend;
189200140619            endif;
189300140619         endif;
189400140522         fnlry00o0.BUTindiriz='1';
189500140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
189600140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
189700140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
189800140611       //?Controllo dati comuni a tutti tipi di disposizione
189900140611             exsr sr_ctrUNI;
190000140611
190100140609             exsr sr_ctrind;
190200140609            endif;
190300140616       else;
190400140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
190500140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
190600141016               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
190700141016                 prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
190800141016                 prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
190900141016                 exsr RoutEnd;
191000140616            endif;
191100140616       endif;
191200140522       endsr;
191300140522       //--------------------------------------------------------------
191400140522       //?Controlli per abilitare la richiesta di Altre Istruzioni
191500140522       //--------------------------------------------------------------
191600140522       BEGSR  sr_AltreIstruz;
191700140522
191800140616       if §ivpdalr='S';
191900140522         fnlry00o0.BUTaltro='1';
192000140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
192100140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
192200140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
192300140609             exsr sr_ctrUNI;
192400140609            endif;
192500140616       else;
192600140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
192700140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
192800140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
192900140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
193000140617              prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
193100140617              exsr RoutEnd;
193200140616            endif;
193300140616       endif;
193400140522       endsr;
193500140522
193600140523       //--------------------------------------------------------------
193700140523       //?Controllo Disposizioni di Appuntamento
193800140523       //--------------------------------------------------------------
193900140523       BEGSR  sr_ctrApp     ;
194000140611       //?Controllo dati comuni a tutti tipi di disposizione
194100140611          exsr sr_ctrUNI;
194200140528       //?Consegna richiesta
194300140528          exsr sr_ctrConsRich;
194400140523       endsr;
194500140528       //--------------------------------------------------------------
194600140528       //?Controllo Data/ora/tipo consegna richiesta
194700140528       //--------------------------------------------------------------
194800140528       BEGSR  sr_CtrConsRich;
194900140923          clear waltroind;
195000140606          exsr sr_Orarisr;
195100140528       // DATA CONSEGNA RICHIESTA:
195200140528          exsr sr_ctrDCR;
195300140528       // ORA  CONSEGNA RICHIESTA:
195400140528       if fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
195500140528       // Deve contenere solo numeri
195600140528         if %check(digits:fnlry00i1.neworcrich)>0;
195700140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
195800140528              wIdMsg   = 'TIS0746';
195900140528              wIdCampo = 'NEWORCRICH';
196000140530              wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
196100140530                      %subst(fnlry00i1.neworcrich:3:2);
196200140528              exsr Messaggi;
196300140528         else;
196400140709       // Se data consegna richiesta diversa da oggi (no riconsegna)
196500140709       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
196600140528              wora = %dec(fnlry00i1.neworcrich:4:0);
196700141216              if wdcr<>wdataconfr;
196800140709                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
196900140709                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
197000140709                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
197100140709                       wIdMsg   = 'TIS0747';
197200140709                       wIdCampo = 'NEWORCRICH';
197300140709                       wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
197400140709                       %subst(fnlry00i1.neworcrich:3:2);
197500140709                       exsr Messaggi;
197600140709                 endif;
197700140709              else;
197800140709       //  Se Data Consegna Richiesta=oggi (riconsegna) l'orario deve essere incluso negli orari
197900140610       //  inizio/fine previsti per la riconsegna
198000140709                 if  wora < OOR2RIDS  or
198100140709                     wora > OOR2RIAS ;
198200140709                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
198300140709                        wIdMsg   = 'TIS0778';
198400140709                        wIdCampo = 'NEWORCRICH';
198500140709                        wParm = %editw(oor2rids:'  :  ') +
198600140709                                %editW(oor2rias:'  :  ')  ;
198700140709                        exsr Messaggi;
198800140709                 endif;
198900140709              endif;
199000140528         endif;
199100140528       endif;
199200140529       // PREFERENZA MATTINO/POMERIGGIO
199300140529       if fnlry00i1.prconsric<>*blanks and
199400140529          fnlry00i1.prconsric<>'M' and
199500140529          fnlry00i1.prconsric<>'P'               ;
199600140528                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
199700140528                    wIdMsg   = 'TIS0749';
199800140529                    wIdCampo = 'PRCONSRIC ';
199900140528                    clear wParm;
200000140528                    exsr Messaggi;
200100140528       endif;
200200140710       // NO RICONSEGNA:
200300140710       // Verifico preferenza m/p con gli orari standard o min/max in base
200400140710       // all'ora limite del mattino
200500141216       if wdcr<>wdataconfr;
200600140710         if (fnlry00i1.prconsric='P' and §ivptipo='S' and §ivporlm>=OOR2STFS) or
200700140710            (fnlry00i1.prconsric='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
200800140710            (fnlry00i1.prconsric='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS) or
200900140710            (fnlry00i1.prconsric='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
201000140710            If fnlry00i1.prconsric='P';
201100140710                widmsg='TIS0676';
201200140710            else;
201300140710                widmsg='TIS0667';
201400140710            endif;
201500140710            matpom=rtvMsgLang(widmsg : widlingua);
201600140710                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
201700141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
201800140710                      wIdMsg   = 'TIS0760';
201900140710                      wIdCampo = 'PRCONSRIC ';
202000140710                      wParm = MatPom + wbolla;
202100141006         else;
202200141006                      wIdMsg   = 'TISA760';
202300141006                      wIdCampo = 'PRCONSRIC ';
202400141006                      wParm = MatPom ;
202500141006         endif;
202600140710                      exsr Messaggi;
202700140710         ENDIF;
202800140710       ELSE;
202900140710       // SI RICONSEGNA:
203000140710       // Verifico preferenza m/p con gli orari previsti per la riconsegna
203100140710       // in base all'ora limite del mattino
203200140710          if (fnlry00i1.prconsric='P' and §ivporlm>=OOR2rias) or
203300140710             (fnlry00i1.prconsric='M' and §ivporlm<OOR2rids);
203400140710             If fnlry00i1.prconsric='P';
203500140710                 widmsg='TIS0676';
203600140710             else;
203700140710                 widmsg='TIS0667';
203800140710             endif;
203900140710             matpom=rtvMsgLang(widmsg : widlingua);
204000140710                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
204100140710                       wIdMsg   = 'TIS0779';
204200140710                       wIdCampo = 'PRCONSRIC ';
204300140710                       wParm = MatPom ;
204400140710                       exsr Messaggi;
204500140710          ENDIF;
204600140606       endif;
204700140529       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
204800140529       if fnlry00i1.prconsric<>*blanks and
204900140529          fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
205000140529                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
205100140530                    wIdMsg   = 'TIS0753';
205200140530                    wIdCampo = 'NEWORCRICH';
205300140530                    clear wParm;
205400140529                    exsr Messaggi;
205500140530                    wIdCampo = 'PRCONSRIC ';
205600140529                    exsr Messaggi;
205700140529       endif;
205800140528       //  Se Campi tutti vuoti errore
205900140529       //if (fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros) and
206000140529       //   (fnlry00i1.neworcrich=*blanks or fnlry00i1.neworcrich=*zeros) and
206100140529       //    fnlry00i1.prconsric=*blanks;
206200140529       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
206300140529       //             wIdMsg   = 'TIS0748';
206400140529       //             wIdCampo = '*NOSELEZ';
206500140529       //             clear wParm;
206600140529       //             exsr Messaggi;
206700140529       //endif;
206800140528       endsr;
206900140528       //--------------------------------------------------------------
207000140528       //?Controllo Data Consegna richiesta
207100140528       //--------------------------------------------------------------
207200140528       BEGSR  sr_ctrDCR     ;
207300140528       // DATA CONSEGNA RICHIESTA:
207400140529       // Obbligatoria
207500140529       if fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros;
207600140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
207700140529             wIdMsg   = 'TIS0130';
207800140529             wIdCampo = 'NEWDTCRICH';
207900140529             clear wParm;
208000140529             exsr Messaggi;
208100140529             leavesr;
208200140529       endif;
208300140528       //  Controllo correttezza formale
2084001405282        monitor  ;
208500140528         dataiso=%date(fnlry00i1.newdtcrich:  *eur)   ;
208600140528         on-error ;
208700140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
208800140528             wIdMsg   = 'TIS0121';
208900140528             wIdCampo = 'NEWDTCRICH';
209000140528             clear wParm;
209100140528             exsr Messaggi;
209200140528             leavesr;
209300140528 2       endmon  ;
209400140528
209500140528       wdcr=%dec(dataiso);
209600140528
209700140528       //  Non inferiore a oggi
209800140528       if wdcr < dataoggi;
209900140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
210000140528             wIdMsg   = 'TIS0744';
210100140528             wIdCampo = 'NEWDTCRICH';
210200140611             wParm=%char(%date(wdcr:*iso):*eur);
210300140528             exsr Messaggi;
210400140528             leavesr;
210500140528       endif;
210600161214       if w_tbo='A' ;
210700161202        exsr sr_reppct;
210800161202       endif;
210900161130       //  Non inferiore a data prevista consegna se spedizione da partire o senza
211000161129       //  Tentativi di consegna
211100161202       if (w_tbo='P' or (arbntc=0 and wtentacons=' ')) and wdcr < dataPrvC;
211200161129             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
211300170113             wIdMsg   = 'TIS0860';
211400161129             wIdCampo = 'NEWDTCRICH';
211500161129             wParm=%char(%date(wdcr:*iso):*eur);
211600161129             exsr Messaggi;
211700161129             leavesr;
211800161129       endif;
211900140709       //  Uguale ad oggi solo se possibile la riconsegna in giornata
212000141215       //  Per chkistruco considero "data oggi" la data in cui è stata
212100141215       //  fatta la  richiesta
212200141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
212300141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
212400141215         else;
212500141218              wdataconfr=dataoggi;
212600141215         endif;
212700141215       if wdcr=wdataconfr;
212800161202       //if oor2fric='S' and §ivparic='S';
212900161202       // exsr sr_reppct;
213000161202       //endif;
213100140709         if oor2fric='N' or §ivparic<>'S' or wtentacons=' ';
213200140606             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
213300140606             wIdMsg   = 'TIS0759';
213400140606             wIdCampo = 'NEWDTCRICH';
213500140606             clear wparm;
213600140606             exsr Messaggi;
213700140606             leavesr;
213800140709         endif;
213900140606       endif;
214000140528       //  Non superiore ai Giorni limite giac. con data cons.rich.
214100140528       //      Verifico se presenti limiti personalizzati per il cliente
214200140528
214300140605       exsr sr_PersGGiac;
214400161206       //Calcolo giorni lavorativi fra data inizio conteggio e data consegna richiesta
214500161216       exsr sr_date;
214600170217       exsr sr_limitdcrMod;
214700161214       datada=wdata;
214800140528       dataa=wdcr;
214900140606       clear tfptfa;
215000140610       p_tfa =wfgslna;
215100140606       xsrlav8(wdat8:tfptfa);
215200140528       if ggl > wlimitdcr;
215300140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
215400140528             wIdMsg   = 'TIS0745';
215500140528             wIdCampo = 'NEWDTCRICH';
215600140611             wParm=%char(%date(wdcr:*iso):*eur);
215700140528             exsr Messaggi;
215800140528             leavesr;
215900140528       endif;
216000140606       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
216100140528       //  Controllo anche con la data consegna richiesta originale
216200140609       if (arbtcr='D' and wdcr<=arbdcr) or
216300140609         (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
216400140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
216500140529             wIdMsg   = 'TIS0750';
216600140528             wIdCampo = 'NEWDTCRICH';
216700140606             if arbtcr='D' ;
216800140606             dataeur=%date(%dec(arbdcr:8:0):*iso);
216900140606             else;
217000140606             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
217100140606             endif;
217200140611             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
217300140528             exsr Messaggi;
217400140528             leavesr;
217500140528       endif;
217600140605       // Non deve cadere in giorno festivo
217700140605       chain (0:wfgslna:%subdt(%date(wdcr):*years):
217800140605                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
217900140605       if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
218000140605                            or pom(%subdt(%date(wdcr):*days)) = 'F';
218100140605             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
218200140605             wIdMsg   = 'TIS0758';
218300140605             wIdCampo = 'NEWDTCRICH';
218400140611             wParm=%char(%date(wdcr:*iso):*eur);
218500140605             exsr Messaggi;
218600140605             leavesr;
218700140605       endif;
218800140528
218900140528       endsr;
219000140605       //--------------------------------------------------------------
219100140605       //?Rperimento eventuale personalizzazione giorni attesa prima di apert.giac
219200140605       //--------------------------------------------------------------
219300140605       BEGSR  sr_PersGGiac  ;
219400140606       // Faccio come fa fnlg80r --> prima prova on arbksc
219500140606       // e se non trova con questo prova poi con arbccm
219600140606       clear ds7u;
219700140606       w008a=%editc(arbksc:'X')+'Q';
219800140606       chain (1:'7U':w008a) tabel00f;
219900140606       if %found (tabel00f);
220000140606          ds7u=tbluni;
220100140606       else;
220200140606          w008a=%editc(arbCCM:'X')+'Q';
220300140606          chain (1:'7U':w008a) tabel00f;
220400140606          if %found (tabel00f);
220500140606             ds7u=tbluni;
220600140606          endif;
220700140606       endif;
220800140606       if §7uldcr>'00';
220900140606          wlimitdcr=%dec(§7uldcr:2:0);
221000140606       endif;
221100170217       wlimitdcr_s=wlimitdcr;
221200140605       // Cerco con il ccm se presente altrimenti con KSC
221300140606       // clear ds7u;
221400140606       //if arbccm > 0;
221500140606       //   w008a=%editc(arbCCM:'X')+'Q';
221600140606       //   chain (1:'7U':w008a) tabel00f;
221700140606       //   if %found (tabel00f);
221800140606       //      ds7u=tbluni;
221900140606       //   endif;
222000140606       //else;
222100140606       //   w008a=%editc(arbKSC:'X')+'Q';
222200140606       //   chain (1:'7U':w008a) tabel00f;
222300140606       //   if %found (tabel00f);
222400140606       //      ds7u=tbluni;
222500140606       //   endif;
222600140606       //endif;
222700140606       //if §7uldcr>'00';
222800140606       //   wlimitdcr=%dec(§7uldcr:2:0);
222900140606       //endif;
223000140605       endsr;
223100170217       //--------------------------------------------------------------
223200170217       //?Aggiustamento giorni limite per apertura giacenza per filiali
223300170217       //?con tempi cliente>tempi interni
223400170217       //--------------------------------------------------------------
223500170217       BEGSR  sr_limitdcrMod;
223600170217       // Se data calcolata > data odierna e tempi clienti > tempi interni
223700170217       // tolgo un giorno al numero dei giorni limite previsti per l'apertura giacenza
223800170217       // altrimenti verrebbe porposta una data in più nella quale è già possibile
223900170217       // aprire giacenza
224000170217               wlimitdcr=wlimitdcr_s;
224100170217               if wdata>dataoggi and d98ttc>d98tti;
224200170217                if %rem(d98ttc:24)=0 or (d98ttc-d98tti)>12  ;
224300170217                  wlimitdcr-=1;
224400170217                endif  ;
224500170217               endif;
224600170217       endsr;
224700140609       //--------------------------------------------------------------
224800140609       //?Controlli Disposizioni di consegna ad altro indirizzo
224900140609       //--------------------------------------------------------------
225000140609       BEGSR  sr_ctrind;
225100141006          clear wolna;
225200140610       // Per il controllo dell'indirizzo (CAP escluso) non viene richiamato
225300140610       // fnlv14r (quest'ultimo, se cap vuoto richiama l'interrogazione
225400140609       // del cappario e controlla la nazione che qui non serve)
225500140609       //?Ragione sociale, Indirizzo, Località e Provincia --> obbligatori
225600140610          if fnlry00i1.newrsd = *blanks;
225700140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
225800140610             wIdMsg   = 'TIS0761';
225900140610             wIdCampo = 'NEWRSD    ';
226000140610             clear wParm;
226100140610             exsr Messaggi;
226200141006             leavesr;
226300140610          endif;
226400140610          if fnlry00i1.newind = *blanks;
226500140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
226600140610             wIdMsg   = 'TIS0762';
226700140610             wIdCampo = 'NEWIND    ';
226800140610             clear wParm;
226900140610             exsr Messaggi;
227000141006             leavesr;
227100140610          endif;
227200140610          if fnlry00i1.newcad = *blanks;
227300140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
227400140610             wIdMsg   = 'TIS0763';
227500140610             wIdCampo = 'NEWCAD    ';
227600140610             clear wParm;
227700140610             exsr Messaggi;
227800141006             leavesr;
227900140610          endif;
228000140610          if fnlry00i1.newlod = *blanks;
228100140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
228200140610             wIdMsg   = 'TIS0764';
228300140610             wIdCampo = 'NEWLOD    ';
228400140610             clear wParm;
228500140610             exsr Messaggi;
228600141006             leavesr;
228700140610          endif;
228800140610          if fnlry00i1.newprd = *blanks;
228900140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
229000140610             wIdMsg   = 'TIS0765';
229100140610             wIdCampo = 'NEWPRD    ';
229200140610             clear wParm;
229300140610             exsr Messaggi;
229400141006             leavesr;
229500140609          endif;
229600140610       //?La nazione deve essere vuota
229700140610          if fnlry00i1.newnzd <> *blanks;
229800140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
229900140610             wIdMsg   = 'TIS0384';
230000140610             wIdCampo = 'NEWNZD    ';
230100140610             clear wParm;
230200140610             exsr Messaggi;
230300141006             leavesr;
230400140610          endif;
230500140611       //?Richiamo pgm per controllo CAP e calcolo instradamento
230600140610          if fnlry00i1.newcad<>fnlry00i1.oldcad or
230700140610             fnlry00i1.newlod<>fnlry00i1.oldlod or
230800140610             fnlry00i1.newprd<>fnlry00i1.oldprd or
230900140610             fnlry00i1.newnzd<>fnlry00i1.oldnzd         ;
231000140611             exsr sr_call_fispe02r;
231100141003          else;
231200141003             if fnlry00i1.newrsd=fnlry00i1.oldrsd and
231300141003                fnlry00i1.newind=fnlry00i1.oldind;
231400141003       // Errore se non eseguita alcuna variazione di indirizzo
231500141003                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
231600141003                wIdMsg   = 'TIS0794';
231700141003                wIdCampo = '*NOCHGIND ';
231800141003                clear wParm;
231900141003                exsr Messaggi;
232000141006                leavesr;
232100141003             endif;
232200140610       endif;
232300140930
232400140930         waltroind='1';
232500140930         exsr sr_orarisr ;
232600140930
232700141016       //?SE NO DIROTTAMENTO
232800141016       //if wolna = 0 ;
232900140923       //?DATA/ORA CONSEGNA RICHIESTA
233000140923       if fnlry00i1.newdtcrici<>*blanks and fnlry00i1.newdtcrici<>*zeros;
233100141006       // Se richiesto dirottamento non si può inserire la consegna richiesta
233200141016         if  wolna>0;
233300141016                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
233400141016                wIdMsg   = 'TIS0795';
233500141016                wIdCampo = 'NEWDTCRICI';
233600141016                clear wParm;
233700141016                exsr Messaggi;
233800141016                leavesr;
233900141016         endif;
234000140923       // DATA CONSEGNA RICHIESTA:
234100140923       //  Controllo correttezza formale
234200140923         monitor  ;
234300140923         dataiso=%date(fnlry00i1.newdtcrici:  *eur)   ;
234400140923         on-error ;
234500140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
234600140923             wIdMsg   = 'TIS0121';
234700140923             wIdCampo = 'NEWDTCRICI';
234800140923             clear wParm;
234900140923             exsr Messaggi;
235000140923             leavesr;
235100140923         endmon  ;
235200140923
235300140923         wdcr=%dec(dataiso);
235400141215       //  Per chkistruco considero "data oggi" la data in cui è stata
235500141215       //  fatta la  richiesta
235600141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
235700141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
235800141215         else;
235900141218              wdataconfr=dataoggi;
236000141215         endif;
236100140923
236200140923       //  Non inferiore o uguale a oggi
236300141215         if wdcr <= wdataconfr;
236400140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
236500140929             wIdMsg   = 'TIS0759';
236600140923             wIdCampo = 'NEWDTCRICI';
236700140923             clear wparm;
236800140923             exsr Messaggi;
236900140923             leavesr;
237000140923         endif;
237100161130       //  Non inferiore a data prevista consegna se spedizione da partire o senza
237200161129       //  Tentativi di consegna
237300161214       if w_tbo='A' ;
237400161202          exsr sr_RepPct;
237500161202       endif;
237600161202       if (w_tbo='P' or (arbntc=0 and wtentacons=' ')) and wdcr < dataPrvC;
237700161129             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
237800170113             wIdMsg   = 'TIS0860';
237900161129             wIdCampo = 'NEWDTCRICI';
238000161129             wParm=%char(%date(wdcr:*iso):*eur);
238100161129             exsr Messaggi;
238200161129             leavesr;
238300161129       endif;
238400140923       //  Non superiore ai Giorni limite giac. con data cons.rich.
238500140923       //      Verifico se presenti limiti personalizzati per il cliente
238600140923
238700140923         exsr sr_PersGGiac;
238800161215       //Calcolo giorni lavorativi fra data inizio conteggio e data consegna richiesta
238900161216       exsr sr_date;
239000170217       exsr sr_limitdcrMod;
239100161215       datada=wdata;
239200140923         dataa=wdcr;
239300140923         clear tfptfa;
239400140923         p_tfa =wfgslna;
239500140923         xsrlav8(wdat8:tfptfa);
239600140923         if ggl > wlimitdcr;
239700140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
239800140923             wIdMsg   = 'TIS0745';
239900140923             wIdCampo = 'NEWDTCRICI';
240000140923             wParm=%char(%date(wdcr:*iso):*eur);
240100140923             exsr Messaggi;
240200140923             leavesr;
240300140923         endif;
240400140923       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
240500140923       //  Controllo anche con la data consegna richiesta originale
240600140923         if (arbtcr='D' and wdcr<=arbdcr) or
240700140923           (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
240800140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
240900140923             wIdMsg   = 'TIS0750';
241000140923             wIdCampo = 'NEWDTCRICI';
241100140923             if arbtcr='D' ;
241200140923             dataeur=%date(%dec(arbdcr:8:0):*iso);
241300140923             else;
241400140923             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
241500140923             endif;
241600140923             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
241700140923             exsr Messaggi;
241800140923             leavesr;
241900140923         endif;
242000140923       // Non deve cadere in giorno festivo
242100140923         chain (0:wfgslna:%subdt(%date(wdcr):*years):
242200140923                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
242300140923         if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
242400140923                            or pom(%subdt(%date(wdcr):*days)) = 'F';
242500140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
242600140923             wIdMsg   = 'TIS0758';
242700140923             wIdCampo = 'NEWDTCRICI';
242800140923             wParm=%char(%date(wdcr:*iso):*eur);
242900140923             exsr Messaggi;
243000140923             leavesr;
243100140923         endif;
243200140923       // ORA CONSEGNA RICHIESTA:
243300140923         if fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
243400140923       // Deve contenere solo numeri
243500140923           if %check(digits:fnlry00i1.neworcrici)>0;
243600140923                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
243700140923                wIdMsg   = 'TIS0746';
243800140923                wIdCampo = 'NEWORCRICH';
243900140923                wParm = %subst(fnlry00i1.neworcrici:1:2) + ':' +
244000140923                        %subst(fnlry00i1.neworcrici:3:2);
244100140923                exsr Messaggi;
244200140923           else;
244300140923       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
244400140923                wora = %dec(fnlry00i1.neworcrici:4:0);
244500140923                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
244600140923                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
244700140923                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
244800140923                       wIdMsg   = 'TIS0780';
244900140923                       wIdCampo = 'NEWORCRICI';
245000140925       //              wParm = fnlry00i1.newlod;
245100140923                       if §ivptipo='S';
245200140925                          wParm = fnlry00i1.newlod +
245300140925                                  %editw(oor2stis:'  :  ') +
245400140925                                  %editw(oor2stfs:'  :  ') ;
245500140923                       else;
245600140925                          wParm = fnlry00i1.newlod +
245700140925                                  %editw(oor2miis:'  :  ') +
245800140925                                 %editw(oor2mxfs:'  :  ') ;
245900140923                       endif;
246000140923                       exsr Messaggi;
246100140923                 endif;
246200140923           endif;
246300140923         endif;
246400140923       // PREFERENZA MATTINO/POMERIGGIO
246500140923         if fnlry00i1.prconsrici<>*blanks and
246600140923            fnlry00i1.prconsrici<>'M' and
246700140923            fnlry00i1.prconsrici<>'P'               ;
246800140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
246900140923                    wIdMsg   = 'TIS0749';
247000140923                    wIdCampo = 'PRCONSRICI';
247100140923                    clear wParm;
247200140923                    exsr Messaggi;
247300140923         endif;
247400140923       // Verifico preferenza m/p con gli orari standard o min/max in base
247500140923       // all'ora limite del mattino
247600140923         if (fnlry00i1.prconsrici='P' and §ivptipo='S' and §ivporlm>=OOR2STFS)or
247700140923            (fnlry00i1.prconsrici='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
247800140923            (fnlry00i1.prconsrici='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS)or
247900140923            (fnlry00i1.prconsrici='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
248000140923            If fnlry00i1.prconsricI='P';
248100140923                widmsg='TIS0676';
248200140923            else;
248300140923                widmsg='TIS0667';
248400140923            endif;
248500140923            matpom=rtvMsgLang(widmsg : widlingua);
248600140923                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
248700141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
248800140923                      wIdMsg   = 'TIS0760';
248900140923                      wIdCampo = 'PRCONSRICI';
249000140923                      wParm = MatPom + wbolla;
249100141006         else;
249200141006                      wIdMsg   = 'TISA760';
249300141006                      wIdCampo = 'PRCONSRICI';
249400141006                      wParm = MatPom ;
249500141006         endif;
249600140923                      exsr Messaggi;
249700140923         ENDIF;
249800140923       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
249900140923         if fnlry00i1.prconsrici<>*blanks and
250000140923            fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
250100140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
250200140923                    wIdMsg   = 'TIS0753';
250300140923                    wIdCampo = 'NEWORCRICI';
250400140923                    clear wParm;
250500140923                    exsr Messaggi;
250600140923                    wIdCampo = 'PRCONSRICI';
250700140923                    exsr Messaggi;
250800140923         endif;
250900141001         else;
251000141001       // se non immessa la data consegna richiesta errore se immessa l'ora o la preferenza M/P
251100141001           if fnlry00i1.prconsrici<>*blanks ;
251200141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
251300141016              if  wolna>0;
251400141016                        wIdMsg   = 'TIS0795';
251500141016              else;
251600141001                        wIdMsg   = 'TIS0782';
251700141016              endif;
251800141001                        wIdCampo = 'PRCONSRICI';
251900141001                        clear wParm;
252000141001                        exsr Messaggi;
252100141001           endif;
252200141001           if  fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros ;
252300141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
252400141016              if  wolna>0;
252500141016                        wIdMsg   = 'TIS0795';
252600141016              else;
252700141001                        wIdMsg   = 'TIS0782';
252800141016              endif;
252900141001                        wIdCampo = 'NEWORCRICI';
253000141001                        clear wParm;
253100141001                        exsr Messaggi;
253200141001           endif;
253300141001         endif;
253400141016       //else;
253500141016       //?SE DIROTTAMENTO pulisco data/ora preferenza
253600141016       //   clear fnlry00i1.newdtcrici ;
253700141016       //   clear fnlry00i1.neworcrici ;
253800141016       //   clear fnlry00i1.prconsrici ;
253900141016       //endif;
254000140609       endsr;
254100140611       //--------------------------------------------------------------
254200140611       //?Richiamo stored procedure per controllo e calcolo instradamento
254300140611       //--------------------------------------------------------------
254400140611       BEGSR sr_Call_FISPE02R;
254500141006
254600141006
254700140611         clear  oTFA;
254800140611         clear  oLNA;
254900140611         clear  oZNC;
255000140611         clear  oLNPD;
255100140611         clear  oLNAD;
255200140611         clear  oERR;
255300140611         clear  oMSG;
255400140611         clear  oTAG;
255500140611         clear  RTNesito;
255600140611         clear  RTNopcode;
255700140611         clear  RTNstatus;
255800140611         iTYPE='C';
255900140611         iLANG=Widlingua;
256000140611         iDATA=dataoggi;
256100140611         clear iNTW ;
256200140611         iLNP=arblnp;
256300140611         iNZD=fnlry00i1.newnzd;
256400140611         iPRD=fnlry00i1.newprd;
256500140611         iCAD=fnlry00i1.newcad;
256600170404         iLOD=fnlry00i1.newlod;
256700140611         iIND=fnlry00i1.newind;
256800140611         iRSD=fnlry00i1.newrsd;
256900140611         iNCL=arbncl;
257000140611         iPKB=arbpkb;
257100140611         iVLB=arbvlb;
257200140611         iFFD=arbffd;
257300140611         iTSP=arbtsp;
257400140611         iTC1=arbtc1;
257500140611         iTC2=arbtc2;
257600140611         iCBO=arbcbo;
257700140611         fispE02r ( iTYPE :
257800140611                    iLANG :
257900140611                    iDATA :
258000140611                    iNTW :
258100140611                    iLNP :
258200140611                    iNZD :
258300140611                    iPRD :
258400140611                    iCAD :
258500140611                    iLOD :
258600140611                    iIND :
258700140611                    iRSD :
258800140611                    iNCL :
258900140611                    iPKB :
259000140611                    iVLB :
259100140611                    iFFD :
259200140611                    iTSP :
259300140611                    iTC1 :
259400140611                    iTC2 :
259500140611                    iCBO :
259600140611                    oTFA :
259700140611                    oLNA :
259800140611                    oZNC :
259900140611                    oLNPD :
260000140611                    oLNAD :
260100140611                    oERR :
260200140611                    oMSG :
260300140611                    oTAG :
260400140611                    RTNesito :
260500140611                    RTNopcode :
260600140611                    RTNstatus );
260700140925       //if RTNesito<0;
260800140925       //ELSe;
260900140611          if oERR<>*blanks;
261000140611             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
261100140611             clear wIdMsg;
261200140611             wTextMsg=oMSG;
261300140611             select;
261400140611             when  %scan('Provincia':otag)>0;
261500140611                wIdCampo = 'NEWPRD    ';
261600140611             when  %scan('Localita':otag)>0;
261700140611                wIdCampo = 'NEWLOD    ';
261800140611             other;
261900140611                wIdCampo = 'NEWCAD    ';
262000140611             endsl;
262100140611             clear wParm;
262200140611             exsr Messaggi;
262300140611       // Se non ci sono errori verifico che la linea di arrivo calcolata sia la
262400141006       // stessa della bolla --> se dirottamento non possono selezionare la data/ora
262500140611          else;
262600140611             if arblna<>olna;
262700141006       //       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
262800141006       //       wIdMsg   = 'TIS0767';
262900141006       //       wIdCampo = '*CHGLNA   ';
263000141006       //       clear wParm;
263100141006       //       exsr Messaggi;
263200141006                Wolna=olna;
263300140611             endif;
263400140611          endif;
263500140925       //endif;
263600140611       endsr;
263700140610       //--------------------------------------------------------------
263800140610       //?Controllo Dati comuni a tutti i tipi di disposizioni
263900140610       //--------------------------------------------------------------
264000140610       BEGSR  sr_ds3A       ;
264100140610                   clear kkey;
264200140610                   clear ds3a;
264300140610                   %subst(kkey:1:2)=arbcbo;
264400140610                   chain (1:'3A':kkey) tabel00f ;
264500140610                   if %found(tabel00f);
264600140610                      ds3a=tbluni;
264700140610                   endif;
264800140610       endsr;
264900140523       //--------------------------------------------------------------
265000140523       //?Controllo Dati comuni a tutti i tipi di disposizioni
265100140523       //--------------------------------------------------------------
265200140523       BEGSR  sr_ctrUNI     ;
265300140609       //?Referente Consegna - obbligatorio
265400140530       if fnlry00i1.newrefcons=  *blanks;
265500140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
265600140530             wIdMsg   = 'TIS0754';
265700140530             wIdCampo = 'NEWREFCONS';
265800140530             clear wParm;
265900140530             exsr Messaggi;
266000140530       endif;
266100140530       //?Telefono Destinatario - obbligatorio
266200140530       if fnlry00i1.newtelcons=  *blanks;
266300140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
266400140530             wIdMsg   = 'TIS0755';
266500140530             wIdCampo = 'NEWTELCONS';
266600140530             clear wParm;
266700140530             exsr Messaggi;
266800140530       endif;
266900140523       //?Indirizzo e-mail
267000140528       if fnlry00i1.newdesmail<> *blanks;
267100140528          clear dsemail                        ;
267200140528          §emlindi = fnlry00i1.newdesmail;
267300140528          xemail (dsemail);
267400140528          if §emlerro <> '0';
267500140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
267600140528             wIdMsg   = 'TIS0732';
267700140528             wIdCampo = 'NEWDESMAIL';
267800140528             clear wParm;
267900140528             exsr Messaggi;
268000140528          endif;
268100140528       endif;
268200140523       //?N.Telefono per SMS
268300140528       if fnlry00i1.newdescell<> *blanks;
268400140528          pInCell = %trim(fnlry00i1.newdescell);
268500140528          pOutErr = *blanks;
268600140528          if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
268700140528           // Numero di cellulare KO
268800140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
268900140528             wIdMsg   = 'TIS0725';
269000140528             wIdCampo = 'NEWDESCELL';
269100140528             clear wParm;
269200140528             exsr Messaggi;
269300140528          endif;
269400140528       endif;
269500140523       endsr;
269600140529       //--------------------------------------------------------------
269700140529       //?Routine per recuperare flag di autorizzazione al Fermo Deposito
269800140529       //--------------------------------------------------------------
269900140529       BEGSR  sr_tab7r;
270000140529            clear kkey;
270100140529            clear ds7r;
270200140529            %subst(kkey:1:2)=arbgma;
270300140529            chain (1:'7R':kkey) tabel00f ;
270400140529            if %found(tabel00f);
270500140529               ds7r=tbluni;
270600140529            endif;
270700140529       endsr;
270800140528      /end-free
270900140521      **************************************************************************
271000140528     c     sr_OrariSr    BEGSR
271100140521
271200140521     c                   clear                   trulorSds
271300161129     c                   clear                   dataPRVC
271400140522      /free
271500140522              // reperisco gli orari di consegna previsti
271600140522              clear tnsd99ds;
271700161130              d98tbo=w_tbo;
271800140522              d98aas=arbaas;
271900140522              d98lnp=arblnp;
272000140522              d98nrs=arbnrs;
272100140522              d98nsp=arbnsp;
272200161130       // Per la distribuzione forzo tipo servizio "C":
272300161130       // - SEMPRE se la bolla è già in arrivo perchè in questo caso il collo
272400161130       //   o è in IMP oppure è partito ma comunque entrato
272500161130       // - Solo se entrato almeno un collo se la bolla è ancora "Da partire"
272600161130              if arbtsp='D' and (w_tbo='A' or arbdse>0 );
272700140522                 I98TSP_FOR='C';
272800140522              endif;
272900140522              tnsd99r(tnsd99ds);
273000140522              if d98err='0';
273100140522                IOREDTA = d98dci;
273200161130               if d98dee>0;
273300170206                if arbdcr=0 or arbtcr<>*blank  ;
273400161129                DataPrvC= d98dEE;
273500170206                else;
273600170206                DataPrvC= d98dEi;
273700170206                endif;
273800161130               else;
273900170208                 if d98dtctd>*zeros  ;
274000170208                   DataPrvC= %dec(d98dtctd:8:0);
274100170208                   else  ;
274200170208                   DataPrvC= dataoggi          ;
274300170208                 endif;
274400170208               endif;
274500140522              endif;
274600140522      /end-free
274700140521     c                   eval       IOREtfp = ARBtfp
274800140521     c                   eval       IOREtfa = ARBtfa
274900150107     c                   if         arbdti>0
275000140521     c                   eval       IOREdti = ARBdti
275100150107     c                   else
275200150107     c* Caso di consegna parziale:
275300150107     c* Visto che vogliamo prendere gli orari "tutto il giorno", se dti vuoto non lo
275400150107     c* prenderebbe mai e qunidi metto ARBDAM
275500150107     c                   eval       IOREdti = ARBdam
275600150107     c                   endif
275700140528     c* imposto a 0 hti per recuperare sempre gli orari ' '=Tutto il giorno
275800140522     c****               eval       IOREhti = ARBhti
275900140521     c                   eval       IOREdcr = ARBdcr
276000140521     c                   eval       IOREhcr = ARBhcr
276100140521     c                   eval       IOREtcr = ARBtcr
276200140522     c                   eval       IOREDEI = d98dei
276300140522     c                   eval       IOREOEI = d98oei
276400150729
276500150729     c                   clear                   diore01
276600150729     c                   eval       §IORETRAZC=%editc(d98ttc:'X')
276700150729     c                   eval       §IORECONSC=%editc(d98tcc:'X')
276800150729     c                   eval       ioreflo=diore01
276900150729
277000140521     c                   eval       IOREfil = ARBlna
277100140923     c                   if         waltroind=' '
277200140521     c                   eval       IOREcap = ARBcad
277300140521     c                   eval       IOREloc = ARBlod
277400140521     c                   eval       IOREnar = ARBnzd
277500140923     c                   else
277600140925     c                   eval       IOREcap = fnlry00i1.newcad
277700140925     c                   eval       IOREloc = fnlry00i1.newlod
277800140925     c                   eval       IOREnar = fnlry00i1.newnzd
277900140923     c                   endif
278000140521     c
278100140521     c                   eval       IOREtser = 'C'
278200140521     c                   eval       IOREDDC=arbddc
278300140521     c                   eval       IOREnDC=arbndc
278400140521     c
278500140521     c
278600140521     c                   eval       IOREtsp = ARBtsp
278700140618     c                   if          ARBfbc = *blanks and
278800140618     c                               ARBddc > 0 and
278900140618     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
279000140618     c                               arbica='R' or arbicc=' ' or arbicc='R')
279100140618     c                   eval       IOREcons = 'S'
279200140618     c                   endif
279300140521     c
279400140521
279500140521     c                   call      'TRULORSR'
279600140521     c                   parm                    kpjba
279700140521     c                   parm                    trulorSds
279800140521     c                   parm                    trulor2ds
279900140521     c
280000140521     c                   ENDSR
280100140521      /free
280200140924       //--------------------------------------------------------------
280300140924       //?Routine per determinare il giorno della settimana di dataoggi + 1gg lav.
280400140924       //--------------------------------------------------------------
280500140924       BEGSR  sr_wdesgio ;
280600140924       //       Determino il giorno della settimana di dataoggi+1giorno lavorativo
280700140924       //       da restituire nel messaggio
280800140924               wdata=dataoggi;
280900140924       //       Incremento la data di un giorno
281000140924               exsr sr_incredata;
281100140924       //       Verifico se data festiva e in tal caso incremento finchè non trovo giorno
281200140924       //       lavorativo
281300140924               exsr sr_datafes;
281400140924       //       Determino il giorno della settimana della data calcolata
281500140924               dataiso=%date(wdata:*iso);
281600140924               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
281700140924               wdesgio=rtvMsgLang(dayOfWeek.msgId(dw) : widlingua);
281800140924       endsr;
281900140603       //--------------------------------------------------------------
282000140603       //?Routine per scrittura disposizioni di consegna
282100140603       //--------------------------------------------------------------
282200140924       BEGSR  SR_Tiidc;
282300140603       // Per la spedizione non devono esserci disposizioni ancora da elaborare
282400140603       exsr sr_ChkDispo;
282500140603       clear tiidc000;
282600141013       clear tiidcds ;
282700140718       clear tivpi000;
282800140604       exsr repnum;
282900140604       if esito = *blanks;
283000140604          IDCPRG=kprg                         ;
283100140604       else;
283200140619       // prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
283300140619  //   // wIdMsg   = 'TIS0757';
283400140619  //   // wIdCampo = '*REPNUM   ';
283500140619  //   // clear wParm;
283600140619  //   // exsr Messaggi;
283700140616          prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
283800140616          prmRpyIdMsg  = FNLRY00_ESITO_NUMERATORE_DISPO_NONREPERITO;
283900140604          exsr RoutEnd;
284000140604       endif;
284100141009       //WDCINSDATA = %dec(fnlry00i1.ksutimest);
284200141009       //IDCINSDATA = %dec(%subst(%editc(wdcinsdata:'X'):1:14):14:0);
284300141009       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
284400141009                                    : 1 : 14 ) );
284500141009       IDCtimestn   = fnlry00i1.ksutimestn;
284600140603       //IDCMODACC                           ;
284700140610       if wksu<>*blanks;
284800140610       IDCMODACC='L'                       ;
284900140610       else;
285000140610       endif;
285100140603       IDCIP = fnlry00I1.cltipaddr;
285200140603       IDCIDCLI = fnlry00i1.ksu;
285300140603       IDCTIPOCLI= Wchi;
285400140603       IDCWAAS   = kaas;
285500140603       IDCWLNP   = klnp;
285600140603       IDCWNRS   = knrs;
285700140603       IDCWNSP   = knsp;
285800140620       IDCAAS    = arbaas;
285900140620       IDCLNP    = arblnp;
286000140620       IDCNRS    = arbnrs;
286100140620       IDCNSP    = arbnsp;
286200140603       IDCFGS    = wfgslna;
286300140603       IDCTIPODIS = fnlry00i1.tipodispo;
286400140604       IDCMAILAVV = fnlry00i1.newdesmail;
286500140604       IDCTELAVV  = fnlry00i1.newdescell;
286600161202       //IDCALERT Metto nell'ultimo byte informazione del momento in cui
286700161202       //         è stata immessa la dispo rispetto allo stato di avanzamento
286800161202       //         della bolla: P=Bolla da Partire,
286900161202       //                      A=Arrivata ma non ancora fatto il primo tentativo di consegna
287000161202       //                  blank=Arrivata e già fatto il primo tentativo di consegna
287100161202       clear  didcal;
287200161202       select;
287300161202       when w_tbo='P';
287400161202          eval §IDCSTA='P'         ;
287500161202       when w_tbo='A' and arbntc=0 and wtentacons=*blanks;
287600161202          eval §IDCSTA='A'         ;
287700161202       endsl;
287800161202       eval idcalert=didcal;
287900161202
288000140604       if fnlry00i1.newdtcrich <> *blanks and fnlry00i1.tipodispo='1';
288100140604           IDCDCR =  %dec(%date(fnlry00i1.newdtcrich:*eur):*iso);
288200140604       endif;
288300140604       if fnlry00i1.neworcrich <> *blanks and fnlry00i1.tipodispo='1';
288400140604           IDCHCR =  %dec(fnlry00i1.neworcrich:4:0);
288500140604       endif;
288600140924       if fnlry00i1.newdtcrici <> *blanks and fnlry00i1.tipodispo='3';
288700140924           IDCDCR =  %dec(%date(fnlry00i1.newdtcrici:*eur):*iso);
288800140924       endif;
288900140924       if fnlry00i1.neworcrici <> *blanks and fnlry00i1.tipodispo='3';
289000140924           IDCHCR =  %dec(fnlry00i1.neworcrici:4:0);
289100140924       endif;
289200140603       //IDCTCR                         ;
289300140924       if fnlry00i1.tipodispo='1';
289400140924          IDCPRCR=  fnlry00i1.prconsric  ;
289500140924       endif;
289600140924       if fnlry00i1.tipodispo='3';
289700140924          IDCPRCR=  fnlry00i1.prconsrici ;
289800140924       endif;
289900140603       IDCREF  = fnlry00i1.newrefcons ;
290000140603       IDCTELD = fnlry00i1.newtelcons ;
290100140603       if fnlry00i1.tipodispo='2';
290200140603          IDCFFD  = 'S';
290300140603       endif;
290400140611       if fnlry00i1.tipodispo='3';
290500141007          wtxt     = fnlry00i1.newrsd;
290600141007          uppercase='1';
290700170404          clear ElencoChar;
290800170404          clear TipoElenco;
290900141007          exsr sr_chk;
291000141007          IDCRSD   = wtxt;
291100141007          wtxt     = fnlry00i1.newind;
291200141007          uppercase='1';
291300170404          clear ElencoChar;
291400170404          clear TipoElenco;
291500141007          exsr sr_chk;
291600141007          IDCIND   = wtxt            ;
291700140611          IDCCAD   = fnlry00i1.newcad;
291800170404
291900170404       // wtxt     = fnlry00i1.newlod;
292000170404       // exsr sr_chklod;
292100170404       // IDCLOD   = wtxt            ;
292200170404          IDCLOD   = fnlry00i1.newlod;
292300170404
292400140611          IDCPRD   = fnlry00i1.newprd;
292500140611          IDCNAZ   = fnlry00i1.newnzd;
292600141006       // bolla da dirottare
292700141006          if wolna>0;
292800141006               IDCNEWLNA=wolna;
292900141006               IDCFLA   ='S';
293000141006          endif;
293100140611       endif;
293200140611       if fnlry00i1.tipodispo='4';
293300140611          IDCNOTE  = fnlry00i1.altro;
293400140611       endif;
293500140603       IDCELAFLG='S';
293600140603
293700140718       VPITIP='TI';
293800140718       VPIISV='TT';
293900140718       // indicazioni ricevute da cliente loggato
294000140718       if wksu<>*blanks;
294100140801          VPISUN=%editc(fnlry00i1.sun:'X');
294200140718          VPIKSU=wksu    ;
294300140718       else;
294400140718       // indicazioni ricevute mediante BRTCODE
294500140718          VPISUN='8IDC00000';
294600140718          VPIKSU='0IDC0000';
294700140718       endif;
294800140718       VPIDTA=tiidcds ;
294900140718       write tivpi000;
295000140814       // se AS888 scrivo anche tiidc
295100140814
295200140814      /end-free
295300140814     c* controllo se sono in AS888
295400141006     c                   eval      comman=chklib(1)
295500141006     c                   eval      lengh=35
295600141006     c                   call      'QCMDEXC'                            99
295700141006     c                   parm                    comman           80
295800141006     c                   parm                    lengh            15 5
295900140814     c
296000141006     c                   if        not *in99
296100140814      /free
296200140814
296300141013         write tiidc000;
296400141006         endif  ;
296500140603
296600140603       endsr;
296700140603       //--------------------------------------------------------------
296800140603       //?Verifica presenza di diposizioni ancora da elaborare
296900140603       //--------------------------------------------------------------
297000140603       BEGSR  SR_ChkDispo;
297100140908       clear w0140;
297200140908       setll (arbaas:arblnp:arbnrs:arbnsp:w0140) tiidc01l;
297300140603       if %equal(tiidc01l);
297400140603              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
297500140603              wIdMsg   = 'TIS0756';
297600140603              wIdCampo = '*TIIDC    ';
297700140611              wParm = wbolla;
297800140603              exsr Messaggi;
297900140603              exsr Routend;
298000140603       endif;
298100140718       // verifico anche che non siano ancora su tivpi
298200140801       eval wkeyspa=%editc(arbaas:'X')+%editc(arblnp:'X')+
298300140801                    %editc(arbnrs:'X')+%editc(arbnsp:'X')  ;
298400140801      /end-free
298500140718     C/EXEC SQL
298600140718     C+ SELECT count(*) INTO :totale FROM tivpi00f where
298700140718     c+ VPITIP='TI' and VPIISV='TT' and
298800141002     C+ substr(vpidta, 67, 16) = :wkeyspa
298900140718     C/END-EXEC
299000140801      /free
299100140718          if totale>0 ;
299200140718              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
299300140718              wIdMsg   = 'TIS0756';
299400140718              wIdCampo = '*TIVPI    ';
299500140718              wParm = wbolla;
299600140718              exsr Messaggi;
299700140718              exsr Routend;
299800140718       endif;
299900140603       endsr;
300000140604        //-----------------------------------------------------------------------*
300100140604        // Reperimento progressivo richiesta da numeratore 650
300200140604        //-----------------------------------------------------------------------*
300300140604        begsr RepNum;
300400140604
300500140604          clear esito;
300600140604          $finenum=*off;
300700140604          clear trul33ds;
300800140604
300900140604          I33TLa = 'L';
301000140604          I33Ope = *ZEROS;
301100140604          I33CNu = 650;
301200140604          I33Num = 1;
301300140604
301400140604          KPJBU = TRUL33DS;
301500140604
301600140604          dow $finenum=*off   ;
301700140604             Trul33R(kpjba);
301800140604
301900140604             TRUL33DS = KPJBU;
302000140604
302100140604             if O33Err <> *zeros;
302200140604               Esito = '2';
302300140604               $finenum=*on;
302400140604             else;
302500140908               w0090=o33nri;
302600140908               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
302700140604               setll kprg tiidc02l;
302800140604        // Se numero già presente nel file TIIDC ne cerco un altro
302900140604               if not %equal;
303000140604                  $finenum=*on;
303100140604               endif;
303200140604             endif;
303300140604          enddo;
303400140604
303500140604          endsr;
303600140604
303700140708       //--------------------------------------------------------------
303800140708       //?Routine per schierare i messaggi nella ds FNLRY00M0
303900140708       //--------------------------------------------------------------
304000140708       BEGSR  sr_RepPct;
304100140708
304200140708       clear wtentacons;
304300140708
304400140708      /end-free
304500161202     c                   if        arbndc>0
304600140708     c                   move      arbndc        pctndc
304700140708     c                   move      arbifp        pctfgs
304800140708     c                   move      'CET'         ttrd
304900140708     c     kcet          setgt     fipct02l
305000140708     c     kcet          readpe    fipct02l
305100140708     c                   if        not %eof(fipct02l)
305200140708     c                   movel     pctdati       fipctcetds
305300140708     c* consegnata
305400140708     c                   if        §pctcmc = ' '
305500140708     c                   eval      prmRpyOpCode=FNLRY00_RPYOPCODE_ERROR
305600141006     c                   if        prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO
305700141006     c                   eval      wIdMsg   = 'TISA738'
305800141006     c                   eval      wIdCampo = '*BOLLA    '
305900141006     c                   clear                   wbolla
306000141006     c                   else
306100140708     c                   eval      wIdMsg   = 'TIS0738'
306200140708     c                   eval      wIdCampo = '*BOLLA    '
306300140708     c                   eval      wParm = wbolla
306400141006     c                   endif
306500140708     c                   exsr      Messaggi
306600140708     c                   exsr      routend
306700140708     c                   else
306800140708       //?deve essere già stato fatto almeno un tentativo di consegna
306900140708     c                   eval      wtentacons='1'
307000140708     c                   endif
307100140708     c                   endif
307200161202     c                   endif
307300140708      /free
307400140708       endsr;
307500140929       //--------------------------------------------------------------
307600140929       //?Routine per tradurre mattino/pomeriggio
307700140929       //--------------------------------------------------------------
307800140929       BEGSR  sr_matpom;
307900140929
308000140929            If wconsric='P';
308100140929                widmsg='TIS0676';
308200140929            else;
308300140929                widmsg='TIS0667';
308400140929            endif;
308500140929            matpom=rtvMsgLang(widmsg : widlingua);
308600140929       endsr;
308700140604
308800140527       //--------------------------------------------------------------
308900140527       //?Routine per schierare i messaggi nella ds FNLRY00M0
309000140527       //--------------------------------------------------------------
309100140527       BEGSR  Messaggi;
309200140527
309300140527       //?Se ho già caricato 99 messaggi esco
309400140527         IF  fnlry00M0.nrmsg = 99;
309500140527           exsr RoutEnd;
309600140527           clear fnlry00M0.nrmsg;
309700140527         ENDIF;
309800140527
309900140527         fnlry00M0.nrmsg += 1;
310000140527         fnlry00M0.skIdMsg(fnlry00M0.nrmsg) = wIdMsg;
310100140527         fnlry00M0.skIdCampo(fnlry00M0.nrmsg) = wIdCampo;
310200140527         fnlry00M0.skErrWarn(fnlry00M0.nrmsg) = FNLRY00_MSG_ERRORE;
310300140527         IF  wParm <> *blanks;
310400140527           fnlry00M0.skTextMsg (fnlry00M0.nrmsg) =
310500140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
310600140527         ELSE;
310700140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
310800140611           if wIdMsg=*blanks;
310900140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=wTextMsg;
311000140611           else;
311100140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=
311200140611             rtvMsgLang(wIdMsg:wIdlingua);
311300140611           endif;
311400140527         ENDIF;
311500140527
311600140527       ENDSR;
311700140527
311800140521       //--------------------------------------------------------------
311900140408       //?Carico tabelle.
312000140408       //--------------------------------------------------------------
312100140408       BEGSR  CaricaTab;
312200140408
312300140408
312400140408       ENDSR;
312500131010
312600131010       //--------------------------------------------------------------
312700131010       //?Operazioni finali.
312800131010       //--------------------------------------------------------------
312900131010       BEGSR  RoutEnd;
313000140408
313100140519       //?Se richiamato per GET_ISTRUCO
313200140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
313300140519           %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o0;
313400140408         ELSE;
313500131010
313600140522       //?Se richiamato per PUT_ISTRUCO
313700140522          %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o1;
313800140521       //%subst(prmRpyForzatu:1:prmRpyForSize) = fnlry0F0;
313900140408
314000140408         ENDIF;
314100140530
314200140530          %subst(prmRpyMessage:1:prmRpyMsgSize) = fnlrY00M0;
314300131010
314400131010         return;
314500131010
314600131010       ENDSR;
314700131009
314800131007      /end-free
314900141007      *---------------------------------------------------------
315000141007      * Controllo i caratteri dei campi alfanumerici
315100141007      *---------------------------------------------------------
315200141007     c     Sr_Chk        BegSr
315300141007
315400141007     c                   clear                   esito
315500141007     c                   Eval      TxtInOut = wtxt
315600141007     c                   Call      'XCHKCHAR'
315700141007     c                   Parm                    TxtInOut
315800141007     c                   Parm                    ElencoChar
315900141007     c                   Parm                    TipoElenco
316000141007     c                   Parm                    CharSost
316100141007     c                   Parm                    UpperCase
316200141007     c                   Parm                    ChkNull
316300141007     c                   Parm                    CharNull
316400141007     c                   Parm                    Esito
316500141007     c                   If        Esito = '1'
316600141007     c                   Move      TxtInOut      wtxt
316700141007     c                   EndIf
316800141007
316900141007     c                   EndSr
317000141007      *---------------------------------------------------------
317100141007      * Controllo caratteri di punteggiatura precedenti e seguenti
317200141007      *---------------------------------------------------------
317300141007     c     chkstringa    BegSr
317400141007
317500141007     c* trovo la prima posizione "buona" da utilizzare
317600170404     c                   eval      posl=%check(charnooki:wtxt)
317700141007     c* trovo l'ultima posizione "buona" da utilizzare (escludo il ".")
317800141007     c                   eval      posr=%checkr(charnookf:wtxt)
317900141007     c                   if        posl=0
318000141007     c                   clear                   wtxt
318100141007     c                   else
318200141007     c* determino la lunghezza della stringa "buona"
318300141007     c                   eval      lung=(posr-posl)+1
318400141007     c                   eval      wtxt=%subst(wtxt:posl:lung)
318500141007     c                   endif
318600141007
318700141007     c                   EndSr
318800170404       //--------------------------------------------------------------
318900170404       //?Operazioni di stringa su località
319000170404       //--------------------------------------------------------------
319100170404       BEGSR sr_chklod    ;
319200170404          uppercase='1';
319300170404          clear ElencoChar;
319400170404          clear TipoElenco;
319500170404          exsr sr_chk;
319600170404       // eseguo routine per eliminare caratteri non ammessi nella località come
319700170404       // da tabella "CSP"
319800170404       // per i caratteri non ammessi in tutto il campo richiamo XCHKCHAR
319900170404          clear uppercase   ;
320000170404          ElencoChar=charnook;
320100170404          TipoElenco='1';
320200170404          exsr sr_chk;
320300170404          exsr chkstringa;
320400170404       endsr;
320500161129       //--------------------------------------------------------------
320600161129       //?Pulizia campi di blp non presenti su arb
320700161129       //--------------------------------------------------------------
320800161129       BEGSR sr_inzsoloblp;
320900161129          clear arbflp;
321000161129          clear arbdim;
321100161129          clear arbsop;
321200161129          clear arbfst;
321300161129          clear arbctm;
321400161129          clear arbdrt;
321500161129          clear arbnrt;
321600161129          clear arbfpp;
321700161129          clear arbpdr;
321800161129          clear arbdse;
321900161129       ENDSR;
322000161209       //--------------------------------------------------------------
322100161209       //?Cerca ultimo evento valido come tentaivo di consegna
322200161209       //--------------------------------------------------------------
322300161209       BEGSR sr_evb;
322400161209             clear wdatai;
322500161215             setgt  (arbaas:arblnp:arbnrs:arbnsp) fnevb01l ;
322600161215             readpe (arbaas:arblnp:arbnrs:arbnsp) fnevb01l ;
322700161209             dow not %eof(fnevb01l) and wdatai=0;
322800161209                exsr sr_2a ;
322900161209                if §2antc='S'              ;
323000161209                   wdatai=evbdev;
323100161209                endif;
323200161215             readpe (arbaas:arblnp:arbnrs:arbnsp) fnevb01l ;
323300161209             enddo ;
323400161209       endsr;
323500161206      *-------------------------------------------------------------------------
323600161206      *  Decoditifa causale evento
323700161206      *-------------------------------------------------------------------------
323800161206       BEGSR sr_2a;
323900161206         clear ds2a;
324000161206         clear kkey     ;
324100161206         %subst(kkey:1:3)=evbcev;
324200161206         chain (1:'2A':kkey) tabel00f;
324300161206         if %found(tabel00f);
324400161206            ds2a=tbluni;
324500161206         endif;
324600161206       endsr;
324700161215      *-------------------------------------------------------------------------
324800161216      *  Calcola data minima proponibile e data da cui parte il conteggio per la data
324900161216      *  max proponibile
325000161215      *-------------------------------------------------------------------------
325100161216       BEGSR sr_date   ;
325200161216       // DATA MINIMA PROPONIBILE
325300161215       clear  wdatam;
325400161215       if w_tbo='A' and (arbntc>0 or wtentacons='1') ;
325500161215         wdatam=dataoggi;
325600161215       else;
325700161215       // Se bolla ancora da partire non parto da Oggi ma dalla Data Prevista Consegna
325800161215       // Idem se bolla in arrivo ma non ancora fatti tentativi di consegna
325900161215         wdatam=dataPrvC;
326000161215       // Se Data prevista consegna inferiore a oggi
326100161215       // Sarebbe sbagliato proporre una data già decorsa quindi in questo caso parto da oggi
326200161215               if DataPrvC<dataoggi;
326300161215                wdatam  = dataoggi;
326400161215               endif;
326500161215       endif;
326600161216
326700161216       // DATA INIZIO (da cui parte il conteggio per la data max prima che la sped. venga
326800161216       // proposta in apertura giacenza)
326900161216
327000161216       clear wdata ;
327100161216       if arbndc>0;
327200161216          if wtentacons='1';
327300161216             wdata =%dec(%subst(%editc(pctdtorin:'X'):1:8):8:0);
327400161216          else ;
327500161216             wdata =arbddc;
327600161216          endif;
327700161216       else;
327800161216          exsr sr_evb ;
327900161216          wdata=wdataI;
328000161216          if wdata =0;
328100170208       // Se non presenti eventi validi come tentativo di consegna assumo la data arrivo merce
328200170208       // reale/teorica. La data prevista consegna la assumo solo come paracadute se damrtc dovesse
328300170208       // essere vuota. L'uso della damrtc è necessario per le spedizioni per le quali è prevista
328400170208       // la consegna in 48 ore o per quelle  che arrivano dopo la prima soglia e hanno
328500170208       // tempi di consegna > 12
328600170208       // In questi casi infatti la giacenza viene aperta partendo dalla data di arrivo e non dalla
328700170208       // data prevista consegna
328800170208           if d98dam_rtc>0;
328900170208             wdata =d98dam_rtc;
329000170208               if wdata<dataoggi;
329100170208                wdata  = dataoggi;
329200170208               endif;
329300170208            else;
329400161216             wdata =wdatam;
329500170208            endif;
329600161216          endif;
329700161216       endif;
329800161215       endsr;
329900140814**
330000141006CHKOBJ OBJ(FILTRAPRD) OBJTYPE(*LIB)
