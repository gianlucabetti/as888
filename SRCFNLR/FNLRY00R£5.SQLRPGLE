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
003300140528     FFiar401l  IF   E           K DISK    extfile(wlibfil) usropn
003400140521     FFipct02l  IF   E           K DISK    extfile(wlibfil) usropn
003500161215     FFnevb01l  IF   E           K DISK
003600140521     FTIgcp21l  IF   E           K DISK
003700140605     Fazcln01l  IF   E           K DISK
003800140620     Ffnlbl02l  IF   E           K DISK
003900131008
004000131007       //--------------------------------------------------------------
004100131007       //?Definizione costanti.                                        ?
004200131007       //--------------------------------------------------------------
004300140520      /copy gaitrasrc/srcconst,FNLRY00R
004400140520
004500140520     D digits          c                   '0123456789'
004600140617
004700140617     D MSGID_LUNEDI...
004800140617     D                 C                   'TIS0351'
004900140617     D MSGID_MARTEDI...
005000140617     D                 C                   'TIS0354'
005100140617     D MSGID_MERCOLEDI...
005200140617     D                 C                   'TIS0356'
005300140617     D MSGID_GIOVEDI...
005400140617     D                 C                   'TIS0203'
005500140617     D MSGID_VENERDI...
005600140617     D                 C                   'TIS0607'
005700140617     D MSGID_SABATO...
005800140617     D                 C                   'TIS0533'
005900140617     D MSGID_DOMENICA...
006000140617     D                 C                   'TIS0159'
006100140114
006200131007       //--------------------------------------------------------------
006300131007       //?Definizione strutture dati.                                  ?
006400131007       //--------------------------------------------------------------
006500131007       // -?Dati INPUT
006600140519     d Fnlry00i0     e ds                  QUALIFIED INZ(*EXTDFT)
006700140528     d Fnlry00i1     e ds                  QUALIFIED INZ(*EXTDFT)
006800141006     d TIIDCds       e ds                  extname(tiidc00f)
006900131007
007000131007       // -?Dati OUTPUT
007100140519     d Fnlry00o0     e ds                  QUALIFIED INZ(*EXTDFT)
007200140605     d  schdcr                       10a   dim(9)  overlay(dateconst)
007300140923     d  schdcri                      10a   dim(9)  overlay(dateconti)
007400140521     d Fnlry00o1     e ds                  QUALIFIED INZ(*EXTDFT)
007500131007
007600131007       // -?Messaggi
007700140522     d Fnlry00m0     e ds                  QUALIFIED INZ(*EXTDFT)
007800140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
007900140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
008000140522     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
008100140522     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
008200131007
008300131007       // -?Forzature
008400140522     d Fnlry00f0     e ds                  QUALIFIED INZ(*EXTDFT)
008500140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
008600140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
008700140522     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
008800140520       // -?Legami clienti
008900140520     d TIBS10DS      e ds                  INZ(*EXTDFT)
009000140520     d  skc                          11    dim(500) overlay(d10skc)
009100140521       // -?Controllo BRTCODE
009200140521     d TRUL28DS0     e ds                  INZ(*EXTDFT)
009300140528       // -?Controllo indirizzo mail
009400140528     d dsemail       e ds                  INZ
009500140521       // -?Calcola orari di servizio
009600140521     d TRULORSDS     e ds                  INZ
009700140521     d TRULOR2ds     e ds                  INZ
009800150729     d diore01       e ds                  INZ
009900140604
010000161129       // -?File bolle arrivi
010100161129     d FNARBds       e ds                  extname(FNARB00F)
010200140522     D tnsd99ds      e ds
010300140604     D TRUL33DS      E DS
010400140923     D Tisio9ds      E DS
010500140521
010600140521     d fidn12ds      e ds
010700140603     d tibs02ds      e ds
010800140603     d fnlv55ds      e ds
010900140604     d KPJBA         e ds
011000161128
011100161128     d TRULVPODS     e ds
011200161128     D  skFilOk               16    765    dim(250)
011300140521
011400161202     d didcal        e ds
011500140521     d fipctcetds    e ds
011600140521     d og143         e ds
011700140522     d dar5emd       e ds
011800140523     d dar5gen       e ds
011900140522     d ds7r          e ds
012000140522     d ds3a          e ds
012100140613     d ds2a          e ds
012200140528     d ds2g          e ds
012300140528     d ds7u          e ds
012400140619     d ds7w          e ds
012500140603     d divp          e ds
012600140924     d dvpooraris    e ds
012700140528
012800140528     D WDAT8           DS
012900140528     D  DATADA                 1      8  0
013000140528     D  DATAA                  9     16  0
013100140528     D  GGL                   17     21  0
013200140610     D tfptfa          DS
013300140610     D  p_tfp                  1      3  0
013400140610     D  p_tfa                  4      6  0
013500140606
013600140606     D clnmat          DS
013700140606     D  mat                    1     31    dim(31)
013800140606     D clnpom          DS
013900140606     D  pom                    1     31    dim(31)
014000140617     D dayOfWeek       DS                  QUALIFIED
014100140617     D                                7A   INZ(MSGID_LUNEDI)
014200140617     D                                7A   INZ(MSGID_MARTEDI)
014300140617     D                                7A   INZ(MSGID_MERCOLEDI)
014400140617     D                                7A   INZ(MSGID_GIOVEDI)
014500140617     D                                7A   INZ(MSGID_VENERDI)
014600140617     D                                7A   INZ(MSGID_SABATO)
014700140617     D                                7A   INZ(MSGID_DOMENICA)
014800140617     D  msgId                         7A   DIM(7) OVERLAY(dayOfWeek)
014900140814
015000140814     D chklib          S             35    DIM(1) CTDATA PERRCD(1)
015100140312
015200131009       //--------------------------------------------------------------
015300131009       //?Definizione schiere.
015400131009       //--------------------------------------------------------------
015500140210
015600140210     d skFIdMsg        s              7a   dim(99)
015700140210     d skFIdCampo      s             10a   dim(99)
015800140210     d skFGiaForza     s              1a   dim(99)
015900131007
016000131007       //--------------------------------------------------------------
016100131007       //?Definizione campi.
016200131007       //--------------------------------------------------------------
016300140527     d wIdMsg          s              7a
016400140527     d wIdCampo        s             10a
016500140527     d wcampo          s             10a
016600140519     d wksu            s                   like(fnlry00i0.ksu)
016700140520     d widbolla        s                   like(fnlry00i0.idbolla)
016800140521     d wbolla          s                   like(widbolla)
016900140520     d wbrtcode        s                   like(fnlry00i0.brtcode)
017000140527     d wIdlingua       s                   like(fnlry00i0.liniso2)
017100140527     d wdcreur         s                   like(fnlry00i1.olddtcrich)
017200140529     d wrbhcr          s                   like(fnlry00i1.OLDORCRICH)
017300140528     d wtprich         s              1
017400140530     d wtentacons      s              1
017500140528     d dataoggi        s              8s 0
017600161129     d dataPrvC        s              8s 0
017700141218     d wdataconfr      s              8s 0
017800140604     d annocur         s              4s 0
017900140528     d wdcr            s              8s 0
018000140605     d wdata           s              8s 0
018100161206     d wdatam          s              8s 0
018200161206     d wdatai          s              8s 0
018300140528     d wora            s              4s 0
018400140527     d wrsd2           s             35
018500140522     d wchi            s              1
018600140521     d kaas            s                   like(arbaas)
018700140521     d klnp            s                   like(arblnp)
018800140521     d knrs            s                   like(arbnrs)
018900140521     d knsp            s                   like(arbnsp)
019000140522     d kkey            s                   like(tblkey)
019100140523     d wrefbolla       s                   like(§ar5ref)
019200140523     d wtelbolla       s                   like(§ar5teld)
019300140528     d wlimitdcr       s                   like(§2gldr)
019400140613     d wlimitFD        s                   like(§2agla)
019500140521     d wcliente        s             11
019600140521     D Wlibfil         S             21
019700140521     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
019800140521     D Wlibfil201      S             21    INZ('FILTRA201/        ')
019900140521     d dataeur         s               d   datfmt(*eur)
020000140528     d dataiso         s               d   datfmt(*iso)
020100140528     d wParm           s            512a   inz
020200140528     d w008a           s              8a   inz
020300140908     d w0140           s             14  0 inz
020400140603     d wfgslna         s                   like(d55tfa)
020500140616     d wfgslnad        s             20a
020600140604     D Esito           s              1a
020700141031     d w0090           s              9  0
020800140604     d kprg            s                   like(idcprg)
020900140604     d WDCINSDATA      s             20  0
021000140605     d I               s              2  0
021100140606     d Ix              s              2  0
021200140923     d Iy              s              2  0
021300140606     d wfes            s              1
021400140606     d matpom          s             10
021500140611     d wTextMsg        s            255
021600140604     d $Finenum        s               n   inz(*off)
021700140617     d dw              s              1s 0
021800140617     d wdesgio         s             20a
021900140718     d totale          s              2  0
022000140801     d wkeyspa         s             16
022100140924     D wtempoh         S              2  0 inz
022200140924     D wtempom         S              2  0 inz
022300140925     D waltroind       S              1a   inz
022400140930     D wconsric        S                   like(fnlry00i1.prconsric)
022500140930     D w_dalle         S               t
022600140930     D w_alle          S               t
022700140930     D w_dalleoser     S               t
022800140930     D w_alleoser      S               t
022900140930     D w_dallemsg      S              5a
023000140930     D w_allemsg       S              5a
023100140930     D w_orcric        S                   like(fnlry00i1.neworcrich)
023200141006     D wolna           S                   like(olna)
023300141007     d wtxt            S           2048
023400141007     d TxtInOut        S           2048
023500141007     d ElencoChar      S            256
023600141007     d TipoElenco      S              1
023700141007     d CharSost        S              1
023800141007     d UpperCase       S              1
023900141007     d ChkNull         S              1    Inz('1')
024000141007     d CharNull        S              1
024100141007     d posl            s              2  0
024200141007     d posr            s              2  0
024300141007     d lung            s              2  0
024400141007     d* Lunghezza di CHARNOOK deve essere:  lunghezza di tbeuni + 1
024500141007     d* Il +1 è necessario per assicurarmi che nella stringa sia sempre
024600141007     d* presente un blank
024700141007     d charnook        s            257
024800141007     d charnookf       s                   like(charnook)
024900161130     d w_tbo           s              1    inz
025000161129     d wDecofiAll      s               n   inz
025100140611       //?- Parametri per FISPE02R: EasySpedWeb: S.P.  TISIT5R+TISI95R?
025200140611     d iTYPE           s              1    inz
025300140611     d iLANG           s              3    inz
025400140611     d iDATA           s              8s 0 inz
025500140611     d iNTW            s              1    inz
025600140611     d iLNP            s              3s 0 inz
025700140611     d iNZD            s              3    inz
025800140611     d iPRD            s              2    inz
025900140611     d iCAD            s              9    inz
026000140611     d iLOD            s             35    inz
026100140611     d iIND            s             35    inz
026200140611     d iRSD            s             35    inz
026300140611     d iNCL            s              5s 0 inz
026400140611     d iPKB            s              7s 1 inz
026500140611     d iVLB            s              5s 3 inz
026600140611     d iFFD            s              1    inz
026700140611     d iTSP            s              1    inz
026800140611     d iTC1            s              1    inz
026900140611     d iTC2            s              1    inz
027000140611     d iCBO            s              2    inz
027100140611      *
027200140611     d oTFA            s              3s 0 inz
027300140611     d oLNA            s              3s 0 inz
027400140611     d oZNC            s              2s 0 inz
027500140611     d oLNPD           s             20    inz
027600140611     d oLNAD           s             20    inz
027700140611     d oERR            s              1    inz
027800140611     d oMSG            s            256    inz
027900140611     d oTAG            s             30    inz
028000140611      *
028100140611     d RTNesito        s             10I 0 inz
028200140611     d RTNopcode       s             10    inz
028300140611     d RTNstatus       s             10I 0 inz
028400131008
028500131008       //--------------------------------------------------------------
028600131008       //?Definizione procedure.
028700131008       //--------------------------------------------------------------
028800140603     d tibs02r         pr                  extpgm('TIBS02R')
028900140604     d  kpjba                              likeds(kpjba)
029000140603     d  tibs02ds                           likeds(tibs02ds)
029100140604     D trul33R         pr                  extpgm('TRUL33R')
029200140604     D  kpjba                              likeds(kpjba)
029300140923     D tisio9r         pr                  extpgm('TISIO9R')
029400140923     D  kpjba                              likeds(kpjba)
029500140923     D  tisio9ds                           likeds(tisio9ds)
029600140611       //?- Stored Procedure per EasySpedWeb?
029700140611     d fispE02r        pr                  extpgm('FISPE02R')
029800140611     d   i_TYPE                            like(iTYPE)
029900140611     d   i_LANG                            like(iLANG)
030000140611     d   i_DATA                            like(iDATA)
030100140611     d   i_NTW                             like(iNTW)
030200140611     d   i_LNP                             like(iLNP)
030300140611     d   i_NZD                             like(iNZD)
030400140611     d   i_PRD                             like(iPRD)
030500140611     d   i_CAD                             like(iCAD)
030600140611     d   i_LOD                             like(iLOD)
030700140611     d   i_IND                             like(iIND)
030800140611     d   i_RSD                             like(iRSD)
030900140611     d   i_NCL                             like(iNCL)
031000140611     d   i_PKB                             like(iPKB)
031100140611     d   i_VLB                             like(iVLB)
031200140611     d   i_FFD                             like(iFFD)
031300140611     d   i_TSP                             like(iTSP)
031400140611     d   i_TC1                             like(iTC1)
031500140611     d   i_TC2                             like(iTC2)
031600140611     d   i_CBO                             like(iCBO)
031700140611     d   o_TFA                             like(oTFA)
031800140611     d   o_LNA                             like(oLNA)
031900140611     d   o_ZNC                             like(oZNC)
032000140611     d   o_LNPD                            like(oLNPD)
032100140611     d   o_LNAD                            like(oLNAD)
032200140611     d   o_ERR                             like(oERR)
032300140611     d   o_MSG                             like(oMSG)
032400140611     d   o_TAG                             like(oTAG)
032500140611     d   RTN_esito                         like(RTNesito)
032600140611     d   RTN_opcode                        like(RTNopcode)
032700140611     d   RTN_status                        like(RTNstatus)
032800131008
032900131008       //--------------------------------------------------------------
033000131008       //?Definizione prototipi.
033100131008       //--------------------------------------------------------------
033200140519      /copy gaitrasrc/srcprotopr,Fnlry00r
033300140520      /copy gaitrasrc/srcprotopr,TIBS10R
033400140521      /copy gaitrasrc/srcprotopr,TRUL28R0
033500140521      /copy gaitrasrc/srcprotopr,FIDN12R
033600140522      /copy gaitrasrc/srcprotopr,tnsd99r
033700131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
033800131230      /copy gaitrasrc/srcprotopr,TIBS0800R
033900140311      /copy gaitrasrc/srcprotopr,UBCHKCEL
034000140311      /copy gaitrasrc/srcprotopi,UBCHKCEL
034100140311      /copy gaitrasrc/srcprotopr,XEMAIL
034200140528      /copy gaitrasrc/srcprotopr,xsrlav8
034300140603      /copy gaitrasrc/srcprotopr,fnlv55r
034400131219
034500131219       //--------------------------------------------------------------
034600131219       //?Definizione parametri programma.
034700131219       //--------------------------------------------------------------
034800140519     D Fnlry00_Immetti...
034900140519     D                 PI
035000140519     D prmRqsOpCode...
035100140519     D                               10I 0 CONST
035200140519     D prmRpyOpCode...
035300140519     D                               10I 0
035400140519     D prmRpyIdMsg...
035500140519     D                               10I 0
035600140519     D prmRqsFormato...
035700140519     D                               10A   CONST
035800140519     D prmRqsData...
035900140519     D                            32767A   OPTIONS(*VARSIZE)
036000140519     D prmRqsDataSize...
036100140519     D                               10I 0 CONST
036200140519     D prmRpyFormato...
036300140520     D                               10A   CONST
036400140519     D prmRpyData...
036500140519     D                            32767A   OPTIONS(*VARSIZE)
036600140519     D prmRpyDataSize...
036700140519     D                               10I 0 CONST
036800140519     D prmRpyFormMsg...
036900140519     D                               10A   CONST OPTIONS(*NOPASS)
037000140519     D prmRpyMessage...
037100140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
037200140519     D prmRpyMsgSize...
037300140519     D                               10I 0 CONST OPTIONS(*NOPASS)
037400140519     D prmRpyFormForz...
037500140519     D                               10A   CONST OPTIONS(*NOPASS)
037600140519     D prmRpyForzatu...
037700140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
037800140519     D prmRpyForSize...
037900140519     D                               10I 0 CONST OPTIONS(*NOPASS)
038000131007
038100131007       //--------------------------------------------------------------
038200131007       //?MAIN.
038300131007       //--------------------------------------------------------------
038400131008
038500131007      /free
038600131008
038700131008       //?Operazioni iniziali
038800131008       exsr RoutInz;
038900131008
039000140523       //?Controllo formale dei dati di accesso
039100131008       exsr Controlla;
039200140523       //?Controllo stato bolla: deve essere in uno stato che renda possibile
039300140523       //?l'immissione delle disposizioni di consegna
039400140523       exsr StatoBolla;
039500140529
039600140523       //?Controllo che l'immagine della bolla da Web sia la stessa della bolla su
039700140523       //?AS/400
039800140529       //IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
039900140529       //   exsr ImmagineB;
040000140529       //endif;
040100140529
040200140529       //?Chaino estensioni bolla
040300140529       exsr sr_chainEstBolla;
040400140523       //?Controllo dettaglio dei campi ricevuti
040500140521       exsr ControllaDett;
040600140603       //?Per PUTISTRUCO in modalità Controllo + Inserimento scrivo file TIIDC
040700140611       IF  prmRqsOpCode=FNLRY00_RQSOPCODE_PUT_ISTRUCO and fnlry00i1.tipoela='2'
040800140611                                                      and fnlry00m0.nrmsg=0;
040900140604          exsr sr_Tiidc;
041000140613       // Restituzione messaggio di conferma
041100140613          exsr sr_MsgConf;
041200140603       endif;
041300131010
041400131010       //?Operazioni finali
041500131010       exsr RoutEnd;
041600131008
041700140613       //--------------------------------------------------------------
041800140613       //?Messaggio di avvenuta registrazione delle Indicazioni di Cons.
041900140613       //--------------------------------------------------------------
042000140613       BEGSR  sr_MsgConf;
042100140613
042200140929       // MESSAGGIO GENERICO
042300140617       wIdMsg   = 'TIS0771';
042400140617       fnlry00o1.MSGCONFERM=rtvMsgLang(wIdMsg:widlingua);
042500141007       fnlry00o1.MSGCONFERM=%trim(fnlry00o1.MSGCONFERM)+' del '
042600141007                                                  + %char(%date():*eur)
042700141007                                                  + ' '+  %char(%time()) ;
042800140613
042900140929       // MESSAGGIO SPECIFICO per Fermo Deposito e Consegna richiesta:
043000140929       // FERMO DEPOSITO
043100140929       select;
043200140929       when fnlry00i1.tipodispo='2';
043300161216        if w_tbo='A' and arbdam>0 ;
043400140930          exsr sr_wdesgio;
043500140924          wIdMsg   = 'TIS0772';
043600140924          wparm=wdesgio + %editc(WlimitFD:'X') ;
043700140929          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
043800140617          fnlry00o1.fdidfil=wfgslna;
043900161130        else ;
044000161216          clear wParm;
044100161216       // !!!!!!!!!!!!!!!!!!!!
044200161216       // Nuovo messaggio da inserire nel file messaggi:
044300161216       // "Il destinatario verrà contattato per il ritiro della merce "
044400161216          wIdMsg   = 'TIS0857';
044500161216          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
044600161216          fnlry00o1.fdidfil=wfgslna;
044700161130        endif;
044800140929       // APPUNTAMENTO
044900140929       when fnlry00i1.tipodispo='1';
045000140930          wIdMsg   = 'TIS0787';
045100140930          wparm=fnlry00i1.newdtcrich ;
045200140929       // presente orario
045300140929          if fnlry00i1.neworcrich>*zeros;
045400140930             wIdMsg   = 'TIS0785';
045500140930             w_orcric=fnlry00i1.neworcrich;
045600140930             exsr sr_dallealle;
045700140930            wparm=fnlry00i1.newdtcrich + w_dALLEmsg + W_ALLEMSG;
045800140929          endif;
045900140929       // Presente preferenza matt/pomeriggio
046000140929          if fnlry00i1.prconsric<>*blanks;
046100140929             wconsric=fnlry00i1.prconsric;
046200140929             exsr sr_matpom;
046300140930             wIdMsg   = 'TIS0789';
046400140929             wparm=fnlry00i1.newdtcrich + matpom ;
046500140929          endif;
046600140930          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
046700140929       // ALTRO INDIRIZZO
046800140930       when fnlry00i1.tipodispo='3';
046900140930       // presente orario --> determino il "Dalle - Alle"
047000140930          if fnlry00i1.neworcrici>*zeros;
047100140930             w_orcric=fnlry00i1.neworcrici;
047200140930             exsr sr_dallealle;
047300140930          endif;
047400140930       // Presente preferenza matt/pomeriggio --> Traduco il flag nella descrizione
047500140930          if fnlry00i1.prconsrici<>*blanks;
047600140930             wconsric=fnlry00i1.prconsrici;
047700140930             exsr sr_matpom;
047800140930          endif;
047900141001       // Messaggi in presenza della data
048000140930          if fnlry00i1.newdtcrici>*zeros;
048100140930             wIdMsg   = 'TIS0788';
048200140930             wparm=fnlry00i1.newdtcrici ;
048300140930             if fnlry00i1.neworcrici>*zeros;
048400140930                wIdMsg   = 'TIS0786';
048500140930                wparm=fnlry00i1.newdtcrici + w_dALLEmsg + W_ALLEMSG;
048600140930             endif;
048700140930       // Presente preferenza matt/pomeriggio
048800140930             if fnlry00i1.prconsrici<>*blanks;
048900140930                wIdMsg   = 'TIS0790';
049000140930                wparm=fnlry00i1.newdtcrici + matpom ;
049100140930             endif;
049200141001             fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
049300140930          else;
049400141001       // Messaggi in assenza della data (caso che non si verifica perchè se presente
049500141001       //   ora o preferenza devo avere anche la data)
049600140930             if fnlry00i1.neworcrici>*zeros;
049700140930                wIdMsg   = 'TIS0791';
049800140930                wparm=w_dALLEmsg + W_ALLEMSG;
049900141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
050000140930             endif;
050100140930       // Presente preferenza matt/pomeriggio
050200140930             if fnlry00i1.prconsrici<>*blanks;
050300140930                wIdMsg   = 'TIS0792';
050400140930                wparm=matpom ;
050500141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
050600140930             endif;
050700140930          endif;
050800140929
050900140929       endsl;
051000140613
051100140613       endsr;
051200140930       //--------------------------------------------------------------
051300140930       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
051400140930       //--------------------------------------------------------------
051500140930       BEGSR  sr_dallealle;
051600140930
051700140930       //   determino il Dalle/alle da mettere nel messaggio
051800140930            w_dalle=%time((%dec(w_orcric:4:0)*100):*hms)-%minutes(§VPORST_MW);
051900140930            w_alle=%time((%dec(w_orcric:4:0)*100):*hms)+%minutes(§VPORST_PW);
052000140930       //   determino orari servizio comprensivi della tolleranza
052100140930            if §ivptipo='S';
052200140930               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
052300140930               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
052400140930            else;
052500140930               w_dalleoser=%time((oor2miis *100):*hms)-%minutes(§VPORST_MT);
052600140930               w_alleoser=%time((oor2mxfs *100):*hms)+%minutes(§VPORST_MT);
052700140930            endif;
052800140930       //   Confronto dalle/alle calcolato con dalle/alle orari ser
052900140930       //      Dalle < del "dalle" orario ser --> prendo quest'ultimo e l'alle
053000140930       //      lo ricalcolo aggiungendo i minuti da tabella vpo
053100140930            if w_dalle<w_dalleoser;
053200140930               w_dalle=w_dalleoser;
053300140930               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
053400140930            endif;
053500140930       //      Alle  > del "alle" orario ser --> prendo quest'ultimo e il dalle
053600140930       //      lo ricalcolo sottraendo i minuti da tabella vpo purchè non diventi
053700140930       //      minore del "dalle" orario ser
053800140930            if w_alle>w_alleoser;
053900140930               w_alle=w_alleoser;
054000140930               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
054100140930                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
054200140930               endif;
054300140930            endif;
054400140930            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
054500140930            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
054600140930
054700140930       endsr;
054800131008       //--------------------------------------------------------------
054900131008       //?Operazioni iniziali.                                         ?
055000131008       //--------------------------------------------------------------
055100131008       BEGSR  RoutInz;
055200140211
055300140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
055400131008
055500140519         prmRpyOpCode = FNLRY00_RPYOPCODE_DONE;
055600140519         prmRpyIdMsg  = FNLRY00_ESITO_OK;
055700131009
055800131009       //?Data del giorno
055900131009         dataoggi = %dec(%date());
056000140604         annocur  = %subdt(%date():*years);
056100131009
056200131009       //?Pulisco ds di output
056300140519         reset Fnlry00o0;
056400140519         reset fnlry00o1;
056500140522         reset fnlry00m0;
056600140521
056700140521       //?Apertura file
056800140521         wlibfil=wlibfil201;
056900140521         %subst(wlibfil:11:8)='FNARB01L';
057000140521         open(e) fnarb01l;
057100140521         if not %open(fnarb01l);
057200140521            wlibfil=wlibfilprd;
057300140521            %subst(wlibfil:11:8)='FNARB01L';
057400140521            open fnarb01l;
057500140521         endif;
057600161128         %subst(wlibfil:11:8)='FNBLP01L';
057700161128         open(e) fnblp01l;
057800161128         if not %open(fnblp01l);
057900161128            wlibfil=wlibfilprd;
058000161128            %subst(wlibfil:11:8)='FNBLP01L';
058100161128            open fnblp01l;
058200161128         endif;
058300140521         wlibfil=wlibfil201;
058400140521         %subst(wlibfil:11:8)='FIPCT02L';
058500140521         open(e) fipct02l;
058600140521         if not %open(fipct02l);
058700140521            wlibfil=wlibfilprd;
058800140521            %subst(wlibfil:11:8)='FIPCT02L';
058900140521            open fipct02l;
059000140521         endif;
059100140522         wlibfil=wlibfil201;
059200140522         %subst(wlibfil:11:8)='FIAR501L';
059300140522         open(e) fiar501l;
059400140522         if not %open(fiar501l);
059500140522            wlibfil=wlibfilprd;
059600140522            %subst(wlibfil:11:8)='FIAR501L';
059700140522            open fiar501l;
059800140522         endif;
059900140528         wlibfil=wlibfil201;
060000140528         %subst(wlibfil:11:8)='FIAR401L';
060100140528         open(e) fiar401l;
060200140528         if not %open(fiar401l);
060300140528            wlibfil=wlibfilprd;
060400140528            %subst(wlibfil:11:8)='FIAR401L';
060500140528            open fiar401l;
060600140528         endif;
060700140807         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
060800140807            open tivpi00f ;
060900140807         endif ;
061000140528
061100140528       // Reperimento G.LIMITE X GIAC.CON DATA RIC
061200140528       clear wlimitdcr;
061300140528       chain (1:'2G':'1       ') tabel00f;
061400140528       if %found(tabel00f);
061500140528          ds2g=tbluni;
061600140528          wlimitdcr=§2gldr;
061700140528       endif;
061800140603       // Reperimento dati tabella IVP - Variabili programma per INTERNET
061900140603       clear tibs02ds;
062000140603       clear divp;
062100140603       t02mod='C';
062200140603       t02cod='IVP';
062300140603       t02ke1='1  ';
062400140603       t02tla='L  ';
062500140603       tibs02r(kpjba:tibs02ds);
062600140603       if t02err=*blanks;
062700140603          divp=t02uni;
062800140603       endif;
062900140613       // Reperimento Giorni lavorativi entro cui ritirare le sped. in FD
063000140613       clear wlimitFD ;
063100140613       chain (1:'2A':'F       ') tabel00f;
063200140613       if %found(tabel00f);
063300140613          ds2a=tbluni;
063400140613          wlimitFD =§2agla;
063500140613       endif;
063600140924       // Reperimento dati tabella VPO - ORARISER
063700140924       clear tibs02ds;
063800140925       clear dvpooraris;
063900140924       t02mod='C';
064000140924       t02cod='VPO';
064100140925       t02ke1='ORARISER';
064200140924       t02tla='L  ';
064300140924       tibs02r(kpjba:tibs02ds);
064400140924       if t02err=*blanks;
064500140924          dvpooraris=t02uni;
064600140924       endif;
064700141007       // Reperimento dati tabella CSP - Caratteri speciali per eliminazione
064800141007      *                           caratteri di punteggiatura da loc.dest.
064900141007       clear tibs02ds;
065000141007       clear charnook  ;
065100141007       t02mod='C';
065200141007       t02cod='CSP';
065300141007       t02ke1='LOC';
065400141007       t02tla='L  ';
065500141007       tibs02r(kpjba:tibs02ds);
065600141007       if t02err=*blanks;
065700141007          charnook=t02uni;
065800141007       endif;
065900141007     c* imposto stringa contenente caratteri speciali per il controllo
066000141007     c* della parte finale (uguale alla charnook maa senza l'eventuale '.')
066100141007     c                   eval      charnookf=%xlate('.':' ':charnook)
066200161128     c* carico le filiali abilitate all'immissione dispo di consegna su bolle in partenza
066300161128     c                   clear                   trulvpods
066400161128     c                   eval      ivpoke1 = 'DECOFIIDC'
066500161128     c                   call      'TRULVPOR'
066600161128     c                   parm                    trulvpods
066700161129      *
066800161129     c                   eval      wDecofiAll=*off
066900161129     c                   if        %lookup('999':skFilOk)>0
067000161129     c                   eval      wDecofiAll=*on
067100161129     c                   endif
067200161128     c
067300140521      /end-free
067400140521     C     Kcet          KLIST
067500140521     C                   KFLD                    pctfgs
067600140521     C                   KFLD                    pctNDC
067700140521     C                   KFLD                    arbpdc
067800140521     C                   KFLD                    TTRD              3
067900140521     C                   KFLD                    arbAAS
068000140521     C                   KFLD                    arbLNP
068100140521     C                   KFLD                    arbNRS
068200140521     C                   KFLD                    arbNSP
068300140521      /free
068400131008
068500131008         *inLR = *on;
068600131008
068700131008       ENDSR;
068800131008
068900131008       //--------------------------------------------------------------
069000131008       //?Controllo formale dei dati passati.
069100131008       //--------------------------------------------------------------
069200131008       BEGSR  Controlla;
069300131008
069400140519         clear wksu;
069500140520         clear widbolla;
069600140520         clear wbrtcode;
069700140522         clear wchi    ;
069800140527         clear widlingua;
069900140527         clear wcampo;
070000140519
070100131008       //?OpCode
070200140519         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_GET_ISTRUCO  and
070300140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_PUT_ISTRUCO  and
070400140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
070500140519           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
070600140519           prmRpyIdMsg  = FNLRY00_ESITO_RQSOPCODE_SCONOSCIUTO;
070700140210           exsr RoutEnd;
070800131008         ENDIF;
070900140408
071000140519       //?per GET_ISTRUCO
071100140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
071200140408       //?Formato e size RQS
071300140519           IF  prmRqsFormato = fnlry00I0.formato;
071400140519             fnlry00I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
071500140519             wksu=fnlry00i0.ksu;
071600140520             widbolla=fnlry00i0.idbolla;
071700140520             wbrtcode=fnlry00i0.brtcode;
071800140527             widlingua=fnlry00i0.liniso2;
071900140408           ELSE;
072000140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
072100140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
072200140408             exsr RoutEnd;
072300140408           ENDIF;
072400140408           IF  prmRqsDataSize > 0;
072500140408           ELSE;
072600140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
072700140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
072800140408             exsr RoutEnd;
072900140408           ENDIF;
073000140408         //?Formato e size RPY
073100140519           IF  prmRpyFormato = fnlry00O0.formato;
073200140408           ELSE;
073300140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
073400140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
073500140408             exsr RoutEnd;
073600140408           ENDIF;
073700140408           IF  prmRpyDataSize > 0;
073800140408           ELSE;
073900140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
074000140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
074100140408             exsr RoutEnd;
074200140408           ENDIF;
074300140408         ENDIF;
074400140210
074500140522       //?per PUT_ISTRUCO
074600140522         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
074700140522       //?Formato e size RQS
074800140522           IF  prmRqsFormato = fnlry00I1.formato;
074900140522             fnlry00I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
075000140522             wksu=fnlry00i1.ksu;
075100140522             widbolla=fnlry00i1.idbolla;
075200140522             wbrtcode=fnlry00i1.brtcode;
075300140527             widlingua=fnlry00i1.liniso2;
075400140522           ELSE;
075500140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
075600140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
075700140522             exsr RoutEnd;
075800140522           ENDIF;
075900140522           IF  prmRqsDataSize > 0;
076000140522           ELSE;
076100140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
076200140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
076300140522             exsr RoutEnd;
076400140522           ENDIF;
076500140522         //?Formato e size RPY
076600140522           IF  prmRpyFormato = fnlry00O1.formato;
076700140522           ELSE;
076800140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
076900140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
077000140522             exsr RoutEnd;
077100140522           ENDIF;
077200140522           IF  prmRpyDataSize > 0;
077300140522           ELSE;
077400140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
077500140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
077600140522             exsr RoutEnd;
077700140522           ENDIF;
077800140522       //?Formato e size MSG
077900140522         IF  prmRpyFormMsg = FNLRY00M0.formato;
078000140522         ELSE;
078100140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
078200140522           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
078300140522           exsr RoutEnd;
078400140522         ENDIF;
078500140522         IF  prmRpyMsgSize > 0;
078600140522         ELSE;
078700140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
078800140522           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
078900140522           exsr RoutEnd;
079000140522         ENDIF;
079100140523       //?Tipo Elaborazione
079200140523         if fnlry00i1.tipoela<> '1' and fnlry00i1.tipoela<> '2';
079300140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
079400140523           prmRpyIdMsg = FNLRY00_ESITO_TIPOELA_ERRATO;
079500140523           exsr RoutEnd;
079600140523         endif;
079700140523       //?Tipo Disposizione
079800140523         if fnlry00i1.tipodispo <> '1' and
079900140523            fnlry00i1.tipodispo <> '2' and
080000140523            fnlry00i1.tipodispo <> '3' and
080100151112            fnlry00i1.tipodispo <> '4' and
080200151112            fnlry00i1.tipodispo <> '5'     ;
080300140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
080400140523           prmRpyIdMsg = FNLRY00_ESITO_TIPODISPO_ERRATO;
080500140523           exsr RoutEnd;
080600140523         endif;
080700140522         ENDIF;
080800140624
080900140624       //?per CHK_ISTRUCO
081000140624         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
081100140624       //?Formato e size RQS
081200140624           IF  prmRqsFormato = FNLRY00_RQSFORMATO_CHK_ISTRUCO;
081300140624             TIIDCDS = %SUBST(prmRqsData:1:prmRqsDataSize);
081400140624             wksu=idcidcli     ;
081500141223             widbolla=%subst(%editc(idcwaas:'X'):3:2) + %editc(idcwlnp:'X') +
081600141223                             %editc(idcwnrs:'X') + %editc(idcwnsp:'X');
081700140627             if wksu=*blanks;
081800140627             wbrtcode=widbolla;
081900140627             exsr sr_trul28;
082000140627             wbrtcode=o280cod ;
082100140627             else;
082200140624             clear wbrtcode;
082300140627             endif;
082400140624             widlingua='it';
082500140624           ELSE;
082600140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
082700140624             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
082800140624             exsr RoutEnd;
082900140624           ENDIF;
083000140624           IF  prmRqsDataSize > 0;
083100140624           ELSE;
083200140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
083300140624             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
083400140624             exsr RoutEnd;
083500140624           ENDIF;
083600140624       // per comodità valorizzo campi FNLRY00I1 prendendoli dal record del TIIDC
083700140624           fnlry00i1.TIPODISPO = IDCTIPODIS;
083800140624           fnlry00i1.NEWREFCONS= IDCREF;
083900140624           fnlry00i1.NEWTELCONS= IDCTELD;
084000140624           fnlry00i1.NEWDESMAIL= IDCMAILAVV;
084100140624           fnlry00i1.NEWDESCELL= IDCTELAVV ;
084200140923       // Se no dispo di indirizzo alternativo
084300140923           if fnlry00i1.tipodispo<>'3';
084400140923              if idcdcr>0;
084500140923                 fnlry00i1.NEWDTCRICH= %char(%date(IDCDCR:*iso):*eur);
084600140923              endif;
084700140923              fnlry00i1.NEWORCRICH= %editc(IDCHCR:'X') ;
084800140923              fnlry00i1.NEWTPCRICH= IDCTCR    ;
084900140923              fnlry00i1.PRCONSRIC = IDCPRCR   ;
085000140923           endif;
085100140624           fnlry00i1.NEWRSD    = IDCRSD    ;
085200140624           fnlry00i1.NEWIND    = IDCIND    ;
085300140624           fnlry00i1.NEWCAD    = IDCCAD    ;
085400140624           fnlry00i1.NEWLOD    = IDCLOD    ;
085500140624           fnlry00i1.NEWPRD    = IDCPRD    ;
085600140624           fnlry00i1.NEWNZD    = IDCNAZ    ;
085700140624           fnlry00i1.ALTRO     = IDCNOTE   ;
085800140923       // Se indirizzo alternativo
085900140923           if fnlry00i1.tipodispo='3';
086000140923              if idcdcr>0;
086100140923                 fnlry00i1.NEWDTCRICI=%char(%date(IDCDCR:*iso):*eur);
086200140923              endif;
086300140923                 fnlry00i1.NEWORCRICi= %editc(IDCHCR:'X') ;
086400140923                 fnlry00i1.NEWTPCRICi= IDCTCR    ;
086500140923                 fnlry00i1.PRCONSRICi= IDCPRCR   ;
086600140923           endif;
086700140624
086800140624         //?Formato e size RPY
086900140624       //?Formato e size MSG
087000140624         IF  prmRpyFormMsg = FNLRY00M0.formato;
087100140624         ELSE;
087200140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
087300140624           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
087400140624           exsr RoutEnd;
087500140624         ENDIF;
087600140624         IF  prmRpyMsgSize > 0;
087700140624         ELSE;
087800140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
087900140624           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
088000140624           exsr RoutEnd;
088100140624         ENDIF;
088200140624         ENDIF;
088300140522
088400140210
088500140210       //?Formato e size Forzature
088600140522       //IF  prmRpyFormForz = fNLRY00F0.formato;
088700140522       //ELSE;
088800140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
088900140522       //  prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
089000140522       //  exsr RoutEnd;
089100140522       //ENDIF;
089200140522       //IF  prmRpyForSize > 0;
089300140522       //ELSE;
089400140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
089500140522       //  prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
089600140522       //  exsr RoutEnd;
089700140522       //ENDIF;
089800140519
089900140520       //?devo ricevere ID cliente unificante o BRTCODE
090000140520         if wksu=*blanks and wbrtcode=*blanks;
090100140520           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
090200140520           prmRpyIdMsg = FNLRY00_ESITO_ID_RICHIAMO_ERRATO;
090300140520           exsr RoutEnd;
090400140520         endif;
090500140520       //?Accesso per cliente unificante e ID Bolla
090600140519         if wksu <>*blanks;
090700140520       // ID Bolla mancante
090800140520           if widbolla=*blanks;
090900140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
091000140527              wIdMsg   = 'TIS0734';
091100140527              wIdCampo = 'IDBOLLA   ';
091200140527              clear wParm;
091300140527              exsr Messaggi;
091400140520              exsr RoutEnd;
091500140520           endif;
091600141003       // KSU può contenere solo numeri
091700141003           if %check(digits:wksu)>0;
091800141003                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
091900141003                      wIdMsg   = 'TIS0736';
092000141003                      wIdCampo = 'KSU       ';
092100141003                      wParm = widbolla;
092200141003                      exsr Messaggi;
092300141003                      exsr RoutEnd;
092400141003           endif;
092500140520       // ID cliente unificante
092600140520           clear tibs10ds ;
092700140520           d10tle='WW'  ;
092800140520           d10paf='F'    ;     // verifico se è padre
092900140520           d10cod=%dec(wksu:8:0);
093000140520           TIBS10R (tibs10ds)   ;
093100140520  1        if d10err<>*blanks   ;
093200140520           // Se errore verifico se è figlio
093300140520              clear tibs10ds ;
093400140520              d10tle='WW'        ;
093500140520              d10paf='P'    ;
093600140520              d10cod=%dec(wksu:8:0);
093700140520              TIBS10R (tibs10ds)   ;
093800140520           endif;
093900140520
094000140520  2        if d10err<>*blanks   ;
094100140520             // Imposto nella skiera e come padre solo se stesso
094200140520             skc(1)=%editc(d10cod:'X');
094300140520             d10cop=d10cod      ;
094400140520  2        endif                ;
094500140520       // Verifico correttezza dell'ID bolla
094600140520       // Deve contenere solo numeri
094700140520         if %check(digits:widbolla)>0;
094800140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
094900140527              wIdMsg   = 'TIS0734';
095000140527              wIdCampo = 'IDBOLLA   ';
095100140527              clear wParm;
095200140527              exsr Messaggi;
095300140520              exsr RoutEnd;
095400140520         endif;
095500140529       // Se ricevuto anche il BRTCODE lo controllo
095600140529         if wbrtcode<>*blanks;
095700140529            exsr sr_trul28;
095800140529       //   Id bolla e brtcode devono riferirsi alla stessa spedizione
095900140529            if widbolla<>%subst(wbrtcode:1:14) ;
096000140529              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
096100140529              wIdMsg   = 'TIS0751';
096200140529              wIdCampo = 'IDBOLLA   ';
096300140530              wparm = wbrtcode + widbolla;
096400140529              exsr Messaggi;
096500140529              wIdCampo = 'BRTCODE   ';
096600140529              exsr Messaggi;
096700140529              exsr RoutEnd;
096800140529            endif;
096900140529         endif;
097000161128       // La bolla deve esistere su archivio bolle:
097100140521          wbolla=widbolla;
097200140527          wcampo='IDBOLLA';
097300140521          exsr sr_chainbolla;
097400140520       // Verifico che il cliente sia il mittente o il destinatario della bolla
097500140522             if arbccm>0;
097600140522                eval wcliente='0000'+%editc(arbccm:'X');
097700140522                if %lookup(wcliente:skc)=0;
097800140522                   eval wcliente='0000'+%editc(arbksc:'X');
097900140522                   if %lookup(wcliente:skc)=0;
098000140522                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
098100140527                      wIdMsg   = 'TIS0736';
098200140527                      wIdCampo = 'KSU       ';
098300140530                      wParm = widbolla;
098400140527                      exsr Messaggi;
098500140522                      exsr RoutEnd;
098600140522                   else;
098700140522                      wchi='D';
098800140522                   endif;
098900140522                else;
099000140522                   wchi='M';
099100140522                endif;
099200140522             else;
099300140522                eval wcliente='0000'+%editc(arbksc:'X');
099400140522                if %lookup(wcliente:skc)=0;
099500140522                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
099600140527                   wIdMsg   = 'TIS0736';
099700140527                   wIdCampo = 'KSU       ';
099800140530                   wParm = widbolla;
099900140527                   exsr Messaggi;
100000140522                   exsr RoutEnd;
100100140522                else;
100200140610                   exsr sr_ds3A;
100300140528                      if %subst(§3atb1:1:1)='F';
100400140522       //                se franco    --> mittente
100500140522                         wchi='M';
100600140522                      else;
100700140522       //                se Assegnato --> Destinatario
100800140522                         wchi='D';
100900140522                      endif;
101000140522                endif;
101100140522             endif;
101200140519         else;
101300140520       //?Accesso per BRTCode
101400140529          exsr sr_trul28;
101500161128       // La bolla deve esistere su archivio bolle:
101600140521          wbolla=%subst(wbrtcode:1:14);
101700140527          wcampo='BRTCODE';
101800140521          exsr sr_chainbolla;
101900140522          wchi='D';
102000140519         endif;
102100140620       //?Verifico se presenti legami bolla
102200140620         exsr sr_lbl;
102300131008
102400131008       ENDSR;
102500140529       //--------------------------------------------------------------
102600140529       //?Controllo correttezza BRTCODE
102700140529       //--------------------------------------------------------------
102800140529       BEGSR  sr_trul28 ;
102900140529          clear trul28ds0;
103000140529          i280cod=wbrtcode;
103100140529          trul28r0(trul28ds0);
103200140529          if o280err='1';
103300140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
103400140529             wIdMsg   = 'TIS0737';
103500140529             wIdCampo = 'BRTCODE   ';
103600140530             wparm = wbrtcode ;
103700140529             exsr Messaggi;
103800140529             exsr RoutEnd;
103900140529          endif;
104000140529       ENDSR;
104100140523       //--------------------------------------------------------------
104200140523       //?Controlli stato della bolla per abilitare le istruzioni di consegna
104300161202       //--------------------------------------------------------------
104400140523       BEGSR  statobolla;
104500140530          clear wtentacons;
104600140620          clear wdcreur;
104700140620          clear wrsd2  ;
104800140620
104900140620       // DETErMINO IL GESTORE DELLA LNA
105000140620         clear fnlv55ds;
105100140620         d55lin=arblna;
105200140620         d55tpt='6';
105300140620         d55drf=dataoggi;
105400140620         fnlv55r(fnlv55ds);
105500140620         wfgslna=d55tfA;
105600140620         clear wfgslnad;
105700140620         chain (wfgslna) azorg01l;
105800140620         if %found(azorg01l);
105900140620            wfgslnad=orgdes;
106000140620         endif;
106100140620
106200140620         clear dataeur;
106300140620         if arbdcr>0 ;
106400140620            dataeur=%date(arbdcr:*iso);
106500140620            wdcreur=%char(dataeur);
106600140620         endif;
106700140620
106800140620         clear wrbhcr ;
106900140620         if arbhcr>0 ;
107000140620            wrbhcr=%editc(arbhcr:'X');
107100140620         endif;
107200140620
107300140620       //wtprich=%subst(arbgc1:2:1);
107400140620       //if wtprich=*blanks;
107500140620       //    wtprich=%subst(arbgc2:2:1);
107600140620       //endif;
107700140620
107800140620            chain (arbaas:arblnp:arbnrs:arbnsp:'D') fiar401l ;
107900140620            if %found(fiar401l);
108000140620               eval wrsd2=%subst(ar4not:1:35);
108100140620            endif;
108200140620
108300140530       //?deve essere diretta in Italia
108400140530         clear og143;
108500140530         chain (arblna) azorg01l;
108600140530         if %found(azorg01l);
108700140530            og143=orgde3;
108800140530            if §ogntw = 'EEX' or §ogntw= 'FED' or §ogntw= 'DPD';
108900140530               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
109000140530               wIdMsg   = 'TIS0739';
109100140530               wIdCampo = '*BOLLA    ';
109200140530               wParm = wbolla;
109300140530               exsr Messaggi;
109400140530               exsr routend;
109500140530            endif;
109600140530         endif;
109700161129        if w_tbo='A';
109800140523       //?Spedizione non deve essere consegnata
109900140523          if arbdcm>0;
110000141007            IF  prmRqsOpCode =  FNLRY00_RQSOPCODE_CHK_ISTRUCO;
110100141006                wIdMsg   = 'TISA738';
110200141006                wIdCampo = '*BOLLA    ';
110300141006                clear wParm ;
110400141006            else;
110500140527                wIdMsg   = 'TIS0738';
110600140527                wIdCampo = '*BOLLA    ';
110700140530                wParm = wbolla ;
110800141006            endif;
110900141006                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
111000140527                exsr Messaggi;
111100141006                exsr routend;
111200140523          else;
111300140523      /end-free
111400140523     c* prima vedo se c'e' da PDA
111500140708     c                   exsr      sr_RepPct
111600140523      /free
111700140523          endif;
111800140523       //?non deve avere giacenze o c.a. aperte
111900140620         chain (Arbaas:arblnp:arbnrs:arbnsp) tigcp21l;
112000140523         if %found(tigcp21l) and gcpatb=' ' and gcpdcg=0;
112100140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
112200141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
112300140527            wIdMsg   = 'TIS0740';
112400140527            wIdCampo = '*BOLLA    ';
112500140619            wParm = wbolla + wfgslnad;
112600141006           else;
112700141006            wIdMsg   = 'TISA740';
112800141006            wIdCampo = '*BOLLA    ';
112900141006            clear wParm ;
113000141006           endif;
113100140527            exsr Messaggi;
113200140523            exsr routend;
113300140523         endif;
113400140523         clear fidn12ds;
113500140620         i12aas=arbaas;
113600140620         i12lnp=arblnp;
113700140620         i12nrs=arbnrs;
113800140620         i12nsp=arbnsp;
113900140523         i12fel='E';
114000140523         i12fan='E';
114100140523         i12fch='E';
114200140523         fidn12r(fidn12ds);
114300140523         if o12nca>0;
114400140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
114500141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
114600140527            wIdMsg   = 'TIS0741';
114700140527            wIdCampo = '*BOLLA    ';
114800140530            wParm = wbolla;
114900141006           else;
115000141006            wIdMsg   = 'TISA741';
115100141006            wIdCampo = '*BOLLA    ';
115200141006            clear wParm ;
115300141006           endif;
115400140527            exsr Messaggi;
115500140523            exsr routend;
115600140523         endif;
115700140619       //?non deve essere bloccata
115800140917         if arbfbc<>*blanks and arbfbc<>'A';
115900140619            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
116000141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
116100140619            wIdMsg   = 'TIS0740';
116200140619            wIdCampo = '*BOLLA    ';
116300140619            wParm = wbolla + wfgslnad;
116400141006           else;
116500141006            wIdMsg   = 'TISA740';
116600141006            wIdCampo = '*BOLLA    ';
116700141006            clear wParm ;
116800141006           endif;
116900140619            exsr Messaggi;
117000140619            exsr routend;
117100140619         endif;
117200161129        endif;
117300140910       //?non deve avere un codice mancata consegna di tipo giacenza
117400140911       //if arbcmc <> *blanks;
117500140911       //  clear ds2a;
117600140911       //  w008a = arbcmc;
117700140911       //  chain (1:'2A':w008a) tabel00f;
117800140911       //  if %found(tabel00f);
117900140911       //     ds2a=tbluni;
118000140911       //  endif;
118100140911       //  if §2aftc='G';
118200140911       //     prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
118300140911       //     wIdMsg   = 'TIS0740';
118400140911       //     wIdCampo = '*BOLLA    ';
118500140911       //     wParm = wbolla + wfgslnad;
118600140911       //     exsr Messaggi;
118700140911       //     exsr routend;
118800140911       //  endif;
118900140911       //endif;
119000140530       //?deve essere già stato fatto almeno un tentativo di consegna
119100161129        if wDecofiAll=*on or %lookup(%editc(arblna:'X'):skFilOk)=0 ;
119200161202         if §IVPTENC='S' and (W_tbo='P' or (arbntc=0 and wtentacons=*blanks))  ;
119300140530             wIdMsg   = 'TIS0752';
119400140530             wIdCampo = '*NOTENTAT ';
119500140530             wParm = wbolla;
119600140530             exsr Messaggi;
119700140530             exsr routend;
119800140530         endif;
119900161129        endif;
120000141016       //?non deve essere una spedizione per i supermercati
120100141016         if arbtc1='S' or arbtc2='S'                          ;
120200141016             wIdMsg   = 'TIS0812';
120300141016             wIdCampo = '*BOLLA    ';
120400141016             clear wparm;
120500141016             exsr Messaggi;
120600141016             exsr routend;
120700141016         endif;
120800140604       //?Per la spedizione non devono esserci disposizioni ancora da elaborare
120900140624         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
121000140604          exsr sr_ChkDispo;
121100140624         endif;
121200140530       //?NON TROVATI ERRORI:
121300140530       //?Per GET_ISTRUCO --> attivo il bottone e restituisco i dati della bolla
121400140527         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
121500140527            fnlry00o0.BUTISTRCON='1';
121600170112            if   w_tbo='P' or (arbntc=0 and wtentacons=*blanks);
121700170113            wIdMsg  = 'TIS0859';
121800170112            else;
121900170113            wIdMsg  = 'TIS0858';
122000170112            endif;
122100170113            fnlry00o0.DESISTRCON=rtvMsgLang(wIdMsg:widlingua);
122200140527       // restituisco anche dati relativi alla bolla
122300140527            exsr sr_GetDatiBolla;
122400140527            endif;
122500140523
122600140527       endsr;
122700140523       //--------------------------------------------------------------
122800140523       //?Controlli immagine bolla da WEB con bolla su ARB
122900140523       //--------------------------------------------------------------
123000140523       BEGSR  immagineB;
123100140523       // Chaino estensioni bolla
123200140529       // exsr sr_chainEstBolla;
123300140523       // Errore se il referente di consegna sulla bolla è diverso dall'immagine bolla
123400140523       // che ricevo
123500140523          if wrefbolla<>fnlry00i1.oldrefcons or
123600140523             wtelbolla<>fnlry00i1.oldtelcons or
123700140523             §ar5eml<>fnlry00i1.olddesmail or
123800140528             §ar5tel<>fnlry00i1.olddescell           ;
123900140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
124000140527               wIdMsg   = 'TIS0742';
124100140528               wIdCampo = '*CHGDATA  ';
124200140527               clear wParm;
124300140527               exsr Messaggi;
124400140523               exsr routend;
124500140523          endif;
124600140527         select;
124700140527       // Disposizioni di Appuntamento
124800140527         when fnlry00i1.tipodispo = '1';
124900140527          if wdcreur<>fnlry00i1.olddtcrich or
125000140529             wrbhcr<>fnlry00i1.oldorcrich  or
125100140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
125200140527               wIdMsg   = 'TIS0742';
125300140527               wIdCampo = '*CHGDATA  ';
125400140527               clear wParm;
125500140527               exsr Messaggi;
125600140527               exsr routend;
125700140527          endif;
125800140527       // Disposizioni di consegna ad altro indirizzo
125900140527         when fnlry00i1.tipodispo = '3';
126000140527          if arbrsd+wrsd2 <>fnlry00i1.oldrsd or
126100140527             arbind<>fnlry00i1.oldind or
126200140527             arbcad<>fnlry00i1.oldcad or
126300140527             arblod<>fnlry00i1.oldlod or
126400140527             arbprd<>fnlry00i1.oldprd or
126500140527             arbnzd<>fnlry00i1.oldnzd ;
126600140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
126700140527               wIdMsg   = 'TIS0742';
126800140527               wIdCampo = '*CHGDATA  ';
126900140527               clear wParm;
127000140527               exsr Messaggi;
127100140527               exsr routend;
127200140527          endif;
127300140527         endsl;
127400140523       ENDSR;
127500140523       //--------------------------------------------------------------
127600140523       //?Controllo dettaglio
127700140523       //--------------------------------------------------------------
127800140523       BEGSR  ControllaDett;
127900140523
128000140617       //?Appuntamento
128100140617       // (se non caricate date da proporre per GetIstruCo il bottone non deve
128200140617       //  essere attivato)
128300140609       if (prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO and fnlry00o0.nrdate>0)
128400140624                                                     or fnlry00i1.tipodispo='1';
128500140523          exsr sr_appuntam;
128600140523       endif;
128700140617       //?Fermo Deposito
128800140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='2';
128900140523          exsr sr_FD;
129000140523       endif;
129100140617       //?Altro Indirizzo
129200140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='3';
129300140523          exsr sr_AltroInd;
129400140523       endif;
129500140617       //?Altre Istruzioni
129600140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='4';
129700140523          exsr sr_AltreIstruz;
129800140523       endif;
129900140617       //?Per GETISTRUCO -->  se bottoni tutti in OFF metto in OFF anche bottone
130000140617       //?per le istruzioni di consegna
130100140616         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
130200140616            and fnlry00o0.BUTAPPUNT='0'
130300140616            and fnlry00o0.BUTFERDEP='0'
130400140616            and fnlry00o0.BUTindiriz='0'
130500140616            and fnlry00o0.BUTaltro='0';
130600140616            fnlry00o0.BUTISTRCON='0';
130700140617             wIdMsg   = 'TIS0770';
130800140617             wIdCampo =  '*BOLLA    ';
130900140617             wParm = wbolla;
131000140617             exsr Messaggi;
131100140617             exsr routend;
131200140616         endif;
131300140924       //?Per GETISTRUCO -->  se in ON  bottone per appuntam o altro indirizzo
131400140924       //?valorizzo campo messaggio per la preferenza di orario
131500140924         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
131600140924            and (fnlry00o0.BUTAPPUNT='1'  or fnlry00o0.BUTindiriz='1');
131700140930               wtempoh=%div((§VPORST_MW + §VPORST_PW):60);
131800140930               wtempom=%rem((§VPORST_MW + §VPORST_PW):60);
131900140924               wIdMsg   = 'TIS0781';
132000140929               wparm=      %editc(wtempoh:'Z');
132100140924               if wtempom>0;
132200140929                  wIdMsg   = 'TIS0784';
132300140929                  wparm=      wparm + %editc(wtempom:'Z') ;
132400140924               endif;
132500140924               fnlry00o0.MSGPRFORA=rtvMsgLang(wIdMsg:widlingua:wparm);
132600140924         endif;
132700140523
132800140523       endsr;
132900140408
133000140521       //--------------------------------------------------------------
133100140521       //?Chain FNARB
133200140521       //--------------------------------------------------------------
133300140521       BEGSR  sr_chainbolla;
133400140527
133500161128          clear w_tbo ;
133600161129          clear fnarbds;
133700161129          exsr  sr_inzsoloblp;
133800140521       // imposto la chiave di accesso
133900140521          kaas=2000+%dec(%subst(wbolla:1:2):2:0);
134000140521          klnp=%dec(%subst(wbolla:3:3):3:0);
134100140521          knrs=%dec(%subst(wbolla:6:2):2:0);
134200140521          knsp=%dec(%subst(wbolla:8:7):7:0);
134300140521          chain (kaas:klnp:knrs:knsp) fnarb01l;
134400140521          if not %found(fnarb01l);
134500161128       //  NON TROVATA LA BOLLA IN ARRIVO -> la cerco su blp
134600161128           chain (kaas:klnp:knrs:knsp) fnblp01l;
134700161128       //    ERRORE SE:
134800161128       //    Non trovata neanche su blp oppure
134900161128       //    trovata su blp ma flaggata oppure
135000161129       //    filiale non ancora partita con la nuova procedura
135100161128             if not %found(fnblp01l) or
135200161128                  arbft1<>*blanks  or
135300161129                  (%lookup(%editc(arblna:'X'):skFilOk)=0  and
135400161128                   %lookup('999':skFilOk)=0 );
135500140521              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
135600140527              wIdMsg   = 'TIS0735';
135700140527              wIdCampo = wcampo      ;
135800140530              wParm=wbolla;
135900140527              exsr Messaggi;
136000140521              exsr RoutEnd;
136100161128             else;
136200161128             eval w_tbo='P';
136300161128             endif    ;
136400161128          else;
136500161128             eval w_tbo='A';
136600140521          endif;
136700141211       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
136800141211         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
136900141211           fnlry00i1.oldCAD    = arbCAD    ;
137000141211           fnlry00i1.oldLOD    = arbLOD    ;
137100141211           fnlry00i1.oldPRD    = arbPRD    ;
137200141211           fnlry00i1.oldNZD    = arbNzd    ;
137300141211         endif;
137400140521       endsr;
137500140620       //--------------------------------------------------------------
137600140620       //?Operazioni per bolle legate
137700140620       //--------------------------------------------------------------
137800140620       BEGSR  sr_lbl          ;
137900140620          chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl02l;
138000140620          if %found(fnlbl02l);
138100140624       // Per CHK_ISTRUCO --> errore se la bolla da elaborare ha una figlia
138200141223       //    IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
138300141223       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
138400141223       //             wIdMsg   = 'TIS0777';
138500141223       //             wIdCampo = '*NODISPO  ';
138600141223       //             wParm = wbolla;
138700141223       //             exsr Messaggi;
138800141223       //             exsr RoutEnd;
138900141223       //    endif;
139000140620       //    BOLLA CONSEGNATA
139100140620             if arbdcm>0;
139200140620       //       E' LA BOLLA ORIGINALE
139300140620                if LBLAAO=arbaas and LBLLPO=arblnp and
139400140620                   LBLNRO=arbnrs and LBLNSO=arbnsp;
139500140620       //       Non è il Mittente --> Errore: utente non abilitato a dare disposizioni
139600140620                   if wchi <>'M';
139700140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
139800140620                      wIdMsg   = 'TIS0736';
139900140620                      wIdCampo = '*NOAUT    ';
140000140620                      wParm = wbolla;
140100140620                      exsr Messaggi;
140200140620                      exsr RoutEnd;
140300140620                   endif;
140400140620                else;
140500140620       //       NON E' LA BOLLA ORIGINALE
140600140620       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
140700140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
140800140627                      wIdMsg   = 'TIS0770';
140900140620                      wIdCampo = '*NODISPO  ';
141000140620                      wParm = wbolla;
141100140620                      exsr Messaggi;
141200140620                      exsr RoutEnd;
141300140620                endif;
141400140620             endif;
141500140620       //    Recupero i dati dell'ultima figlia
141600140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
141700140620             dow %found(fnlbl02l);
141800140620                chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
141900140620             enddo;
142000161129             clear fnarbds;
142100161129             exsr  sr_inzsoloblp;
142200140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnarb01l;
142300140620             if not %found(fnarb01l);
142400140627       //    Se non trovo l'ultima figlia su arb errore
142500140627       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
142600140627                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
142700140627                      wIdMsg   = 'TIS0777';
142800140627                      wIdCampo = '*NODISPO  ';
142900140627                      wParm = wbolla;
143000140627                      exsr Messaggi;
143100140627                      exsr RoutEnd;
143200161128             else;
143300161128                eval w_tbo='A';
143400140620             endif;
143500141223       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
143600141223         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
143700141223           fnlry00i1.oldCAD    = arbCAD    ;
143800141223           fnlry00i1.oldLOD    = arbLOD    ;
143900141223           fnlry00i1.oldPRD    = arbPRD    ;
144000141223           fnlry00i1.oldNZD    = arbNzd    ;
144100141223         endif;
144200140620          endif;
144300140620       endsr;
144400140523       //--------------------------------------------------------------
144500140523       //?Chain Estensioni bolla
144600140523       //--------------------------------------------------------------
144700140523       BEGSR  sr_chainEstBolla;
144800140523          clear dar5gen;
144900140523          clear wrefbolla;
145000140523          clear wtelbolla;
145100140620          chain (arbaas:arblnp:arbnrs:arbnsp:'GEN') fiar501l ;
145200140523          if %found(fiar501l);
145300140523             dar5gen=ar5uni;
145400140523       // Se presente # nel referente e nel telefono
145500140523       // lo devo togliere
145600140523             if %subst(§ar5ref:1:1)='#';
145700140523                eval wrefbolla=%subst(§ar5ref:2);
145800140523             else;
145900140523                eval wrefbolla=§ar5ref;
146000140523             endif;
146100140523             if %subst(§ar5teld:1:1)='#';
146200140523                eval wtelbolla=%subst(§ar5teld:2);
146300140523             else;
146400140523                eval wtelbolla=§ar5teld;
146500140523             endif;
146600140523          endif;
146700140523          clear dar5emd;
146800140620          chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
146900140523          if %found(fiar501l);
147000140523              dar5emd=ar5uni;
147100140523         endif;
147200140523       ENDSR;
147300140522       //--------------------------------------------------------------
147400140522       //?Recupera dati bolla da passare a chiamante
147500140522       //--------------------------------------------------------------
147600140522       BEGSR  sr_GetDatiBolla;
147700140603       // - orari da passare nella tendina: standard o minimo/massimo in base
147800140603       //   a quanto memorizzato in tabella
147900140923          clear waltroind;
148000140522          exsr sr_orarisr;
148100140604          if §ivptipo='M';
148200140604             fnlry00o0.ORARITNDI=%editc(OOR2MIIS:'X');
148300140604             fnlry00o0.ORARITNDF=%editc(OOR2MXFS:'X');
148400140604          else;
148500140604             fnlry00o0.ORARITNDI=%editc(OOR2STIS:'X');
148600140604             fnlry00o0.ORARITNDF=%editc(OOR2STFS:'X');
148700140604          endif;
148800140923       // - per la tendina con orari di filiale -->
148900140923         clear tisio9ds;
149000140923         do0lna=arblna;
149100140923         do0tric='S';
149200140923         do0tser='C';
149300140923         tisio9r (kpjba:tisio9ds);
149400140923          if §ivptipo='M';
149500140923             fnlry00o0.ORARITNFI=%editc(DO0OMIIS:'X');
149600140923             fnlry00o0.ORARITNFF=%editc(DO0OMXFS:'X');
149700140923          else;
149800140923             fnlry00o0.ORARITNFI=%editc(DO0OSTIS:'X');
149900140923             fnlry00o0.ORARITNFF=%editc(DO0OSTFS:'X');
150000140923          endif;
150100140603          fnlry00o0.MMINTERVC=%editc(§ivpmini:'X');
150200161206       //?DATE DI CONSEGNA DA PROPORRE: -------------------
150300161205       // 1) Giorni di attesa prima di aprire giacenza:
150400161205       //    verifico se previsti giorni personalizzati per cliente
150500140605         exsr sr_PersGGiac;
150600161214       // 2) Verifico se effetuato almeno un tentativo di consegna
150700161214         if w_tbo='A' ;
150800140709            exsr sr_RepPct   ;
150900140709         ENDIF;
151000161206       //?Data Minima . . . . . .:
151100161206       //?Data Da cui inizia il conteggio del giorno limite per l'apertura giacenza:
151200161205       // vengono proposte le date fino al giorno limite previsto per apertura giacenza
151300161216       exsr sr_date;
151400161206
151500161206       // 3) Ciclo per caricare campo delle date consegna richiesta da proporre
151600161206       //    Parto dalla data da cui iniza il conteggio per l'apertura giac (WDATA) e
151700161206       //    scarto le date inferiori alla data minima calcolata (WDATAM)
151800140605         clear ix;
151900140923         clear iy;
152000140605         for I=1 to wlimitdcr;
152100140606       //   se cade in un giorno festivo incremento la data di un giorno
152200140606            exsr sr_datafes;
152300140606       //   Propongo la data di oggi solo se possibile la riconsegna in giornata
152400140709            if (wdata=dataoggi and oor2fric='S'
152500161206                and §ivparic='S' and wtentacons='1') or
152600161206                (wdata<>dataoggi and wdata>=wdatam);
152700140609       //   Memorizzo la data, purchè, in presenza di data cons.rich "Dopo il",
152800140609       //   la stessa non sia <= della data "Dopo Il"
152900140710             if arbtcr<>'D' or (arbtcr='D' and wdata>arbdcr);
153000140606               ix+=1;
153100140606               fnlry00o0.schdcr(ix)=%char(%date(wdata:*iso):*eur);
153200140923       //      la data per l'indirizzo alternativo non deve mai essere "oggi"
153300140923               if wdata<>dataoggi;
153400140923                  iy+=1;
153500140923                  fnlry00o0.schdcri(iy)=%char(%date(wdata:*iso):*eur);
153600140923                  fnlry00o0.nrdatei+=1;
153700140923               endif;
153800140606       //      Data da Consigliare --> quella successiva a oggi oppure
153900140606       //      quella della bolla se maggiore
154000140606               if fnlry00o0.dtconsric=*blanks  and wdata<>dataoggi ;
154100140606                   fnlry00o0.dtconsric=%char(%date(wdata:*iso):*eur);
154200140606                   if arbdcr>0 and arbtcr=*blanks and arbdcr>wdata;
154300140606                      fnlry00o0.dtconsric=%char(%date(arbdcr:*iso):*eur);
154400140606                   endif;
154500140606               endif;
154600140606       //   Incremento contatore date da passare a chiamante
154700140606               fnlry00o0.nrdate+=1;
154800140609             endif;
154900140606            endif;
155000140606       //   Incremento la data di un giorno
155100140606            exsr sr_IncreData;
155200140605         endfor;
155300140522       // - consegna richiesta: data/ora/tipo
155400140606       //if arbdcr>0 ;
155500140606       //   fnlry00o0.DTCONSRIC=wdcreur;
155600140606       //endif;
155700140606       //if arbhcr>0;
155800140606       //   fnlry00o0.ORCONSRIC=%editc(arbhcr:'X');
155900140606       //endif;
156000140606       //fnlry00o0.TPCONSRIC=arbtcr;
156100140613       // indirizzo e-mail e n. telefono per SMS -----------
156200140620         chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
156300140522         if %found(fiar501l);
156400140522             dar5emd=ar5uni;
156500140522             fnlry00o0.destmail=§ar5eml;
156600140522             fnlry00o0.destcell=§ar5tel;
156700140522         endif;
156800140613       // Messaggio di avviso per indicare che il FD deve essere autorizzato dal mitt. ------------
156900140522         if arbffd=*blanks and arbgma<>*blanks;
157000140529            exsr sr_tab7r;
157100140522            if §7rfd='S' and wchi='D';
157200140522               wIdMsg   = 'TIS0733';
157300140617               fnlry00o0.MSGFDMITT=rtvMsgLang(wIdMsg:widlingua);
157400140522            endif;
157500140522         endif;
157600140522       endsr;
157700140606       //--------------------------------------------------------------
157800140606       //?Controllo data se festiva + restituzione data prima data non festiva
157900140606       //--------------------------------------------------------------
158000140606       BEGSR  sr_DataFes;
158100140606       wfes='1';
158200140606       dow wfes='1';
158300161206       chain (0:wfgslna:%subdt(%date(wdata):*years):
158400161206                        %subdt(%date(wdata):*MONTHS)) azcln01l;
158500161206          if %found(azcln01l) and mat(%subdt(%date(wdata):*days)) = 'F'
158600161206                               or pom(%subdt(%date(wdata):*days)) = 'F';
158700140606       // data festiva: la incremento di un giorno
158800140606             exsr sr_incredata;
158900140606          else;
159000140606            clear wfes;
159100140606          endif;
159200140606       enddo;
159300140606       endsr;
159400140606       //--------------------------------------------------------------
159500140606       //?Incremento data di un giorno
159600140606       //--------------------------------------------------------------
159700140606       BEGSR  sr_incredata;
159800161206             dataiso=%date(wdata);
159900140606             dataiso=dataiso+%days(1);
160000161206             wdata=%dec(dataiso);
160100140606       endsr;
160200140521       //--------------------------------------------------------------
160300140521       //?Controlli per abilitare la richiesta di appuntamento
160400140521       //--------------------------------------------------------------
160500140521       BEGSR  sr_appuntam;
160600140528
160700140528       // Non ammesso per spedizioni in fermo deposito
160800140528         if arbffd= 'S';
160900140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
161000140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
161100140528               leavesr;
161200140528            else;
161300140528       //?PUT_ISTRUCO --> uscita da pgm con errore
161400140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
161500141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
161600140528              wIdMsg   = 'TIS0743';
161700140528              wIdCampo = '*NOAPPUNT ';
161800140530              wParm = wbolla ;
161900141006         else;
162000141006              wIdMsg   = 'TISA743';
162100141006              wIdCampo = '*NOAPPUNT ';
162200141006              clear wParm  ;
162300141006         endif;
162400140528              exsr Messaggi;
162500140528              exsr routend;
162600140528            endif;
162700140528         endif;
162800140528
162900140528       //?GET_ISTRUCO --> attivazione bottone per appuntamento
163000140528         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
163100140528            fnlry00o0.BUTAPPUNT='1';
163200140528         else;
163300140528       //?PUT_ISTRUCO --> controllo dati ricevuti
163400140528            exsr sr_ctrApp;
163500140528         endif;
163600140528
163700140521       endsr;
163800140522       //--------------------------------------------------------------
163900140522       //?Controlli per abilitare la richiesta di Fermo Deposito
164000140522       //--------------------------------------------------------------
164100140522       BEGSR  sr_FD      ;
164200140522
164300140528       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
164400140528          if arbffd = 'S';
164500140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
164600140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
164700140528               leavesr;
164800140528            else;
164900140528       //?PUT_ISTRUCO --> uscita da pgm con errore
165000141223            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
165100140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
165200141223       //if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
165300140528              wIdMsg   = 'TIS0743';
165400140528              wIdCampo = '*NOFD     ';
165500140530              wParm = wbolla;
165600141223       //else;
165700141223       //     wIdMsg   = 'TISA743';
165800141223       //     wIdCampo = '*NOFD     ';
165900141223       //     clear wParm;
166000141223       //endif;
166100140528              exsr Messaggi;
166200140528              exsr routend;
166300141223            endif;
166400140528            endif;
166500140528          else;
166600140530       //?GET_ISTRUCO -->
166700141008            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
166800141008       //   and fnlry00o0.MSGFDMITT=*blanks;
166900140616       //?GET_ISTRUCO -->> Restituisco, insieme al bottone attivato anche messaggio
167000140616       //?                 riguardante le codizioni di ritiro in FD
167100140616       //?                 Solo se msgfdmit=*blanks
167200140616             if fnlry00o0.msgfdmitt=*blanks;
167300141008                fnlry00o0.BUTFERDEP='1';
167400161216              if w_tbo='A' and arbdam>0;
167500140924                exsr sr_wdesgio;
167600140617               wparm=wdesgio + %editc(WlimitFD:'X') + wfgslnad;
167700140616               wIdMsg   = 'TIS0768';
167800161216              else;
167900161216               clear wparm;
168000161216               wIdMsg   = 'TIS0857';
168100161216              endif;
168200140617               fnlry00o0.MSGFDCOND=rtvMsgLang(wIdMsg:widlingua:wparm);
168300141008             else;
168400141008                fnlry00o0.BUTFERDEP='2';
168500140616             endif;
168600140616            endif;
168700140606       //?PUT_ISTRUCO --> Controllo dati ricevuti
168800140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
168900140624               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
169000140616       //?                Errore al destinatario se è richiesta autorizzaz.
169100140616       //?                da parte del mittente
169200140616             if arbgma<>*blanks;
169300140616                exsr sr_tab7r;
169400140616                if §7rfd='S' and wchi='D';
169500140616                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
169600140616                   wIdMsg   = 'TIS0733';
169700140616                   wIdCampo = '*NOFD     ';
169800140616                   clear wParm;
169900140616                   exsr Messaggi;
170000140616                   exsr routend;
170100140616                endif;
170200140616             endif;
170300140606             exsr sr_ctrUNI;
170400140606            endif;
170500140530          endif;
170600140522       endsr;
170700140522       //--------------------------------------------------------------
170800140522       //?Controlli per abilitare la richiesta di Altro Indirizzo
170900140522       //--------------------------------------------------------------
171000140522       BEGSR  sr_altroind;
171100140522
171200140616       if §ivpdind='S';
171300140617       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
171400140617          if arbffd = 'S';
171500140617       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
171600140617            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
171700140617               leavesr;
171800140617            else;
171900140617       //?PUT_ISTRUCO --> uscita da pgm con errore
172000140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
172100141007           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
172200140617              wIdMsg   = 'TIS0743';
172300140617              wIdCampo = '*NOALTRIND';
172400140617              wParm = wbolla;
172500141007           else;
172600141007              wIdMsg   = 'TISA743';
172700141007              wIdCampo = '*NOALTRIND';
172800141007              clear wparm ;
172900141007           endif;
173000140617              exsr Messaggi;
173100140617              exsr routend;
173200140617            endif;
173300140617          endif;
173400140619       //NON AMMESSO AL DESTINATARIO SE NON ABILITATO MEDIANTE TABELLA 7W
173500140619         clear ds7W;
173600140619         if wchi='D' and arbgga<>*blanks;
173700140619            w008a=ARBGGA;
173800140619            chain (1 : '7W' : W008A) tabel00f;
173900140619            if %found(tabel00f);
174000140619               ds7w=tbluni;
174100140619            endif;
174200140619         endif;
174300140619         if wchi='D' and §7wdes<>'S';
174400140619       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
174500140918       //?                18/09/2014 --> attivo il bottone ma poi verrà dato errore
174600140918       //?                Il concetto è: il cambio di indirizzo è gestito (e quindi il
174700140918       //?                bottone deve essere attivo) ma non può essere richiesto da
174800140918       //?                destinatario se non autorizzato (e quindi poi viene dato errore)
174900140918            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
175000141007               fnlry00o0.BUTindiriz='2';
175100141007               wIdMsg   = 'TIS0796';
175200141007               fnlry00o0.MSGAUTIND=rtvMsgLang(wIdMsg:widlingua);
175300141007               leavesr;
175400140619            else;
175500140619       //?PUT_ISTRUCO --> uscita da pgm con errore
175600140619              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
175700140619              wIdMsg   = 'TIS0776';
175800140619              wIdCampo = '*NOALTRIND';
175900140924              clear wparm;
176000140924       //     wParm = wbolla;
176100140619              exsr Messaggi;
176200140619              exsr routend;
176300140619            endif;
176400140619         endif;
176500140522         fnlry00o0.BUTindiriz='1';
176600140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
176700140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
176800140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
176900140611       //?Controllo dati comuni a tutti tipi di disposizione
177000140611             exsr sr_ctrUNI;
177100140611
177200140609             exsr sr_ctrind;
177300140609            endif;
177400140616       else;
177500140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
177600140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
177700141016               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
177800141016                 prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
177900141016                 prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
178000141016                 exsr RoutEnd;
178100140616            endif;
178200140616       endif;
178300140522       endsr;
178400140522       //--------------------------------------------------------------
178500140522       //?Controlli per abilitare la richiesta di Altre Istruzioni
178600140522       //--------------------------------------------------------------
178700140522       BEGSR  sr_AltreIstruz;
178800140522
178900140616       if §ivpdalr='S';
179000140522         fnlry00o0.BUTaltro='1';
179100140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
179200140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
179300140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
179400140609             exsr sr_ctrUNI;
179500140609            endif;
179600140616       else;
179700140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
179800140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
179900140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
180000140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
180100140617              prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
180200140617              exsr RoutEnd;
180300140616            endif;
180400140616       endif;
180500140522       endsr;
180600140522
180700140523       //--------------------------------------------------------------
180800140523       //?Controllo Disposizioni di Appuntamento
180900140523       //--------------------------------------------------------------
181000140523       BEGSR  sr_ctrApp     ;
181100140611       //?Controllo dati comuni a tutti tipi di disposizione
181200140611          exsr sr_ctrUNI;
181300140528       //?Consegna richiesta
181400140528          exsr sr_ctrConsRich;
181500140523       endsr;
181600140528       //--------------------------------------------------------------
181700140528       //?Controllo Data/ora/tipo consegna richiesta
181800140528       //--------------------------------------------------------------
181900140528       BEGSR  sr_CtrConsRich;
182000140923          clear waltroind;
182100140606          exsr sr_Orarisr;
182200140528       // DATA CONSEGNA RICHIESTA:
182300140528          exsr sr_ctrDCR;
182400140528       // ORA  CONSEGNA RICHIESTA:
182500140528       if fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
182600140528       // Deve contenere solo numeri
182700140528         if %check(digits:fnlry00i1.neworcrich)>0;
182800140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
182900140528              wIdMsg   = 'TIS0746';
183000140528              wIdCampo = 'NEWORCRICH';
183100140530              wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
183200140530                      %subst(fnlry00i1.neworcrich:3:2);
183300140528              exsr Messaggi;
183400140528         else;
183500140709       // Se data consegna richiesta diversa da oggi (no riconsegna)
183600140709       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
183700140528              wora = %dec(fnlry00i1.neworcrich:4:0);
183800141216              if wdcr<>wdataconfr;
183900140709                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
184000140709                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
184100140709                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
184200140709                       wIdMsg   = 'TIS0747';
184300140709                       wIdCampo = 'NEWORCRICH';
184400140709                       wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
184500140709                       %subst(fnlry00i1.neworcrich:3:2);
184600140709                       exsr Messaggi;
184700140709                 endif;
184800140709              else;
184900140709       //  Se Data Consegna Richiesta=oggi (riconsegna) l'orario deve essere incluso negli orari
185000140610       //  inizio/fine previsti per la riconsegna
185100140709                 if  wora < OOR2RIDS  or
185200140709                     wora > OOR2RIAS ;
185300140709                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
185400140709                        wIdMsg   = 'TIS0778';
185500140709                        wIdCampo = 'NEWORCRICH';
185600140709                        wParm = %editw(oor2rids:'  :  ') +
185700140709                                %editW(oor2rias:'  :  ')  ;
185800140709                        exsr Messaggi;
185900140709                 endif;
186000140709              endif;
186100140528         endif;
186200140528       endif;
186300140529       // PREFERENZA MATTINO/POMERIGGIO
186400140529       if fnlry00i1.prconsric<>*blanks and
186500140529          fnlry00i1.prconsric<>'M' and
186600140529          fnlry00i1.prconsric<>'P'               ;
186700140528                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
186800140528                    wIdMsg   = 'TIS0749';
186900140529                    wIdCampo = 'PRCONSRIC ';
187000140528                    clear wParm;
187100140528                    exsr Messaggi;
187200140528       endif;
187300140710       // NO RICONSEGNA:
187400140710       // Verifico preferenza m/p con gli orari standard o min/max in base
187500140710       // all'ora limite del mattino
187600141216       if wdcr<>wdataconfr;
187700140710         if (fnlry00i1.prconsric='P' and §ivptipo='S' and §ivporlm>=OOR2STFS) or
187800140710            (fnlry00i1.prconsric='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
187900140710            (fnlry00i1.prconsric='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS) or
188000140710            (fnlry00i1.prconsric='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
188100140710            If fnlry00i1.prconsric='P';
188200140710                widmsg='TIS0676';
188300140710            else;
188400140710                widmsg='TIS0667';
188500140710            endif;
188600140710            matpom=rtvMsgLang(widmsg : widlingua);
188700140710                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
188800141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
188900140710                      wIdMsg   = 'TIS0760';
189000140710                      wIdCampo = 'PRCONSRIC ';
189100140710                      wParm = MatPom + wbolla;
189200141006         else;
189300141006                      wIdMsg   = 'TISA760';
189400141006                      wIdCampo = 'PRCONSRIC ';
189500141006                      wParm = MatPom ;
189600141006         endif;
189700140710                      exsr Messaggi;
189800140710         ENDIF;
189900140710       ELSE;
190000140710       // SI RICONSEGNA:
190100140710       // Verifico preferenza m/p con gli orari previsti per la riconsegna
190200140710       // in base all'ora limite del mattino
190300140710          if (fnlry00i1.prconsric='P' and §ivporlm>=OOR2rias) or
190400140710             (fnlry00i1.prconsric='M' and §ivporlm<OOR2rids);
190500140710             If fnlry00i1.prconsric='P';
190600140710                 widmsg='TIS0676';
190700140710             else;
190800140710                 widmsg='TIS0667';
190900140710             endif;
191000140710             matpom=rtvMsgLang(widmsg : widlingua);
191100140710                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
191200140710                       wIdMsg   = 'TIS0779';
191300140710                       wIdCampo = 'PRCONSRIC ';
191400140710                       wParm = MatPom ;
191500140710                       exsr Messaggi;
191600140710          ENDIF;
191700140606       endif;
191800140529       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
191900140529       if fnlry00i1.prconsric<>*blanks and
192000140529          fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
192100140529                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
192200140530                    wIdMsg   = 'TIS0753';
192300140530                    wIdCampo = 'NEWORCRICH';
192400140530                    clear wParm;
192500140529                    exsr Messaggi;
192600140530                    wIdCampo = 'PRCONSRIC ';
192700140529                    exsr Messaggi;
192800140529       endif;
192900140528       //  Se Campi tutti vuoti errore
193000140529       //if (fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros) and
193100140529       //   (fnlry00i1.neworcrich=*blanks or fnlry00i1.neworcrich=*zeros) and
193200140529       //    fnlry00i1.prconsric=*blanks;
193300140529       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
193400140529       //             wIdMsg   = 'TIS0748';
193500140529       //             wIdCampo = '*NOSELEZ';
193600140529       //             clear wParm;
193700140529       //             exsr Messaggi;
193800140529       //endif;
193900140528       endsr;
194000140528       //--------------------------------------------------------------
194100140528       //?Controllo Data Consegna richiesta
194200140528       //--------------------------------------------------------------
194300140528       BEGSR  sr_ctrDCR     ;
194400140528       // DATA CONSEGNA RICHIESTA:
194500140529       // Obbligatoria
194600140529       if fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros;
194700140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
194800140529             wIdMsg   = 'TIS0130';
194900140529             wIdCampo = 'NEWDTCRICH';
195000140529             clear wParm;
195100140529             exsr Messaggi;
195200140529             leavesr;
195300140529       endif;
195400140528       //  Controllo correttezza formale
1955001405282        monitor  ;
195600140528         dataiso=%date(fnlry00i1.newdtcrich:  *eur)   ;
195700140528         on-error ;
195800140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
195900140528             wIdMsg   = 'TIS0121';
196000140528             wIdCampo = 'NEWDTCRICH';
196100140528             clear wParm;
196200140528             exsr Messaggi;
196300140528             leavesr;
196400140528 2       endmon  ;
196500140528
196600140528       wdcr=%dec(dataiso);
196700140528
196800140528       //  Non inferiore a oggi
196900140528       if wdcr < dataoggi;
197000140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
197100140528             wIdMsg   = 'TIS0744';
197200140528             wIdCampo = 'NEWDTCRICH';
197300140611             wParm=%char(%date(wdcr:*iso):*eur);
197400140528             exsr Messaggi;
197500140528             leavesr;
197600140528       endif;
197700161214       if w_tbo='A' ;
197800161202        exsr sr_reppct;
197900161202       endif;
198000161130       //  Non inferiore a data prevista consegna se spedizione da partire o senza
198100161129       //  Tentativi di consegna
198200161202       if (w_tbo='P' or (arbntc=0 and wtentacons=' ')) and wdcr < dataPrvC;
198300161129             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
198400170113             wIdMsg   = 'TIS0860';
198500161129             wIdCampo = 'NEWDTCRICH';
198600161129             wParm=%char(%date(wdcr:*iso):*eur);
198700161129             exsr Messaggi;
198800161129             leavesr;
198900161129       endif;
199000140709       //  Uguale ad oggi solo se possibile la riconsegna in giornata
199100141215       //  Per chkistruco considero "data oggi" la data in cui è stata
199200141215       //  fatta la  richiesta
199300141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
199400141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
199500141215         else;
199600141218              wdataconfr=dataoggi;
199700141215         endif;
199800141215       if wdcr=wdataconfr;
199900161202       //if oor2fric='S' and §ivparic='S';
200000161202       // exsr sr_reppct;
200100161202       //endif;
200200140709         if oor2fric='N' or §ivparic<>'S' or wtentacons=' ';
200300140606             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
200400140606             wIdMsg   = 'TIS0759';
200500140606             wIdCampo = 'NEWDTCRICH';
200600140606             clear wparm;
200700140606             exsr Messaggi;
200800140606             leavesr;
200900140709         endif;
201000140606       endif;
201100140528       //  Non superiore ai Giorni limite giac. con data cons.rich.
201200140528       //      Verifico se presenti limiti personalizzati per il cliente
201300140528
201400140605       exsr sr_PersGGiac;
201500161206       //Calcolo giorni lavorativi fra data inizio conteggio e data consegna richiesta
201600161216       exsr sr_date;
201700161214       datada=wdata;
201800140528       dataa=wdcr;
201900140606       clear tfptfa;
202000140610       p_tfa =wfgslna;
202100140606       xsrlav8(wdat8:tfptfa);
202200140528       if ggl > wlimitdcr;
202300140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
202400140528             wIdMsg   = 'TIS0745';
202500140528             wIdCampo = 'NEWDTCRICH';
202600140611             wParm=%char(%date(wdcr:*iso):*eur);
202700140528             exsr Messaggi;
202800140528             leavesr;
202900140528       endif;
203000140606       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
203100140528       //  Controllo anche con la data consegna richiesta originale
203200140609       if (arbtcr='D' and wdcr<=arbdcr) or
203300140609         (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
203400140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
203500140529             wIdMsg   = 'TIS0750';
203600140528             wIdCampo = 'NEWDTCRICH';
203700140606             if arbtcr='D' ;
203800140606             dataeur=%date(%dec(arbdcr:8:0):*iso);
203900140606             else;
204000140606             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
204100140606             endif;
204200140611             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
204300140528             exsr Messaggi;
204400140528             leavesr;
204500140528       endif;
204600140605       // Non deve cadere in giorno festivo
204700140605       chain (0:wfgslna:%subdt(%date(wdcr):*years):
204800140605                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
204900140605       if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
205000140605                            or pom(%subdt(%date(wdcr):*days)) = 'F';
205100140605             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
205200140605             wIdMsg   = 'TIS0758';
205300140605             wIdCampo = 'NEWDTCRICH';
205400140611             wParm=%char(%date(wdcr:*iso):*eur);
205500140605             exsr Messaggi;
205600140605             leavesr;
205700140605       endif;
205800140528
205900140528       endsr;
206000140605       //--------------------------------------------------------------
206100140605       //?Rperimento eventuale personalizzazione giorni attesa prima di apert.giac
206200140605       //--------------------------------------------------------------
206300140605       BEGSR  sr_PersGGiac  ;
206400140606       // Faccio come fa fnlg80r --> prima prova on arbksc
206500140606       // e se non trova con questo prova poi con arbccm
206600140606       clear ds7u;
206700140606       w008a=%editc(arbksc:'X')+'Q';
206800140606       chain (1:'7U':w008a) tabel00f;
206900140606       if %found (tabel00f);
207000140606          ds7u=tbluni;
207100140606       else;
207200140606          w008a=%editc(arbCCM:'X')+'Q';
207300140606          chain (1:'7U':w008a) tabel00f;
207400140606          if %found (tabel00f);
207500140606             ds7u=tbluni;
207600140606          endif;
207700140606       endif;
207800140606       if §7uldcr>'00';
207900140606          wlimitdcr=%dec(§7uldcr:2:0);
208000140606       endif;
208100140605       // Cerco con il ccm se presente altrimenti con KSC
208200140606       // clear ds7u;
208300140606       //if arbccm > 0;
208400140606       //   w008a=%editc(arbCCM:'X')+'Q';
208500140606       //   chain (1:'7U':w008a) tabel00f;
208600140606       //   if %found (tabel00f);
208700140606       //      ds7u=tbluni;
208800140606       //   endif;
208900140606       //else;
209000140606       //   w008a=%editc(arbKSC:'X')+'Q';
209100140606       //   chain (1:'7U':w008a) tabel00f;
209200140606       //   if %found (tabel00f);
209300140606       //      ds7u=tbluni;
209400140606       //   endif;
209500140606       //endif;
209600140606       //if §7uldcr>'00';
209700140606       //   wlimitdcr=%dec(§7uldcr:2:0);
209800140606       //endif;
209900140605       endsr;
210000140609       //--------------------------------------------------------------
210100140609       //?Controlli Disposizioni di consegna ad altro indirizzo
210200140609       //--------------------------------------------------------------
210300140609       BEGSR  sr_ctrind;
210400141006          clear wolna;
210500140610       // Per il controllo dell'indirizzo (CAP escluso) non viene richiamato
210600140610       // fnlv14r (quest'ultimo, se cap vuoto richiama l'interrogazione
210700140609       // del cappario e controlla la nazione che qui non serve)
210800140609       //?Ragione sociale, Indirizzo, Località e Provincia --> obbligatori
210900140610          if fnlry00i1.newrsd = *blanks;
211000140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
211100140610             wIdMsg   = 'TIS0761';
211200140610             wIdCampo = 'NEWRSD    ';
211300140610             clear wParm;
211400140610             exsr Messaggi;
211500141006             leavesr;
211600140610          endif;
211700140610          if fnlry00i1.newind = *blanks;
211800140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
211900140610             wIdMsg   = 'TIS0762';
212000140610             wIdCampo = 'NEWIND    ';
212100140610             clear wParm;
212200140610             exsr Messaggi;
212300141006             leavesr;
212400140610          endif;
212500140610          if fnlry00i1.newcad = *blanks;
212600140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
212700140610             wIdMsg   = 'TIS0763';
212800140610             wIdCampo = 'NEWCAD    ';
212900140610             clear wParm;
213000140610             exsr Messaggi;
213100141006             leavesr;
213200140610          endif;
213300140610          if fnlry00i1.newlod = *blanks;
213400140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
213500140610             wIdMsg   = 'TIS0764';
213600140610             wIdCampo = 'NEWLOD    ';
213700140610             clear wParm;
213800140610             exsr Messaggi;
213900141006             leavesr;
214000140610          endif;
214100140610          if fnlry00i1.newprd = *blanks;
214200140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
214300140610             wIdMsg   = 'TIS0765';
214400140610             wIdCampo = 'NEWPRD    ';
214500140610             clear wParm;
214600140610             exsr Messaggi;
214700141006             leavesr;
214800140609          endif;
214900140610       //?La nazione deve essere vuota
215000140610          if fnlry00i1.newnzd <> *blanks;
215100140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
215200140610             wIdMsg   = 'TIS0384';
215300140610             wIdCampo = 'NEWNZD    ';
215400140610             clear wParm;
215500140610             exsr Messaggi;
215600141006             leavesr;
215700140610          endif;
215800140611       //?Richiamo pgm per controllo CAP e calcolo instradamento
215900140610          if fnlry00i1.newcad<>fnlry00i1.oldcad or
216000140610             fnlry00i1.newlod<>fnlry00i1.oldlod or
216100140610             fnlry00i1.newprd<>fnlry00i1.oldprd or
216200140610             fnlry00i1.newnzd<>fnlry00i1.oldnzd         ;
216300140611             exsr sr_call_fispe02r;
216400141003          else;
216500141003             if fnlry00i1.newrsd=fnlry00i1.oldrsd and
216600141003                fnlry00i1.newind=fnlry00i1.oldind;
216700141003       // Errore se non eseguita alcuna variazione di indirizzo
216800141003                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
216900141003                wIdMsg   = 'TIS0794';
217000141003                wIdCampo = '*NOCHGIND ';
217100141003                clear wParm;
217200141003                exsr Messaggi;
217300141006                leavesr;
217400141003             endif;
217500140610       endif;
217600140930
217700140930         waltroind='1';
217800140930         exsr sr_orarisr ;
217900140930
218000141016       //?SE NO DIROTTAMENTO
218100141016       //if wolna = 0 ;
218200140923       //?DATA/ORA CONSEGNA RICHIESTA
218300140923       if fnlry00i1.newdtcrici<>*blanks and fnlry00i1.newdtcrici<>*zeros;
218400141006       // Se richiesto dirottamento non si può inserire la consegna richiesta
218500141016         if  wolna>0;
218600141016                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
218700141016                wIdMsg   = 'TIS0795';
218800141016                wIdCampo = 'NEWDTCRICI';
218900141016                clear wParm;
219000141016                exsr Messaggi;
219100141016                leavesr;
219200141016         endif;
219300140923       // DATA CONSEGNA RICHIESTA:
219400140923       //  Controllo correttezza formale
219500140923         monitor  ;
219600140923         dataiso=%date(fnlry00i1.newdtcrici:  *eur)   ;
219700140923         on-error ;
219800140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
219900140923             wIdMsg   = 'TIS0121';
220000140923             wIdCampo = 'NEWDTCRICI';
220100140923             clear wParm;
220200140923             exsr Messaggi;
220300140923             leavesr;
220400140923         endmon  ;
220500140923
220600140923         wdcr=%dec(dataiso);
220700141215       //  Per chkistruco considero "data oggi" la data in cui è stata
220800141215       //  fatta la  richiesta
220900141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
221000141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
221100141215         else;
221200141218              wdataconfr=dataoggi;
221300141215         endif;
221400140923
221500140923       //  Non inferiore o uguale a oggi
221600141215         if wdcr <= wdataconfr;
221700140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
221800140929             wIdMsg   = 'TIS0759';
221900140923             wIdCampo = 'NEWDTCRICI';
222000140923             clear wparm;
222100140923             exsr Messaggi;
222200140923             leavesr;
222300140923         endif;
222400161130       //  Non inferiore a data prevista consegna se spedizione da partire o senza
222500161129       //  Tentativi di consegna
222600161214       if w_tbo='A' ;
222700161202          exsr sr_RepPct;
222800161202       endif;
222900161202       if (w_tbo='P' or (arbntc=0 and wtentacons=' ')) and wdcr < dataPrvC;
223000161129             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
223100170113             wIdMsg   = 'TIS0860';
223200161129             wIdCampo = 'NEWDTCRICI';
223300161129             wParm=%char(%date(wdcr:*iso):*eur);
223400161129             exsr Messaggi;
223500161129             leavesr;
223600161129       endif;
223700140923       //  Non superiore ai Giorni limite giac. con data cons.rich.
223800140923       //      Verifico se presenti limiti personalizzati per il cliente
223900140923
224000140923         exsr sr_PersGGiac;
224100161215       //Calcolo giorni lavorativi fra data inizio conteggio e data consegna richiesta
224200161216       exsr sr_date;
224300161215       datada=wdata;
224400140923         dataa=wdcr;
224500140923         clear tfptfa;
224600140923         p_tfa =wfgslna;
224700140923         xsrlav8(wdat8:tfptfa);
224800140923         if ggl > wlimitdcr;
224900140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
225000140923             wIdMsg   = 'TIS0745';
225100140923             wIdCampo = 'NEWDTCRICI';
225200140923             wParm=%char(%date(wdcr:*iso):*eur);
225300140923             exsr Messaggi;
225400140923             leavesr;
225500140923         endif;
225600140923       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
225700140923       //  Controllo anche con la data consegna richiesta originale
225800140923         if (arbtcr='D' and wdcr<=arbdcr) or
225900140923           (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
226000140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
226100140923             wIdMsg   = 'TIS0750';
226200140923             wIdCampo = 'NEWDTCRICI';
226300140923             if arbtcr='D' ;
226400140923             dataeur=%date(%dec(arbdcr:8:0):*iso);
226500140923             else;
226600140923             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
226700140923             endif;
226800140923             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
226900140923             exsr Messaggi;
227000140923             leavesr;
227100140923         endif;
227200140923       // Non deve cadere in giorno festivo
227300140923         chain (0:wfgslna:%subdt(%date(wdcr):*years):
227400140923                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
227500140923         if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
227600140923                            or pom(%subdt(%date(wdcr):*days)) = 'F';
227700140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
227800140923             wIdMsg   = 'TIS0758';
227900140923             wIdCampo = 'NEWDTCRICI';
228000140923             wParm=%char(%date(wdcr:*iso):*eur);
228100140923             exsr Messaggi;
228200140923             leavesr;
228300140923         endif;
228400140923       // ORA CONSEGNA RICHIESTA:
228500140923         if fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
228600140923       // Deve contenere solo numeri
228700140923           if %check(digits:fnlry00i1.neworcrici)>0;
228800140923                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
228900140923                wIdMsg   = 'TIS0746';
229000140923                wIdCampo = 'NEWORCRICH';
229100140923                wParm = %subst(fnlry00i1.neworcrici:1:2) + ':' +
229200140923                        %subst(fnlry00i1.neworcrici:3:2);
229300140923                exsr Messaggi;
229400140923           else;
229500140923       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
229600140923                wora = %dec(fnlry00i1.neworcrici:4:0);
229700140923                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
229800140923                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
229900140923                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
230000140923                       wIdMsg   = 'TIS0780';
230100140923                       wIdCampo = 'NEWORCRICI';
230200140925       //              wParm = fnlry00i1.newlod;
230300140923                       if §ivptipo='S';
230400140925                          wParm = fnlry00i1.newlod +
230500140925                                  %editw(oor2stis:'  :  ') +
230600140925                                  %editw(oor2stfs:'  :  ') ;
230700140923                       else;
230800140925                          wParm = fnlry00i1.newlod +
230900140925                                  %editw(oor2miis:'  :  ') +
231000140925                                 %editw(oor2mxfs:'  :  ') ;
231100140923                       endif;
231200140923                       exsr Messaggi;
231300140923                 endif;
231400140923           endif;
231500140923         endif;
231600140923       // PREFERENZA MATTINO/POMERIGGIO
231700140923         if fnlry00i1.prconsrici<>*blanks and
231800140923            fnlry00i1.prconsrici<>'M' and
231900140923            fnlry00i1.prconsrici<>'P'               ;
232000140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
232100140923                    wIdMsg   = 'TIS0749';
232200140923                    wIdCampo = 'PRCONSRICI';
232300140923                    clear wParm;
232400140923                    exsr Messaggi;
232500140923         endif;
232600140923       // Verifico preferenza m/p con gli orari standard o min/max in base
232700140923       // all'ora limite del mattino
232800140923         if (fnlry00i1.prconsrici='P' and §ivptipo='S' and §ivporlm>=OOR2STFS)or
232900140923            (fnlry00i1.prconsrici='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
233000140923            (fnlry00i1.prconsrici='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS)or
233100140923            (fnlry00i1.prconsrici='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
233200140923            If fnlry00i1.prconsricI='P';
233300140923                widmsg='TIS0676';
233400140923            else;
233500140923                widmsg='TIS0667';
233600140923            endif;
233700140923            matpom=rtvMsgLang(widmsg : widlingua);
233800140923                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
233900141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
234000140923                      wIdMsg   = 'TIS0760';
234100140923                      wIdCampo = 'PRCONSRICI';
234200140923                      wParm = MatPom + wbolla;
234300141006         else;
234400141006                      wIdMsg   = 'TISA760';
234500141006                      wIdCampo = 'PRCONSRICI';
234600141006                      wParm = MatPom ;
234700141006         endif;
234800140923                      exsr Messaggi;
234900140923         ENDIF;
235000140923       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
235100140923         if fnlry00i1.prconsrici<>*blanks and
235200140923            fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
235300140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
235400140923                    wIdMsg   = 'TIS0753';
235500140923                    wIdCampo = 'NEWORCRICI';
235600140923                    clear wParm;
235700140923                    exsr Messaggi;
235800140923                    wIdCampo = 'PRCONSRICI';
235900140923                    exsr Messaggi;
236000140923         endif;
236100141001         else;
236200141001       // se non immessa la data consegna richiesta errore se immessa l'ora o la preferenza M/P
236300141001           if fnlry00i1.prconsrici<>*blanks ;
236400141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
236500141016              if  wolna>0;
236600141016                        wIdMsg   = 'TIS0795';
236700141016              else;
236800141001                        wIdMsg   = 'TIS0782';
236900141016              endif;
237000141001                        wIdCampo = 'PRCONSRICI';
237100141001                        clear wParm;
237200141001                        exsr Messaggi;
237300141001           endif;
237400141001           if  fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros ;
237500141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
237600141016              if  wolna>0;
237700141016                        wIdMsg   = 'TIS0795';
237800141016              else;
237900141001                        wIdMsg   = 'TIS0782';
238000141016              endif;
238100141001                        wIdCampo = 'NEWORCRICI';
238200141001                        clear wParm;
238300141001                        exsr Messaggi;
238400141001           endif;
238500141001         endif;
238600141016       //else;
238700141016       //?SE DIROTTAMENTO pulisco data/ora preferenza
238800141016       //   clear fnlry00i1.newdtcrici ;
238900141016       //   clear fnlry00i1.neworcrici ;
239000141016       //   clear fnlry00i1.prconsrici ;
239100141016       //endif;
239200140609       endsr;
239300140611       //--------------------------------------------------------------
239400140611       //?Richiamo stored procedure per controllo e calcolo instradamento
239500140611       //--------------------------------------------------------------
239600140611       BEGSR sr_Call_FISPE02R;
239700141006
239800141006
239900140611         clear  oTFA;
240000140611         clear  oLNA;
240100140611         clear  oZNC;
240200140611         clear  oLNPD;
240300140611         clear  oLNAD;
240400140611         clear  oERR;
240500140611         clear  oMSG;
240600140611         clear  oTAG;
240700140611         clear  RTNesito;
240800140611         clear  RTNopcode;
240900140611         clear  RTNstatus;
241000140611         iTYPE='C';
241100140611         iLANG=Widlingua;
241200140611         iDATA=dataoggi;
241300140611         clear iNTW ;
241400140611         iLNP=arblnp;
241500140611         iNZD=fnlry00i1.newnzd;
241600140611         iPRD=fnlry00i1.newprd;
241700140611         iCAD=fnlry00i1.newcad;
241800140611         iLOD=fnlry00i1.newlod;
241900140611         iIND=fnlry00i1.newind;
242000140611         iRSD=fnlry00i1.newrsd;
242100140611         iNCL=arbncl;
242200140611         iPKB=arbpkb;
242300140611         iVLB=arbvlb;
242400140611         iFFD=arbffd;
242500140611         iTSP=arbtsp;
242600140611         iTC1=arbtc1;
242700140611         iTC2=arbtc2;
242800140611         iCBO=arbcbo;
242900140611         fispE02r ( iTYPE :
243000140611                    iLANG :
243100140611                    iDATA :
243200140611                    iNTW :
243300140611                    iLNP :
243400140611                    iNZD :
243500140611                    iPRD :
243600140611                    iCAD :
243700140611                    iLOD :
243800140611                    iIND :
243900140611                    iRSD :
244000140611                    iNCL :
244100140611                    iPKB :
244200140611                    iVLB :
244300140611                    iFFD :
244400140611                    iTSP :
244500140611                    iTC1 :
244600140611                    iTC2 :
244700140611                    iCBO :
244800140611                    oTFA :
244900140611                    oLNA :
245000140611                    oZNC :
245100140611                    oLNPD :
245200140611                    oLNAD :
245300140611                    oERR :
245400140611                    oMSG :
245500140611                    oTAG :
245600140611                    RTNesito :
245700140611                    RTNopcode :
245800140611                    RTNstatus );
245900140925       //if RTNesito<0;
246000140925       //ELSe;
246100140611          if oERR<>*blanks;
246200140611             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
246300140611             clear wIdMsg;
246400140611             wTextMsg=oMSG;
246500140611             select;
246600140611             when  %scan('Provincia':otag)>0;
246700140611                wIdCampo = 'NEWPRD    ';
246800140611             when  %scan('Localita':otag)>0;
246900140611                wIdCampo = 'NEWLOD    ';
247000140611             other;
247100140611                wIdCampo = 'NEWCAD    ';
247200140611             endsl;
247300140611             clear wParm;
247400140611             exsr Messaggi;
247500140611       // Se non ci sono errori verifico che la linea di arrivo calcolata sia la
247600141006       // stessa della bolla --> se dirottamento non possono selezionare la data/ora
247700140611          else;
247800140611             if arblna<>olna;
247900141006       //       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
248000141006       //       wIdMsg   = 'TIS0767';
248100141006       //       wIdCampo = '*CHGLNA   ';
248200141006       //       clear wParm;
248300141006       //       exsr Messaggi;
248400141006                Wolna=olna;
248500140611             endif;
248600140611          endif;
248700140925       //endif;
248800140611       endsr;
248900140610       //--------------------------------------------------------------
249000140610       //?Controllo Dati comuni a tutti i tipi di disposizioni
249100140610       //--------------------------------------------------------------
249200140610       BEGSR  sr_ds3A       ;
249300140610                   clear kkey;
249400140610                   clear ds3a;
249500140610                   %subst(kkey:1:2)=arbcbo;
249600140610                   chain (1:'3A':kkey) tabel00f ;
249700140610                   if %found(tabel00f);
249800140610                      ds3a=tbluni;
249900140610                   endif;
250000140610       endsr;
250100140523       //--------------------------------------------------------------
250200140523       //?Controllo Dati comuni a tutti i tipi di disposizioni
250300140523       //--------------------------------------------------------------
250400140523       BEGSR  sr_ctrUNI     ;
250500140609       //?Referente Consegna - obbligatorio
250600140530       if fnlry00i1.newrefcons=  *blanks;
250700140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
250800140530             wIdMsg   = 'TIS0754';
250900140530             wIdCampo = 'NEWREFCONS';
251000140530             clear wParm;
251100140530             exsr Messaggi;
251200140530       endif;
251300140530       //?Telefono Destinatario - obbligatorio
251400140530       if fnlry00i1.newtelcons=  *blanks;
251500140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
251600140530             wIdMsg   = 'TIS0755';
251700140530             wIdCampo = 'NEWTELCONS';
251800140530             clear wParm;
251900140530             exsr Messaggi;
252000140530       endif;
252100140523       //?Indirizzo e-mail
252200140528       if fnlry00i1.newdesmail<> *blanks;
252300140528          clear dsemail                        ;
252400140528          §emlindi = fnlry00i1.newdesmail;
252500140528          xemail (dsemail);
252600140528          if §emlerro <> '0';
252700140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
252800140528             wIdMsg   = 'TIS0732';
252900140528             wIdCampo = 'NEWDESMAIL';
253000140528             clear wParm;
253100140528             exsr Messaggi;
253200140528          endif;
253300140528       endif;
253400140523       //?N.Telefono per SMS
253500140528       if fnlry00i1.newdescell<> *blanks;
253600140528          pInCell = %trim(fnlry00i1.newdescell);
253700140528          pOutErr = *blanks;
253800140528          if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
253900140528           // Numero di cellulare KO
254000140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
254100140528             wIdMsg   = 'TIS0725';
254200140528             wIdCampo = 'NEWDESCELL';
254300140528             clear wParm;
254400140528             exsr Messaggi;
254500140528          endif;
254600140528       endif;
254700140523       endsr;
254800140529       //--------------------------------------------------------------
254900140529       //?Routine per recuperare flag di autorizzazione al Fermo Deposito
255000140529       //--------------------------------------------------------------
255100140529       BEGSR  sr_tab7r;
255200140529            clear kkey;
255300140529            clear ds7r;
255400140529            %subst(kkey:1:2)=arbgma;
255500140529            chain (1:'7R':kkey) tabel00f ;
255600140529            if %found(tabel00f);
255700140529               ds7r=tbluni;
255800140529            endif;
255900140529       endsr;
256000140528      /end-free
256100140521      **************************************************************************
256200140528     c     sr_OrariSr    BEGSR
256300140521
256400140521     c                   clear                   trulorSds
256500161129     c                   clear                   dataPRVC
256600140522      /free
256700140522              // reperisco gli orari di consegna previsti
256800140522              clear tnsd99ds;
256900161130              d98tbo=w_tbo;
257000140522              d98aas=arbaas;
257100140522              d98lnp=arblnp;
257200140522              d98nrs=arbnrs;
257300140522              d98nsp=arbnsp;
257400161130       // Per la distribuzione forzo tipo servizio "C":
257500161130       // - SEMPRE se la bolla è già in arrivo perchè in questo caso il collo
257600161130       //   o è in IMP oppure è partito ma comunque entrato
257700161130       // - Solo se entrato almeno un collo se la bolla è ancora "Da partire"
257800161130              if arbtsp='D' and (w_tbo='A' or arbdse>0 );
257900140522                 I98TSP_FOR='C';
258000140522              endif;
258100140522              tnsd99r(tnsd99ds);
258200140522              if d98err='0';
258300140522                IOREDTA = d98dci;
258400161130               if d98dee>0;
258500170206                if arbdcr=0 or arbtcr<>*blank  ;
258600161129                DataPrvC= d98dEE;
258700170206                else;
258800170206                DataPrvC= d98dEi;
258900170206                endif;
259000161130               else;
259100170208                 if d98dtctd>*zeros  ;
259200170208                   DataPrvC= %dec(d98dtctd:8:0);
259300170208                   else  ;
259400170208                   DataPrvC= dataoggi          ;
259500170208                 endif;
259600170208               endif;
259700140522              endif;
259800140522      /end-free
259900140521     c                   eval       IOREtfp = ARBtfp
260000140521     c                   eval       IOREtfa = ARBtfa
260100150107     c                   if         arbdti>0
260200140521     c                   eval       IOREdti = ARBdti
260300150107     c                   else
260400150107     c* Caso di consegna parziale:
260500150107     c* Visto che vogliamo prendere gli orari "tutto il giorno", se dti vuoto non lo
260600150107     c* prenderebbe mai e qunidi metto ARBDAM
260700150107     c                   eval       IOREdti = ARBdam
260800150107     c                   endif
260900140528     c* imposto a 0 hti per recuperare sempre gli orari ' '=Tutto il giorno
261000140522     c****               eval       IOREhti = ARBhti
261100140521     c                   eval       IOREdcr = ARBdcr
261200140521     c                   eval       IOREhcr = ARBhcr
261300140521     c                   eval       IOREtcr = ARBtcr
261400140522     c                   eval       IOREDEI = d98dei
261500140522     c                   eval       IOREOEI = d98oei
261600150729
261700150729     c                   clear                   diore01
261800150729     c                   eval       §IORETRAZC=%editc(d98ttc:'X')
261900150729     c                   eval       §IORECONSC=%editc(d98tcc:'X')
262000150729     c                   eval       ioreflo=diore01
262100150729
262200140521     c                   eval       IOREfil = ARBlna
262300140923     c                   if         waltroind=' '
262400140521     c                   eval       IOREcap = ARBcad
262500140521     c                   eval       IOREloc = ARBlod
262600140521     c                   eval       IOREnar = ARBnzd
262700140923     c                   else
262800140925     c                   eval       IOREcap = fnlry00i1.newcad
262900140925     c                   eval       IOREloc = fnlry00i1.newlod
263000140925     c                   eval       IOREnar = fnlry00i1.newnzd
263100140923     c                   endif
263200140521     c
263300140521     c                   eval       IOREtser = 'C'
263400140521     c                   eval       IOREDDC=arbddc
263500140521     c                   eval       IOREnDC=arbndc
263600140521     c
263700140521     c
263800140521     c                   eval       IOREtsp = ARBtsp
263900140618     c                   if          ARBfbc = *blanks and
264000140618     c                               ARBddc > 0 and
264100140618     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
264200140618     c                               arbica='R' or arbicc=' ' or arbicc='R')
264300140618     c                   eval       IOREcons = 'S'
264400140618     c                   endif
264500140521     c
264600140521
264700140521     c                   call      'TRULORSR'
264800140521     c                   parm                    kpjba
264900140521     c                   parm                    trulorSds
265000140521     c                   parm                    trulor2ds
265100140521     c
265200140521     c                   ENDSR
265300140521      /free
265400140924       //--------------------------------------------------------------
265500140924       //?Routine per determinare il giorno della settimana di dataoggi + 1gg lav.
265600140924       //--------------------------------------------------------------
265700140924       BEGSR  sr_wdesgio ;
265800140924       //       Determino il giorno della settimana di dataoggi+1giorno lavorativo
265900140924       //       da restituire nel messaggio
266000140924               wdata=dataoggi;
266100140924       //       Incremento la data di un giorno
266200140924               exsr sr_incredata;
266300140924       //       Verifico se data festiva e in tal caso incremento finchè non trovo giorno
266400140924       //       lavorativo
266500140924               exsr sr_datafes;
266600140924       //       Determino il giorno della settimana della data calcolata
266700140924               dataiso=%date(wdata:*iso);
266800140924               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
266900140924               wdesgio=rtvMsgLang(dayOfWeek.msgId(dw) : widlingua);
267000140924       endsr;
267100140603       //--------------------------------------------------------------
267200140603       //?Routine per scrittura disposizioni di consegna
267300140603       //--------------------------------------------------------------
267400140924       BEGSR  SR_Tiidc;
267500140603       // Per la spedizione non devono esserci disposizioni ancora da elaborare
267600140603       exsr sr_ChkDispo;
267700140603       clear tiidc000;
267800141013       clear tiidcds ;
267900140718       clear tivpi000;
268000140604       exsr repnum;
268100140604       if esito = *blanks;
268200140604          IDCPRG=kprg                         ;
268300140604       else;
268400140619       // prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
268500140619  //   // wIdMsg   = 'TIS0757';
268600140619  //   // wIdCampo = '*REPNUM   ';
268700140619  //   // clear wParm;
268800140619  //   // exsr Messaggi;
268900140616          prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
269000140616          prmRpyIdMsg  = FNLRY00_ESITO_NUMERATORE_DISPO_NONREPERITO;
269100140604          exsr RoutEnd;
269200140604       endif;
269300141009       //WDCINSDATA = %dec(fnlry00i1.ksutimest);
269400141009       //IDCINSDATA = %dec(%subst(%editc(wdcinsdata:'X'):1:14):14:0);
269500141009       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
269600141009                                    : 1 : 14 ) );
269700141009       IDCtimestn   = fnlry00i1.ksutimestn;
269800140603       //IDCMODACC                           ;
269900140610       if wksu<>*blanks;
270000140610       IDCMODACC='L'                       ;
270100140610       else;
270200140610       endif;
270300140603       IDCIP = fnlry00I1.cltipaddr;
270400140603       IDCIDCLI = fnlry00i1.ksu;
270500140603       IDCTIPOCLI= Wchi;
270600140603       IDCWAAS   = kaas;
270700140603       IDCWLNP   = klnp;
270800140603       IDCWNRS   = knrs;
270900140603       IDCWNSP   = knsp;
271000140620       IDCAAS    = arbaas;
271100140620       IDCLNP    = arblnp;
271200140620       IDCNRS    = arbnrs;
271300140620       IDCNSP    = arbnsp;
271400140603       IDCFGS    = wfgslna;
271500140603       IDCTIPODIS = fnlry00i1.tipodispo;
271600140604       IDCMAILAVV = fnlry00i1.newdesmail;
271700140604       IDCTELAVV  = fnlry00i1.newdescell;
271800161202       //IDCALERT Metto nell'ultimo byte informazione del momento in cui
271900161202       //         è stata immessa la dispo rispetto allo stato di avanzamento
272000161202       //         della bolla: P=Bolla da Partire,
272100161202       //                      A=Arrivata ma non ancora fatto il primo tentativo di consegna
272200161202       //                  blank=Arrivata e già fatto il primo tentativo di consegna
272300161202       clear  didcal;
272400161202       select;
272500161202       when w_tbo='P';
272600161202          eval §IDCSTA='P'         ;
272700161202       when w_tbo='A' and arbntc=0 and wtentacons=*blanks;
272800161202          eval §IDCSTA='A'         ;
272900161202       endsl;
273000161202       eval idcalert=didcal;
273100161202
273200140604       if fnlry00i1.newdtcrich <> *blanks and fnlry00i1.tipodispo='1';
273300140604           IDCDCR =  %dec(%date(fnlry00i1.newdtcrich:*eur):*iso);
273400140604       endif;
273500140604       if fnlry00i1.neworcrich <> *blanks and fnlry00i1.tipodispo='1';
273600140604           IDCHCR =  %dec(fnlry00i1.neworcrich:4:0);
273700140604       endif;
273800140924       if fnlry00i1.newdtcrici <> *blanks and fnlry00i1.tipodispo='3';
273900140924           IDCDCR =  %dec(%date(fnlry00i1.newdtcrici:*eur):*iso);
274000140924       endif;
274100140924       if fnlry00i1.neworcrici <> *blanks and fnlry00i1.tipodispo='3';
274200140924           IDCHCR =  %dec(fnlry00i1.neworcrici:4:0);
274300140924       endif;
274400140603       //IDCTCR                         ;
274500140924       if fnlry00i1.tipodispo='1';
274600140924          IDCPRCR=  fnlry00i1.prconsric  ;
274700140924       endif;
274800140924       if fnlry00i1.tipodispo='3';
274900140924          IDCPRCR=  fnlry00i1.prconsrici ;
275000140924       endif;
275100140603       IDCREF  = fnlry00i1.newrefcons ;
275200140603       IDCTELD = fnlry00i1.newtelcons ;
275300140603       if fnlry00i1.tipodispo='2';
275400140603          IDCFFD  = 'S';
275500140603       endif;
275600140611       if fnlry00i1.tipodispo='3';
275700141007          wtxt     = fnlry00i1.newrsd;
275800141007          uppercase='1';
275900141007          exsr sr_chk;
276000141007          IDCRSD   = wtxt;
276100141007          wtxt     = fnlry00i1.newind;
276200141007          uppercase='1';
276300141007          exsr sr_chk;
276400141007          IDCIND   = wtxt            ;
276500140611          IDCCAD   = fnlry00i1.newcad;
276600141007          wtxt     = fnlry00i1.newlod;
276700141007          uppercase='1';
276800141007          exsr sr_chk;
276900141007          exsr chkstringa;
277000141007          IDCLOD   = wtxt            ;
277100140611          IDCPRD   = fnlry00i1.newprd;
277200140611          IDCNAZ   = fnlry00i1.newnzd;
277300141006       // bolla da dirottare
277400141006          if wolna>0;
277500141006               IDCNEWLNA=wolna;
277600141006               IDCFLA   ='S';
277700141006          endif;
277800140611       endif;
277900140611       if fnlry00i1.tipodispo='4';
278000140611          IDCNOTE  = fnlry00i1.altro;
278100140611       endif;
278200140603       IDCELAFLG='S';
278300140603
278400140718       VPITIP='TI';
278500140718       VPIISV='TT';
278600140718       // indicazioni ricevute da cliente loggato
278700140718       if wksu<>*blanks;
278800140801          VPISUN=%editc(fnlry00i1.sun:'X');
278900140718          VPIKSU=wksu    ;
279000140718       else;
279100140718       // indicazioni ricevute mediante BRTCODE
279200140718          VPISUN='8IDC00000';
279300140718          VPIKSU='0IDC0000';
279400140718       endif;
279500140718       VPIDTA=tiidcds ;
279600140718       write tivpi000;
279700140814       // se AS888 scrivo anche tiidc
279800140814
279900140814      /end-free
280000140814     c* controllo se sono in AS888
280100141006     c                   eval      comman=chklib(1)
280200141006     c                   eval      lengh=35
280300141006     c                   call      'QCMDEXC'                            99
280400141006     c                   parm                    comman           80
280500141006     c                   parm                    lengh            15 5
280600140814     c
280700141006     c                   if        not *in99
280800140814      /free
280900140814
281000141013         write tiidc000;
281100141006         endif  ;
281200140603
281300140603       endsr;
281400140603       //--------------------------------------------------------------
281500140603       //?Verifica presenza di diposizioni ancora da elaborare
281600140603       //--------------------------------------------------------------
281700140603       BEGSR  SR_ChkDispo;
281800140908       clear w0140;
281900140908       setll (arbaas:arblnp:arbnrs:arbnsp:w0140) tiidc01l;
282000140603       if %equal(tiidc01l);
282100140603              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
282200140603              wIdMsg   = 'TIS0756';
282300140603              wIdCampo = '*TIIDC    ';
282400140611              wParm = wbolla;
282500140603              exsr Messaggi;
282600140603              exsr Routend;
282700140603       endif;
282800140718       // verifico anche che non siano ancora su tivpi
282900140801       eval wkeyspa=%editc(arbaas:'X')+%editc(arblnp:'X')+
283000140801                    %editc(arbnrs:'X')+%editc(arbnsp:'X')  ;
283100140801      /end-free
283200140718     C/EXEC SQL
283300140718     C+ SELECT count(*) INTO :totale FROM tivpi00f where
283400140718     c+ VPITIP='TI' and VPIISV='TT' and
283500141002     C+ substr(vpidta, 67, 16) = :wkeyspa
283600140718     C/END-EXEC
283700140801      /free
283800140718          if totale>0 ;
283900140718              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
284000140718              wIdMsg   = 'TIS0756';
284100140718              wIdCampo = '*TIVPI    ';
284200140718              wParm = wbolla;
284300140718              exsr Messaggi;
284400140718              exsr Routend;
284500140718       endif;
284600140603       endsr;
284700140604        //-----------------------------------------------------------------------*
284800140604        // Reperimento progressivo richiesta da numeratore 650
284900140604        //-----------------------------------------------------------------------*
285000140604        begsr RepNum;
285100140604
285200140604          clear esito;
285300140604          $finenum=*off;
285400140604          clear trul33ds;
285500140604
285600140604          I33TLa = 'L';
285700140604          I33Ope = *ZEROS;
285800140604          I33CNu = 650;
285900140604          I33Num = 1;
286000140604
286100140604          KPJBU = TRUL33DS;
286200140604
286300140604          dow $finenum=*off   ;
286400140604             Trul33R(kpjba);
286500140604
286600140604             TRUL33DS = KPJBU;
286700140604
286800140604             if O33Err <> *zeros;
286900140604               Esito = '2';
287000140604               $finenum=*on;
287100140604             else;
287200140908               w0090=o33nri;
287300140908               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
287400140604               setll kprg tiidc02l;
287500140604        // Se numero già presente nel file TIIDC ne cerco un altro
287600140604               if not %equal;
287700140604                  $finenum=*on;
287800140604               endif;
287900140604             endif;
288000140604          enddo;
288100140604
288200140604          endsr;
288300140604
288400140708       //--------------------------------------------------------------
288500140708       //?Routine per schierare i messaggi nella ds FNLRY00M0
288600140708       //--------------------------------------------------------------
288700140708       BEGSR  sr_RepPct;
288800140708
288900140708       clear wtentacons;
289000140708
289100140708      /end-free
289200161202     c                   if        arbndc>0
289300140708     c                   move      arbndc        pctndc
289400140708     c                   move      arbifp        pctfgs
289500140708     c                   move      'CET'         ttrd
289600140708     c     kcet          setgt     fipct02l
289700140708     c     kcet          readpe    fipct02l
289800140708     c                   if        not %eof(fipct02l)
289900140708     c                   movel     pctdati       fipctcetds
290000140708     c* consegnata
290100140708     c                   if        §pctcmc = ' '
290200140708     c                   eval      prmRpyOpCode=FNLRY00_RPYOPCODE_ERROR
290300141006     c                   if        prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO
290400141006     c                   eval      wIdMsg   = 'TISA738'
290500141006     c                   eval      wIdCampo = '*BOLLA    '
290600141006     c                   clear                   wbolla
290700141006     c                   else
290800140708     c                   eval      wIdMsg   = 'TIS0738'
290900140708     c                   eval      wIdCampo = '*BOLLA    '
291000140708     c                   eval      wParm = wbolla
291100141006     c                   endif
291200140708     c                   exsr      Messaggi
291300140708     c                   exsr      routend
291400140708     c                   else
291500140708       //?deve essere già stato fatto almeno un tentativo di consegna
291600140708     c                   eval      wtentacons='1'
291700140708     c                   endif
291800140708     c                   endif
291900161202     c                   endif
292000140708      /free
292100140708       endsr;
292200140929       //--------------------------------------------------------------
292300140929       //?Routine per tradurre mattino/pomeriggio
292400140929       //--------------------------------------------------------------
292500140929       BEGSR  sr_matpom;
292600140929
292700140929            If wconsric='P';
292800140929                widmsg='TIS0676';
292900140929            else;
293000140929                widmsg='TIS0667';
293100140929            endif;
293200140929            matpom=rtvMsgLang(widmsg : widlingua);
293300140929       endsr;
293400140604
293500140527       //--------------------------------------------------------------
293600140527       //?Routine per schierare i messaggi nella ds FNLRY00M0
293700140527       //--------------------------------------------------------------
293800140527       BEGSR  Messaggi;
293900140527
294000140527       //?Se ho già caricato 99 messaggi esco
294100140527         IF  fnlry00M0.nrmsg = 99;
294200140527           exsr RoutEnd;
294300140527           clear fnlry00M0.nrmsg;
294400140527         ENDIF;
294500140527
294600140527         fnlry00M0.nrmsg += 1;
294700140527         fnlry00M0.skIdMsg(fnlry00M0.nrmsg) = wIdMsg;
294800140527         fnlry00M0.skIdCampo(fnlry00M0.nrmsg) = wIdCampo;
294900140527         fnlry00M0.skErrWarn(fnlry00M0.nrmsg) = FNLRY00_MSG_ERRORE;
295000140527         IF  wParm <> *blanks;
295100140527           fnlry00M0.skTextMsg (fnlry00M0.nrmsg) =
295200140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
295300140527         ELSE;
295400140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
295500140611           if wIdMsg=*blanks;
295600140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=wTextMsg;
295700140611           else;
295800140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=
295900140611             rtvMsgLang(wIdMsg:wIdlingua);
296000140611           endif;
296100140527         ENDIF;
296200140527
296300140527       ENDSR;
296400140527
296500140521       //--------------------------------------------------------------
296600140408       //?Carico tabelle.
296700140408       //--------------------------------------------------------------
296800140408       BEGSR  CaricaTab;
296900140408
297000140408
297100140408       ENDSR;
297200131010
297300131010       //--------------------------------------------------------------
297400131010       //?Operazioni finali.
297500131010       //--------------------------------------------------------------
297600131010       BEGSR  RoutEnd;
297700140408
297800140519       //?Se richiamato per GET_ISTRUCO
297900140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
298000140519           %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o0;
298100140408         ELSE;
298200131010
298300140522       //?Se richiamato per PUT_ISTRUCO
298400140522          %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o1;
298500140521       //%subst(prmRpyForzatu:1:prmRpyForSize) = fnlry0F0;
298600140408
298700140408         ENDIF;
298800140530
298900140530          %subst(prmRpyMessage:1:prmRpyMsgSize) = fnlrY00M0;
299000131010
299100131010         return;
299200131010
299300131010       ENDSR;
299400131009
299500131007      /end-free
299600141007      *---------------------------------------------------------
299700141007      * Controllo i caratteri dei campi alfanumerici
299800141007      *---------------------------------------------------------
299900141007     c     Sr_Chk        BegSr
300000141007
300100141007     c                   clear                   esito
300200141007     c                   Eval      TxtInOut = wtxt
300300141007     c                   Call      'XCHKCHAR'
300400141007     c                   Parm                    TxtInOut
300500141007     c                   Parm                    ElencoChar
300600141007     c                   Parm                    TipoElenco
300700141007     c                   Parm                    CharSost
300800141007     c                   Parm                    UpperCase
300900141007     c                   Parm                    ChkNull
301000141007     c                   Parm                    CharNull
301100141007     c                   Parm                    Esito
301200141007     c                   If        Esito = '1'
301300141007     c                   Move      TxtInOut      wtxt
301400141007     c                   EndIf
301500141007
301600141007     c                   EndSr
301700141007      *---------------------------------------------------------
301800141007      * Controllo caratteri di punteggiatura precedenti e seguenti
301900141007      *---------------------------------------------------------
302000141007     c     chkstringa    BegSr
302100141007
302200141007     c* trovo la prima posizione "buona" da utilizzare
302300141007     c                   eval      posl=%check(charnook:wtxt)
302400141007     c* trovo l'ultima posizione "buona" da utilizzare (escludo il ".")
302500141007     c                   eval      posr=%checkr(charnookf:wtxt)
302600141007     c                   if        posl=0
302700141007     c                   clear                   wtxt
302800141007     c                   else
302900141007     c* determino la lunghezza della stringa "buona"
303000141007     c                   eval      lung=(posr-posl)+1
303100141007     c                   eval      wtxt=%subst(wtxt:posl:lung)
303200141007     c                   endif
303300141007
303400141007     c                   EndSr
303500161129       //--------------------------------------------------------------
303600161129       //?Pulizia campi di blp non presenti su arb
303700161129       //--------------------------------------------------------------
303800161129       BEGSR sr_inzsoloblp;
303900161129          clear arbflp;
304000161129          clear arbdim;
304100161129          clear arbsop;
304200161129          clear arbfst;
304300161129          clear arbctm;
304400161129          clear arbdrt;
304500161129          clear arbnrt;
304600161129          clear arbfpp;
304700161129          clear arbpdr;
304800161129          clear arbdse;
304900161129       ENDSR;
305000161209       //--------------------------------------------------------------
305100161209       //?Cerca ultimo evento valido come tentaivo di consegna
305200161209       //--------------------------------------------------------------
305300161209       BEGSR sr_evb;
305400161209             clear wdatai;
305500161215             setgt  (arbaas:arblnp:arbnrs:arbnsp) fnevb01l ;
305600161215             readpe (arbaas:arblnp:arbnrs:arbnsp) fnevb01l ;
305700161209             dow not %eof(fnevb01l) and wdatai=0;
305800161209                exsr sr_2a ;
305900161209                if §2antc='S'              ;
306000161209                   wdatai=evbdev;
306100161209                endif;
306200161215             readpe (arbaas:arblnp:arbnrs:arbnsp) fnevb01l ;
306300161209             enddo ;
306400161209       endsr;
306500161206      *-------------------------------------------------------------------------
306600161206      *  Decoditifa causale evento
306700161206      *-------------------------------------------------------------------------
306800161206       BEGSR sr_2a;
306900161206         clear ds2a;
307000161206         clear kkey     ;
307100161206         %subst(kkey:1:3)=evbcev;
307200161206         chain (1:'2A':kkey) tabel00f;
307300161206         if %found(tabel00f);
307400161206            ds2a=tbluni;
307500161206         endif;
307600161206       endsr;
307700161215      *-------------------------------------------------------------------------
307800161216      *  Calcola data minima proponibile e data da cui parte il conteggio per la data
307900161216      *  max proponibile
308000161215      *-------------------------------------------------------------------------
308100161216       BEGSR sr_date   ;
308200161216       // DATA MINIMA PROPONIBILE
308300161215       clear  wdatam;
308400161215       if w_tbo='A' and (arbntc>0 or wtentacons='1') ;
308500161215         wdatam=dataoggi;
308600161215       else;
308700161215       // Se bolla ancora da partire non parto da Oggi ma dalla Data Prevista Consegna
308800161215       // Idem se bolla in arrivo ma non ancora fatti tentativi di consegna
308900161215         wdatam=dataPrvC;
309000161215       // Se Data prevista consegna inferiore a oggi
309100161215       // Sarebbe sbagliato proporre una data già decorsa quindi in questo caso parto da oggi
309200161215               if DataPrvC<dataoggi;
309300161215                wdatam  = dataoggi;
309400161215               endif;
309500161215       endif;
309600161216
309700161216       // DATA INIZIO (da cui parte il conteggio per la data max prima che la sped. venga
309800161216       // proposta in apertura giacenza)
309900161216
310000161216       clear wdata ;
310100161216       if arbndc>0;
310200161216          if wtentacons='1';
310300161216             wdata =%dec(%subst(%editc(pctdtorin:'X'):1:8):8:0);
310400161216          else ;
310500161216             wdata =arbddc;
310600161216          endif;
310700161216       else;
310800161216          exsr sr_evb ;
310900161216          wdata=wdataI;
311000161216          if wdata =0;
311100161216             wdata =wdatam;
311200161216          endif;
311300161216       endif;
311400161215       endsr;
311500140814**
311600141006CHKOBJ OBJ(FILTRAPRD) OBJTYPE(*LIB)
