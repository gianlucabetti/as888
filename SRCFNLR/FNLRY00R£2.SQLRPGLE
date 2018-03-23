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
006901140923     d  schdcri                      10a   dim(9)  overlay(dateconti)
007000140521     d Fnlry00o1     e ds                  QUALIFIED INZ(*EXTDFT)
007100131007
007200131007       // -?Messaggi
007300140522     d Fnlry00m0     e ds                  QUALIFIED INZ(*EXTDFT)
007400140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
007500140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
007600140522     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
007700140522     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
007800131007
007900131007       // -?Forzature
008000140522     d Fnlry00f0     e ds                  QUALIFIED INZ(*EXTDFT)
008100140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
008200140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
008300140522     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
008400140520       // -?Legami clienti
008500140520     d TIBS10DS      e ds                  INZ(*EXTDFT)
008600140520     d  skc                          11    dim(500) overlay(d10skc)
008700140521       // -?Controllo BRTCODE
008800140521     d TRUL28DS0     e ds                  INZ(*EXTDFT)
008900140528       // -?Controllo indirizzo mail
009000140528     d dsemail       e ds                  INZ
009100140521       // -?Calcola orari di servizio
009200140521     d TRULORSDS     e ds                  INZ
009300140521     d TRULOR2ds     e ds                  INZ
009301150729     d diore01       e ds                  INZ
009400140604
009500140522     D tnsd99ds      e ds
009600140604     D TRUL33DS      E DS
009601140923     D Tisio9ds      E DS
009700140521
009800140521     d fidn12ds      e ds
009900140603     d tibs02ds      e ds
010000140603     d fnlv55ds      e ds
010100140604     d KPJBA         e ds
010200140521
010300140521     d xcfivads      e ds                  INZ
010400140521     d fipctcetds    e ds
010500140521     d og143         e ds
010600140522     d dar5emd       e ds
010700140523     d dar5gen       e ds
010800140522     d ds7r          e ds
010900140522     d ds3a          e ds
011000140613     d ds2a          e ds
011100140528     d ds2g          e ds
011200140528     d ds7u          e ds
011300140619     d ds7w          e ds
011400140603     d divp          e ds
011401140924     d dvpooraris    e ds
011500140528
011600140528     D WDAT8           DS
011700140528     D  DATADA                 1      8  0
011800140528     D  DATAA                  9     16  0
011900140528     D  GGL                   17     21  0
012000140610     D tfptfa          DS
012100140610     D  p_tfp                  1      3  0
012200140610     D  p_tfa                  4      6  0
012300140606
012400140606     D clnmat          DS
012500140606     D  mat                    1     31    dim(31)
012600140606     D clnpom          DS
012700140606     D  pom                    1     31    dim(31)
012800140617     D dayOfWeek       DS                  QUALIFIED
012900140617     D                                7A   INZ(MSGID_LUNEDI)
013000140617     D                                7A   INZ(MSGID_MARTEDI)
013100140617     D                                7A   INZ(MSGID_MERCOLEDI)
013200140617     D                                7A   INZ(MSGID_GIOVEDI)
013300140617     D                                7A   INZ(MSGID_VENERDI)
013400140617     D                                7A   INZ(MSGID_SABATO)
013500140617     D                                7A   INZ(MSGID_DOMENICA)
013600140617     D  msgId                         7A   DIM(7) OVERLAY(dayOfWeek)
013700140814
013800140814     D chklib          S             35    DIM(1) CTDATA PERRCD(1)
013900140312
014000131009       //--------------------------------------------------------------
014100131009       //?Definizione schiere.
014200131009       //--------------------------------------------------------------
014300140210
014400140210     d skFIdMsg        s              7a   dim(99)
014500140210     d skFIdCampo      s             10a   dim(99)
014600140210     d skFGiaForza     s              1a   dim(99)
014700131007
014800131007       //--------------------------------------------------------------
014900131007       //?Definizione campi.
015000131007       //--------------------------------------------------------------
015100140527     d wIdMsg          s              7a
015200140527     d wIdCampo        s             10a
015300140527     d wcampo          s             10a
015400140519     d wksu            s                   like(fnlry00i0.ksu)
015500140520     d widbolla        s                   like(fnlry00i0.idbolla)
015600140521     d wbolla          s                   like(widbolla)
015700140520     d wbrtcode        s                   like(fnlry00i0.brtcode)
015800140527     d wIdlingua       s                   like(fnlry00i0.liniso2)
015900140527     d wdcreur         s                   like(fnlry00i1.olddtcrich)
016000140529     d wrbhcr          s                   like(fnlry00i1.OLDORCRICH)
016100140528     d wtprich         s              1
016200140530     d wtentacons      s              1
016300140528     d dataoggi        s              8s 0
016301141218     d wdataconfr      s              8s 0
016400140604     d annocur         s              4s 0
016500140528     d wdcr            s              8s 0
016600140605     d wdata           s              8s 0
016700140528     d wora            s              4s 0
016800140527     d wrsd2           s             35
016900140522     d wchi            s              1
017000140521     d kaas            s                   like(arbaas)
017100140521     d klnp            s                   like(arblnp)
017200140521     d knrs            s                   like(arbnrs)
017300140521     d knsp            s                   like(arbnsp)
017400140522     d kkey            s                   like(tblkey)
017500140523     d wrefbolla       s                   like(§ar5ref)
017600140523     d wtelbolla       s                   like(§ar5teld)
017700140528     d wlimitdcr       s                   like(§2gldr)
017800140613     d wlimitFD        s                   like(§2agla)
017900140521     d wcliente        s             11
018000140521     D Wlibfil         S             21
018100140521     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
018200140521     D Wlibfil201      S             21    INZ('FILTRA201/        ')
018300140521     d dataeur         s               d   datfmt(*eur)
018400140528     d dataiso         s               d   datfmt(*iso)
018500140528     d wParm           s            512a   inz
018600140528     d w008a           s              8a   inz
018601140908     d w0140           s             14  0 inz
018700140603     d wfgslna         s                   like(d55tfa)
018800140616     d wfgslnad        s             20a
018900140604     D Esito           s              1a
019000141031     d w0090           s              9  0
019100140604     d kprg            s                   like(idcprg)
019200140604     d WDCINSDATA      s             20  0
019300140605     d I               s              2  0
019400140606     d Ix              s              2  0
019401140923     d Iy              s              2  0
019500140606     d wfes            s              1
019600140606     d matpom          s             10
019700140611     d wTextMsg        s            255
019800140604     d $Finenum        s               n   inz(*off)
019900140617     d dw              s              1s 0
020000140617     d wdesgio         s             20a
020200140718     d totale          s              2  0
020300140801     d wkeyspa         s             16
020301140924     D wtempoh         S              2  0 inz
020302140924     D wtempom         S              2  0 inz
020303140925     D waltroind       S              1a   inz
020304140930     D wconsric        S                   like(fnlry00i1.prconsric)
020305140930     D w_dalle         S               t
020306140930     D w_alle          S               t
020307140930     D w_dalleoser     S               t
020308140930     D w_alleoser      S               t
020309140930     D w_dallemsg      S              5a
020310140930     D w_allemsg       S              5a
020311140930     D w_orcric        S                   like(fnlry00i1.neworcrich)
020312141006     D wolna           S                   like(olna)
020313141007     d wtxt            S           2048
020314141007     d TxtInOut        S           2048
020315141007     d ElencoChar      S            256
020316141007     d TipoElenco      S              1
020317141007     d CharSost        S              1
020318141007     d UpperCase       S              1
020319141007     d ChkNull         S              1    Inz('1')
020320141007     d CharNull        S              1
020321141007     d posl            s              2  0
020322141007     d posr            s              2  0
020323141007     d lung            s              2  0
020324141007     d* Lunghezza di CHARNOOK deve essere:  lunghezza di tbeuni + 1
020325141007     d* Il +1 è necessario per assicurarmi che nella stringa sia sempre
020326141007     d* presente un blank
020327141007     d charnook        s            257
020328141007     d charnookf       s                   like(charnook)
020400140611       //?- Parametri per FISPE02R: EasySpedWeb: S.P.  TISIT5R+TISI95R?
020500140611     d iTYPE           s              1    inz
020600140611     d iLANG           s              3    inz
020700140611     d iDATA           s              8s 0 inz
020800140611     d iNTW            s              1    inz
020900140611     d iLNP            s              3s 0 inz
021000140611     d iNZD            s              3    inz
021100140611     d iPRD            s              2    inz
021200140611     d iCAD            s              9    inz
021300140611     d iLOD            s             35    inz
021400140611     d iIND            s             35    inz
021500140611     d iRSD            s             35    inz
021600140611     d iNCL            s              5s 0 inz
021700140611     d iPKB            s              7s 1 inz
021800140611     d iVLB            s              5s 3 inz
021900140611     d iFFD            s              1    inz
022000140611     d iTSP            s              1    inz
022100140611     d iTC1            s              1    inz
022200140611     d iTC2            s              1    inz
022300140611     d iCBO            s              2    inz
022400140611      *
022500140611     d oTFA            s              3s 0 inz
022600140611     d oLNA            s              3s 0 inz
022700140611     d oZNC            s              2s 0 inz
022800140611     d oLNPD           s             20    inz
022900140611     d oLNAD           s             20    inz
023000140611     d oERR            s              1    inz
023100140611     d oMSG            s            256    inz
023200140611     d oTAG            s             30    inz
023300140611      *
023400140611     d RTNesito        s             10I 0 inz
023500140611     d RTNopcode       s             10    inz
023600140611     d RTNstatus       s             10I 0 inz
023700131008
023800131008       //--------------------------------------------------------------
023900131008       //?Definizione procedure.
024000131008       //--------------------------------------------------------------
024100140603     d tibs02r         pr                  extpgm('TIBS02R')
024200140604     d  kpjba                              likeds(kpjba)
024300140603     d  tibs02ds                           likeds(tibs02ds)
024400140604     D trul33R         pr                  extpgm('TRUL33R')
024500140604     D  kpjba                              likeds(kpjba)
024501140923     D tisio9r         pr                  extpgm('TISIO9R')
024502140923     D  kpjba                              likeds(kpjba)
024503140923     D  tisio9ds                           likeds(tisio9ds)
024600140611       //?- Stored Procedure per EasySpedWeb?
024700140611     d fispE02r        pr                  extpgm('FISPE02R')
024800140611     d   i_TYPE                            like(iTYPE)
024900140611     d   i_LANG                            like(iLANG)
025000140611     d   i_DATA                            like(iDATA)
025100140611     d   i_NTW                             like(iNTW)
025200140611     d   i_LNP                             like(iLNP)
025300140611     d   i_NZD                             like(iNZD)
025400140611     d   i_PRD                             like(iPRD)
025500140611     d   i_CAD                             like(iCAD)
025600140611     d   i_LOD                             like(iLOD)
025700140611     d   i_IND                             like(iIND)
025800140611     d   i_RSD                             like(iRSD)
025900140611     d   i_NCL                             like(iNCL)
026000140611     d   i_PKB                             like(iPKB)
026100140611     d   i_VLB                             like(iVLB)
026200140611     d   i_FFD                             like(iFFD)
026300140611     d   i_TSP                             like(iTSP)
026400140611     d   i_TC1                             like(iTC1)
026500140611     d   i_TC2                             like(iTC2)
026600140611     d   i_CBO                             like(iCBO)
026700140611     d   o_TFA                             like(oTFA)
026800140611     d   o_LNA                             like(oLNA)
026900140611     d   o_ZNC                             like(oZNC)
027000140611     d   o_LNPD                            like(oLNPD)
027100140611     d   o_LNAD                            like(oLNAD)
027200140611     d   o_ERR                             like(oERR)
027300140611     d   o_MSG                             like(oMSG)
027400140611     d   o_TAG                             like(oTAG)
027500140611     d   RTN_esito                         like(RTNesito)
027600140611     d   RTN_opcode                        like(RTNopcode)
027700140611     d   RTN_status                        like(RTNstatus)
027800131008
027900131008       //--------------------------------------------------------------
028000131008       //?Definizione prototipi.
028100131008       //--------------------------------------------------------------
028200140519      /copy gaitrasrc/srcprotopr,Fnlry00r
028300140520      /copy gaitrasrc/srcprotopr,TIBS10R
028400140521      /copy gaitrasrc/srcprotopr,TRUL28R0
028500140521      /copy gaitrasrc/srcprotopr,FIDN12R
028600140522      /copy gaitrasrc/srcprotopr,tnsd99r
028700131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
028800131230      /copy gaitrasrc/srcprotopr,TIBS0800R
028900140311      /copy gaitrasrc/srcprotopr,UBCHKCEL
029000140311      /copy gaitrasrc/srcprotopi,UBCHKCEL
029100131008      /copy gaitrasrc/srcprotopr,XCFIVAR
029200140311      /copy gaitrasrc/srcprotopr,XEMAIL
029300140528      /copy gaitrasrc/srcprotopr,xsrlav8
029400140603      /copy gaitrasrc/srcprotopr,fnlv55r
029500131219
029600131219       //--------------------------------------------------------------
029700131219       //?Definizione parametri programma.
029800131219       //--------------------------------------------------------------
029900140519     D Fnlry00_Immetti...
030000140519     D                 PI
030100140519     D prmRqsOpCode...
030200140519     D                               10I 0 CONST
030300140519     D prmRpyOpCode...
030400140519     D                               10I 0
030500140519     D prmRpyIdMsg...
030600140519     D                               10I 0
030700140519     D prmRqsFormato...
030800140519     D                               10A   CONST
030900140519     D prmRqsData...
031000140519     D                            32767A   OPTIONS(*VARSIZE)
031100140519     D prmRqsDataSize...
031200140519     D                               10I 0 CONST
031300140519     D prmRpyFormato...
031400140520     D                               10A   CONST
031500140519     D prmRpyData...
031600140519     D                            32767A   OPTIONS(*VARSIZE)
031700140519     D prmRpyDataSize...
031800140519     D                               10I 0 CONST
031900140519     D prmRpyFormMsg...
032000140519     D                               10A   CONST OPTIONS(*NOPASS)
032100140519     D prmRpyMessage...
032200140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
032300140519     D prmRpyMsgSize...
032400140519     D                               10I 0 CONST OPTIONS(*NOPASS)
032500140519     D prmRpyFormForz...
032600140519     D                               10A   CONST OPTIONS(*NOPASS)
032700140519     D prmRpyForzatu...
032800140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
032900140519     D prmRpyForSize...
033000140519     D                               10I 0 CONST OPTIONS(*NOPASS)
033100131007
033200131007       //--------------------------------------------------------------
033300131007       //?MAIN.
033400131007       //--------------------------------------------------------------
033500131008
033600131007      /free
033700131008
033800131008       //?Operazioni iniziali
033900131008       exsr RoutInz;
034000131008
034100140523       //?Controllo formale dei dati di accesso
034200131008       exsr Controlla;
034300140523       //?Controllo stato bolla: deve essere in uno stato che renda possibile
034400140523       //?l'immissione delle disposizioni di consegna
034500140523       exsr StatoBolla;
034600140529
034700140523       //?Controllo che l'immagine della bolla da Web sia la stessa della bolla su
034800140523       //?AS/400
034900140529       //IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
035000140529       //   exsr ImmagineB;
035100140529       //endif;
035200140529
035300140529       //?Chaino estensioni bolla
035400140529       exsr sr_chainEstBolla;
035500140523       //?Controllo dettaglio dei campi ricevuti
035600140521       exsr ControllaDett;
035700140603       //?Per PUTISTRUCO in modalità Controllo + Inserimento scrivo file TIIDC
035800140611       IF  prmRqsOpCode=FNLRY00_RQSOPCODE_PUT_ISTRUCO and fnlry00i1.tipoela='2'
035900140611                                                      and fnlry00m0.nrmsg=0;
036000140604          exsr sr_Tiidc;
036100140613       // Restituzione messaggio di conferma
036200140613          exsr sr_MsgConf;
036300140603       endif;
036400131010
036500131010       //?Operazioni finali
036600131010       exsr RoutEnd;
036700131008
036800140613       //--------------------------------------------------------------
036900140613       //?Messaggio di avvenuta registrazione delle Indicazioni di Cons.
037000140613       //--------------------------------------------------------------
037100140613       BEGSR  sr_MsgConf;
037200140613
037201140929       // MESSAGGIO GENERICO
037300140617       wIdMsg   = 'TIS0771';
037400140617       fnlry00o1.MSGCONFERM=rtvMsgLang(wIdMsg:widlingua);
037401141007       fnlry00o1.MSGCONFERM=%trim(fnlry00o1.MSGCONFERM)+' del '
037402141007                                                  + %char(%date():*eur)
037403141007                                                  + ' '+  %char(%time()) ;
037500140613
037501140929       // MESSAGGIO SPECIFICO per Fermo Deposito e Consegna richiesta:
037600140929       // FERMO DEPOSITO
037601140929       select;
037700140929       when fnlry00i1.tipodispo='2';
037701140930          exsr sr_wdesgio;
037702140924          wIdMsg   = 'TIS0772';
037703140924          wparm=wdesgio + %editc(WlimitFD:'X') ;
038100140929          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
038200140617          fnlry00o1.fdidfil=wfgslna;
038201140929       // APPUNTAMENTO
038202140929       when fnlry00i1.tipodispo='1';
038203140930          wIdMsg   = 'TIS0787';
038204140930          wparm=fnlry00i1.newdtcrich ;
038205140929       // presente orario
038206140929          if fnlry00i1.neworcrich>*zeros;
038207140930             wIdMsg   = 'TIS0785';
038208140930             w_orcric=fnlry00i1.neworcrich;
038209140930             exsr sr_dallealle;
038241140930            wparm=fnlry00i1.newdtcrich + w_dALLEmsg + W_ALLEMSG;
038242140929          endif;
038243140929       // Presente preferenza matt/pomeriggio
038244140929          if fnlry00i1.prconsric<>*blanks;
038245140929             wconsric=fnlry00i1.prconsric;
038246140929             exsr sr_matpom;
038247140930             wIdMsg   = 'TIS0789';
038248140929             wparm=fnlry00i1.newdtcrich + matpom ;
038249140929          endif;
038250140930          fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
038251140929       // ALTRO INDIRIZZO
038252140930       when fnlry00i1.tipodispo='3';
038258140930       // presente orario --> determino il "Dalle - Alle"
038259140930          if fnlry00i1.neworcrici>*zeros;
038261140930             w_orcric=fnlry00i1.neworcrici;
038262140930             exsr sr_dallealle;
038264140930          endif;
038265140930       // Presente preferenza matt/pomeriggio --> Traduco il flag nella descrizione
038266140930          if fnlry00i1.prconsrici<>*blanks;
038267140930             wconsric=fnlry00i1.prconsrici;
038268140930             exsr sr_matpom;
038271140930          endif;
038272141001       // Messaggi in presenza della data
038273140930          if fnlry00i1.newdtcrici>*zeros;
038275140930             wIdMsg   = 'TIS0788';
038276140930             wparm=fnlry00i1.newdtcrici ;
038277140930             if fnlry00i1.neworcrici>*zeros;
038278140930                wIdMsg   = 'TIS0786';
038281140930                wparm=fnlry00i1.newdtcrici + w_dALLEmsg + W_ALLEMSG;
038282140930             endif;
038283140930       // Presente preferenza matt/pomeriggio
038284140930             if fnlry00i1.prconsrici<>*blanks;
038287140930                wIdMsg   = 'TIS0790';
038288140930                wparm=fnlry00i1.newdtcrici + matpom ;
038289140930             endif;
038290141001             fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
038291140930          else;
038292141001       // Messaggi in assenza della data (caso che non si verifica perchè se presente
038293141001       //   ora o preferenza devo avere anche la data)
038294140930             if fnlry00i1.neworcrici>*zeros;
038295140930                wIdMsg   = 'TIS0791';
038296140930                wparm=w_dALLEmsg + W_ALLEMSG;
038297141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
038298140930             endif;
038299140930       // Presente preferenza matt/pomeriggio
038300140930             if fnlry00i1.prconsrici<>*blanks;
038301140930                wIdMsg   = 'TIS0792';
038302140930                wparm=matpom ;
038303141001                fnlry00o1.MSGCONFERS=rtvMsgLang(wIdMsg:widlingua:wparm);
038304140930             endif;
038305140930          endif;
038306140929
038307140929       endsl;
038400140613
038500140613       endsr;
038501140930       //--------------------------------------------------------------
038502140930       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
038503140930       //--------------------------------------------------------------
038504140930       BEGSR  sr_dallealle;
038505140930
038506140930       //   determino il Dalle/alle da mettere nel messaggio
038507140930            w_dalle=%time((%dec(w_orcric:4:0)*100):*hms)-%minutes(§VPORST_MW);
038508140930            w_alle=%time((%dec(w_orcric:4:0)*100):*hms)+%minutes(§VPORST_PW);
038509140930       //   determino orari servizio comprensivi della tolleranza
038510140930            if §ivptipo='S';
038511140930               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
038512140930               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
038513140930            else;
038514140930               w_dalleoser=%time((oor2miis *100):*hms)-%minutes(§VPORST_MT);
038515140930               w_alleoser=%time((oor2mxfs *100):*hms)+%minutes(§VPORST_MT);
038516140930            endif;
038517140930       //   Confronto dalle/alle calcolato con dalle/alle orari ser
038518140930       //      Dalle < del "dalle" orario ser --> prendo quest'ultimo e l'alle
038519140930       //      lo ricalcolo aggiungendo i minuti da tabella vpo
038520140930            if w_dalle<w_dalleoser;
038521140930               w_dalle=w_dalleoser;
038522140930               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
038523140930            endif;
038524140930       //      Alle  > del "alle" orario ser --> prendo quest'ultimo e il dalle
038525140930       //      lo ricalcolo sottraendo i minuti da tabella vpo purchè non diventi
038526140930       //      minore del "dalle" orario ser
038527140930            if w_alle>w_alleoser;
038528140930               w_alle=w_alleoser;
038529140930               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
038530140930                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
038531140930               endif;
038532140930            endif;
038533140930            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
038534140930            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
038535140930
038536140930       endsr;
038600131008       //--------------------------------------------------------------
038700131008       //?Operazioni iniziali.                                         ?
038800131008       //--------------------------------------------------------------
038900131008       BEGSR  RoutInz;
039000140211
039100140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
039200131008
039300140519         prmRpyOpCode = FNLRY00_RPYOPCODE_DONE;
039400140519         prmRpyIdMsg  = FNLRY00_ESITO_OK;
039500131009
039600131009       //?Data del giorno
039700131009         dataoggi = %dec(%date());
039800140604         annocur  = %subdt(%date():*years);
039900131009
040000131009       //?Pulisco ds di output
040100140519         reset Fnlry00o0;
040200140519         reset fnlry00o1;
040300140522         reset fnlry00m0;
040400140521
040500140521       //?Apertura file
040600140521         wlibfil=wlibfil201;
040700140521         %subst(wlibfil:11:8)='FNARB01L';
040800140521         open(e) fnarb01l;
040900140521         if not %open(fnarb01l);
041000140521            wlibfil=wlibfilprd;
041100140521            %subst(wlibfil:11:8)='FNARB01L';
041200140521            open fnarb01l;
041300140521         endif;
041400140521         wlibfil=wlibfil201;
041500140521         %subst(wlibfil:11:8)='FIPCT02L';
041600140521         open(e) fipct02l;
041700140521         if not %open(fipct02l);
041800140521            wlibfil=wlibfilprd;
041900140521            %subst(wlibfil:11:8)='FIPCT02L';
042000140521            open fipct02l;
042100140521         endif;
042200140522         wlibfil=wlibfil201;
042300140522         %subst(wlibfil:11:8)='FIAR501L';
042400140522         open(e) fiar501l;
042500140522         if not %open(fiar501l);
042600140522            wlibfil=wlibfilprd;
042700140522            %subst(wlibfil:11:8)='FIAR501L';
042800140522            open fiar501l;
042900140522         endif;
043000140528         wlibfil=wlibfil201;
043100140528         %subst(wlibfil:11:8)='FIAR401L';
043200140528         open(e) fiar401l;
043300140528         if not %open(fiar401l);
043400140528            wlibfil=wlibfilprd;
043500140528            %subst(wlibfil:11:8)='FIAR401L';
043600140528            open fiar401l;
043700140528         endif;
043800140807         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
043900140807            open tivpi00f ;
044000140807         endif ;
044100140528
044200140528       // Reperimento G.LIMITE X GIAC.CON DATA RIC
044300140528       clear wlimitdcr;
044400140528       chain (1:'2G':'1       ') tabel00f;
044500140528       if %found(tabel00f);
044600140528          ds2g=tbluni;
044700140528          wlimitdcr=§2gldr;
044800140528       endif;
044900140603       // Reperimento dati tabella IVP - Variabili programma per INTERNET
045000140603       clear tibs02ds;
045100140603       clear divp;
045200140603       t02mod='C';
045300140603       t02cod='IVP';
045400140603       t02ke1='1  ';
045500140603       t02tla='L  ';
045600140603       tibs02r(kpjba:tibs02ds);
045700140603       if t02err=*blanks;
045800140603          divp=t02uni;
045900140603       endif;
046000140613       // Reperimento Giorni lavorativi entro cui ritirare le sped. in FD
046100140613       clear wlimitFD ;
046200140613       chain (1:'2A':'F       ') tabel00f;
046300140613       if %found(tabel00f);
046400140613          ds2a=tbluni;
046500140613          wlimitFD =§2agla;
046600140613       endif;
046601140924       // Reperimento dati tabella VPO - ORARISER
046602140924       clear tibs02ds;
046603140925       clear dvpooraris;
046604140924       t02mod='C';
046605140924       t02cod='VPO';
046606140925       t02ke1='ORARISER';
046607140924       t02tla='L  ';
046609140924       tibs02r(kpjba:tibs02ds);
046610140924       if t02err=*blanks;
046611140924          dvpooraris=t02uni;
046612140924       endif;
046613141007       // Reperimento dati tabella CSP - Caratteri speciali per eliminazione
046614141007      *                           caratteri di punteggiatura da loc.dest.
046615141007       clear tibs02ds;
046616141007       clear charnook  ;
046617141007       t02mod='C';
046618141007       t02cod='CSP';
046619141007       t02ke1='LOC';
046620141007       t02tla='L  ';
046621141007       tibs02r(kpjba:tibs02ds);
046622141007       if t02err=*blanks;
046623141007          charnook=t02uni;
046624141007       endif;
046625141007     c* imposto stringa contenente caratteri speciali per il controllo
046626141007     c* della parte finale (uguale alla charnook maa senza l'eventuale '.')
046627141007     c                   eval      charnookf=%xlate('.':' ':charnook)
046700140521      /end-free
046800140521     C     Kcet          KLIST
046900140521     C                   KFLD                    pctfgs
047000140521     C                   KFLD                    pctNDC
047100140521     C                   KFLD                    arbpdc
047200140521     C                   KFLD                    TTRD              3
047300140521     C                   KFLD                    arbAAS
047400140521     C                   KFLD                    arbLNP
047500140521     C                   KFLD                    arbNRS
047600140521     C                   KFLD                    arbNSP
047700140521      /free
047800131008
047900131008         *inLR = *on;
048000131008
048100131008       ENDSR;
048200131008
048300131008       //--------------------------------------------------------------
048400131008       //?Controllo formale dei dati passati.
048500131008       //--------------------------------------------------------------
048600131008       BEGSR  Controlla;
048700131008
048800140519         clear wksu;
048900140520         clear widbolla;
049000140520         clear wbrtcode;
049100140522         clear wchi    ;
049200140527         clear widlingua;
049300140527         clear wcampo;
049400140519
049500131008       //?OpCode
049600140519         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_GET_ISTRUCO  and
049700140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_PUT_ISTRUCO  and
049800140624             prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
049900140519           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
050000140519           prmRpyIdMsg  = FNLRY00_ESITO_RQSOPCODE_SCONOSCIUTO;
050100140210           exsr RoutEnd;
050200131008         ENDIF;
050300140408
050400140519       //?per GET_ISTRUCO
050500140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
050600140408       //?Formato e size RQS
050700140519           IF  prmRqsFormato = fnlry00I0.formato;
050800140519             fnlry00I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
050900140519             wksu=fnlry00i0.ksu;
051000140520             widbolla=fnlry00i0.idbolla;
051100140520             wbrtcode=fnlry00i0.brtcode;
051200140527             widlingua=fnlry00i0.liniso2;
051300140408           ELSE;
051400140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
051500140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
051600140408             exsr RoutEnd;
051700140408           ENDIF;
051800140408           IF  prmRqsDataSize > 0;
051900140408           ELSE;
052000140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
052100140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
052200140408             exsr RoutEnd;
052300140408           ENDIF;
052400140408         //?Formato e size RPY
052500140519           IF  prmRpyFormato = fnlry00O0.formato;
052600140408           ELSE;
052700140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
052800140519             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
052900140408             exsr RoutEnd;
053000140408           ENDIF;
053100140408           IF  prmRpyDataSize > 0;
053200140408           ELSE;
053300140519             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
053400140519             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
053500140408             exsr RoutEnd;
053600140408           ENDIF;
053700140408         ENDIF;
053800140210
053900140522       //?per PUT_ISTRUCO
054000140522         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
054100140522       //?Formato e size RQS
054200140522           IF  prmRqsFormato = fnlry00I1.formato;
054300140522             fnlry00I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
054400140522             wksu=fnlry00i1.ksu;
054500140522             widbolla=fnlry00i1.idbolla;
054600140522             wbrtcode=fnlry00i1.brtcode;
054700140527             widlingua=fnlry00i1.liniso2;
054800140522           ELSE;
054900140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
055000140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
055100140522             exsr RoutEnd;
055200140522           ENDIF;
055300140522           IF  prmRqsDataSize > 0;
055400140522           ELSE;
055500140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
055600140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
055700140522             exsr RoutEnd;
055800140522           ENDIF;
055900140522         //?Formato e size RPY
056000140522           IF  prmRpyFormato = fnlry00O1.formato;
056100140522           ELSE;
056200140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
056300140522             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
056400140522             exsr RoutEnd;
056500140522           ENDIF;
056600140522           IF  prmRpyDataSize > 0;
056700140522           ELSE;
056800140522             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
056900140522             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
057000140522             exsr RoutEnd;
057100140522           ENDIF;
057200140522       //?Formato e size MSG
057300140522         IF  prmRpyFormMsg = FNLRY00M0.formato;
057400140522         ELSE;
057500140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
057600140522           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
057700140522           exsr RoutEnd;
057800140522         ENDIF;
057900140522         IF  prmRpyMsgSize > 0;
058000140522         ELSE;
058100140522           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
058200140522           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
058300140522           exsr RoutEnd;
058400140522         ENDIF;
058500140523       //?Tipo Elaborazione
058600140523         if fnlry00i1.tipoela<> '1' and fnlry00i1.tipoela<> '2';
058700140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
058800140523           prmRpyIdMsg = FNLRY00_ESITO_TIPOELA_ERRATO;
058900140523           exsr RoutEnd;
059000140523         endif;
059100140523       //?Tipo Disposizione
059200140523         if fnlry00i1.tipodispo <> '1' and
059300140523            fnlry00i1.tipodispo <> '2' and
059400140523            fnlry00i1.tipodispo <> '3' and
059500140523            fnlry00i1.tipodispo <> '4'     ;
059600140523           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
059700140523           prmRpyIdMsg = FNLRY00_ESITO_TIPODISPO_ERRATO;
059800140523           exsr RoutEnd;
059900140523         endif;
060000140522         ENDIF;
060100140624
060200140624       //?per CHK_ISTRUCO
060300140624         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
060400140624       //?Formato e size RQS
060500140624           IF  prmRqsFormato = FNLRY00_RQSFORMATO_CHK_ISTRUCO;
060600140624             TIIDCDS = %SUBST(prmRqsData:1:prmRqsDataSize);
060700140624             wksu=idcidcli     ;
060800141223             widbolla=%subst(%editc(idcwaas:'X'):3:2) + %editc(idcwlnp:'X') +
060900141223                             %editc(idcwnrs:'X') + %editc(idcwnsp:'X');
061000140627             if wksu=*blanks;
061100140627             wbrtcode=widbolla;
061200140627             exsr sr_trul28;
061300140627             wbrtcode=o280cod ;
061400140627             else;
061500140624             clear wbrtcode;
061600140627             endif;
061700140624             widlingua='it';
061800140624           ELSE;
061900140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
062000140624             prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
062100140624             exsr RoutEnd;
062200140624           ENDIF;
062300140624           IF  prmRqsDataSize > 0;
062400140624           ELSE;
062500140624             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
062600140624             prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
062700140624             exsr RoutEnd;
062800140624           ENDIF;
062900140624       // per comodità valorizzo campi FNLRY00I1 prendendoli dal record del TIIDC
063000140624           fnlry00i1.TIPODISPO = IDCTIPODIS;
063100140624           fnlry00i1.NEWREFCONS= IDCREF;
063200140624           fnlry00i1.NEWTELCONS= IDCTELD;
063300140624           fnlry00i1.NEWDESMAIL= IDCMAILAVV;
063400140624           fnlry00i1.NEWDESCELL= IDCTELAVV ;
063401140923       // Se no dispo di indirizzo alternativo
063402140923           if fnlry00i1.tipodispo<>'3';
063500140923              if idcdcr>0;
063600140923                 fnlry00i1.NEWDTCRICH= %char(%date(IDCDCR:*iso):*eur);
063700140923              endif;
063800140923              fnlry00i1.NEWORCRICH= %editc(IDCHCR:'X') ;
063900140923              fnlry00i1.NEWTPCRICH= IDCTCR    ;
064000140923              fnlry00i1.PRCONSRIC = IDCPRCR   ;
064001140923           endif;
064100140624           fnlry00i1.NEWRSD    = IDCRSD    ;
064200140624           fnlry00i1.NEWIND    = IDCIND    ;
064300140624           fnlry00i1.NEWCAD    = IDCCAD    ;
064400140624           fnlry00i1.NEWLOD    = IDCLOD    ;
064500140624           fnlry00i1.NEWPRD    = IDCPRD    ;
064600140624           fnlry00i1.NEWNZD    = IDCNAZ    ;
064700140624           fnlry00i1.ALTRO     = IDCNOTE   ;
064701140923       // Se indirizzo alternativo
064702140923           if fnlry00i1.tipodispo='3';
064703140923              if idcdcr>0;
064704140923                 fnlry00i1.NEWDTCRICI=%char(%date(IDCDCR:*iso):*eur);
064705140923              endif;
064706140923                 fnlry00i1.NEWORCRICi= %editc(IDCHCR:'X') ;
064707140923                 fnlry00i1.NEWTPCRICi= IDCTCR    ;
064708140923                 fnlry00i1.PRCONSRICi= IDCPRCR   ;
064709140923           endif;
064800140624
064900140624         //?Formato e size RPY
065000140624       //?Formato e size MSG
065100140624         IF  prmRpyFormMsg = FNLRY00M0.formato;
065200140624         ELSE;
065300140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
065400140624           prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
065500140624           exsr RoutEnd;
065600140624         ENDIF;
065700140624         IF  prmRpyMsgSize > 0;
065800140624         ELSE;
065900140624           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
066000140624           prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
066100140624           exsr RoutEnd;
066200140624         ENDIF;
066300140624         ENDIF;
066400140522
066500140210
066600140210       //?Formato e size Forzature
066700140522       //IF  prmRpyFormForz = fNLRY00F0.formato;
066800140522       //ELSE;
066900140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
067000140522       //  prmRpyIdMsg = FNLRY00_ESITO_NOME_FORMATO_SCONOSCIUTO;
067100140522       //  exsr RoutEnd;
067200140522       //ENDIF;
067300140522       //IF  prmRpyForSize > 0;
067400140522       //ELSE;
067500140522       //  prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
067600140522       //  prmRpyIdMsg = FNLRY00_ESITO_SIZE_DATA_ERRATO;
067700140522       //  exsr RoutEnd;
067800140522       //ENDIF;
067900140519
068000140520       //?devo ricevere ID cliente unificante o BRTCODE
068100140520         if wksu=*blanks and wbrtcode=*blanks;
068200140520           prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
068300140520           prmRpyIdMsg = FNLRY00_ESITO_ID_RICHIAMO_ERRATO;
068400140520           exsr RoutEnd;
068500140520         endif;
068600140520       //?Accesso per cliente unificante e ID Bolla
068700140519         if wksu <>*blanks;
068800140520       // ID Bolla mancante
068900140520           if widbolla=*blanks;
069000140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
069100140527              wIdMsg   = 'TIS0734';
069200140527              wIdCampo = 'IDBOLLA   ';
069300140527              clear wParm;
069400140527              exsr Messaggi;
069500140520              exsr RoutEnd;
069600140520           endif;
069601141003       // KSU può contenere solo numeri
069602141003           if %check(digits:wksu)>0;
069603141003                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
069604141003                      wIdMsg   = 'TIS0736';
069605141003                      wIdCampo = 'KSU       ';
069606141003                      wParm = widbolla;
069607141003                      exsr Messaggi;
069608141003                      exsr RoutEnd;
069609141003           endif;
069700140520       // ID cliente unificante
069800140520           clear tibs10ds ;
069900140520           d10tle='WW'  ;
070000140520           d10paf='F'    ;     // verifico se è padre
070100140520           d10cod=%dec(wksu:8:0);
070200140520           TIBS10R (tibs10ds)   ;
070300140520  1        if d10err<>*blanks   ;
070400140520           // Se errore verifico se è figlio
070500140520              clear tibs10ds ;
070600140520              d10tle='WW'        ;
070700140520              d10paf='P'    ;
070800140520              d10cod=%dec(wksu:8:0);
070900140520              TIBS10R (tibs10ds)   ;
071000140520           endif;
071100140520
071200140520  2        if d10err<>*blanks   ;
071300140520             // Imposto nella skiera e come padre solo se stesso
071400140520             skc(1)=%editc(d10cod:'X');
071500140520             d10cop=d10cod      ;
071600140520  2        endif                ;
071700140520       // Verifico correttezza dell'ID bolla
071800140520       // Deve contenere solo numeri
071900140520         if %check(digits:widbolla)>0;
072000140520              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
072100140527              wIdMsg   = 'TIS0734';
072200140527              wIdCampo = 'IDBOLLA   ';
072300140527              clear wParm;
072400140527              exsr Messaggi;
072500140520              exsr RoutEnd;
072600140520         endif;
072700140529       // Se ricevuto anche il BRTCODE lo controllo
072800140529         if wbrtcode<>*blanks;
072900140529            exsr sr_trul28;
073000140529       //   Id bolla e brtcode devono riferirsi alla stessa spedizione
073100140529            if widbolla<>%subst(wbrtcode:1:14) ;
073200140529              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
073300140529              wIdMsg   = 'TIS0751';
073400140529              wIdCampo = 'IDBOLLA   ';
073500140530              wparm = wbrtcode + widbolla;
073600140529              exsr Messaggi;
073700140529              wIdCampo = 'BRTCODE   ';
073800140529              exsr Messaggi;
073900140529              exsr RoutEnd;
074000140529            endif;
074100140529         endif;
074200140521       // La bolla deve esistere su archivio bolle arrivi:
074300140521          wbolla=widbolla;
074400140527          wcampo='IDBOLLA';
074500140521          exsr sr_chainbolla;
074600140520       // Verifico che il cliente sia il mittente o il destinatario della bolla
074700140522             if arbccm>0;
074800140522                eval wcliente='0000'+%editc(arbccm:'X');
074900140522                if %lookup(wcliente:skc)=0;
075000140522                   eval wcliente='0000'+%editc(arbksc:'X');
075100140522                   if %lookup(wcliente:skc)=0;
075200140522                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
075300140527                      wIdMsg   = 'TIS0736';
075400140527                      wIdCampo = 'KSU       ';
075500140530                      wParm = widbolla;
075600140527                      exsr Messaggi;
075700140522                      exsr RoutEnd;
075800140522                   else;
075900140522                      wchi='D';
076000140522                   endif;
076100140522                else;
076200140522                   wchi='M';
076300140522                endif;
076400140522             else;
076500140522                eval wcliente='0000'+%editc(arbksc:'X');
076600140522                if %lookup(wcliente:skc)=0;
076700140522                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
076800140527                   wIdMsg   = 'TIS0736';
076900140527                   wIdCampo = 'KSU       ';
077000140530                   wParm = widbolla;
077100140527                   exsr Messaggi;
077200140522                   exsr RoutEnd;
077300140522                else;
077400140610                   exsr sr_ds3A;
077500140528                      if %subst(§3atb1:1:1)='F';
077600140522       //                se franco    --> mittente
077700140522                         wchi='M';
077800140522                      else;
077900140522       //                se Assegnato --> Destinatario
078000140522                         wchi='D';
078100140522                      endif;
078200140522                endif;
078300140522             endif;
078400140519         else;
078500140520       //?Accesso per BRTCode
078600140529          exsr sr_trul28;
078700140521       // La bolla deve esistere su archivio bolle arrivi:
078800140521          wbolla=%subst(wbrtcode:1:14);
078900140527          wcampo='BRTCODE';
079000140521          exsr sr_chainbolla;
079100140522          wchi='D';
079200140519         endif;
079300140620       //?Verifico se presenti legami bolla
079400140620         exsr sr_lbl;
079500131008
079600131008       ENDSR;
079700140529       //--------------------------------------------------------------
079800140529       //?Controllo correttezza BRTCODE
079900140529       //--------------------------------------------------------------
080000140529       BEGSR  sr_trul28 ;
080100140529          clear trul28ds0;
080200140529          i280cod=wbrtcode;
080300140529          trul28r0(trul28ds0);
080400140529          if o280err='1';
080500140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
080600140529             wIdMsg   = 'TIS0737';
080700140529             wIdCampo = 'BRTCODE   ';
080800140530             wparm = wbrtcode ;
080900140529             exsr Messaggi;
081000140529             exsr RoutEnd;
081100140529          endif;
081200140529       ENDSR;
081300140523       //--------------------------------------------------------------
081400140523       //?Controlli stato della bolla per abilitare le istruzioni di consegna
081500140523       //--------------------------------------------------------------
081600140523       BEGSR  statobolla;
081700140530          clear wtentacons;
081800140620          clear wdcreur;
081900140620          clear wrsd2  ;
082000140620
082100140620       // DETErMINO IL GESTORE DELLA LNA
082200140620         clear fnlv55ds;
082300140620         d55lin=arblna;
082400140620         d55tpt='6';
082500140620         d55drf=dataoggi;
082600140620         fnlv55r(fnlv55ds);
082700140620         wfgslna=d55tfA;
082800140620         clear wfgslnad;
082900140620         chain (wfgslna) azorg01l;
083000140620         if %found(azorg01l);
083100140620            wfgslnad=orgdes;
083200140620         endif;
083300140620
083400140620         clear dataeur;
083500140620         if arbdcr>0 ;
083600140620            dataeur=%date(arbdcr:*iso);
083700140620            wdcreur=%char(dataeur);
083800140620         endif;
083900140620
084000140620         clear wrbhcr ;
084100140620         if arbhcr>0 ;
084200140620            wrbhcr=%editc(arbhcr:'X');
084300140620         endif;
084400140620
084500140620       //wtprich=%subst(arbgc1:2:1);
084600140620       //if wtprich=*blanks;
084700140620       //    wtprich=%subst(arbgc2:2:1);
084800140620       //endif;
084900140620
085000140620            chain (arbaas:arblnp:arbnrs:arbnsp:'D') fiar401l ;
085100140620            if %found(fiar401l);
085200140620               eval wrsd2=%subst(ar4not:1:35);
085300140620            endif;
085400140620
085500140530       //?deve essere diretta in Italia
085600140530         clear og143;
085700140530         chain (arblna) azorg01l;
085800140530         if %found(azorg01l);
085900140530            og143=orgde3;
086000140530            if §ogntw = 'EEX' or §ogntw= 'FED' or §ogntw= 'DPD';
086100140530               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
086200140530               wIdMsg   = 'TIS0739';
086300140530               wIdCampo = '*BOLLA    ';
086400140530               wParm = wbolla;
086500140530               exsr Messaggi;
086600140530               exsr routend;
086700140530            endif;
086800140530         endif;
086900140523       //?Spedizione non deve essere consegnata
087000140523          if arbdcm>0;
087001141007            IF  prmRqsOpCode =  FNLRY00_RQSOPCODE_CHK_ISTRUCO;
087002141006                wIdMsg   = 'TISA738';
087003141006                wIdCampo = '*BOLLA    ';
087004141006                clear wParm ;
087005141006            else;
087200140527                wIdMsg   = 'TIS0738';
087300140527                wIdCampo = '*BOLLA    ';
087400140530                wParm = wbolla ;
087401141006            endif;
087402141006                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
087500140527                exsr Messaggi;
087600141006                exsr routend;
087700140523          else;
087800140523      /end-free
087900140523     c* prima vedo se c'e' da PDA
088000140708     c                   exsr      sr_RepPct
088100140523      /free
088200140523          endif;
088300140523       //?non deve avere giacenze o c.a. aperte
088400140620         chain (Arbaas:arblnp:arbnrs:arbnsp) tigcp21l;
088500140523         if %found(tigcp21l) and gcpatb=' ' and gcpdcg=0;
088600140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
088601141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
088700140527            wIdMsg   = 'TIS0740';
088800140527            wIdCampo = '*BOLLA    ';
088900140619            wParm = wbolla + wfgslnad;
088901141006           else;
088902141006            wIdMsg   = 'TISA740';
088903141006            wIdCampo = '*BOLLA    ';
088904141006            clear wParm ;
088905141006           endif;
089000140527            exsr Messaggi;
089100140523            exsr routend;
089200140523         endif;
089300140523         clear fidn12ds;
089400140620         i12aas=arbaas;
089500140620         i12lnp=arblnp;
089600140620         i12nrs=arbnrs;
089700140620         i12nsp=arbnsp;
089800140523         i12fel='E';
089900140523         i12fan='E';
090000140523         i12fch='E';
090100140523         fidn12r(fidn12ds);
090200140523         if o12nca>0;
090300140527            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
090301141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
090400140527            wIdMsg   = 'TIS0741';
090500140527            wIdCampo = '*BOLLA    ';
090600140530            wParm = wbolla;
090601141006           else;
090602141006            wIdMsg   = 'TISA741';
090603141006            wIdCampo = '*BOLLA    ';
090604141006            clear wParm ;
090605141006           endif;
090700140527            exsr Messaggi;
090800140523            exsr routend;
090900140523         endif;
091000140619       //?non deve essere bloccata
091100140917         if arbfbc<>*blanks and arbfbc<>'A';
091200140619            prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
091201141006           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
091300140619            wIdMsg   = 'TIS0740';
091400140619            wIdCampo = '*BOLLA    ';
091500140619            wParm = wbolla + wfgslnad;
091501141006           else;
091502141006            wIdMsg   = 'TISA740';
091503141006            wIdCampo = '*BOLLA    ';
091504141006            clear wParm ;
091505141006           endif;
091600140619            exsr Messaggi;
091700140619            exsr routend;
091800140619         endif;
091801140910       //?non deve avere un codice mancata consegna di tipo giacenza
091802140911       //if arbcmc <> *blanks;
091803140911       //  clear ds2a;
091804140911       //  w008a = arbcmc;
091805140911       //  chain (1:'2A':w008a) tabel00f;
091806140911       //  if %found(tabel00f);
091807140911       //     ds2a=tbluni;
091809140911       //  endif;
091810140911       //  if §2aftc='G';
091811140911       //     prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
091812140911       //     wIdMsg   = 'TIS0740';
091813140911       //     wIdCampo = '*BOLLA    ';
091814140911       //     wParm = wbolla + wfgslnad;
091815140911       //     exsr Messaggi;
091816140911       //     exsr routend;
091817140911       //  endif;
091818140911       //endif;
091900140530       //?deve essere già stato fatto almeno un tentativo di consegna
092000140603         if §IVPTENC='S' and arbntc=0 and wtentacons=*blanks  ;
092100140530             wIdMsg   = 'TIS0752';
092200140530             wIdCampo = '*NOTENTAT ';
092300140530             wParm = wbolla;
092400140530             exsr Messaggi;
092500140530             exsr routend;
092600140530         endif;
092601141016       //?non deve essere una spedizione per i supermercati
092602141016         if arbtc1='S' or arbtc2='S'                          ;
092603141016             wIdMsg   = 'TIS0812';
092604141016             wIdCampo = '*BOLLA    ';
092605141016             clear wparm;
092606141016             exsr Messaggi;
092607141016             exsr routend;
092608141016         endif;
092700140604       //?Per la spedizione non devono esserci disposizioni ancora da elaborare
092800140624         IF  prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
092900140604          exsr sr_ChkDispo;
093000140624         endif;
093100140530       //?NON TROVATI ERRORI:
093200140530       //?Per GET_ISTRUCO --> attivo il bottone e restituisco i dati della bolla
093300140527         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
093400140527            fnlry00o0.BUTISTRCON='1';
093500140527       // restituisco anche dati relativi alla bolla
093600140527            exsr sr_GetDatiBolla;
093700140527            endif;
093800140523
093900140527       endsr;
094000140523       //--------------------------------------------------------------
094100140523       //?Controlli immagine bolla da WEB con bolla su ARB
094200140523       //--------------------------------------------------------------
094300140523       BEGSR  immagineB;
094400140523       // Chaino estensioni bolla
094500140529       // exsr sr_chainEstBolla;
094600140523       // Errore se il referente di consegna sulla bolla è diverso dall'immagine bolla
094700140523       // che ricevo
094800140523          if wrefbolla<>fnlry00i1.oldrefcons or
094900140523             wtelbolla<>fnlry00i1.oldtelcons or
095000140523             §ar5eml<>fnlry00i1.olddesmail or
095100140528             §ar5tel<>fnlry00i1.olddescell           ;
095200140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
095300140527               wIdMsg   = 'TIS0742';
095400140528               wIdCampo = '*CHGDATA  ';
095500140527               clear wParm;
095600140527               exsr Messaggi;
095700140523               exsr routend;
095800140523          endif;
095900140527         select;
096000140527       // Disposizioni di Appuntamento
096100140527         when fnlry00i1.tipodispo = '1';
096200140527          if wdcreur<>fnlry00i1.olddtcrich or
096300140529             wrbhcr<>fnlry00i1.oldorcrich  or
096400140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
096500140527               wIdMsg   = 'TIS0742';
096600140527               wIdCampo = '*CHGDATA  ';
096700140527               clear wParm;
096800140527               exsr Messaggi;
096900140527               exsr routend;
097000140527          endif;
097100140527       // Disposizioni di consegna ad altro indirizzo
097200140527         when fnlry00i1.tipodispo = '3';
097300140527          if arbrsd+wrsd2 <>fnlry00i1.oldrsd or
097400140527             arbind<>fnlry00i1.oldind or
097500140527             arbcad<>fnlry00i1.oldcad or
097600140527             arblod<>fnlry00i1.oldlod or
097700140527             arbprd<>fnlry00i1.oldprd or
097800140527             arbnzd<>fnlry00i1.oldnzd ;
097900140527               prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
098000140527               wIdMsg   = 'TIS0742';
098100140527               wIdCampo = '*CHGDATA  ';
098200140527               clear wParm;
098300140527               exsr Messaggi;
098400140527               exsr routend;
098500140527          endif;
098600140527         endsl;
098700140523       ENDSR;
098800140523       //--------------------------------------------------------------
098900140523       //?Controllo dettaglio
099000140523       //--------------------------------------------------------------
099100140523       BEGSR  ControllaDett;
099200140523
099300140617       //?Appuntamento
099400140617       // (se non caricate date da proporre per GetIstruCo il bottone non deve
099500140617       //  essere attivato)
099600140609       if (prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO and fnlry00o0.nrdate>0)
099700140624                                                     or fnlry00i1.tipodispo='1';
099800140523          exsr sr_appuntam;
099900140523       endif;
100000140617       //?Fermo Deposito
100100140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='2';
100200140523          exsr sr_FD;
100300140523       endif;
100400140617       //?Altro Indirizzo
100500140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='3';
100600140523          exsr sr_AltroInd;
100700140523       endif;
100800140617       //?Altre Istruzioni
100900140624       if prmRqsOpCode=FNLRY00_RQSOPCODE_GET_ISTRUCO or fnlry00i1.tipodispo='4';
101000140523          exsr sr_AltreIstruz;
101100140523       endif;
101200140617       //?Per GETISTRUCO -->  se bottoni tutti in OFF metto in OFF anche bottone
101300140617       //?per le istruzioni di consegna
101400140616         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
101500140616            and fnlry00o0.BUTAPPUNT='0'
101600140616            and fnlry00o0.BUTFERDEP='0'
101700140616            and fnlry00o0.BUTindiriz='0'
101800140616            and fnlry00o0.BUTaltro='0';
101900140616            fnlry00o0.BUTISTRCON='0';
102000140617             wIdMsg   = 'TIS0770';
102100140617             wIdCampo =  '*BOLLA    ';
102200140617             wParm = wbolla;
102300140617             exsr Messaggi;
102400140617             exsr routend;
102500140616         endif;
102501140924       //?Per GETISTRUCO -->  se in ON  bottone per appuntam o altro indirizzo
102502140924       //?valorizzo campo messaggio per la preferenza di orario
102503140924         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO
102504140924            and (fnlry00o0.BUTAPPUNT='1'  or fnlry00o0.BUTindiriz='1');
102506140930               wtempoh=%div((§VPORST_MW + §VPORST_PW):60);
102507140930               wtempom=%rem((§VPORST_MW + §VPORST_PW):60);
102508140924               wIdMsg   = 'TIS0781';
102509140929               wparm=      %editc(wtempoh:'Z');
102510140924               if wtempom>0;
102511140929                  wIdMsg   = 'TIS0784';
102512140929                  wparm=      wparm + %editc(wtempom:'Z') ;
102513140924               endif;
102514140924               fnlry00o0.MSGPRFORA=rtvMsgLang(wIdMsg:widlingua:wparm);
102515140924         endif;
102600140523
102700140523       endsr;
102800140408
102900140521       //--------------------------------------------------------------
103000140521       //?Chain FNARB
103100140521       //--------------------------------------------------------------
103200140521       BEGSR  sr_chainbolla;
103300140527
103400140521       // imposto la chiave di accesso
103500140521          kaas=2000+%dec(%subst(wbolla:1:2):2:0);
103600140521          klnp=%dec(%subst(wbolla:3:3):3:0);
103700140521          knrs=%dec(%subst(wbolla:6:2):2:0);
103800140521          knsp=%dec(%subst(wbolla:8:7):7:0);
103900140521          chain (kaas:klnp:knrs:knsp) fnarb01l;
104000140521          if not %found(fnarb01l);
104100140521              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
104200140527              wIdMsg   = 'TIS0735';
104300140527              wIdCampo = wcampo      ;
104400140530              wParm=wbolla;
104500140527              exsr Messaggi;
104600140521              exsr RoutEnd;
104700140521          endif;
104701141211       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
104702141211         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
104703141211           fnlry00i1.oldCAD    = arbCAD    ;
104704141211           fnlry00i1.oldLOD    = arbLOD    ;
104705141211           fnlry00i1.oldPRD    = arbPRD    ;
104706141211           fnlry00i1.oldNZD    = arbNzd    ;
104707141211         endif;
104800140521       endsr;
104900140620       //--------------------------------------------------------------
105000140620       //?Operazioni per bolle legate
105100140620       //--------------------------------------------------------------
105200140620       BEGSR  sr_lbl          ;
105300140620          chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl02l;
105400140620          if %found(fnlbl02l);
105500140624       // Per CHK_ISTRUCO --> errore se la bolla da elaborare ha una figlia
105600141223       //    IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
105700141223       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
105800141223       //             wIdMsg   = 'TIS0777';
105900141223       //             wIdCampo = '*NODISPO  ';
106000141223       //             wParm = wbolla;
106100141223       //             exsr Messaggi;
106200141223       //             exsr RoutEnd;
106300141223       //    endif;
106400140620       //    BOLLA CONSEGNATA
106500140620             if arbdcm>0;
106600140620       //       E' LA BOLLA ORIGINALE
106700140620                if LBLAAO=arbaas and LBLLPO=arblnp and
106800140620                   LBLNRO=arbnrs and LBLNSO=arbnsp;
106900140620       //       Non è il Mittente --> Errore: utente non abilitato a dare disposizioni
107000140620                   if wchi <>'M';
107100140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
107200140620                      wIdMsg   = 'TIS0736';
107300140620                      wIdCampo = '*NOAUT    ';
107400140620                      wParm = wbolla;
107500140620                      exsr Messaggi;
107600140620                      exsr RoutEnd;
107700140620                   endif;
107800140620                else;
107900140620       //       NON E' LA BOLLA ORIGINALE
108000140620       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
108100140620                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
108200140627                      wIdMsg   = 'TIS0770';
108300140620                      wIdCampo = '*NODISPO  ';
108400140620                      wParm = wbolla;
108500140620                      exsr Messaggi;
108600140620                      exsr RoutEnd;
108700140620                endif;
108800140620             endif;
108900140620       //    Recupero i dati dell'ultima figlia
109000140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
109100140620             dow %found(fnlbl02l);
109200140620                chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnlbl02l;
109300140620             enddo;
109400140620             chain (LBLAAN:LBLLPN:LBLNRN:LBLNSN) fnarb01l;
109500140620             if not %found(fnarb01l);
109600140627       //    Se non trovo l'ultima figlia su arb errore
109700140627       //       Errore --> per questa spedizione non è possibile dare disposizioni di consegna
109800140627                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
109900140627                      wIdMsg   = 'TIS0777';
110000140627                      wIdCampo = '*NODISPO  ';
110100140627                      wParm = wbolla;
110200140627                      exsr Messaggi;
110300140627                      exsr RoutEnd;
110400140620             endif;
110401141223       // se CHKISTRUCO devo impostare anche i campi old della FNLRY00I1
110402141223         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
110403141223           fnlry00i1.oldCAD    = arbCAD    ;
110404141223           fnlry00i1.oldLOD    = arbLOD    ;
110405141223           fnlry00i1.oldPRD    = arbPRD    ;
110406141223           fnlry00i1.oldNZD    = arbNzd    ;
110407141223         endif;
110500140620          endif;
110600140620       endsr;
110700140523       //--------------------------------------------------------------
110800140523       //?Chain Estensioni bolla
110900140523       //--------------------------------------------------------------
111000140523       BEGSR  sr_chainEstBolla;
111100140523          clear dar5gen;
111200140523          clear wrefbolla;
111300140523          clear wtelbolla;
111400140620          chain (arbaas:arblnp:arbnrs:arbnsp:'GEN') fiar501l ;
111500140523          if %found(fiar501l);
111600140523             dar5gen=ar5uni;
111700140523       // Se presente # nel referente e nel telefono
111800140523       // lo devo togliere
111900140523             if %subst(§ar5ref:1:1)='#';
112000140523                eval wrefbolla=%subst(§ar5ref:2);
112100140523             else;
112200140523                eval wrefbolla=§ar5ref;
112300140523             endif;
112400140523             if %subst(§ar5teld:1:1)='#';
112500140523                eval wtelbolla=%subst(§ar5teld:2);
112600140523             else;
112700140523                eval wtelbolla=§ar5teld;
112800140523             endif;
112900140523          endif;
113000140523          clear dar5emd;
113100140620          chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
113200140523          if %found(fiar501l);
113300140523              dar5emd=ar5uni;
113400140523         endif;
113500140523       ENDSR;
113600140522       //--------------------------------------------------------------
113700140522       //?Recupera dati bolla da passare a chiamante
113800140522       //--------------------------------------------------------------
113900140522       BEGSR  sr_GetDatiBolla;
114000140603       // - orari da passare nella tendina: standard o minimo/massimo in base
114100140603       //   a quanto memorizzato in tabella
114101140923          clear waltroind;
114200140522          exsr sr_orarisr;
114300140604          if §ivptipo='M';
114400140604             fnlry00o0.ORARITNDI=%editc(OOR2MIIS:'X');
114500140604             fnlry00o0.ORARITNDF=%editc(OOR2MXFS:'X');
114600140604          else;
114700140604             fnlry00o0.ORARITNDI=%editc(OOR2STIS:'X');
114800140604             fnlry00o0.ORARITNDF=%editc(OOR2STFS:'X');
114900140604          endif;
114901140923       // - per la tendina con orari di filiale -->
114902140923         clear tisio9ds;
114903140923         do0lna=arblna;
114904140923         do0tric='S';
114905140923         do0tser='C';
114906140923         tisio9r (kpjba:tisio9ds);
114907140923          if §ivptipo='M';
114908140923             fnlry00o0.ORARITNFI=%editc(DO0OMIIS:'X');
114909140923             fnlry00o0.ORARITNFF=%editc(DO0OMXFS:'X');
114910140923          else;
114911140923             fnlry00o0.ORARITNFI=%editc(DO0OSTIS:'X');
114912140923             fnlry00o0.ORARITNFF=%editc(DO0OSTFS:'X');
114913140923          endif;
115000140603          fnlry00o0.MMINTERVC=%editc(§ivpmini:'X');
115100140613       // DATE DI CONSEGNA DA PROPORRE  -------------------
115200140605       // a partire da oggi vengono proposte le date fino al giorno limite
115300140605       // previsto per apertura giacenza
115400140605       // 1) verifico se previsti giorni personalizzati per cliente
115500140605         exsr sr_PersGGiac;
115600140709       // 2) Verifico se effetuato un tentativo di consegna ai fini dell'abilitazione
115700140709       //    della riconsegna
115800140709         IF §IVPARIC='S'AND OOR2FRIC='S';
115900140709            exsr sr_RepPct   ;
116000140709         ENDIF;
116100140709       // 3) Ciclo da oggi per numero di giorni per caricare campo delle date
116200140605       //    consegna richiesta da proporre
116300140605         wdata=dataoggi;
116400140605         clear ix;
116401140923         clear iy;
116500140605         for I=1 to wlimitdcr;
116600140606       //   se cade in un giorno festivo incremento la data di un giorno
116700140606            exsr sr_datafes;
116800140606       //   Propongo la data di oggi solo se possibile la riconsegna in giornata
116900140709            if (wdata=dataoggi and oor2fric='S'
117000140709                and §ivparic='S' and wtentacons='1') or wdata<>dataoggi;
117100140609       //   Memorizzo la data, purchè, in presenza di data cons.rich "Dopo il",
117200140609       //   la stessa non sia <= della data "Dopo Il"
117300140710             if arbtcr<>'D' or (arbtcr='D' and wdata>arbdcr);
117400140606               ix+=1;
117500140606               fnlry00o0.schdcr(ix)=%char(%date(wdata:*iso):*eur);
117501140923       //      la data per l'indirizzo alternativo non deve mai essere "oggi"
117502140923               if wdata<>dataoggi;
117503140923                  iy+=1;
117504140923                  fnlry00o0.schdcri(iy)=%char(%date(wdata:*iso):*eur);
117505140923                  fnlry00o0.nrdatei+=1;
117506140923               endif;
117600140606       //      Data da Consigliare --> quella successiva a oggi oppure
117700140606       //      quella della bolla se maggiore
117800140606               if fnlry00o0.dtconsric=*blanks  and wdata<>dataoggi ;
117900140606                   fnlry00o0.dtconsric=%char(%date(wdata:*iso):*eur);
118000140606                   if arbdcr>0 and arbtcr=*blanks and arbdcr>wdata;
118100140606                      fnlry00o0.dtconsric=%char(%date(arbdcr:*iso):*eur);
118200140606                   endif;
118300140606               endif;
118400140606       //   Incremento contatore date da passare a chiamante
118500140606               fnlry00o0.nrdate+=1;
118600140609             endif;
118700140606            endif;
118800140606       //   Incremento la data di un giorno
118900140606            exsr sr_IncreData;
119000140605         endfor;
119100140522       // - consegna richiesta: data/ora/tipo
119200140606       //if arbdcr>0 ;
119300140606       //   fnlry00o0.DTCONSRIC=wdcreur;
119400140606       //endif;
119500140606       //if arbhcr>0;
119600140606       //   fnlry00o0.ORCONSRIC=%editc(arbhcr:'X');
119700140606       //endif;
119800140606       //fnlry00o0.TPCONSRIC=arbtcr;
119900140613       // indirizzo e-mail e n. telefono per SMS -----------
120000140620         chain (arbaas:arblnp:arbnrs:arbnsp:'EMD') fiar501l ;
120100140522         if %found(fiar501l);
120200140522             dar5emd=ar5uni;
120300140522             fnlry00o0.destmail=§ar5eml;
120400140522             fnlry00o0.destcell=§ar5tel;
120500140522         endif;
120600140613       // Messaggio di avviso per indicare che il FD deve essere autorizzato dal mitt. ------------
120700140522         if arbffd=*blanks and arbgma<>*blanks;
120800140529            exsr sr_tab7r;
120900140522            if §7rfd='S' and wchi='D';
121000140522               wIdMsg   = 'TIS0733';
121100140617               fnlry00o0.MSGFDMITT=rtvMsgLang(wIdMsg:widlingua);
121200140522            endif;
121300140522         endif;
121400140522       endsr;
121500140606       //--------------------------------------------------------------
121600140606       //?Controllo data se festiva + restituzione data prima data non festiva
121700140606       //--------------------------------------------------------------
121800140606       BEGSR  sr_DataFes;
121900140606       wfes='1';
122000140606       dow wfes='1';
122100140606       chain (0:wfgslna:%subdt(%date(wdata):*years):
122200140606                        %subdt(%date(wdata):*MONTHS)) azcln01l;
122300140606          if %found(azcln01l) and mat(%subdt(%date(wdata):*days)) = 'F'
122400140606                               or pom(%subdt(%date(wdata):*days)) = 'F';
122500140606       // data festiva: la incremento di un giorno
122600140606             exsr sr_incredata;
122700140606          else;
122800140606            clear wfes;
122900140606          endif;
123000140606       enddo;
123100140606       endsr;
123200140606       //--------------------------------------------------------------
123300140606       //?Incremento data di un giorno
123400140606       //--------------------------------------------------------------
123500140606       BEGSR  sr_incredata;
123600140606             dataiso=%date(wdata);
123700140606             dataiso=dataiso+%days(1);
123800140606             wdata=%dec(dataiso);
123900140606       endsr;
124000140521       //--------------------------------------------------------------
124100140521       //?Controlli per abilitare la richiesta di appuntamento
124200140521       //--------------------------------------------------------------
124300140521       BEGSR  sr_appuntam;
124400140528
124500140528       // Non ammesso per spedizioni in fermo deposito
124600140528         if arbffd= 'S';
124700140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
124800140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
124900140528               leavesr;
125000140528            else;
125100140528       //?PUT_ISTRUCO --> uscita da pgm con errore
125200140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
125201141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
125300140528              wIdMsg   = 'TIS0743';
125400140528              wIdCampo = '*NOAPPUNT ';
125500140530              wParm = wbolla ;
125501141006         else;
125502141006              wIdMsg   = 'TISA743';
125503141006              wIdCampo = '*NOAPPUNT ';
125504141006              clear wParm  ;
125505141006         endif;
125600140528              exsr Messaggi;
125700140528              exsr routend;
125800140528            endif;
125900140528         endif;
126000140528
126100140528       //?GET_ISTRUCO --> attivazione bottone per appuntamento
126200140528         if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
126300140528            fnlry00o0.BUTAPPUNT='1';
126400140528         else;
126500140528       //?PUT_ISTRUCO --> controllo dati ricevuti
126600140528            exsr sr_ctrApp;
126700140528         endif;
126800140528
126900140521       endsr;
127000140522       //--------------------------------------------------------------
127100140522       //?Controlli per abilitare la richiesta di Fermo Deposito
127200140522       //--------------------------------------------------------------
127300140522       BEGSR  sr_FD      ;
127400140522
127500140528       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
127600140528          if arbffd = 'S';
127700140528       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
127800140528            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
127900140528               leavesr;
128000140528            else;
128100140528       //?PUT_ISTRUCO --> uscita da pgm con errore
128101141223            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO;
128200140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
128201141223       //if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
128300140528              wIdMsg   = 'TIS0743';
128400140528              wIdCampo = '*NOFD     ';
128500140530              wParm = wbolla;
128501141223       //else;
128502141223       //     wIdMsg   = 'TISA743';
128503141223       //     wIdCampo = '*NOFD     ';
128504141223       //     clear wParm;
128505141223       //endif;
128600140528              exsr Messaggi;
128700140528              exsr routend;
128701141223            endif;
128800140528            endif;
128900140528          else;
129000140530       //?GET_ISTRUCO -->
129100141008            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
129200141008       //   and fnlry00o0.MSGFDMITT=*blanks;
129400140616       //?GET_ISTRUCO -->> Restituisco, insieme al bottone attivato anche messaggio
129500140616       //?                 riguardante le codizioni di ritiro in FD
129600140616       //?                 Solo se msgfdmit=*blanks
129700140616             if fnlry00o0.msgfdmitt=*blanks;
129701141008                fnlry00o0.BUTFERDEP='1';
129702140924                exsr sr_wdesgio;
131000140617               wparm=wdesgio + %editc(WlimitFD:'X') + wfgslnad;
131100140616               wIdMsg   = 'TIS0768';
131200140617               fnlry00o0.MSGFDCOND=rtvMsgLang(wIdMsg:widlingua:wparm);
131201141008             else;
131202141008                fnlry00o0.BUTFERDEP='2';
131300140616             endif;
131400140616            endif;
131500140606       //?PUT_ISTRUCO --> Controllo dati ricevuti
131600140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
131700140624               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
131800140616       //?                Errore al destinatario se è richiesta autorizzaz.
131900140616       //?                da parte del mittente
132000140616             if arbgma<>*blanks;
132100140616                exsr sr_tab7r;
132200140616                if §7rfd='S' and wchi='D';
132300140616                   prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
132400140616                   wIdMsg   = 'TIS0733';
132500140616                   wIdCampo = '*NOFD     ';
132600140616                   clear wParm;
132700140616                   exsr Messaggi;
132800140616                   exsr routend;
132900140616                endif;
133000140616             endif;
133100140606             exsr sr_ctrUNI;
133200140606            endif;
133300140530          endif;
133400140522       endsr;
133500140522       //--------------------------------------------------------------
133600140522       //?Controlli per abilitare la richiesta di Altro Indirizzo
133700140522       //--------------------------------------------------------------
133800140522       BEGSR  sr_altroind;
133900140522
134000140616       if §ivpdind='S';
134100140617       // NON AMMESSO PER SPEDIZIONI IN FERMO DEPOSITO
134200140617          if arbffd = 'S';
134300140617       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
134400140617            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
134500140617               leavesr;
134600140617            else;
134700140617       //?PUT_ISTRUCO --> uscita da pgm con errore
134800140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
134801141007           if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
134900140617              wIdMsg   = 'TIS0743';
135000140617              wIdCampo = '*NOALTRIND';
135100140617              wParm = wbolla;
135101141007           else;
135102141007              wIdMsg   = 'TISA743';
135103141007              wIdCampo = '*NOALTRIND';
135104141007              clear wparm ;
135105141007           endif;
135200140617              exsr Messaggi;
135300140617              exsr routend;
135400140617            endif;
135500140617          endif;
135600140619       //NON AMMESSO AL DESTINATARIO SE NON ABILITATO MEDIANTE TABELLA 7W
135700140619         clear ds7W;
135800140619         if wchi='D' and arbgga<>*blanks;
135900140619            w008a=ARBGGA;
136000140619            chain (1 : '7W' : W008A) tabel00f;
136100140619            if %found(tabel00f);
136200140619               ds7w=tbluni;
136300140619            endif;
136400140619         endif;
136500140619         if wchi='D' and §7wdes<>'S';
136600140619       //?GET_ISTRUCO --> Uscita dalla routine con bottone disattivato
136601140918       //?                18/09/2014 --> attivo il bottone ma poi verrà dato errore
136602140918       //?                Il concetto è: il cambio di indirizzo è gestito (e quindi il
136603140918       //?                bottone deve essere attivo) ma non può essere richiesto da
136604140918       //?                destinatario se non autorizzato (e quindi poi viene dato errore)
136700140918            if prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
136701141007               fnlry00o0.BUTindiriz='2';
136702141007               wIdMsg   = 'TIS0796';
136704141007               fnlry00o0.MSGAUTIND=rtvMsgLang(wIdMsg:widlingua);
136800141007               leavesr;
136900140619            else;
137000140619       //?PUT_ISTRUCO --> uscita da pgm con errore
137100140619              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
137200140619              wIdMsg   = 'TIS0776';
137300140619              wIdCampo = '*NOALTRIND';
137301140924              clear wparm;
137400140924       //     wParm = wbolla;
137500140619              exsr Messaggi;
137600140619              exsr routend;
137700140619            endif;
137800140619         endif;
137900140522         fnlry00o0.BUTindiriz='1';
138000140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
138100140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
138200140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
138300140611       //?Controllo dati comuni a tutti tipi di disposizione
138400140611             exsr sr_ctrUNI;
138500140611
138600140609             exsr sr_ctrind;
138700140609            endif;
138800140616       else;
138900140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
139000140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO or
139100141016               prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO ;
139200141016                 prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
139300141016                 prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
139400141016                 exsr RoutEnd;
139500140616            endif;
139600140616       endif;
139700140522       endsr;
139800140522       //--------------------------------------------------------------
139900140522       //?Controlli per abilitare la richiesta di Altre Istruzioni
140000140522       //--------------------------------------------------------------
140100140522       BEGSR  sr_AltreIstruz;
140200140522
140300140616       if §ivpdalr='S';
140400140522         fnlry00o0.BUTaltro='1';
140500140609       //?PUT_ISTRUCO --> Controllo dati ricevuti
140600140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
140700140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
140800140609             exsr sr_ctrUNI;
140900140609            endif;
141000140616       else;
141100140616       //?PUT_ISTRUCO --> Errore se disposizione non attivabile
141200140624            if prmRqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO
141300140624            or prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
141400140617              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
141500140617              prmRpyIdMsg  = FNLRY00_ESITO_TIPODISPO_NONAMMESSO;
141600140617              exsr RoutEnd;
141700140616            endif;
141800140616       endif;
141900140522       endsr;
142000140522
142100140523       //--------------------------------------------------------------
142200140523       //?Controllo Disposizioni di Appuntamento
142300140523       //--------------------------------------------------------------
142400140523       BEGSR  sr_ctrApp     ;
142500140611       //?Controllo dati comuni a tutti tipi di disposizione
142600140611          exsr sr_ctrUNI;
142700140528       //?Consegna richiesta
142800140528          exsr sr_ctrConsRich;
142900140523       endsr;
143000140528       //--------------------------------------------------------------
143100140528       //?Controllo Data/ora/tipo consegna richiesta
143200140528       //--------------------------------------------------------------
143300140528       BEGSR  sr_CtrConsRich;
143301140923          clear waltroind;
143400140606          exsr sr_Orarisr;
143500140528       // DATA CONSEGNA RICHIESTA:
143600140528          exsr sr_ctrDCR;
143700140528       // ORA  CONSEGNA RICHIESTA:
143800140528       if fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
143900140528       // Deve contenere solo numeri
144000140528         if %check(digits:fnlry00i1.neworcrich)>0;
144100140528              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
144200140528              wIdMsg   = 'TIS0746';
144300140528              wIdCampo = 'NEWORCRICH';
144400140530              wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
144500140530                      %subst(fnlry00i1.neworcrich:3:2);
144600140528              exsr Messaggi;
144700140528         else;
144800140709       // Se data consegna richiesta diversa da oggi (no riconsegna)
144900140709       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
145000140528              wora = %dec(fnlry00i1.neworcrich:4:0);
145100141216              if wdcr<>wdataconfr;
145200140709                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
145300140709                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
145400140709                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
145500140709                       wIdMsg   = 'TIS0747';
145600140709                       wIdCampo = 'NEWORCRICH';
145700140709                       wParm = %subst(fnlry00i1.neworcrich:1:2) + ':' +
145800140709                       %subst(fnlry00i1.neworcrich:3:2);
145900140709                       exsr Messaggi;
146000140709                 endif;
146100140709              else;
146200140709       //  Se Data Consegna Richiesta=oggi (riconsegna) l'orario deve essere incluso negli orari
146300140610       //  inizio/fine previsti per la riconsegna
146400140709                 if  wora < OOR2RIDS  or
146500140709                     wora > OOR2RIAS ;
146600140709                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
146700140709                        wIdMsg   = 'TIS0778';
146800140709                        wIdCampo = 'NEWORCRICH';
146900140709                        wParm = %editw(oor2rids:'  :  ') +
147000140709                                %editW(oor2rias:'  :  ')  ;
147100140709                        exsr Messaggi;
147200140709                 endif;
147300140709              endif;
147400140528         endif;
147500140528       endif;
147600140529       // PREFERENZA MATTINO/POMERIGGIO
147700140529       if fnlry00i1.prconsric<>*blanks and
147800140529          fnlry00i1.prconsric<>'M' and
147900140529          fnlry00i1.prconsric<>'P'               ;
148000140528                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
148100140528                    wIdMsg   = 'TIS0749';
148200140529                    wIdCampo = 'PRCONSRIC ';
148300140528                    clear wParm;
148400140528                    exsr Messaggi;
148500140528       endif;
148600140710       // NO RICONSEGNA:
148700140710       // Verifico preferenza m/p con gli orari standard o min/max in base
148800140710       // all'ora limite del mattino
148900141216       if wdcr<>wdataconfr;
149000140710         if (fnlry00i1.prconsric='P' and §ivptipo='S' and §ivporlm>=OOR2STFS) or
149100140710            (fnlry00i1.prconsric='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
149200140710            (fnlry00i1.prconsric='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS) or
149300140710            (fnlry00i1.prconsric='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
149400140710            If fnlry00i1.prconsric='P';
149500140710                widmsg='TIS0676';
149600140710            else;
149700140710                widmsg='TIS0667';
149800140710            endif;
149900140710            matpom=rtvMsgLang(widmsg : widlingua);
150000140710                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
150001141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
150100140710                      wIdMsg   = 'TIS0760';
150200140710                      wIdCampo = 'PRCONSRIC ';
150300140710                      wParm = MatPom + wbolla;
150301141006         else;
150302141006                      wIdMsg   = 'TISA760';
150303141006                      wIdCampo = 'PRCONSRIC ';
150304141006                      wParm = MatPom ;
150305141006         endif;
150400140710                      exsr Messaggi;
150500140710         ENDIF;
150600140710       ELSE;
150700140710       // SI RICONSEGNA:
150800140710       // Verifico preferenza m/p con gli orari previsti per la riconsegna
150900140710       // in base all'ora limite del mattino
151000140710          if (fnlry00i1.prconsric='P' and §ivporlm>=OOR2rias) or
151100140710             (fnlry00i1.prconsric='M' and §ivporlm<OOR2rids);
151200140710             If fnlry00i1.prconsric='P';
151300140710                 widmsg='TIS0676';
151400140710             else;
151500140710                 widmsg='TIS0667';
151600140710             endif;
151700140710             matpom=rtvMsgLang(widmsg : widlingua);
151800140710                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
151900140710                       wIdMsg   = 'TIS0779';
152000140710                       wIdCampo = 'PRCONSRIC ';
152100140710                       wParm = MatPom ;
152200140710                       exsr Messaggi;
152300140710          ENDIF;
152400140606       endif;
152500140529       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
152600140529       if fnlry00i1.prconsric<>*blanks and
152700140529          fnlry00i1.neworcrich<>*blanks and fnlry00i1.neworcrich<>*zeros;
152800140529                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
152900140530                    wIdMsg   = 'TIS0753';
153000140530                    wIdCampo = 'NEWORCRICH';
153100140530                    clear wParm;
153200140529                    exsr Messaggi;
153300140530                    wIdCampo = 'PRCONSRIC ';
153400140529                    exsr Messaggi;
153500140529       endif;
153600140528       //  Se Campi tutti vuoti errore
153700140529       //if (fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros) and
153800140529       //   (fnlry00i1.neworcrich=*blanks or fnlry00i1.neworcrich=*zeros) and
153900140529       //    fnlry00i1.prconsric=*blanks;
154000140529       //             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
154100140529       //             wIdMsg   = 'TIS0748';
154200140529       //             wIdCampo = '*NOSELEZ';
154300140529       //             clear wParm;
154400140529       //             exsr Messaggi;
154500140529       //endif;
154600140528       endsr;
154700140528       //--------------------------------------------------------------
154800140528       //?Controllo Data Consegna richiesta
154900140528       //--------------------------------------------------------------
155000140528       BEGSR  sr_ctrDCR     ;
155100140528       // DATA CONSEGNA RICHIESTA:
155200140529       // Obbligatoria
155300140529       if fnlry00i1.newdtcrich=*blanks or fnlry00i1.newdtcrich=*zeros;
155400140529             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
155500140529             wIdMsg   = 'TIS0130';
155600140529             wIdCampo = 'NEWDTCRICH';
155700140529             clear wParm;
155800140529             exsr Messaggi;
155900140529             leavesr;
156000140529       endif;
156100140528       //  Controllo correttezza formale
1562001405282        monitor  ;
156300140528         dataiso=%date(fnlry00i1.newdtcrich:  *eur)   ;
156400140528         on-error ;
156500140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
156600140528             wIdMsg   = 'TIS0121';
156700140528             wIdCampo = 'NEWDTCRICH';
156800140528             clear wParm;
156900140528             exsr Messaggi;
157000140528             leavesr;
157100140528 2       endmon  ;
157200140528
157300140528       wdcr=%dec(dataiso);
157400140528
157500140528       //  Non inferiore a oggi
157600140528       if wdcr < dataoggi;
157700140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
157800140528             wIdMsg   = 'TIS0744';
157900140528             wIdCampo = 'NEWDTCRICH';
158000140611             wParm=%char(%date(wdcr:*iso):*eur);
158100140528             exsr Messaggi;
158200140528             leavesr;
158300140528       endif;
158400140709       //  Uguale ad oggi solo se possibile la riconsegna in giornata
158401141215       //  Per chkistruco considero "data oggi" la data in cui è stata
158402141215       //  fatta la  richiesta
158403141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
158405141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
158406141215         else;
158407141218              wdataconfr=dataoggi;
158408141215         endif;
158500141215       if wdcr=wdataconfr;
158600140709         if oor2fric='S' and §ivparic='S';
158700140709          exsr sr_reppct;
158800140709         endif;
158900140709         if oor2fric='N' or §ivparic<>'S' or wtentacons=' ';
159000140606             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
159100140606             wIdMsg   = 'TIS0759';
159200140606             wIdCampo = 'NEWDTCRICH';
159300140606             clear wparm;
159400140606             exsr Messaggi;
159500140606             leavesr;
159600140709         endif;
159700140606       endif;
159800140528       //  Non superiore ai Giorni limite giac. con data cons.rich.
159900140528       //      Verifico se presenti limiti personalizzati per il cliente
160000140528
160100140605       exsr sr_PersGGiac;
160200140528       //Calcolo giorni lavorativi fra data odierna e data consegna richiesta
160300141215       datada=wdataconfr;
160400140528       dataa=wdcr;
160500140606       clear tfptfa;
160600140610       p_tfa =wfgslna;
160700140606       xsrlav8(wdat8:tfptfa);
160800140528       if ggl > wlimitdcr;
160900140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
161000140528             wIdMsg   = 'TIS0745';
161100140528             wIdCampo = 'NEWDTCRICH';
161200140611             wParm=%char(%date(wdcr:*iso):*eur);
161300140528             exsr Messaggi;
161400140528             leavesr;
161500140528       endif;
161600140606       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
161700140528       //  Controllo anche con la data consegna richiesta originale
161800140609       if (arbtcr='D' and wdcr<=arbdcr) or
161900140609         (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
162000140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
162100140529             wIdMsg   = 'TIS0750';
162200140528             wIdCampo = 'NEWDTCRICH';
162300140606             if arbtcr='D' ;
162400140606             dataeur=%date(%dec(arbdcr:8:0):*iso);
162500140606             else;
162600140606             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
162700140606             endif;
162800140611             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
162900140528             exsr Messaggi;
163000140528             leavesr;
163100140528       endif;
163200140605       // Non deve cadere in giorno festivo
163300140605       chain (0:wfgslna:%subdt(%date(wdcr):*years):
163400140605                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
163500140605       if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
163600140605                            or pom(%subdt(%date(wdcr):*days)) = 'F';
163700140605             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
163800140605             wIdMsg   = 'TIS0758';
163900140605             wIdCampo = 'NEWDTCRICH';
164000140611             wParm=%char(%date(wdcr:*iso):*eur);
164100140605             exsr Messaggi;
164200140605             leavesr;
164300140605       endif;
164400140528
164500140528       endsr;
164600140605       //--------------------------------------------------------------
164700140605       //?Rperimento eventuale personalizzazione giorni attesa prima di apert.giac
164800140605       //--------------------------------------------------------------
164900140605       BEGSR  sr_PersGGiac  ;
165000140606       // Faccio come fa fnlg80r --> prima prova on arbksc
165100140606       // e se non trova con questo prova poi con arbccm
165200140606       clear ds7u;
165300140606       w008a=%editc(arbksc:'X')+'Q';
165400140606       chain (1:'7U':w008a) tabel00f;
165500140606       if %found (tabel00f);
165600140606          ds7u=tbluni;
165700140606       else;
165800140606          w008a=%editc(arbCCM:'X')+'Q';
165900140606          chain (1:'7U':w008a) tabel00f;
166000140606          if %found (tabel00f);
166100140606             ds7u=tbluni;
166200140606          endif;
166300140606       endif;
166400140606       if §7uldcr>'00';
166500140606          wlimitdcr=%dec(§7uldcr:2:0);
166600140606       endif;
166700140605       // Cerco con il ccm se presente altrimenti con KSC
166800140606       // clear ds7u;
166900140606       //if arbccm > 0;
167000140606       //   w008a=%editc(arbCCM:'X')+'Q';
167100140606       //   chain (1:'7U':w008a) tabel00f;
167200140606       //   if %found (tabel00f);
167300140606       //      ds7u=tbluni;
167400140606       //   endif;
167500140606       //else;
167600140606       //   w008a=%editc(arbKSC:'X')+'Q';
167700140606       //   chain (1:'7U':w008a) tabel00f;
167800140606       //   if %found (tabel00f);
167900140606       //      ds7u=tbluni;
168000140606       //   endif;
168100140606       //endif;
168200140606       //if §7uldcr>'00';
168300140606       //   wlimitdcr=%dec(§7uldcr:2:0);
168400140606       //endif;
168500140605       endsr;
168600140609       //--------------------------------------------------------------
168700140609       //?Controlli Disposizioni di consegna ad altro indirizzo
168800140609       //--------------------------------------------------------------
168900140609       BEGSR  sr_ctrind;
168901141006          clear wolna;
169000140610       // Per il controllo dell'indirizzo (CAP escluso) non viene richiamato
169100140610       // fnlv14r (quest'ultimo, se cap vuoto richiama l'interrogazione
169200140609       // del cappario e controlla la nazione che qui non serve)
169300140609       //?Ragione sociale, Indirizzo, Località e Provincia --> obbligatori
169400140610          if fnlry00i1.newrsd = *blanks;
169500140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
169600140610             wIdMsg   = 'TIS0761';
169700140610             wIdCampo = 'NEWRSD    ';
169800140610             clear wParm;
169900140610             exsr Messaggi;
169901141006             leavesr;
170000140610          endif;
170100140610          if fnlry00i1.newind = *blanks;
170200140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
170300140610             wIdMsg   = 'TIS0762';
170400140610             wIdCampo = 'NEWIND    ';
170500140610             clear wParm;
170600140610             exsr Messaggi;
170601141006             leavesr;
170700140610          endif;
170800140610          if fnlry00i1.newcad = *blanks;
170900140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
171000140610             wIdMsg   = 'TIS0763';
171100140610             wIdCampo = 'NEWCAD    ';
171200140610             clear wParm;
171300140610             exsr Messaggi;
171301141006             leavesr;
171400140610          endif;
171500140610          if fnlry00i1.newlod = *blanks;
171600140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
171700140610             wIdMsg   = 'TIS0764';
171800140610             wIdCampo = 'NEWLOD    ';
171900140610             clear wParm;
172000140610             exsr Messaggi;
172001141006             leavesr;
172100140610          endif;
172200140610          if fnlry00i1.newprd = *blanks;
172300140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
172400140610             wIdMsg   = 'TIS0765';
172500140610             wIdCampo = 'NEWPRD    ';
172600140610             clear wParm;
172700140610             exsr Messaggi;
172701141006             leavesr;
172800140609          endif;
172900140610       //?La nazione deve essere vuota
173000140610          if fnlry00i1.newnzd <> *blanks;
173100140610             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
173200140610             wIdMsg   = 'TIS0384';
173300140610             wIdCampo = 'NEWNZD    ';
173400140610             clear wParm;
173500140610             exsr Messaggi;
173501141006             leavesr;
173600140610          endif;
173700140611       //?Richiamo pgm per controllo CAP e calcolo instradamento
173800140610          if fnlry00i1.newcad<>fnlry00i1.oldcad or
173900140610             fnlry00i1.newlod<>fnlry00i1.oldlod or
174000140610             fnlry00i1.newprd<>fnlry00i1.oldprd or
174100140610             fnlry00i1.newnzd<>fnlry00i1.oldnzd         ;
174200140611             exsr sr_call_fispe02r;
174201141003          else;
174202141003             if fnlry00i1.newrsd=fnlry00i1.oldrsd and
174203141003                fnlry00i1.newind=fnlry00i1.oldind;
174204141003       // Errore se non eseguita alcuna variazione di indirizzo
174205141003                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174206141003                wIdMsg   = 'TIS0794';
174207141003                wIdCampo = '*NOCHGIND ';
174208141003                clear wParm;
174209141003                exsr Messaggi;
174210141006                leavesr;
174211141003             endif;
174300140610       endif;
174301140930
174302140930         waltroind='1';
174303140930         exsr sr_orarisr ;
174304140930
174305141016       //?SE NO DIROTTAMENTO
174306141016       //if wolna = 0 ;
174307140923       //?DATA/ORA CONSEGNA RICHIESTA
174308140923       if fnlry00i1.newdtcrici<>*blanks and fnlry00i1.newdtcrici<>*zeros;
174309141006       // Se richiesto dirottamento non si può inserire la consegna richiesta
174310141016         if  wolna>0;
174311141016                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174312141016                wIdMsg   = 'TIS0795';
174313141016                wIdCampo = 'NEWDTCRICI';
174314141016                clear wParm;
174315141016                exsr Messaggi;
174316141016                leavesr;
174317141016         endif;
174318140923       // DATA CONSEGNA RICHIESTA:
174319140923       //  Controllo correttezza formale
174320140923         monitor  ;
174321140923         dataiso=%date(fnlry00i1.newdtcrici:  *eur)   ;
174322140923         on-error ;
174323140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174324140923             wIdMsg   = 'TIS0121';
174325140923             wIdCampo = 'NEWDTCRICI';
174326140923             clear wParm;
174327140923             exsr Messaggi;
174328140923             leavesr;
174329140923         endmon  ;
174330140923
174331140923         wdcr=%dec(dataiso);
174332141215       //  Per chkistruco considero "data oggi" la data in cui è stata
174333141215       //  fatta la  richiesta
174334141215         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO;
174335141215              wdataconfr= %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
174336141215         else;
174337141218              wdataconfr=dataoggi;
174338141215         endif;
174339140923
174340140923       //  Non inferiore o uguale a oggi
174341141215         if wdcr <= wdataconfr;
174342140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174343140929             wIdMsg   = 'TIS0759';
174344140923             wIdCampo = 'NEWDTCRICI';
174345140923             clear wparm;
174346140923             exsr Messaggi;
174347140923             leavesr;
174348140923         endif;
174349140923       //  Non superiore ai Giorni limite giac. con data cons.rich.
174350140923       //      Verifico se presenti limiti personalizzati per il cliente
174351140923
174352140923         exsr sr_PersGGiac;
174353140923       //Calcolo giorni lavorativi fra data odierna e data consegna richiesta
174354141215         datada=wdataconfr;
174355140923         dataa=wdcr;
174356140923         clear tfptfa;
174357140923         p_tfa =wfgslna;
174358140923         xsrlav8(wdat8:tfptfa);
174359140923         if ggl > wlimitdcr;
174360140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174361140923             wIdMsg   = 'TIS0745';
174362140923             wIdCampo = 'NEWDTCRICI';
174363140923             wParm=%char(%date(wdcr:*iso):*eur);
174364140923             exsr Messaggi;
174365140923             leavesr;
174366140923         endif;
174367140923       //  Non inferiore ad eventuale data consegna richiesta "DOPO IL" già presente
174368140923       //  Controllo anche con la data consegna richiesta originale
174369140923         if (arbtcr='D' and wdcr<=arbdcr) or
174370140923           (§AR5TCR='D' and §ar5dcr>*zeros and wdcr<=%dec(§ar5dcr:8:0));
174371140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174372140923             wIdMsg   = 'TIS0750';
174373140923             wIdCampo = 'NEWDTCRICI';
174374140923             if arbtcr='D' ;
174375140923             dataeur=%date(%dec(arbdcr:8:0):*iso);
174376140923             else;
174377140923             dataeur=%date(%dec(§ar5dcr:8:0):*iso);
174378140923             endif;
174379140923             wParm=%char(%date(wdcr:*iso):*eur)+%char(dataeur);
174380140923             exsr Messaggi;
174381140923             leavesr;
174382140923         endif;
174383140923       // Non deve cadere in giorno festivo
174384140923         chain (0:wfgslna:%subdt(%date(wdcr):*years):
174385140923                        %subdt(%date(wdcr):*MONTHS)) azcln01l;
174386140923         if %found(azcln01l) and mat(%subdt(%date(wdcr):*days)) = 'F'
174387140923                            or pom(%subdt(%date(wdcr):*days)) = 'F';
174388140923             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174389140923             wIdMsg   = 'TIS0758';
174390140923             wIdCampo = 'NEWDTCRICI';
174391140923             wParm=%char(%date(wdcr:*iso):*eur);
174392140923             exsr Messaggi;
174393140923             leavesr;
174394140923         endif;
174395140923       // ORA CONSEGNA RICHIESTA:
174396140923         if fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
174398140923       // Deve contenere solo numeri
174399140923           if %check(digits:fnlry00i1.neworcrici)>0;
174400140923                prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174401140923                wIdMsg   = 'TIS0746';
174402140923                wIdCampo = 'NEWORCRICH';
174403140923                wParm = %subst(fnlry00i1.neworcrici:1:2) + ':' +
174404140923                        %subst(fnlry00i1.neworcrici:3:2);
174405140923                exsr Messaggi;
174406140923           else;
174407140923       //  l'orario deve essere Incluso negli orari standard o min/max previsti per il cap/località
174408140923                wora = %dec(fnlry00i1.neworcrici:4:0);
174409140923                 if (§ivptipo='S' and (wora < OOR2STIS  or wora > OOR2STFS)) or
174410140923                    (§ivptipo='M' and (wora < OOR2MIIS  or wora > OOR2MXFS)) ;
174411140923                       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174412140923                       wIdMsg   = 'TIS0780';
174413140923                       wIdCampo = 'NEWORCRICI';
174414140925       //              wParm = fnlry00i1.newlod;
174415140923                       if §ivptipo='S';
174416140925                          wParm = fnlry00i1.newlod +
174417140925                                  %editw(oor2stis:'  :  ') +
174418140925                                  %editw(oor2stfs:'  :  ') ;
174419140923                       else;
174420140925                          wParm = fnlry00i1.newlod +
174421140925                                  %editw(oor2miis:'  :  ') +
174422140925                                 %editw(oor2mxfs:'  :  ') ;
174423140923                       endif;
174424140923                       exsr Messaggi;
174425140923                 endif;
174426140923           endif;
174427140923         endif;
174428140923       // PREFERENZA MATTINO/POMERIGGIO
174440140923         if fnlry00i1.prconsrici<>*blanks and
174441140923            fnlry00i1.prconsrici<>'M' and
174442140923            fnlry00i1.prconsrici<>'P'               ;
174443140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174444140923                    wIdMsg   = 'TIS0749';
174445140923                    wIdCampo = 'PRCONSRICI';
174446140923                    clear wParm;
174447140923                    exsr Messaggi;
174448140923         endif;
174449140923       // Verifico preferenza m/p con gli orari standard o min/max in base
174450140923       // all'ora limite del mattino
174451140923         if (fnlry00i1.prconsrici='P' and §ivptipo='S' and §ivporlm>=OOR2STFS)or
174452140923            (fnlry00i1.prconsrici='M' and §ivptipo='S' and §ivporlm<OOR2STIS) or
174453140923            (fnlry00i1.prconsrici='P' and §ivptipo='M' and §ivporlm>=OOR2mxFS)or
174454140923            (fnlry00i1.prconsrici='M' and §ivptipo='M' and §ivporlm<OOR2miIS) ;
174455140923            If fnlry00i1.prconsricI='P';
174456140923                widmsg='TIS0676';
174457140923            else;
174458140923                widmsg='TIS0667';
174459140923            endif;
174460140923            matpom=rtvMsgLang(widmsg : widlingua);
174461140923                      prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174462141006         if prmRqsOpCode <> FNLRY00_RQSOPCODE_CHK_ISTRUCO;
174463140923                      wIdMsg   = 'TIS0760';
174464140923                      wIdCampo = 'PRCONSRICI';
174465140923                      wParm = MatPom + wbolla;
174466141006         else;
174467141006                      wIdMsg   = 'TISA760';
174468141006                      wIdCampo = 'PRCONSRICI';
174469141006                      wParm = MatPom ;
174470141006         endif;
174471140923                      exsr Messaggi;
174472140923         ENDIF;
174473140923       // ORA CONSEGNA RICHIESTA In ALTERNATIVA A PREFERENZA M/P
174474140923         if fnlry00i1.prconsrici<>*blanks and
174475140923            fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros;
174476140923                    prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174477140923                    wIdMsg   = 'TIS0753';
174478140923                    wIdCampo = 'NEWORCRICI';
174479140923                    clear wParm;
174480140923                    exsr Messaggi;
174481140923                    wIdCampo = 'PRCONSRICI';
174482140923                    exsr Messaggi;
174483140923         endif;
174484141001         else;
174485141001       // se non immessa la data consegna richiesta errore se immessa l'ora o la preferenza M/P
174486141001           if fnlry00i1.prconsrici<>*blanks ;
174489141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174491141016              if  wolna>0;
174492141016                        wIdMsg   = 'TIS0795';
174493141016              else;
174494141001                        wIdMsg   = 'TIS0782';
174495141016              endif;
174496141001                        wIdCampo = 'PRCONSRICI';
174497141001                        clear wParm;
174498141001                        exsr Messaggi;
174499141001           endif;
174501141001           if  fnlry00i1.neworcrici<>*blanks and fnlry00i1.neworcrici<>*zeros ;
174502141001                        prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
174504141016              if  wolna>0;
174505141016                        wIdMsg   = 'TIS0795';
174506141016              else;
174507141001                        wIdMsg   = 'TIS0782';
174508141016              endif;
174509141001                        wIdCampo = 'NEWORCRICI';
174510141001                        clear wParm;
174511141001                        exsr Messaggi;
174512141001           endif;
174514141001         endif;
174515141016       //else;
174516141016       //?SE DIROTTAMENTO pulisco data/ora preferenza
174517141016       //   clear fnlry00i1.newdtcrici ;
174518141016       //   clear fnlry00i1.neworcrici ;
174519141016       //   clear fnlry00i1.prconsrici ;
174520141016       //endif;
174521140609       endsr;
174522140611       //--------------------------------------------------------------
174600140611       //?Richiamo stored procedure per controllo e calcolo instradamento
174700140611       //--------------------------------------------------------------
174800140611       BEGSR sr_Call_FISPE02R;
174801141006
174803141006
174900140611         clear  oTFA;
175000140611         clear  oLNA;
175100140611         clear  oZNC;
175200140611         clear  oLNPD;
175300140611         clear  oLNAD;
175400140611         clear  oERR;
175500140611         clear  oMSG;
175600140611         clear  oTAG;
175700140611         clear  RTNesito;
175800140611         clear  RTNopcode;
175900140611         clear  RTNstatus;
176000140611         iTYPE='C';
176100140611         iLANG=Widlingua;
176200140611         iDATA=dataoggi;
176300140611         clear iNTW ;
176400140611         iLNP=arblnp;
176500140611         iNZD=fnlry00i1.newnzd;
176600140611         iPRD=fnlry00i1.newprd;
176700140611         iCAD=fnlry00i1.newcad;
176800140611         iLOD=fnlry00i1.newlod;
176900140611         iIND=fnlry00i1.newind;
177000140611         iRSD=fnlry00i1.newrsd;
177100140611         iNCL=arbncl;
177200140611         iPKB=arbpkb;
177300140611         iVLB=arbvlb;
177400140611         iFFD=arbffd;
177500140611         iTSP=arbtsp;
177600140611         iTC1=arbtc1;
177700140611         iTC2=arbtc2;
177800140611         iCBO=arbcbo;
177900140611         fispE02r ( iTYPE :
178000140611                    iLANG :
178100140611                    iDATA :
178200140611                    iNTW :
178300140611                    iLNP :
178400140611                    iNZD :
178500140611                    iPRD :
178600140611                    iCAD :
178700140611                    iLOD :
178800140611                    iIND :
178900140611                    iRSD :
179000140611                    iNCL :
179100140611                    iPKB :
179200140611                    iVLB :
179300140611                    iFFD :
179400140611                    iTSP :
179500140611                    iTC1 :
179600140611                    iTC2 :
179700140611                    iCBO :
179800140611                    oTFA :
179900140611                    oLNA :
180000140611                    oZNC :
180100140611                    oLNPD :
180200140611                    oLNAD :
180300140611                    oERR :
180400140611                    oMSG :
180500140611                    oTAG :
180600140611                    RTNesito :
180700140611                    RTNopcode :
180800140611                    RTNstatus );
181000140925       //if RTNesito<0;
181100140925       //ELSe;
181200140611          if oERR<>*blanks;
181300140611             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
181400140611             clear wIdMsg;
181500140611             wTextMsg=oMSG;
181600140611             select;
181700140611             when  %scan('Provincia':otag)>0;
181800140611                wIdCampo = 'NEWPRD    ';
181900140611             when  %scan('Localita':otag)>0;
182000140611                wIdCampo = 'NEWLOD    ';
182100140611             other;
182200140611                wIdCampo = 'NEWCAD    ';
182300140611             endsl;
182400140611             clear wParm;
182500140611             exsr Messaggi;
182600140611       // Se non ci sono errori verifico che la linea di arrivo calcolata sia la
182700141006       // stessa della bolla --> se dirottamento non possono selezionare la data/ora
182800140611          else;
182900140611             if arblna<>olna;
183000141006       //       prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
183100141006       //       wIdMsg   = 'TIS0767';
183200141006       //       wIdCampo = '*CHGLNA   ';
183300141006       //       clear wParm;
183400141006       //       exsr Messaggi;
183401141006                Wolna=olna;
183500140611             endif;
183600140611          endif;
183700140925       //endif;
183800140611       endsr;
183900140610       //--------------------------------------------------------------
184000140610       //?Controllo Dati comuni a tutti i tipi di disposizioni
184100140610       //--------------------------------------------------------------
184200140610       BEGSR  sr_ds3A       ;
184300140610                   clear kkey;
184400140610                   clear ds3a;
184500140610                   %subst(kkey:1:2)=arbcbo;
184600140610                   chain (1:'3A':kkey) tabel00f ;
184700140610                   if %found(tabel00f);
184800140610                      ds3a=tbluni;
184900140610                   endif;
185000140610       endsr;
185100140523       //--------------------------------------------------------------
185200140523       //?Controllo Dati comuni a tutti i tipi di disposizioni
185300140523       //--------------------------------------------------------------
185400140523       BEGSR  sr_ctrUNI     ;
185500140609       //?Referente Consegna - obbligatorio
185600140530       if fnlry00i1.newrefcons=  *blanks;
185700140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
185800140530             wIdMsg   = 'TIS0754';
185900140530             wIdCampo = 'NEWREFCONS';
186000140530             clear wParm;
186100140530             exsr Messaggi;
186200140530       endif;
186300140530       //?Telefono Destinatario - obbligatorio
186400140530       if fnlry00i1.newtelcons=  *blanks;
186500140530             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
186600140530             wIdMsg   = 'TIS0755';
186700140530             wIdCampo = 'NEWTELCONS';
186800140530             clear wParm;
186900140530             exsr Messaggi;
187000140530       endif;
187100140523       //?Indirizzo e-mail
187200140528       if fnlry00i1.newdesmail<> *blanks;
187300140528          clear dsemail                        ;
187400140528          §emlindi = fnlry00i1.newdesmail;
187500140528          xemail (dsemail);
187600140528          if §emlerro <> '0';
187700140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
187800140528             wIdMsg   = 'TIS0732';
187900140528             wIdCampo = 'NEWDESMAIL';
188000140528             clear wParm;
188100140528             exsr Messaggi;
188200140528          endif;
188300140528       endif;
188400140523       //?N.Telefono per SMS
188500140528       if fnlry00i1.newdescell<> *blanks;
188600140528          pInCell = %trim(fnlry00i1.newdescell);
188700140528          pOutErr = *blanks;
188800140528          if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
188900140528           // Numero di cellulare KO
189000140528             prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
189100140528             wIdMsg   = 'TIS0725';
189200140528             wIdCampo = 'NEWDESCELL';
189300140528             clear wParm;
189400140528             exsr Messaggi;
189500140528          endif;
189600140528       endif;
189700140523       endsr;
189800140529       //--------------------------------------------------------------
189900140529       //?Routine per recuperare flag di autorizzazione al Fermo Deposito
190000140529       //--------------------------------------------------------------
190100140529       BEGSR  sr_tab7r;
190200140529            clear kkey;
190300140529            clear ds7r;
190400140529            %subst(kkey:1:2)=arbgma;
190500140529            chain (1:'7R':kkey) tabel00f ;
190600140529            if %found(tabel00f);
190700140529               ds7r=tbluni;
190800140529            endif;
190900140529       endsr;
191000140528      /end-free
191100140521      **************************************************************************
191200140528     c     sr_OrariSr    BEGSR
191300140521
191400140521     c                   clear                   trulorSds
191500140522      /free
191600140522              // reperisco gli orari di consegna previsti
191700140522              clear tnsd99ds;
191800140522              d98tbo='A';
191900140522              d98aas=arbaas;
192000140522              d98lnp=arblnp;
192100140522              d98nrs=arbnrs;
192200140522              d98nsp=arbnsp;
192300140522              if arbtsp='D';
192400140522                 I98TSP_FOR='C';
192500140522              endif;
192600140522              tnsd99r(tnsd99ds);
192700140522              if d98err='0';
192800140522                IOREDTA = d98dci;
192900140522              endif;
193000140522      /end-free
193100140521     c                   eval       IOREtfp = ARBtfp
193200140521     c                   eval       IOREtfa = ARBtfa
193201150107     c                   if         arbdti>0
193300140521     c                   eval       IOREdti = ARBdti
193301150107     c                   else
193302150107     c* Caso di consegna parziale:
193303150107     c* Visto che vogliamo prendere gli orari "tutto il giorno", se dti vuoto non lo
193304150107     c* prenderebbe mai e qunidi metto ARBDAM
193305150107     c                   eval       IOREdti = ARBdam
193306150107     c                   endif
193400140528     c* imposto a 0 hti per recuperare sempre gli orari ' '=Tutto il giorno
193500140522     c****               eval       IOREhti = ARBhti
193600140521     c                   eval       IOREdcr = ARBdcr
193700140521     c                   eval       IOREhcr = ARBhcr
193800140521     c                   eval       IOREtcr = ARBtcr
193900140522     c                   eval       IOREDEI = d98dei
194000140522     c                   eval       IOREOEI = d98oei
194001150729
194002150729     c                   clear                   diore01
194003150729     c                   eval       §IORETRAZC=%editc(d98ttc:'X')
194004150729     c                   eval       §IORECONSC=%editc(d98tcc:'X')
194005150729     c                   eval       ioreflo=diore01
194006150729
194100140521     c                   eval       IOREfil = ARBlna
194101140923     c                   if         waltroind=' '
194200140521     c                   eval       IOREcap = ARBcad
194300140521     c                   eval       IOREloc = ARBlod
194400140521     c                   eval       IOREnar = ARBnzd
194401140923     c                   else
194402140925     c                   eval       IOREcap = fnlry00i1.newcad
194403140925     c                   eval       IOREloc = fnlry00i1.newlod
194404140925     c                   eval       IOREnar = fnlry00i1.newnzd
194405140923     c                   endif
194500140521     c
194600140521     c                   eval       IOREtser = 'C'
194700140521     c                   eval       IOREDDC=arbddc
194800140521     c                   eval       IOREnDC=arbndc
194900140521     c
195000140521     c
195100140521     c                   eval       IOREtsp = ARBtsp
195200140618     c                   if          ARBfbc = *blanks and
195300140618     c                               ARBddc > 0 and
195400140618     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
195500140618     c                               arbica='R' or arbicc=' ' or arbicc='R')
195600140618     c                   eval       IOREcons = 'S'
195700140618     c                   endif
195800140521     c
195900140521
196000140521     c                   call      'TRULORSR'
196100140521     c                   parm                    kpjba
196200140521     c                   parm                    trulorSds
196300140521     c                   parm                    trulor2ds
196400140521     c
196500140521     c                   ENDSR
196600140521      /free
196601140924       //--------------------------------------------------------------
196602140924       //?Routine per determinare il giorno della settimana di dataoggi + 1gg lav.
196603140924       //--------------------------------------------------------------
196604140924       BEGSR  sr_wdesgio ;
196605140924       //       Determino il giorno della settimana di dataoggi+1giorno lavorativo
196606140924       //       da restituire nel messaggio
196607140924               wdata=dataoggi;
196608140924       //       Incremento la data di un giorno
196609140924               exsr sr_incredata;
196610140924       //       Verifico se data festiva e in tal caso incremento finchè non trovo giorno
196611140924       //       lavorativo
196612140924               exsr sr_datafes;
196613140924       //       Determino il giorno della settimana della data calcolata
196614140924               dataiso=%date(wdata:*iso);
196615140924               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
196616140924               wdesgio=rtvMsgLang(dayOfWeek.msgId(dw) : widlingua);
196617140924       endsr;
196700140603       //--------------------------------------------------------------
196800140603       //?Routine per scrittura disposizioni di consegna
196900140603       //--------------------------------------------------------------
197000140924       BEGSR  SR_Tiidc;
197100140603       // Per la spedizione non devono esserci disposizioni ancora da elaborare
197200140603       exsr sr_ChkDispo;
197300140603       clear tiidc000;
197301141013       clear tiidcds ;
197400140718       clear tivpi000;
197500140604       exsr repnum;
197600140604       if esito = *blanks;
197700140604          IDCPRG=kprg                         ;
197800140604       else;
197900140619       // prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
198000140619  //   // wIdMsg   = 'TIS0757';
198100140619  //   // wIdCampo = '*REPNUM   ';
198200140619  //   // clear wParm;
198300140619  //   // exsr Messaggi;
198400140616          prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
198500140616          prmRpyIdMsg  = FNLRY00_ESITO_NUMERATORE_DISPO_NONREPERITO;
198600140604          exsr RoutEnd;
198700140604       endif;
198800141009       //WDCINSDATA = %dec(fnlry00i1.ksutimest);
198900141009       //IDCINSDATA = %dec(%subst(%editc(wdcinsdata:'X'):1:14):14:0);
198901141009       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
198902141009                                    : 1 : 14 ) );
198903141009       IDCtimestn   = fnlry00i1.ksutimestn;
199000140603       //IDCMODACC                           ;
199100140610       if wksu<>*blanks;
199200140610       IDCMODACC='L'                       ;
199300140610       else;
199400140610       endif;
199500140603       IDCIP = fnlry00I1.cltipaddr;
199600140603       IDCIDCLI = fnlry00i1.ksu;
199700140603       IDCTIPOCLI= Wchi;
199800140603       IDCWAAS   = kaas;
199900140603       IDCWLNP   = klnp;
200000140603       IDCWNRS   = knrs;
200100140603       IDCWNSP   = knsp;
200200140620       IDCAAS    = arbaas;
200300140620       IDCLNP    = arblnp;
200400140620       IDCNRS    = arbnrs;
200500140620       IDCNSP    = arbnsp;
200600140603       IDCFGS    = wfgslna;
200700140603       IDCTIPODIS = fnlry00i1.tipodispo;
200800140604       IDCMAILAVV = fnlry00i1.newdesmail;
200900140604       IDCTELAVV  = fnlry00i1.newdescell;
201000140603       //IDCALERT
201100140604       if fnlry00i1.newdtcrich <> *blanks and fnlry00i1.tipodispo='1';
201200140604           IDCDCR =  %dec(%date(fnlry00i1.newdtcrich:*eur):*iso);
201300140604       endif;
201400140604       if fnlry00i1.neworcrich <> *blanks and fnlry00i1.tipodispo='1';
201500140604           IDCHCR =  %dec(fnlry00i1.neworcrich:4:0);
201600140604       endif;
201601140924       if fnlry00i1.newdtcrici <> *blanks and fnlry00i1.tipodispo='3';
201602140924           IDCDCR =  %dec(%date(fnlry00i1.newdtcrici:*eur):*iso);
201603140924       endif;
201604140924       if fnlry00i1.neworcrici <> *blanks and fnlry00i1.tipodispo='3';
201605140924           IDCHCR =  %dec(fnlry00i1.neworcrici:4:0);
201606140924       endif;
201700140603       //IDCTCR                         ;
201701140924       if fnlry00i1.tipodispo='1';
201800140924          IDCPRCR=  fnlry00i1.prconsric  ;
201801140924       endif;
201802140924       if fnlry00i1.tipodispo='3';
201803140924          IDCPRCR=  fnlry00i1.prconsrici ;
201804140924       endif;
201900140603       IDCREF  = fnlry00i1.newrefcons ;
202000140603       IDCTELD = fnlry00i1.newtelcons ;
202100140603       if fnlry00i1.tipodispo='2';
202200140603          IDCFFD  = 'S';
202300140603       endif;
202400140611       if fnlry00i1.tipodispo='3';
202501141007          wtxt     = fnlry00i1.newrsd;
202502141007          uppercase='1';
202503141007          exsr sr_chk;
202504141007          IDCRSD   = wtxt;
202505141007          wtxt     = fnlry00i1.newind;
202506141007          uppercase='1';
202507141007          exsr sr_chk;
202600141007          IDCIND   = wtxt            ;
202700140611          IDCCAD   = fnlry00i1.newcad;
202701141007          wtxt     = fnlry00i1.newlod;
202702141007          uppercase='1';
202703141007          exsr sr_chk;
202704141007          exsr chkstringa;
202800141007          IDCLOD   = wtxt            ;
202900140611          IDCPRD   = fnlry00i1.newprd;
203000140611          IDCNAZ   = fnlry00i1.newnzd;
203001141006       // bolla da dirottare
203002141006          if wolna>0;
203003141006               IDCNEWLNA=wolna;
203004141006               IDCFLA   ='S';
203005141006          endif;
203100140611       endif;
203200140611       if fnlry00i1.tipodispo='4';
203300140611          IDCNOTE  = fnlry00i1.altro;
203400140611       endif;
203500140603       IDCELAFLG='S';
203600140603
203700140718       VPITIP='TI';
203800140718       VPIISV='TT';
203900140718       // indicazioni ricevute da cliente loggato
204000140718       if wksu<>*blanks;
204100140801          VPISUN=%editc(fnlry00i1.sun:'X');
204200140718          VPIKSU=wksu    ;
204300140718       else;
204400140718       // indicazioni ricevute mediante BRTCODE
204500140718          VPISUN='8IDC00000';
204600140718          VPIKSU='0IDC0000';
204700140718       endif;
204800140718       VPIDTA=tiidcds ;
204900140718       write tivpi000;
205000140814       // se AS888 scrivo anche tiidc
205100140814
205200140814      /end-free
205300140814     c* controllo se sono in AS888
205400141006     c                   eval      comman=chklib(1)
205500141006     c                   eval      lengh=35
205600141006     c                   call      'QCMDEXC'                            99
205700141006     c                   parm                    comman           80
205800141006     c                   parm                    lengh            15 5
205900140814     c
206000141006     c                   if        not *in99
206100140814      /free
206200140814
206300141013         write tiidc000;
206400141006         endif  ;
206500140603
206600140603       endsr;
206700140603       //--------------------------------------------------------------
206800140603       //?Verifica presenza di diposizioni ancora da elaborare
206900140603       //--------------------------------------------------------------
207000140603       BEGSR  SR_ChkDispo;
207001140908       clear w0140;
207100140908       setll (arbaas:arblnp:arbnrs:arbnsp:w0140) tiidc01l;
207200140603       if %equal(tiidc01l);
207300140603              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
207400140603              wIdMsg   = 'TIS0756';
207500140603              wIdCampo = '*TIIDC    ';
207600140611              wParm = wbolla;
207700140603              exsr Messaggi;
207800140603              exsr Routend;
207900140603       endif;
208000140718       // verifico anche che non siano ancora su tivpi
208100140801       eval wkeyspa=%editc(arbaas:'X')+%editc(arblnp:'X')+
208200140801                    %editc(arbnrs:'X')+%editc(arbnsp:'X')  ;
208300140801      /end-free
208400140718     C/EXEC SQL
208500140718     C+ SELECT count(*) INTO :totale FROM tivpi00f where
208600140718     c+ VPITIP='TI' and VPIISV='TT' and
208700141002     C+ substr(vpidta, 67, 16) = :wkeyspa
208800140718     C/END-EXEC
208900140801      /free
209000140718          if totale>0 ;
209100140718              prmRpyOpCode = FNLRY00_RPYOPCODE_ERROR;
209200140718              wIdMsg   = 'TIS0756';
209300140718              wIdCampo = '*TIVPI    ';
209400140718              wParm = wbolla;
209500140718              exsr Messaggi;
209600140718              exsr Routend;
209700140718       endif;
209800140603       endsr;
209900140604        //-----------------------------------------------------------------------*
210000140604        // Reperimento progressivo richiesta da numeratore 650
210100140604        //-----------------------------------------------------------------------*
210200140604        begsr RepNum;
210300140604
210400140604          clear esito;
210500140604          $finenum=*off;
210600140604          clear trul33ds;
210700140604
210800140604          I33TLa = 'L';
210900140604          I33Ope = *ZEROS;
211000140604          I33CNu = 650;
211100140604          I33Num = 1;
211200140604
211300140604          KPJBU = TRUL33DS;
211400140604
211500140604          dow $finenum=*off   ;
211600140604             Trul33R(kpjba);
211700140604
211800140604             TRUL33DS = KPJBU;
211900140604
212000140604             if O33Err <> *zeros;
212100140604               Esito = '2';
212200140604               $finenum=*on;
212300140604             else;
212400140908               w0090=o33nri;
212500140908               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
212600140604               setll kprg tiidc02l;
212700140604        // Se numero già presente nel file TIIDC ne cerco un altro
212800140604               if not %equal;
212900140604                  $finenum=*on;
213000140604               endif;
213100140604             endif;
213200140604          enddo;
213300140604
213400140604          endsr;
213500140604
213600140708       //--------------------------------------------------------------
213700140708       //?Routine per schierare i messaggi nella ds FNLRY00M0
213800140708       //--------------------------------------------------------------
213900140708       BEGSR  sr_RepPct;
214000140708
214100140708       clear wtentacons;
214200140708
214300140708      /end-free
214400140708     c                   move      arbndc        pctndc
214500140708     c                   move      arbifp        pctfgs
214600140708     c                   move      'CET'         ttrd
214700140708     c     kcet          setgt     fipct02l
214800140708     c     kcet          readpe    fipct02l
214900140708     c                   if        not %eof(fipct02l)
215000140708     c                   movel     pctdati       fipctcetds
215100140708     c* consegnata
215200140708     c                   if        §pctcmc = ' '
215300140708     c                   eval      prmRpyOpCode=FNLRY00_RPYOPCODE_ERROR
215301141006     c                   if        prmRqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO
215302141006     c                   eval      wIdMsg   = 'TISA738'
215303141006     c                   eval      wIdCampo = '*BOLLA    '
215304141006     c                   clear                   wbolla
215305141006     c                   else
215400140708     c                   eval      wIdMsg   = 'TIS0738'
215500140708     c                   eval      wIdCampo = '*BOLLA    '
215600140708     c                   eval      wParm = wbolla
215601141006     c                   endif
215700140708     c                   exsr      Messaggi
215800140708     c                   exsr      routend
215900140708     c                   else
216000140708       //?deve essere già stato fatto almeno un tentativo di consegna
216100140708     c                   eval      wtentacons='1'
216200140708     c                   endif
216300140708     c                   endif
216400140708      /free
216500140708       endsr;
216501140929       //--------------------------------------------------------------
216502140929       //?Routine per tradurre mattino/pomeriggio
216503140929       //--------------------------------------------------------------
216504140929       BEGSR  sr_matpom;
216505140929
216506140929            If wconsric='P';
216507140929                widmsg='TIS0676';
216508140929            else;
216509140929                widmsg='TIS0667';
216510140929            endif;
216511140929            matpom=rtvMsgLang(widmsg : widlingua);
216512140929       endsr;
216600140604
216700140527       //--------------------------------------------------------------
216800140527       //?Routine per schierare i messaggi nella ds FNLRY00M0
216900140527       //--------------------------------------------------------------
217000140527       BEGSR  Messaggi;
217100140527
217200140527       //?Se ho già caricato 99 messaggi esco
217300140527         IF  fnlry00M0.nrmsg = 99;
217400140527           exsr RoutEnd;
217500140527           clear fnlry00M0.nrmsg;
217600140527         ENDIF;
217700140527
217800140527         fnlry00M0.nrmsg += 1;
217900140527         fnlry00M0.skIdMsg(fnlry00M0.nrmsg) = wIdMsg;
218000140527         fnlry00M0.skIdCampo(fnlry00M0.nrmsg) = wIdCampo;
218100140527         fnlry00M0.skErrWarn(fnlry00M0.nrmsg) = FNLRY00_MSG_ERRORE;
218200140527         IF  wParm <> *blanks;
218300140527           fnlry00M0.skTextMsg (fnlry00M0.nrmsg) =
218400140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
218500140527         ELSE;
218600140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
218700140611           if wIdMsg=*blanks;
218800140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=wTextMsg;
218900140611           else;
219000140611             fnlry00M0.skTextMsg (fnlry00M0.nrmsg)=
219100140611             rtvMsgLang(wIdMsg:wIdlingua);
219200140611           endif;
219300140527         ENDIF;
219400140527
219500140527       ENDSR;
219600140527
219700140521       //--------------------------------------------------------------
219800140408       //?Carico tabelle.
219900140408       //--------------------------------------------------------------
220000140408       BEGSR  CaricaTab;
220100140408
220200140408
220300140408       ENDSR;
220400131010
220500131010       //--------------------------------------------------------------
220600131010       //?Operazioni finali.
220700131010       //--------------------------------------------------------------
220800131010       BEGSR  RoutEnd;
220900140408
221000140519       //?Se richiamato per GET_ISTRUCO
221100140521         IF  prmRqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO;
221200140519           %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o0;
221300140408         ELSE;
221400131010
221500140522       //?Se richiamato per PUT_ISTRUCO
221600140522          %subst(prmRpyData:1:prmRpyDataSize) = fnlry00o1;
221700140521       //%subst(prmRpyForzatu:1:prmRpyForSize) = fnlry0F0;
221800140408
221900140408         ENDIF;
222000140530
222100140530          %subst(prmRpyMessage:1:prmRpyMsgSize) = fnlrY00M0;
222200131010
222300131010         return;
222400131010
222500131010       ENDSR;
222600131009
222700131007      /end-free
222701141007      *---------------------------------------------------------
222702141007      * Controllo i caratteri dei campi alfanumerici
222703141007      *---------------------------------------------------------
222704141007     c     Sr_Chk        BegSr
222705141007
222706141007     c                   clear                   esito
222707141007     c                   Eval      TxtInOut = wtxt
222708141007     c                   Call      'XCHKCHAR'
222709141007     c                   Parm                    TxtInOut
222710141007     c                   Parm                    ElencoChar
222711141007     c                   Parm                    TipoElenco
222712141007     c                   Parm                    CharSost
222713141007     c                   Parm                    UpperCase
222714141007     c                   Parm                    ChkNull
222715141007     c                   Parm                    CharNull
222716141007     c                   Parm                    Esito
222717141007     c                   If        Esito = '1'
222718141007     c                   Move      TxtInOut      wtxt
222719141007     c                   EndIf
222720141007
222721141007     c                   EndSr
222722141007      *---------------------------------------------------------
222723141007      * Controllo caratteri di punteggiatura precedenti e seguenti
222724141007      *---------------------------------------------------------
222725141007     c     chkstringa    BegSr
222726141007
222727141007     c* trovo la prima posizione "buona" da utilizzare
222728141007     c                   eval      posl=%check(charnook:wtxt)
222729141007     c* trovo l'ultima posizione "buona" da utilizzare (escludo il ".")
222730141007     c                   eval      posr=%checkr(charnookf:wtxt)
222731141007     c                   if        posl=0
222732141007     c                   clear                   wtxt
222733141007     c                   else
222734141007     c* determino la lunghezza della stringa "buona"
222735141007     c                   eval      lung=(posr-posl)+1
222736141007     c                   eval      wtxt=%subst(wtxt:posl:lung)
222737141007     c                   endif
222738141007
222739141007     c                   EndSr
222800140814**
222900141006CHKOBJ OBJ(FILTRAPRD) OBJTYPE(*LIB)
