000100960530      ***********************************************************************
000200130725      *   ESPORTA DATI BOLLE x DPD oltre gli SCAN  ESTERO    Export         *
000300130725      *     ?   - NUOVO TRACCIATO DPD -    ?
000400960530      ***********************************************************************
000500891004     H DECEDIT('0,') DATEDIT(*DMY/)
000600000207      *---------------------------------------------------------------------*
000700140311     FFNARB01L  iF   E           K DISK    Commit
000800171018     FfiPND01L  IF   E           K DISK
000900060213     Ffiar401L  IF   E           K DISK
001000031210     Ffiar501L  IF   E           K DISK
001100960530     FTABEL00F  IF   E           K DISK
001200170904     FTntbe01l  IF   E           K DISK
001300020219     FAzOrg01L  IF   E           K DISK
001400130725      ***
001500130826     ftivgd00f  if a E             DISK    commit
001600130826      *
001700130826     Ftivir02L  IF   E           K DISK
001800130826     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001900130826     F                                     PREFIX(f_) usropn
002000140617      *---------------------------------------------------------------------*
002100140617      **  Flag di attivazione
002200140617      *---------------------------------------------------------------------*
002300140617      * il servizio mail di alert è predisposto
002400140617      *    ma NON attivato poichè adesso impostato a (N) --> non decodifica i dati
002500140617      *    né esegue la scrittura del record "MSG" xchè con DPD non è attivo il servizio.
002600140617      *
002700140617     d  serv_mail_Attivato...
002800150617     d                 s              1    INZ('S')
002900140617      **
003000000207      *---------------------------------------------------------------------*
003100000207      * Schiere
003200000207      *---------------------------------------------------------------------*
003300020220     D Num             S              1    DIM(11)
003400000207      * Codici ISO nazioni x E.D.I.
003500011217     D C15             S              3    DIM(500)
003600011217     D ISO             S              3    DIM(500)
003700130828     D CIE             S              3  0 DIM(500)
003800020219     D DNZ             S             25    DIM(500)
003900020412     D ACN             S              1    DIM(500)
004000140611     D PRF             S              4    DIM(500)
004100000207      *---------------------------------------------------------------------*
004200000207      * DS
004300000207      *---------------------------------------------------------------------*
004400171018     D**** DSBL4i        E DS
004500031210     D dar5gen       e ds
004600130827     D dar5emd       e ds
004700020227     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
004800960530     D DS3A          E DS
004900030521     D DSQT1         E DS                  INZ
005000960822     D DS15          E DS
005100130828     D DDPP          E DS
005200950428     D KPJBA         E DS
005300171010      *------
005400171010      * per decodificare il DEPOT i Partenza mediante la LNP
005500171010     D OG143         E DS
005600171010     D ds3IDP        E DS
005700171010      *------
005800130827      *
005900130828     D BOLLA16         DS                  INZ
006000130827     d  anno                          4
006100130827     d  linea                         3
006200130827     d  serie                         2
006300130827     d  numero                        7
006400130827      *
006500960530     D WLBDA8          DS                  INZ
006600950428     D  G02DAT                 1      8  0
006700950428     D  G02INV                 9     16  0
006800950428     D  G02ERR                17     17
006900950428     D  G02TGI                18     22  0
007000130826     d  KAAS           s                   like(ARBAAS)
007100130826     d  KLNP           s                   like(ARBLNP)
007200130826     d  KNRS           s                   like(ARBNRS)
007300130826     d  KNSP           s                   like(ARBNSP)
007400130826     d  KTRC           s                   like(AR4TRC)
007500130826     d  KKUT           s                   like(TBLKUT)
007600130826     d  KCOD           s                   like(TBLCOD)
007700130826     d  KKEY           s                   like(TBLKEY)
007800130828     D Var_ISV         s                   like(Virfi1)
007900130828     d  fldRSM         s                   like(orgDES)                            fldDES
008000130828     d  fldINM         s                   like(orgIND)                            fldIND
008100130828     d  fldCPM         s              5  0
008200130828     d  fldLOM         s                   like(orgLOC)                            fldLOC
008300130828     d  fldPRM         s                   like(orgPRO)                            fldPRO
008400130828     d  fldTLM         s                   like(orgTEL)                            fldTEL
008500130828     d  fldFXM         s                   like(orgFAX)                            fldFAX
008600130827     d  fldRSD         s                   like(arbRSD)
008700130827     d  fldRSD2        s                   like(ar4NOT)
008800130827     d  fldIND         s                   like(arbIND)
008900130828     d  fldCAD         s                   like(arbCAD)
009000130827     d  fldLOD         s                   like(arbLOD)
009100130827     d  fldPRD         s                   like(arbPRD)
009200130827     d  fldCTA         s                   like(§AR5REF)
009300130827     d  fldTEL         s                   like(§ar5teld)
009400140611     d  fldTELcontact  s             30A
009500140611     d  fldPREF        s              4A
009600130827     d  fldEML         s                   like(§ar5EML)
009700130827     d  fldSMS         s                   like(§ar5TEL)
009800170822     d  fldEML1        s                   like(§ar5EML)
009900140617     d  fldSMScontact  s             30A
010000150218     d  fldNOTE1       s                   like(ar4NOT)
010100150218     d  fldNOTE2       s                   like(ar4NOT)
010200171018     d  fldPARCEL      s                   like(pndIPN)
010300171018     d  fldISC         s                   like(pndISC)
010400171018     d  fldCHKdgt      s              1
010500171018     d DPNDDP4       e ds
010600171018     d DPNDDP5       e ds
010700130827      **
010800130827     d  WTARA          s                   like(§qttpc)
010900130826     d  wvol           s                   like(arbvlb)
011000130826     d  wpes           s                   like(arbpkb)
011100130827     d  wpes100        s              9  0
011200130827      **
011300130826     D dwlprg          s             10    INZ(*all'0')
011400130826     D wrkprg          s              8  0 INZ(*zeros)
011500130827      **
011600130826     d trul47ds      e ds
011700130826     D W0140           S             14  0 inz
011800130826     D Segmento        s           2048
011900130826     D Conta_righe     s              5i 0
012000130826     d  esito          s              1
012100130826     d lunghezza_campo...
012200130826     d                 s              3i 0
012300130826     D*  Definisco costanti
012400130826     D TipoF           C                   CONST('$M')
012500130826     D CLIENTE         C                   CONST('0DPD0OUT')
012600001222     D digits          C                   CONST('0123456789')
012700130828     d Normal_Parcel   c                   101
012800130828     d Small_Parcel    c                   136
012900130828     d Return          c                   300
013000071213      *
013100020222     C*---------------------------------------------------------------------*
013200960530     C* Ciclo principale                                                   -*
013300960530     C*---------------------------------------------------------------------*
013400010426     C     *ENTRY        PLIST
013500010426     C                   PARM                    KPJBA
013600130826     C                   PARM                    flagHDE           1
013700130826      **
013800130826      *  il pgm viene chiamato 3 volte x gestire la scrittura della testata
013900130826      *                                          dei singoli dettagli
014000130826      *                                          e per chiudere il file con #END
014100130826     c                   seton                                        RT
014200130826     c                   setoff                                         LR
014300130826     c                   clear                   segmento
014400130826      **
014500130826      **  flag di testata / dettaglio / end
014600130826      * apre il TIVGD
014700130826      *  scrive la testata e la prima riga di dettaglio
014800130826     c                   if               flagHDE = 'H'
014900130826     C                   EXSR      vgd_OPEN
015000130827     c                   exsr      INIZIO_MSG
015100130826      **
015200130827      * scrive il dettaglio
015300130826     c                   elseif           flagHDE = 'D'
015400130827     c                   exsr      SPEDIZIONE
015500130826      **
015600130826      * quindi chiude il TIVGD quando il chiamante è andato in EoF
015700130826     c                   elseif           flagHDE = 'E'
015800130827     c                   exsr      FINE_MSG
015900130826     C                   EXSR      vgd_CLOSE
016000130826      **
016100130826     c                   COMMIT
016200130826      **
016300130826      * chiude definitivamente dopo aver Committato
016400130826     c                   seton                                        LR
016500130826     c                   setoff                                         RT
016600130826     c                   end
016700130826      **
016800130826     C                   Return
016900130826     C*--------------------------------------------------------------*
017000130826     C* scrive la TESTATA
017100130826     C*--------------------------------------------------------------*
017200130827     C     INIZIO_MSG    BEGSR
017300130826     C*
017400140423     c                   eval      segmento = '#FILE;GEODATA_SHPNOT_0820_DRK_D'
017500140423     c                             + %editc(WOGGI:'X') +'T'+ %editc(WORA:'X')
017600140307     c                             + ';'
017700130826     c                   exsr      VGD_WRITE
017800130826     C*
017900140307     c                   eval      segmento = '#ENCODING;ISO 8859-1;'
018000130826     c                   exsr      VGD_WRITE
018100130826     C*
018200170926     c                   eval      segmento = '#VERSION;03.10;'
018300130826     c                   exsr      VGD_WRITE
018400130826     C*
018500140307     c                   eval      segmento = '#DEF;GEODATA:HEADER;VERSION;'+
018600140307     c                             'CLASSIFICATION;;'
018700130826     c                   exsr      VGD_WRITE
018800130826     C*
018900140307     c                   eval      segmento = '#DEF;GEODATA:SHIPMENT;NUMORDER;'+
019000130828     c                             'MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MPSCRE'+
019100130828     c                             'F1;MPSCREF2;MPSCREF3;MPSCREF4;MPSBILLREF;M'+
019200130828     c                             'PSCOUNT;MPSVOLUME;MPSWEIGHT;SDEPOT;CDATE;C'+
019300130828     c                             'TIME;HARDWARE;RDEPOT;DSORT;SPTDATE;SPTTIME'+
019400130828     c                             ';SPTREALDATE;SPTREALTIME;DELMODALLOW;ROUTI'+
019500170901     c                             'NGPLANVERSION;SPARTNERREF1;SPARTNERREF2;SP'+
019600170901     c                             'ARTNERCODE;OSORT;SSORT;MSORT;COLREQUESTFLA'+
019700170901     c                             'G;ROUTINGPLACE;;'
019800130826     c                   exsr      VGD_WRITE
019900130826     C*
020000140307     c                   eval      segmento = '#DEF;GEODATA:SENDER;NUMORDER;'+
020100130828     c                             'SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPNAME'+
020200130828     c                             ';SUNIQCUSTID;SNAME1;SNAME2;SSTREET;SPROPNU'+
020300130828     c                             'M;SADD2;SADD3;SFLOOR;SBUILDING;SDEPARTMENT'+
020400130828     c                             ';SCOUNTRYCODE;SSTATE;SZIPCODE;STOWN;SCONTA'+
020500170901     c                             'CT;SPHONE;SFAX;SEMAIL;SCOMMENT;SGLN;SGPSLA'+
020600170901     c                             'T;SGPSLONG;SACCOUNTOWNER;;'
020700130826     c                   exsr      VGD_WRITE
020800130827     C*
020900140307     c                   eval      segmento = '#DEF;GEODATA:RECEIVER;NUMORDER;'+
021000130828     c                             'RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPNAME2'+
021100130828     c                             ';RPUDOID;RSTREET;RPROPNUM;RADD2;RADD3;RFLO'+
021200130828     c                             'OR;RBUILDING;RDEPARTMENT;RCOUNTRYCODE;RSTA'+
021300130828     c                             'TE;RZIPCODE;RTOWN;RDOORCODE;RCONTACT;RCONT'+
021400130828     c                             'ACTPHO1;RCONTACTPHO2;RINTERPHONENAME;RFAX;'+
021500170901     c                             'REMAIL;RCOMMENT;RGPSLAT;RGPSLONG;;'
021600130827     c                   exsr      VGD_WRITE
021700130827      *
021800140307     c                   eval      segmento = '#DEF;GEODATA:PARCEL;NUMORDER;'+
021900130828     c                             'PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRANK;'+
022000130828     c                             'SENDPARCELREF1;SENDPARCELREF2;SENDPARCELRE'+
022100130828     c                             'F3;SENDPARCELREF4;RECPARCELREF;SERVICECODE'+
022200170901     c                             ';PPARTNERREF1;PPARTNERREF2;PPARTNERREF3;PP'+
022300170901     c                             'ARTNERREF4;DIMENSION;DECLAREDWEIGHT;MEASUR'+
022400170901     c                             'EDWEIGHT;HINSAMOUNT;HINSCURRENCY;HINSCONTE'+
022500170901     c                             'NT;HAZLQ;HZDPACKCODE;OPCODE;PCONTENT;ORIGI'+
022600170901     c                             'NPARCELNUMBER;POWNERBU;PPARTNERCODE;ASCODE'+
022700170901     c                             ';BAGNO;;'
022800130827     c                   exsr      VGD_WRITE
022900130827      *
023000140307     c                   eval      segmento = '#DEF;GEODATA:MSG;NUMORDER;'+
023100130828     c                             'NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE'+
023200130828     c                             ';MSGDESTTYPE;MSGDESTINATION;MSGTRIGGER;MSG'+
023300140307     c                             'LANG;MSGSENDERURL;;'
023400130827     c                   exsr      VGD_WRITE
023500130827      *
023600170926     c                   eval      segmento = 'HEADER;03.10;SHPNOT;'
023700130827     c                   exsr      VGD_WRITE
023800130827     C*
023900130826     C                   endSR
024000130826     C*--------------------------------------------------------------*
024100130826     C* scrive la CODA
024200130826     C*--------------------------------------------------------------*
024300130827     C     FINE_MSG      BEGSR
024400130826     C*
024500130826     c                   eval      segmento = '#END;'
024600130826     c                   exsr      VGD_WRITE
024700130826     C*
024800130826     C                   endSR
024900960530     C*--------------------------------------------------------------*
025000020219     C* dettaglio spedizione
025100960530     C*--------------------------------------------------------------*
025200130827     C     SPEDIZIONE    BEGSR
025300020227     C*
025400130826     C                   MOVEL     KPJBU         TRTC831
025500130826     C*
025600130826     C*  Se c'è numero spedizione scrivo dettaglio
025700130826    2C     F83NSP        IFNE      0
025800130826     C*
025900130826     C*  Eseguo posizionamento su archivio bolle per passaggio param.
026000130827      * B O L L A
026100130826     C                   Z-ADD     F83AAS        KAAS
026200130826     C                   Z-ADD     F83LNP        KLNP
026300130826     C                   Z-ADD     F83NRS        KNRS
026400130826     C                   Z-ADD     F83NSP        KNSP
026500130826     C     KARB          CHAIN     FNARB01L                           31
026600130827    3C                   IF        %Found(FNARB01L)
026700130827     C* dati bolla
026800130827     C                   exsr      DECODIFICHE
026900130827     C*   su più righe
027000170926     c                   exsr      SHIPMENT                                      livello 2
027100170926     c                   exsr      SENDER                                        livello 3
027200170926     c                   exsr      RECEIVER                                      livello 3
027300170926     c                   exsr      PARCEL                                        livello 3
027400170926     C* il messaggio MSG è all'interno del PARCEL (strutturalmente)              livello 4
027500130826    2C                   END
027600130828     c                   end
027700130828     C*
027800130826     C*
027900130827    2C                   ENDSR
028000130826     C*--------------------------------------------------------------*
028100130827     C* dettaglio spedizione
028200130826     C*--------------------------------------------------------------*
028300130827     C     SHIPMENT      BEGSR
028400130827     C*
028500130827     C* parcel MASTER
028600170926     c                   eval      segmento = 'SHIPMENT;2;'
028700171018     c                             + %trim(fldPARCEL)
028800140307     c                             +';'
028900140307     c                             + %trim(bolla)
029000170926     c                             + ';'
029100171018     c                             + %trim(fldCHKdgt)
029200170926     c                             + ';0;'
029300140307     c                             + %trim(BOLLA)
029400140307     c                             +';'
029500140307     c                             + %Trim(ARBRMA)
029600140307     c                             +';'
029700140307     c                             + %trim(%EDITC(ARBRMN:'Z'))
029800140307     c                             + ';;;'
029900140307     c                             + %trim(%EDITC(ARBNCL:'Z'))
030000140307     c                             + ';;'
030100140307     c                             + %trim(%EDITC(wpes100:'Z'))
030200171010     c                             + ';'
030300171010     c                             + %Trim(SenderDepot)
030400171010     c                             + ';'
030500140307     c                             + Anno
030600140307     c                             + %EDITC(arbMGS:'X')
030700140307     c                             + ';090000;I;;;'
030800140307     c                             + %editc(WOGGI:'X')
030900140307     c                             + ';'
031000140307     c                             + %editc(WORA:'X')
031100170904     c                             + ';;;0;;;;;;;'
031200170904     c                             + %Trim(fld_daORM)
031300170904     c                             + ';;;'
031400130827     c                   exsr      VGD_WRITE
031500130827     C*
031600130827    2C                   ENDSR
031700130827     C*--------------------------------------------------------------*
031800130827     C* dettaglio Mittente
031900130827     C*--------------------------------------------------------------*
032000130827     C     SENDER        BEGSR
032100130827     C*
032200130827     C* SENDER
032300170926     c                   eval      segmento = 'SENDER;3;;;;;'
032400140307     c                             + %Trim(fldRSM)
032500140307     c                             + ';;'
032600140307     c                             + %Trim(fldINM)
032700140307     C                             + ';;;;;;;380;;'
032800140307     C                             + %Trim(%editc(fldCPM:'X'))
032900140307     c                             + ';'
033000140307     c                             + %Trim(fldLOM)
033100140307     c                             + ';;'
033200140307     c                             + %Trim(fldTLM)
033300170901     c                             + ';;;;;;;;'
033400130828     c                   exsr      VGD_WRITE
033500130828     C*
033600130828    2C                   ENDSR
033700130827     C*--------------------------------------------------------------*
033800130827     C* dettaglio Destinatario
033900130827     C*--------------------------------------------------------------*
034000130827     C     RECEIVER      BEGSR
034100130827     C*
034200130827     C* RECEIVER
034300170926     c                   eval      segmento = 'RECEIVER;3;;'
034400140307     c                             + %Trim(fldRSD)
034500140307     c                             + ';;'
034600140307     c                             + %Trim(fldRSD)
034700140307     c                             + ';'
034800140307     C                             + %Trim(fldRSD2)
034900140307     C                             + ';;'
035000140307     C                             + %Trim(fldIND)
035100140307     C                             + ';;;;;;;'
035200140307     C                             + %Trim(fldNZD)
035300140307     C                             + ';;'
035400140307     C                             + %Trim(fldCAD)
035500140307     C                             + ';'
035600140307     C                             + %Trim(fldLOD)
035700140307     C                             + ';;'
035800140307     c                             + %Trim(fldCTA)
035900140307     c                             + ';'
036000140611     c                             + %Trim(fldTELcontact)
036100140617     c                             + ';;;;;'
036200150219     c                             + %TrimL(fldNOTE1)
036300150219     c                             + %TrimR(fldNOTE2)
036400170901     c                             + ';;;'
036500130827     c                   exsr      VGD_WRITE
036600130827     C*
036700130827    2C                   ENDSR
036800130827     C*--------------------------------------------------------------*
036900130827     C* dettaglio collo
037000130827     C*--------------------------------------------------------------*
037100130827     C     PARCEL        BEGSR
037200130828
037300130828      * reC 'I'  PARCEL  (se un domani il multicollo)
037400171018     c******             clear                   DSBL4i
037500171018     c****               move      'I'           ktrc
037600171018     c**** Kar4          setll     fiar401l
037700171018     c**** Kar4          reade     fiar401l
037800171018     c******             dow       not %EoF(fiar401l)
037900171018     c******             movel     AR4not        DSBL4i
038000171018      ******
038100171018     C* PARCEL   NO MULTICOLLO
038200170926     c                   eval      segmento = 'PARCEL;3;'
038300171018     c                             + %Trim(fldPARCEL)
038400170926     c                             + ';'
038500171018     c                             + %Trim(fldCHKdgt)
038600170926     c                             + ';;'
038700140307     c                             + %Trim(BOLLA)
038800140307     c                             + ';'
038900140307     c                             + %Trim(arbRMA)
039000140307     c                             + ';'
039100140307     c                             + %trim(%EDITC(ARBRMN:'Z'))
039200140307     C                             + ';;;'
039300171018     C                             + %Trim(fldISC)
039400170901     C                             + ';;;;;;'
039500140307     c                             + %trim(%EDITC(WPes100:'Z'))
039600140307     c                             + ';'
039700140307     c                             + %trim(%EDITC(WPes100:'Z'))
039800170901     c                             + ';;;;;;;;;;;;;'
039900130828     c                   exsr      VGD_WRITE
040000130828     C*
040100140617     c                   if        serv_mail_Attivato = 'S'
040200130828     c                   exsr      MSG
040300140617     c                   end
040400171018      ******
040500171018     c**** Kar4          reade     fiar401l
040600171018     c****               end
040700130827     C*
040800130827    2C                   ENDSR
040900130827     C*--------------------------------------------------------------*
041000130828     C*  Se vi è un messaggio da comunicare è legato al PARCEL
041100130827     C*--------------------------------------------------------------*
041200130828     C     MSG           BEGSR
041300130827      *
041400130828     C* MSG   invio indirizzo Mail
041500140617      *     (è stato provato solo x la MAIL e non per l'SMS)
041600140617      *
041700140617      *   ATTENZIONE provato solo con la MAIL  / l'SMS va implementato
041800140617      *              se si deciderà di attivare il servizio con DPD (a pagamento)
041900140617      *
042000130828     c                   if        fldEML <> *blank
042100170926     c                   eval      segmento = 'MSG;4;'
042200150623     c                             + ';;1;0;'
042300140307     c                             + %trim(fldEML)
042400140307     c                             + ';904;;;'
042500130828     c                   exsr      VGD_WRITE
042600130828     c                   end
042700170822      *
042800170822     c                   if        fldEML1<> *blank
042900170926     c                   eval      segmento = 'MSG;4;'
043000170822     c                             + ';;1;0;'
043100170822     c                             + %trim(fldEML1)
043200170822     c                             + ';904;;;'
043300170822     c                   exsr      VGD_WRITE
043400170822     c                   end
043500130828     C*
043600150617      *
043700150617     c                   if        fldSMS <> *blank
043800170926     c                   eval      segmento = 'MSG;4;'
043900150624     c                             + ';;3;0;'
044000150617     c                             + %trim(fldSMS)
044100150617     c                             + ';904;;;'
044200150617     c                   exsr      VGD_WRITE
044300150617     c                   end
044400170822      *
044500150617     C*
044600130828    2C                   ENDSR
044700130828     C*--------------------------------------------------------------*
044800130828     C* dettaglio spedizione dagli archivi BRT
044900130828     C*--------------------------------------------------------------*
045000130828     C     DECODIFICHE   BEGSR
045100130828      *
045200130827      * con la LNA si iidentificano i dati del Mittente (filiale di Lancio Manifest)
045300130828     c                   clear                   fldRSM
045400130828     c                   clear                   fldINM
045500130828     c                   clear                   fldCPM
045600130828     c                   clear                   fldLOM
045700130828     c                   clear                   fldPRM
045800130828     c                   clear                   fldTLM
045900130828     c                   clear                   fldFXM
046000130827     c                   clear                   fldRSD
046100130827     c                   clear                   fldRSD2
046200130827     c                   clear                   FldIND
046300130827     c                   clear                   fldCAD
046400130827     c                   clear                   fldLOD
046500130827     c                   clear                   fldPRD
046600130827     c                   clear                   fldNZD
046700130827     c                   clear                   fldCTA
046800130827     c                   clear                   fldTEL
046900140611     c                   clear                   fldPREF
047000140611     c                   clear                   fldTELcontact
047100130827     c                   clear                   fldEML
047200170822     c                   clear                   fldEML1
047300130827     c                   clear                   fldSMS
047400140617     c                   clear                   fldSMScontact
047500150218     c                   clear                   fldNOTE1
047600150218     c                   clear                   fldNOTE2
047700170904     c                   movel     '0'           fld_daORM         1
047800171018     C*
047900171018     c                   clear                   fldPARCEL
048000171018     c                   clear                   fldISC
048100171018     c                   clear                   fldCHKdgt
048200140310     C*
048300140310     C* Rif.spedizione BRT
048400140310     c                   move      arbaas        anno              4
048500140310     c                   move      arblnp        linea             3
048600140310     c                   move      arbnrs        serie             2
048700140310     c                   move      arbnsp        numero            7
048800171010      *---
048900171010      *  decodifica il DEPOT di Partenza mediante la LNP
049000171010     c                   clear                   SenderDepot       7
049100171010     c     arbLNP        chain     azorg01l
049200171010     c                   if        %Found(azorg01l)
049300171010     c                   movel     orgDE3        og143
049400171010     c                   move      §OGDP1        SenderDepot
049500171010     c                   movel     buBRT         SenderDepot
049600171010     c                   end
049700171010      *---
049800130828     c                   move      bolla16       bolla            14
049900130828
050000130827      * reC 'D'  RAG.SOCIALE 2
050100130827     c                   move      'D'           ktrc
050200130827     c     Kar4          chain     fiar401l
050300130827     c                   if        %found(fiar401l)
050400130827     c     ';':'.'       xlate     ar4not        fldRSD2
050500130827     c                   end
050600171018      *****
050700130827      * reC 'I'  PARCEL
050800171018     c*****              clear                   DSBL4i
050900171018     c****               move      'I'           ktrc
051000171018     c**** Kar4          chain     fiar401l
051100171018     c*****              if        %found(fiar401l)
051200171018     c*****              movel     AR4not        DSBL4i
051300171018     c*****              end
051400171018      **
051500171018      *  sostituisce il vecchio AR4 tipo "I" --> nuovo FIPND01l
051600171018     c     Kpnd          chain     fipnd01l
051700171018     c                   if        %found(fipnd01l)
051800171018     c                   movel     pndIPN        fldPARCEL
051900171018     c                   movel     pndISC        fldISC
052000171018      **
052100171018      *   prende il CheckDigit
052200171018     c                   if        pndDPE  = 'DP4'
052300171018     c                   eval      DPNDDP4 = pndETIU
052400171018     c                   movel     §b4ICD        fldCHKdgt
052500171018     c                   elseif    pndDPE  = 'DP5'
052600171018     c                   eval      DPNDDP5 = pndETIU
052700171018     c                   movel     §pndICD       fldCHKdgt
052800171018     c                   end
052900171018     c                   end
053000171018      *
053100170904      * reC 'M'  se la spedizione nasce da un ORM
053200170904     c                   move      'M'           ktrc
053300170904     c     Kar4          chain     fiar401l
053400170904     c                   if        %found(fiar401l)
053500170904      *
053600170904     c                   eval       tbeCOD = 'LPD'
053700170904     c                   eval       tbeKE1 = %subst(ar4not:1:3)
053800170904     c     Ktbe1         chain     tntbe01l
053900170904     c                   if        %found(tntbe01l)
054000170904     c                   movel     '1'           fld_daORM
054100170904     c                   end
054200170904      *
054300170904     c                   end
054400170904      *
054500130827     C*  reperisce il volume/peso da CML o da Bollettazione
054600130827     c                   exsr      VOL_PES
054700130827      * mittente
054800130827     c     arbLNA        chain     azorg01l
054900130827     c                   if        %Found(azorg01l)
055000130828     c     ';':'.'       xlate     orgDES        fldRSM
055100130828     c     ';':'.'       xlate     orgIND        fldINM
055200130828     c                   z-add     orgCPF        fldCPM
055300130828     c     ';':'.'       xlate     orgLOC        fldLOM
055400130828     c     ';':'.'       xlate     orgPRO        fldPRM
055500130828     c     ';':'.'       xlate     orgTEL        fldTLM
055600130828     c     ';':'.'       xlate     orgFAX        fldFXM
055700130827     c                   end
055800130827      *
055900130827     C* Destinatario
056000130827     c     ';':'.'       xlate     arbRSD        fldRSD
056100130827     c     ';':'.'       xlate     arbIND        fldIND
056200130827     c     ';':'.'       xlate     arbCAD        fldCAD
056300130827     c     ';':'.'       xlate     arbLOD        fldLOD
056400130827     c     ';':'.'       xlate     arbPRD        fldPRD
056500140424      *
056600140424      **** DPD non vuole avere i CAP con degli spazi in mezzo
056700140424      ** quindi compatto il CAP ove vi fossero degli spazi
056800140424     c                   clear                   compat9CAP        9
056900140424     c                   clear                   nr                3 0
057000140424      * compatta i 9 caratteri togliendo i blank fra una sigla e l'altra
057100140424     c                   DO        9             cap               3 0
057200140424     c                   if        %subst(fldCAD:cap:1) <> ' '
057300140424     c                   eval      nr = nr + 1
057400140424     c                   eval      %subst(compat9CAP:nr:1)
057500140424     c                             =  %subst(fldCAD:cap:1)
057600140424     c                   end
057700140429     c                   endDO
057800140424     c                   eval      fldCAD = compat9CAP
057900140429      *
058000140429      * Queste specifiche sull'Irlanda erano sul FIEU16R:
058100140429      *  (CHIODO xchè Irlanda non ha il CAP)
058200140429      *   Controlla il CAP se scritto "EIRE" (Irlanda) deve essere sostituito
058300140429      *   con "0000001". (Richiesta di Villa da Meeting del 14/10/2008 con DPD.
058400140429     C                   if        fldCAD = 'EIRE'
058500140506      * e se non è DUBLIN deve essere "0000002"
058600140623      *    dopo una mail di rettifica del 23/6/2014 si deve inviare solo 1 o 2
058700140623      *    formalmente nel cap di DUBLINO e IRLANDA
058800140623      *  1 --> RDEPOT= 1601    2 --> RDEPOT= 1602
058900140623      *
059000140623     c                   eval      fldCAD = '2'
059100140506      * ma solo    DUBLIN deve essere "0000001"
059200140506     c     'DUBLIN'      SCAN      fldLOD
059300140506     c                   if        %Found
059400140623     c                   eval      fldCAD = '1'
059500140506     c                   end
059600140429     c                   end
059700130827      *
059800130827     C* Nazione Destinatario
059900130827     C                   Z-ADD     1             YY                3 0
060000130827     C     arbNZD        LOOKUP    C15(YY)                                32
060100130827     C   32ISO(YY)       COMP      *BLANKS                            3232
060200140611     c                   if        *in32
060300140611     C                   MOVEL     CIE(YY)       fldNZD            3
060400140611      * se c'è il pref.internazionale in tabella Nazioni
060500140611      * imposta il codice preceduto dal '+'
060600140611     c                   if        PRF(YY) <> *blank
060700140611     C                   MOVEL     PRF(YY)       fldPREF
060800140611     c                   end
060900140611     c                   end
061000130827      *
061100031210     c                   movel     'GEN'         ktrd
061200031210     c                   clear                   DAr5Gen
061300031210     c     kar5          chain     fiar501L                           34
061400031210     c                   if        %Found(fiar501L)
061500031210     c                   Movel     Ar5Uni        DAr5Gen
061600130827     c     ';':'.'       xlate     §ar5ref       fldCTA
061700130827     c     ';':'.'       xlate     §ar5teld      fldTEL
061800140612     c                   movel     fldTEL        input16          16
061900140612     c                   exsr      compatta_NR
062000140612     c                   movel     out16         fldTEL
062100140611      * se c'è già il prefisso
062200140611     c                   if        %subst(fldTEL:1:1) = '+' or
062300140611     c                             %subst(fldTEL:1:2) = '00'
062400140611     c                   eval      fldTELcontact = fldTEL
062500140611     c                   else
062600140611      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
062700140612     c                   if        fldPREF <> *blank and fldTEL <> *blank
062800140611     c                   eval      fldTELcontact = '+'
062900140612     c                               + %Trim(fldPREF) + %Trim(fldTEL)
063000140611     c                   end
063100140611     c                   end
063200031211     c                   end
063300130827      *
063400140617      * solo se il servizio verrà attivato (al momento salta decodifica)
063500140617      *  nelle definizioni il flag è inizializzato a (N)
063600140617      *
063700140617      *  EMAIL o SMS - come servizio di Alert di consegna -
063800140617      *     ATTENZIONE il programma al momento ha gestito solo la MAIL e
063900140617      *        NON l'SMS --> che dovrà essere implementato se si deciderà
064000140617      *                      l'attivazione del servizio.
064100140617     c                   if        serv_mail_Attivato = 'S'
064200140617      *-
064300130827     c                   movel     'EMD'         ktrd
064400130827     c                   clear                   DAr5emd
064500130827     c     kar5          chain     fiar501L                           34
064600130827     c                   if        %Found(fiar501L)
064700130827     c                   Movel     Ar5Uni        DAr5emd
064800170822     c                   z-add     0             position          3 0
064900170822     c     ';'           scan      §ar5EML:1     position
065000170822     c                   if        position > 0
065100170822     c                   eval      fldEML1= %subst(§ar5EML:position+1)
065200170822     c                   eval      fldEML = %subst(§ar5EML:1:position-1)
065300170822     c                   else
065400170822     c                   eval      fldEML= §ar5EML
065500170822     c                   end
065600170822      **
065700140617     c     ';':' '       xlate     §ar5TEL       fldSMS
065800140617     c                   movel     fldSMS        input16          16
065900140617     c                   exsr      compatta_NR
066000140617     c                   movel     out16         fldSMS
066100140617      * se c'è già il prefisso
066200140617     c                   if        %subst(fldSMS:1:1) = '+' or
066300140617     c                             %subst(fldSMS:1:2) = '00'
066400140617     c                   eval      fldSMScontact = fldSMS
066500140617     c                   else
066600140617      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
066700140617     c                   if        fldPREF <> *blank and fldSMS <> *blank
066800140617     c                   eval      fldSMScontact = '+'
066900140617     c                               + %Trim(fldPREF) + %Trim(fldSMS)
067000140617     c                   end
067100140617     c                   end
067200130827     c                   end
067300140617      *-
067400140617     c                   endIF
067500020219     C*----------------
067600020220     C*
067700020219     C*----------------
067800020219     C*  Controllo se ci sono delle note di consegna
067900020219     C                   MOVEL     '8'           KTRC
068000060213     C     KAR4          CHAIN     fiar401L                           32
068100020220     C*  Shipment Ref.notes (1°campo note)             solo se c'è
068200150218     c  n32';':'.'       xlate     ar4not        fldnote1
068300030226      *
068400150218     C                   MOVEL     '9'           KTRC
068500150218     C     KAR4          CHAIN     fiar401L                           32
068600150218     C*  Shipment Ref.notes (2°campo note)             solo se c'è
068700150218     c  n32';':'.'       xlate     ar4not        fldnote2
068800030226      **
068900020220     C*----------------
069000020226      *
069100020219     c                   endsr
069200020227     C*****************************************************************
069300140612     C*  Compatta il campo numero di telefono avvicinando i numeri
069400030317     C*****************************************************************
069500140612     C     compatta_NR   BEGSR
069600030317      *
069700140612     c                   z-add     0             in                3 0
069800140612     c                   clear                   Byte_uno          1
069900140612     c                   clear                   out16            16
070000140612      *
070100140612      *  controlla presenza di caratteri divisori o blank
070200140612      *   li elimina e deve compattare il numero
070300140612     c                   do        16            Nx                3 0
070400140612     c                   eval      Byte_uno = %subst(input16:Nx:1)
070500140612      * controlla che NON sia un numero :
070600140612     c                   move      'N'           alfa              1
070700140612     c     digits        check     Byte_uno
070800140612     c                   if        %found
070900140612     c                   move      'S'           alfa
071000140612     c                   end
071100140612      *
071200140612      * tranne sul primo che può esserci il '+'
071300140612     c                   if        Nx = 1 and Byte_uno ='+'
071400140612     c                             or alfa = 'N'
071500140612     c                   add       1             in
071600140612     c                   eval      %subst(out16:in:1) = %subst(input16:Nx:1)
071700140612     c                   end
071800140612     c                   enddo
071900140612      *
072000140612     c                   endsr
072100140612     C*****************************************************************
072200140612     C*  Reperisce Peso/Volume o da CML o da Bollettato
072300140612     C*****************************************************************
072400140612     C     VOL_PES       BEGSR
072500140612      *
072600030317      * Prima cerca il volume fra bollettato e CML
072700050112     C     arbncR        ifeq      arbncl
072800030317      * se è considerata un'unica spedizione
072900030317     C                   z-add     ARBVLC        WVOL
073000030317     C                   else
073100030317     C*
073200030317     C     ARBVLC        ifge      ARBVLb
073300030317     C                   z-add     ARBVLC        WVOL
073400030317     C                   else
073500030317     C                   z-add     ARBVLb        WVOL
073600030317     C                   end
073700030317     C*
073800030317     C                   endif
073900030317      *
074000030521     C* PESO VDL - TARA
074100030521     C     §QTTPC        MULT      arbncp        WTARA
074200030521     C                   Z-ADD     arbPKC        wpes
074300150825     c                   sub(h)    wtara         wpes
074400030521      *
074500030317      * Poi cerca il peso fra bollettato e CML
074600050112     C     arbncP        ifeq      arbncl
074700030317      * se è considerata un'unica spedizione
074800030317     C                   ELSE
074900030521     C*  altrimenti se il bollettato è superiore passo il Bollettato.
075000030521     C     wpes          iflt      arbPKb
075100030317     C                   Z-ADD     arbPKb        wpes
075200030317     C                   end
075300030317     C*
075400030317     C                   ENDIF
075500150624     C*
075600150624     C* Se il peso è superiore al limite del fuori misura impostiamo fisso 31 kg.
075700150624     c                   if        wpes > 31,5
075800150624     c                   eval      wpes = 31
075900150624     C                   end
076000150624     C*
076100130827     c                   eval      Wpes100 = ( wpes * 100)
076200030317     C*
076300030317     C                   ENDSR
076400130826      **?__________________________________________________________________ */
076500130826      *    Apertura del TIVGD
076600130826      **?__________________________________________________________________ */
076700130826     C     VGD_OPEN      Begsr
076800130826      *
076900130826      *  recupera il progressivo
077000130826     c                   exsr      Piglia_progr
077100130826      *
077200130826      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
077300130826     C                   clear                   trul47ds
077400130826     C                   eval      d47opz  = 'I'
077500130826     C                   eval      d47tip  = TipoF
077600130826     C                   eval      d47lck  = 'N'
077700130826     C                   eval      d47chkj = 'N'
077800130826     C                   eval      d47pgm  = 'TRTC83R13'
077900130826     C                   call      'TRUL47R'
078000130826     C                   parm                    trul47ds
078100130826      *
078200130826     c                   endsr
078300130826      **?__________________________________________________________________ */
078400130826      *   Scrittura del TIVGD
078500130826      **?__________________________________________________________________ */
078600130826     C     VGD_WRITE     Begsr
078700130826     c                   eval       Conta_righe = 1 + Conta_righe
078800130826      *
078900130826     c                   clear                   tivgd000
079000130826     c                   eval      vgddta = %TrimR(Segmento)
079100130826     c                   eval      vgdtip = TipoF
079200130826     c                   eval      vgdksu = CLIENTE
079300130826     C                   eval      vgdprg = Progressivo
079400130826     c                   eval      vgdtsc = 'WW'
079500130826     c                   eval      vgdpgm = 'TRTC83R13'
079600130828     c                   eval      vgddat = woggi
079700130826      *
079800130826     C                   WRITE     tivgd000
079900130826      *
080000130826     c                   endsr
080100130826      **?__________________________________________________________________ */
080200130826      *    Chiusura del TIVGD
080300130826      **?__________________________________________________________________ */
080400130826     C     VGD_CLOSE     Begsr
080500130826      *
080600130826     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
080700130826     C                   clear                   trul47ds
080800130826     C                   eval      d47opz  = 'F'
080900130826     C                   eval      d47tip  = TipoF
081000130826     C                   call      'TRUL47R'
081100130826     C                   parm                    trul47ds
081200130826      *
081300130826     c                   endsr
081400130826      **?__________________________________________________________________ */
081500130826      *   Prende il Progressivo
081600130826      **?__________________________________________________________________ */
081700130826     C     Piglia_progr  Begsr
081800130826      *
081900130826     C                   MOVEL     CLIENTE       virKSC
082000130826     C                   MOVEL     TipoF         virTIP
082100130826     c     kvir02        chain     tivir02l
082200130828     c                   move      'OF'          Var_ISV
082300130826     c                   if        %Found(tivir02l)
082400130826     c                   move      VirFI1        Var_ISV
082500130826     C                   End
082600130826
082700130826      *       prende numeratore strategi
082800130826     C                   exsr      calprog
082900130826      *
083000130826     c                   endsr
083100130826      **?__________________________________________________________________ */
083200130826      *
083300130826      **?__________________________________________________________________ */
083400130826     C     calprog       begsr
083500130826     C*
083600130828     c                   clear                   progressivo      10
083700130826     C                   open      tis7prgf
083800130826     C                   read(e)   tis7prgf
083900130826     C*
084000130826     C                   if        not %error
084100130826     C                   eval      dwlprg = f_tis7prgf
084200130826     C*
084300130826     C                   move(p)   dwlprg        wrkprg
084400130826     C                   add       1             wrkprg
084500130826     C                   move(p)   wrkprg        dwlprg
084600130826     C                   movel     Var_ISV       dwlprg
084700130826     C*
084800130826     C                   eval       f_tis7prgf = dwlprg
084900130826     C                   eval      Progressivo = dwlprg
085000130826     C*
085100130826     C                   update    tis7prg0
085200130826     C                   endif
085300130826     C*
085400130826     C                   close     tis7prgf
085500130826     C*
085600130826     C                   endsr
085700130826      **?__________________________________________________________________ */
085800130826      **?__________________________________________________________________ */
085900130826     C     *INZSR        BEGSR
086000130826     C*
086100130826     C* Definisco chiavi di accesso
086200130828     c*
086300130828     c     kvir02        klist
086400130828     C                   kfld                    virKSC
086500130828     C                   kfld                    virTIP
086600130828     c*
086700130826     C     KARB          KLIST
086800130826     C                   KFLD                    KAAS
086900130826     C                   KFLD                    KLNP
087000130826     C                   KFLD                    KNRS
087100130826     C                   KFLD                    KNSP
087200130827      *
087300130827     C     KAR4          KLIST
087400130827     C                   KFLD                    KAAS
087500130827     C                   KFLD                    KLNP
087600130827     C                   KFLD                    KNRS
087700130827     C                   KFLD                    KNSP
087800130827     C                   KFLD                    KTRC              1
087900130826      *
088000130826     C     KAR5          KLIST
088100130826     C                   KFLD                    KAAS
088200130826     C                   KFLD                    KLNP
088300130826     C                   KFLD                    KNRS
088400130826     C                   KFLD                    KNSP
088500130826     C                   KFLD                    KTRD              3
088600171018      *
088700171018     C     Kpnd          KLIST
088800171018     C                   KFLD                    KAAS
088900171018     C                   KFLD                    KLNP
089000171018     C                   KFLD                    KNRS
089100171018     C                   KFLD                    KNSP
089200130826      *
089300130826     C     KTAB          KLIST
089400130826     C                   KFLD                    KKUT
089500130826     C                   KFLD                    KCOD
089600130826      *
089700130826     C     KTAB1         KLIST
089800130826     C                   KFLD                    KKUT
089900130826     C                   KFLD                    KCOD
090000130826     C                   KFLD                    KKEY
090100170904      *
090200170904     C     KTBE1         KLIST
090300170904     C                   KFLD                    tbecod
090400170904     C                   KFLD                    tbeke1
090500130826      *
090600130826      *------------
090700130826     C* Inizializzo variabili totale
090800130826     C* Imposto data del giorno
090900130826     C                   TIME                    W0140            14 0
091000130826     C                   MOVE      W0140         G02DAT            8 0
091100130826     C                   MOVEL     W0140         Wora              6 0
091200130826     C                   MOVEL     *BLANK        G02ERR
091300130826     C                   CALL      'XSRDA8'
091400130826     C                   PARM                    WLBDA8
091500130826     C                   MOVEL     G02INV        WOGGI             8 0
091600130826      *
091700130826     C** CARICAMENTO DATI VARI TASSAZIONE
091800130826     C                   Z-ADD     1             KKUT
091900130826     C                   MOVEL     'QT'          kcod
092000130826     C                   MOVEL     *BLANKS       kKEY
092100130826     C                   MOVEL     '1'           kKEY
092200130826     C     KTAB1         CHAIN     TABEL00F                           30
092300130826     C  N30              MOVEL     TBLUNI        DSQT1
092400130826     C*
092500130826      *------------
092600130826     C* Caricamento Tabella codici nazioni
092700130826     C                   Z-ADD     0             X                 3 0
092800130826     C                   MOVEL     '15'          KCOD
092900130826     C                   Z-ADD     1             KKUT
093000130826     C     KTAB          CHAIN     TABEL00F                           30
093100130826     C     *IN30         DOWEQ     '0'
093200130826     C     TBLFLG        IFEQ      *BLANKS
093300130826     C                   ADD       1             X
093400130826     C                   MOVEL     TBLKEY        C15(X)
093500130826     C                   MOVEL     TBLUNI        DS15
093600130826     C                   MOVEL     §15COD        ISO(X)
093700130826     C                   MOVEL     §15des        DNZ(X)
093800130826     C                   MOVEL     §15ACN        ACN(X)
093900130828     C                   MOVEL     §15CIE        CIE(X)
094000140611     C                   MOVEL     §15PREF       prf(X)
094100130826     C                   END
094200130826     C     KTAB          READE     TABEL00F                               30
094300130826     C                   END
094400130826      *------------
094500171010     C*  prende dalla "3I" il nostro codice di B.U.
094600171010     C                   Z-ADD     1             KKUT
094700171010     C                   MOVEL     '3I'          kcod
094800171010     C                   MOVEL     *BLANKS       kKEY
094900171010     C                   MOVEL     'DPD'         kKEY
095000171010     C                   clear                   BUBRT             3 0
095100171010     C     KTAB1         CHAIN     TABEL00F                           30
095200171010     C  N30              MOVEL     TBLUNI        DS3Idp
095300171010     C  N30              MOVEL     §3IBUCN       BUBRT
095400171010     C*
095500171010      *------------
095600171010     C*
095700130826     C                   ENDSR
095800130826      **?__________________________________________________________________ */
