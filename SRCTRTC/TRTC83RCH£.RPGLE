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
009900171108     d  fldEML2        s                   like(§ar5EML)
010000171108     d  fldEML3        s                   like(§ar5EML)
010100140617     d  fldSMScontact  s             30A
010200150218     d  fldNOTE1       s                   like(ar4NOT)
010300150218     d  fldNOTE2       s                   like(ar4NOT)
010400171018     d  fldPARCEL      s                   like(pndIPN)
010500171018     d  fldISC         s                   like(pndISC)
010600171018     d  fldCHKdgt      s              1
010700171018     d DPNDDP4       e ds
010800171018     d DPNDDP5       e ds
010900130827      **
011000130827     d  WTARA          s                   like(§qttpc)
011100130826     d  wvol           s                   like(arbvlb)
011200130826     d  wpes           s                   like(arbpkb)
011300130827     d  wpes100        s              9  0
011400130827      **
011500130826     D dwlprg          s             10    INZ(*all'0')
011600130826     D wrkprg          s              8  0 INZ(*zeros)
011700130827      **
011800130826     d trul47ds      e ds
011900130826     D W0140           S             14  0 inz
012000130826     D Segmento        s           2048
012100130826     D Conta_righe     s              5i 0
012200130826     d  esito          s              1
012300130826     d lunghezza_campo...
012400130826     d                 s              3i 0
012500130826     D*  Definisco costanti
012600130826     D TipoF           C                   CONST('$M')
012700130826     D CLIENTE         C                   CONST('0DPD0OUT')
012800001222     D digits          C                   CONST('0123456789')
012900130828     d Normal_Parcel   c                   101
013000130828     d Small_Parcel    c                   136
013100130828     d Return          c                   300
013200071213      *
013300020222     C*---------------------------------------------------------------------*
013400960530     C* Ciclo principale                                                   -*
013500960530     C*---------------------------------------------------------------------*
013600010426     C     *ENTRY        PLIST
013700010426     C                   PARM                    KPJBA
013800130826     C                   PARM                    flagHDE           1
013900130826      **
014000130826      *  il pgm viene chiamato 3 volte x gestire la scrittura della testata
014100130826      *                                          dei singoli dettagli
014200130826      *                                          e per chiudere il file con #END
014300130826     c                   seton                                        RT
014400130826     c                   setoff                                         LR
014500130826     c                   clear                   segmento
014600130826      **
014700130826      **  flag di testata / dettaglio / end
014800130826      * apre il TIVGD
014900130826      *  scrive la testata e la prima riga di dettaglio
015000130826     c                   if               flagHDE = 'H'
015100130826     C                   EXSR      vgd_OPEN
015200130827     c                   exsr      INIZIO_MSG
015300130826      **
015400130827      * scrive il dettaglio
015500130826     c                   elseif           flagHDE = 'D'
015600130827     c                   exsr      SPEDIZIONE
015700130826      **
015800130826      * quindi chiude il TIVGD quando il chiamante è andato in EoF
015900130826     c                   elseif           flagHDE = 'E'
016000130827     c                   exsr      FINE_MSG
016100130826     C                   EXSR      vgd_CLOSE
016200130826      **
016300130826     c                   COMMIT
016400130826      **
016500130826      * chiude definitivamente dopo aver Committato
016600130826     c                   seton                                        LR
016700130826     c                   setoff                                         RT
016800130826     c                   end
016900130826      **
017000130826     C                   Return
017100130826     C*--------------------------------------------------------------*
017200130826     C* scrive la TESTATA
017300130826     C*--------------------------------------------------------------*
017400130827     C     INIZIO_MSG    BEGSR
017500130826     C*
017600140423     c                   eval      segmento = '#FILE;GEODATA_SHPNOT_0820_DRK_D'
017700140423     c                             + %editc(WOGGI:'X') +'T'+ %editc(WORA:'X')
017800140307     c                             + ';'
017900130826     c                   exsr      VGD_WRITE
018000130826     C*
018100140307     c                   eval      segmento = '#ENCODING;ISO 8859-1;'
018200130826     c                   exsr      VGD_WRITE
018300130826     C*
018400170926     c                   eval      segmento = '#VERSION;03.10;'
018500130826     c                   exsr      VGD_WRITE
018600130826     C*
018700140307     c                   eval      segmento = '#DEF;GEODATA:HEADER;VERSION;'+
018800140307     c                             'CLASSIFICATION;;'
018900130826     c                   exsr      VGD_WRITE
019000130826     C*
019100140307     c                   eval      segmento = '#DEF;GEODATA:SHIPMENT;NUMORDER;'+
019200130828     c                             'MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MPSCRE'+
019300130828     c                             'F1;MPSCREF2;MPSCREF3;MPSCREF4;MPSBILLREF;M'+
019400130828     c                             'PSCOUNT;MPSVOLUME;MPSWEIGHT;SDEPOT;CDATE;C'+
019500130828     c                             'TIME;HARDWARE;RDEPOT;DSORT;SPTDATE;SPTTIME'+
019600130828     c                             ';SPTREALDATE;SPTREALTIME;DELMODALLOW;ROUTI'+
019700170901     c                             'NGPLANVERSION;SPARTNERREF1;SPARTNERREF2;SP'+
019800170901     c                             'ARTNERCODE;OSORT;SSORT;MSORT;COLREQUESTFLA'+
019900170901     c                             'G;ROUTINGPLACE;;'
020000130826     c                   exsr      VGD_WRITE
020100130826     C*
020200140307     c                   eval      segmento = '#DEF;GEODATA:SENDER;NUMORDER;'+
020300130828     c                             'SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPNAME'+
020400130828     c                             ';SUNIQCUSTID;SNAME1;SNAME2;SSTREET;SPROPNU'+
020500130828     c                             'M;SADD2;SADD3;SFLOOR;SBUILDING;SDEPARTMENT'+
020600130828     c                             ';SCOUNTRYCODE;SSTATE;SZIPCODE;STOWN;SCONTA'+
020700170901     c                             'CT;SPHONE;SFAX;SEMAIL;SCOMMENT;SGLN;SGPSLA'+
020800170901     c                             'T;SGPSLONG;SACCOUNTOWNER;;'
020900130826     c                   exsr      VGD_WRITE
021000130827     C*
021100140307     c                   eval      segmento = '#DEF;GEODATA:RECEIVER;NUMORDER;'+
021200130828     c                             'RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPNAME2'+
021300130828     c                             ';RPUDOID;RSTREET;RPROPNUM;RADD2;RADD3;RFLO'+
021400130828     c                             'OR;RBUILDING;RDEPARTMENT;RCOUNTRYCODE;RSTA'+
021500130828     c                             'TE;RZIPCODE;RTOWN;RDOORCODE;RCONTACT;RCONT'+
021600130828     c                             'ACTPHO1;RCONTACTPHO2;RINTERPHONENAME;RFAX;'+
021700170901     c                             'REMAIL;RCOMMENT;RGPSLAT;RGPSLONG;;'
021800130827     c                   exsr      VGD_WRITE
021900130827      *
022000140307     c                   eval      segmento = '#DEF;GEODATA:PARCEL;NUMORDER;'+
022100130828     c                             'PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRANK;'+
022200130828     c                             'SENDPARCELREF1;SENDPARCELREF2;SENDPARCELRE'+
022300130828     c                             'F3;SENDPARCELREF4;RECPARCELREF;SERVICECODE'+
022400170901     c                             ';PPARTNERREF1;PPARTNERREF2;PPARTNERREF3;PP'+
022500170901     c                             'ARTNERREF4;DIMENSION;DECLAREDWEIGHT;MEASUR'+
022600170901     c                             'EDWEIGHT;HINSAMOUNT;HINSCURRENCY;HINSCONTE'+
022700170901     c                             'NT;HAZLQ;HZDPACKCODE;OPCODE;PCONTENT;ORIGI'+
022800170901     c                             'NPARCELNUMBER;POWNERBU;PPARTNERCODE;ASCODE'+
022900170901     c                             ';BAGNO;;'
023000130827     c                   exsr      VGD_WRITE
023100130827      *
023200140307     c                   eval      segmento = '#DEF;GEODATA:MSG;NUMORDER;'+
023300130828     c                             'NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE'+
023400130828     c                             ';MSGDESTTYPE;MSGDESTINATION;MSGTRIGGER;MSG'+
023500140307     c                             'LANG;MSGSENDERURL;;'
023600130827     c                   exsr      VGD_WRITE
023700130827      *
023800170926     c                   eval      segmento = 'HEADER;03.10;SHPNOT;'
023900130827     c                   exsr      VGD_WRITE
024000130827     C*
024100130826     C                   endSR
024200130826     C*--------------------------------------------------------------*
024300130826     C* scrive la CODA
024400130826     C*--------------------------------------------------------------*
024500130827     C     FINE_MSG      BEGSR
024600130826     C*
024700130826     c                   eval      segmento = '#END;'
024800130826     c                   exsr      VGD_WRITE
024900130826     C*
025000130826     C                   endSR
025100960530     C*--------------------------------------------------------------*
025200020219     C* dettaglio spedizione
025300960530     C*--------------------------------------------------------------*
025400130827     C     SPEDIZIONE    BEGSR
025500020227     C*
025600130826     C                   MOVEL     KPJBU         TRTC831
025700130826     C*
025800130826     C*  Se c'è numero spedizione scrivo dettaglio
025900130826    2C     F83NSP        IFNE      0
026000130826     C*
026100130826     C*  Eseguo posizionamento su archivio bolle per passaggio param.
026200130827      * B O L L A
026300130826     C                   Z-ADD     F83AAS        KAAS
026400130826     C                   Z-ADD     F83LNP        KLNP
026500130826     C                   Z-ADD     F83NRS        KNRS
026600130826     C                   Z-ADD     F83NSP        KNSP
026700130826     C     KARB          CHAIN     FNARB01L                           31
026800130827    3C                   IF        %Found(FNARB01L)
026900130827     C* dati bolla
027000130827     C                   exsr      DECODIFICHE
027100130827     C*   su più righe
027200170926     c                   exsr      SHIPMENT                                      livello 2
027300170926     c                   exsr      SENDER                                        livello 3
027400170926     c                   exsr      RECEIVER                                      livello 3
027500170926     c                   exsr      PARCEL                                        livello 3
027600170926     C* il messaggio MSG è all'interno del PARCEL (strutturalmente)              livello 4
027700130826    2C                   END
027800130828     c                   end
027900130828     C*
028000130826     C*
028100130827    2C                   ENDSR
028200130826     C*--------------------------------------------------------------*
028300130827     C* dettaglio spedizione
028400130826     C*--------------------------------------------------------------*
028500130827     C     SHIPMENT      BEGSR
028600130827     C*
028700130827     C* parcel MASTER
028800170926     c                   eval      segmento = 'SHIPMENT;2;'
028900171018     c                             + %trim(fldPARCEL)
029000140307     c                             +';'
029100140307     c                             + %trim(bolla)
029200170926     c                             + ';'
029300171018     c                             + %trim(fldCHKdgt)
029400170926     c                             + ';0;'
029500140307     c                             + %trim(BOLLA)
029600140307     c                             +';'
029700140307     c                             + %Trim(ARBRMA)
029800140307     c                             +';'
029900140307     c                             + %trim(%EDITC(ARBRMN:'Z'))
030000140307     c                             + ';;;'
030100140307     c                             + %trim(%EDITC(ARBNCL:'Z'))
030200140307     c                             + ';;'
030300140307     c                             + %trim(%EDITC(wpes100:'Z'))
030400171010     c                             + ';'
030500171010     c                             + %Trim(SenderDepot)
030600171010     c                             + ';'
030700140307     c                             + Anno
030800140307     c                             + %EDITC(arbMGS:'X')
030900140307     c                             + ';090000;I;;;'
031000140307     c                             + %editc(WOGGI:'X')
031100140307     c                             + ';'
031200140307     c                             + %editc(WORA:'X')
031300170904     c                             + ';;;0;;;;;;;'
031400170904     c                             + %Trim(fld_daORM)
031500170904     c                             + ';;;'
031600130827     c                   exsr      VGD_WRITE
031700130827     C*
031800130827    2C                   ENDSR
031900130827     C*--------------------------------------------------------------*
032000130827     C* dettaglio Mittente
032100130827     C*--------------------------------------------------------------*
032200130827     C     SENDER        BEGSR
032300130827     C*
032400130827     C* SENDER
032500170926     c                   eval      segmento = 'SENDER;3;;;;;'
032600140307     c                             + %Trim(fldRSM)
032700140307     c                             + ';;'
032800140307     c                             + %Trim(fldINM)
032900140307     C                             + ';;;;;;;380;;'
033000140307     C                             + %Trim(%editc(fldCPM:'X'))
033100140307     c                             + ';'
033200140307     c                             + %Trim(fldLOM)
033300140307     c                             + ';;'
033400140307     c                             + %Trim(fldTLM)
033500170901     c                             + ';;;;;;;;'
033600130828     c                   exsr      VGD_WRITE
033700130828     C*
033800130828    2C                   ENDSR
033900130827     C*--------------------------------------------------------------*
034000130827     C* dettaglio Destinatario
034100130827     C*--------------------------------------------------------------*
034200130827     C     RECEIVER      BEGSR
034300130827     C*
034400130827     C* RECEIVER
034500170926     c                   eval      segmento = 'RECEIVER;3;;'
034600140307     c                             + %Trim(fldRSD)
034700140307     c                             + ';;'
034800140307     c                             + %Trim(fldRSD)
034900140307     c                             + ';'
035000140307     C                             + %Trim(fldRSD2)
035100140307     C                             + ';;'
035200140307     C                             + %Trim(fldIND)
035300140307     C                             + ';;;;;;;'
035400140307     C                             + %Trim(fldNZD)
035500140307     C                             + ';;'
035600140307     C                             + %Trim(fldCAD)
035700140307     C                             + ';'
035800140307     C                             + %Trim(fldLOD)
035900140307     C                             + ';;'
036000140307     c                             + %Trim(fldCTA)
036100140307     c                             + ';'
036200140611     c                             + %Trim(fldTELcontact)
036300140617     c                             + ';;;;;'
036400150219     c                             + %TrimL(fldNOTE1)
036500150219     c                             + %TrimR(fldNOTE2)
036600170901     c                             + ';;;'
036700130827     c                   exsr      VGD_WRITE
036800130827     C*
036900130827    2C                   ENDSR
037000130827     C*--------------------------------------------------------------*
037100130827     C* dettaglio collo
037200130827     C*--------------------------------------------------------------*
037300130827     C     PARCEL        BEGSR
037400130828
037500130828      * reC 'I'  PARCEL  (se un domani il multicollo)
037600171018     c******             clear                   DSBL4i
037700171018     c****               move      'I'           ktrc
037800171018     c**** Kar4          setll     fiar401l
037900171018     c**** Kar4          reade     fiar401l
038000171018     c******             dow       not %EoF(fiar401l)
038100171018     c******             movel     AR4not        DSBL4i
038200171018      ******
038300171018     C* PARCEL   NO MULTICOLLO
038400170926     c                   eval      segmento = 'PARCEL;3;'
038500171018     c                             + %Trim(fldPARCEL)
038600170926     c                             + ';'
038700171018     c                             + %Trim(fldCHKdgt)
038800170926     c                             + ';;'
038900140307     c                             + %Trim(BOLLA)
039000140307     c                             + ';'
039100140307     c                             + %Trim(arbRMA)
039200140307     c                             + ';'
039300140307     c                             + %trim(%EDITC(ARBRMN:'Z'))
039400140307     C                             + ';;;'
039500171018     C                             + %Trim(fldISC)
039600170901     C                             + ';;;;;;'
039700140307     c                             + %trim(%EDITC(WPes100:'Z'))
039800140307     c                             + ';'
039900140307     c                             + %trim(%EDITC(WPes100:'Z'))
040000170901     c                             + ';;;;;;;;;;;;;'
040100130828     c                   exsr      VGD_WRITE
040200130828     C*
040300140617     c                   if        serv_mail_Attivato = 'S'
040400130828     c                   exsr      MSG
040500140617     c                   end
040600171018      ******
040700171018     c**** Kar4          reade     fiar401l
040800171018     c****               end
040900130827     C*
041000130827    2C                   ENDSR
041100130827     C*--------------------------------------------------------------*
041200130828     C*  Se vi è un messaggio da comunicare è legato al PARCEL
041300130827     C*--------------------------------------------------------------*
041400130828     C     MSG           BEGSR
041500130827      *
041600130828     C* MSG   invio indirizzo Mail
041700140617      *     (è stato provato solo x la MAIL e non per l'SMS)
041800140617      *
041900140617      *   ATTENZIONE provato solo con la MAIL  / l'SMS va implementato
042000140617      *              se si deciderà di attivare il servizio con DPD (a pagamento)
042100140617      *
042200130828     c                   if        fldEML <> *blank
042300170926     c                   eval      segmento = 'MSG;4;'
042400150623     c                             + ';;1;0;'
042500140307     c                             + %trim(fldEML)
042600140307     c                             + ';904;;;'
042700130828     c                   exsr      VGD_WRITE
042800130828     c                   end
042900170822      *
043000170822     c                   if        fldEML1<> *blank
043100170926     c                   eval      segmento = 'MSG;4;'
043200170822     c                             + ';;1;0;'
043300170822     c                             + %trim(fldEML1)
043400170822     c                             + ';904;;;'
043500170822     c                   exsr      VGD_WRITE
043600170822     c                   end
043700130828     C*
043800171108      *
043900171108     c                   if        fldEML2<> *blank
044000171108     c                   eval      segmento = 'MSG;4;'
044100171108     c                             + ';;1;0;'
044200171108     c                             + %trim(fldEML2)
044300171108     c                             + ';904;;;'
044400171108     c                   exsr      VGD_WRITE
044500171108     c                   end
044600171108     C*
044700171108     c                   if        fldEML3<> *blank
044800171108     c                   eval      segmento = 'MSG;4;'
044900171108     c                             + ';;1;0;'
045000171108     c                             + %trim(fldEML3)
045100171108     c                             + ';904;;;'
045200171108     c                   exsr      VGD_WRITE
045300171108     c                   end
045400171108     C*
045500150617      *
045600150617     c                   if        fldSMS <> *blank
045700170926     c                   eval      segmento = 'MSG;4;'
045800150624     c                             + ';;3;0;'
045900150617     c                             + %trim(fldSMS)
046000150617     c                             + ';904;;;'
046100150617     c                   exsr      VGD_WRITE
046200150617     c                   end
046300170822      *
046400150617     C*
046500130828    2C                   ENDSR
046600130828     C*--------------------------------------------------------------*
046700130828     C* dettaglio spedizione dagli archivi BRT
046800130828     C*--------------------------------------------------------------*
046900130828     C     DECODIFICHE   BEGSR
047000130828      *
047100130827      * con la LNA si iidentificano i dati del Mittente (filiale di Lancio Manifest)
047200130828     c                   clear                   fldRSM
047300130828     c                   clear                   fldINM
047400130828     c                   clear                   fldCPM
047500130828     c                   clear                   fldLOM
047600130828     c                   clear                   fldPRM
047700130828     c                   clear                   fldTLM
047800130828     c                   clear                   fldFXM
047900130827     c                   clear                   fldRSD
048000130827     c                   clear                   fldRSD2
048100130827     c                   clear                   FldIND
048200130827     c                   clear                   fldCAD
048300130827     c                   clear                   fldLOD
048400130827     c                   clear                   fldPRD
048500130827     c                   clear                   fldNZD
048600130827     c                   clear                   fldCTA
048700130827     c                   clear                   fldTEL
048800140611     c                   clear                   fldPREF
048900140611     c                   clear                   fldTELcontact
049000130827     c                   clear                   fldEML
049100170822     c                   clear                   fldEML1
049200171108     c                   clear                   fldEML2
049300171108     c                   clear                   fldEML3
049400130827     c                   clear                   fldSMS
049500140617     c                   clear                   fldSMScontact
049600150218     c                   clear                   fldNOTE1
049700150218     c                   clear                   fldNOTE2
049800170904     c                   movel     '0'           fld_daORM         1
049900171018     C*
050000171018     c                   clear                   fldPARCEL
050100171018     c                   clear                   fldISC
050200171018     c                   clear                   fldCHKdgt
050300140310     C*
050400140310     C* Rif.spedizione BRT
050500140310     c                   move      arbaas        anno              4
050600140310     c                   move      arblnp        linea             3
050700140310     c                   move      arbnrs        serie             2
050800140310     c                   move      arbnsp        numero            7
050900171010      *---
051000171010      *  decodifica il DEPOT di Partenza mediante la LNP
051100171010     c                   clear                   SenderDepot       7
051200171010     c     arbLNP        chain     azorg01l
051300171010     c                   if        %Found(azorg01l)
051400171010     c                   movel     orgDE3        og143
051500171010     c                   move      §OGDP1        SenderDepot
051600171010     c                   movel     buBRT         SenderDepot
051700171010     c                   end
051800171010      *---
051900130828     c                   move      bolla16       bolla            14
052000130828
052100130827      * reC 'D'  RAG.SOCIALE 2
052200130827     c                   move      'D'           ktrc
052300130827     c     Kar4          chain     fiar401l
052400130827     c                   if        %found(fiar401l)
052500130827     c     ';':'.'       xlate     ar4not        fldRSD2
052600130827     c                   end
052700171018      *****
052800130827      * reC 'I'  PARCEL
052900171018     c*****              clear                   DSBL4i
053000171018     c****               move      'I'           ktrc
053100171018     c**** Kar4          chain     fiar401l
053200171018     c*****              if        %found(fiar401l)
053300171018     c*****              movel     AR4not        DSBL4i
053400171018     c*****              end
053500171018      **
053600171018      *  sostituisce il vecchio AR4 tipo "I" --> nuovo FIPND01l
053700171018     c     Kpnd          chain     fipnd01l
053800171018     c                   if        %found(fipnd01l)
053900171018     c                   movel     pndIPN        fldPARCEL
054000171018     c                   movel     pndISC        fldISC
054100171018      **
054200171018      *   prende il CheckDigit
054300171018     c                   if        pndDPE  = 'DP4'
054400171018     c                   eval      DPNDDP4 = pndETIU
054500171018     c                   movel     §b4ICD        fldCHKdgt
054600171018     c                   elseif    pndDPE  = 'DP5'
054700171018     c                   eval      DPNDDP5 = pndETIU
054800171018     c                   movel     §pndICD       fldCHKdgt
054900171018     c                   end
055000171018     c                   end
055100171018      *
055200170904      * reC 'M'  se la spedizione nasce da un ORM
055300170904     c                   move      'M'           ktrc
055400170904     c     Kar4          chain     fiar401l
055500170904     c                   if        %found(fiar401l)
055600170904      *
055700170904     c                   eval       tbeCOD = 'LPD'
055800170904     c                   eval       tbeKE1 = %subst(ar4not:1:3)
055900170904     c     Ktbe1         chain     tntbe01l
056000170904     c                   if        %found(tntbe01l)
056100170904     c                   movel     '1'           fld_daORM
056200170904     c                   end
056300170904      *
056400170904     c                   end
056500170904      *
056600130827     C*  reperisce il volume/peso da CML o da Bollettazione
056700130827     c                   exsr      VOL_PES
056800130827      * mittente
056900130827     c     arbLNA        chain     azorg01l
057000130827     c                   if        %Found(azorg01l)
057100130828     c     ';':'.'       xlate     orgDES        fldRSM
057200130828     c     ';':'.'       xlate     orgIND        fldINM
057300130828     c                   z-add     orgCPF        fldCPM
057400130828     c     ';':'.'       xlate     orgLOC        fldLOM
057500130828     c     ';':'.'       xlate     orgPRO        fldPRM
057600130828     c     ';':'.'       xlate     orgTEL        fldTLM
057700130828     c     ';':'.'       xlate     orgFAX        fldFXM
057800130827     c                   end
057900130827      *
058000130827     C* Destinatario
058100130827     c     ';':'.'       xlate     arbRSD        fldRSD
058200130827     c     ';':'.'       xlate     arbIND        fldIND
058300130827     c     ';':'.'       xlate     arbCAD        fldCAD
058400130827     c     ';':'.'       xlate     arbLOD        fldLOD
058500130827     c     ';':'.'       xlate     arbPRD        fldPRD
058600140424      *
058700140424      **** DPD non vuole avere i CAP con degli spazi in mezzo
058800140424      ** quindi compatto il CAP ove vi fossero degli spazi
058900140424     c                   clear                   compat9CAP        9
059000140424     c                   clear                   nr                3 0
059100140424      * compatta i 9 caratteri togliendo i blank fra una sigla e l'altra
059200140424     c                   DO        9             cap               3 0
059300140424     c                   if        %subst(fldCAD:cap:1) <> ' '
059400140424     c                   eval      nr = nr + 1
059500140424     c                   eval      %subst(compat9CAP:nr:1)
059600140424     c                             =  %subst(fldCAD:cap:1)
059700140424     c                   end
059800140429     c                   endDO
059900140424     c                   eval      fldCAD = compat9CAP
060000140429      *
060100140429      * Queste specifiche sull'Irlanda erano sul FIEU16R:
060200140429      *  (CHIODO xchè Irlanda non ha il CAP)
060300140429      *   Controlla il CAP se scritto "EIRE" (Irlanda) deve essere sostituito
060400140429      *   con "0000001". (Richiesta di Villa da Meeting del 14/10/2008 con DPD.
060500140429     C                   if        fldCAD = 'EIRE'
060600140506      * e se non è DUBLIN deve essere "0000002"
060700140623      *    dopo una mail di rettifica del 23/6/2014 si deve inviare solo 1 o 2
060800140623      *    formalmente nel cap di DUBLINO e IRLANDA
060900140623      *  1 --> RDEPOT= 1601    2 --> RDEPOT= 1602
061000140623      *
061100140623     c                   eval      fldCAD = '2'
061200140506      * ma solo    DUBLIN deve essere "0000001"
061300140506     c     'DUBLIN'      SCAN      fldLOD
061400140506     c                   if        %Found
061500140623     c                   eval      fldCAD = '1'
061600140506     c                   end
061700140429     c                   end
061800130827      *
061900130827     C* Nazione Destinatario
062000130827     C                   Z-ADD     1             YY                3 0
062100130827     C     arbNZD        LOOKUP    C15(YY)                                32
062200130827     C   32ISO(YY)       COMP      *BLANKS                            3232
062300140611     c                   if        *in32
062400140611     C                   MOVEL     CIE(YY)       fldNZD            3
062500140611      * se c'è il pref.internazionale in tabella Nazioni
062600140611      * imposta il codice preceduto dal '+'
062700140611     c                   if        PRF(YY) <> *blank
062800140611     C                   MOVEL     PRF(YY)       fldPREF
062900140611     c                   end
063000140611     c                   end
063100130827      *
063200031210     c                   movel     'GEN'         ktrd
063300031210     c                   clear                   DAr5Gen
063400031210     c     kar5          chain     fiar501L                           34
063500031210     c                   if        %Found(fiar501L)
063600031210     c                   Movel     Ar5Uni        DAr5Gen
063700130827     c     ';':'.'       xlate     §ar5ref       fldCTA
063800130827     c     ';':'.'       xlate     §ar5teld      fldTEL
063900140612     c                   movel     fldTEL        input16          16
064000140612     c                   exsr      compatta_NR
064100140612     c                   movel     out16         fldTEL
064200140611      * se c'è già il prefisso
064300140611     c                   if        %subst(fldTEL:1:1) = '+' or
064400140611     c                             %subst(fldTEL:1:2) = '00'
064500140611     c                   eval      fldTELcontact = fldTEL
064600140611     c                   else
064700140611      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
064800140612     c                   if        fldPREF <> *blank and fldTEL <> *blank
064900140611     c                   eval      fldTELcontact = '+'
065000140612     c                               + %Trim(fldPREF) + %Trim(fldTEL)
065100140611     c                   end
065200140611     c                   end
065300031211     c                   end
065400130827      *
065500140617      * solo se il servizio verrà attivato (al momento salta decodifica)
065600140617      *  nelle definizioni il flag è inizializzato a (N)
065700140617      *
065800140617      *  EMAIL o SMS - come servizio di Alert di consegna -
065900140617      *     ATTENZIONE il programma al momento ha gestito solo la MAIL e
066000140617      *        NON l'SMS --> che dovrà essere implementato se si deciderà
066100140617      *                      l'attivazione del servizio.
066200140617     c                   if        serv_mail_Attivato = 'S'
066300140617      *-
066400130827     c                   movel     'EMD'         ktrd
066500130827     c                   clear                   DAr5emd
066600130827     c     kar5          chain     fiar501L                           34
066700130827     c                   if        %Found(fiar501L)
066800130827     c                   Movel     Ar5Uni        DAr5emd
066900171108      **
067000171108     c                   z-add     0             Pos               3 0
067100171108     c                   z-add     0             Pos1              3 0
067200171108     c                   z-add     0             Pos2              3 0
067300171108     c                   z-add     0             Pos3              3 0
067400171108      **
067500171108     c     ';'           scan      §ar5EML:1     Pos1
067600171108     c                   if        Pos1 > 0
067700171108     c                   eval      Pos = Pos1 +1
067800171108     c     ';'           scan      §ar5EML:Pos   Pos2
067900171108     c                   if        Pos2 > 0
068000171108     c                   eval      Pos = Pos2 +1
068100171108     c     ';'           scan      §ar5EML:Pos   Pos3
068200171108     c                   end
068300171108     c                   end
068400171108      **
068500171108      **  1 solo indirizzo
068600171108     c                   if        Pos1 = 0
068700171108     c                   eval      fldEML= §ar5EML
068800171108      **  2 indirizzi
068900171108     c                   elseif    Pos1 > 0 and Pos2 = 0
069000171108     c                   eval      fldEML = %subst(§ar5EML:1:Pos1-1)
069100171108     c                   eval      fldEML1= %subst(§ar5EML:Pos1+1)
069200171108      **  3 indirizzi
069300171108     c                   elseif    Pos2 > 0 and Pos3 = 0
069400171108     c                   eval      fldEML = %subst(§ar5EML:1:Pos1-1)
069500171108     c                   eval      fldEML1= %subst(§ar5EML:Pos1+1:Pos2-Pos1-1)
069600171108     c                   eval      fldEML2= %subst(§ar5EML:Pos2+1)
069700171108      **  4 indirizzi
069800171108     c                   elseif    Pos3 > 0
069900171108     c                   eval      fldEML = %subst(§ar5EML:1:Pos1-1)
070000171108     c                   eval      fldEML1= %subst(§ar5EML:Pos1+1:Pos2-Pos1-1)
070100171108     c                   eval      fldEML2= %subst(§ar5EML:Pos2+1:Pos3-Pos2-1)
070200171108     c                   eval      fldEML3= %subst(§ar5EML:Pos3+1)
070300170822     c                   end
070400170822      **
070500140617     c     ';':' '       xlate     §ar5TEL       fldSMS
070600140617     c                   movel     fldSMS        input16          16
070700140617     c                   exsr      compatta_NR
070800140617     c                   movel     out16         fldSMS
070900140617      * se c'è già il prefisso
071000140617     c                   if        %subst(fldSMS:1:1) = '+' or
071100140617     c                             %subst(fldSMS:1:2) = '00'
071200140617     c                   eval      fldSMScontact = fldSMS
071300140617     c                   else
071400140617      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
071500140617     c                   if        fldPREF <> *blank and fldSMS <> *blank
071600140617     c                   eval      fldSMScontact = '+'
071700140617     c                               + %Trim(fldPREF) + %Trim(fldSMS)
071800140617     c                   end
071900140617     c                   end
072000130827     c                   end
072100140617      *-
072200140617     c                   endIF
072300020219     C*----------------
072400020220     C*
072500020219     C*----------------
072600020219     C*  Controllo se ci sono delle note di consegna
072700020219     C                   MOVEL     '8'           KTRC
072800060213     C     KAR4          CHAIN     fiar401L                           32
072900020220     C*  Shipment Ref.notes (1°campo note)             solo se c'è
073000150218     c  n32';':'.'       xlate     ar4not        fldnote1
073100030226      *
073200150218     C                   MOVEL     '9'           KTRC
073300150218     C     KAR4          CHAIN     fiar401L                           32
073400150218     C*  Shipment Ref.notes (2°campo note)             solo se c'è
073500150218     c  n32';':'.'       xlate     ar4not        fldnote2
073600030226      **
073700020220     C*----------------
073800020226      *
073900020219     c                   endsr
074000020227     C*****************************************************************
074100140612     C*  Compatta il campo numero di telefono avvicinando i numeri
074200030317     C*****************************************************************
074300140612     C     compatta_NR   BEGSR
074400030317      *
074500140612     c                   z-add     0             in                3 0
074600140612     c                   clear                   Byte_uno          1
074700140612     c                   clear                   out16            16
074800140612      *
074900140612      *  controlla presenza di caratteri divisori o blank
075000140612      *   li elimina e deve compattare il numero
075100140612     c                   do        16            Nx                3 0
075200140612     c                   eval      Byte_uno = %subst(input16:Nx:1)
075300140612      * controlla che NON sia un numero :
075400140612     c                   move      'N'           alfa              1
075500140612     c     digits        check     Byte_uno
075600140612     c                   if        %found
075700140612     c                   move      'S'           alfa
075800140612     c                   end
075900140612      *
076000140612      * tranne sul primo che può esserci il '+'
076100140612     c                   if        Nx = 1 and Byte_uno ='+'
076200140612     c                             or alfa = 'N'
076300140612     c                   add       1             in
076400140612     c                   eval      %subst(out16:in:1) = %subst(input16:Nx:1)
076500140612     c                   end
076600140612     c                   enddo
076700140612      *
076800140612     c                   endsr
076900140612     C*****************************************************************
077000140612     C*  Reperisce Peso/Volume o da CML o da Bollettato
077100140612     C*****************************************************************
077200140612     C     VOL_PES       BEGSR
077300140612      *
077400030317      * Prima cerca il volume fra bollettato e CML
077500050112     C     arbncR        ifeq      arbncl
077600030317      * se è considerata un'unica spedizione
077700030317     C                   z-add     ARBVLC        WVOL
077800030317     C                   else
077900030317     C*
078000030317     C     ARBVLC        ifge      ARBVLb
078100030317     C                   z-add     ARBVLC        WVOL
078200030317     C                   else
078300030317     C                   z-add     ARBVLb        WVOL
078400030317     C                   end
078500030317     C*
078600030317     C                   endif
078700030317      *
078800030521     C* PESO VDL - TARA
078900030521     C     §QTTPC        MULT      arbncp        WTARA
079000030521     C                   Z-ADD     arbPKC        wpes
079100150825     c                   sub(h)    wtara         wpes
079200030521      *
079300030317      * Poi cerca il peso fra bollettato e CML
079400050112     C     arbncP        ifeq      arbncl
079500030317      * se è considerata un'unica spedizione
079600030317     C                   ELSE
079700030521     C*  altrimenti se il bollettato è superiore passo il Bollettato.
079800030521     C     wpes          iflt      arbPKb
079900030317     C                   Z-ADD     arbPKb        wpes
080000030317     C                   end
080100030317     C*
080200030317     C                   ENDIF
080300150624     C*
080400150624     C* Se il peso è superiore al limite del fuori misura impostiamo fisso 31 kg.
080500150624     c                   if        wpes > 31,5
080600150624     c                   eval      wpes = 31
080700150624     C                   end
080800150624     C*
080900130827     c                   eval      Wpes100 = ( wpes * 100)
081000030317     C*
081100030317     C                   ENDSR
081200130826      **?__________________________________________________________________ */
081300130826      *    Apertura del TIVGD
081400130826      **?__________________________________________________________________ */
081500130826     C     VGD_OPEN      Begsr
081600130826      *
081700130826      *  recupera il progressivo
081800130826     c                   exsr      Piglia_progr
081900130826      *
082000130826      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
082100130826     C                   clear                   trul47ds
082200130826     C                   eval      d47opz  = 'I'
082300130826     C                   eval      d47tip  = TipoF
082400130826     C                   eval      d47lck  = 'N'
082500130826     C                   eval      d47chkj = 'N'
082600130826     C                   eval      d47pgm  = 'TRTC83R13'
082700130826     C                   call      'TRUL47R'
082800130826     C                   parm                    trul47ds
082900130826      *
083000130826     c                   endsr
083100130826      **?__________________________________________________________________ */
083200130826      *   Scrittura del TIVGD
083300130826      **?__________________________________________________________________ */
083400130826     C     VGD_WRITE     Begsr
083500130826     c                   eval       Conta_righe = 1 + Conta_righe
083600130826      *
083700130826     c                   clear                   tivgd000
083800130826     c                   eval      vgddta = %TrimR(Segmento)
083900130826     c                   eval      vgdtip = TipoF
084000130826     c                   eval      vgdksu = CLIENTE
084100130826     C                   eval      vgdprg = Progressivo
084200130826     c                   eval      vgdtsc = 'WW'
084300130826     c                   eval      vgdpgm = 'TRTC83R13'
084400130828     c                   eval      vgddat = woggi
084500130826      *
084600130826     C                   WRITE     tivgd000
084700130826      *
084800130826     c                   endsr
084900130826      **?__________________________________________________________________ */
085000130826      *    Chiusura del TIVGD
085100130826      **?__________________________________________________________________ */
085200130826     C     VGD_CLOSE     Begsr
085300130826      *
085400130826     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
085500130826     C                   clear                   trul47ds
085600130826     C                   eval      d47opz  = 'F'
085700130826     C                   eval      d47tip  = TipoF
085800130826     C                   call      'TRUL47R'
085900130826     C                   parm                    trul47ds
086000130826      *
086100130826     c                   endsr
086200130826      **?__________________________________________________________________ */
086300130826      *   Prende il Progressivo
086400130826      **?__________________________________________________________________ */
086500130826     C     Piglia_progr  Begsr
086600130826      *
086700130826     C                   MOVEL     CLIENTE       virKSC
086800130826     C                   MOVEL     TipoF         virTIP
086900130826     c     kvir02        chain     tivir02l
087000130828     c                   move      'OF'          Var_ISV
087100130826     c                   if        %Found(tivir02l)
087200130826     c                   move      VirFI1        Var_ISV
087300130826     C                   End
087400130826
087500130826      *       prende numeratore strategi
087600130826     C                   exsr      calprog
087700130826      *
087800130826     c                   endsr
087900130826      **?__________________________________________________________________ */
088000130826      *
088100130826      **?__________________________________________________________________ */
088200130826     C     calprog       begsr
088300130826     C*
088400130828     c                   clear                   progressivo      10
088500130826     C                   open      tis7prgf
088600130826     C                   read(e)   tis7prgf
088700130826     C*
088800130826     C                   if        not %error
088900130826     C                   eval      dwlprg = f_tis7prgf
089000130826     C*
089100130826     C                   move(p)   dwlprg        wrkprg
089200130826     C                   add       1             wrkprg
089300130826     C                   move(p)   wrkprg        dwlprg
089400130826     C                   movel     Var_ISV       dwlprg
089500130826     C*
089600130826     C                   eval       f_tis7prgf = dwlprg
089700130826     C                   eval      Progressivo = dwlprg
089800130826     C*
089900130826     C                   update    tis7prg0
090000130826     C                   endif
090100130826     C*
090200130826     C                   close     tis7prgf
090300130826     C*
090400130826     C                   endsr
090500130826      **?__________________________________________________________________ */
090600130826      **?__________________________________________________________________ */
090700130826     C     *INZSR        BEGSR
090800130826     C*
090900130826     C* Definisco chiavi di accesso
091000130828     c*
091100130828     c     kvir02        klist
091200130828     C                   kfld                    virKSC
091300130828     C                   kfld                    virTIP
091400130828     c*
091500130826     C     KARB          KLIST
091600130826     C                   KFLD                    KAAS
091700130826     C                   KFLD                    KLNP
091800130826     C                   KFLD                    KNRS
091900130826     C                   KFLD                    KNSP
092000130827      *
092100130827     C     KAR4          KLIST
092200130827     C                   KFLD                    KAAS
092300130827     C                   KFLD                    KLNP
092400130827     C                   KFLD                    KNRS
092500130827     C                   KFLD                    KNSP
092600130827     C                   KFLD                    KTRC              1
092700130826      *
092800130826     C     KAR5          KLIST
092900130826     C                   KFLD                    KAAS
093000130826     C                   KFLD                    KLNP
093100130826     C                   KFLD                    KNRS
093200130826     C                   KFLD                    KNSP
093300130826     C                   KFLD                    KTRD              3
093400171018      *
093500171018     C     Kpnd          KLIST
093600171018     C                   KFLD                    KAAS
093700171018     C                   KFLD                    KLNP
093800171018     C                   KFLD                    KNRS
093900171018     C                   KFLD                    KNSP
094000130826      *
094100130826     C     KTAB          KLIST
094200130826     C                   KFLD                    KKUT
094300130826     C                   KFLD                    KCOD
094400130826      *
094500130826     C     KTAB1         KLIST
094600130826     C                   KFLD                    KKUT
094700130826     C                   KFLD                    KCOD
094800130826     C                   KFLD                    KKEY
094900170904      *
095000170904     C     KTBE1         KLIST
095100170904     C                   KFLD                    tbecod
095200170904     C                   KFLD                    tbeke1
095300130826      *
095400130826      *------------
095500130826     C* Inizializzo variabili totale
095600130826     C* Imposto data del giorno
095700130826     C                   TIME                    W0140            14 0
095800130826     C                   MOVE      W0140         G02DAT            8 0
095900130826     C                   MOVEL     W0140         Wora              6 0
096000130826     C                   MOVEL     *BLANK        G02ERR
096100130826     C                   CALL      'XSRDA8'
096200130826     C                   PARM                    WLBDA8
096300130826     C                   MOVEL     G02INV        WOGGI             8 0
096400130826      *
096500130826     C** CARICAMENTO DATI VARI TASSAZIONE
096600130826     C                   Z-ADD     1             KKUT
096700130826     C                   MOVEL     'QT'          kcod
096800130826     C                   MOVEL     *BLANKS       kKEY
096900130826     C                   MOVEL     '1'           kKEY
097000130826     C     KTAB1         CHAIN     TABEL00F                           30
097100130826     C  N30              MOVEL     TBLUNI        DSQT1
097200130826     C*
097300130826      *------------
097400130826     C* Caricamento Tabella codici nazioni
097500130826     C                   Z-ADD     0             X                 3 0
097600130826     C                   MOVEL     '15'          KCOD
097700130826     C                   Z-ADD     1             KKUT
097800130826     C     KTAB          CHAIN     TABEL00F                           30
097900130826     C     *IN30         DOWEQ     '0'
098000130826     C     TBLFLG        IFEQ      *BLANKS
098100130826     C                   ADD       1             X
098200130826     C                   MOVEL     TBLKEY        C15(X)
098300130826     C                   MOVEL     TBLUNI        DS15
098400130826     C                   MOVEL     §15COD        ISO(X)
098500130826     C                   MOVEL     §15des        DNZ(X)
098600130826     C                   MOVEL     §15ACN        ACN(X)
098700130828     C                   MOVEL     §15CIE        CIE(X)
098800140611     C                   MOVEL     §15PREF       prf(X)
098900130826     C                   END
099000130826     C     KTAB          READE     TABEL00F                               30
099100130826     C                   END
099200130826      *------------
099300171010     C*  prende dalla "3I" il nostro codice di B.U.
099400171010     C                   Z-ADD     1             KKUT
099500171010     C                   MOVEL     '3I'          kcod
099600171010     C                   MOVEL     *BLANKS       kKEY
099700171010     C                   MOVEL     'DPD'         kKEY
099800171010     C                   clear                   BUBRT             3 0
099900171010     C     KTAB1         CHAIN     TABEL00F                           30
100000171010     C  N30              MOVEL     TBLUNI        DS3Idp
100100171010     C  N30              MOVEL     §3IBUCN       BUBRT
100200171010     C*
100300171010      *------------
100400171010     C*
100500130826     C                   ENDSR
100600130826      **?__________________________________________________________________ */
