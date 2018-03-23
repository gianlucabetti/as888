000100960530      ***********************************************************************
000200130725      *   ESPORTA DATI BOLLE x DPD oltre gli SCAN  ESTERO    Export         *
000300130725      *     ?   - NUOVO TRACCIATO DPD -    ?
000400960530      ***********************************************************************
000500891004     H DECEDIT('0,') DATEDIT(*DMY/)
000600000207      *---------------------------------------------------------------------*
000700140311     FFNARB01L  iF   E           K DISK    Commit
000800060213     Ffiar401L  IF   E           K DISK
000900031210     Ffiar501L  IF   E           K DISK
001000960530     FTABEL00F  IF   E           K DISK
001100170904     FTntbe01l  IF   E           K DISK
001200020219     FAzOrg01L  IF   E           K DISK
001300130725      ***
001400130826     ftivgd00f  if a E             DISK    commit
001500130826      *
001600130826     Ftivir02L  IF   E           K DISK
001700130826     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001800130826     F                                     PREFIX(f_) usropn
001900140617      *---------------------------------------------------------------------*
002000140617      **  Flag di attivazione
002100140617      *---------------------------------------------------------------------*
002200140617      * il servizio mail di alert è predisposto
002300140617      *    ma NON attivato poichè adesso impostato a (N) --> non decodifica i dati
002400140617      *    né esegue la scrittura del record "MSG" xchè con DPD non è attivo il servizio.
002500140617      *
002600140617     d  serv_mail_Attivato...
002700150617     d                 s              1    INZ('S')
002800140617      **
002900000207      *---------------------------------------------------------------------*
003000000207      * Schiere
003100000207      *---------------------------------------------------------------------*
003200020220     D Num             S              1    DIM(11)
003300000207      * Codici ISO nazioni x E.D.I.
003400011217     D C15             S              3    DIM(500)
003500011217     D ISO             S              3    DIM(500)
003600130828     D CIE             S              3  0 DIM(500)
003700020219     D DNZ             S             25    DIM(500)
003800020412     D ACN             S              1    DIM(500)
003900140611     D PRF             S              4    DIM(500)
004000000207      *---------------------------------------------------------------------*
004100000207      * DS
004200000207      *---------------------------------------------------------------------*
004300130827     D DSBL4i        E DS
004400031210     D dar5gen       e ds
004500130827     D dar5emd       e ds
004600020227     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
004700960530     D DS3A          E DS
004800030521     D DSQT1         E DS                  INZ
004900960822     D DS15          E DS
005000130828     D DDPP          E DS
005100950428     D KPJBA         E DS
005200130827      *
005300130828     D BOLLA16         DS                  INZ
005400130827     d  anno                          4
005500130827     d  linea                         3
005600130827     d  serie                         2
005700130827     d  numero                        7
005800130827      *
005900960530     D WLBDA8          DS                  INZ
006000950428     D  G02DAT                 1      8  0
006100950428     D  G02INV                 9     16  0
006200950428     D  G02ERR                17     17
006300950428     D  G02TGI                18     22  0
006400130826     d  KAAS           s                   like(ARBAAS)
006500130826     d  KLNP           s                   like(ARBLNP)
006600130826     d  KNRS           s                   like(ARBNRS)
006700130826     d  KNSP           s                   like(ARBNSP)
006800130826     d  KTRC           s                   like(AR4TRC)
006900130826     d  KKUT           s                   like(TBLKUT)
007000130826     d  KCOD           s                   like(TBLCOD)
007100130826     d  KKEY           s                   like(TBLKEY)
007200130828     D Var_ISV         s                   like(Virfi1)
007300130828     d  fldRSM         s                   like(orgDES)                            fldDES
007400130828     d  fldINM         s                   like(orgIND)                            fldIND
007500130828     d  fldCPM         s              5  0
007600130828     d  fldLOM         s                   like(orgLOC)                            fldLOC
007700130828     d  fldPRM         s                   like(orgPRO)                            fldPRO
007800130828     d  fldTLM         s                   like(orgTEL)                            fldTEL
007900130828     d  fldFXM         s                   like(orgFAX)                            fldFAX
008000130827     d  fldRSD         s                   like(arbRSD)
008100130827     d  fldRSD2        s                   like(ar4NOT)
008200130827     d  fldIND         s                   like(arbIND)
008300130828     d  fldCAD         s                   like(arbCAD)
008400130827     d  fldLOD         s                   like(arbLOD)
008500130827     d  fldPRD         s                   like(arbPRD)
008600130827     d  fldCTA         s                   like(§AR5REF)
008700130827     d  fldTEL         s                   like(§ar5teld)
008800140611     d  fldTELcontact  s             30A
008900140611     d  fldPREF        s              4A
009000130827     d  fldEML         s                   like(§ar5EML)
009100130827     d  fldSMS         s                   like(§ar5TEL)
009200170822     d  fldEML1        s                   like(§ar5EML)
009300140617     d  fldSMScontact  s             30A
009400150218     d  fldNOTE1       s                   like(ar4NOT)
009500150218     d  fldNOTE2       s                   like(ar4NOT)
009600150709     d  srvCODE        s              3
009700150709     d  nPARCEL        s             14
009800130827      **
009900130827     d  WTARA          s                   like(§qttpc)
010000130826     d  wvol           s                   like(arbvlb)
010100130826     d  wpes           s                   like(arbpkb)
010200130827     d  wpes100        s              9  0
010300130827      **
010400130826     D dwlprg          s             10    INZ(*all'0')
010500130826     D wrkprg          s              8  0 INZ(*zeros)
010600130827      **
010700130826     d trul47ds      e ds
010800130826     D W0140           S             14  0 inz
010900130826     D Segmento        s           2048
011000130826     D Conta_righe     s              5i 0
011100130826     d  esito          s              1
011200130826     d lunghezza_campo...
011300130826     d                 s              3i 0
011400130826     D*  Definisco costanti
011500130826     D TipoF           C                   CONST('$M')
011600130826     D CLIENTE         C                   CONST('0DPD0OUT')
011700001222     D digits          C                   CONST('0123456789')
011800130828     d Normal_Parcel   c                   101
011900130828     d Small_Parcel    c                   136
012000130828     d Return          c                   300
012100071213      *
012200020222     C*---------------------------------------------------------------------*
012300960530     C* Ciclo principale                                                   -*
012400960530     C*---------------------------------------------------------------------*
012500010426     C     *ENTRY        PLIST
012600010426     C                   PARM                    KPJBA
012700130826     C                   PARM                    flagHDE           1
012800130826      **
012900130826      *  il pgm viene chiamato 3 volte x gestire la scrittura della testata
013000130826      *                                          dei singoli dettagli
013100130826      *                                          e per chiudere il file con #END
013200130826     c                   seton                                        RT
013300130826     c                   setoff                                         LR
013400130826     c                   clear                   segmento
013500130826      **
013600130826      **  flag di testata / dettaglio / end
013700130826      * apre il TIVGD
013800130826      *  scrive la testata e la prima riga di dettaglio
013900130826     c                   if               flagHDE = 'H'
014000130826     C                   EXSR      vgd_OPEN
014100130827     c                   exsr      INIZIO_MSG
014200130826      **
014300130827      * scrive il dettaglio
014400130826     c                   elseif           flagHDE = 'D'
014500130827     c                   exsr      SPEDIZIONE
014600130826      **
014700130826      * quindi chiude il TIVGD quando il chiamante è andato in EoF
014800130826     c                   elseif           flagHDE = 'E'
014900130827     c                   exsr      FINE_MSG
015000130826     C                   EXSR      vgd_CLOSE
015100130826      **
015200130826     c                   COMMIT
015300130826      **
015400130826      * chiude definitivamente dopo aver Committato
015500130826     c                   seton                                        LR
015600130826     c                   setoff                                         RT
015700130826     c                   end
015800130826      **
015900130826     C                   Return
016000130826     C*--------------------------------------------------------------*
016100130826     C* scrive la TESTATA
016200130826     C*--------------------------------------------------------------*
016300130827     C     INIZIO_MSG    BEGSR
016400130826     C*
016500140423     c                   eval      segmento = '#FILE;GEODATA_SHPNOT_0820_DRK_D'
016600140423     c                             + %editc(WOGGI:'X') +'T'+ %editc(WORA:'X')
016700140307     c                             + ';'
016800130826     c                   exsr      VGD_WRITE
016900130826     C*
017000140307     c                   eval      segmento = '#ENCODING;ISO 8859-1;'
017100130826     c                   exsr      VGD_WRITE
017200130826     C*
017300170831     c                   eval      segmento = '#VERSION;03.01;'
017400130826     c                   exsr      VGD_WRITE
017500130826     C*
017600140307     c                   eval      segmento = '#DEF;GEODATA:HEADER;VERSION;'+
017700140307     c                             'CLASSIFICATION;;'
017800130826     c                   exsr      VGD_WRITE
017900130826     C*
018000140307     c                   eval      segmento = '#DEF;GEODATA:SHIPMENT;NUMORDER;'+
018100130828     c                             'MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MPSCRE'+
018200130828     c                             'F1;MPSCREF2;MPSCREF3;MPSCREF4;MPSBILLREF;M'+
018300130828     c                             'PSCOUNT;MPSVOLUME;MPSWEIGHT;SDEPOT;CDATE;C'+
018400130828     c                             'TIME;HARDWARE;RDEPOT;DSORT;SPTDATE;SPTTIME'+
018500130828     c                             ';SPTREALDATE;SPTREALTIME;DELMODALLOW;ROUTI'+
018600170901     c                             'NGPLANVERSION;SPARTNERREF1;SPARTNERREF2;SP'+
018700170901     c                             'ARTNERCODE;OSORT;SSORT;MSORT;COLREQUESTFLA'+
018800170901     c                             'G;ROUTINGPLACE;;'
018900130826     c                   exsr      VGD_WRITE
019000130826     C*
019100140307     c                   eval      segmento = '#DEF;GEODATA:SENDER;NUMORDER;'+
019200130828     c                             'SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPNAME'+
019300130828     c                             ';SUNIQCUSTID;SNAME1;SNAME2;SSTREET;SPROPNU'+
019400130828     c                             'M;SADD2;SADD3;SFLOOR;SBUILDING;SDEPARTMENT'+
019500130828     c                             ';SCOUNTRYCODE;SSTATE;SZIPCODE;STOWN;SCONTA'+
019600170901     c                             'CT;SPHONE;SFAX;SEMAIL;SCOMMENT;SGLN;SGPSLA'+
019700170901     c                             'T;SGPSLONG;SACCOUNTOWNER;;'
019800130826     c                   exsr      VGD_WRITE
019900130827     C*
020000140307     c                   eval      segmento = '#DEF;GEODATA:RECEIVER;NUMORDER;'+
020100130828     c                             'RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPNAME2'+
020200130828     c                             ';RPUDOID;RSTREET;RPROPNUM;RADD2;RADD3;RFLO'+
020300130828     c                             'OR;RBUILDING;RDEPARTMENT;RCOUNTRYCODE;RSTA'+
020400130828     c                             'TE;RZIPCODE;RTOWN;RDOORCODE;RCONTACT;RCONT'+
020500130828     c                             'ACTPHO1;RCONTACTPHO2;RINTERPHONENAME;RFAX;'+
020600170901     c                             'REMAIL;RCOMMENT;RGPSLAT;RGPSLONG;;'
020700130827     c                   exsr      VGD_WRITE
020800130827      *
020900140307     c                   eval      segmento = '#DEF;GEODATA:PARCEL;NUMORDER;'+
021000130828     c                             'PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRANK;'+
021100130828     c                             'SENDPARCELREF1;SENDPARCELREF2;SENDPARCELRE'+
021200130828     c                             'F3;SENDPARCELREF4;RECPARCELREF;SERVICECODE'+
021300170901     c                             ';PPARTNERREF1;PPARTNERREF2;PPARTNERREF3;PP'+
021400170901     c                             'ARTNERREF4;DIMENSION;DECLAREDWEIGHT;MEASUR'+
021500170901     c                             'EDWEIGHT;HINSAMOUNT;HINSCURRENCY;HINSCONTE'+
021600170901     c                             'NT;HAZLQ;HZDPACKCODE;OPCODE;PCONTENT;ORIGI'+
021700170901     c                             'NPARCELNUMBER;POWNERBU;PPARTNERCODE;ASCODE'+
021800170901     c                             ';BAGNO;;'
021900130827     c                   exsr      VGD_WRITE
022000130827      *
022100140307     c                   eval      segmento = '#DEF;GEODATA:MSG;NUMORDER;'+
022200130828     c                             'NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE'+
022300130828     c                             ';MSGDESTTYPE;MSGDESTINATION;MSGTRIGGER;MSG'+
022400140307     c                             'LANG;MSGSENDERURL;;'
022500130827     c                   exsr      VGD_WRITE
022600130827      *
022700170831     c                   eval      segmento = 'HEADER;03.01;SHPNOT;'
022800130827     c                   exsr      VGD_WRITE
022900130827     C*
023000130826     C                   endSR
023100130826     C*--------------------------------------------------------------*
023200130826     C* scrive la CODA
023300130826     C*--------------------------------------------------------------*
023400130827     C     FINE_MSG      BEGSR
023500130826     C*
023600130826     c                   eval      segmento = '#END;'
023700130826     c                   exsr      VGD_WRITE
023800130826     C*
023900130826     C                   endSR
024000960530     C*--------------------------------------------------------------*
024100020219     C* dettaglio spedizione
024200960530     C*--------------------------------------------------------------*
024300130827     C     SPEDIZIONE    BEGSR
024400020227     C*
024500130826     C                   MOVEL     KPJBU         TRTC831
024600130826     C*
024700130826     C*  Se c'è numero spedizione scrivo dettaglio
024800130826    2C     F83NSP        IFNE      0
024900130826     C*
025000130826     C*  Eseguo posizionamento su archivio bolle per passaggio param.
025100130827      * B O L L A
025200130826     C                   Z-ADD     F83AAS        KAAS
025300130826     C                   Z-ADD     F83LNP        KLNP
025400130826     C                   Z-ADD     F83NRS        KNRS
025500130826     C                   Z-ADD     F83NSP        KNSP
025600130826     C     KARB          CHAIN     FNARB01L                           31
025700130827    3C                   IF        %Found(FNARB01L)
025800130827     C* dati bolla
025900130827     C                   exsr      DECODIFICHE
026000130827     C*   su più righe
026100130827     c                   exsr      SHIPMENT                                      livello 3
026200130827     c                   exsr      SENDER                                        livello 4
026300130827     c                   exsr      RECEIVER                                      livello 4
026400130827     c                   exsr      PARCEL                                        livello 4
026500130827     C* il messaggio MSG è all'interno del PARCEL (strutturalmente)              livello 5
026600130826    2C                   END
026700130828     c                   end
026800130828     C*
026900130826     C*
027000130827    2C                   ENDSR
027100130826     C*--------------------------------------------------------------*
027200130827     C* dettaglio spedizione
027300130826     C*--------------------------------------------------------------*
027400130827     C     SHIPMENT      BEGSR
027500130827     C*
027600130827     C* parcel MASTER
027700140307     c                   eval      segmento = 'SHIPMENT;3;'
027800140307     c                             + %trim(§B4IPN)
027900140307     c                             +';'
028000140307     c                             + %trim(bolla)
028100140307     c                             + ';;0;'
028200140307     c                             + %trim(BOLLA)
028300140307     c                             +';'
028400140307     c                             + %Trim(ARBRMA)
028500140307     c                             +';'
028600140307     c                             + %trim(%EDITC(ARBRMN:'Z'))
028700140307     c                             + ';;;'
028800140307     c                             + %trim(%EDITC(ARBNCL:'Z'))
028900140307     c                             + ';;'
029000140307     c                             + %trim(%EDITC(wpes100:'Z'))
029100140307     c                             + ';0820;'
029200140307     c                             + Anno
029300140307     c                             + %EDITC(arbMGS:'X')
029400140307     c                             + ';090000;I;;;'
029500140307     c                             + %editc(WOGGI:'X')
029600140307     c                             + ';'
029700140307     c                             + %editc(WORA:'X')
029800170904     c                             + ';;;0;;;;;;;'
029900170904     c                             + %Trim(fld_daORM)
030000170904     c                             + ';;;'
030100130827     c                   exsr      VGD_WRITE
030200130827     C*
030300130827    2C                   ENDSR
030400130827     C*--------------------------------------------------------------*
030500130827     C* dettaglio Mittente
030600130827     C*--------------------------------------------------------------*
030700130827     C     SENDER        BEGSR
030800130827     C*
030900130827     C* SENDER
031000140307     c                   eval      segmento = 'SENDER;4;;;;;'
031100140307     c                             + %Trim(fldRSM)
031200140307     c                             + ';;'
031300140307     c                             + %Trim(fldINM)
031400140307     C                             + ';;;;;;;380;;'
031500140307     C                             + %Trim(%editc(fldCPM:'X'))
031600140307     c                             + ';'
031700140307     c                             + %Trim(fldLOM)
031800140307     c                             + ';;'
031900140307     c                             + %Trim(fldTLM)
032000170901     c                             + ';;;;;;;;'
032100130828     c                   exsr      VGD_WRITE
032200130828     C*
032300130828    2C                   ENDSR
032400130827     C*--------------------------------------------------------------*
032500130827     C* dettaglio Destinatario
032600130827     C*--------------------------------------------------------------*
032700130827     C     RECEIVER      BEGSR
032800130827     C*
032900130827     C* RECEIVER
033000140307     c                   eval      segmento = 'RECEIVER;4;;'
033100140307     c                             + %Trim(fldRSD)
033200140307     c                             + ';;'
033300140307     c                             + %Trim(fldRSD)
033400140307     c                             + ';'
033500140307     C                             + %Trim(fldRSD2)
033600140307     C                             + ';;'
033700140307     C                             + %Trim(fldIND)
033800140307     C                             + ';;;;;;;'
033900140307     C                             + %Trim(fldNZD)
034000140307     C                             + ';;'
034100140307     C                             + %Trim(fldCAD)
034200140307     C                             + ';'
034300140307     C                             + %Trim(fldLOD)
034400140307     C                             + ';;'
034500140307     c                             + %Trim(fldCTA)
034600140307     c                             + ';'
034700140611     c                             + %Trim(fldTELcontact)
034800140617     c                             + ';;;;;'
034900150219     c                             + %TrimL(fldNOTE1)
035000150219     c                             + %TrimR(fldNOTE2)
035100170901     c                             + ';;;'
035200130827     c                   exsr      VGD_WRITE
035300130827     C*
035400130827    2C                   ENDSR
035500130827     C*--------------------------------------------------------------*
035600130827     C* dettaglio collo
035700130827     C*--------------------------------------------------------------*
035800130827     C     PARCEL        BEGSR
035900130828
036000130828      * reC 'I'  PARCEL  (se un domani il multicollo)
036100130828     c                   clear                   DSBL4i
036200130828     c                   move      'I'           ktrc
036300130828     c     Kar4          setll     fiar401l
036400130828     c     Kar4          reade     fiar401l
036500130828     c                   dow       not %EoF(fiar401l)
036600130828     c                   movel     AR4not        DSBL4i
036700130828      **
036800130828     c                   clear                   srvCODE
036900150709     c                   eval      srvCODE = §B4ISC
037000150709     c                   eval      nPARCEL = §B4IPN
037100130828     C* PARCEL
037200140307     c                   eval      segmento = 'PARCEL;4;'
037300150709     c                             + %Trim(nPARCEL)
037400140307     c                             + ';;;'
037500140307     c                             + %Trim(BOLLA)
037600140307     c                             + ';'
037700140307     c                             + %Trim(arbRMA)
037800140307     c                             + ';'
037900140307     c                             + %trim(%EDITC(ARBRMN:'Z'))
038000140307     C                             + ';;;'
038100150709     C                             + %Trim(srvCODE)
038200170901     C                             + ';;;;;;'
038300140307     c                             + %trim(%EDITC(WPes100:'Z'))
038400140307     c                             + ';'
038500140307     c                             + %trim(%EDITC(WPes100:'Z'))
038600170901     c                             + ';;;;;;;;;;;;;'
038700130828     c                   exsr      VGD_WRITE
038800130828     C*
038900140617     c                   if        serv_mail_Attivato = 'S'
039000130828     c                   exsr      MSG
039100140617     c                   end
039200130828      **
039300130828     c     Kar4          reade     fiar401l
039400130828     c                   end
039500130827     C*
039600130827    2C                   ENDSR
039700130827     C*--------------------------------------------------------------*
039800130828     C*  Se vi è un messaggio da comunicare è legato al PARCEL
039900130827     C*--------------------------------------------------------------*
040000130828     C     MSG           BEGSR
040100130827      *
040200130828     C* MSG   invio indirizzo Mail
040300140617      *     (è stato provato solo x la MAIL e non per l'SMS)
040400140617      *
040500140617      *   ATTENZIONE provato solo con la MAIL  / l'SMS va implementato
040600140617      *              se si deciderà di attivare il servizio con DPD (a pagamento)
040700140617      *
040800130828     c                   if        fldEML <> *blank
040900140307     c                   eval      segmento = 'MSG;5;'
041000150623     c                             + ';;1;0;'
041100140307     c                             + %trim(fldEML)
041200140307     c                             + ';904;;;'
041300130828     c                   exsr      VGD_WRITE
041400130828     c                   end
041500170822      *
041600170822     c                   if        fldEML1<> *blank
041700170822     c                   eval      segmento = 'MSG;5;'
041800170822     c                             + ';;1;0;'
041900170822     c                             + %trim(fldEML1)
042000170822     c                             + ';904;;;'
042100170822     c                   exsr      VGD_WRITE
042200170822     c                   end
042300130828     C*
042400150617      *
042500150617     c                   if        fldSMS <> *blank
042600150617     c                   eval      segmento = 'MSG;5;'
042700150624     c                             + ';;3;0;'
042800150617     c                             + %trim(fldSMS)
042900150617     c                             + ';904;;;'
043000150617     c                   exsr      VGD_WRITE
043100150617     c                   end
043200170822      *
043300150617     C*
043400130828    2C                   ENDSR
043500130828     C*--------------------------------------------------------------*
043600130828     C* dettaglio spedizione dagli archivi BRT
043700130828     C*--------------------------------------------------------------*
043800130828     C     DECODIFICHE   BEGSR
043900130828      *
044000130827      * con la LNA si iidentificano i dati del Mittente (filiale di Lancio Manifest)
044100130828     c                   clear                   fldRSM
044200130828     c                   clear                   fldINM
044300130828     c                   clear                   fldCPM
044400130828     c                   clear                   fldLOM
044500130828     c                   clear                   fldPRM
044600130828     c                   clear                   fldTLM
044700130828     c                   clear                   fldFXM
044800130827     c                   clear                   fldRSD
044900130827     c                   clear                   fldRSD2
045000130827     c                   clear                   FldIND
045100130827     c                   clear                   fldCAD
045200130827     c                   clear                   fldLOD
045300130827     c                   clear                   fldPRD
045400130827     c                   clear                   fldNZD
045500130827     c                   clear                   fldCTA
045600130827     c                   clear                   fldTEL
045700140611     c                   clear                   fldPREF
045800140611     c                   clear                   fldTELcontact
045900130827     c                   clear                   fldEML
046000170822     c                   clear                   fldEML1
046100130827     c                   clear                   fldSMS
046200140617     c                   clear                   fldSMScontact
046300150218     c                   clear                   fldNOTE1
046400150218     c                   clear                   fldNOTE2
046500170904     c                   movel     '0'           fld_daORM         1
046600140310     C*
046700140310     C* Rif.spedizione BRT
046800140310     c                   move      arbaas        anno              4
046900140310     c                   move      arblnp        linea             3
047000140310     c                   move      arbnrs        serie             2
047100140310     c                   move      arbnsp        numero            7
047200130827
047300130828     c                   move      bolla16       bolla            14
047400130828
047500130827      * reC 'D'  RAG.SOCIALE 2
047600130827     c                   move      'D'           ktrc
047700130827     c     Kar4          chain     fiar401l
047800130827     c                   if        %found(fiar401l)
047900130827     c     ';':'.'       xlate     ar4not        fldRSD2
048000130827     c                   end
048100130827
048200130827      * reC 'I'  PARCEL
048300130827     c                   clear                   DSBL4i
048400130827     c                   move      'I'           ktrc
048500130827     c     Kar4          chain     fiar401l
048600130827     c                   if        %found(fiar401l)
048700130827     c                   movel     AR4not        DSBL4i
048800130827     c                   end
048900130827      *
049000170904      * reC 'M'  se la spedizione nasce da un ORM
049100170904     c                   move      'M'           ktrc
049200170904     c     Kar4          chain     fiar401l
049300170904     c                   if        %found(fiar401l)
049400170904      *
049500170904     c                   eval       tbeCOD = 'LPD'
049600170904     c                   eval       tbeKE1 = %subst(ar4not:1:3)
049700170904     c     Ktbe1         chain     tntbe01l
049800170904     c                   if        %found(tntbe01l)
049900170904     c                   movel     '1'           fld_daORM
050000170904     c                   end
050100170904      *
050200170904     c                   end
050300170904      *
050400130827     C*  reperisce il volume/peso da CML o da Bollettazione
050500130827     c                   exsr      VOL_PES
050600130827      * mittente
050700130827     c     arbLNA        chain     azorg01l
050800130827     c                   if        %Found(azorg01l)
050900130828     c     ';':'.'       xlate     orgDES        fldRSM
051000130828     c     ';':'.'       xlate     orgIND        fldINM
051100130828     c                   z-add     orgCPF        fldCPM
051200130828     c     ';':'.'       xlate     orgLOC        fldLOM
051300130828     c     ';':'.'       xlate     orgPRO        fldPRM
051400130828     c     ';':'.'       xlate     orgTEL        fldTLM
051500130828     c     ';':'.'       xlate     orgFAX        fldFXM
051600130827     c                   end
051700130827      *
051800130827     C* Destinatario
051900130827     c     ';':'.'       xlate     arbRSD        fldRSD
052000130827     c     ';':'.'       xlate     arbIND        fldIND
052100130827     c     ';':'.'       xlate     arbCAD        fldCAD
052200130827     c     ';':'.'       xlate     arbLOD        fldLOD
052300130827     c     ';':'.'       xlate     arbPRD        fldPRD
052400140424      *
052500140424      **** DPD non vuole avere i CAP con degli spazi in mezzo
052600140424      ** quindi compatto il CAP ove vi fossero degli spazi
052700140424     c                   clear                   compat9CAP        9
052800140424     c                   clear                   nr                3 0
052900140424      * compatta i 9 caratteri togliendo i blank fra una sigla e l'altra
053000140424     c                   DO        9             cap               3 0
053100140424     c                   if        %subst(fldCAD:cap:1) <> ' '
053200140424     c                   eval      nr = nr + 1
053300140424     c                   eval      %subst(compat9CAP:nr:1)
053400140424     c                             =  %subst(fldCAD:cap:1)
053500140424     c                   end
053600140429     c                   endDO
053700140424     c                   eval      fldCAD = compat9CAP
053800140429      *
053900140429      * Queste specifiche sull'Irlanda erano sul FIEU16R:
054000140429      *  (CHIODO xchè Irlanda non ha il CAP)
054100140429      *   Controlla il CAP se scritto "EIRE" (Irlanda) deve essere sostituito
054200140429      *   con "0000001". (Richiesta di Villa da Meeting del 14/10/2008 con DPD.
054300140429     C                   if        fldCAD = 'EIRE'
054400140506      * e se non è DUBLIN deve essere "0000002"
054500140623      *    dopo una mail di rettifica del 23/6/2014 si deve inviare solo 1 o 2
054600140623      *    formalmente nel cap di DUBLINO e IRLANDA
054700140623      *  1 --> RDEPOT= 1601    2 --> RDEPOT= 1602
054800140623      *
054900140623     c                   eval      fldCAD = '2'
055000140506      * ma solo    DUBLIN deve essere "0000001"
055100140506     c     'DUBLIN'      SCAN      fldLOD
055200140506     c                   if        %Found
055300140623     c                   eval      fldCAD = '1'
055400140506     c                   end
055500140429     c                   end
055600130827      *
055700130827     C* Nazione Destinatario
055800130827     C                   Z-ADD     1             YY                3 0
055900130827     C     arbNZD        LOOKUP    C15(YY)                                32
056000130827     C   32ISO(YY)       COMP      *BLANKS                            3232
056100140611     c                   if        *in32
056200140611     C                   MOVEL     CIE(YY)       fldNZD            3
056300140611      * se c'è il pref.internazionale in tabella Nazioni
056400140611      * imposta il codice preceduto dal '+'
056500140611     c                   if        PRF(YY) <> *blank
056600140611     C                   MOVEL     PRF(YY)       fldPREF
056700140611     c                   end
056800140611     c                   end
056900130827      *
057000031210     c                   movel     'GEN'         ktrd
057100031210     c                   clear                   DAr5Gen
057200031210     c     kar5          chain     fiar501L                           34
057300031210     c                   if        %Found(fiar501L)
057400031210     c                   Movel     Ar5Uni        DAr5Gen
057500130827     c     ';':'.'       xlate     §ar5ref       fldCTA
057600130827     c     ';':'.'       xlate     §ar5teld      fldTEL
057700140612     c                   movel     fldTEL        input16          16
057800140612     c                   exsr      compatta_NR
057900140612     c                   movel     out16         fldTEL
058000140611      * se c'è già il prefisso
058100140611     c                   if        %subst(fldTEL:1:1) = '+' or
058200140611     c                             %subst(fldTEL:1:2) = '00'
058300140611     c                   eval      fldTELcontact = fldTEL
058400140611     c                   else
058500140611      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
058600140612     c                   if        fldPREF <> *blank and fldTEL <> *blank
058700140611     c                   eval      fldTELcontact = '+'
058800140612     c                               + %Trim(fldPREF) + %Trim(fldTEL)
058900140611     c                   end
059000140611     c                   end
059100031211     c                   end
059200130827      *
059300140617      * solo se il servizio verrà attivato (al momento salta decodifica)
059400140617      *  nelle definizioni il flag è inizializzato a (N)
059500140617      *
059600140617      *  EMAIL o SMS - come servizio di Alert di consegna -
059700140617      *     ATTENZIONE il programma al momento ha gestito solo la MAIL e
059800140617      *        NON l'SMS --> che dovrà essere implementato se si deciderà
059900140617      *                      l'attivazione del servizio.
060000140617     c                   if        serv_mail_Attivato = 'S'
060100140617      *-
060200130827     c                   movel     'EMD'         ktrd
060300130827     c                   clear                   DAr5emd
060400130827     c     kar5          chain     fiar501L                           34
060500130827     c                   if        %Found(fiar501L)
060600130827     c                   Movel     Ar5Uni        DAr5emd
060700170822     c                   z-add     0             position          3 0
060800170822     c     ';'           scan      §ar5EML:1     position
060900170822     c                   if        position > 0
061000170822     c                   eval      fldEML1= %subst(§ar5EML:position+1)
061100170822     c                   eval      fldEML = %subst(§ar5EML:1:position-1)
061200170822     c                   else
061300170822     c                   eval      fldEML= §ar5EML
061400170822     c                   end
061500170822      **
061600140617     c     ';':' '       xlate     §ar5TEL       fldSMS
061700140617     c                   movel     fldSMS        input16          16
061800140617     c                   exsr      compatta_NR
061900140617     c                   movel     out16         fldSMS
062000140617      * se c'è già il prefisso
062100140617     c                   if        %subst(fldSMS:1:1) = '+' or
062200140617     c                             %subst(fldSMS:1:2) = '00'
062300140617     c                   eval      fldSMScontact = fldSMS
062400140617     c                   else
062500140617      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
062600140617     c                   if        fldPREF <> *blank and fldSMS <> *blank
062700140617     c                   eval      fldSMScontact = '+'
062800140617     c                               + %Trim(fldPREF) + %Trim(fldSMS)
062900140617     c                   end
063000140617     c                   end
063100130827     c                   end
063200140617      *-
063300140617     c                   endIF
063400020219     C*----------------
063500020220     C*
063600020219     C*----------------
063700020219     C*  Controllo se ci sono delle note di consegna
063800020219     C                   MOVEL     '8'           KTRC
063900060213     C     KAR4          CHAIN     fiar401L                           32
064000020220     C*  Shipment Ref.notes (1°campo note)             solo se c'è
064100150218     c  n32';':'.'       xlate     ar4not        fldnote1
064200030226      *
064300150218     C                   MOVEL     '9'           KTRC
064400150218     C     KAR4          CHAIN     fiar401L                           32
064500150218     C*  Shipment Ref.notes (2°campo note)             solo se c'è
064600150218     c  n32';':'.'       xlate     ar4not        fldnote2
064700030226      **
064800020220     C*----------------
064900020226      *
065000020219     c                   endsr
065100020227     C*****************************************************************
065200140612     C*  Compatta il campo numero di telefono avvicinando i numeri
065300030317     C*****************************************************************
065400140612     C     compatta_NR   BEGSR
065500030317      *
065600140612     c                   z-add     0             in                3 0
065700140612     c                   clear                   Byte_uno          1
065800140612     c                   clear                   out16            16
065900140612      *
066000140612      *  controlla presenza di caratteri divisori o blank
066100140612      *   li elimina e deve compattare il numero
066200140612     c                   do        16            Nx                3 0
066300140612     c                   eval      Byte_uno = %subst(input16:Nx:1)
066400140612      * controlla che NON sia un numero :
066500140612     c                   move      'N'           alfa              1
066600140612     c     digits        check     Byte_uno
066700140612     c                   if        %found
066800140612     c                   move      'S'           alfa
066900140612     c                   end
067000140612      *
067100140612      * tranne sul primo che può esserci il '+'
067200140612     c                   if        Nx = 1 and Byte_uno ='+'
067300140612     c                             or alfa = 'N'
067400140612     c                   add       1             in
067500140612     c                   eval      %subst(out16:in:1) = %subst(input16:Nx:1)
067600140612     c                   end
067700140612     c                   enddo
067800140612      *
067900140612     c                   endsr
068000140612     C*****************************************************************
068100140612     C*  Reperisce Peso/Volume o da CML o da Bollettato
068200140612     C*****************************************************************
068300140612     C     VOL_PES       BEGSR
068400140612      *
068500030317      * Prima cerca il volume fra bollettato e CML
068600050112     C     arbncR        ifeq      arbncl
068700030317      * se è considerata un'unica spedizione
068800030317     C                   z-add     ARBVLC        WVOL
068900030317     C                   else
069000030317     C*
069100030317     C     ARBVLC        ifge      ARBVLb
069200030317     C                   z-add     ARBVLC        WVOL
069300030317     C                   else
069400030317     C                   z-add     ARBVLb        WVOL
069500030317     C                   end
069600030317     C*
069700030317     C                   endif
069800030317      *
069900030521     C* PESO VDL - TARA
070000030521     C     §QTTPC        MULT      arbncp        WTARA
070100030521     C                   Z-ADD     arbPKC        wpes
070200150825     c                   sub(h)    wtara         wpes
070300030521      *
070400030317      * Poi cerca il peso fra bollettato e CML
070500050112     C     arbncP        ifeq      arbncl
070600030317      * se è considerata un'unica spedizione
070700030317     C                   ELSE
070800030521     C*  altrimenti se il bollettato è superiore passo il Bollettato.
070900030521     C     wpes          iflt      arbPKb
071000030317     C                   Z-ADD     arbPKb        wpes
071100030317     C                   end
071200030317     C*
071300030317     C                   ENDIF
071400150624     C*
071500150624     C* Se il peso è superiore al limite del fuori misura impostiamo fisso 31 kg.
071600150624     c                   if        wpes > 31,5
071700150624     c                   eval      wpes = 31
071800150624     C                   end
071900150624     C*
072000130827     c                   eval      Wpes100 = ( wpes * 100)
072100030317     C*
072200030317     C                   ENDSR
072300130826      **?__________________________________________________________________ */
072400130826      *    Apertura del TIVGD
072500130826      **?__________________________________________________________________ */
072600130826     C     VGD_OPEN      Begsr
072700130826      *
072800130826      *  recupera il progressivo
072900130826     c                   exsr      Piglia_progr
073000130826      *
073100130826      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
073200130826     C                   clear                   trul47ds
073300130826     C                   eval      d47opz  = 'I'
073400130826     C                   eval      d47tip  = TipoF
073500130826     C                   eval      d47lck  = 'N'
073600130826     C                   eval      d47chkj = 'N'
073700130826     C                   eval      d47pgm  = 'TRTC83R13'
073800130826     C                   call      'TRUL47R'
073900130826     C                   parm                    trul47ds
074000130826      *
074100130826     c                   endsr
074200130826      **?__________________________________________________________________ */
074300130826      *   Scrittura del TIVGD
074400130826      **?__________________________________________________________________ */
074500130826     C     VGD_WRITE     Begsr
074600130826     c                   eval       Conta_righe = 1 + Conta_righe
074700130826      *
074800130826     c                   clear                   tivgd000
074900130826     c                   eval      vgddta = %TrimR(Segmento)
075000130826     c                   eval      vgdtip = TipoF
075100130826     c                   eval      vgdksu = CLIENTE
075200130826     C                   eval      vgdprg = Progressivo
075300130826     c                   eval      vgdtsc = 'WW'
075400130826     c                   eval      vgdpgm = 'TRTC83R13'
075500130828     c                   eval      vgddat = woggi
075600130826      *
075700130826     C                   WRITE     tivgd000
075800130826      *
075900130826     c                   endsr
076000130826      **?__________________________________________________________________ */
076100130826      *    Chiusura del TIVGD
076200130826      **?__________________________________________________________________ */
076300130826     C     VGD_CLOSE     Begsr
076400130826      *
076500130826     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
076600130826     C                   clear                   trul47ds
076700130826     C                   eval      d47opz  = 'F'
076800130826     C                   eval      d47tip  = TipoF
076900130826     C                   call      'TRUL47R'
077000130826     C                   parm                    trul47ds
077100130826      *
077200130826     c                   endsr
077300130826      **?__________________________________________________________________ */
077400130826      *   Prende il Progressivo
077500130826      **?__________________________________________________________________ */
077600130826     C     Piglia_progr  Begsr
077700130826      *
077800130826     C                   MOVEL     CLIENTE       virKSC
077900130826     C                   MOVEL     TipoF         virTIP
078000130826     c     kvir02        chain     tivir02l
078100130828     c                   move      'OF'          Var_ISV
078200130826     c                   if        %Found(tivir02l)
078300130826     c                   move      VirFI1        Var_ISV
078400130826     C                   End
078500130826
078600130826      *       prende numeratore strategi
078700130826     C                   exsr      calprog
078800130826      *
078900130826     c                   endsr
079000130826      **?__________________________________________________________________ */
079100130826      *
079200130826      **?__________________________________________________________________ */
079300130826     C     calprog       begsr
079400130826     C*
079500130828     c                   clear                   progressivo      10
079600130826     C                   open      tis7prgf
079700130826     C                   read(e)   tis7prgf
079800130826     C*
079900130826     C                   if        not %error
080000130826     C                   eval      dwlprg = f_tis7prgf
080100130826     C*
080200130826     C                   move(p)   dwlprg        wrkprg
080300130826     C                   add       1             wrkprg
080400130826     C                   move(p)   wrkprg        dwlprg
080500130826     C                   movel     Var_ISV       dwlprg
080600130826     C*
080700130826     C                   eval       f_tis7prgf = dwlprg
080800130826     C                   eval      Progressivo = dwlprg
080900130826     C*
081000130826     C                   update    tis7prg0
081100130826     C                   endif
081200130826     C*
081300130826     C                   close     tis7prgf
081400130826     C*
081500130826     C                   endsr
081600130826      **?__________________________________________________________________ */
081700130826      **?__________________________________________________________________ */
081800130826     C     *INZSR        BEGSR
081900130826     C*
082000130826     C* Definisco chiavi di accesso
082100130828     c*
082200130828     c     kvir02        klist
082300130828     C                   kfld                    virKSC
082400130828     C                   kfld                    virTIP
082500130828     c*
082600130826     C     KARB          KLIST
082700130826     C                   KFLD                    KAAS
082800130826     C                   KFLD                    KLNP
082900130826     C                   KFLD                    KNRS
083000130826     C                   KFLD                    KNSP
083100130827      *
083200130827     C     KAR4          KLIST
083300130827     C                   KFLD                    KAAS
083400130827     C                   KFLD                    KLNP
083500130827     C                   KFLD                    KNRS
083600130827     C                   KFLD                    KNSP
083700130827     C                   KFLD                    KTRC              1
083800130826      *
083900130826     C     KAR5          KLIST
084000130826     C                   KFLD                    KAAS
084100130826     C                   KFLD                    KLNP
084200130826     C                   KFLD                    KNRS
084300130826     C                   KFLD                    KNSP
084400130826     C                   KFLD                    KTRD              3
084500130826      *
084600130826     C     KTAB          KLIST
084700130826     C                   KFLD                    KKUT
084800130826     C                   KFLD                    KCOD
084900130826      *
085000130826     C     KTAB1         KLIST
085100130826     C                   KFLD                    KKUT
085200130826     C                   KFLD                    KCOD
085300130826     C                   KFLD                    KKEY
085400170904      *
085500170904     C     KTBE1         KLIST
085600170904     C                   KFLD                    tbecod
085700170904     C                   KFLD                    tbeke1
085800130826      *
085900130826      *------------
086000130826     C* Inizializzo variabili totale
086100130826     C* Imposto data del giorno
086200130826     C                   TIME                    W0140            14 0
086300130826     C                   MOVE      W0140         G02DAT            8 0
086400130826     C                   MOVEL     W0140         Wora              6 0
086500130826     C                   MOVEL     *BLANK        G02ERR
086600130826     C                   CALL      'XSRDA8'
086700130826     C                   PARM                    WLBDA8
086800130826     C                   MOVEL     G02INV        WOGGI             8 0
086900130826      *
087000130826     C** CARICAMENTO DATI VARI TASSAZIONE
087100130826     C                   Z-ADD     1             KKUT
087200130826     C                   MOVEL     'QT'          kcod
087300130826     C                   MOVEL     *BLANKS       kKEY
087400130826     C                   MOVEL     '1'           kKEY
087500130826     C     KTAB1         CHAIN     TABEL00F                           30
087600130826     C  N30              MOVEL     TBLUNI        DSQT1
087700130826     C*
087800130826      *------------
087900130826     C* Caricamento Tabella codici nazioni
088000130826     C                   Z-ADD     0             X                 3 0
088100130826     C                   MOVEL     '15'          KCOD
088200130826     C                   Z-ADD     1             KKUT
088300130826     C     KTAB          CHAIN     TABEL00F                           30
088400130826     C     *IN30         DOWEQ     '0'
088500130826     C     TBLFLG        IFEQ      *BLANKS
088600130826     C                   ADD       1             X
088700130826     C                   MOVEL     TBLKEY        C15(X)
088800130826     C                   MOVEL     TBLUNI        DS15
088900130826     C                   MOVEL     §15COD        ISO(X)
089000130826     C                   MOVEL     §15des        DNZ(X)
089100130826     C                   MOVEL     §15ACN        ACN(X)
089200130828     C                   MOVEL     §15CIE        CIE(X)
089300140611     C                   MOVEL     §15PREF       prf(X)
089400130826     C                   END
089500130826     C     KTAB          READE     TABEL00F                               30
089600130826     C                   END
089700130826      *------------
089800130826     C*
089900130826     C                   ENDSR
090000130826      **?__________________________________________________________________ */
