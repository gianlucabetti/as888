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
001100020219     FAzOrg01L  IF   E           K DISK
001200130725      ***
001300130826     ftivgd00f  if a E             DISK    commit
001400130826      *
001500130826     Ftivir02L  IF   E           K DISK
001600130826     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001700130826     F                                     PREFIX(f_) usropn
001800140617      *---------------------------------------------------------------------*
001900140617      **  Flag di attivazione
002000140617      *---------------------------------------------------------------------*
002100140617      * il servizio mail di alert è predisposto
002200140617      *    ma NON attivato poichè adesso impostato a (N) --> non decodifica i dati
002300140617      *    né esegue la scrittura del record "MSG" xchè con DPD non è attivo il servizio.
002400140617      *
002500140617     d  serv_mail_Attivato...
002600150617     d                 s              1    INZ('S')
002700140617      **
002800000207      *---------------------------------------------------------------------*
002900000207      * Schiere
003000000207      *---------------------------------------------------------------------*
003100020220     D Num             S              1    DIM(11)
003200000207      * Codici ISO nazioni x E.D.I.
003300011217     D C15             S              3    DIM(500)
003400011217     D ISO             S              3    DIM(500)
003500130828     D CIE             S              3  0 DIM(500)
003600020219     D DNZ             S             25    DIM(500)
003700020412     D ACN             S              1    DIM(500)
003800140611     D PRF             S              4    DIM(500)
003900000207      *---------------------------------------------------------------------*
004000000207      * DS
004100000207      *---------------------------------------------------------------------*
004200130827     D DSBL4i        E DS
004300031210     D dar5gen       e ds
004400130827     D dar5emd       e ds
004500020227     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
004600960530     D DS3A          E DS
004700030521     D DSQT1         E DS                  INZ
004800960822     D DS15          E DS
004900130828     D DDPP          E DS
005000950428     D KPJBA         E DS
005100130827      *
005200130828     D BOLLA16         DS                  INZ
005300130827     d  anno                          4
005400130827     d  linea                         3
005500130827     d  serie                         2
005600130827     d  numero                        7
005700130827      *
005800960530     D WLBDA8          DS                  INZ
005900950428     D  G02DAT                 1      8  0
006000950428     D  G02INV                 9     16  0
006100950428     D  G02ERR                17     17
006200950428     D  G02TGI                18     22  0
006300130826     d  KAAS           s                   like(ARBAAS)
006400130826     d  KLNP           s                   like(ARBLNP)
006500130826     d  KNRS           s                   like(ARBNRS)
006600130826     d  KNSP           s                   like(ARBNSP)
006700130826     d  KTRC           s                   like(AR4TRC)
006800130826     d  KKUT           s                   like(TBLKUT)
006900130826     d  KCOD           s                   like(TBLCOD)
007000130826     d  KKEY           s                   like(TBLKEY)
007100130828     D Var_ISV         s                   like(Virfi1)
007200130828     d  fldRSM         s                   like(orgDES)                            fldDES
007300130828     d  fldINM         s                   like(orgIND)                            fldIND
007400130828     d  fldCPM         s              5  0
007500130828     d  fldLOM         s                   like(orgLOC)                            fldLOC
007600130828     d  fldPRM         s                   like(orgPRO)                            fldPRO
007700130828     d  fldTLM         s                   like(orgTEL)                            fldTEL
007800130828     d  fldFXM         s                   like(orgFAX)                            fldFAX
007900130827     d  fldRSD         s                   like(arbRSD)
008000130827     d  fldRSD2        s                   like(ar4NOT)
008100130827     d  fldIND         s                   like(arbIND)
008200130828     d  fldCAD         s                   like(arbCAD)
008300130827     d  fldLOD         s                   like(arbLOD)
008400130827     d  fldPRD         s                   like(arbPRD)
008500130827     d  fldCTA         s                   like(§AR5REF)
008600130827     d  fldTEL         s                   like(§ar5teld)
008700140611     d  fldTELcontact  s             30A
008800140611     d  fldPREF        s              4A
008900130827     d  fldEML         s                   like(§ar5EML)
009000130827     d  fldSMS         s                   like(§ar5TEL)
009100170822     d  fldEML1        s                   like(§ar5EML)
009200140617     d  fldSMScontact  s             30A
009300150218     d  fldNOTE1       s                   like(ar4NOT)
009400150218     d  fldNOTE2       s                   like(ar4NOT)
009500150709     d  srvCODE        s              3
009600150709     d  nPARCEL        s             14
009700130827      **
009800130827     d  WTARA          s                   like(§qttpc)
009900130826     d  wvol           s                   like(arbvlb)
010000130826     d  wpes           s                   like(arbpkb)
010100130827     d  wpes100        s              9  0
010200130827      **
010300130826     D dwlprg          s             10    INZ(*all'0')
010400130826     D wrkprg          s              8  0 INZ(*zeros)
010500130827      **
010600130826     d trul47ds      e ds
010700130826     D W0140           S             14  0 inz
010800130826     D Segmento        s           2048
010900130826     D Conta_righe     s              5i 0
011000130826     d  esito          s              1
011100130826     d lunghezza_campo...
011200130826     d                 s              3i 0
011300130826     D*  Definisco costanti
011400130826     D TipoF           C                   CONST('$M')
011500130826     D CLIENTE         C                   CONST('0DPD0OUT')
011600001222     D digits          C                   CONST('0123456789')
011700130828     d Normal_Parcel   c                   101
011800130828     d Small_Parcel    c                   136
011900130828     d Return          c                   300
012000071213      *
012100020222     C*---------------------------------------------------------------------*
012200960530     C* Ciclo principale                                                   -*
012300960530     C*---------------------------------------------------------------------*
012400010426     C     *ENTRY        PLIST
012500010426     C                   PARM                    KPJBA
012600130826     C                   PARM                    flagHDE           1
012700130826      **
012800130826      *  il pgm viene chiamato 3 volte x gestire la scrittura della testata
012900130826      *                                          dei singoli dettagli
013000130826      *                                          e per chiudere il file con #END
013100130826     c                   seton                                        RT
013200130826     c                   setoff                                         LR
013300130826     c                   clear                   segmento
013400130826      **
013500130826      **  flag di testata / dettaglio / end
013600130826      * apre il TIVGD
013700130826      *  scrive la testata e la prima riga di dettaglio
013800130826     c                   if               flagHDE = 'H'
013900130826     C                   EXSR      vgd_OPEN
014000130827     c                   exsr      INIZIO_MSG
014100130826      **
014200130827      * scrive il dettaglio
014300130826     c                   elseif           flagHDE = 'D'
014400130827     c                   exsr      SPEDIZIONE
014500130826      **
014600130826      * quindi chiude il TIVGD quando il chiamante è andato in EoF
014700130826     c                   elseif           flagHDE = 'E'
014800130827     c                   exsr      FINE_MSG
014900130826     C                   EXSR      vgd_CLOSE
015000130826      **
015100130826     c                   COMMIT
015200130826      **
015300130826      * chiude definitivamente dopo aver Committato
015400130826     c                   seton                                        LR
015500130826     c                   setoff                                         RT
015600130826     c                   end
015700130826      **
015800130826     C                   Return
015900130826     C*--------------------------------------------------------------*
016000130826     C* scrive la TESTATA
016100130826     C*--------------------------------------------------------------*
016200130827     C     INIZIO_MSG    BEGSR
016300130826     C*
016400140423     c                   eval      segmento = '#FILE;GEODATA_SHPNOT_0820_DRK_D'
016500140423     c                             + %editc(WOGGI:'X') +'T'+ %editc(WORA:'X')
016600140307     c                             + ';'
016700130826     c                   exsr      VGD_WRITE
016800130826     C*
016900140307     c                   eval      segmento = '#ENCODING;ISO 8859-1;'
017000130826     c                   exsr      VGD_WRITE
017100130826     C*
017200140307     c                   eval      segmento = '#VERSION;01.00;'
017300130826     c                   exsr      VGD_WRITE
017400130826     C*
017500140307     c                   eval      segmento = '#DEF;GEODATA:HEADER;VERSION;'+
017600140307     c                             'CLASSIFICATION;;'
017700130826     c                   exsr      VGD_WRITE
017800130826     C*
017900140307     c                   eval      segmento = '#DEF;GEODATA:SHIPMENT;NUMORDER;'+
018000130828     c                             'MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MPSCRE'+
018100130828     c                             'F1;MPSCREF2;MPSCREF3;MPSCREF4;MPSBILLREF;M'+
018200130828     c                             'PSCOUNT;MPSVOLUME;MPSWEIGHT;SDEPOT;CDATE;C'+
018300130828     c                             'TIME;HARDWARE;RDEPOT;DSORT;SPTDATE;SPTTIME'+
018400130828     c                             ';SPTREALDATE;SPTREALTIME;DELMODALLOW;ROUTI'+
018500140307     c                             'NGPLANVERSION;SPARTNERREF1;SPARTNERREF2;;'
018600130826     c                   exsr      VGD_WRITE
018700130826     C*
018800140307     c                   eval      segmento = '#DEF;GEODATA:SENDER;NUMORDER;'+
018900130828     c                             'SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPNAME'+
019000130828     c                             ';SUNIQCUSTID;SNAME1;SNAME2;SSTREET;SPROPNU'+
019100130828     c                             'M;SADD2;SADD3;SFLOOR;SBUILDING;SDEPARTMENT'+
019200130828     c                             ';SCOUNTRYCODE;SSTATE;SZIPCODE;STOWN;SCONTA'+
019300140307     c                             'CT;SPHONE;SFAX;SEMAIL;SCOMMENT;SGLN;;'
019400130826     c                   exsr      VGD_WRITE
019500130827     C*
019600140307     c                   eval      segmento = '#DEF;GEODATA:RECEIVER;NUMORDER;'+
019700130828     c                             'RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPNAME2'+
019800130828     c                             ';RPUDOID;RSTREET;RPROPNUM;RADD2;RADD3;RFLO'+
019900130828     c                             'OR;RBUILDING;RDEPARTMENT;RCOUNTRYCODE;RSTA'+
020000130828     c                             'TE;RZIPCODE;RTOWN;RDOORCODE;RCONTACT;RCONT'+
020100130828     c                             'ACTPHO1;RCONTACTPHO2;RINTERPHONENAME;RFAX;'+
020200140307     c                             'REMAIL;RCOMMENT;;'
020300130827     c                   exsr      VGD_WRITE
020400130827      *
020500140307     c                   eval      segmento = '#DEF;GEODATA:PARCEL;NUMORDER;'+
020600130828     c                             'PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRANK;'+
020700130828     c                             'SENDPARCELREF1;SENDPARCELREF2;SENDPARCELRE'+
020800130828     c                             'F3;SENDPARCELREF4;RECPARCELREF;SERVICECODE'+
020900130828     c                             ';PPARTNERREF1;PPARTNERREF2;DIMENSION;DECLA'+
021000130828     c                             'REDWEIGHT;MEASUREDWEIGHT;HINSAMOUNT;HINSCU'+
021100130828     c                             'RRENCY;HINSCONTENT;HAZLQ;HZDPACKCODE;OPCOD'+
021200140307     c                             'E;;'
021300130827     c                   exsr      VGD_WRITE
021400130827      *
021500140307     c                   eval      segmento = '#DEF;GEODATA:MSG;NUMORDER;'+
021600130828     c                             'NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE'+
021700130828     c                             ';MSGDESTTYPE;MSGDESTINATION;MSGTRIGGER;MSG'+
021800140307     c                             'LANG;MSGSENDERURL;;'
021900130827     c                   exsr      VGD_WRITE
022000130827      *
022100140307     c                   eval      segmento = 'HEADER;01.00;SHPNOT;'
022200130827     c                   exsr      VGD_WRITE
022300130827     C*
022400130826     C                   endSR
022500130826     C*--------------------------------------------------------------*
022600130826     C* scrive la CODA
022700130826     C*--------------------------------------------------------------*
022800130827     C     FINE_MSG      BEGSR
022900130826     C*
023000130826     c                   eval      segmento = '#END;'
023100130826     c                   exsr      VGD_WRITE
023200130826     C*
023300130826     C                   endSR
023400960530     C*--------------------------------------------------------------*
023500020219     C* dettaglio spedizione
023600960530     C*--------------------------------------------------------------*
023700130827     C     SPEDIZIONE    BEGSR
023800020227     C*
023900130826     C                   MOVEL     KPJBU         TRTC831
024000130826     C*
024100130826     C*  Se c'è numero spedizione scrivo dettaglio
024200130826    2C     F83NSP        IFNE      0
024300130826     C*
024400130826     C*  Eseguo posizionamento su archivio bolle per passaggio param.
024500130827      * B O L L A
024600130826     C                   Z-ADD     F83AAS        KAAS
024700130826     C                   Z-ADD     F83LNP        KLNP
024800130826     C                   Z-ADD     F83NRS        KNRS
024900130826     C                   Z-ADD     F83NSP        KNSP
025000130826     C     KARB          CHAIN     FNARB01L                           31
025100130827    3C                   IF        %Found(FNARB01L)
025200130827     C* dati bolla
025300130827     C                   exsr      DECODIFICHE
025400130827     C*   su più righe
025500130827     c                   exsr      SHIPMENT                                      livello 3
025600130827     c                   exsr      SENDER                                        livello 4
025700130827     c                   exsr      RECEIVER                                      livello 4
025800130827     c                   exsr      PARCEL                                        livello 4
025900130827     C* il messaggio MSG è all'interno del PARCEL (strutturalmente)              livello 5
026000130826    2C                   END
026100130828     c                   end
026200130828     C*
026300130826     C*
026400130827    2C                   ENDSR
026500130826     C*--------------------------------------------------------------*
026600130827     C* dettaglio spedizione
026700130826     C*--------------------------------------------------------------*
026800130827     C     SHIPMENT      BEGSR
026900130827     C*
027000130827     C* parcel MASTER
027100140307     c                   eval      segmento = 'SHIPMENT;3;'
027200140307     c                             + %trim(§B4IPN)
027300140307     c                             +';'
027400140307     c                             + %trim(bolla)
027500140307     c                             + ';;0;'
027600140307     c                             + %trim(BOLLA)
027700140307     c                             +';'
027800140307     c                             + %Trim(ARBRMA)
027900140307     c                             +';'
028000140307     c                             + %trim(%EDITC(ARBRMN:'Z'))
028100140307     c                             + ';;;'
028200140307     c                             + %trim(%EDITC(ARBNCL:'Z'))
028300140307     c                             + ';;'
028400140307     c                             + %trim(%EDITC(wpes100:'Z'))
028500140307     c                             + ';0820;'
028600140307     c                             + Anno
028700140307     c                             + %EDITC(arbMGS:'X')
028800140307     c                             + ';090000;I;;;'
028900140307     c                             + %editc(WOGGI:'X')
029000140307     c                             + ';'
029100140307     c                             + %editc(WORA:'X')
029200140307     c                             + ';;;0;;;;'
029300130827     c                   exsr      VGD_WRITE
029400130827     C*
029500130827    2C                   ENDSR
029600130827     C*--------------------------------------------------------------*
029700130827     C* dettaglio Mittente
029800130827     C*--------------------------------------------------------------*
029900130827     C     SENDER        BEGSR
030000130827     C*
030100130827     C* SENDER
030200140307     c                   eval      segmento = 'SENDER;4;;;;;'
030300140307     c                             + %Trim(fldRSM)
030400140307     c                             + ';;'
030500140307     c                             + %Trim(fldINM)
030600140307     C                             + ';;;;;;;380;;'
030700140307     C                             + %Trim(%editc(fldCPM:'X'))
030800140307     c                             + ';'
030900140307     c                             + %Trim(fldLOM)
031000140307     c                             + ';;'
031100140307     c                             + %Trim(fldTLM)
031200140307     c                             + ';;;;;'
031300130828     c                   exsr      VGD_WRITE
031400130828     C*
031500130828    2C                   ENDSR
031600130827     C*--------------------------------------------------------------*
031700130827     C* dettaglio Destinatario
031800130827     C*--------------------------------------------------------------*
031900130827     C     RECEIVER      BEGSR
032000130827     C*
032100130827     C* RECEIVER
032200140307     c                   eval      segmento = 'RECEIVER;4;;'
032300140307     c                             + %Trim(fldRSD)
032400140307     c                             + ';;'
032500140307     c                             + %Trim(fldRSD)
032600140307     c                             + ';'
032700140307     C                             + %Trim(fldRSD2)
032800140307     C                             + ';;'
032900140307     C                             + %Trim(fldIND)
033000140307     C                             + ';;;;;;;'
033100140307     C                             + %Trim(fldNZD)
033200140307     C                             + ';;'
033300140307     C                             + %Trim(fldCAD)
033400140307     C                             + ';'
033500140307     C                             + %Trim(fldLOD)
033600140307     C                             + ';;'
033700140307     c                             + %Trim(fldCTA)
033800140307     c                             + ';'
033900140611     c                             + %Trim(fldTELcontact)
034000140617     c                             + ';;;;;'
034100150219     c                             + %TrimL(fldNOTE1)
034200150219     c                             + %TrimR(fldNOTE2)
034300140307     c                             + ';'
034400130827     c                   exsr      VGD_WRITE
034500130827     C*
034600130827    2C                   ENDSR
034700130827     C*--------------------------------------------------------------*
034800130827     C* dettaglio collo
034900130827     C*--------------------------------------------------------------*
035000130827     C     PARCEL        BEGSR
035100130828
035200130828      * reC 'I'  PARCEL  (se un domani il multicollo)
035300130828     c                   clear                   DSBL4i
035400130828     c                   move      'I'           ktrc
035500130828     c     Kar4          setll     fiar401l
035600130828     c     Kar4          reade     fiar401l
035700130828     c                   dow       not %EoF(fiar401l)
035800130828     c                   movel     AR4not        DSBL4i
035900130828      **
036000130828     c                   clear                   srvCODE
036100150709     c                   eval      srvCODE = §B4ISC
036200150709     c                   eval      nPARCEL = §B4IPN
036300130828     C* PARCEL
036400140307     c                   eval      segmento = 'PARCEL;4;'
036500150709     c                             + %Trim(nPARCEL)
036600140307     c                             + ';;;'
036700140307     c                             + %Trim(BOLLA)
036800140307     c                             + ';'
036900140307     c                             + %Trim(arbRMA)
037000140307     c                             + ';'
037100140307     c                             + %trim(%EDITC(ARBRMN:'Z'))
037200140307     C                             + ';;;'
037300150709     C                             + %Trim(srvCODE)
037400140307     C                             + ';;;;'
037500140307     c                             + %trim(%EDITC(WPes100:'Z'))
037600140307     c                             + ';'
037700140307     c                             + %trim(%EDITC(WPes100:'Z'))
037800140310     c                             + ';;;;;;;'
037900130828     c                   exsr      VGD_WRITE
038000130828     C*
038100140617     c                   if        serv_mail_Attivato = 'S'
038200130828     c                   exsr      MSG
038300140617     c                   end
038400130828      **
038500130828     c     Kar4          reade     fiar401l
038600130828     c                   end
038700130827     C*
038800130827    2C                   ENDSR
038900130827     C*--------------------------------------------------------------*
039000130828     C*  Se vi è un messaggio da comunicare è legato al PARCEL
039100130827     C*--------------------------------------------------------------*
039200130828     C     MSG           BEGSR
039300130827      *
039400130828     C* MSG   invio indirizzo Mail
039500140617      *     (è stato provato solo x la MAIL e non per l'SMS)
039600140617      *
039700140617      *   ATTENZIONE provato solo con la MAIL  / l'SMS va implementato
039800140617      *              se si deciderà di attivare il servizio con DPD (a pagamento)
039900140617      *
040000130828     c                   if        fldEML <> *blank
040100140307     c                   eval      segmento = 'MSG;5;'
040200150623     c                             + ';;1;0;'
040300140307     c                             + %trim(fldEML)
040400140307     c                             + ';904;;;'
040500130828     c                   exsr      VGD_WRITE
040600130828     c                   end
040700170822      *
040800170822     c                   if        fldEML1<> *blank
040900170822     c                   eval      segmento = 'MSG;5;'
041000170822     c                             + ';;1;0;'
041100170822     c                             + %trim(fldEML1)
041200170822     c                             + ';904;;;'
041300170822     c                   exsr      VGD_WRITE
041400170822     c                   end
041500130828     C*
041600150617      *
041700150617     c                   if        fldSMS <> *blank
041800150617     c                   eval      segmento = 'MSG;5;'
041900150624     c                             + ';;3;0;'
042000150617     c                             + %trim(fldSMS)
042100150617     c                             + ';904;;;'
042200150617     c                   exsr      VGD_WRITE
042300150617     c                   end
042400170822      *
042500150617     C*
042600130828    2C                   ENDSR
042700130828     C*--------------------------------------------------------------*
042800130828     C* dettaglio spedizione dagli archivi BRT
042900130828     C*--------------------------------------------------------------*
043000130828     C     DECODIFICHE   BEGSR
043100130828      *
043200130827      * con la LNA si iidentificano i dati del Mittente (filiale di Lancio Manifest)
043300130828     c                   clear                   fldRSM
043400130828     c                   clear                   fldINM
043500130828     c                   clear                   fldCPM
043600130828     c                   clear                   fldLOM
043700130828     c                   clear                   fldPRM
043800130828     c                   clear                   fldTLM
043900130828     c                   clear                   fldFXM
044000130827     c                   clear                   fldRSD
044100130827     c                   clear                   fldRSD2
044200130827     c                   clear                   FldIND
044300130827     c                   clear                   fldCAD
044400130827     c                   clear                   fldLOD
044500130827     c                   clear                   fldPRD
044600130827     c                   clear                   fldNZD
044700130827     c                   clear                   fldCTA
044800130827     c                   clear                   fldTEL
044900140611     c                   clear                   fldPREF
045000140611     c                   clear                   fldTELcontact
045100130827     c                   clear                   fldEML
045200170822     c                   clear                   fldEML1
045300130827     c                   clear                   fldSMS
045400140617     c                   clear                   fldSMScontact
045500150218     c                   clear                   fldNOTE1
045600150218     c                   clear                   fldNOTE2
045700140310     C*
045800140310     C* Rif.spedizione BRT
045900140310     c                   move      arbaas        anno              4
046000140310     c                   move      arblnp        linea             3
046100140310     c                   move      arbnrs        serie             2
046200140310     c                   move      arbnsp        numero            7
046300130827
046400130828     c                   move      bolla16       bolla            14
046500130828
046600130827      * reC 'D'  RAG.SOCIALE 2
046700130827     c                   move      'D'           ktrc
046800130827     c     Kar4          chain     fiar401l
046900130827     c                   if        %found(fiar401l)
047000130827     c     ';':'.'       xlate     ar4not        fldRSD2
047100130827     c                   end
047200130827
047300130827      * reC 'I'  PARCEL
047400130827     c                   clear                   DSBL4i
047500130827     c                   move      'I'           ktrc
047600130827     c     Kar4          chain     fiar401l
047700130827     c                   if        %found(fiar401l)
047800130827     c                   movel     AR4not        DSBL4i
047900130827     c                   end
048000130827      *
048100130827     C*  reperisce il volume/peso da CML o da Bollettazione
048200130827     c                   exsr      VOL_PES
048300130827      * mittente
048400130827     c     arbLNA        chain     azorg01l
048500130827     c                   if        %Found(azorg01l)
048600130828     c     ';':'.'       xlate     orgDES        fldRSM
048700130828     c     ';':'.'       xlate     orgIND        fldINM
048800130828     c                   z-add     orgCPF        fldCPM
048900130828     c     ';':'.'       xlate     orgLOC        fldLOM
049000130828     c     ';':'.'       xlate     orgPRO        fldPRM
049100130828     c     ';':'.'       xlate     orgTEL        fldTLM
049200130828     c     ';':'.'       xlate     orgFAX        fldFXM
049300130827     c                   end
049400130827      *
049500130827     C* Destinatario
049600130827     c     ';':'.'       xlate     arbRSD        fldRSD
049700130827     c     ';':'.'       xlate     arbIND        fldIND
049800130827     c     ';':'.'       xlate     arbCAD        fldCAD
049900130827     c     ';':'.'       xlate     arbLOD        fldLOD
050000130827     c     ';':'.'       xlate     arbPRD        fldPRD
050100140424      *
050200140424      **** DPD non vuole avere i CAP con degli spazi in mezzo
050300140424      ** quindi compatto il CAP ove vi fossero degli spazi
050400140424     c                   clear                   compat9CAP        9
050500140424     c                   clear                   nr                3 0
050600140424      * compatta i 9 caratteri togliendo i blank fra una sigla e l'altra
050700140424     c                   DO        9             cap               3 0
050800140424     c                   if        %subst(fldCAD:cap:1) <> ' '
050900140424     c                   eval      nr = nr + 1
051000140424     c                   eval      %subst(compat9CAP:nr:1)
051100140424     c                             =  %subst(fldCAD:cap:1)
051200140424     c                   end
051300140429     c                   endDO
051400140424     c                   eval      fldCAD = compat9CAP
051500140429      *
051600140429      * Queste specifiche sull'Irlanda erano sul FIEU16R:
051700140429      *  (CHIODO xchè Irlanda non ha il CAP)
051800140429      *   Controlla il CAP se scritto "EIRE" (Irlanda) deve essere sostituito
051900140429      *   con "0000001". (Richiesta di Villa da Meeting del 14/10/2008 con DPD.
052000140429     C                   if        fldCAD = 'EIRE'
052100140506      * e se non è DUBLIN deve essere "0000002"
052200140623      *    dopo una mail di rettifica del 23/6/2014 si deve inviare solo 1 o 2
052300140623      *    formalmente nel cap di DUBLINO e IRLANDA
052400140623      *  1 --> RDEPOT= 1601    2 --> RDEPOT= 1602
052500140623      *
052600140623     c                   eval      fldCAD = '2'
052700140506      * ma solo    DUBLIN deve essere "0000001"
052800140506     c     'DUBLIN'      SCAN      fldLOD
052900140506     c                   if        %Found
053000140623     c                   eval      fldCAD = '1'
053100140506     c                   end
053200140429     c                   end
053300130827      *
053400130827     C* Nazione Destinatario
053500130827     C                   Z-ADD     1             YY                3 0
053600130827     C     arbNZD        LOOKUP    C15(YY)                                32
053700130827     C   32ISO(YY)       COMP      *BLANKS                            3232
053800140611     c                   if        *in32
053900140611     C                   MOVEL     CIE(YY)       fldNZD            3
054000140611      * se c'è il pref.internazionale in tabella Nazioni
054100140611      * imposta il codice preceduto dal '+'
054200140611     c                   if        PRF(YY) <> *blank
054300140611     C                   MOVEL     PRF(YY)       fldPREF
054400140611     c                   end
054500140611     c                   end
054600130827      *
054700031210     c                   movel     'GEN'         ktrd
054800031210     c                   clear                   DAr5Gen
054900031210     c     kar5          chain     fiar501L                           34
055000031210     c                   if        %Found(fiar501L)
055100031210     c                   Movel     Ar5Uni        DAr5Gen
055200130827     c     ';':'.'       xlate     §ar5ref       fldCTA
055300130827     c     ';':'.'       xlate     §ar5teld      fldTEL
055400140612     c                   movel     fldTEL        input16          16
055500140612     c                   exsr      compatta_NR
055600140612     c                   movel     out16         fldTEL
055700140611      * se c'è già il prefisso
055800140611     c                   if        %subst(fldTEL:1:1) = '+' or
055900140611     c                             %subst(fldTEL:1:2) = '00'
056000140611     c                   eval      fldTELcontact = fldTEL
056100140611     c                   else
056200140611      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
056300140612     c                   if        fldPREF <> *blank and fldTEL <> *blank
056400140611     c                   eval      fldTELcontact = '+'
056500140612     c                               + %Trim(fldPREF) + %Trim(fldTEL)
056600140611     c                   end
056700140611     c                   end
056800031211     c                   end
056900130827      *
057000140617      * solo se il servizio verrà attivato (al momento salta decodifica)
057100140617      *  nelle definizioni il flag è inizializzato a (N)
057200140617      *
057300140617      *  EMAIL o SMS - come servizio di Alert di consegna -
057400140617      *     ATTENZIONE il programma al momento ha gestito solo la MAIL e
057500140617      *        NON l'SMS --> che dovrà essere implementato se si deciderà
057600140617      *                      l'attivazione del servizio.
057700140617     c                   if        serv_mail_Attivato = 'S'
057800140617      *-
057900130827     c                   movel     'EMD'         ktrd
058000130827     c                   clear                   DAr5emd
058100130827     c     kar5          chain     fiar501L                           34
058200130827     c                   if        %Found(fiar501L)
058300130827     c                   Movel     Ar5Uni        DAr5emd
058400170822     c                   z-add     0             position          3 0
058500170822     c     ';'           scan      §ar5EML:1     position
058600170822     c                   if        position > 0
058700170822     c                   eval      fldEML1= %subst(§ar5EML:position+1)
058800170822     c                   eval      fldEML = %subst(§ar5EML:1:position-1)
058900170822     c                   else
059000170822     c                   eval      fldEML= §ar5EML
059100170822     c                   end
059200170822      **
059300140617     c     ';':' '       xlate     §ar5TEL       fldSMS
059400140617     c                   movel     fldSMS        input16          16
059500140617     c                   exsr      compatta_NR
059600140617     c                   movel     out16         fldSMS
059700140617      * se c'è già il prefisso
059800140617     c                   if        %subst(fldSMS:1:1) = '+' or
059900140617     c                             %subst(fldSMS:1:2) = '00'
060000140617     c                   eval      fldSMScontact = fldSMS
060100140617     c                   else
060200140617      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
060300140617     c                   if        fldPREF <> *blank and fldSMS <> *blank
060400140617     c                   eval      fldSMScontact = '+'
060500140617     c                               + %Trim(fldPREF) + %Trim(fldSMS)
060600140617     c                   end
060700140617     c                   end
060800130827     c                   end
060900140617      *-
061000140617     c                   endIF
061100020219     C*----------------
061200020220     C*
061300020219     C*----------------
061400020219     C*  Controllo se ci sono delle note di consegna
061500020219     C                   MOVEL     '8'           KTRC
061600060213     C     KAR4          CHAIN     fiar401L                           32
061700020220     C*  Shipment Ref.notes (1°campo note)             solo se c'è
061800150218     c  n32';':'.'       xlate     ar4not        fldnote1
061900030226      *
062000150218     C                   MOVEL     '9'           KTRC
062100150218     C     KAR4          CHAIN     fiar401L                           32
062200150218     C*  Shipment Ref.notes (2°campo note)             solo se c'è
062300150218     c  n32';':'.'       xlate     ar4not        fldnote2
062400030226      **
062500020220     C*----------------
062600020226      *
062700020219     c                   endsr
062800020227     C*****************************************************************
062900140612     C*  Compatta il campo numero di telefono avvicinando i numeri
063000030317     C*****************************************************************
063100140612     C     compatta_NR   BEGSR
063200030317      *
063300140612     c                   z-add     0             in                3 0
063400140612     c                   clear                   Byte_uno          1
063500140612     c                   clear                   out16            16
063600140612      *
063700140612      *  controlla presenza di caratteri divisori o blank
063800140612      *   li elimina e deve compattare il numero
063900140612     c                   do        16            Nx                3 0
064000140612     c                   eval      Byte_uno = %subst(input16:Nx:1)
064100140612      * controlla che NON sia un numero :
064200140612     c                   move      'N'           alfa              1
064300140612     c     digits        check     Byte_uno
064400140612     c                   if        %found
064500140612     c                   move      'S'           alfa
064600140612     c                   end
064700140612      *
064800140612      * tranne sul primo che può esserci il '+'
064900140612     c                   if        Nx = 1 and Byte_uno ='+'
065000140612     c                             or alfa = 'N'
065100140612     c                   add       1             in
065200140612     c                   eval      %subst(out16:in:1) = %subst(input16:Nx:1)
065300140612     c                   end
065400140612     c                   enddo
065500140612      *
065600140612     c                   endsr
065700140612     C*****************************************************************
065800140612     C*  Reperisce Peso/Volume o da CML o da Bollettato
065900140612     C*****************************************************************
066000140612     C     VOL_PES       BEGSR
066100140612      *
066200030317      * Prima cerca il volume fra bollettato e CML
066300050112     C     arbncR        ifeq      arbncl
066400030317      * se è considerata un'unica spedizione
066500030317     C                   z-add     ARBVLC        WVOL
066600030317     C                   else
066700030317     C*
066800030317     C     ARBVLC        ifge      ARBVLb
066900030317     C                   z-add     ARBVLC        WVOL
067000030317     C                   else
067100030317     C                   z-add     ARBVLb        WVOL
067200030317     C                   end
067300030317     C*
067400030317     C                   endif
067500030317      *
067600030521     C* PESO VDL - TARA
067700030521     C     §QTTPC        MULT      arbncp        WTARA
067800030521     C                   Z-ADD     arbPKC        wpes
067900150825     c                   sub(h)    wtara         wpes
068000030521      *
068100030317      * Poi cerca il peso fra bollettato e CML
068200050112     C     arbncP        ifeq      arbncl
068300030317      * se è considerata un'unica spedizione
068400030317     C                   ELSE
068500030521     C*  altrimenti se il bollettato è superiore passo il Bollettato.
068600030521     C     wpes          iflt      arbPKb
068700030317     C                   Z-ADD     arbPKb        wpes
068800030317     C                   end
068900030317     C*
069000030317     C                   ENDIF
069100150624     C*
069200150624     C* Se il peso è superiore al limite del fuori misura impostiamo fisso 31 kg.
069300150624     c                   if        wpes > 31,5
069400150624     c                   eval      wpes = 31
069500150624     C                   end
069600150624     C*
069700130827     c                   eval      Wpes100 = ( wpes * 100)
069800030317     C*
069900030317     C                   ENDSR
070000130826      **?__________________________________________________________________ */
070100130826      *    Apertura del TIVGD
070200130826      **?__________________________________________________________________ */
070300130826     C     VGD_OPEN      Begsr
070400130826      *
070500130826      *  recupera il progressivo
070600130826     c                   exsr      Piglia_progr
070700130826      *
070800130826      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
070900130826     C                   clear                   trul47ds
071000130826     C                   eval      d47opz  = 'I'
071100130826     C                   eval      d47tip  = TipoF
071200130826     C                   eval      d47lck  = 'N'
071300130826     C                   eval      d47chkj = 'N'
071400130826     C                   eval      d47pgm  = 'TRTC83R13'
071500130826     C                   call      'TRUL47R'
071600130826     C                   parm                    trul47ds
071700130826      *
071800130826     c                   endsr
071900130826      **?__________________________________________________________________ */
072000130826      *   Scrittura del TIVGD
072100130826      **?__________________________________________________________________ */
072200130826     C     VGD_WRITE     Begsr
072300130826     c                   eval       Conta_righe = 1 + Conta_righe
072400130826      *
072500130826     c                   clear                   tivgd000
072600130826     c                   eval      vgddta = %TrimR(Segmento)
072700130826     c                   eval      vgdtip = TipoF
072800130826     c                   eval      vgdksu = CLIENTE
072900130826     C                   eval      vgdprg = Progressivo
073000130826     c                   eval      vgdtsc = 'WW'
073100130826     c                   eval      vgdpgm = 'TRTC83R13'
073200130828     c                   eval      vgddat = woggi
073300130826      *
073400130826     C                   WRITE     tivgd000
073500130826      *
073600130826     c                   endsr
073700130826      **?__________________________________________________________________ */
073800130826      *    Chiusura del TIVGD
073900130826      **?__________________________________________________________________ */
074000130826     C     VGD_CLOSE     Begsr
074100130826      *
074200130826     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
074300130826     C                   clear                   trul47ds
074400130826     C                   eval      d47opz  = 'F'
074500130826     C                   eval      d47tip  = TipoF
074600130826     C                   call      'TRUL47R'
074700130826     C                   parm                    trul47ds
074800130826      *
074900130826     c                   endsr
075000130826      **?__________________________________________________________________ */
075100130826      *   Prende il Progressivo
075200130826      **?__________________________________________________________________ */
075300130826     C     Piglia_progr  Begsr
075400130826      *
075500130826     C                   MOVEL     CLIENTE       virKSC
075600130826     C                   MOVEL     TipoF         virTIP
075700130826     c     kvir02        chain     tivir02l
075800130828     c                   move      'OF'          Var_ISV
075900130826     c                   if        %Found(tivir02l)
076000130826     c                   move      VirFI1        Var_ISV
076100130826     C                   End
076200130826
076300130826      *       prende numeratore strategi
076400130826     C                   exsr      calprog
076500130826      *
076600130826     c                   endsr
076700130826      **?__________________________________________________________________ */
076800130826      *
076900130826      **?__________________________________________________________________ */
077000130826     C     calprog       begsr
077100130826     C*
077200130828     c                   clear                   progressivo      10
077300130826     C                   open      tis7prgf
077400130826     C                   read(e)   tis7prgf
077500130826     C*
077600130826     C                   if        not %error
077700130826     C                   eval      dwlprg = f_tis7prgf
077800130826     C*
077900130826     C                   move(p)   dwlprg        wrkprg
078000130826     C                   add       1             wrkprg
078100130826     C                   move(p)   wrkprg        dwlprg
078200130826     C                   movel     Var_ISV       dwlprg
078300130826     C*
078400130826     C                   eval       f_tis7prgf = dwlprg
078500130826     C                   eval      Progressivo = dwlprg
078600130826     C*
078700130826     C                   update    tis7prg0
078800130826     C                   endif
078900130826     C*
079000130826     C                   close     tis7prgf
079100130826     C*
079200130826     C                   endsr
079300130826      **?__________________________________________________________________ */
079400130826      **?__________________________________________________________________ */
079500130826     C     *INZSR        BEGSR
079600130826     C*
079700130826     C* Definisco chiavi di accesso
079800130828     c*
079900130828     c     kvir02        klist
080000130828     C                   kfld                    virKSC
080100130828     C                   kfld                    virTIP
080200130828     c*
080300130826     C     KARB          KLIST
080400130826     C                   KFLD                    KAAS
080500130826     C                   KFLD                    KLNP
080600130826     C                   KFLD                    KNRS
080700130826     C                   KFLD                    KNSP
080800130827      *
080900130827     C     KAR4          KLIST
081000130827     C                   KFLD                    KAAS
081100130827     C                   KFLD                    KLNP
081200130827     C                   KFLD                    KNRS
081300130827     C                   KFLD                    KNSP
081400130827     C                   KFLD                    KTRC              1
081500130826      *
081600130826     C     KAR5          KLIST
081700130826     C                   KFLD                    KAAS
081800130826     C                   KFLD                    KLNP
081900130826     C                   KFLD                    KNRS
082000130826     C                   KFLD                    KNSP
082100130826     C                   KFLD                    KTRD              3
082200130826      *
082300130826     C     KTAB          KLIST
082400130826     C                   KFLD                    KKUT
082500130826     C                   KFLD                    KCOD
082600130826      *
082700130826     C     KTAB1         KLIST
082800130826     C                   KFLD                    KKUT
082900130826     C                   KFLD                    KCOD
083000130826     C                   KFLD                    KKEY
083100130826      *
083200130826      *------------
083300130826     C* Inizializzo variabili totale
083400130826     C* Imposto data del giorno
083500130826     C                   TIME                    W0140            14 0
083600130826     C                   MOVE      W0140         G02DAT            8 0
083700130826     C                   MOVEL     W0140         Wora              6 0
083800130826     C                   MOVEL     *BLANK        G02ERR
083900130826     C                   CALL      'XSRDA8'
084000130826     C                   PARM                    WLBDA8
084100130826     C                   MOVEL     G02INV        WOGGI             8 0
084200130826      *
084300130826     C** CARICAMENTO DATI VARI TASSAZIONE
084400130826     C                   Z-ADD     1             KKUT
084500130826     C                   MOVEL     'QT'          kcod
084600130826     C                   MOVEL     *BLANKS       kKEY
084700130826     C                   MOVEL     '1'           kKEY
084800130826     C     KTAB1         CHAIN     TABEL00F                           30
084900130826     C  N30              MOVEL     TBLUNI        DSQT1
085000130826     C*
085100130826      *------------
085200130826     C* Caricamento Tabella codici nazioni
085300130826     C                   Z-ADD     0             X                 3 0
085400130826     C                   MOVEL     '15'          KCOD
085500130826     C                   Z-ADD     1             KKUT
085600130826     C     KTAB          CHAIN     TABEL00F                           30
085700130826     C     *IN30         DOWEQ     '0'
085800130826     C     TBLFLG        IFEQ      *BLANKS
085900130826     C                   ADD       1             X
086000130826     C                   MOVEL     TBLKEY        C15(X)
086100130826     C                   MOVEL     TBLUNI        DS15
086200130826     C                   MOVEL     §15COD        ISO(X)
086300130826     C                   MOVEL     §15des        DNZ(X)
086400130826     C                   MOVEL     §15ACN        ACN(X)
086500130828     C                   MOVEL     §15CIE        CIE(X)
086600140611     C                   MOVEL     §15PREF       prf(X)
086700130826     C                   END
086800130826     C     KTAB          READE     TABEL00F                               30
086900130826     C                   END
087000130826      *------------
087100130826     C*
087200130826     C                   ENDSR
087300130826      **?__________________________________________________________________ */
