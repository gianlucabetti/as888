000100180314      //**********************************************************************
000200180314      //   ESPORTA DATI BOLLE x DPD CH fuori da Clearing      Export         *
000300180314      //     ?   - GEODATA FILE vers.3.1    ?
000400180314      //**********************************************************************
000500891004     H DECEDIT('0,') DATEDIT(*DMY/)
000600180314      //---------------------------------------------------------------------*
000700140311     FFNARB01L  iF   E           K DISK    Commit
000800171018     FfiPND01L  IF   E           K DISK
000900060213     Ffiar401L  IF   E           K DISK
001000031210     Ffiar501L  IF   E           K DISK
001100960530     FTABEL00F  IF   E           K DISK
001200170904     FTntbe01l  IF   E           K DISK
001300020219     FAzOrg01L  IF   E           K DISK
001400180314      //**
001500130826     ftivgd00f  if a E             DISK    commit
001600180314      //
001700130826     Ftivir02L  IF   E           K DISK
001800130826     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001900130826     F                                     PREFIX(f_) usropn
002000180314      //---------------------------------------------------------------------*
002100180314      //*  Flag di attivazione
002200180314      //---------------------------------------------------------------------*
002300180314      // il servizio mail di alert è predisposto
002400180314      //    ma NON attivato poichè adesso impostato a (N) --> non decodifica i dati
002500180314      //    né esegue la scrittura del record "MSG" xchè con DPD non è attivo il servizio.
002600180314      //
002700140617     d  serv_mail_Attivato...
002800150617     d                 s              1    INZ('S')
002900180314      //*
003000180314      //---------------------------------------------------------------------*
003100180314      // Schiere
003200180314      //---------------------------------------------------------------------*
003300020220     D Num             S              1    DIM(11)
003400180314      // Codici ISO nazioni x E.D.I.
003500011217     D C15             S              3    DIM(500)
003600011217     D ISO             S              3    DIM(500)
003700130828     D CIE             S              3  0 DIM(500)
003800020219     D DNZ             S             25    DIM(500)
003900020412     D ACN             S              1    DIM(500)
004000140611     D PRF             S              4    DIM(500)
004100180314      //---------------------------------------------------------------------*
004200180314      // DS
004300180314      //---------------------------------------------------------------------*
004400180314      //*** DSBL4i        E DS
004500031210     D dar5gen       e ds
004600130827     D dar5emd       e ds
004700020227     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
004800960530     D DS3A          E DS
004900030521     D DSQT1         E DS                  INZ
005000960822     D DS15          E DS
005100130828     D DDPP          E DS
005200950428     D KPJBA         E DS
005300180314      //------
005400180314      // per decodificare il DEPOT i Partenza mediante la LNP
005500171010     D OG143         E DS
005600171010     D ds3IDP        E DS
005700180314      //------
005800180314      //
005900130828     D BOLLA16         DS                  INZ
006000130827     d  anno                          4
006100130827     d  linea                         3
006200130827     d  serie                         2
006300130827     d  numero                        7
006400180314      //
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
010900180314      //*
011000130827     d  WTARA          s                   like(§qttpc)
011100130826     d  wvol           s                   like(arbvlb)
011200130826     d  wpes           s                   like(arbpkb)
011300130827     d  wpes100        s              9  0
011400180314      //*
011500130826     D dwlprg          s             10    INZ(*all'0')
011600130826     D wrkprg          s              8  0 INZ(*zeros)
011700180314      //*
011800130826     d trul47ds      e ds
011900130826     D W0140           S             14  0 inz
012000130826     D Segmento        s           2048
012100130826     D Conta_righe     s              5i 0
012200130826     d  esito          s              1
012300130826     d lunghezza_campo...
012400130826     d                 s              3i 0
012500180314      //  Definisco costanti
012600180314     D TipoF           C                   CONST('CH')
012700180314     D CLIENTE         C                   CONST('3800009')
012800001222     D digits          C                   CONST('0123456789')
012900130828     d Normal_Parcel   c                   101
013000130828     d Small_Parcel    c                   136
013100130828     d Return          c                   300
013200180314      //
013300180314      //---------------------------------------------------------------------*
013400180314      // Ciclo principale                                                   -*
013500180314      //---------------------------------------------------------------------*
013600010426     C     *ENTRY        PLIST
013700010426     C                   PARM                    KPJBA
013800130826     C                   PARM                    flagHDE           1
013900180314      //*
014000180314      //  il pgm viene chiamato 3 volte x gestire la scrittura della testata
014100180314      //                                          dei singoli dettagli
014200180314      //                                          e per chiudere il file con #END
014201180314      /FREE
014300180314       *INRT = *ON;
014400180314       *INLR = *OFF;
014500180314       clear segmento;
014600180314       //*
014700180314       //*  flag di testata / dettaglio / end
014800180314       // apre il TIVGD
014900180314       //  scrive la testata e la prima riga di dettaglio
015000180314       if flagHDE = 'H';
015100180314         EXSR vgd_OPEN;
015200180314         exsr INIZIO_MSG;
015300180314
015400180314         // scrive il dettaglio
015500180314       elseif flagHDE = 'D';
015600180314         exsr SPEDIZIONE;
015700180314
015800180314         // quindi chiude il TIVGD quando il chiamante è andato in EoF
015900180314       elseif flagHDE = 'E';
016000180314         exsr FINE_MSG;
016100180314         EXSR vgd_CLOSE;
016200180314
016300180314         COMMIT;
016400180314
016500180314         // chiude definitivamente dopo aver Committato
016600180314         *INLR = *ON;
016700180314         *INRT = *OFF;
016800180314       ENDIF;
016900180314       //*
017000180314       Return;
017100180314       //--------------------------------------------------------------*
017200180314       // scrive la TESTATA
017300180314       //--------------------------------------------------------------*
017400180314       BEGSR INIZIO_MSG;
017500180314         //
017600180314         segmento = '#FILE;GEODATA_SHPNOTCUS_BRT_DPDCH_D'
017700180314             + %editc(WOGGI:'X') +'T'+ %editc(WORA:'X')
017800180314             + ';';
017900180314         exsr VGD_WRITE;
018000180314         //
018100180314         segmento = '#ENCODING;ISO 8859-1;';
018200180314         exsr VGD_WRITE;
018300180314         //
018400180314         segmento = '#VERSION;03.10;';
018500180314         exsr VGD_WRITE;
018600180314         //
018700180314         segmento = '#DEF;GEODATA:HEADER;VERSION;'+
018800180314             'CLASSIFICATION;;';
018900180314         exsr VGD_WRITE;
019000180314         //
019100180314         segmento = '#DEF;GEODATA:SHIPMENT;NUMORDER;'+
019200180314             'MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MPSCRE'+
019300180314             'F1;MPSCREF2;MPSCREF3;MPSCREF4;MPSBILLREF;M'+
019400180314             'PSCOUNT;MPSVOLUME;MPSWEIGHT;SDEPOT;CDATE;C'+
019500180314             'TIME;HARDWARE;RDEPOT;DSORT;SPTDATE;SPTTIME'+
019600180314             ';SPTREALDATE;SPTREALTIME;DELMODALLOW;ROUTI'+
019700180314             'NGPLANVERSION;SPARTNERREF1;SPARTNERREF2;SP'+
019800180314             'ARTNERCODE;OSORT;SSORT;MSORT;COLREQUESTFLA'+
019900180314             'G;ROUTINGPLACE;;';
020000180314         exsr VGD_WRITE;
020100180314         //
020200180314         segmento = '#DEF;GEODATA:SENDER;NUMORDER;'+
020300180314             'SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPNAME'+
020400180314             ';SUNIQCUSTID;SNAME1;SNAME2;SSTREET;SPROPNU'+
020500180314             'M;SADD2;SADD3;SFLOOR;SBUILDING;SDEPARTMENT'+
020600180314             ';SCOUNTRYCODE;SSTATE;SZIPCODE;STOWN;SCONTA'+
020700180314             'CT;SPHONE;SFAX;SEMAIL;SCOMMENT;SGLN;SGPSLA'+
020800180314             'T;SGPSLONG;SACCOUNTOWNER;;';
020900180314         exsr VGD_WRITE;
021000180314         //
021100180314         segmento = '#DEF;GEODATA:RECEIVER;NUMORDER;'+
021200180314             'RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPNAME2'+
021300180314             ';RPUDOID;RSTREET;RPROPNUM;RADD2;RADD3;RFLO'+
021400180314             'OR;RBUILDING;RDEPARTMENT;RCOUNTRYCODE;RSTA'+
021500180314             'TE;RZIPCODE;RTOWN;RDOORCODE;RCONTACT;RCONT'+
021600180314             'ACTPHO1;RCONTACTPHO2;RINTERPHONENAME;RFAX;'+
021700180314             'REMAIL;RCOMMENT;RGPSLAT;RGPSLONG;;';
021800180314         exsr VGD_WRITE;
021900180314         //
022000180314         segmento = '#DEF;GEODATA:PARCEL;NUMORDER;'+
022100180314             'PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRANK;'+
022200180314             'SENDPARCELREF1;SENDPARCELREF2;SENDPARCELRE'+
022300180314             'F3;SENDPARCELREF4;RECPARCELREF;SERVICECODE'+
022400180314             ';PPARTNERREF1;PPARTNERREF2;PPARTNERREF3;PP'+
022500180314             'ARTNERREF4;DIMENSION;DECLAREDWEIGHT;MEASUR'+
022600180314             'EDWEIGHT;HINSAMOUNT;HINSCURRENCY;HINSCONTE'+
022700180314             'NT;HAZLQ;HZDPACKCODE;OPCODE;PCONTENT;ORIGI'+
022800180314             'NPARCELNUMBER;POWNERBU;PPARTNERCODE;ASCODE'+
022900180314             ';BAGNO;;';
023000180314         exsr VGD_WRITE;
023100180314         //
023200180314         segmento = '#DEF;GEODATA:MSG;NUMORDER;'+
023300180314             'NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE'+
023400180314             ';MSGDESTTYPE;MSGDESTINATION;MSGTRIGGER;MSG'+
023500180314             'LANG;MSGSENDERURL;;';
023600180314         exsr VGD_WRITE;
023700180314         //
023800180314         segmento = 'HEADER;03.10;SHPNOT;';
023900180314         exsr VGD_WRITE;
024000180314         //
024100180314       endSR;
024200180314       //--------------------------------------------------------------*
024300180314       // scrive la CODA
024400180314       //--------------------------------------------------------------*
024500180314       BEGSR FINE_MSG;
024600180314         //
024700180314         segmento = '#END;';
024800180314         exsr VGD_WRITE;
024900180314         //
025000180314       endSR;
025100180314       //--------------------------------------------------------------*
025200180314       // dettaglio spedizione
025300180314       //--------------------------------------------------------------*
025400180314       BEGSR SPEDIZIONE;
025500180314         //
025501180314      /END-FREE
025600130826     C                   MOVEL     KPJBU         TRTC831
025700180314      //
025800180314      //  Se c'è numero spedizione scrivo dettaglio
025801180314      /FREE
025900180314         IF F83NSP <> 0;
026000180314           //
026100180314           //  Eseguo posizionamento su archivio bolle per passaggio param.
026200180314           // B O L L A
026201180314      /END-FREE
026300130826     C                   Z-ADD     F83AAS        KAAS
026400130826     C                   Z-ADD     F83LNP        KLNP
026500130826     C                   Z-ADD     F83NRS        KNRS
026600130826     C                   Z-ADD     F83NSP        KNSP
026601180314      /FREE
026602180314             // dati bolla
026700180314           CHAIN KARB FNARB01L;
026800180314           IF %Found(FNARB01L);
027000180314             exsr DECODIFICHE;
027100180314             //   su più righe
027200180314             exsr SHIPMENT;                                                   // livello 2
027300180314             exsr SENDER;                                                     // livello 3
027400180314             exsr RECEIVER;                                                   // livello 3
027500180314             exsr PARCEL;                                                     // livello 3
027600180314             // il messaggio MSG è all'interno del PARCEL (strutturalmente)              livello 4
027700180314           ENDIF;
027800180314         ENDIF;
027900180314         //
028000180314         //
028100180314       ENDSR;
028200180314       //--------------------------------------------------------------*
028300180314       // dettaglio spedizione
028400180314       //--------------------------------------------------------------*
028500180314       BEGSR SHIPMENT;
028600180314         //
028700180314         // parcel MASTER
028800180314         segmento = 'SHIPMENT;2;'
028900180314             + %trim(fldPARCEL)
029000180314             +';'
029100180314             + %trim(bolla)
029200180314             + ';'
029300180314             + %trim(fldCHKdgt)
029400180314             + ';0;'
029500180314             + %trim(BOLLA)
029600180314             +';'
029700180314             + %Trim(ARBRMA)
029800180314             +';'
029900180314             + %trim(%EDITC(ARBRMN:'Z'))
030000180314             + ';;;'
030100180314             + %trim(%EDITC(ARBNCL:'Z'))
030200180314             + ';;'
030300180314             + %trim(%EDITC(wpes100:'Z'))
030400180314             + ';'
030500180314             + %Trim(SenderDepot)
030600180314             + ';'
030700180314             + Anno
030800180314             + %EDITC(arbMGS:'X')
030900180314             + ';090000;I;;;'
031000180314             + %editc(WOGGI:'X')
031100180314             + ';'
031200180314             + %editc(WORA:'X')
031300180314             + ';;;0;;;;;;;'
031400180314             + %Trim(fld_daORM)
031500180314             + ';;;';
031600180314         exsr VGD_WRITE;
031700180314         //
031800180314    2  ENDSR;
031900180314       //--------------------------------------------------------------*
032000180314       // dettaglio Mittente
032100180314       //--------------------------------------------------------------*
032200180314       BEGSR SENDER;
032300180314         //
032400180314         // SENDER
032500180314         segmento = 'SENDER;3;;;;;'
032600180314             + %Trim(fldRSM)
032700180314             + ';;'
032800180314             + %Trim(fldINM)
032900180314             + ';;;;;;;380;;'
033000180314             + %Trim(%editc(fldCPM:'X'))
033100180314             + ';'
033200180314             + %Trim(fldLOM)
033300180314             + ';;'
033400180314             + %Trim(fldTLM)
033500180314             + ';;;;;;;;';
033600180314         exsr VGD_WRITE;
033700180314         //
033800180314    2  ENDSR;
033900180314       //--------------------------------------------------------------*
034000180314       // dettaglio Destinatario
034100180314       //--------------------------------------------------------------*
034200180314       BEGSR RECEIVER;
034300180314         //
034400180314         // RECEIVER
034500180314         segmento = 'RECEIVER;3;;'
034600180314             + %Trim(fldRSD)
034700180314             + ';;'
034800180314             + %Trim(fldRSD)
034900180314             + ';'
035000180314             + %Trim(fldRSD2)
035100180314             + ';;'
035200180314             + %Trim(fldIND)
035300180314             + ';;;;;;;'
035400180314             + %Trim(fldNZD)
035500180314             + ';;'
035600180314             + %Trim(fldCAD)
035700180314             + ';'
035800180314             + %Trim(fldLOD)
035900180314             + ';;'
036000180314             + %Trim(fldCTA)
036100180314             + ';'
036200180314             + %Trim(fldTELcontact)
036300180314             + ';;;;;'
036400180314             + %TrimL(fldNOTE1)
036500180314             + %TrimR(fldNOTE2)
036600180314             + ';;;';
036700180314         exsr VGD_WRITE;
036800180314         //
036900180314    2  ENDSR;
037000180314       //--------------------------------------------------------------*
037100180314       // dettaglio collo
037200180314       //--------------------------------------------------------------*
037300180314       BEGSR PARCEL;
037400130828
037500180314         // reC 'I'  PARCEL  (se un domani il multicollo)
037600180314         //*****             clear                   DSBL4i
037700180314         //***               move      'I'           ktrc
037800180314         //*** Kar4          setll     fiar401l
037900180314         //*** Kar4          reade     fiar401l
038000180314         //*****             dow       not %EoF(fiar401l)
038100180314         //*****             movel     AR4not        DSBL4i
038200180314         //*****
038300180314         // PARCEL   NO MULTICOLLO
038400180314         segmento = 'PARCEL;3;'
038500180314             + %Trim(fldPARCEL)
038600180314             + ';'
038700180314             + %Trim(fldCHKdgt)
038800180314             + ';;'
038900180314             + %Trim(BOLLA)
039000180314             + ';'
039100180314             + %Trim(arbRMA)
039200180314             + ';'
039300180314             + %trim(%EDITC(ARBRMN:'Z'))
039400180314             + ';;;'
039500180314             + %Trim(fldISC)
039600180314             + ';;;;;;'
039700180314             + %trim(%EDITC(WPes100:'Z'))
039800180314             + ';'
039900180314             + %trim(%EDITC(WPes100:'Z'))
040000180314             + ';;;;;;;;;;;;;';
040100180314         exsr VGD_WRITE;
040200180314         //
040300180314         if serv_mail_Attivato = 'S';
040400180314           exsr MSG;
040500180314         ENDIF;
040600180314         //*****
040700180314         //*** Kar4          reade     fiar401l
040800180314         //***               end
040900180314         //
041000180314    2  ENDSR;
041100180314       //--------------------------------------------------------------*
041200180314       //  Se vi è un messaggio da comunicare è legato al PARCEL
041300180314       //--------------------------------------------------------------*
041400180314       BEGSR MSG;
041500180314         //
041600180314         // MSG   invio indirizzo Mail
041700180314         //     (è stato provato solo x la MAIL e non per l'SMS)
041800180314         //
041900180314         //   ATTENZIONE provato solo con la MAIL  / l'SMS va implementato
042000180314         //              se si deciderà di attivare il servizio con DPD (a pagamento)
042100180314         //
042200180314         if fldEML <> *blank;
042300180314           segmento = 'MSG;4;'
042400180314               + ';;1;0;'
042500180314               + %trim(fldEML)
042600180314               + ';904;;;';
042700180314           exsr VGD_WRITE;
042800180314         ENDIF;
042900180314         //
043000180314         if fldEML1<> *blank;
043100180314           segmento = 'MSG;4;'
043200180314               + ';;1;0;'
043300180314               + %trim(fldEML1)
043400180314               + ';904;;;';
043500180314           exsr VGD_WRITE;
043600180314         ENDIF;
043700180314         //
043800180314         //
043900180314         if fldEML2<> *blank;
044000180314           segmento = 'MSG;4;'
044100180314               + ';;1;0;'
044200180314               + %trim(fldEML2)
044300180314               + ';904;;;';
044400180314           exsr VGD_WRITE;
044500180314         ENDIF;
044600180314         //
044700180314         if fldEML3<> *blank;
044800180314           segmento = 'MSG;4;'
044900180314               + ';;1;0;'
045000180314               + %trim(fldEML3)
045100180314               + ';904;;;';
045200180314           exsr VGD_WRITE;
045300180314         ENDIF;
045400180314         //
045500180314         //
045600180314         if fldSMS <> *blank;
045700180314           segmento = 'MSG;4;'
045800180314               + ';;3;0;'
045900180314               + %trim(fldSMS)
046000180314               + ';904;;;';
046100180314           exsr VGD_WRITE;
046200180314         ENDIF;
046300180314         //
046400180314         //
046500180314    2  ENDSR;
046600180314       //--------------------------------------------------------------*
046700180314       // dettaglio spedizione dagli archivi BRT
046800180314       //--------------------------------------------------------------*
046900180314       BEGSR DECODIFICHE;
047000180314         //
047100180314         // con la LNA si iidentificano i dati del Mittente (filiale di Lancio Manifest)
047200180314         clear fldRSM;
047300180314         clear fldINM;
047400180314         clear fldCPM;
047500180314         clear fldLOM;
047600180314         clear fldPRM;
047700180314         clear fldTLM;
047800180314         clear fldFXM;
047900180314         clear fldRSD;
048000180314         clear fldRSD2;
048100180314         clear FldIND;
048200180314         clear fldCAD;
048300180314         clear fldLOD;
048400180314         clear fldPRD;
048500180314         clear fldNZD;
048600180314         clear fldCTA;
048700180314         clear fldTEL;
048800180314         clear fldPREF;
048900180314         clear fldTELcontact;
049000180314         clear fldEML;
049100180314         clear fldEML1;
049200180314         clear fldEML2;
049300180314         clear fldEML3;
049400180314         clear fldSMS;
049500180314         clear fldSMScontact;
049600180314         clear fldNOTE1;
049700180314         clear fldNOTE2;
049701180314      /END-FREE
049800170904     c                   movel     '0'           fld_daORM         1
049900180314      //
049901180314      /FREE
050000180314         clear fldPARCEL;
050100180314         clear fldISC;
050200180314         clear fldCHKdgt;
050300180314         //
050400180314         // Rif.spedizione BRT
050401180314      /END-FREE
050500140310     c                   move      arbaas        anno              4
050600140310     c                   move      arblnp        linea             3
050700140310     c                   move      arbnrs        serie             2
050800140310     c                   move      arbnsp        numero            7
050900180314      //---
051000180314      //  decodifica il DEPOT di Partenza mediante la LNP
051100171010     c                   clear                   SenderDepot       7
051101180314      /FREE
051200180314         chain arbLNP azorg01l;
051300180314         if %Found(azorg01l);
051301180314      /END-FREE
051400171010     c                   movel     orgDE3        og143
051500171010     c                   move      §OGDP1        SenderDepot
051600171010     c                   movel     buBRT         SenderDepot
051601180314      /FREE
051700180314         ENDIF;
051800180314         //---
051801180314      /END-FREE
051900130828     c                   move      bolla16       bolla            14
052000130828
052100180314      // reC 'D'  RAG.SOCIALE 2
052200130827     c                   move      'D'           ktrc
052201180314      /FREE
052300180314         chain Kar4 fiar401l;
052400180314         if %found(fiar401l);
052401180314      /END-FREE
052500130827     c     ';':'.'       xlate     ar4not        fldRSD2
052501180314      /FREE
052600180314         ENDIF;
052700180314         //****
052800180314         // reC 'I'  PARCEL
052900180314         //****              clear                   DSBL4i
053000180314         //***               move      'I'           ktrc
053100180314         //*** Kar4          chain     fiar401l
053200180314         //****              if        %found(fiar401l)
053300180314         //****              movel     AR4not        DSBL4i
053400180314         //****              end
053500180314         //*
053600180314         //  sostituisce il vecchio AR4 tipo "I" --> nuovo FIPND01l
053700180314         chain Kpnd fipnd01l;
053800180314         if %found(fipnd01l);
053801180314      /END-FREE
053900171018     c                   movel     pndIPN        fldPARCEL
054000171018     c                   movel     pndISC        fldISC
054100180314      //*
054200180314      //   prende il CheckDigit
054201180314      /FREE
054300180314           if pndDPE  = 'DP4';
054400180314             DPNDDP4 = pndETIU;
054401180314      /END-FREE
054500171018     c                   movel     §b4ICD        fldCHKdgt
054501180314      /FREE
054600180314           elseif pndDPE  = 'DP5';
054700180314             DPNDDP5 = pndETIU;
054701180314      /END-FREE
054800171018     c                   movel     §pndICD       fldCHKdgt
054801180314      /FREE
054900180314           ENDIF;
055000180314         ENDIF;
055100180314         //
055200180314         // reC 'M'  se la spedizione nasce da un ORM
055201180314      /END-FREE
055300170904     c                   move      'M'           ktrc
055301180314      /FREE
055400180314         chain Kar4 fiar401l;
055500180314         if %found(fiar401l);
055600180314           //
055700180314           tbeCOD = 'LPD';
055800180314           tbeKE1 = %subst(ar4not:1:3);
055900180314           chain Ktbe1 tntbe01l;
056000180314           if %found(tntbe01l);
056001180314      /END-FREE
056100170904     c                   movel     '1'           fld_daORM
056101180314      /FREE
056200180314           ENDIF;
056300180314           //
056400180314         ENDIF;
056500180314         //
056600180314         //  reperisce il volume/peso da CML o da Bollettazione
056700180314         exsr VOL_PES;
056800180314         // mittente
056900180314         chain arbLNA azorg01l;
057000180314         if %Found(azorg01l);
057001180314      /END-FREE
057100130828     c     ';':'.'       xlate     orgDES        fldRSM
057200130828     c     ';':'.'       xlate     orgIND        fldINM
057300130828     c                   z-add     orgCPF        fldCPM
057400130828     c     ';':'.'       xlate     orgLOC        fldLOM
057500130828     c     ';':'.'       xlate     orgPRO        fldPRM
057600130828     c     ';':'.'       xlate     orgTEL        fldTLM
057700130828     c     ';':'.'       xlate     orgFAX        fldFXM
057701180314      /FREE
057800180314         ENDIF;
057900180314         //
058000180314         // Destinatario
058001180314      /END-FREE
058100130827     c     ';':'.'       xlate     arbRSD        fldRSD
058200130827     c     ';':'.'       xlate     arbIND        fldIND
058300130827     c     ';':'.'       xlate     arbCAD        fldCAD
058400130827     c     ';':'.'       xlate     arbLOD        fldLOD
058500130827     c     ';':'.'       xlate     arbPRD        fldPRD
058600180314      //
058700180314      //*** DPD non vuole avere i CAP con degli spazi in mezzo
058800180314      //* quindi compatto il CAP ove vi fossero degli spazi
058900140424     c                   clear                   compat9CAP        9
059000140424     c                   clear                   nr                3 0
059100180314      // compatta i 9 caratteri togliendo i blank fra una sigla e l'altra
059200140424     c                   DO        9             cap               3 0
059201180314      /FREE
059300180314           if %subst(fldCAD:cap:1) <> ' ';
059400180314             nr = nr + 1;
059500180314             %subst(compat9CAP:nr:1)
059600180314                 =  %subst(fldCAD:cap:1);
059700180314           ENDIF;
059800180314         endDO;
059900180314         fldCAD = compat9CAP;
060000180314         //
060100180314         // Queste specifiche sull'Irlanda erano sul FIEU16R:
060200180314         //  (CHIODO xchè Irlanda non ha il CAP)
060300180314         //   Controlla il CAP se scritto "EIRE" (Irlanda) deve essere sostituito
060400180314         //   con "0000001". (Richiesta di Villa da Meeting del 14/10/2008 con DPD.
060500180314         if fldCAD = 'EIRE';
060600180314           // e se non è DUBLIN deve essere "0000002"
060700180314           //    dopo una mail di rettifica del 23/6/2014 si deve inviare solo 1 o 2
060800180314           //    formalmente nel cap di DUBLINO e IRLANDA
060900180314           //  1 --> RDEPOT= 1601    2 --> RDEPOT= 1602
061000180314           //
061100180314           fldCAD = '2';
061200180314           // ma solo    DUBLIN deve essere "0000001"
061201180314      /END-FREE
061300140506     c     'DUBLIN'      SCAN      fldLOD
061301180314      /FREE
061400180314           if %Found;
061500180314             fldCAD = '1';
061600180314           ENDIF;
061700180314         ENDIF;
061800180314         //
061900180314         // Nazione Destinatario
061901180314      /END-FREE
062000130827     C                   Z-ADD     1             YY                3 0
062100130827     C     arbNZD        LOOKUP    C15(YY)                                32
062200130827     C   32ISO(YY)       COMP      *BLANKS                            3232
062201180314      /FREE
062300180314         if *in32;
062301180314      /END-FREE
062400140611     C                   MOVEL     CIE(YY)       fldNZD            3
062500180314      // se c'è il pref.internazionale in tabella Nazioni
062600180314      // imposta il codice preceduto dal '+'
062601180314      /FREE
062700180314           if PRF(YY) <> *blank;
062701180314      /END-FREE
062800140611     C                   MOVEL     PRF(YY)       fldPREF
062801180314      /FREE
062900180314           ENDIF;
063000180314         ENDIF;
063100180314         //
063101180314      /END-FREE
063200031210     c                   movel     'GEN'         ktrd
063201180314      /FREE
063300180314         clear DAr5Gen;
063400180314         chain kar5 fiar501L;
063401180314         *IN34 = NOT %FOUND;
063500180314         if %Found(fiar501L);
063501180314      /END-FREE
063600031210     c                   Movel     Ar5Uni        DAr5Gen
063700130827     c     ';':'.'       xlate     §ar5ref       fldCTA
063800130827     c     ';':'.'       xlate     §ar5teld      fldTEL
063900140612     c                   movel     fldTEL        input16          16
063901180314      /FREE
064000180314           exsr compatta_NR;
064001180314      /END-FREE
064100140612     c                   movel     out16         fldTEL
064200180314      // se c'è già il prefisso
064201180314      /FREE
064300180314           if %subst(fldTEL:1:1) = '+' or
064400180314                 %subst(fldTEL:1:2) = '00';
064500180314             fldTELcontact = fldTEL;
064600180314           else;
064700180314             // se non c'è il prefisso lo imposta solo se presente sulla tab.15
064800180314             if fldPREF <> *blank and fldTEL <> *blank;
064900180314               fldTELcontact = '+'
065000180314                   + %Trim(fldPREF) + %Trim(fldTEL);
065100180314             ENDIF;
065200180314           ENDIF;
065300180314         ENDIF;
065400180314         //
065500180314         // solo se il servizio verrà attivato (al momento salta decodifica)
065600180314         //  nelle definizioni il flag è inizializzato a (N)
065700180314         //
065800180314         //  EMAIL o SMS - come servizio di Alert di consegna -
065900180314         //     ATTENZIONE il programma al momento ha gestito solo la MAIL e
066000180314         //        NON l'SMS --> che dovrà essere implementato se si deciderà
066100180314         //                      l'attivazione del servizio.
066200180314         if serv_mail_Attivato = 'S';
066300180314           //-
066301180314      /END-FREE
066400130827     c                   movel     'EMD'         ktrd
066401180314      /FREE
066500180314           clear DAr5emd;
066600180314           chain kar5 fiar501L;
066601180314           *IN34 = NOT %FOUND;
066700180314           if %Found(fiar501L);
066701180314      /END-FREE
066800130827     c                   Movel     Ar5Uni        DAr5emd
066900180314      //*
067000171108     c                   z-add     0             Pos               3 0
067100171108     c                   z-add     0             Pos1              3 0
067200171108     c                   z-add     0             Pos2              3 0
067300171108     c                   z-add     0             Pos3              3 0
067400180314      //*
067500171108     c     ';'           scan      §ar5EML:1     Pos1
067501180314      /FREE
067600180314             if Pos1 > 0;
067700180314               Pos = Pos1 +1;
067701180314      /END-FREE
067800171108     c     ';'           scan      §ar5EML:Pos   Pos2
067801180314      /FREE
067900180314               if Pos2 > 0;
068000180314                 Pos = Pos2 +1;
068001180314      /END-FREE
068100171108     c     ';'           scan      §ar5EML:Pos   Pos3
068101180314      /FREE
068200180314               ENDIF;
068300180314             ENDIF;
068400180314             //*
068500180314             //*  1 solo indirizzo
068600180314             if Pos1 = 0;
068700180314               fldEML= §ar5EML;
068800180314               //*  2 indirizzi
068900180314             elseif Pos1 > 0 and Pos2 = 0;
069000180314               fldEML = %subst(§ar5EML:1:Pos1-1);
069100180314               fldEML1= %subst(§ar5EML:Pos1+1);
069200180314               //*  3 indirizzi
069300180314             elseif Pos2 > 0 and Pos3 = 0;
069400180314               fldEML = %subst(§ar5EML:1:Pos1-1);
069500180314               fldEML1= %subst(§ar5EML:Pos1+1:Pos2-Pos1-1);
069600180314               fldEML2= %subst(§ar5EML:Pos2+1);
069700180314               //*  4 indirizzi
069800180314             elseif Pos3 > 0;
069900180314               fldEML = %subst(§ar5EML:1:Pos1-1);
070000180314               fldEML1= %subst(§ar5EML:Pos1+1:Pos2-Pos1-1);
070100180314               fldEML2= %subst(§ar5EML:Pos2+1:Pos3-Pos2-1);
070200180314               fldEML3= %subst(§ar5EML:Pos3+1);
070300180314             ENDIF;
070400180314             //*
070401180314      /END-FREE
070500140617     c     ';':' '       xlate     §ar5TEL       fldSMS
070600140617     c                   movel     fldSMS        input16          16
070601180314      /FREE
070700180314             exsr compatta_NR;
070701180314      /END-FREE
070800140617     c                   movel     out16         fldSMS
070900180314      // se c'è già il prefisso
070901180314      /FREE
071000180314             if %subst(fldSMS:1:1) = '+' or
071100180314                   %subst(fldSMS:1:2) = '00';
071200180314               fldSMScontact = fldSMS;
071300180314             else;
071400180314               // se non c'è il prefisso lo imposta solo se presente sulla tab.15
071500180314               if fldPREF <> *blank and fldSMS <> *blank;
071600180314                 fldSMScontact = '+'
071700180314                     + %Trim(fldPREF) + %Trim(fldSMS);
071800180314               ENDIF;
071900180314             ENDIF;
072000180314           ENDIF;
072100180314           //-
072200180314         endIF;
072300180314         //----------------
072400180314         //
072500180314         //----------------
072600180314         //  Controllo se ci sono delle note di consegna
072601180314      /END-FREE
072700020219     C                   MOVEL     '8'           KTRC
072701180314      /FREE
072800180314         CHAIN KAR4 fiar401L;
072801180314         *IN32 = NOT %FOUND;
072900180314         //  Shipment Ref.notes (1°campo note)             solo se c'è
072901180314      /END-FREE
073000150218     c  n32';':'.'       xlate     ar4not        fldnote1
073100180314      //
073200150218     C                   MOVEL     '9'           KTRC
073201180314      /FREE
073300180314         CHAIN KAR4 fiar401L;
073301180314         *IN32 = NOT %FOUND;
073400180314         //  Shipment Ref.notes (2°campo note)             solo se c'è
073401180314      /END-FREE
073500150218     c  n32';':'.'       xlate     ar4not        fldnote2
073600180314      //*
073700180314      //----------------
073800180314      //
073801180314      /FREE
073900180314       endsr;
074000180314       //****************************************************************
074100180314       //  Compatta il campo numero di telefono avvicinando i numeri
074200180314       //****************************************************************
074300180314       BEGSR compatta_NR;
074400180314         //
074401180314      /END-FREE
074500140612     c                   z-add     0             in                3 0
074600140612     c                   clear                   Byte_uno          1
074700140612     c                   clear                   out16            16
074800180314      //
074900180314      //  controlla presenza di caratteri divisori o blank
075000180314      //   li elimina e deve compattare il numero
075100140612     c                   do        16            Nx                3 0
075101180314      /FREE
075200180314           Byte_uno = %subst(input16:Nx:1);
075300180314           // controlla che NON sia un numero :
075301180314      /END-FREE
075400140612     c                   move      'N'           alfa              1
075500140612     c     digits        check     Byte_uno
075501180314      /FREE
075600180314           if %found;
075601180314      /END-FREE
075700140612     c                   move      'S'           alfa
075701180314      /FREE
075800180314           ENDIF;
075900180314           //
076000180314           // tranne sul primo che può esserci il '+'
076100180314           if Nx = 1 and Byte_uno ='+'
076200180314                 or alfa = 'N';
076201180314      /END-FREE
076300140612     c                   add       1             in
076301180314      /FREE
076400180314             %subst(out16:in:1) = %subst(input16:Nx:1);
076500180314           ENDIF;
076600180314         enddo;
076700180314         //
076800180314       endsr;
076900180314       //****************************************************************
077000180314       //  Reperisce Peso/Volume o da CML o da Bollettato
077100180314       //****************************************************************
077200180314       BEGSR VOL_PES;
077300180314         //
077400180314         // Prima cerca il volume fra bollettato e CML
077500180314         IF arbncR = arbncl;
077600180314           // se è considerata un'unica spedizione
077601180314      /END-FREE
077700030317     C                   z-add     ARBVLC        WVOL
077701180314      /FREE
077800180314         else;
077900180314           //
078000180314           IF ARBVLC >= ARBVLb;
078001180314      /END-FREE
078100030317     C                   z-add     ARBVLC        WVOL
078101180314      /FREE
078200180314           else;
078201180314      /END-FREE
078300030317     C                   z-add     ARBVLb        WVOL
078301180314      /FREE
078400180314           ENDIF;
078500180314           //
078600180314         endif;
078700180314         //
078800180314         // PESO VDL - TARA
078801180314      /END-FREE
078900030521     C     §QTTPC        MULT      arbncp        WTARA
079000030521     C                   Z-ADD     arbPKC        wpes
079100150825     c                   sub(h)    wtara         wpes
079200180314      //
079300180314      // Poi cerca il peso fra bollettato e CML
079301180314      /FREE
079400180314         IF arbncP = arbncl;
079500180314           // se è considerata un'unica spedizione
079600180314         ELSE;
079700180314           //  altrimenti se il bollettato è superiore passo il Bollettato.
079800180314           IF wpes < arbPKb;
079801180314      /END-FREE
079900030317     C                   Z-ADD     arbPKb        wpes
079901180314      /FREE
080000180314           ENDIF;
080100180314           //
080200180314         ENDIF;
080300180314         //
080400180314         // Se il peso è superiore al limite del fuori misura impostiamo fisso 31 kg.
080500180314         if wpes > 31,5;
080600180314           wpes = 31;
080700180314         ENDIF;
080800180314         //
080900180314         Wpes100 = ( wpes * 100);
081000180314         //
081100180314       ENDSR;
081200180314       //*?__________________________________________________________________ */
081300180314       //    Apertura del TIVGD
081400180314       //*?__________________________________________________________________ */
081500180314       Begsr VGD_OPEN;
081600180314         //
081700180314         //  recupera il progressivo
081800180314         exsr Piglia_progr;
081900180314         //
082000180314         //  istruzioni apertura blocco scrittura TIVGD    Edi Standard
082100180314         clear trul47ds;
082200180314         d47opz  = 'I';
082300180314         d47tip  = TipoF;
082400180314         d47lck  = 'N';
082500180314         d47chkj = 'N';
082600180314         d47pgm  = 'TRTC83R13';
082601180314      /END-FREE
082700130826     C                   call      'TRUL47R'
082800130826     C                   parm                    trul47ds
082900180314      //
082901180314      /FREE
083000180314       endsr;
083100180314       //*?__________________________________________________________________ */
083200180314       //   Scrittura del TIVGD
083300180314       //*?__________________________________________________________________ */
083400180314       Begsr VGD_WRITE;
083500180314         eval Conta_righe = 1 + Conta_righe;
083600180314         //
083700180314         clear tivgd000;
083800180314         vgddta = %TrimR(Segmento);
083900180314         vgdtip = TipoF;
084000180314         vgdksu = CLIENTE;
084100180314         vgdprg = Progressivo;
084200180314         vgdtsc = 'WW';
084300180314         vgdpgm = 'TRTC83R13';
084400180314         vgddat = woggi;
084500180314         //
084600180314         WRITE tivgd000;
084700180314         //
084800180314       endsr;
084900180314       //*?__________________________________________________________________ */
085000180314       //    Chiusura del TIVGD
085100180314       //*?__________________________________________________________________ */
085200180314       Begsr VGD_CLOSE;
085300180314         //
085400180314         // Infine elimino il blocco elaborazione TIVGD    Edi Standard
085500180314         clear trul47ds;
085600180314         d47opz  = 'F';
085700180314         d47tip  = TipoF;
085701180314      /END-FREE
085800130826     C                   call      'TRUL47R'
085900130826     C                   parm                    trul47ds
086000180314      //
086001180314      /FREE
086100180314       endsr;
086200180314       //*?__________________________________________________________________ */
086300180314       //   Prende il Progressivo
086400180314       //*?__________________________________________________________________ */
086500180314       Begsr Piglia_progr;
086600180314         //
086601180314      /END-FREE
086700130826     C                   MOVEL     CLIENTE       virKSC
086800130826     C                   MOVEL     TipoF         virTIP
086801180314      /FREE
086900180314         chain kvir02 tivir02l;
086901180314      /END-FREE
087000130828     c                   move      'OF'          Var_ISV
087001180314      /FREE
087100180314         if %Found(tivir02l);
087101180314      /END-FREE
087200130826     c                   move      VirFI1        Var_ISV
087201180314      /FREE
087300180314         ENDIF;
087400130826
087500180314         //       prende numeratore strategi
087600180314         exsr calprog;
087700180314         //
087800180314       endsr;
087900180314       //*?__________________________________________________________________ */
088000180314       //
088100180314       //*?__________________________________________________________________ */
088200180314       begsr calprog;
088300180314         //
088301180314      /END-FREE
088400130828     c                   clear                   progressivo      10
088401180314      /FREE
088500180314         open tis7prgf;
088600180314         read(e) tis7prgf;
088700180314         //
088800180314         if not %error;
088900180314           dwlprg = f_tis7prgf;
089000180314           //
089001180314      /END-FREE
089100130826     C                   move(p)   dwlprg        wrkprg
089200130826     C                   add       1             wrkprg
089300130826     C                   move(p)   wrkprg        dwlprg
089400130826     C                   movel     Var_ISV       dwlprg
089500180314      //
089501180314      /FREE
089600180314           eval f_tis7prgf = dwlprg;
089700180314           Progressivo = dwlprg;
089800180314           //
089900180314           update tis7prg0;
090000180314         endif;
090100180314         //
090200180314         close tis7prgf;
090300180314         //
090400180314       endsr;
090500180314       //*?__________________________________________________________________ */
090600180314       //*?__________________________________________________________________ */
090700180314       BEGSR *INZSR;
090800180314         //
090900180314         // Definisco chiavi di accesso
091000180314         //
091001180314      /END-FREE
091100130828     c     kvir02        klist
091200130828     C                   kfld                    virKSC
091300130828     C                   kfld                    virTIP
091400180314      //
091500130826     C     KARB          KLIST
091600130826     C                   KFLD                    KAAS
091700130826     C                   KFLD                    KLNP
091800130826     C                   KFLD                    KNRS
091900130826     C                   KFLD                    KNSP
092000180314      //
092100130827     C     KAR4          KLIST
092200130827     C                   KFLD                    KAAS
092300130827     C                   KFLD                    KLNP
092400130827     C                   KFLD                    KNRS
092500130827     C                   KFLD                    KNSP
092600130827     C                   KFLD                    KTRC              1
092700180314      //
092800130826     C     KAR5          KLIST
092900130826     C                   KFLD                    KAAS
093000130826     C                   KFLD                    KLNP
093100130826     C                   KFLD                    KNRS
093200130826     C                   KFLD                    KNSP
093300130826     C                   KFLD                    KTRD              3
093400180314      //
093500171018     C     Kpnd          KLIST
093600171018     C                   KFLD                    KAAS
093700171018     C                   KFLD                    KLNP
093800171018     C                   KFLD                    KNRS
093900171018     C                   KFLD                    KNSP
094000180314      //
094100130826     C     KTAB          KLIST
094200130826     C                   KFLD                    KKUT
094300130826     C                   KFLD                    KCOD
094400180314      //
094500130826     C     KTAB1         KLIST
094600130826     C                   KFLD                    KKUT
094700130826     C                   KFLD                    KCOD
094800130826     C                   KFLD                    KKEY
094900180314      //
095000170904     C     KTBE1         KLIST
095100170904     C                   KFLD                    tbecod
095200170904     C                   KFLD                    tbeke1
095300180314      //
095400180314      //------------
095500180314      // Inizializzo variabili totale
095600180314      // Imposto data del giorno
095700130826     C                   TIME                    W0140            14 0
095800130826     C                   MOVE      W0140         G02DAT            8 0
095900130826     C                   MOVEL     W0140         Wora              6 0
096000130826     C                   MOVEL     *BLANK        G02ERR
096100130826     C                   CALL      'XSRDA8'
096200130826     C                   PARM                    WLBDA8
096300130826     C                   MOVEL     G02INV        WOGGI             8 0
096400180314      //
096500180314      //* CARICAMENTO DATI VARI TASSAZIONE
096600130826     C                   Z-ADD     1             KKUT
096700130826     C                   MOVEL     'QT'          kcod
096800130826     C                   MOVEL     *BLANKS       kKEY
096900130826     C                   MOVEL     '1'           kKEY
096901180314      /FREE
097000180314         CHAIN KTAB1 TABEL00F;
097001180314         *IN30 = NOT %FOUND;
097002180314      /END-FREE
097100130826     C  N30              MOVEL     TBLUNI        DSQT1
097200180314      //
097300180314      //------------
097400180314      // Caricamento Tabella codici nazioni
097500130826     C                   Z-ADD     0             X                 3 0
097600130826     C                   MOVEL     '15'          KCOD
097700130826     C                   Z-ADD     1             KKUT
097701180314      /FREE
097800180314         CHAIN KTAB TABEL00F;
097801180314         *IN30 = NOT %FOUND;
097900180314         DOW *IN30 = '0';
098000180314           IF TBLFLG = *BLANKS;
098001180314      /END-FREE
098100130826     C                   ADD       1             X
098200130826     C                   MOVEL     TBLKEY        C15(X)
098300130826     C                   MOVEL     TBLUNI        DS15
098400130826     C                   MOVEL     §15COD        ISO(X)
098500130826     C                   MOVEL     §15des        DNZ(X)
098600130826     C                   MOVEL     §15ACN        ACN(X)
098700130828     C                   MOVEL     §15CIE        CIE(X)
098800140611     C                   MOVEL     §15PREF       prf(X)
098801180314      /FREE
098900180314           ENDIF;
099000180314           READE KTAB TABEL00F;
099001180314           *IN30 = %EOF;
099100180314         ENDDO;
099200180314         //------------
099300180314         //  prende dalla "3I" il nostro codice di B.U.
099301180314      /END-FREE
099400171010     C                   Z-ADD     1             KKUT
099500171010     C                   MOVEL     '3I'          kcod
099600171010     C                   MOVEL     *BLANKS       kKEY
099700171010     C                   MOVEL     'DPD'         kKEY
099800171010     C                   clear                   BUBRT             3 0
099801180314      /FREE
099900180314         CHAIN KTAB1 TABEL00F;
099901180314         *IN30 = NOT %FOUND;
099902180314      /END-FREE
100000171010     C  N30              MOVEL     TBLUNI        DS3Idp
100100171010     C  N30              MOVEL     §3IBUCN       BUBRT
100200180314      //
100300180314      //------------
100400180314      //
100401180314      /FREE
100500180314       ENDSR;
100600180314       //*?__________________________________________________________________ */
100601180314      /END-FREE
