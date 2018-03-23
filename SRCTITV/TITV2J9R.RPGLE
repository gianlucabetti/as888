000100120508     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('UBBNDDIR')
000200080613
000300120222     FFIVAB04L  UF A E           K DISK    usropn commit
000400120222     F                                     extfile(LibFileVAB)
000500120222     FFIVAT00F  O    E             DISK    usropn commit
000600120222     F                                     extfile(LibFileVAT)
000700120403     FTIVGD00F  O    E             DISK
000800080612
000900120222     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
001000120222     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
001100120222     D/COPY GAITRASRC/SRCTITV,TITV2J9DS
001200120222
001300120222
001400120222     d LibFileVAB      s             21A   inz
001500120222     d LibFileVAT      s             21A   inz
001600120222     d currSysNeta     s              8A   inz('*NULL')
001700120403
001800091207
001900120222     d tivlrds       e ds                  extname(tivlr00f)
002000120222     d esito           s              1a
002100120222     d prmlit          s             10a
002200120222     d prmfir          s             10a
002300120222     d prmpathIFS      s            255a
002400120222     d prmfilIFS       s            255a
002500120222     d prmextIFS       s            255a
002600120222     d wrkesito        s              1a
002700120222     d wrkfile         s           1024a   varying
002800120222
002900090112
003000080617     ddsCommarea       ds                  qualified
003100080618     d Ok                              n
003200080618     d Totale                        10i 0
003300120222     d Seg                          255a
003400120222     d Fld                          255a
003500080624
003600080514
003700120222     dHndNNPmde        pr            10i 0
003800080618     d commarea                            likeds(dscommarea)
003900120222     d NNPmde                              likeds(dsNNPmde) dim(1) const
004000080618     d numRecs                       10i 0 value
004100081117
004200120222
004300120403     d K_FIVAB04_C   e ds                  extname(fivab04l:*key) prefix(K_) inz
004400120403     d fnvabds       e ds                  extname(fnvab00t) inz
004500120222
004600120222
004700120222     c     *entry        plist
004800120222     c                   parm                    tivlrds
004900120222     c                   parm      wrkesito      esito
005000120222     c                   parm                    prmlit
005100120222     c                   parm                    prmfir
005200120222     c                   parm                    prmpathIFS
005300120222     c                   parm                    prmfilIFS
005400120222     c                   parm                    prmextIFS
005500120222
005600091207
005700080514      /free
005800080919
005900081201       *inlr = *on;
006000081201
006100120222       wrkesito = '0';
006200120222       wrkfile = %trim(prmpathIFS) + %trim(prmfilIFS) + %trim(prmextIFS);
006300080922
006400081201       monitor;
006500120222
006600120222         // Reperisco il sistema AS/400 corrente
006700120222         if UBRTVNETA_Rtv(currSysNeta) = *zero;
006800120222            if %subst(currSysNeta:1:6) = 'SETRAS';
006900120222               LibFileVAB = 'FILTRAGRU/FIVAB04L';
007000120222               LibFileVAT = 'FILTRAGRU/FIVAT00F';
007100120222            else;
007200120222               LibFileVAB = 'FILTRAGRPF/FIVAB04L';
007300120222               LibFileVAT = 'FILTRAGRPF/FIVAT00F';
007400120222            endif;
007500120222         else;
007600120222            LibFileVAB = 'FILTRAGRPF/FIVAB04L';
007700120222            LibFileVAT = 'FILTRAGRPF/FIVAT00F';
007800120222         endif;
007900120222
008000120222         if not %open(FIVAB04L);
008100120222            open FIVAB04L;
008200120222         endif;
008300120222
008400120222         if not %open(FIVAT00F);
008500120222            open FIVAT00F;
008600120222         endif;
008700120222
008800081201
008900120222         // Caricamento intero XML - da segmenti NNPmde
009000091207         dscommarea.ok  = *on;
009100091207         dscommarea.totale = *zeros;
009200120222         dscommarea.seg = 'NNPmde';
009300120222         xml-into %handler(HndNNPmde : dscommarea)
009400120222                  %xml(wrkfile : 'doc=file ' +
009500120222                  'path=NNPInboundManifest/Packages/NNPmde ' +
009600091207                  'allowmissing=yes allowextra=yes case=any');
009700081201
009800081201         commit;
009900081201         return;
010000081201
010100081201       on-error;
010200120222         wrkesito = '2';
010300081201         rolbk;
010400081201         return;
010500081201       endmon;
010600120222
010700120222      /end-free
010800080902
010900120222
011000091207      ****************************************************************
011100120222      ** HndNNPmde - Handler segmento NNPmde
011200091207      ****************************************************************
011300120222     pHndNNPmde        b
011400120222     dHndNNPmde        pi            10i 0
011500091207     d commarea                            likeds(dscommarea)
011600120222     d NNPmde                              likeds(dsNNPmde) dim(1) const
011700091207     d numRecs                       10i 0 value
011800120222
011900120222     d wShipVolume     s             30  9 inz
012000120403
012100101015
012200120222      /free
012300120222
012400120222       commarea.totale = commarea.totale + 1;
012500120222
012600120222       exsr ScriviVAT;
012700120222       exsr ScriviVAB;
012800101015
012900091207       return 0;
013000120222
013100120222
013200101015       //***********************************************************************
013300120222       //********************************************* Routine scrittura FIVAT
013400101015       //***********************************************************************
013500101015
013600120222       begsr ScriviVAT;
013700101015
013800120222         clear fivat000;
013900101015
014000120222         vatFGS = vlrPOI;
014100120223         vatCCM = %dec(%subst(vlrPPT:1:7):7:0);
014200120223         vatLNP = %dec(%subst(vlrPPT:8:3):3:0);
014300120222         vatAAS = %subdt(%date() : *years);
014400120222         vatNRS = *zeros;
014500120223         vatNSP = %int(%subst(%editc(NNPmde(1).Master.ShipmentPointer:'X'):3));
014600150430
014700150430
014800150430         // Gestione traffici mittenti CCM e relative tariffe CTR
014900150430         if NNPmde(1).Shipment.ServiceCode = 70 or
015000150430            NNPmde(1).Shipment.ServiceCode = 86;
015100150430            eval VATCCM = 0501331;
015200150430         else;
015300151217            // eval VATCCM = 0506514;
015400151217            eval VATCCM = 0501331;
015500150430         endif;
015600101015
015700150430
015800120222         // Record x "chi sono"
015900120222         vatTRC = 'E';
016000120228         vatNOT = %subst(%editc(NNPmde(1).Package.TrackingNumber:'X'):3:12);
016100120223         if vatNOT <> *blanks;
016200120223            write fivat000;
016300120223         endif;
016400120222
016500120222         // Calcolo il volume del collo corrente
016600120508         wShipVolume = NNPmde(1).Package.Dimension.Length *
016700120508                       NNPmde(1).Package.Dimension.Width  *
016800120508                       NNPmde(1).Package.Dimension.Height / 1000000;
016900101015
017000101015       endsr;
017100120222
017200120222
017300120222       //***********************************************************************
017400120222       //********************************************* Routine scrittura FIVAB
017500120222       //***********************************************************************
017600120222
017700120222       begsr ScriviVAB;
017800120222
017900120222         clear fivab000;
018000120223
018100120223         vabCBO = '1';
018200120223         vabCTR = *zeros;
018300120222
018400120222         vabFGS = vlrPOI;
018500150430         vabCCM = vatCCM;
018600150430         vabLNP = vatLNP;
018700120223         vabCTM = %subst(vlrPPT:11:2);
018800150430         vabAAS = vatAAS;
018900150417         vabMGS = %subdt(%date() : *months)*100 + %subdt(%date() : *days);
019000150430         vabNRS = vatNRS;
019100150430         vabNSP = vatNSP;
019200120403
019300120403
019400120403         K_vabAAS = vabAAS;
019500120403         K_vabLNP = vabLNP;
019600120403         K_vabNRS = vabNRS;
019700120403         K_vabNSP = vabNSP;
019800120222         chain  %kds(K_FIVAB04_C) fivab04l;
019900120222         if %found(fivab04l);
020000120222            vabVLB = vabVLB + wShipVolume;
020100120222            update fivab000;
020200120403            exsr wrivgd;
020300120222         else;
020400120222            vabRMA = %trim(%editc(NNPmde(1).Master.ShipmentPointer:'X'));
020500120222            vabRMN = NNPmde(1).Master.AirbillNbr;
020600120222            vabNAS = NNPmde(1).Shipment.Content;
020700120223            vabRSD = NNPmde(1).Delivery.Consignee.CompanyName;
020800120223            if vabRSD = *blanks;
020900120223               vabRSD = NNPmde(1).Delivery.Consignee.Name;
021000120223            endif;
021100120223            vabIND = NNPmde(1).Delivery.Consignee.StreetAddress1;
021200120508            vabRD2 = NNPmde(1).Delivery.Consignee.StreetAddress2;
021300120223            vabLOD = NNPmde(1).Delivery.Consignee.City;
021400120223            vabPRD = NNPmde(1).Delivery.Consignee.ProvinceCode;
021500120223            vabCAD = NNPmde(1).Delivery.Consignee.PostalCode;
021600120223            if NNPmde(1).Delivery.Consignee.CountryCode = 'IT';
021700120222            else;
021800120223               vabNZD = NNPmde(1).Delivery.Consignee.CountryCode;
021900120222            endif;
022000120222            vabVLB = vabVLB + wShipVolume;
022100120222            vabNCL = NNPmde(1).Shipment.TotalePieces;
022200120222            if NNPmde(1).Shipment.WeightType = 'K';
022300120223               eval(h) vabPKB = %int(%subst(
022400120223                        %editc(NNPmde(1).Shipment.TotalWeight:'X'):3))/100;
022500120222            else;
022600120223               eval(h) vabPKB = %int(%subst(
022700120223                        %editc(NNPmde(1).Shipment.TotalWeight:'X'):3))/100
022800120223                        * 0,450 / 100;
022900120222            endif;
023000150430
023100150430
023200150430            // Forzatura codici tariffe VABCTR
023300150430            if VABCCM = 0501331;
023400150430               eval VABCTR = 000;
023500150430            else;
023600150430               eval VABCTR = 300;
023700150430            endif;
023800120222
023900150430
024000120222            // Gestione contrassegno
024100120223            if NNPmde(1).Clearance.ROD.RodTotalAmount > 0;
024200120223               vabCBO = '4';
024300120223               vabCAS = NNPmde(1).Clearance.ROD.RodTotalAmount;
024400120223               vabVCA = NNPmde(1).Clearance.ROD.RodCurrency;
024500120222               vabTIC = 'BM';
024600120222            endif;
024700120222
024800120223
024900120223            // Record x Referente consegna
025000120223            vatTRC = 'A';
025100120223            vatNOT = NNPmde(1).Delivery.Consignee.Name;
025200120223            if vatNOT <> *blanks;
025300120223               write fivat000;
025400120223            endif;
025500120223
025600120223            // Record x Telefono referente consegna
025700120223            vatTRC = 'B';
025800120223            vatNOT = NNPmde(1).Delivery.Consignee.PhoneNumber;
025900120223            if vatNOT <> *blanks;
026000120223               write fivat000;
026100120223            endif;
026200120223
026300120223            write fivab000;
026400120403            exsr wrivgd;
026500120223         endif;
026600120222
026700120222       endsr;
026800120222
026900120222      /end-free
027000120403
027100120403
027200120403
027300120403     C*------------------------------------------------------------------------*
027400120403     C/TITLE Scrittura record FNVAB00F in file TIVGD00F (file VAS generico download)
027500120403     C     wriVGD        BEGSR
027600120403     C*
027700120403     C                   clear                   tivgd000
027800120403     C                   eval      vgdDTA = fnvabds
027900120403     C                   eval      vgdTIP = 'EB'
028000120403     C                   eval      vgdKSU = vlrKSC
028100120403     C                   eval      vgdTSC = 'WW'
028200120403     C                   eval      vgdDAT = %dec(%date() : *ISO)
028300120403     C                   eval      vgdPGM = 'TITV2J9C'
028400120403     C                   write     tivgd000
028500120403     C*
028600120403     C                   ENDSR
028700120403     C*------------------------------------------------------------------------*
028800120403
028900120403
029000120222
029100120222     pHndNNPmde        e
029200120222
029300120222
