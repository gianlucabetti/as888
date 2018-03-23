000100120508     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('UBBNDDIR')
000200080613
000300120222     FFIVAB04L  UF A E           K DISK    usropn commit
000400120222     F                                     extfile(LibFileVAB)
000500120222     FFIVAT00F  O    E             DISK    usropn commit
000600120222     F                                     extfile(LibFileVAT)
000700080612
000800120222     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
000900120222     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
001000170921     D/COPY GAITRASRC/SRCTITV,TITV3A8DS1
001100120222
001200120222
001300120222     d LibFileVAB      s             21A   inz
001400120222     d LibFileVAT      s             21A   inz
001500120222     d currSysNeta     s              8A   inz('*NULL')
001600120403
001700091207
001800120222     d tivlrds       e ds                  extname(tivlr00f)
001900120222     d esito           s              1a
002000120222     d prmlit          s             10a
002100120222     d prmfir          s             10a
002200120222     d prmpathIFS      s            255a
002300120222     d prmfilIFS       s            255a
002400120222     d prmextIFS       s            255a
002500120222     d wrkesito        s              1a
002600120222     d wrkfile         s           1024a   varying
002700170921     d oracor          s              6s 0 inz
002800120222
002900090112
003000080617     ddsCommarea       ds                  qualified
003100080618     d Ok                              n
003200080618     d Totale                        10i 0
003300120222     d Seg                          255a
003400120222     d Fld                          255a
003500080624
003600080514
003700170921     dHndE1EDT20       pr            10i 0
003800080618     d commarea                            likeds(dscommarea)
003900170921     d E1EDT20                             likeds(dsE1EDT20) dim(1) const
004000080618     d numRecs                       10i 0 value
004100081117
004200120222
004300120403     d K_FIVAB04_C   e ds                  extname(fivab04l:*key) prefix(K_) inz
004400120222
004500120222
004600120222     c     *entry        plist
004700120222     c                   parm                    tivlrds
004800120222     c                   parm      wrkesito      esito
004900120222     c                   parm                    prmlit
005000120222     c                   parm                    prmfir
005100120222     c                   parm                    prmpathIFS
005200120222     c                   parm                    prmfilIFS
005300120222     c                   parm                    prmextIFS
005400120222
005500080919
005600081201       *inlr = *on;
005700081201
005800170921       oracor = %dec(%time() : *iso);
005900120222       wrkfile = %trim(prmpathIFS) + %trim(prmfilIFS) + %trim(prmextIFS);
006000080922
006100081201       monitor;
006200120222
006300120222         // Reperisco il sistema AS/400 corrente
006400120222         if UBRTVNETA_Rtv(currSysNeta) = *zero;
006500120222            if %subst(currSysNeta:1:6) = 'SETRAS';
006600120222               LibFileVAB = 'FILTRAGRU/FIVAB04L';
006700120222               LibFileVAT = 'FILTRAGRU/FIVAT00F';
006800120222            else;
006900120222               LibFileVAB = 'FILTRAGRPF/FIVAB04L';
007000120222               LibFileVAT = 'FILTRAGRPF/FIVAT00F';
007100120222            endif;
007200120222         else;
007300120222            LibFileVAB = 'FILTRAGRPF/FIVAB04L';
007400120222            LibFileVAT = 'FILTRAGRPF/FIVAT00F';
007500120222         endif;
007600120222
007700120222         if not %open(FIVAB04L);
007800120222            open FIVAB04L;
007900120222         endif;
008000120222
008100120222         if not %open(FIVAT00F);
008200120222            open FIVAT00F;
008300120222         endif;
008400120222
008500081201
008600120222         // Caricamento intero XML - da segmenti NNPmde
008700091207         dscommarea.ok  = *on;
008800091207         dscommarea.totale = *zeros;
008900170921         dscommarea.seg = 'E1EDT20';
009000170921         xml-into %handler(HndE1EDT20 : dscommarea)
009100120222                  %xml(wrkfile : 'doc=file ' +
009200170921                  'path=SHPMNT05/IDOC/E1EDT20 ' +
009300091207                  'allowmissing=yes allowextra=yes case=any');
009400081201
009500081201         commit;
009600081201
009700081201       on-error;
009800120222         wrkesito = '2';
009900081201         rolbk;
010000170921         dump(A);
010100081201       endmon;
010200170921
010300170921         if %open(FIVAB04L);
010400170921            close FIVAB04L;
010500170921         endif;
010600170921
010700170921         if %open(FIVAT00F);
010800170921            close FIVAT00F;
010900170921         endif;
011000170921
011100170921       wrkesito = '0';
011200170921       return;
011300120222
011400080902
011500120222
011600091207      ****************************************************************
011700170921      ** HndE1EDT20 - Handler segmento E1EDT20
011800091207      ****************************************************************
011900170921     pHndE1EDT20       b
012000170921     dHndE1EDT20       pi            10i 0
012100091207     d commarea                            likeds(dscommarea)
012200170921     d E1EDT20                             likeds(dsE1EDT20) dim(1) const
012300091207     d numRecs                       10i 0 value
012400120403
012500170921     d wRcdCount       s              6s 0 inz
012600170921     d wEmail          s             70a   inz
012700170921     d wUnitWeight     s             30s 9 inz
012800170921
012900120222
013000120222       commarea.totale = commarea.totale + 1;
013100120222
013200120222       exsr ScriviVAT;
013300120222       exsr ScriviVAB;
013400101015
013500091207       return 0;
013600120222
013700120222
013800101015       //***********************************************************************
013900120222       //********************************************* Routine scrittura FIVAT
014000101015       //***********************************************************************
014100101015
014200120222       begsr ScriviVAT;
014300101015
014400170921         // Incremento il contatore di Record
014500170921         wRcdCount = wRcdCount + 1;
014600170921
014700170921
014800170921         // Inizializzo e imposto il buffer output VAT
014900120222         clear fivat000;
015000101015
015100120222         vatFGS = vlrPOI;
015200120223         vatCCM = %dec(%subst(vlrPPT:1:7):7:0);
015300120223         vatLNP = %dec(%subst(vlrPPT:8:3):3:0);
015400120222         vatAAS = %subdt(%date() : *years);
015500120222         vatNRS = *zeros;
015600170921         vatNSP = oracor + wRcdCount;
015700170921
015800150430
015900120222         // Record x "chi sono"
016000170921         vatTRC = 'E';
016100170921         vatNOT = %trim(E1EDT20(1).E1EDT37.VEBEZ);
016200120223         if vatNOT <> *blanks;
016300120223            write fivat000;
016400120223         endif;
016500170921
016600170921         // Record x Telefono Referente consegna
016700170921         vatTRC = 'B';
016800170921         vatNOT = %trim(E1EDT20(1).E1ADRM4.TELEPHONE1);
016900170921         if vatNOT <> *blanks;
017000170921            write fivat000;
017100170921         endif;
017200170921
017300170921         // Record x Email destinatario
017400170921         wEmail = %trim(E1EDT20(1).E1ADRM4.E_MAIL);
017500170921         vatTRC = 'I';
017600170921         vatNOT = %subst(wEmail:1:35);
017700170921         if vatNOT <> *blanks;
017800170921            write fivat000;
017900170921         endif;
018000170921         vatTRC = 'J';
018100170921         vatNOT = %subst(wEmail:36:35);
018200170921         if vatNOT <> *blanks;
018300170921            write fivat000;
018400170921         endif;
018500101015
018600101015       endsr;
018700120222
018800120222
018900120222       //***********************************************************************
019000120222       //********************************************* Routine scrittura FIVAB
019100120222       //***********************************************************************
019200120222
019300120222       begsr ScriviVAB;
019400120222
019500170921         // Inizializzo e imposto il buffer output VAB
019600120222         clear fivab000;
019700120223
019800120223         vabCBO = '1';
019900120223         vabCTR = *zeros;
020000170921         vabNCL = 1;
020100120222
020200120222         vabFGS = vlrPOI;
020300150430         vabCCM = vatCCM;
020400150430         vabLNP = vatLNP;
020500120223         vabCTM = %subst(vlrPPT:11:2);
020600150430         vabAAS = vatAAS;
020700150417         vabMGS = %subdt(%date() : *months)*100 + %subdt(%date() : *days);
020800150430         vabNRS = vatNRS;
020900150430         vabNSP = vatNSP;
021000120403
021100120403
021200120403         K_vabAAS = vabAAS;
021300120403         K_vabLNP = vabLNP;
021400120403         K_vabNRS = vabNRS;
021500120403         K_vabNSP = vabNSP;
021600120222         chain  %kds(K_FIVAB04_C) fivab04l;
021700120222         if %found(fivab04l);
021800170921            vabNCL = vabNCL + 1;
021900170921            vabPKB = vabPKB + wUnitWeight;
022000120222            update fivab000;
022100120222         else;
022200170921            wUnitWeight = E1EDT20(1).E1EDT37.BRGEW;
022300170921            vabRMA = %trim(E1EDT20(1).TKNUM);
022400170921            vabRMN = %dec(E1EDT20(1).E1EDL20.VBELN:15:0);
022500170921            vabNAS = %trim(E1EDT20(1).ROUTE);
022600170921            vabDCR = E1EDT20(1).E1EDT10.NTEND;
022700170921            vabHCR = %int(E1EDT20(1).E1EDT10.NTENZ/100);
022800170921            if %trim(E1EDT20(1).E1ADRM4.PARTNER_Q) = 'WE';
022900170921               vabRSD = %trim(E1EDT20(1).E1ADRM4.NAME1);
023000170921               vabRD2 = %trim(E1EDT20(1).E1ADRM4.NAME2);
023100170921               vabIND = %trim(E1EDT20(1).E1ADRM4.STREET1);
023200170921               vabCAD = %trim(E1EDT20(1).E1ADRM4.POSTL_COD1);
023300170921               vabLOD = %trim(E1EDT20(1).E1ADRM4.CITY1);
023400170921               vabNZD = %trim(E1EDT20(1).E1ADRM4.COUNTRY1);
023500170921               if vabNZD = 'IT' or vabNZD = 'it';
023600170921                  vabNZD = *blanks;
023700170921               endif;
023800170921            endif;
023900120223
024000120223            write fivab000;
024100120223         endif;
024200120222
024300120222       endsr;
024400120403
024500120403
024600120222
024700170921     pHndE1EDT20       e
024800120222
024900120222
