000100970526     H*------------------------------------------------------------------------*
000200161110     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000300970526     F*------------------------------------------------------------------------*
000400170327
000500170629     FWFCAPDPD0FO    E             DISK
000600170629     F                                     extdesc('EDPFGDPD/WFCAPDPD0F')
000700170629     F                                     extfile('EDPFGDPD/WFCAPDPD0F')
000800170629     F                                     prefix(x_)
000900170327
001000170327       //--------------------------------------------------------------
001100170327       // Definizione costanti.
001200170327       //--------------------------------------------------------------
001300170327
001400170327
001500170327       //--------------------------------------------------------------
001600170327       // Definizione schiere.
001700170327       //--------------------------------------------------------------
001800170327
001900170327
002000170327       //--------------------------------------------------------------
002100170327       // Definizione aree dati.
002200170327       //--------------------------------------------------------------
002300170327
002400170327
002500170327       //--------------------------------------------------------------
002600170327       // Definizione strutture dati.
002700170327       //--------------------------------------------------------------
002800170327
002900170629     d titas00f      e ds
003000170629
003100170629     D TISIE3DS      E DS                  INZ
003200170629
003300170629     D TISID4DS      E DS                  INZ
003400170629
003500170629     d CAPDPD_fetch    ds                  inz
003600170629     d  AAS                                like(tasaas)
003700170629     d  MGS                                like(tasmgs)
003800170629     d  NZD                                like(tasnzd)
003900170629     d  CAD                                like(tascad)
004000170629     d  LOD                                like(taslod)
004100170629     d  LNP                                like(taslnp)
004200170629     d  LNA                                like(taslna)
004300170328
004400170327       // - Status
004500170327     d Psds           sds
004600170327     d   SDSpgm          *proc
004700170327     d   JobName             244    253                                         Job name
004800170327     d   JobUser             254    263                                         User name
004900170327     d   JobNumber           264    269s 0                                      Job number
005000170327
005100170327       //--------------------------------------------------------------
005200170327       // Definizione variabili.
005300170327       //--------------------------------------------------------------
005400170629
005500170629       // parametro d'ingresso
005600170629     d p_DtSpeD        s              8
005700170629     d p_DtSpeA        s              8
005800170629     d p_LNA           s              3
005900170629     d p_SRVC          s              3
006000170629     d p_ERR           s              1
006100170327
006200170327       // Campi di comodo
006300170629     d wSQl            s           1024    varying
006400170703     d OUTTIPO         s                   like(x_OUTTIPO)
006500170327
006600170327       //--------------------------------------------------------------
006700170327       // Definizione prototipi procedure.
006800170327       //--------------------------------------------------------------
006900170327
007000170327       // Parametri API QCAPCMD (Process Commands)?
007100170327     d Qcmd            s           2048    inz  varying
007200170327      /copy qSysInc/qRpgleSrc,QCAPCMD
007300170327       // API QCAPCMD (Process Commands)?
007400170327      /copy gaitrasrc/srcProtoPR,QCAPCMD
007500170327
007600170327       // - Parametri gestione errori API
007700170327      /copy qSysInc/qRpgleSrc,QUSEC
007800170327
007900170327       //--------------------------------------------------------------
008000170327       // Definizione key-list.
008100170327       //--------------------------------------------------------------
008200170327
008300170327
008400170327       //--------------------------------------------------------------
008500170327       // Definizione parametri procedura.
008600170327       //--------------------------------------------------------------
008700170327
008800170629     C*
008900170629     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009000170629     C
009100170629     C/EXEC SQL
009200170629     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009300170629     C/END-EXEC
009400170629     C*
009500170327     c     *Entry        plist
009600170629     c                   parm                    p_DtSpeD
009700170629     c                   parm                    p_DtSpeA
009800170629     c                   parm                    p_LNA
009900170629     c                   parm                    p_SRVC
010000170629     c                   parm                    p_ERR
010100170327
010200170327       //--------------------------------------------------------------
010300170327       // M A I N - L I N E
010400170327       //--------------------------------------------------------------
010500170327
010600170327       // Operazioni iniziali?
010700170327       exsr  sr_RoutInz;
010800170327
010900170629       // Scorro il cursore, chiamo i driver e scrivo incongruenze
011000170629       exsr  CurList;
011100170327
011200170327       // esco
011300170327       exsr sr_RoutEnd;
011400170327
011500170327       //--------------------------------------------------------------
011600170327       // Operazioni iniziali.                                         ?
011700170327       //--------------------------------------------------------------
011800170327       begsr  sr_RoutInz;
011900170327
012000170629         *inLR = *on;
012100170327
012200170327         // ctrl parametri in ingresso
012300170327         exsr CtrlParmIn;
012400170629
012500170629         //a ogni lancio pulisco il file di arrivo
012600170629         exec sql
012700170629          delete from edpfgdpd/wfcapdpd0f;
012800170327
012900170629         // preparo statement di lettura
013000170629         WSQL = 'select +
013100170629                 tasaas, tasmgs, tasnzd, tascad, taslod, taslnp, taslna +
013200170629                 from titas00f +
013300170629                 where char(tasaas*10000 + tasmgs) between';
013400170629         WSQL = %trim(wSQL) + ' ''' +
013500170629                p_DtSpeD + ''' and ''' + p_DtSpeA + '''';
013600170629         if p_LNA <> '000';
013700170629           WSQL = %trim(wSQL) +
013800170629                ' and ''' + p_lna + ''' = char(taslna) ';
013900170629         endif;
014000170629         WSQL = %trim(wSQL) +
014100170629                ' and tastbl in (''F1'',''A2'') and tasnzd<>'' '' +
014200170630                 and taslna in ( +
014300170630                 select orgfil from azorg00f +
014400170630                 where substr(orgde3, 11, 3) = ''DPD'') +
014500170629                 union all +
014600170629                 select +
014700170629                 tasaas, tasmgs, tasnzd, tascad, taslod, taslnp, taslna +
014800170629                 from titas10f +
014900170629                 where char(tasaas*10000 + tasmgs) between';
015000170629         WSQL = %trim(wSQL) + ' ''' +
015100170629                p_DtSpeD + ''' and ''' + p_DtSpeA + '''';
015200170629         if p_LNA <> '000';
015300170629           WSQL = %trim(wSQL) +
015400170629                ' and ''' + p_lna + ''' = char(taslna) ';
015500170629         endif;
015600170629         WSQL = %trim(wSQL) +
015700170630                ' and tastbl in (''F1'',''A2'') and tasnzd<>'' '' +
015800170630                 and taslna in ( +
015900170630                 select orgfil from azorg00f +
016000170630                 where substr(orgde3, 11, 3) = ''DPD'')';
016100170327
016200170327       endsr;
016300170327
016400170327       //--------------------------------------------------------------
016500170327       // Controllo parametri in ingresso                             ?
016600170327       //--------------------------------------------------------------
016700170327       begsr  CtrlParmIn;
016800170327
016900170327
017000170327       endsr;
017100160831
017200170327       //--------------------------------------------------------------
017300170629       // Scorro il cursore, chiamo i driver e scrivo incongruenze
017400170327       //--------------------------------------------------------------
017500170629       begsr  CurList;
017600160901
017700160831         // Dichiarazione cursore
017800160831         exec sql   prepare S1   from :wSQL;
017900160831         exec sql   declare C1  asensitive   cursor for S1;
018000160831
018100160831         // Apertura del cursore
018200160831         exec sql   open C1;
018300170328
018400170328         // lettura cursore
018500170629         exec sql  fetch next  from C1  into :CAPDPD_fetch;
018600160831
018700170328         // errore SQL
018800170328         if SQLCode < 0;
018900170629           p_ERR = '1';
019000170328           exsr sr_RoutEnd;
019100160831         endif;
019200170328
019300170328         // nessun rcd
019400170328         if SQLCode = 100;
019500170629           p_ERR = '2';
019600170329           exsr sr_RoutEnd;
019700170328         endif;
019800170327
019900170327         dou SQLCode = 100
020000170327          or SQLCode < 0;
020100170328
020200170629           // chiamo driver OLD
020300170629           exsr call_E3;
020400170629
020500170629           // chiamo driver NEW
020600170629           exsr call_D4;
020700170629
020800170629           // confronto
020900170629           exsr comp_E3D4;
021000170328
021100170629           exec sql  fetch next  from C1  into :CAPDPD_fetch;
021200170328
021300170328         enddo;
021400170327
021500170327         // Chiudo il cursore
021600170327         exec sql  close C1;
021700160831
021800160831       ENDSR;
021900170328
022000170328       //--------------------------------------------------------------
022100170629       // Chiamo vecchio driver                                        ?
022200170328       //--------------------------------------------------------------
022300170629       begsr  call_E3;
022400170328
022500170629     C                   CLEAR                   TISIE3DS
022600170629     C                   EVAL      ISIE3TLA  = 'E'
022700170629     C***                EVAL      ISIE3DRI  = %dec(%date() : *iso)
022800170630     C***                EVAL      ISIE3DSP  =    aas * 10000 +    mgs
022900170629     C                   EVAL      ISIE3DRI  = 20170801
023000170630     C                   EVAL      ISIE3DSP  = ISIE3DRI
023100170629     C                   EVAL      ISIE3NZD  =    NZD
023200170629     C                   EVAL      ISIE3CAD  =    CAD
023300170629     C                   EVAL      ISIE3CTY  =    LOD
023400170629     C                   EVAL      ISIE3SRV  = %dec(p_SRVC:3:0)
023500170629     C                   EVAL      ISIE3LNP  =    LNP
023600170629     C                   EVAL      ISIE3EXC  = 'N'
023700170629     C*
023800170629     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD
023900170629     C                   EVAL      ISIE3TLA = 'E'
024000170629     C                   CALL      'TISIE3R'
024100170629     C                   PARM                    TISIE3DS
024200170629
024300170629       ENDSR;
024400170629
024500170629       //--------------------------------------------------------------
024600170629       // Chiamo nuovo driver                                        ?
024700170629       //--------------------------------------------------------------
024800170629       begsr  call_D4;
024900170629
025000170629     C                   CLEAR                   TISID4DS
025100170629     C***                EVAL      ISID4DRI  = %dec(%date() : *iso)
025200170629     C                   EVAL      ISID4DRI  = 20170801
025300170629     C                   EVAL      ISID4LNP  =    LNP
025400170629     C                   EVAL      ISID4NZD  =    NZD
025500170629     C                   EVAL      ISID4CAD  =    CAD
025600170629     C                   EVAL      ISID4LOD  =    LOD
025700170629     C                   EVAL      ISID4SORC = %dec(p_SRVC:3:0)
025800170629     C*
025900170629     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD
026000170629     C                   CALL      'TISID4R'
026100170629     C                   PARM                    TISID4DS
026200170629
026300170629       ENDSR;
026400170629
026500170629       //--------------------------------------------------------------
026600170629       // Confronto risultati vecchio/nuovo driver                                        ?
026700170629       //--------------------------------------------------------------
026800170629       begsr  comp_E3D4;
026900170629
027000170703         *in55 = *off;
027100170703         clear OUTTIPO;
027200170703
027300170703         select;
027400170703           when USIE3CAD  <> USID4CAD;
027500170703                *in55 = *on;
027600170703                OUTTIPO = 'UCAD';
027700170703           when OSIE3RDEP <> %subst(OSID4ODPT:4:4);
027800170703                *in55 = *on;
027900170703                OUTTIPO = 'O-DEPOT';
028000170703           when OSIE3IATAD<> OSID4IATA;
028100170703                *in55 = *on;
028200170703                OUTTIPO = 'IATA';
028300170703           when OSIE3ALWID<> OSID4ALWID;
028400170703                *in55 = *on;
028500170703                OUTTIPO = 'ALLOW ID';
028600170703           when %editc(OSIE3NAZND:'X') <> OSID4DNTWC;
028700170703                *in55 = *on;
028800170703                OUTTIPO = 'D-NTWC';
028900170703           when OSIE3NAZ2D<> OSID4DDPTC;
029000170703                *in55 = *on;
029100170703                OUTTIPO = 'D-COUNTRY';
029200170703           when OSIE3DDEP <> OSID4DSTR;
029300170703                *in55 = *on;
029400170703                OUTTIPO = 'D-DEPOT';
029500170703           when OSIE3OSRT <> OSID4OSORT;
029600170703                *in55 = *on;
029700170703                OUTTIPO = 'O-SORT';
029800170703           when %trim(OSIE3DSRT) <> %trim(OSID4DSORT);
029900170703                *in55 = *on;
030000170703                OUTTIPO = 'D-SORT';
030100170703           when %trim(OSIE3SRVD) <> %trim(OSID4SRVX);
030200170703                *in55 = *on;
030300170703                OUTTIPO = 'SRV-TXT';
030400170703           when OSIE3SRVM <> OSID4SRVM;
030500170703                *in55 = *on;
030600170703                OUTTIPO = 'SRV-MRK';
030700170703           when OSIE3BIDP <> OSID4BCID;
030800170703                *in55 = *on;
030900170703                OUTTIPO = 'BARCODE-ID';
031000170703           when OSIE3ERRL  <> OSID4ERRL;
031100170703                *in55 = *on;
031200170703                OUTTIPO = 'ERROR';
031300170703         endsl;
031400170703         if *in55;
031500170629           // scrivo il file delle differenze
031600170629           exsr wrt_CAPDPD;
031700170629         endif;
031800170629
031900170629       endsr;
032000170629
032100170629       //--------------------------------------------------------------
032200170629       // scrivo differenze
032300170629       //--------------------------------------------------------------
032400170629       begsr  wrt_CAPDPD;
032500170629
032600170629         x_TASLNP      =    LNP     ;
032700170629         x_TASNZD      =    NZD     ;
032800170629         x_TASCAD      =    CAD     ;
032900170629         x_TASLOD      =    LOD     ;
033000170629         x_TASLNA      =    LNA     ;
033100170629         x_USIE3CAD    = USIE3CAD   ;
033200170629         x_OSIE3RDEP   = OSIE3RDEP  ;
033300170629         x_OSIE3IATAD  = OSIE3IATAD ;
033400170629         x_OSIE3ALWID  = OSIE3ALWID ;
033500170629         x_OSIE3NAZND  = OSIE3NAZND ;
033600170629         x_OSIE3NAZ2D  = OSIE3NAZ2D ;
033700170629         x_OSIE3DDEP   = OSIE3DDEP  ;
033800170629         x_OSIE3OSRT   = OSIE3OSRT  ;
033900170629         x_OSIE3DSRT   = OSIE3DSRT  ;
034000170629         x_OSIE3SRVD   = OSIE3SRVD  ;
034100170629         x_OSIE3SRVM   = OSIE3SRVM  ;
034200170629         x_OSIE3BIDP   = OSIE3BIDP  ;
034300170629         x_OSIE3ERRL   = OSIE3ERRL  ;
034400170629         x_USID4CAD    = USID4CAD   ;
034500170629         x_OSID4ODPT   = OSID4ODPT  ;
034600170629         x_OSID4IATA   = OSID4IATA  ;
034700170629         x_OSID4ALWID  = OSID4ALWID ;
034800170629         x_OSID4DNTWC  = OSID4DNTWC ;
034900170629         x_OSID4DDPTC  = OSID4DDPTC ;
035000170629         x_OSID4DSTR   = OSID4DSTR  ;
035100170629         x_OSID4SSORT  = OSID4SSORT ;
035200170629         x_OSID4OSORT  = OSID4OSORT ;
035300170629         x_OSID4DSORT  = OSID4DSORT ;
035400170629         x_OSID4SRVX   = OSID4SRVX  ;
035500170629         x_OSID4SRVM   = OSID4SRVM  ;
035600170629         x_OSID4BCID   = OSID4BCID  ;
035700170629         x_OSID4ERRL   = OSID4ERRL  ;
035800170630         x_SRVC        = %dec(p_SRVC:3:0);
035900170703         x_OUTTIPO     = OUTTIPO;
036000170629
036100170629         write CAPDP000;
036200170328
036300170328       endsr;
036400170629
036500170629       //--------------------------------------------------------------
036600170629       // Operazioni finali.                                           ?
036700170629       //--------------------------------------------------------------
036800170629       begsr  sr_RoutEnd;
036900170629
037000170629         // Uscita dal *pgm?
037100170629         eval *inlr=*on;
037200170629         return;
037300170629
037400170629       endsr;
