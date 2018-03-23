000100170130       //==============================================================
000200170130       //?TNVRSMBRVR - Recupero SPUNTE (FIBAR) da FLTR700F/FITR6ND     ?
000300170130       //==============================================================
000400170130
000500170130       //--------------------------------------------------------------
000600170130       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170130       //--------------------------------------------------------------
000800170130
000900170130     /*PRM  dbgview(*source)
001000170130     /*CMD  ovrdbf file(FITR600F) tofile(FILTRAPRD/FITR600F) +
001100170130     /*CMD         ovrscope(*calllvl)
001200170130     /*CMD  ovrdbf file(FLTR700F) tofile(FILTRAPRD/FLTR700F) +
001300170130     /*CMD         ovrscope(*calllvl)
001400170130     /*END  dltovr file(FITR600F FLTR700F) lvl(*)
001500170130     /*END
001600170130
001700170130       //--------------------------------------------------------------
001800170130       //?Specifiche di controllo.                                     ?
001900170130       //--------------------------------------------------------------
002000170130
002100170130     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200170130     h dftactgrp(*no)
002300170130     h alwnull(*inputonly)
002400170130
002500170130       //--------------------------------------------------------------
002600170130       //?Dichiarazione file.                                          ?
002700170130       //--------------------------------------------------------------
002800170130
002900170130       // -?Organigramma?
003000170130     fAZORG01L  if   e           k disk
003100170130
003200170130       // -?Segnacolli Spuntati da caricare su AS/400?
003300170130     fFIBARND   O    e             disk
003400170130     f                                     extdesc('FILTRAPRD/FIBAR00F')
003500170130
003600170130       //--------------------------------------------------------------
003700170130       //?Definizione costanti.                                        ?
003800170130       //--------------------------------------------------------------
003900170130
004000170130       // -?Costante per controllo "caratteri solo numerici"?
004100170130     d c_Digits        c                   const('0123456789')
004200170130
004300170130       //--------------------------------------------------------------
004400170130       //?Definizione schiere.                                         ?
004500170130       //--------------------------------------------------------------
004600170130
004700170130
004800170130       //--------------------------------------------------------------
004900170130       //?Definizione aree dati.                                       ?
005000170130       //--------------------------------------------------------------
005100170130
005200170130
005300170130       //--------------------------------------------------------------
005400170130       //?Definizione strutture dati.                                  ?
005500170130       //--------------------------------------------------------------
005600170130
005700170130       // -?Campi estratti via SQL?
005800170130     d FITR6_ds      e ds                  inz    extname(FITR600F)
005900170130     d                                     qualified
006000170130     d FLTR7_ds      e ds                  inz    extname(FLTR700F)
006100170130     d                                     qualified
006200170130
006300170130       // -?150ª descrizione dell'organigramma?
006400170130     d Og150         e ds                  qualified  inz
006500170130
006600170130       // -?Segnacollo?
006700170130     d wBarCode        ds                  inz    qualified
006800170130     d   wBar_xx1              1      5    inz
006900170130     d   wBar_xx2              6      6    inz
007000170130     d   wBar_LNP              7      9    inz
007100170130     d   wBar_LNA             10     12    inz
007200170130     d   wBar_NRS             13     14    inz
007300170130     d   wBar_NSC             15     21    inz
007400170130     d   wBar_ZNC             22     23    inz
007500170130     d     wBar_BAR            7     23    inz
007600170130     d   wBar_CHK             24     24    inz
007700170130     d     wBar_BarCo          7     24    inz
007800170130     d   wBar_xx3             25     25    inz
007900170130
008000170130       //--------------------------------------------------------------
008100170130       //?Definizione variabili globali.                               ?
008200170130       //--------------------------------------------------------------
008300170130
008400170130       // -?Stringa SQL da eseguire?
008500170130     d wSql            s          32740    inz    varying
008600170130
008700170130       // -?Flags booleani?
008800170130     d $Fine           s               n   inz
008900170130
009000170130       //--------------------------------------------------------------
009100170130       //?Definizione prototipi procedure usate.                       ?
009200170130       //--------------------------------------------------------------
009300170130
009400170130       // -?API QCAPCMD (Process Commands)?
009500170130     d Qcmd            s           2048    inz   varying
009600170130      /copy qSysInc/qRpgleSrc,QCAPCMD
009700170130      /copy gaitrasrc/srcProtoPR,QCAPCMD
009800170130       // -?Parametri gestione errori API.?
009900170130      /copy qsysinc/qRpgleSrc,QUSEC
010000170130
010100170130       //--------------------------------------------------------------
010200170130       //?Definizione key-list.                                        ?
010300170130       //--------------------------------------------------------------
010400170130
010500170130
010600170130       //--------------------------------------------------------------
010700170130       //?Riepilogo indicatori utilizzati.                             ?
010800170130       //--------------------------------------------------------------
010900170130
011000170130
011100170130       //--------------------------------------------------------------
011200170130       //?M A I N - L I N E                                            ?
011300170130       //--------------------------------------------------------------
011400170130
011500170130      /free
011600170130
011700170130       // -?Operazioni iniziali?
011800170130       exsr  sr_RoutInz;
011900170130
012000170130       // -?Preparazione stringa SQL?
012100170130       exsr  sr_PrepSQL;
012200170130
012300170130       // -?Apertura del cursore?
012400170130       exsr  sr_OpenCursor;
012500170130
012600170130       // -?Lettura del file?
012700170130       DoW  Not $Fine;
012800170130         exsr  sr_ReadCursor;
012900170130       EndDo;
013000170130
013100170130       // -?Chiusura del cursore?
013200170130       exsr  sr_CloseCursor;
013300170130
013400170130       // -?Operazioni finali?
013500170130       exsr  sr_RoutEnd;
013600170130
013700170130       //--------------------------------------------------------------
013800170130       //?Operazioni iniziali.                                         ?
013900170130       //--------------------------------------------------------------
014000170130       BEGSR  sr_RoutInz;
014100170130
014200170130         // -?Impostazione opzioni per SQL?
014300170130         exec sql   set option   DynUsrPrf = *Owner,
014400170130                                 CloSqlCsr = *EndMod;
014500170130
014600170130         // -?Impostazione chiusura?
014700170130         *inLR = *on;
014800170130
014900170130         // -?Reperimento dati anagrafici della Filiale?
015000170130         chain  ( 172 )  AZORG;
015100170130         if  %found(AZORG01L);
015200170130           Og150 = ORGdf0;
015300170130         endif;
015400170130         // -?Se filiale NON abilitata all'autogenerazione delle spunte?
015500170130         //  ?uscita => Fine?
015600170130         if  Og150.§OGagSpUs <> 'S';
015700170130           exsr  sr_RoutEnd;
015800170130         endif;
015900170130
016000170130       ENDSR;
016100170130
016200170130       //--------------------------------------------------------------
016300170130       //?Preparazione stringa SQL per estrazione dati da FLTR700F.    ?
016400170130       //--------------------------------------------------------------
016500170130       BEGSR  sr_PrepSQL;
016600170130
016700170130         wSQL =  'with FLTR7 as ( select * +
016800170130                                    from FILTRA201/FLTR700F +
016900170130                                   where TR7020 = ''36'' +
017000170130                                     and TR7ctl = ''2'' +
017100170130                                     and TR7fil = ''172'' +
017200170130                                     and TR7crc = '' '' +
017300170130                                     and TR7bar > ''00000000000000000'' +
017400170130                                ) +
017500170130
017600170130                  select * +
017700170130
017800170130                    from EDPND/FITR6ND inner join FLTR7 +
017900170130                      on substr(TR6who, 1, 15) = substr(TR7bar, 1, 15) +
018000170130
018100170130                   order by TR7fil, substr(TR7tim, 1, 12), TR7pep +
018200170130
018300170130                     for fetch only';
018400170130
018500170130       ENDSR;
018600170130
018700170130       //--------------------------------------------------------------
018800170130       //?Apertura cursore.                                            ?
018900170130       //--------------------------------------------------------------
019000170130       BEGSR  sr_OpenCursor;
019100170130
019200170130         // -?Dichiarazione cursore?
019300170130         exec sql   prepare S1   from :wSQL;
019400170130         exec sql   declare C1   cursor for S1;
019500170130
019600170130         if  SQLcode < *zero;
019700170130           $Fine = *on;
019800170130           exsr  sr_PrintERR;
019900170130         endif;
020000170130
020100170130         // -?Apertura del cursore?
020200170130         exec sql   open C1;
020300170130
020400170130         if  SQLcode < *zero;
020500170130           $Fine = *on;
020600170130           exsr  sr_PrintERR;
020700170130         endif;
020800170130
020900170130       ENDSR;
021000170130
021100170130       //--------------------------------------------------------------
021200170130       //?Lettura cursore.                                             ?
021300170130       //--------------------------------------------------------------
021400170130       BEGSR  sr_ReadCursor;
021500170130
021600170130         $Fine = *off;
021700170130         clear  FLTR7_ds;
021800170130
021900170130         exec sql   fetch next   from C1   into :FITR6_ds, :FLTR7_ds;
022000170130
022100170130         select;
022200170130           when  SQLcode = 100;
022300170130             $Fine = *on;
022400170130           when  SQLcode < *zero;
022500170130             $Fine = *on;
022600170130             exsr  sr_PrintERR;
022700170130           other;
022800170130             exsr  sr_Write_FIBAR;
022900170130         endsl;
023000170130
023100170130       ENDSR;
023200170130
023300170130       //--------------------------------------------------------------
023400170130       //?Chiusura cursore.                                            ?
023500170130       //--------------------------------------------------------------
023600170130       BEGSR  sr_CloseCursor;
023700170130
023800170130         // -?Chiusura del cursore?
023900170130         exec sql   close C1;
024000170130
024100170130       ENDSR;
024200170130
024300170130       //--------------------------------------------------------------
024400170130       //?Scrittura del record nel *file FIBAR00F.                     ?
024500170130       //--------------------------------------------------------------
024600170130       BEGSR  sr_Write_FIBAR;
024700170130
024800170130         // -?Se distinta messaggerie non creo spunta.?
024900170130         //  ?Se la postazione non è IMA creo la spunta con il FV del?
025000170130         //     ?file colli.?
025100170130         if  FITR6_ds.TR6npg =  3  or
025200170130             FITR6_ds.TR6fl3 = 'M';
025300170130           leavesr;
025400170130         endif;
025500170130         // -?Se FV in FITR6 è *zero: NON è da scrivere alcun rec.?
025600170130         if  FITR6_ds.TR6nfv = *zero;
025700170130           leavesr;
025800170130         endif;
025900170130
026000170130         clear  BARCO;
026100170130
026200170130         clear  wBarCode;
026300170130         //*·eval(R) wBarCode    = FLTR7_ds.TR7pd1;
026400170130         wBarCode.wBar_Bar = FLTR7_ds.TR7bar;
026500170130         wBarCode.wBAR_Chk = %subst( FLTR7_ds.TR7pd1 :
026600170130                                     %len( FLTR7_ds.TR7pd1 ) - 1 :
026700170130                                     1 );
026800170130
026900170130         if  %check( c_Digits : wBarCode.wBAR_Bar )  = *zero;
027000170130           barLNP = %int( wBarCode.wBAR_lnp );
027100170130           barLNA = %int( wBarCode.wBAR_lna );
027200170130           barNRS = %int( wBarCode.wBAR_nrs );
027300170130           barNSC = %int( wBarCode.wBAR_nsc );
027400170130           barZNC = %int( wBarCode.wBAR_znc );
027500170130         endif;
027600170130
027700170130         if  %check( c_Digits : wBarCode.wBAR_BarCo )  = *zero;
027800170130           barCTR = %int( wBarCode.wBAR_chk );
027900170130         endif;
028000170130
028100170130         //*·if  FLTR7_ds.TR7nfv                      >  *zero  and
028200170130         //*·    %subst( FLTR7_ds.TR7nfv : 5 : 1 )    >= *zero  and
028300170130         //*·    %check( c_Digits : FLTR7_ds.TR7nfv )  = *zero;
028400170130         //*·  barNFV = %int( FLTR7_ds.TR7nfv );
028500170130         //*·endif;
028600170130         barNFV = FITR6_ds.TR6nfv;
028700170130
028800170130         //*·barNPS = 49;
028900170130         barNPS  = 50;
029000170130
029100170130         //*·if  FLTR7_ds.TR7ctl > *zero;
029200170130         //*·  barNPG = %int( FLTR7_ds.TR7ctl );
029300170130         //*·endif;
029400170130         barNPG = FITR6_ds.TR6npg;
029500170130
029600170130         //*·clear  barCAN;
029700170130         //*·clear  barVUC;
029800170130         //*·clear  barPUC;
029900170130
030000170130         if  %check( c_Digits : FLTR7_ds.TR7tim ) =  *zero;
030100170130           barDFS = %int( %subst( FLTR7_ds.TR7tim : 1 : 8 ) );
030200170130           barHMS = %int( %subst( FLTR7_ds.TR7tim : 9 : 6 ) );
030300170130         endif;
030400170130
030500170130         barPRU = 'VDL172';
030600170130
030700170130         if  %check( c_Digits : FLTR7_ds.TR7tim ) =  *zero;
030800170130           barMIS = %int( %subst( FLTR7_ds.TR7mil : 5 : 3 ) );
030900170130         endif;
031000170130
031100170130         //___________
031200170130         write  BARCO;
031300170130         //¯¯¯¯¯¯¯¯¯¯¯
031400170130
031500170130       ENDSR;
031600170130
031700170130       //--------------------------------------------------------------
031800170130       //?Stampa segnalazione dell'errore rilevato via SQL             ?
031900170130       //--------------------------------------------------------------
032000170130       BEGSR  sr_PrintERR;
032100170130
032200170130         // -?Stampa del Dump?
032300170130         Dump(A);
032400170130
032500170130         // -?Stampa del Job-Log?
032600170130         Qcmd = 'DSPJOBLOG job(*) output(*print)';
032700170130         exsr  sr_ExecCmd;
032800170130
032900170130         // -?Chiusura del programma?
033000170130         exsr  sr_RoutEnd;
033100170130
033200170130       ENDSR;
033300170130
033400170130       //--------------------------------------------------------------
033500170130       //?Esecuzione del comando (già impostato).                      ?
033600170130       //--------------------------------------------------------------
033700170130       BEGSR  sr_ExecCmd;
033800170130
033900170130         clear Qcap0100;
034000170130         Qcabcsdh = *off;
034100170130         Qcapa    = *off;
034200170130         Qcacmdss = *off;
034300170130         Qcaerved = *allX'00';
034400170130
034500170130         clear Qusec;
034600170130         Qusbprv  = %size(Qusec);
034700170130
034800170130         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
034900170130                           %size(Qcap0100) : 'CPOP0100' : *omit :
035000170130                           0 : 0 : Qusec);
035100170130
035200170130         //*·if  Qusei <> *blank;
035300170130         //*·  ...;
035400170130         //*·endif;
035500170130
035600170130       ENDSR;
035700170130
035800170130       //--------------------------------------------------------------
035900170130       //?Operazioni finali.                                           ?
036000170130       //--------------------------------------------------------------
036100170130       BEGSR  sr_RoutEnd;
036200170130
036300170130         // -?Uscita dal *pgm?
036400170130         return;
036500170130
036600170130       ENDSR;
036700170130
036800170130      /end-free
