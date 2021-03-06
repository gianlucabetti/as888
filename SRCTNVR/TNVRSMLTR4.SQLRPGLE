000100161021       //==============================================================
000200161021       //?TNVRSMITR4 * RI-generazione FLTR400F da FLTR700F.            ?
000300161021       //==============================================================
000400161021
000500161021       //--------------------------------------------------------------
000600161021       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700161021       //--------------------------------------------------------------
000800161021
000900161021     /*PRM  dbgview(*source)
001000161021     /*END
001100161021
001200161021       //--------------------------------------------------------------
001300161021       //?Specifiche di controllo.                                     ?
001400161021       //--------------------------------------------------------------
001500161021
001600161021     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700161021     h dftactgrp(*no)
001800161021     h alwnull(*inputonly)
001900161021
002000161021       //--------------------------------------------------------------
002100161021       //?Dichiarazione file.                                          ?
002200161021       //--------------------------------------------------------------
002300161021
002400161021       // -?Spunte da VanDerLande - senza tracciato?
002500161021     fFLTR400F  o    e             disk
002600161021
002700161021       //--------------------------------------------------------------
002800161021       //?Definizione costanti.                                        ?
002900161021       //--------------------------------------------------------------
003000161021
003100161024       // -?Costante per controllo "caratteri solo numerici"?
003200161024     d c_Digits        c                   const('0123456789')
003300161021
003400161021       //--------------------------------------------------------------
003500161021       //?Definizione schiere.                                         ?
003600161021       //--------------------------------------------------------------
003700161021
003800161021
003900161021       //--------------------------------------------------------------
004000161021       //?Definizione aree dati.                                       ?
004100161021       //--------------------------------------------------------------
004200161021
004300161021
004400161021       //--------------------------------------------------------------
004500161021       //?Definizione strutture dati.                                  ?
004600161021       //--------------------------------------------------------------
004700161021
004800161021       // -?Parametri Ricevuti?
004900161021     d KPJBA         e ds
005000161021     d tnvrLTR4ds      ds                  inz    qualified
005100161021     d   prm_Fil                      3s 0 inz
005200161021     d   prm_Time_Ini                14s 0 inz
005300161021     d   prm_Time_End                14s 0 inz
005400161108     d*//  prm_Date_End...
005500161108     d*//                     18     25s 0 inz
005600161021
005700161021       // -?Status ds?
005800161021     d Status         sds
005900161021     d   SDSpgm          *proc
006000161021     d*//SDSprm          *parms
006100161021     d*//SDSdta              191    198
006200161021     d   SDSjob              244    253
006300161021     d   SDSusr              254    263
006400161021
006500161021       // -?Campi Formato Record FLTR400F?
006600161021     d FLTR4_ds        ds                  inz
006700161021     d   TR4ctl                1      1
006800161021     d   TR4npl                2      3
006900161021     d   TR4nfv                4      8
007000161021     d   TR4tim                9     22
007100161021     d   TR4ms1               23     27
007200161021     d   TR4msx               28     28
007300161021     d   TR4ms2               29     31
007400161021     d   TR4cod               32     36
007500161021     d   TR4020               37     38
007600161021     d   TR4f01               39     39
007700161021     d   TR4pic               40     44
007800161021     d   TR4f02               45     45
007900161021     d   TR4pgr               46     51
008000161021     d   TR4f03               52     52
008100161021     d   TR4pod               53     55
008200161021     d   TR4f04               56     56
008300161021     d   TR4ods               57     57
008400161021     d   TR4f05               58     58
008500161021     d   TR4pad               59     61
008600161021     d   TR4f06               62     62
008700161021     d   TR4dts               63     63
008800161021     d   TR4f07               64     64
008900161021     d   TR4ps1               65     65
009000161021     d   TR4pn1               66     67
009100161021     d   TR4pd1               68     97
009200161021     d   TR4f08               98     98
009300161021     d   TR4ps2               99     99
009400161021     d   TR4pn2              100    101
009500161021     d   TR4pd2              102    130
009600161021     d***???                 131    131
009700161021     d   TR4f09              132    132
009800161021     d   TR4ps3              133    133
009900161021     d   TR4pn3              134    135
010000161021     d   TR4pd3              136    165
010100161021     d   TR4f10              166    166
010200161021     d   TR4ps4              167    167
010300161021     d   TR4pn4              168    169
010400161021     d   TR4pd4              170    199
010500161021     d       TR4dm1          181    184
010600161021     d       TR4dm2          185    188
010700161021     d       TR4dm3          189    192
010800161021     d     TR4dm             181    192
010900161021     d   TR4f11              200    200
011000161021     d   TR4pep              201    214
011100161021     d   TR4f12              215    215
011200161021     d   TR4pes              216    216
011300161021     d   TR4f13              217    217
011400161021     d   TR4pup              218    231
011500161021     d   TR4f14              232    232
011600161021     d   TR4pus              233    233
011700161021     d   TR4f15              234    234
011800161021     d   TR4crc              235    236
011900161021
012000161021       // -?Campi Formato Record FLTR700F?
012100161021     d FLTR7_ds      e ds                  inz   extname(FLTR700F)
012200161021
012300161021       //--------------------------------------------------------------
012400161021       //?Definizione variabili globali.                               ?
012500161021       //--------------------------------------------------------------
012600161021
012700161021       // -?Flags booleani?
012800161021     d $Fine           s               n   inz
012900161021     d $First          s               n   inz
013000161021
013100161021       // -?Indici di schiera?
013200161021     d xx              s              3  0 inz
013300161021
013400161021       // -?Stringhe SQL da eseguire?
013500161021     d wSql            s          32740    inz   varying
013600161021
013700161021       //--------------------------------------------------------------
013800161021       //?Definizione prototipi procedure usate.                       ?
013900161021       //--------------------------------------------------------------
014000161021
014100161021       // -?Caricamento �x?
014200161024     d*// ds_�1           ds                  inz
014300161024     d*//   sk_�1                        3s 0 dim(30)  inz
014400161024     d*// TRUL06ds      e ds                  inz
014500161024      *///copy gaitrasrc/srcProtoPR,TRUL06R
014600161021
014700161021       // -?API QCAPCMD (Process Commands)?
014800161021     d Qcmd            s           2048    inz  varying
014900161021      /copy qSysInc/qRpgleSrc,QCAPCMD
015000161021      /copy gaitrasrc/srcProtoPR,QCAPCMD
015100161021       // -?Parametri gestione errori API.?
015200161021      /copy qsysinc/qRpgleSrc,QUSEC
015300161021
015400161021       //--------------------------------------------------------------
015500161021       //?Definizione key-list.                                        ?
015600161021       //--------------------------------------------------------------
015700161021
015800161021
015900161021       //--------------------------------------------------------------
016000161021       //?Riepilogo indicatori utilizzati.                             ?
016100161021       //--------------------------------------------------------------
016200161021
016300161021
016400161021       //--------------------------------------------------------------
016500161021       //?M A I N - L I N E                                            ?
016600161021       //--------------------------------------------------------------
016700161021
016800161021     c     *Entry        plist
016900161021     c                   parm                    KPJBA
017000161021
017100161021      /free
017200161021
017300161021       // -?Operazioni iniziali?
017400161021       exsr  sr_RoutInz;
017500161021
017600161021       // -?Preparazione stringa SQL?
017700161021       exsr  sr_PrepSQL;
017800161021
017900161021       // -?Apertura del cursore?
018000161021       exsr  sr_OpenCursor;
018100161021
018200161021       // -?Lettura del file?
018300161021       DoW  Not $Fine;
018400161021         exsr  sr_ReadCursor;
018500161021       EndDo;
018600161021
018700161021       // -?Chiusura del cursore?
018800161021       exsr  sr_CloseCursor;
018900161021
019000161021       // -?Operazioni finali?
019100161021       exsr  sr_RoutEnd;
019200161021
019300161021       //--------------------------------------------------------------
019400161021       //?Operazioni iniziali.                                         ?
019500161021       //--------------------------------------------------------------
019600161021       BEGSR  sr_RoutInz;
019700161021
019800161021         // -?Impostazione opzioni per SQL?
019900161021         exec sql   set  option  DynUsrPrf = *Owner,
020000161021                                 CloSqlCsr = *EndMod;
020100161021
020200161021         // -?Impostazione chiusura?
020300161021         *inLR = *on;
020400161021
020500161021         // -?Ricezione Parametri?
020600161021         tnvrLTR4ds = KPJBU;
020700161024         if  %check( c_Digits : %subst( KPJBU : 1 :
020800161024                                %size(tnvrLTR4ds.prm_Fil) ) )      > *zero;
020900161024           clear  tnvrLTR4ds.prm_Fil;
021000161024         endif;
021100161024         if  %check( c_Digits : %subst( KPJBU : 1 +
021200161024                                %size(tnvrLTR4ds.prm_Fil) :
021300161024                                %size(tnvrLTR4ds.prm_Time_Ini) ) ) > *zero;
021400161024           clear  tnvrLTR4ds.prm_Time_Ini;
021500161024         endif;
021600161024         if  %check( c_Digits : %subst( KPJBU : 1 +
021700161024                                %size(tnvrLTR4ds.prm_Fil) +
021800161024                                %size(tnvrLTR4ds.prm_Time_Ini ) :
021900161024                                %size(tnvrLTR4ds.prm_Time_End) ) ) > *zero;
022000161024           clear  tnvrLTR4ds.prm_Time_End;
022100161024         endif;
022200161021
022300161024         //*�// -?Reperimento Filiali dell'Area ("�1")?
022400161024         //*�clear  sk_�1;
022500161024         //*�clear  KPJBU;
022600161024         //*�clear  TRUL06ds;
022700161024         //*�if  tnvrLTR4ds.prm_Fil > *zero;
022800161024         //*�  D06cod = '�1';
022900161024         //*�  D06key = %editc( tnvrLTR4ds.prm_Fil : 'X' ) + '    S';
023000161024         //*�  if  tnvrLTR4ds.prm_Date_End > *zero;
023100161024         //*�    D06drf = tnvrLTR4ds.prm_Date_End;
023200161024         //*�  else;
023300161024         //*�    D06drf = %int( %subst( %char( %dec( %timestamp() ) )
023400161024         //*�                           : 1 : 8 ) );
023500161024         //*�  endif;
023600161024         //*�  KPJBU  = TRUL06ds;
023700161024         //*�  trul06r ( KPJBA );
023800161024         //*�  TRUL06ds = KPJBU;
023900161024         //*�  ds_�1 = D06lia;
024000161024         //*�endif;
024100161021
024200161021       ENDSR;
024300161021
024400161021       //--------------------------------------------------------------
024500161021       //?Preparazione stringa SQL per estrazione dati da FLTR700F.    ?
024600161021       //--------------------------------------------------------------
024700161021       BEGSR  sr_PrepSQL;
024800161021
024900161021         wSQL =  'select * +
025000161024                    from FLTR700F';
025100161024
025200161024
025300161024         If  tnvrLTR4ds.prm_Fil > *zero;
025400161024
025500161024           //*�$First  = *on;
025600161024           //*�wSQL += ' +
025700161024           //*�        where TR7fil in ( ';
025800161024           //*�
025900161024           //*�For  xx = 1  To %elem( sk_�1 );
026000161024           //*�
026100161024           //*�  if  sk_�1(xx) > *zero;
026200161024           //*�    if  NOT $First;
026300161024           //*�      wSQL += ', ';
026400161024           //*�    endif;
026500161024           //*�    wSQL   += '''' + %editc( sk_�1(xx) : 'X' ) + '''';
026600161024           //*�    $First  = *off;
026700161024           //*�  else;
026800161024           //*�    leave;
026900161024           //*�  endif;
027000161024           //*�
027100161024           //*�EndFor;
027200161024           //*�
027300161024           //*�wSQL +=       ' )';
027400161024
027500161024           wSQL += ' +
027600161024                   where TR7fil = ' + %char( tnvrLTR4ds.prm_Fil );
027700161024
027800161024         EndIf;
027900161021
028000161024         if  tnvrLTR4ds.prm_Time_Ini > *zero  or
028100161024             tnvrLTR4ds.prm_Time_End > *zero;
028200161024           if  tnvrLTR4ds.prm_Fil <= *zero;
028300161024             wSQL +=   ' where';
028400161024           else;
028500161024             wSQL +=   ' and';
028600161024           endif;
028700161024           wSQL +=     ' TR7tim between ' +
028800161021                                %char( tnvrLTR4ds.prm_Time_Ini ) +
028900161021                                  ' and ' +
029000161021                                %char( tnvrLTR4ds.prm_Time_End );
029100161021         endif;
029200161021
029300161021         wSQL += ' order by TR7fil, TR7tim, TR7ctl, TR7020 +
029400161021                     for fetch only';
029500161021
029600161021       ENDSR;
029700161021
029800161021       //--------------------------------------------------------------
029900161021       //?Apertura cursore.                                            ?
030000161021       //--------------------------------------------------------------
030100161021       BEGSR  sr_OpenCursor;
030200161021
030300161021         // -?Dichiarazione cursore?
030400161021         exec sql   prepare S1   from :wSQL;
030500161021         exec sql   declare C1   cursor for S1;
030600161021
030700161021         if  SQLcode < *zero;
030800161021           $Fine = *on;
030900161021           exsr  sr_PrintERR;
031000161021         endif;
031100161021
031200161021         // -?Apertura del cursore?
031300161021         exec sql   open C1;
031400161021
031500161021         if  SQLcode < *zero;
031600161021           $Fine = *on;
031700161021           exsr  sr_PrintERR;
031800161021         endif;
031900161021
032000161021       ENDSR;
032100161021
032200161021       //--------------------------------------------------------------
032300161021       //?Lettura cursore.                                             ?
032400161021       //--------------------------------------------------------------
032500161021       BEGSR  sr_ReadCursor;
032600161021
032700161021         $Fine = *off;
032800161021         clear  FLTR7_ds;
032900161021
033000161021         exec sql   fetch next   from C1   into :FLTR7_ds;
033100161021
033200161021         select;
033300161021           when  SQLcode = 100;
033400161021             $Fine = *on;
033500161021           when  SQLcode < *zero;
033600161021             $Fine = *on;
033700161021             exsr  sr_PrintERR;
033800161021           other;
033900161021             exsr  sr_Write_FLTR4;
034000161021         endsl;
034100161021
034200161021       ENDSR;
034300161021
034400161021       //--------------------------------------------------------------
034500161021       //?Chiusura cursore.                                            ?
034600161021       //--------------------------------------------------------------
034700161021       BEGSR  sr_CloseCursor;
034800161021
034900161021         // -?Chiusura del cursore?
035000161021         exec sql   close C1;
035100161021
035200161021       ENDSR;
035300161021
035400161021       //--------------------------------------------------------------
035500161021       //?Caricamento dati nel record di FLTR4.                        ?
035600161021       //--------------------------------------------------------------
035700161021       BEGSR  sr_Write_FLTR4;
035800161021
035900161021         // - Raggruppa i dati del record nei campi della DS
036000161021         clear  FLTR4_ds;
036100161021
036200161021         TR4ctl  = TR7ctl;
036300161021         TR4npl  = TR7npl;
036400161021         TR4nfv  = TR7nfv;
036500161021         TR4tim  = TR7tim;
036600161021         TR4ms1  = %subst( TR7mil : 1 : %len( TR4ms1 )  );
036700161021         //*�TR4msx  = '-';
036800161021         TR4ms2  = %subst( TR7mil : %len( TR4ms1 ) + 1 : %len( TR4ms2 ) );
036900161021         TR4cod  = TR7cod;
037000161021         TR4020  = TR7020;
037100161021         TR4pic  = TR7pic;
037200161021         TR4pgr  = TR7pgr;
037300161021         TR4pod  = TR7pod;
037400161021         TR4ods  = TR7ods;
037500161021         TR4pad  = TR7pad;
037600161021         TR4dts  = TR7dts;
037700161021         TR4ps1  = TR7ps1;
037800161021         TR4pn1  = TR7pn1;
037900161021         TR4pd1  = TR7pd1;
038000161021         TR4ps2  = TR7ps2;
038100161021         TR4pn2  = TR7pn2;
038200161021         //*�TR4pd2  = TR7pd2
038300161021         evalR  TR4pd2 = TR7pd2;
038400161021         TR4ps3  = TR7ps3;
038500161021         TR4pn3  = TR7pn3;
038600161021         TR4pd3  = TR7pd3;
038700161021         TR4ps4  = TR7ps4;
038800161021         TR4pn4  = TR7pn4;
038900161021         TR4pd4  = TR7pd4;
039000161021         TR4pep  = TR7pep;
039100161021         TR4pes  = TR7pes;
039200161021         TR4pup  = TR7pup;
039300161021         TR4pus  = TR7pus;
039400161021         TR4crc  = TR7crc;
039500161021
039600161021         TR4da1  = FLTR4_ds;
039700161021
039800161021         //___________
039900161021         write  FLTR4;
040000161021         //�����������
040100161021
040200161021       ENDSR;
040300161021
040400161021       //--------------------------------------------------------------
040500161021       //?Stampa segnalazione dell'errore rilevato via SQL             ?
040600161021       //--------------------------------------------------------------
040700161021       BEGSR  sr_PrintERR;
040800161021
040900161021         // -?Stampa del Dump?
041000161021         Dump(A);
041100161021
041200161021         // -?Stampa del Job-Log?
041300161021         Qcmd = 'DSPJOBLOG job(*) output(*print)';
041400161021         exsr  sr_ExecCmd;
041500161021
041600161021         // -?Chiusura del programma?
041700161021         exsr  sr_RoutEnd;
041800161021
041900161021       ENDSR;
042000161021
042100161021       //--------------------------------------------------------------
042200161021       //?Esecuzione del comando (gi� impostato).                      ?
042300161021       //--------------------------------------------------------------
042400161021       BEGSR  sr_ExecCmd;
042500161021
042600161021         clear Qcap0100;
042700161021         Qcabcsdh = *off;
042800161021         Qcapa    = *off;
042900161021         Qcacmdss = *off;
043000161021         Qcaerved = *allX'00';
043100161021
043200161021         clear Qusec;
043300161021         Qusbprv  = %size(Qusec);
043400161021
043500161021         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
043600161021                           %size(Qcap0100) : 'CPOP0100' : *omit :
043700161021                           0 : 0 : Qusec);
043800161021
043900161021         //if  Qusei <> *blank;
044000161021         //  ...;
044100161021         //endif;
044200161021
044300161021       ENDSR;
044400161021
044500161021       //--------------------------------------------------------------
044600161021       //?Operazioni finali.                                           ?
044700161021       //--------------------------------------------------------------
044800161021       BEGSR  sr_RoutEnd;
044900161021
045000161021         // -?Uscita dal *pgm?
045100161021         return;
045200161021
045300161021       ENDSR;
045400161021
045500161021      /end-free
