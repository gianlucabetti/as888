000100170713       //==============================================================
000200170713       //?TNSF26R - Manutenzione Straordinaria CLIENTI Split Payment   ?
000300170713       //==============================================================
000400170713
000500170713       //--------------------------------------------------------------
000600170713       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170713       //--------------------------------------------------------------
000800170713
000900170713     /*PRM  dbgview(*source)
001000170713     /*END
001100170713
001200170713       //--------------------------------------------------------------
001300170713       //?Specifiche di controllo.                                     ?
001400170713       //--------------------------------------------------------------
001500170713
001600170405     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170713     h dftactgrp(*no)
001800170713     h alwnull(*inputonly)
001900170405
002000170405      //---------------------------------------------------------------
002100170405      //?Dichiarazione file.
002200170405      //---------------------------------------------------------------
002300170405
002400170713       // -?BAR_ Anagrafica clienti x gestionale?
002500170713     fYBACL01L  Uf   e           k disk
002600170406
002700170713       // -?Anagrafica Clienti Split Payment con Distinta?
002800170713     fANSPIKCF  O    e             disk
002900170405
003000170713       //--------------------------------------------------------------
003100170713       //?Definizione costanti.                                        ?
003200170713       //--------------------------------------------------------------
003300170713
003400170713       // -?Società BRT?
003500170713     d c_Soc_BRT       c                   const('201')
003600170405
003700170713       //--------------------------------------------------------------
003800170713       //?Definizione schiere.                                         ?
003900170713       //--------------------------------------------------------------
004000170405
004100170713
004200170713       //--------------------------------------------------------------
004300170713       //?Definizione aree dati.                                       ?
004400170713       //--------------------------------------------------------------
004500170713
004600170713       // -?Dati utente?
004700170405     d §AzUte        e ds                  extname(AZUTE00F)
004800170405     d                                     dtaara
004900170405     d §DatiUte      e ds                  extname(dDatiUte)
005000170405     d                                     dtaara
005100170405
005200170713       //--------------------------------------------------------------
005300170713       //?Definizione strutture dati.                                  ?
005400170713       //--------------------------------------------------------------
005500170405
005600170713       // -?Parametri ricevuti?
005700170405     d KPJBA         e ds
005800170713
005900170713       // -?Dati del *file CNIND00F?
006000170713     d CNIND00F      e ds                  based(nullPtr)
006100170713     d                                     qualified
006200170713       // -?Dati estratti via SQL?
006300170713     d wSQL_ds         ds                  inz   qualified
006400170713     d   INDkcc                            inz   like( CNIND00F.INDkcc )
006500170713     d   INDksc                            inz   like( CNIND00F.INDksc )
006600170713     d   INDiva                            inz   like( CNIND00F.INDiva )
006700170713     d   INDcdf                            inz   like( CNIND00F.INDcdf )
006800170405
006900170713       //--------------------------------------------------------------
007000170713       //?Definizione variabili globali.                               ?
007100170713       //--------------------------------------------------------------
007200170713
007300170713       // -?Flags booleani?
007400170713     d $Fine           s               n   inz(*off)
007500170405
007600170713       //--------------------------------------------------------------
007700170713       //?Definizione procedure usate.                                 ?
007800170713       //--------------------------------------------------------------
007900170713
008000170713       // -?Reperimento dati utente?
008100170713     d TIBS34ds      e ds
008200170713      /copy gaitrasrc/srcProtoPR,TIBS34R
008300170405
008400170713       // -?API QCAPCMD (Process Commands)?
008500170713     d Qcmd            s           2048    inz   varying
008600170713      /copy qSysInc/qRpgleSrc,QCAPCMD
008700170713      /copy gaitrasrc/srcProtoPR,QCAPCMD
008800170713       // -?Parametri gestione errori API.?
008900170713      /copy qsysinc/qRpgleSrc,QUSEC
009000170713
009100170713       //--------------------------------------------------------------
009200170713       //?Definizione prototipi.                                       ?
009300170713       //--------------------------------------------------------------
009400170405
009500170713
009600170713       //--------------------------------------------------------------
009700170713       //?Definizione key-list.                                        ?
009800170713       //--------------------------------------------------------------
009900170713
010000170713       // -?File YBACL01L?
010100170713     d keyYBACL01    e ds                  extname( YBACL01L : *key )
010200170713     d                                     prefix( k_ )  inz
010300170405
010400170713       //--------------------------------------------------------------
010500170713       //?M A I N - L I N E                                            ?
010600170713       //--------------------------------------------------------------
010700170713
010800170405     c     *Entry        plist
010900170405     c                   parm                    kpjba
011000170405
011100170405      /free
011200170405
011300170713       // -?Operazioni iniziali?
011400170713       exsr  sr_RoutInz;
011500170405
011600170713       // -?Ciclo di lettura ANSPI00F per aggiornamento YBACL00F?
011700170713       exsr  sr_ElabANSPI_UpdYBACL;
011800170405
011900170713       // -?Operazioni finali?
012000170713       exsr  sr_RoutEnd;
012100170405
012200170405       //--------------------------------------------------------------
012300170713       //?Operazioni iniziali.                                         ?
012400170405       //--------------------------------------------------------------
012500170713       BEGSR  sr_RoutInz;
012600170405
012700170713         exec sql   set option   DynUsrPrf = *Owner,
012800170713                                 CloSqlCsr = *EndMod;
012900170713
013000170713         // -?Impostazione chiusura?
013100170713         *inLR = *on;
013200170405
013300170713         // -?Reperimento dati job?
013400170713         exsr  sr_DatiJob;
013500170405
013600170405       ENDSR;
013700170405
013800170405       //--------------------------------------------------------------
013900170713       //?Reperimento Dati del job (Utente/Operativi).                 ?
014000170405       //--------------------------------------------------------------
014100170713       BEGSR  sr_DatiJob;
014200170405
014300170405         in(E) §AzUte;
014400170713         if  NOT %error;
014500170405           in(E) §DatiUte;
014600170713         endif;
014700170713         if  %error or RSut = *blanks;
014800170405           clear TIBS34ds;
014900170405           tibs34r(tibs34ds);
015000170405           in §AzUte;
015100170405           in §DatiUte;
015200170713         endif;
015300170405
015400170405       ENDSR;
015500170405
015600170712       //--------------------------------------------------------------
015700170713       //?Ciclo di lettura ANSPI00F per aggiornamento YBACL00F:        ?
015800170713       //?Elabora TUTTI i clienti che hanno Partita Iva o Cod. Fiscale ?
015900170713       //?in elenco Soggetti SPLIT PAYMENT.                            ?
016000170712       //--------------------------------------------------------------
016100170713       BEGSR  sr_ElabANSPI_UpdYBACL;
016200170405
016300170713         // -?Dichiarazione cursore?
016400170713         exec sql   declare CLIENTE   cursor for
016500170713
016600170713                    select INDkcc, INDksc, INDiva, INDcdf
016700170713                      from ANSPI00F inner join CNIND00F
016800170713                        on SPIiva = INDiva or
016900170713                           SPIiva = INDcdf;
017000170405
017100170713         // -?Apertura del cursore?
017200170713         exec sql   open CLIENTE;
017300170405
017400170713         if  SQLcode < *zero;
017500170713           exec sql   close CLIENTE;
017600170713           $Fine = *on;
017700170713           exsr  sr_PrintERR;
017800170713         endif;
017900170405
018000170713         // -?Ciclo di lettura del cursore?
018100170713         DoW  Not $Fine;
018200170713
018300170713           exec sql   fetch next   from CLIENTE   into :wSQL_ds.INDkcc,
018400170713                                                       :wSQL_ds.INDksc,
018500170713                                                       :wSQL_ds.INDiva,
018600170713                                                       :wSQL_ds.INDcdf;
018700170405
018800170713           Select;
018900170713             // -?E.o.F.?
019000170713             When  SQLcode = 100;
019100170713               $Fine = *on;
019200170713             // -?Errore?
019300170713             When  SQLcode < *zero;
019400170713               $Fine = *on;
019500170713               exsr  sr_PrintERR;
019600170713             // -?Per ogni CLIENTE aggancio YBACL ed aggiorno?
019700170713             Other;
019800170713               exsr   sr_UpdateYBACL;
019900170713           EndSl;
020000170405
020100170713         EndDo;
020200170405
020300170713         // -?Chiusura del cursore?
020400170713         exec sql   close CLIENTE;
020500170405
020600170405       ENDSR;
020700170405
020800170712       //--------------------------------------------------------------
020900170713       //?Aggiornamento Tipo Fattura in anagrafica clienti YBACL00F    ?
021000170713       //?- SE 0 = Fattura - con memorizzaz. (precedente) in ANSPIKCF. ?
021100170712       //--------------------------------------------------------------
021200170713       BEGSR  sr_UpdateYBACL;
021300170712
021400170713         // -?Aggancio YBACL01L?
021500170713         clear  keyYBACL01;
021600170713         k_ACLsocieta = c_Soc_BRT;
021700170713         k_ACLkcc     = *zero;
021800170713         %subst( k_ACLkcc : %len( k_ACLkcc ) - %len( wSQL_ds.INDkcc ) + 1 )
021900170713                      = %editc( wSQL_ds.INDkcc : 'X' );
022000170713         k_ACLksc     = *zero;
022100170713         %subst( k_ACLksc : %len( k_ACLksc ) - %len( wSQL_ds.INDksc ) + 1 )
022200170713                      = %editc( wSQL_ds.INDksc : 'X' );
022300170713
022400170713         chain  %kds( keyYBACL01 )  YBACL000;
022500170713
022600170713         If  %found( YBACL01L )  and  ACLtft = *zero;
022700170713
022800170713           // -?Memorizzazione dati attuali in ANSPIKCF?
022900170713           clear  ANSPIKSC;
023000170713           ANKsocieta = ACLsocieta;
023100170713           ANKkcc     = ACLkcc;
023200170713           ANKksc     = ACLksc;
023300170713           ANKtft     = ACLtft;
023400170713           ANKfft     = ACLfft;
023500170713           ANKtdf     = ACLtdf;
023600170713           ANKdps     = ACLdps;
023700170713           ANKdus     = ACLdus;
023800170713           ANKcdf     = wSQL_ds.INDcdf;
023900170713           ANKiva     = wSQL_ds.INDiva;
024000170713           //______________
024100170713           write  ANSPIKSC;
024200170713           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
024300170713
024400170713           // -?Aggiornamento di YBACL00F?
024500170713           ACLtft = 1;
024600170713           //_______________
024700170713           update  YBACL000;
024800170713           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
024900170713
025000170713         EndIf;
025100170713
025200170713       ENDSR;
025300170713
025400170713       //--------------------------------------------------------------
025500170713       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
025600170713       //--------------------------------------------------------------
025700170713       BEGSR  sr_PrintERR;
025800170713
025900170713         // -?Stampa del Dump?
026000170713         Dump(A);
026100170713
026200170713         // -?Stampa del Job-Log?
026300170713         Qcmd = 'DSPJOBLOG job(*) output(*print)';
026400170713         exsr  sr_ExecCmd;
026500170713
026600170713         // -?Uscita dal *pgm?
026700170713         exsr  sr_RoutEnd;
026800170713
026900170713       ENDSR;
027000170713
027100170713       //--------------------------------------------------------------
027200170713       //?Esecuzione del comando (già impostato).                      ?
027300170713       //--------------------------------------------------------------
027400170713       BEGSR  sr_ExecCmd;
027500170713
027600170713         clear Qcap0100;
027700170713         Qcabcsdh = *off;
027800170713         Qcapa    = *off;
027900170713         Qcacmdss = *off;
028000170713         Qcaerved = *allX'00';
028100170713
028200170713         clear Qusec;
028300170713         Qusbprv  = %size(Qusec);
028400170713
028500170713         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
028600170713                           %size(Qcap0100) : 'CPOP0100' : *omit :
028700170713                           0 : 0 : Qusec);
028800170713
028900170713         //*·if  Qusei <> *blank;
029000170713         //*·  ...;
029100170713         //*·endif;
029200170713
029300170713       ENDSR;
029400170712
029500170405       //--------------------------------------------------------------
029600170713       //?Operazioni finali.                                           ?
029700170405       //--------------------------------------------------------------
029800170713       BEGSR  sr_RoutEnd;
029900170405
030000170713         // -?Uscita dal *pgm?
030100170405         return;
030200170405
030300170405       ENDSR;
030400170405
030500170405      /end-free
