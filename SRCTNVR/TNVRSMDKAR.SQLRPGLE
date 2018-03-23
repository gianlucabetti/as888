000100150630       //==============================================================
000200150630       //?TNVRSMDKAR - Inserim. Beneficiari pagamento C.A. in FNDKA/P. ?
000300150630       //==============================================================
000400150630
000500150630       //--------------------------------------------------------------
000600150630       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150630       //--------------------------------------------------------------
000800150630
000900150630     /*PRM dbgview(*source)
001000150630     /*END
001100150630
001200150630       //--------------------------------------------------------------
001300150630       //?Specifiche di controllo.                                     ?
001400150630       //--------------------------------------------------------------
001500150630
001600150630     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150630     h dftactgrp(*no) bnddir('UBBNDDIR')
001800150630     h alwnull(*inputonly)
001900150630
002000150630       //--------------------------------------------------------------
002100150630       //?Dichiarazione file.                                          ?
002200150630       //--------------------------------------------------------------
002300150630
002400150713       // -?Bolle Sede?
002500150713     fTITAS30C  if   e           k disk
002600150713     fTITAA30C  if   e           k disk
002700150713
002800150713       // -?Anagrafica Luoghi/Spedizionieri?
002900150713     fFNSPE01L  if   e           k disk
003000150713
003100150713       // -?DANNI: Anagrafiche per DANNI?
003200150713     fFNDKA01L  if   e           k disk
003300150713
003400150713       // -?PROJ - Regole Contabili?
003500150713     fANRCO01L  if   e           k disk
003600150713
003700150713       // -?PROJ - Anagrafica Soggetti?
003800150713     fANSOG01L  if   e           k disk
003900150713
004000150713       // -?PROJ - Anagrafica Indirizzi dei Soggetti?
004100150713     fANIND01L  if   e           k disk
004200150713
004300150713       // -?File Tabelle generiche?
004400150713     fTABEL00F  if   e           k disk
004500150914
004600150914       // -?Stampa segnalazioni di errore?
004700150914     fQSYSPRT   o    f  132        printer usropn
004800150914     f                                     oflind(*inOF)
004900150630
005000150630       //--------------------------------------------------------------
005100150630       //?Definizione costanti.                                        ?
005200150630       //--------------------------------------------------------------
005300150630
005400150713     d c_Societa       c                   const('201')
005500150630
005600150630       //--------------------------------------------------------------
005700150630       //?Definizione schiere.                                         ?
005800150630       //--------------------------------------------------------------
005900150630
006000150914     d sk_Msg          s             78    dim( 4)  ctdata  perrcd( 1)
006100150630
006200150630       //--------------------------------------------------------------
006300150630       //?Definizione aree dati.                                       ?
006400150630       //--------------------------------------------------------------
006500150630
006600150713       // -?Dati utente?
006700150713     d §AzUte        e ds                  extname(AZUTE00F)
006800150713     d                                     dtaara
006900150713     d §DatiUte      e ds                  extname(dDatiUte)
007000150713     d                                     dtaara
007100150630
007200150630       //--------------------------------------------------------------
007300150630       //?Definizione strutture dati.                                  ?
007400150630       //--------------------------------------------------------------
007500150630
007600150630       // -?Status ds?
007700150630     d Status         sds
007800150630     d   SDSpgm          *proc
007900150630     d   SDSusr              254    263
008000150630
008100150630       // -?Dati estratti via SQL dal file FNDCT00F?
008200150630     d FNDCT_ds      e ds                  extname(FNDCT00F)  inz
008300150630
008400150630       // -?Flag operativi di FNDCT (DCTFLO)?
008500150630     d dDCT01        e ds                  qualified          inz
008600150713
008700150713       // -?Tabella "TB" = Tipi Bolla?
008800150713     d dsTB          e ds                  inz
008900150630
009000150630       //--------------------------------------------------------------
009100150630       //?Definizione variabili globali.                               ?
009200150630       //--------------------------------------------------------------
009300150630
009400150630       // -?Flags booleani?
009500150630     d $Fine           s               n   inz
009600150630
009700150630       // -?Stringhe SQL da eseguire?
009800150630     d wSQL            s           1024    Varying            inz
009900150713
010000150713       // -?Campi di Comodo?
010100150713     ***d wEstraz         s              1    inz
010200150914     d wDate_Eur       s               d   inz  datfmt(*eur)
010300150914     d wDate           s              8  0 inz
010400150914     d wTime           s              6  0 inz
010500150914     d wErr            s              3  0 inz
010600150914     d wMsg            s             78    inz
010700150713     d wComo4          s              4  0 inz
010800150713     d w§DCTport       s                   like(dDCT01.§DCTport)  inz
010900150713     d wFIL            s                   like(iDNU1fil)         inz
011000150713     d wCodCLI         s                   like(ioDNU1ksc)        inz
011100150713     d wrkRAG          s                   like(ioDNU1rag)        inz
011200150713     d wrkVIA          s                   like(ioDNU1via)        inz
011300150713     d wrkCIT          s                   like(ioDNU1loc)        inz
011400150713     d wBONcap         s                   like(ioDNU1cap)        inz
011500150713     d wBONprv         s                   like(ioDNU1prv)        inz
011600150713     d wBONnazione     s                   like(ioDNU1naz)        inz
011700150630
011800150630       //--------------------------------------------------------------
011900150630       //?Definizione procedure usate.                                 ?
012000150630       //--------------------------------------------------------------
012100150630
012200150713       // -?Reperimento dati utente?
012300150713     d TIBS34ds      e ds
012400150713      /copy gaitrasrc/srcProtoPR,TIBS34R
012500150713
012600150630       // -?Scrittura Anagrafica per Danni?
012700150630     d fidnU1ds      e ds                  inz
012800150630     d fidnU1r         pr                  extpgm('FIDNU1R')
012900150630     d   fidnU1ds                          likeds(FIDNU1ds)
013000150630
013100150630       // -?Parametri API QCAPCMD (Process Commands)?
013200150630     d Qcmd            s           1024    Varying            inz
013300150630      /copy qSysInc/qRpgleSrc,QCAPCMD
013400150630      /copy gaitrasrc/srcProtoPR,QCAPCMD
013500150630
013600150630       // -?Parametri gestione errori API.?
013700150630      /copy qSysInc/qRpgleSrc,QUSEC
013800150630
013900150630       //--------------------------------------------------------------
014000150630       //?Definizione key-list.                                        ?
014100150630       //--------------------------------------------------------------
014200150630
014300150630       // -?File FNDCT01L?
014400150630     d keyFNDCT01    e ds                  extname( FNDCT01L : *key )
014500150630     d                                     prefix( k_ )  inz
014600150713
014700150713       // -?File TABEL00F?
014800150713     C     *like         Define    tblkut        Kkut
014900150713     C     *like         Define    tblcod        kCodo
015000150713     C     *like         Define    tblkey        Kkeyo
015100150713     C     KEYTABO1      KLIST
015200150713     C                   KFLD                    Kkut
015300150713     C                   KFLD                    kCodo
015400150713     C                   KFLD                    Kkeyo
015500150713     C                   EVAL      KKut = 1
015600150713       // -?File TITAS30C?
015700150713     C     *like         Define    tasaas        Kaas
015800150713     C     *like         Define    taslnp        klnp
015900150713     C     *like         Define    tasnrs        Knrs
016000150713     C     *like         Define    tasnsp        Knsp
016100150713     C     KEYTAS        KLIST
016200150713     C                   KFLD                    Kaas
016300150713     C                   KFLD                    klnp
016400150713     C                   KFLD                    Knrs
016500150713     C                   KFLD                    Knsp
016600150713       // -?File TITAA30C?
016700150713     C     KEYTAA        KLIST
016800150713     C                   KFLD                    Kaas
016900150713     C                   KFLD                    klnp
017000150713     C                   KFLD                    Knrs
017100150713     C                   KFLD                    Knsp
017200150713     C                   KFLD                    taaTrc
017300150713     C                   EVAL      taaTrc = 'M'
017400150713       // -?File TNTMD01L?
017500150713     C     KEYTMD        KLIST
017600150713     C                   KFLD                    Kaas
017700150713     C                   KFLD                    klnp
017800150713     C                   KFLD                    Knrs
017900150713     C                   KFLD                    Knsp
018000150713       // -?File FNDKA01L?
018100150713     C     *LIke         Define    DKAaac        Kaac
018200150713     C     *LIke         Define    DKAfil        kfil
018300150713     C     *LIke         Define    DKAnca        Knca
018400150713     C     *like         Define    dkatrc        Ktrc
018500150713     C     KEYDKA        KLIST
018600150713     C                   KFLD                    Kaac
018700150713     C                   KFLD                    kfil
018800150713     C                   KFLD                    Knca
018900150713     C                   KFLD                    Ktrc
019000150713       // -?File ANRCO01L?
019100150713     C     *LIKE         Define    RCOsocieta    Ksocieta
019200150713     C     *LIKE         Define    RCOkcc        Kkcc
019300150713     C     *LIKE         Define    RCOksc        Kksc
019400150713     C     KEYRCO        KLIST
019500150713     C                   KFLD                    Ksocieta
019600150713     C                   KFLD                    Kkcc
019700150713     C                   KFLD                    kksc
019800150713     C                   eval      Ksocieta = c_Societa
019900150713       // -?File ANIND00F?
020000150713     C     *like         Define    indsogg       Ksogg
020100150713     C     *like         Define    indtpind      Ktpind
020200150713     C     *like         Define    indcdind      kcdind
020300150713     C     KEYIND        KLIST
020400150713     C                   KFLD                    Ksogg
020500150713     C                   KFLD                    Ktpind
020600150713     C                   KFLD                    kcdind
020700150713       // -?File FNSPE01L?
020800150713     C     *like         Define    Spefls        KFls
020900150713     C     *like         Define    Specli        kcli
021000150713     C     *like         DEfine    Specod        Kcodl
021100150713     C     KEYSPE        KLIST
021200150713     C                   KFLD                    KFls
021300150713     C                   KFLD                    kcli
021400150713     C                   KFLD                    Kcodl
021500150630
021600150630       //--------------------------------------------------------------
021700150630       //?Riepilogo indicatori usati.                                  ?
021800150630       //--------------------------------------------------------------
021900150630       //--------------------------------------------------------------
022000150630
022100150630       //--------------------------------------------------------------
022200150630       //?M A I N - L I N E.                                           ?
022300150630       //--------------------------------------------------------------
022400150630
022500150630     c*//  *Entry        plist
022600150630     c*//                parm                    KPJBA
022700150630
022800150630      /free
022900150630
023000150630       // -?Operazioni iniziali?
023100150630       exsr  sr_RoutInz;
023200150630
023300150630       // -?Ciclo di lettura file Danni?
023400150630       DoU  sqlCode <> *zero;
023500150630         exsr  sr_ReadCursor;
023600150630       EndDo;
023700150630
023800150630       // -?Operazioni finali?
023900150630       exsr sr_RoutEnd;
024000150630
024100150630       //--------------------------------------------------------------
024200150630       //?Operazioni iniziali.                                         ?
024300150630       //--------------------------------------------------------------
024400150630       BEGSR  sr_RoutInz;
024500150630
024600150630         *inLR = *on;
024700150713
024800150713         // -?Reperimento dati job?
024900150713         exsr  sr_DatiJob;
025000150914
025100150914         // -?Reperimento data odierna in formato aaaammgg?
025200150914         wDate_Eur = %date();
025300150914         wDate = %dec( wDate_Eur );
025400150914
025500150914         // -?Reperimento orario?
025600150914         wTime = %dec( %time() );
025700150630
025800150630         // -?Impostazione opzioni per SQL?
025900150630         exec SQL   set option   DynUsrPrf = *owner,
026000150630                                 CloSqlCsr = *endmod;
026100150630
026200150630         // -?Creazione stringa SQL da eseguire?
026300150630         wSQL = 'select * +
026400150630                   from FNDCT00F +
026500150630                  where dctCCH <> '' '' +
026600150630                  order by dctAAC, dctFIL, dctNCA';
026700150630
026800150630         // -?Apertura cursore?
026900150630         exsr  sr_OpenCursor;
027000150630
027100150630       ENDSR;
027200150713
027300150713       //--------------------------------------------------------------
027400150713       //?Reperimento Dati del job (Utente/Operativi).                 ?
027500150713       //--------------------------------------------------------------
027600150713       BEGSR  sr_DatiJob;
027700150713
027800150713         in(E) §AzUte;
027900150713         if NOT %error;
028000150713           in(E) §DatiUte;
028100150713         endif;
028200150713         if %error or RSut = *blanks;
028300150713           clear TIBS34ds;
028400150713           tibs34r ( tibs34ds );
028500150713           in §AzUte;
028600150713           in §DatiUte;
028700150713         endif;
028800150713
028900150713       ENDSR;
029000150630
029100150630       //--------------------------------------------------------------
029200150630       //?Apertura cursore C1.                                         ?
029300150630       //--------------------------------------------------------------
029400150630       BEGSR  sr_OpenCursor;
029500150630
029600150630         // -?Dichiarazione del cursore?
029700150630         exec sql  prepare S1  from :wSQL;
029800150630         exec sql  declare C1  cursor for S1;
029900150630
030000150630         // -?Apertura del cursore?
030100150630         exec sql   open C1;
030200150630
030300150630         if  SQLcode < *zero;
030400150630           exsr  sr_PrintErr;
030500150630         endif;
030600150630
030700150630       ENDSR;
030800150630
030900150630       //--------------------------------------------------------------
031000150630       //?Lettura cursore C1.                                          ?
031100150630       //--------------------------------------------------------------
031200150630       BEGSR  sr_ReadCursor;
031300150630
031400150630         clear  FNDCT_ds;
031500150630
031600150630         exec SQL  fetch next  from C1  into :FNDCT_ds;
031700150630
031800150630         select;
031900150630           // -?Fine lettura?
032000150630           when  sqlCode = 100;
032100150630             //exsr  sr_RoutEnd;
032200150630           // -?Errore?
032300150630           when  sqlCode < *zero;
032400150630             exsr  sr_PrintErr;
032500150630           // -?Elaborazione?
032600150630           other;
032700150630             exsr  sr_ElabRec;
032800150630         endsl;
032900150630
033000150630       ENDSR;
033100150630
033200150630       //--------------------------------------------------------------
033300150630       //?Chiusura cursore C1.                                         ?
033400150630       //--------------------------------------------------------------
033500150630       BEGSR  sr_CloseCursor;
033600150630
033700150630         // -?Chiusura del cursore?
033800150630         exec sql   close C1;
033900150630
034000150630       ENDSR;
034100150630
034200150630       //--------------------------------------------------------------
034300150630       //?Elaborazione singolo record estratto.                        ?
034400150630       //--------------------------------------------------------------
034500150630       BEGSR  sr_ElabRec;
034600150915
034700150915         clear  wErr;
034800150915         clear  wrkRAG;
034900150630
035000150630         // -?Verifica se C.A. pagata?
035100150630         dDCT01 = dctFLO;
035200150630
035300151105         if  dDCT01.§DCTpaga <> 'P';
035400150630           leavesr;
035500150630         endif;
035600150630
035700150630         // -?Reperimento dati anagrafici del beneficiario?
035800150713         exsr  sr_RepBenef;
035900150630
036000150713         If  WrkRAG <> *blank;
036100150630
036200150630           clear  fidnU1ds;
036300150713           // -?Impostazione dati per richiamare pgm scrittura Beneficiario?
036400150630           iDNU1mod  = 'S';
036500150630           iDNU1aac  = DCTaac;
036600150630           iDNU1fil  = DCTfil;
036700150630           iDNU1nca  = DCTnca;
036800150630           iDNU1trc  = 'P';
036900150713           ioDNU1kcc = DUTkci;
037000150713           ioDNU1ksc = wCodCli;
037100150713           ioDNU1rag = wrkRag;
037200150713           ioDNU1via = wrkVia;
037300150713           ioDNU1cap = wBONcap;
037400150713           ioDNU1loc = wrkCit;
037500150713           ioDNU1prv = wBONprv;
037600150713           ioDNU1naz = wBONnazione;
037700150713           //ioDNU1tel = ......;
037800150713           //ioDNU1fax = ......;
037900150630
038000150630           fidnU1r ( fidnU1ds );
038100150630
038200150630           if  oDNU1err = *on;
038300150630             exsr  sr_PrintErr;
038400150630           endif;
038500150630
038600150630         EndIf;
038700150630
038800150630       ENDSR;
038900150713
039000150713       //--------------------------------------------------------------
039100150713       //?Reperimento Beneficiario (come in *pgm YHD2R).               ?
039200150713       //--------------------------------------------------------------
039300150713       BEGSR  sr_RepBenef;
039400150713
039500150713      /end-free
039600150713
039700150713     C* Determino la società a cui compete il pagamento del danno
039800150713     C                   exsr      repsoc
039900150915     c                   if        wErr <> *zero
040000150915     c                   leavesr
040100150915     c                   endif
040200150713     C* Se non ho ancora reperito il codice cliente lo ricerco
040300150713     C                   exsr      sr_RepCli
040400150713      /free
040500150713             // -?Sostituzione del cod. xxx9999 con xxx8888?
040600150713             exsr  sr_Cli9999;
040700150713      /end-free
040800150922      *
040900150922      *?Se il codice cliente non è un 8888 ne devo estrarre?
041000150922      *  ?il codice Soggetto?
041100150713     c                   clear                   rcoSogg
041200150713     C                   move      wcodcli       wcomo4
041300150713     C                   if        wcomo4<>8888 and wcomo4<>9999                -- 02 --
041400150713     ***C                   move      hd1soc        ksocieta
041500150915     C                   move      *zeros        kkcc
041600150713     C                   move      DUTkci        kkcc
041700150713     C                   move      *zeros        kksc
041800150713     C                   move      wcodcli       kksc
041900150713     C     keyrco        chain     anrco01l                           31
042000150713     C                   end                                                    -- 02 --
042100150713      *
042200150713     C* Eseguo routine per recuperare i dati anagrafici
042300150713     C                   Exsr      Repanag
042400150713      *
042500150713      /free
042600150713
042700150713       ENDSR;
042800150713
042900150713      /end-free
043000150713
043100150713     C*-----------------------------------------------------*
043200150713     C*  Aggancio DKA                                       *
043300150713     C*-----------------------------------------------------*
043400150713     C     DTADKA        BEGSR
043500150713     C*
043600150713     C     KeyDka        CHAIN     FnDka01L                           31
043700150713     C                   IF        %FOUND AND DkaAtb = *BLANK
043800150713     C* dati anagrafici da FNDKA
043900150713     C                   movel     *blanks       wrkrag
044000150713     C                   movel     *blanks       wrkvia
044100150713     C                   movel     *blanks       wrkcit
044200150713     C                   movel     dkarag        wrkrag
044300150713     C                   movel     dkavia        wrkvia
044400150713     C                   movel     dkaloc        wrkcit
044500150713     C                   movel     dkacap        wBONcap
044600150713     C                   movel     dkaprv        wBONprv
044700150713     C                   movel     dkanaz        wBONnazione
044800150713     C*
044900150713     C                   EVAL      WCodCli = DkaKsc
045000150713     C*
045100150914     c                   Else
045200150914     c*
045300150914     c                   eval      wErr = 3
045400150914     c                   exsr      sr_StampaErr
045500150915     c                   leavesr
045600150914     c*
045700150713     C                   ENDIF
045800150713     C*
045900150713     C                   endsr
046000150713     C*-----------------------------------------------------*
046100150713     C*  Reperisco la società                               *
046200150713     C*-----------------------------------------------------*
046300150713     C     REPSOC        BEGSR
046400150713     C*
046500150713     C                   move      *zeros        Wfil
046600150713     C                   move      *zeros        Wcodcli
046700150713      * Aggancio file bolle
046800150713     C                   move      dctaas        kaas
046900150713     C                   move      dctlnp        klnp
047000150713     C                   move      dctnrs        knrs
047100150713     C                   move      dctnsp        knsp
047200150713     c     KEYTAS        chain     TITAS30C                           30
047300150713      * ecludo le bolle di recupero
047400150713     C                   movel     'TB'          kCODo
047500150713     C                   movel(P)  TAStbl        kKEYo
047600150713     C     keytabo1      CHAIN     TABEL00F                           31
047700150713     C                   movel     TBLuni        DSTB
047800150713     c                   if        §TBrbl = 'R'                                 -- 03 --
047900150713     c     KEYTAS        reade     TITAS30C                               30
048000150713     c                   endif                                                  -- 03 --
048100150713      *
048200150713      /free
048300150713
048400150713         If  *in30;
048500150713           clear  TAScca;
048600150713           clear  TASccm;
048700150713           clear  TASksc;
048800150914           wErr = 1;
048900150914           exsr  sr_StampaErr;
049000150915           leavesr;
049100150713         EndIf;
049200150713
049300150713      /end-free
049400150713      *
049500150713     C* Se il benficiario indicato sulla pratica di danno non
049600150713     C* è un cliente generico reperisco l'azienda di apparten.
049700150713     C* in base al codice della filiale
049800150713     C                   move      dctksc        wcomo4
049900150713     C* Se esiste il codice beneficiario detemino l'azienda
050000150713     C* in base al ramo di appartenenza
050100150713     C                   if        dctksc>0 and wcomo4<>8888 and wcomo4<>9999   -- 01 --
050200150713     C                   movel     dctksc        wfil
050300150713     C                   move      dctksc        Wcodcli
050400150713     C* Se non c'è codice beneficiario sulla pratica di danno
050500150713     C* provo ad agganciare file DKA per vedere se è indicato lì
050600150713     C                   else                                                   -- 01 --
050700150713     C* .. per liquid. transativa se non ho trovato il codice
050800150713     C* beneficiario sulla pratica di danno provo ad agganciare
050900150713     C* la bolla
051000150713      *
051100150713      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
051200150713      *  sped. è "Mamma di cambio porto" e mi trovo in filiale, in questo caso inverto il porto C.A.
051300150713     c                   SELECT                                                 -- 03 --
051400150713     c                   WHEN      TAScca <> '9'
051500150713     c                   eval      w§DCTport = dDCT01.§DCTport
051600150713     c                   WHEN      dDCT01.§DCTport = 'A'
051700150713     c                   movel     'F'           w§DCTport
051800150713     c                   WHEN      dDCT01.§DCTport = 'F'
051900150713     c                   movel     'A'           w§DCTport
052000150713     c                   ENDSL                                                  -- 03 --
052100150713      *
052200150713      * Dalla spedizione determino il codice beneficiario
052300150713     c                   SELECT                                                 -- 03 --
052400150713     c                   WHEN       DCTptr = 'D'  and  W§DCTport = 'F'           DESTINATARIO  PF
052500150713     c                   WHEN       DCTptr = 'M'
052600150713     c                   movel     TASccm        wfil
052700150713     C                   move      tasccm        Wcodcli
052800150713     c                   OTHER
052900150713     c                   movel     TASksc        wfil
053000150713     C                   move      tasksc        Wcodcli
053100150713     C                   ENDSL                                                  -- 03 --
053200150713     C*
053300150713     C* Se non sono ancora riuscita ad recuperare la filiale
053400150713     C* responsabile me la calcolo in base al flag beneficiario:
053500150713     C* se = 'D' (destinatario) la filiale responsabile = lna
053600150713     c                   IF        Wfil = *zeros
053700150713     c                   IF        DCTptr = 'D'
053800150713     C                   z-add     DCTlna        wfil
053900150713     c                   ELSE
054000150713     C* .. altrimenti reperisco la filiale responsabile dalla lnp
054100150713     C*    della C.A.
054200150713     C                   eval      wFIL = %int( dDCT01.§DCTlnpc )
054300150713     c                   ENDIF
054400150713     c                   ENDIF
054500150713     C*
054600150713     C                   end                                                    -- 01 --
054700150914
054800150914      /free
054900150914         if  wFil = *zero;
055000150914           wErr = 2;
055100150914           exsr  sr_StampaErr;
055200150915           leavesr;
055300150914         endif;
055400150914      /end-free
055500150713     C*
055600150713     C     finsoc        endsr
055700150713      *
055800150713      /free
055900150713
056000150713       //----------------------------------------------------?
056100150713       //?Impostazione del codice cliente a xxx8888 SE non   ?
056200150713       //?  reperito.                                        ?
056300150713       //----------------------------------------------------
056400150713       BEGSR  sr_RepCli;
056500150713
056600150713         // -?Se non ho ancora reperito il codice cliente imposto?
056700150713         //  ?(Wcodcli) dalla filiale della C.A. + 8888?
056800150922         //  ?- NON da quella responsabile -?
056900150713         if  wCodCli = *zero;
057000150922           //wCodCli = ( wFil * 10000 ) + 8888;
057100150713           wCodCli = ( DCTfil * 10000 ) + 8888;
057200150713         endif;
057300150713
057400150713       ENDSR;
057500150713
057600150713       //--------------------------------------------------------------
057700150713       //?"Sistemazione" codice cliente Beneficiario SE xxx9999.       ?
057800150713       //--------------------------------------------------------------
057900150713       BEGSR  sr_Cli9999;
058000150713
058100150713         if  %subst( %editc( wCodCli : 'X' ) : 4 : 4 ) = '9999';
058200150922           //wCodCli = ( wFil * 10000 ) + 8888;
058300150713           wCodCli = ( DCTfil * 10000 ) + 8888;
058400150713         endif;
058500150713
058600150713       ENDSR;
058700150713
058800150713      /end-free
058900150713
059000150713     C*-----------------------------------------------------*
059100150713     C*  Reperimento dati anagrafici                        *
059200150713     C*-----------------------------------------------------*
059300150713     C     REPANAG       BEGSR
059400150713     C*
059500150713     C                   CLEAR                   WrkRag
059600150713     C                   CLEAR                   WrkVia
059700150713     C                   CLEAR                   WrkCit
059800150713     C                   CLEAR                   wBonCap
059900150713     C                   CLEAR                   wBonPrv
060000150713     C                   CLEAR                   wBonNazione
060100150713     C* ... controllo che dctksc=0
060200150713     C                   if        dctksc=0                                     -- 01 --
060300150713     C* in tal caso prendo il codice che c'è sulla bolla
060400150713     C                   move      wcodcli       wcomo4
060500150713     C* se il codice che c'è sulla bolla è di un codificato controllo
060600150713     C                   if        wcomo4<>8888 and wcomo4<>9999                -- 02 --
060700150922     C* controllo se c'è luogo 008
060800150713     C                   movel     'L'           Kfls
060900150713     C                   move      wcodcli       Kcli
061000150713     C                   move      '008'         Kcodl
061100150713     C     Keyspe        chain     fnspe01l                           31
061200150713     C* controllo se ci sono i luoghi di spedizione avviso denuncia
061300150713     C                   if        not *in31                                    -- 04 --
061400150713     C                   movel     *blanks       wrkrag
061500150713     C                   movel     *blanks       wrkvia
061600150713     C                   movel     *blanks       wrkcit
061700150713     C                   movel     sperag        wrkrag
061800150713     C                   movel     speind        wrkvia
061900150713     C                   movel     speloc        wrkcit
062000150713     C                   movel     specap        wboncap
062100150713     C                   movel     spepro        wbonprv
062200150713     C                   movel     spenaz        wbonnazione
062300150714     C                   end                                                    -- 04 --
062400150922     C* Se non esiste il luogo 008 controllo se esiste il luogo 006
062500150713     C                   if        *in31                                        -- 03 --
062600150713     C                   movel     'L'           Kfls
062700150713     C                   move      wcodcli       Kcli
062800150713     C                   move      '006'         Kcodl
062900150713     C     Keyspe        chain     fnspe01l                           31
063000150713     C* controllo se ci sono i luoghi di spedizione avviso denuncia
063100150713     C                   if        not *in31                                    -- 04 --
063200150713     C                   movel     *blanks       wrkrag
063300150713     C                   movel     *blanks       wrkvia
063400150713     C                   movel     *blanks       wrkcit
063500150713     C                   movel     sperag        wrkrag
063600150713     C                   movel     speind        wrkvia
063700150713     C                   movel     speloc        wrkcit
063800150713     C                   movel     specap        wboncap
063900150713     C                   movel     spepro        wbonprv
064000150713     C                   movel     spenaz        wbonnazione
064100150713     C                   else
064200150713     C* ... reperisco i
064300150713     C*     i dati da anagrafica
064400150713      *     se non ho trovato niente sui luoghi
064500150713     C                   move      rcosogg       ksogg
064600150713     C                   clear                   ktpind
064700150713     C                   clear                   kcdind
064800150713     C     Ksogg         chain     ansog01l                           31
064900150713     C     Keyind        chain     anind01l                           31
065000150713     C                   movel     *blanks       wrkrag
065100150713     C                   movel     *blanks       wrkvia
065200150713     C                   movel     *blanks       wrkcit
065300150713     C                   movel     sogdes        wrkrag
065400150713     C                   movel     indindriz     wrkvia
065500150713     C                   movel     indlocalit    wrkcit
065600150713     C                   movel     indcap        wboncap
065700150713     C                   movel     indprov       wbonprv
065800150713     C                   movel     indstato      wbonnazione
065900150714     C                   end                                                    -- 04 --
066000150713     C                   end                                                    -- 03 --
066100150713     C* ... se il codice cliente non è un codificato aggancio TNTMD
066200150713     C                   else
066300150713     C                   exsr      chntmd
066400150713     C                   end                                                    -- 02 --
066500150713     c*
066600150713     C* se c'è dctksc ....
066700150713     C                   else                                                   -- 01 --
066800150713     C                   move      dctksc        wcomo4
066900150713      * è un codificato
067000150713     C                   if        wcomo4<>8888 and wcomo4<>9999                -- 02 --
067100150922      * controllo se c'è luogo 008
067200150713     C                   movel     'L'           Kfls
067300150713     C                   move      dctksc        Kcli
067400150713     C                   move      '008'         Kcodl
067500150713     C     Keyspe        chain     fnspe01l                           31
067600150713     C* controllo se ci sono i luoghi di spedizione avviso denuncia
067700150713     C                   if        not *in31                                    -- 04 --
067800150713     C                   movel     *blanks       wrkrag
067900150713     C                   movel     *blanks       wrkvia
068000150713     C                   movel     *blanks       wrkcit
068100150713     C                   movel     sperag        wrkrag
068200150713     C                   movel     speind        wrkvia
068300150713     C                   movel     speloc        wrkcit
068400150713     C                   movel     specap        wboncap
068500150713     C                   movel     spepro        wbonprv
068600150713     C                   movel     spenaz        wbonnazione
068700150713     C                   endif                                                  -- 04 --
068800150922     C* Se non esiste il luogo 008 controllo se esiste il luogo 006
068900150713     C                   if        *in31                                        -- 03 --
069000150713     C                   movel     'L'           Kfls
069100150713     C                   move      dctksc        Kcli
069200150713     C                   move      '006'         Kcodl
069300150713     C     Keyspe        chain     fnspe01l                           31
069400150713     C* controllo se ci sono i luoghi di spedizione avviso denuncia
069500150713     C                   if        not *in31                                    -- 04 --
069600150713     C                   movel     *blanks       wrkrag
069700150713     C                   movel     *blanks       wrkvia
069800150713     C                   movel     *blanks       wrkcit
069900150713     C                   movel     sperag        wrkrag
070000150713     C                   movel     speind        wrkvia
070100150713     C                   movel     speloc        wrkcit
070200150713     C                   movel     specap        wboncap
070300150713     C                   movel     spepro        wbonprv
070400150713     C                   movel     spenaz        wbonnazione
070500150713     C                   endif                                                  -- 04 --
070600150713     C                   endif                                                  -- 03 --
070700150713     C* ... ed il codice non è un cliente generico reperisco i
070800150713     C*     i dati da anagrafica
070900150713      *     se non ho trovato niente sui luoghi
071000150713     c                   If        *In31                                        -- 03 --
071100150713     C                   move      rcosogg       ksogg
071200150713     C                   clear                   ktpind
071300150713     C                   clear                   kcdind
071400150713     C     Ksogg         chain     ansog01l                           31
071500150713     C     Keyind        chain     anind01l                           31
071600150713     C                   movel     *blanks       wrkrag
071700150713     C                   movel     *blanks       wrkvia
071800150713     C                   movel     *blanks       wrkcit
071900150713     C                   movel     sogdes        wrkrag
072000150713     C                   movel     indindriz     wrkvia
072100150713     C                   movel     indlocalit    wrkcit
072200150713     C                   movel     indcap        wboncap
072300150713     C                   movel     indprov       wbonprv
072400150713     C                   movel     indstato      wbonnazione
072500150713     c                   endif
072600150713     C                   else                                                   -- 02 --
072700150713     C* ... se dctksc è un cliente generico aggancio dka con 'C'
072800150713     C* ... e reperisco i dati anagrafici da lì
072900150714     c                   eval      kaac = DCTaac
073000150714     c                   eval      kfil = DCTfil
073100150714     c                   eval      knca = DCTnca
073200150713     C                   move      'C'           Ktrc
073300150713     C                   exsr      dtadka
073400150713     C                   end                                                    -- 02 --
073500150713     C                   end                                                    -- 01 --
073600150713     **?Nel caso di C.A. con tipo gestione "pratica assicurativa con
073700150713     **?franchigia", se presente rivalsa bisogna intestare l'assegno
073800150713     **?alla società assicuratrice che ha il diritto di rivalsa.
073900150713     ***C                   IF        Hd1TpG = 'F' OR Hd1TpG = 'S' OR Hd1TpG = 'G'
074000150713      * Pagamento no progetto liquidazione (già pratica in franchigia):
074100150713      *   CCH = "15"
074200150713      * Pagamento pratica Surroga:
074300150713      *   CCH = "17"
074400150713      * Pagamento pratica Bartolini:
074500150713      *   CCH = "18"
074600150713     c                   IF        DCTcch = '15'  or
074700150713     c                             DCTcch = '17'  or
074800150713     c                             DCTcch = '18'
074900150714     c                   eval      kaac = DCTaac
075000150714     c                   eval      kfil = DCTfil
075100150714     c                   eval      knca = DCTnca
075200150713     ***C                   EVAL      KTrc = Rivalsa
075300150713     C                   EVAL      KTrc = 'R'
075400150713     C                   EXSR      DtaDka
075500150713     C                   ENDIF
075600150914
075700150914      /free
075800150914         if  wrkRAG = *blank;
075900150914           wErr = 4;
076000150914           exsr  sr_StampaErr;
076100150915           leavesr;
076200150914         endif;
076300150914      /end-free
076400150713     C*
076500150713     C                   endsr
076600150713     C*-----------------------------------------------------*
076700150713     C*  Aggancio TNTMD                                     *
076800150713     C*-----------------------------------------------------*
076900150713     C     CHNTMD        BEGSR
077000150713     C*
077100150713     C* Beneficiario=Destinatario
077200150713     c                   IF        DCTptr = 'D'                                 -- 03 --
077300150713     C                   movel     *blanks       wrkrag
077400150713     C                   movel     *blanks       wrkvia
077500150713     C                   movel     *blanks       wrkcit
077600150713     c                   movel     tasrsd        wrkrag
077700150713     c                   movel     tasind        wrkvia
077800150713     c                   movel     tascad        wboncap
077900150713     c                   movel     taslod        wrkcit
078000150713     c                   movel     tasprd        wbonprv
078100150713     c                   movel     tasnzd        wbonnazione
078200150713     c                   ELSE
078300150713     C* Beneficiario=Mittente
078400150713     C     keyTaa        chain     titaa30c                           31
078500150713     C                   IF        %FOUND()
078600150713     C                   movel     *blanks       wrkrag
078700150713     C                   movel     *blanks       wrkvia
078800150713     C                   movel     *blanks       wrkcit
078900150713     c                   movel     taaRsc        wrkrag
079000150713     c                   movel     taaInd        wrkvia
079100150713     c                   movel     taaCap        wBoncap
079200150713     c                   movel     taaLoc        wrkcit
079300150713     c                   movel     taaPrv        wBonprv
079400150713     c                   movel     taaNaz        wBonnazione
079500150713     C                   ENDIF
079600150713     c                   ENDIF
079700150713     C*
079800150713     C                   endsr
079900150713
080000150713      /free
080100150630
080200150630       //--------------------------------------------------------------
080300150630       //?Stampa segnalazione dell'errore rilevato.                    ?
080400150630       //--------------------------------------------------------------
080500150630       BEGSR  sr_PrintErr;
080600150630
080700150630         // -?Stampa del Dump?
080800150630         Dump(A);
080900150630
081000150630         // -?Stampa del Job-Log?
081100150630         Qcmd = 'DSPJOBLOG job(*) output(*print)';
081200150630         exsr  sr_ExecCmd;
081300150630
081400150630         //// -?Chiusura del programma?
081500150630         //exsr  sr_RoutEnd;
081600150630
081700150630       ENDSR;
081800150630
081900150630       //--------------------------------------------------------------
082000150630       //?Esecuzione del comando (già impostato).                      ?
082100150630       //--------------------------------------------------------------
082200150630       BEGSR  sr_ExecCmd;
082300150630
082400150630         clear Qcap0100;
082500150630         Qcabcsdh = *off;
082600150630         Qcapa    = *off;
082700150630         Qcacmdss = *off;
082800150630         Qcaerved = *allX'00';
082900150630
083000150630         clear Qusec;
083100150630         Qusbprv  = %size(Qusec);
083200150630
083300150630         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
083400150630                           %size(Qcap0100) : 'CPOP0100' : *omit :
083500150630                           0 : 0 : Qusec);
083600150630
083700150630         //if  Qusei <> *blank;
083800150630         //  ...;
083900150630         //endif;
084000150630
084100150630       ENDSR;
084200150914
084300150914       //--------------------------------------------------------------
084400150914       //?Stampa elenco errori rilevati nel reperimento dati.          ?
084500150914       //--------------------------------------------------------------
084600150914       BEGSR  sr_StampaErr;
084700150914
084800150914         if  Not  %open(QSYSPRT);
084900150914           open  QSYSPRT;
085000150914           *inOF = *on;
085100150914         endif;
085200150914
085300150914         if  *inOF;
085400150914           except  ErrTXT;
085500150914           *inOF = *off;
085600150914         endif;
085700150914
085800150914         wMsg = sk_Msg(wErr);
085900150914
086000150914         if  wErr = 3;
086100150914           wMsg = %trimR( wMsg ) + ' ' + Ktrc;
086200150914         endif;
086300150914
086400150914         except  ErrDET;
086500150914
086600150914       ENDSR;
086700150630
086800150630       //--------------------------------------------------------------
086900150630       //?Operazioni finali.                                           ?
087000150630       //--------------------------------------------------------------
087100150630       BEGSR  sr_RoutEnd;
087200150630
087300150630         // -?Chiusura cursore?
087400150630         exsr  sr_CloseCursor;
087500150914
087600150914         // -?Chiusura file di stampa errori?
087700150914         if  %open(QSYSPRT);
087800150914           except  ErrEND;
087900150914           close   QSYSPRT;
088000150914         endif;
088100150630
088200150630         // -?Uscita?
088300150630         return;
088400150630
088500150630       ENDSR;
088600150630
088700150630      /end-free
088800150914
088900150914       //--------------------------------------------------------------
089000150914       //?S P O O L - F I L E S                                        ?
089100150914       //--------------------------------------------------------------
089200150914
089300150914     oQSYSPRT   e            ErrTXT            1
089400150914     o                       RSUT
089500150914     o                                        +   6 'LISTA ERRORI RILE-
089600150914     o                                              VATI IN FASE DI IN-
089700150914     o                                              SERIMENTO BENEFICI-
089800150914     o                                              ARI IN C.A.'
089900150914     o                       SDSpgm           +   6
090000150914     o                       wDate         y  +   3
090100150914     o          e            ErrTXT      0
090200150914     o                                          +26 'LISTA ERRORI RILE-
090300150914     o                                              VATI IN FASE DI IN-
090400150914     o                                              SERIMENTO BENEFICI-
090500150914     o                                              ARI IN C.A.'
090600150914     o          e            ErrTXT      1
090700150914     o*//                    KNSIF
090800150914     o                       SDSusr           +  11
090900150914     o                                        +   5 '------------------
091000150914     o                                              -------------------
091100150914     o                                              -------------------
091200150914     o                                              -----------'
091300150914     o                                        +   6 'Pag.'
091400150914     o                       Page          z  +   0
091500150914     o                       wTime            +   5 '  :  :  '
091600150914      *
091700150914     o          e            ErrTXT      2
091800150914     o                                              'Comunic.Anomalia'
091900150914     o                                        +   2 'Spedizione         '
092000150914     o                                        +   2 'Errore'
092100150914     o          e            ErrTXT      0
092200150914     o                                              'Comunic.Anomalia'
092300150914     o                                        +   2 'Spedizione         '
092400150914     o                                        +   2 'Errore'
092500150914     o          e            ErrTXT      1
092600150914     o                                              '----------------'
092700150914     o                                        +   2 '-------------------'
092800150914     o                                        +   2 '------'
092900150914     o          e            ErrTXT      0  1
093000150914     o                                              '----------------'
093100150914     o                                        +   2 '-------------------'
093200150914     o                                        +   2 '------'
093300150914      *
093400150914     o          e            ErrDET      1
093500150914     o                       DCTfil
093600150914     o                       DCTnca        z  +   1
093700150914     o                       DCTaac           +   1
093800150914     o                       DCTlnp           +   2
093900150914     o                       DCTnrs        z  +   1
094000150914     o                       DCTnsp        z  +   1
094100150914     o                       DCTaas           +   1
094200150914     o                       wMsg             +   2
094300150914      *
094400150914     o          e            ErrEND      2
094500150914     o                                        +  10 '***  Fine Lista  ***'
094600150914     o          e            ErrEND      0
094700150914     o                                        +  10 '***  Fine Lista  ***'
094800150914
094900150914       //--------------------------------------------------------------
095000150914       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
095100150914       //--------------------------------------------------------------
095200150914
095300150914** --?sk_Msg:?Errori in stampa.?
095400150914Spedizione non presente in TITAS30C
095500150914Filiale del cliente non reperita
095600150914Spedizione non presente in FNDKA con trc.
095700150914Dati del cliente non reperiti
