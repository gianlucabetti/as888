000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200170412
000300170412
000400170412      //********************************************************************************************
000500170412      //
000600170412      // Definizione prototipi procedure.
000700170412      //
000800170412      //********************************************************************************************
000900170412
001000170412
001100170412      //********************************************************************************************
001200170412      //
001300170412      // Definizione interfacce procedure.
001400170412      //
001500170412      //********************************************************************************************
001600170419     D/COPY GAITRASRC/SRCPROTOPI,UBSGU1R
001700170522     D/COPY GAITRASRC/SRCPROTOPI,UBSI95R
001800170605     D/COPY GAITRASRC/SRCPROTOPI,UBAASGNR
001900170530     D/COPY GAITRASRC/SRCPROTOPI,UBS715R
002000170824     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
002100990907
002200170522     D* Informazioni sul job
002300170522     D psds           sds
002400170522     D  psjobname            244    253
002500170522     D  psuser               254    263
002600170522     D  psjobnbr             264    269s 0
002700010524
002800170419     D*
002900170522     D  DVPC         e ds                  inz
003000170419     D  UBSGU1DSA    e ds                  inz qualified
003100170419     D  UBBRTETIDS   e ds                  inz qualified
003200170516     D RTNsi95esito    s             10I 0
003300170516     D RTNsi95opcode   s             10A
003400170516     D RTNsi95status   s             10I 0
003500170523     D  wSGU1ADSTO     s                   inz like(UBSGU1DSA.iSGU1ADSTO)
003600170605     D  wErrIdx        s              3S 0 inz
003700170605     D  wErrMsg        s            256A   inz
003800170605     D  wErrTag        s             10A   inz
003900170824     D dftTIPF_ETI     s              2A
004000170824     D dftTIP_TRASF    s              2A
004100170516
004200170526     D UBBRTETI_MSG    ds                  inz qualified
004300170523     D   SUBTITLE01                  20
004400170523     D   SUBTITLE02                  20
004500170523     D   SUBTITLE03                  20
004600170523     D   MESSAGE_01                  40
004700170523     D   MESSAGE_02                  40
004800170523     D   MESSAGE_03                  40
004900170523     D   MESSAGE_04                  40
005000170525
005100170526     D*
005200170526     D MSGFLG          S             20    DIM(10) CTDATA PERRCD(1)
005300170525     D MSGITA          S            100    DIM(10) CTDATA PERRCD(1)
005400170525     D MSGENG          S            100    DIM(10) CTDATA PERRCD(1)
005500170525     D MSGFRA          S            100    DIM(10) CTDATA PERRCD(1)
005600170525     D MSGDEU          S            100    DIM(10) CTDATA PERRCD(1)
005700170525     D MSGESP          S            100    DIM(10) CTDATA PERRCD(1)
005800170525
005900170526     D MSGFLG_DS       ds                  qualified
006000170525     D   InviaEmail                   1    inz('1')
006100170525     D   FatalError                   1    inz('0')
006200170526
006300170526     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
006400170526     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
006500010525
006600010525
006700010525     C*-----------------------------------------------------------------------------
006800170412     C*
006900170412     C* Avvio il monitoring del intero flusso
007000170412     C                   monitor
007100170522     C*
007200170522     C* Verifica parametri
007300170522     C                   exsr      chkPar
007400170525     C*
007500170526     C* Inizializzazione di work
007600170526     C                   exsr      exeInz
007700170824     C*
007800170824     C* Caricamento tabella
007900170824     C                   exsr      carTab
008000170519     C*
008100170519     C* Eseguo operazioni
008200170522     C                   exsr      exeOpe
008300170412     C*
008400170412     C* Gestisco eventuale errore
008500170412     C                   on-error
008600170525     C                   eval      wErrIdx = 1
008700170525     C                   eval      wErrMsg = *blanks
008800170525     C                   eval      wErrTag = 'UBS715R'
008900170525     C                   exsr      exeErr
009000170412     C*
009100170412     C* Arresto il monitoring
009200170412     C                   endmon
009300100624     C*
009400171016     C                   return
009500010525     C*
009600010525     C*-----------------------------------------------------------------------------
009700170522
009800170522
009900170522
010000170522     C*
010100170522     C     chkPar        begsr
010200170526     C*
010300170526     C* IN  => Effettuo considerazioni sul ID lingua indicato in input
010400170526     C                   if        iUBS715LIN = *blanks
010500170526     C                   eval      iUBS715LIN = 'IT'
010600170526     C                   else
010700170526     C                   eval      iUBS715LIN =
010800170526     C                             %xlate(Lower:Upper:iUBS715LIN)
010900170526     C                   endif
011000170605     C                   select
011100170605     C                   when      iUBS715LIN = 'IT'
011200170605     C                   when      iUBS715LIN = 'EN'
011300170605     C                   when      iUBS715LIN = 'FR'
011400170605     C                   when      iUBS715LIN = 'DE'
011500170605     C                   when      iUBS715LIN = 'ES'
011600170605     C                   other
011700170605     C                   eval      iUBS715LIN = 'EN'
011800170605     C                   endsl
011900170522     C*
012000170522     C* IN  => Effettuo considerazioni sul tipo dati indicato in input
012100170522     C                   select
012200170522     C                   when      iUBS715TID = *blanks
012300170522     C                   eval      DVPC = iUBS715UNI
012400170522     C                   eval      EDIVABDS = iUBS715DATI
012500170522     C                   other
012600170522     C*
012700170525     C* Se tipo dati non previsto => errore
012800170525     C                   eval      wErrIdx = 2
012900170525     C                   eval      wErrMsg = *blanks
013000170525     C                   eval      wErrTag = 'UBS715R'
013100170522     C                   exsr      exeErr
013200170522     C                   endsl
013300170523     C*
013400170523     C* IN  => Effettuo considerazioni sul ambiente indicato in input
013500170523     C                   select
013600170523     C                   when      iUBS715ENV = 'VUL'                           * VAS UPLOAD
013700170523     C                   eval      wSGU1ADSTO = 'FILE'
013800170523     C                   when      iUBS715ENV = 'VWS'                           * VAS WEB SERVICE
013900170803     C                   eval      wSGU1ADSTO = 'DATI'                            ASCII
014000170803     C                   when      iUBS715ENV = 'VWB'                           * VAS WEB SERVICE
014100170803     C                   eval      wSGU1ADSTO = 'BAS64'                           BASE64
014200170523     C                   other
014300170523     C*
014400171018     C* Se tipo dati non previsto => uscita con errore
014500170525     C                   eval      wErrIdx = 2
014600170525     C                   eval      wErrMsg = *blanks
014700170525     C                   eval      wErrTag = 'UBS715R'
014800170523     C                   exsr      exeErr
014900170523     C                   endsl
015000170522     C*
015100170522     C* OUT => Inizializzo esito ad errore
015200170525     C                   eval      oUBS715ESI = '2'
015300170522     C*
015400170522     C                   endsr
015500170526
015600170526
015700170526
015800170526     C*
015900170526     C     exeInz        begsr
016000170526     C*
016100170526     C* Inizializzazioni varie
016200170605     C                   clear                   wErrIdx
016300170605     C                   clear                   wErrMsg
016400170605     C                   clear                   wErrTag
016500170526     C*
016600170526     C                   endsr
016700170419
016800170419
016900170419
017000170419     C*
017100170419     C     exeOpe        begsr
017200170606     C*
017300170606     C                   setoff                                       55
017400170522     C*
017500170522     C* Se arrivato a questo punto prima del esecuzione vera e propria imposto esito a OK
017600170522     C                   eval      oUBS715ESI = *blanks
017700170420     C*
017800170420     C* Verifico ogni abilitazione presente sul cliente VAS corrente
017900170420     C*
018000170420     C* - se richiesto Calcolo Instradamento Spedizione
018100170420     C                   if        §VPCCIS = 'S'
018200170519     C                   select
018300170522     C                   when      iUBS715OPZ = *blanks
018400170519     C                   exsr      exeCIS_SI95
018500170519     C                   endsl
018600170420     C                   endif
018700170420     C*
018800170420     C* - se richiesto Assegnazione Segnacolli Spedizione
018900170526     C                   if        §VPCASG <> *blanks AND
019000170526     C                             §VPCASG <> 'N'
019100170522     C                   select
019200170522     C                   when      iUBS715OPZ = *blanks
019300170605     C                   exsr      exeASG
019400170522     C                   endsl
019500170420     C                   endif
019600170419     C*
019700170420     C* - se richiesto Creazione Etichette Segnacollo a Cliente
019800170419     C                   if        §VPCSGF <> *blanks
019900170519     C                   select
020000170522     C                   when      iUBS715OPZ = *blanks
020100170606     C  N55              exsr      exeSGF_ETISPE
020200170606     C   55              exsr      exeSGF_ETESPE
020300170523     C                   endsl
020400170419     C                   endif
020500170801     C*
020600170801     C* OUT => Effettuo considerazioni sul tipo dati indicato in input
020700170801     C                   select
020800170801     C                   when      iUBS715TID = *blanks
020900170803     C                   eval      iUBS715DATI = EDIVABDS
021000170801     C                   other
021100170801     C* ---- future opzioni ----
021200170801     C                   endsl
021300170419     C*
021400170419     C                   endsr
021500170526
021600170526
021700170526
021800170526     C*******************************
021900170526     C* Assegnazione Segnacolli Spedizione
022000170526     C*******************************
022100170526     C*
022200170526     C     exeASG        begsr
022300170605     C*
022400170605     C* Eseguo assegnazioe automatica segnacollo
022500170609     C                   eval      iAASGNLIN   = iUBS715LIN
022600170605     C                   eval      iAASGNUNI   = iUBS715UNI
022700170605     C                   eval      iAASGNDSVAB = EDIVABDS
022800170605     C                   call      'UBAASGNR'
022900170609     C                   parm                    iAASGNLIN
023000170605     C                   parm                    iAASGNUNI
023100170605     C                   parm                    iAASGNDSVAB
023200170605     C                   parm                    oAASGNESI
023300170605     C                   parm                    oAASGNMSG
023400170526     C*
023500170605     C* Se in uscita reperimento OK, altrimenti errore
023600170605     C                   if        oAASGNESI <> *zeros
023700170606     C                   seton                                        55
023800170605     C                   eval      wErrIdx = 99
023900170605     C                   eval      wErrMsg = oAASGNMSG
024000170605     C                   eval      wErrTag = 'UBAASGNR'
024100170605     C                   exsr      exeErr
024200170609     C*
024300170609     C* Se tutto OK => recepisco le modifiche ai dati
024400170609     C                   else
024500170609     C                   eval      EDIVABDS = iAASGNDSVAB
024600170605     C                   endif
024700170526     C*
024800170526     C                   endsr
024900170518
025000170522
025100170419
025200170523     C*******************************
025300170606     C* Etichetta SEGNACOLLO - a livello di SPEDIZIONE
025400170523     C*******************************
025500170523     C*
025600170606     C     exeSGF_ETISPE begsr
025700170419     C*
025800170420     C* Inizializzazioni DS di procedure
025900170419     C                   clear                   UBBRTETIDS
026000170420     C                   clear                   UBSGU1IDS
026100170420     C                   clear                   UBSGU1DSA
026200170419     C*
026300170420     C* Valorizzo parametri per stampa segnacollo
026400170419     C                   select
026500170419     C                   when      §VPCSGF = 'P'
026600170419     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
026700170419     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PDF'
026800170419     C                   when      §VPCSGF = 'T'
026900170419     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
027000170419     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TIF'
027100170419     C                   when      §VPCSGF = 'Z'
027200170419     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
027300170419     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
027400171023     C                   when      §VPCSGF = 'L'
027500171023     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
027600171023     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PCL'
027700170419     C                   other
027800170516     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
027900170516     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
028000170419     C                   endsl
028100170419     C*
028200170419     C                   eval      UBBRTETIDS.PIN_TLA   = *blanks
028300170419     C                   eval      UBBRTETIDS.PIN_OFSX  = *zeros
028400170419     C                   eval      UBBRTETIDS.PIN_OFSY  = *zeros
028500170419     C                   eval      UBBRTETIDS.PIN_BORDO = 'N'
028600170419     C                   eval      UBBRTETIDS.PIN_LOGO  = 'S'
028700170829     C                   eval      UBBRTETIDS.PIN_ROW   = 'N'
028800170419     C                   eval      UBBRTETIDS.PIN_TIE   = *blanks
028900170419     C                   eval      UBBRTETIDS.PIN_RST   = *blanks
029000170419     C*
029100170419     C* Valorizzo parametri per driver utilità segnacolli 1 - Formato "A"
029200170419     C                   eval      UBSGU1DSA.ISGU1AOPZ  = *blanks
029300170522     C                   eval      UBSGU1DSA.ISGU1AKSU  = iUBS715KSU
029400170522     C                   eval      UBSGU1DSA.ISGU1AUNI  = iUBS715UNI
029500170523     C                   eval      UBSGU1DSA.ISGU1ADSTO = wSGU1ADSTO
029600170824     C                   eval      UBSGU1DSA.ISGU1ATIPF = dftTIPF_ETI
029700170824     C                   eval      UBSGU1DSA.ISGU1ATIPT = dftTIP_TRASF
029800170419     C                   eval      UBSGU1DSA.ISGU1AMSKF = §VPCSGN
029900170419     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS
030000170419     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS)
030100170419     C*
030200170419     C* Valorizzo parametri per driver utilità segnacolli 1
030300170525     C                   eval      UBSGU1IDS.ISGU1ILIN  = iUBS715LIN
030400170419     C                   eval      UBSGU1IDS.ISGU1IFMT  = 'A'
030500170419     C                   eval      UBSGU1IDS.ISGU1IFLAT = UBSGU1DSA
030600170419     C                   eval      UBSGU1IDS.ISGU1IFLEN = %size(UBSGU1DSA)
030700170419     C                   eval      UBSGU1IDS.ISGU1IBRTE = UBBRTETIDS
030800170419     C                   eval      UBSGU1IDS.ISGU1IBLEN = %size(UBBRTETIDS)
030900170419     C*
031000170419     C* Chiamata al driver stampa e reperimento etichetta segnacolli
031100170419     C                   call      'UBSGU1R'
031200170419     C                   parm                    UBSGU1IDS
031300170804     C                   parm                    UBSGU1OLEN
031400170419     C                   parm                    UBSGU1OUT
031500170518     C*
031600171018     C* Se errore => uscita con errore
031700170518     C                   if        UBSGU1IDS.OSGU1IESI <> *zeros
031800170606     C                   seton                                        55
031900170526     C                   eval      wErrIdx = 99
032000170526     C                   eval      wErrMsg = UBSGU1IDS.OSGU1IMSG
032100170526     C                   eval      wErrTag = 'UBSGU1R'
032200170522     C                   exsr      exeErr
032300170803     C                   else
032400170803     C                   eval      oUBS715DATI = UBSGU1OUT
032500170518     C                   endif
032600170419     C*
032700170419     C                   endsr
032800170525
032900170525
033000170525
033100170525     C*******************************
033200170606     C* Etichetta di ERRORE - a livello di SPEDIZIONE
033300170525     C*******************************
033400170525     C*
033500170606     C     exeSGF_ETESPE begsr
033600170606     C*
033700170606     C* Se richiesto nelle personalizzazioni VPC del cliente genero un etichetta segnacollo
033800170606     C* per evidenziare l'errore occorso
033900171018 xxx C***                if        §VPCSGE = 'E'
034000170525     C*
034100170525     C* Inizializzazioni DS di procedure
034200170525     C                   clear                   UBBRTETIDS
034300170525     C                   clear                   UBSGU1IDS
034400170525     C                   clear                   UBSGU1DSA
034500170525     C*
034600170525     C* Valorizzo parametri per stampa segnacollo
034700171018     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
034800171018     C*
034900170525     C                   select
035000170525     C                   when      §VPCSGF = 'P'
035100170525     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
035200170525     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PDF'
035300170525     C                   when      §VPCSGF = 'T'
035400170525     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
035500170525     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TIF'
035600170525     C                   when      §VPCSGF = 'Z'
035700170525     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
035800170525     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
035900171023     C                   when      §VPCSGF = 'L'
036000171023     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
036100171023     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PCL'
036200170525     C                   other
036300170525     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
036400170525     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
036500170525     C                   endsl
036600170525     C*
036700170525     C                   eval      UBBRTETIDS.PIN_TLA   = *blanks
036800170525     C                   eval      UBBRTETIDS.PIN_OFSX  = *zeros
036900170525     C                   eval      UBBRTETIDS.PIN_OFSY  = *zeros
037000170525     C                   eval      UBBRTETIDS.PIN_BORDO = 'N'
037100170525     C                   eval      UBBRTETIDS.PIN_LOGO  = 'S'
037200170829     C                   eval      UBBRTETIDS.PIN_ROW   = 'N'
037300170525     C                   eval      UBBRTETIDS.PIN_TIE   = *blanks
037400170525     C                   eval      UBBRTETIDS.PIN_RST   = *blanks
037500170526     C*
037600170526     C* Traduco in lingua il messaggio di ERRORE da stampare
037700170526     C                   clear                   UBBRTETI_MSG
037800170526     C                   select
037900171018     C                   when      iUBS715LIN = 'IT' AND wErrIdx<=%elem(MSGFLG)
038000170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGITA(wErrIdx)
038100171018     C                   when      iUBS715LIN = 'EN' AND wErrIdx<=%elem(MSGFLG)
038200170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGENG(wErrIdx)
038300171018     C                   when      iUBS715LIN = 'FR' AND wErrIdx<=%elem(MSGFLG)
038400170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGFRA(wErrIdx)
038500171018     C                   when      iUBS715LIN = 'DE' AND wErrIdx<=%elem(MSGFLG)
038600170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGDEU(wErrIdx)
038700171018     C                   when      iUBS715LIN = 'ES' AND wErrIdx<=%elem(MSGFLG)
038800170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGESP(wErrIdx)
038900170526     C                   other
039000171018     C                   eval      UBBRTETI_MSG.MESSAGE_02 = oUBS715MSG
039100170526     C                   endsl
039200170525     C*
039300170526     C* Valorizzo i parametri per generare etichetta di ERRORE
039400170525     C                   eval      UBBRTETIDS.PIN_SEGERR = 'S'
039500170526     C                   eval      UBBRTETIDS.PIN_MSGERT = 'ERROR'
039600171018     C                   select
039700171018     C                   when      vabRMN  > *zeros
039800171018     C                   eval      UBBRTETI_MSG.SUBTITLE01=
039900171018     C                                      %trim(vabRSD)+' - '+%trim(vabCAD)
040000171018     C                   eval      UBBRTETI_MSG.SUBTITLE02=
040100170526     C                                           'REF: '+%editc(vabRMN:'4')
040200171018     C                   when      vabRMA <> *blanks
040300171018     C                   eval      UBBRTETI_MSG.SUBTITLE01=
040400171018     C                                      %trim(vabRSD)+' - '+%trim(vabCAD)
040500170526     C                   eval      UBBRTETI_MSG.SUBTITLE02=
040600170526     C                                           'REF: '+%trim(vabRMA)
040700171018     C                   endsl
040800171018     C                   if        vabNCD > *zeros OR vabNCA > *zeros
040900170526     C                   eval      UBBRTETI_MSG.SUBTITLE03=
041000170526     C                               'NCD: '+%editc(vabNCD:'4')+'   '+
041100170526     C                               'NCA: '+%editc(vabNCA:'4')
041200171018     C                   endif
041300170526     C*
041400170526     C                   eval      UBBRTETIDS.PIN_MSGERR = UBBRTETI_MSG
041500171018     C*
041600171018     C* Valorizzo parametri per driver utilità segnacolli 1 - Formato "A"
041700171018     C                   eval      UBSGU1DSA.ISGU1AOPZ  = *blanks
041800171018     C                   eval      UBSGU1DSA.ISGU1AKSU  = iUBS715KSU
041900171018     C                   eval      UBSGU1DSA.ISGU1AUNI  = iUBS715UNI
042000171018     C                   eval      UBSGU1DSA.ISGU1ADSTO = wSGU1ADSTO
042100171018     C                   eval      UBSGU1DSA.ISGU1ATIPF = dftTIPF_ETI
042200171018     C                   eval      UBSGU1DSA.ISGU1ATIPT = dftTIP_TRASF
042300171018     C                   eval      UBSGU1DSA.ISGU1AMSKF = §VPCSGN
042400171018     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS
042500171018     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS)
042600171018     C*
042700171018     C* Valorizzo parametri per driver utilità segnacolli 1
042800171018     C                   eval      UBSGU1IDS.ISGU1ILIN  = iUBS715LIN
042900171018     C                   eval      UBSGU1IDS.ISGU1IFMT  = 'A'
043000171018     C                   eval      UBSGU1IDS.ISGU1IFLAT = UBSGU1DSA
043100171018     C                   eval      UBSGU1IDS.ISGU1IFLEN = %size(UBSGU1DSA)
043200171018     C                   eval      UBSGU1IDS.ISGU1IBRTE = UBBRTETIDS
043300171018     C                   eval      UBSGU1IDS.ISGU1IBLEN = %size(UBBRTETIDS)
043400170525     C*
043500170525     C* Chiamata al driver stampa e reperimento etichetta segnacolli
043600170525     C                   call      'UBSGU1R'
043700170525     C                   parm                    UBSGU1IDS
043800170804     C                   parm                    UBSGU1OLEN
043900170525     C                   parm                    UBSGU1OUT
044000170525     C*
044100170525     C* Se errore => segnalazione
044200170525     C                   if        UBSGU1IDS.OSGU1IESI <> *zeros
044300170525     C                   eval      wErrIdx = *zeros
044400170525     C                   eval      wErrMsg = UBSGU1IDS.OSGU1IMSG
044500170525     C                   eval      wErrTag = 'UBSGU1R'
044600170525     C                   exsr      exeErr
044700170525     C                   endif
044800170525     C*
044900171018 xxx C***                endif
045000170606     C*
045100170525     C                   endsr
045200170516
045300170522
045400170516
045500170516     C*
045600170518     C     exeCIS_SI95   begsr
045700170516     C*
045800170516     C                   clear                   UBSI95DS
045900170525     C                   eval      ISI95LIN = iUBS715LIN
046000170522     C                   eval      ISI95TYP = 'F'                               C / V / F / *blnk
046100170522     C                   eval      ISI95CFD = §VPCCFD                           S / *blnk
046200170522     C                   eval      ISI95TLL = §VPCTLL                           S / *blnk
046300170522     C                   eval      ISI95IED = §VPCIED                           S / *blnk
046400170522     C                   eval      ISI95DAT = *zeros
046500170516     C*
046600170516     C                   clear                   RTNsi95esito
046700170516     C                   clear                   RTNsi95opcode
046800170516     C                   clear                   RTNsi95status
046900170516     C*
047000170516     C* Chiamata al driver di Calcolo Instradamento
047100170516     C                   call      'UBSI95R'
047200170516     C                   parm                    UBSI95DS
047300170522     C                   parm                    EDIVABDS
047400170516     C                   parm                    RTNsi95esito
047500170516     C                   parm                    RTNsi95opcode
047600170516     C                   parm                    RTNsi95status
047700170516     C*
047800170516     C* Verifico esito chiamata
047900171018     C                   eval      oUBS715SI95 = UBSI95DS
048000170525     C                   if        RTNsi95esito = 0 AND
048100170525     C                             OSI95LNA > *zeros
048200170522     C                   eval      vabLNA = OSI95LNA
048300170522     C                   eval      vabZNC = OSI95ZNC
048400170518     C                   else
048500171018     C*
048600171018     C* Se errore in calcolo instradamento => gestisco
048700171018     C                   seton                                        55
048800170526     C                   eval      wErrIdx = 99
048900170525     C                   eval      wErrMsg = OSI95MSG
049000170525     C                   eval      wErrTag = 'UBSI95R'
049100170525     C                   exsr      exeErr
049200170516     C                   endif
049300170516     C*
049400170516     C                   endsr
049500170522
049600170522
049700170522
049800170522     C*
049900170522     C     exeErr        begsr
050000170526     C*
050100170526     C* Inizializzo enanling flags del messaggio
050200170526     C                   clear                   MSGFLG_DS
050300170525     C*
050400170525     C* Traduco il messaggio di errore da ritornare
050500170525     C                   select
050600170526     C                   when      wErrIdx = 99 AND
050700170526     C                             wErrMsg <> *blanks
050800171018     C                   if        oUBS715MSG = *blanks
050900170526     C                   eval      oUBS715MSG = wErrMsg
051000171018     C                   endif
051100171018     C***                eval      MSGFLG_DS.FatalError = *on
051200170526     C*
051300170525     C                   when      wErrMsg <> *blanks
051400170525     C                   eval      oUBS715MSG = wErrMsg
051500170525     C*
051600170526     C                   when      wErrIdx  > *zeros AND
051700170526     C                             wErrIdx <= %elem(MSGFLG)
051800170526     C                   eval      MSGFLG_DS = MSGFLG(wErrIdx)
051900170526     C                   select
052000170525     C                   when      iUBS715LIN = 'IT'
052100170525     C                   eval      oUBS715MSG = MSGITA(wErrIdx)
052200170525     C                   when      iUBS715LIN = 'EN'
052300170525     C                   eval      oUBS715MSG = MSGENG(wErrIdx)
052400170525     C                   when      iUBS715LIN = 'FR'
052500170525     C                   eval      oUBS715MSG = MSGFRA(wErrIdx)
052600170525     C                   when      iUBS715LIN = 'DE'
052700170525     C                   eval      oUBS715MSG = MSGDEU(wErrIdx)
052800170525     C                   when      iUBS715LIN = 'ES'
052900170525     C                   eval      oUBS715MSG = MSGESP(wErrIdx)
053000170525     C                   other
053100170525     C                   eval      oUBS715MSG = MSGENG(wErrIdx)
053200170525     C                   endsl
053300170525     C*
053400170525     C                   other
053500170525     C                   eval      oUBS715MSG = *blanks
053600170525     C                   endsl
053700170522     C*
053800170525     C* Se opportuno invio email con dettaglio errore occorso
053900170526     C                   if        MSGFLG_DS.InviaEmail = *on
054000170525     C                   exsr      sndAlert
054100170525     C                   endif
054200170525     C*
054300170526     C* Se errore "locale" valorizzo esito a KO ed esco
054400170526     C                   if        wErrTag = 'UBS715R' OR
054500170526     C                             MSGFLG_DS.FatalError = *on
054600170525     C                   eval      oUBS715ESI = '2'
054700170526     C                   return
054800170525     C                   endif
054900170522     C*
055000170522     C                   endsr
055100170522
055200170526
055300170522
055400170522     C     sndAlert      BEGSR
055500170522     C*
055600170522     C* Inizializzo variabili
055700170522     C                   movel     *blanks       wrkEml          253
055800170522     C                   movel     *blanks       wrkCcEml        253
055900170522     C                   movel     *blanks       wrkEmlMsg      5000
056000170522     C                   movel     *blanks       wrkEmlOgg        44
056100170522     C*
056200170522     C* Valorizzo i campi della e-m@ail
056300170522     C                   eval      wrkEml='cedalert@brt.it'
056400170522     C                   eval      wrkEmlOgg=
056500170522     C                               'VAS - Errore in esecuzione UBS715R'
056600170522     C                   EVAL      wrkEmlMsg = 'Errore in esecuzione UBS715R: '+
056700170522     C                                                                    ':/N'+
056800170522     C                                                                    ':/N'+
056900170525     C                             %trim(wErrTag)                              +
057000170522     C                                                                    ':/N'+
057100170522     C                                                                    ':/N'+
057200170525     C                             %trim(oUBS715MSG)                           +
057300170525     C                                                                    ':/N'+
057400170525     C                                                                    ':/N'+
057500171018     C                             'Verificare il LOG del job:  '              +
057600170522     C                             %editc(psjobnbr:'X')+'/'                    +
057700170522     C                             %trim(psuser)+'/'                           +
057800170522     C                             %trim(psjobname)                      +':/N'+
057900170522     C                             x'00'
058000170522     C*
058100170522     C                   call(e)   'TIS701C1'
058200170522     C                   parm                    wrkEml
058300170522     C                   parm                    wrkCcEml
058400170522     C                   parm                    wrkEmlOgg
058500170522     C                   parm                    wrkEmlMsg
058600170522     C                   parm                    wrkEsito          1
058700170522     C*
058800170522     C                   ENDSR
058900170824
059000170824
059100170824
059200170824     C*
059300170824     C     carTab        begsr
059400170824     C*
059500170824     C* Reperimento TIPO FILE ETICHETTE SEGNACOLLO di DEFAULT "VASWS"
059600170824     C                   eval      iS713TBECOD = 'VPR'
059700170824     C                   eval      iS713TBEKE1 = '*VASWS'
059800170824     C                   eval      iS713TBEKE2 = '*TIPF-ETI'
059900170824     C                   eval      iS713TBELIN = *blanks
060000170824     C                   eval      iS713TBESIF = *blanks
060100170824     C                   call      'TIS713R'
060200170824     C                   parm                    iS713TBECOD
060300170824     C                   parm                    iS713TBEKE1
060400170824     C                   parm                    iS713TBEKE2
060500170824     C                   parm                    iS713TBELIN
060600170824     C                   parm                    iS713TBESIF
060700170824     C                   parm                    oS713TBEUNI
060800170824     C                   parm                    oS713ESITO
060900170824     C*
061000170824     C                   if        oS713ESITO = '1'
061100170824     C                   eval      dftTIPF_ETI = %trim(oS713TBEUNI)
061200170824     C                   else
061300170824     C                   eval      dftTIPF_ETI = 'SG'
061400170824     C                   endif
061500170824     C*
061600170824     C* Reperimento TIPO TRASFERIMENTO OUTPUT di DEFAULT "VASWS"
061700170824     C                   eval      iS713TBECOD = 'VPR'
061800170824     C                   eval      iS713TBEKE1 = '*VASWS'
061900170824     C                   eval      iS713TBEKE2 = '*TIP-TRASF'
062000170824     C                   eval      iS713TBELIN = *blanks
062100170824     C                   eval      iS713TBESIF = *blanks
062200170824     C                   call      'TIS713R'
062300170824     C                   parm                    iS713TBECOD
062400170824     C                   parm                    iS713TBEKE1
062500170824     C                   parm                    iS713TBEKE2
062600170824     C                   parm                    iS713TBELIN
062700170824     C                   parm                    iS713TBESIF
062800170824     C                   parm                    oS713TBEUNI
062900170824     C                   parm                    oS713ESITO
063000170824     C*
063100170824     C                   if        oS713ESITO = '1'
063200170824     C                   eval      dftTIP_TRASF = %trim(oS713TBEUNI)
063300170824     C                   else
063400170824     C                   eval      dftTIP_TRASF = 'BV'
063500170824     C                   endif
063600170824     C*
063700170824     C                   endsr
063800170824     C*
063900000714
064000050920
064100050920
064200000714      /TITLE Routine di *init PGM
064300000714     C*
064400000714     C     *inzsr        begsr
064500060103     C*
064600060103     C     *entry        plist
064700170525     C                   parm                    iUBS715LIN
064800170522     C                   parm                    iUBS715TID
064900170523     C                   parm                    iUBS715ENV
065000170522     C                   parm                    iUBS715OPZ
065100170522     C                   parm                    iUBS715KSU
065200170522     C                   parm                    iUBS715UNI
065300170522     C                   parm                    iUBS715DATI
065400170522     C                   parm                    oUBS715ESI
065500170525     C                   parm                    oUBS715MSG
065600170803     C                   parm                    oUBS715SI95
065700170523     C                   parm                    oUBS715DATI
065800000714     C*
065900000714     C                   endsr
066000170525** MSGFLG - FLAG MESSAGGI
06610017052611
06620017052611
06630017052611
06640017052611
06650017052611
066600170530
066700170525
066800170525
066900170525
067000170525
067100170525** MSGITA - MESSAGGI
067200170525Errore generico
067300170525Parametro non valido
067400170526Segnacollo errato o non consentito
067500170526Segnacollo già utilizzato
067600170526Impossibile prelevare un nuovo ID segnacollo
067700170525
067800170525
067900170525
068000170525
068100170525
068200170525** MSGENG - MESSAGGI
068300170525Generic error
068400170525Invalid parameter
068500170526Parcel number invalid or not allowed
068600170525Parcel number already consumed
068700170526Unable to raise new Parcel number
068800170525
068900170525
069000170525
069100170525
069200170525
069300170525** MSGFRA - MESSAGGI
069400170525Generic error
069500170525Invalid parameter
069600170526Parcel number invalid or not allowed
069700170525Parcel number already consumed
069800170526Unable to raise new Parcel number
069900170525
070000170525
070100170525
070200170525
070300170525
070400170525** MSGDEU - MESSAGGI
070500170525Generic error
070600170525Invalid parameter
070700170526Parcel number invalid or not allowed
070800170525Parcel number already consumed
070900170526Unable to raise new Parcel number
071000170525
071100170525
071200170525
071300170525
071400170525
071500170525** MSGESP - MESSAGGI
071600170525Generic error
071700170525Invalid parameter
071800170526Parcel number invalid or not allowed
071900170525Parcel number already consumed
072000170526Unable to raise new Parcel number
072100170525
072200170525
072300170525
072400170525
072500170525
