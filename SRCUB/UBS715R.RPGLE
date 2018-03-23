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
004100180313     D wTMA            s              3S 0 inz
004200170516
004300170526     D UBBRTETI_MSG    ds                  inz qualified
004400170523     D   SUBTITLE01                  20
004500170523     D   SUBTITLE02                  20
004600170523     D   SUBTITLE03                  20
004700170523     D   MESSAGE_01                  40
004800170523     D   MESSAGE_02                  40
004900170523     D   MESSAGE_03                  40
005000170523     D   MESSAGE_04                  40
005100170525
005200170526     D*
005300170526     D MSGFLG          S             20    DIM(10) CTDATA PERRCD(1)
005400170525     D MSGITA          S            100    DIM(10) CTDATA PERRCD(1)
005500170525     D MSGENG          S            100    DIM(10) CTDATA PERRCD(1)
005600170525     D MSGFRA          S            100    DIM(10) CTDATA PERRCD(1)
005700170525     D MSGDEU          S            100    DIM(10) CTDATA PERRCD(1)
005800170525     D MSGESP          S            100    DIM(10) CTDATA PERRCD(1)
005900170525
006000170526     D MSGFLG_DS       ds                  qualified
006100170525     D   InviaEmail                   1    inz('1')
006200170525     D   FatalError                   1    inz('0')
006300170526
006400170526     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
006500170526     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
006600010525
006700010525
006800010525     C*-----------------------------------------------------------------------------
006900170412     C*
007000170412     C* Avvio il monitoring del intero flusso
007100170412     C                   monitor
007200170522     C*
007300170522     C* Verifica parametri
007400170522     C                   exsr      chkPar
007500170525     C*
007600170526     C* Inizializzazione di work
007700170526     C                   exsr      exeInz
007800170824     C*
007900170824     C* Caricamento tabella
008000170824     C                   exsr      carTab
008100170519     C*
008200170519     C* Eseguo operazioni
008300170522     C                   exsr      exeOpe
008400170412     C*
008500170412     C* Gestisco eventuale errore
008600170412     C                   on-error
008700170525     C                   eval      wErrIdx = 1
008800170525     C                   eval      wErrMsg = *blanks
008900170525     C                   eval      wErrTag = 'UBS715R'
009000170525     C                   exsr      exeErr
009100170412     C*
009200170412     C* Arresto il monitoring
009300170412     C                   endmon
009400100624     C*
009500171016     C                   return
009600010525     C*
009700010525     C*-----------------------------------------------------------------------------
009800170522
009900170522
010000170522
010100170522     C*
010200170522     C     chkPar        begsr
010300170526     C*
010400170526     C* IN  => Effettuo considerazioni sul ID lingua indicato in input
010500170526     C                   if        iUBS715LIN = *blanks
010600170526     C                   eval      iUBS715LIN = 'IT'
010700170526     C                   else
010800170526     C                   eval      iUBS715LIN =
010900170526     C                             %xlate(Lower:Upper:iUBS715LIN)
011000170526     C                   endif
011100170605     C                   select
011200170605     C                   when      iUBS715LIN = 'IT'
011300170605     C                   when      iUBS715LIN = 'EN'
011400170605     C                   when      iUBS715LIN = 'FR'
011500170605     C                   when      iUBS715LIN = 'DE'
011600170605     C                   when      iUBS715LIN = 'ES'
011700170605     C                   other
011800170605     C                   eval      iUBS715LIN = 'EN'
011900170605     C                   endsl
012000170522     C*
012100170522     C* IN  => Effettuo considerazioni sul tipo dati indicato in input
012200170522     C                   select
012300170522     C                   when      iUBS715TID = *blanks
012400170522     C                   eval      DVPC = iUBS715UNI
012500170522     C                   eval      EDIVABDS = iUBS715DATI
012600180313     C* se è stata chiesta una maschera file etichette col Terminal di arrivo,
012700180313     C                   when      iUBS715TID  = '1'
012800180313     C                   eval      DVPC = iUBS715UNI
012900180313     C* ai dati è stato aggiunto il terminal
013000180313     C                   eval      EDIVABDS = %subst(iUBS715DATI : 1 :
013100180313     C                                        %len(EDIVABDS))
013200180313     C                   eval      wTMA = %dec(%subst(iUBS715DATI
013300180313     C                                    : %len(EDIVABDS)+1
013400180313     C                                    : 3) : 3 : 0)
013500170522     C                   other
013600170522     C*
013700170525     C* Se tipo dati non previsto => errore
013800170525     C                   eval      wErrIdx = 2
013900170525     C                   eval      wErrMsg = *blanks
014000170525     C                   eval      wErrTag = 'UBS715R'
014100170522     C                   exsr      exeErr
014200170522     C                   endsl
014300170523     C*
014400170523     C* IN  => Effettuo considerazioni sul ambiente indicato in input
014500170523     C                   select
014600170523     C                   when      iUBS715ENV = 'VUL'                           * VAS UPLOAD
014700170523     C                   eval      wSGU1ADSTO = 'FILE'
014800170523     C                   when      iUBS715ENV = 'VWS'                           * VAS WEB SERVICE
014900170803     C                   eval      wSGU1ADSTO = 'DATI'                            ASCII
015000170803     C                   when      iUBS715ENV = 'VWB'                           * VAS WEB SERVICE
015100170803     C                   eval      wSGU1ADSTO = 'BAS64'                           BASE64
015200170523     C                   other
015300170523     C*
015400171018     C* Se tipo dati non previsto => uscita con errore
015500170525     C                   eval      wErrIdx = 2
015600170525     C                   eval      wErrMsg = *blanks
015700170525     C                   eval      wErrTag = 'UBS715R'
015800170523     C                   exsr      exeErr
015900170523     C                   endsl
016000170522     C*
016100170522     C* OUT => Inizializzo esito ad errore
016200170525     C                   eval      oUBS715ESI = '2'
016300170522     C*
016400170522     C                   endsr
016500170526
016600170526
016700170526
016800170526     C*
016900170526     C     exeInz        begsr
017000170526     C*
017100170526     C* Inizializzazioni varie
017200170605     C                   clear                   wErrIdx
017300170605     C                   clear                   wErrMsg
017400170605     C                   clear                   wErrTag
017500170526     C*
017600170526     C                   endsr
017700170419
017800170419
017900170419
018000170419     C*
018100170419     C     exeOpe        begsr
018200170606     C*
018300170606     C                   setoff                                       55
018400170522     C*
018500170522     C* Se arrivato a questo punto prima del esecuzione vera e propria imposto esito a OK
018600170522     C                   eval      oUBS715ESI = *blanks
018700170420     C*
018800170420     C* Verifico ogni abilitazione presente sul cliente VAS corrente
018900170420     C*
019000170420     C* - se richiesto Calcolo Instradamento Spedizione
019100170420     C                   if        §VPCCIS = 'S'
019200170519     C                   select
019300170522     C                   when      iUBS715OPZ = *blanks
019400170519     C                   exsr      exeCIS_SI95
019500170519     C                   endsl
019600170420     C                   endif
019700170420     C*
019800170420     C* - se richiesto Assegnazione Segnacolli Spedizione
019900170526     C                   if        §VPCASG <> *blanks AND
020000170526     C                             §VPCASG <> 'N'
020100170522     C                   select
020200170522     C                   when      iUBS715OPZ = *blanks
020300170605     C                   exsr      exeASG
020400170522     C                   endsl
020500170420     C                   endif
020600170419     C*
020700170420     C* - se richiesto Creazione Etichette Segnacollo a Cliente
020800170419     C                   if        §VPCSGF <> *blanks
020900170519     C                   select
021000170522     C                   when      iUBS715OPZ = *blanks
021100170606     C  N55              exsr      exeSGF_ETISPE
021200170606     C   55              exsr      exeSGF_ETESPE
021300170523     C                   endsl
021400170419     C                   endif
021500170801     C*
021600170801     C* OUT => Effettuo considerazioni sul tipo dati indicato in input
021700170801     C                   select
021800170801     C                   when      iUBS715TID = *blanks
021900170803     C                   eval      iUBS715DATI = EDIVABDS
022000180313     C                   when      iUBS715TID = '1'
022100180313     C                   eval      iUBS715DATI = EDIVABDS + %editc(wTMA:'X')
022200170801     C                   other
022300170801     C* ---- future opzioni ----
022400170801     C                   endsl
022500170419     C*
022600170419     C                   endsr
022700170526
022800170526
022900170526
023000170526     C*******************************
023100170526     C* Assegnazione Segnacolli Spedizione
023200170526     C*******************************
023300170526     C*
023400170526     C     exeASG        begsr
023500170605     C*
023600170605     C* Eseguo assegnazioe automatica segnacollo
023700170609     C                   eval      iAASGNLIN   = iUBS715LIN
023800170605     C                   eval      iAASGNUNI   = iUBS715UNI
023900170605     C                   eval      iAASGNDSVAB = EDIVABDS
024000170605     C                   call      'UBAASGNR'
024100170609     C                   parm                    iAASGNLIN
024200170605     C                   parm                    iAASGNUNI
024300170605     C                   parm                    iAASGNDSVAB
024400170605     C                   parm                    oAASGNESI
024500170605     C                   parm                    oAASGNMSG
024600170526     C*
024700170605     C* Se in uscita reperimento OK, altrimenti errore
024800170605     C                   if        oAASGNESI <> *zeros
024900170606     C                   seton                                        55
025000170605     C                   eval      wErrIdx = 99
025100170605     C                   eval      wErrMsg = oAASGNMSG
025200170605     C                   eval      wErrTag = 'UBAASGNR'
025300170605     C                   exsr      exeErr
025400170609     C*
025500170609     C* Se tutto OK => recepisco le modifiche ai dati
025600170609     C                   else
025700170609     C                   eval      EDIVABDS = iAASGNDSVAB
025800170605     C                   endif
025900170526     C*
026000170526     C                   endsr
026100170518
026200170522
026300170419
026400170523     C*******************************
026500170606     C* Etichetta SEGNACOLLO - a livello di SPEDIZIONE
026600170523     C*******************************
026700170523     C*
026800170606     C     exeSGF_ETISPE begsr
026900170419     C*
027000170420     C* Inizializzazioni DS di procedure
027100170419     C                   clear                   UBBRTETIDS
027200170420     C                   clear                   UBSGU1IDS
027300170420     C                   clear                   UBSGU1DSA
027400170419     C*
027500170420     C* Valorizzo parametri per stampa segnacollo
027600170419     C                   select
027700170419     C                   when      §VPCSGF = 'P'
027800170419     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
027900170419     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PDF'
028000170419     C                   when      §VPCSGF = 'T'
028100170419     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
028200170419     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TIF'
028300170419     C                   when      §VPCSGF = 'Z'
028400170419     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
028500170419     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
028600171023     C                   when      §VPCSGF = 'L'
028700171023     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
028800171023     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PCL'
028900170419     C                   other
029000170516     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
029100170516     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
029200170419     C                   endsl
029300170419     C*
029400170419     C                   eval      UBBRTETIDS.PIN_TLA   = *blanks
029500170419     C                   eval      UBBRTETIDS.PIN_OFSX  = *zeros
029600170419     C                   eval      UBBRTETIDS.PIN_OFSY  = *zeros
029700170419     C                   eval      UBBRTETIDS.PIN_BORDO = 'N'
029800170419     C                   eval      UBBRTETIDS.PIN_LOGO  = 'S'
029900170829     C                   eval      UBBRTETIDS.PIN_ROW   = 'N'
030000170419     C                   eval      UBBRTETIDS.PIN_TIE   = *blanks
030100170419     C                   eval      UBBRTETIDS.PIN_RST   = *blanks
030200180313     C* se valorizzato il Terminal di arrivo, lo imposto già (altrimenti lo reperisce il pgm
030300180313     C* chiamato)
030400180313     C                   if        wTMA <> 0
030500180313     C                   eval      UBBRTETIDS.PIN_TMA   = wTMA
030600180313     C                   endif
030700170419     C*
030800170419     C* Valorizzo parametri per driver utilità segnacolli 1 - Formato "A"
030900170419     C                   eval      UBSGU1DSA.ISGU1AOPZ  = *blanks
031000170522     C                   eval      UBSGU1DSA.ISGU1AKSU  = iUBS715KSU
031100170522     C                   eval      UBSGU1DSA.ISGU1AUNI  = iUBS715UNI
031200170523     C                   eval      UBSGU1DSA.ISGU1ADSTO = wSGU1ADSTO
031300170824     C                   eval      UBSGU1DSA.ISGU1ATIPF = dftTIPF_ETI
031400170824     C                   eval      UBSGU1DSA.ISGU1ATIPT = dftTIP_TRASF
031500170419     C                   eval      UBSGU1DSA.ISGU1AMSKF = §VPCSGN
031600170419     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS
031700170419     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS)
031800180313
031900180313     C                   select
032000180313     C* se è stata chiesta una maschera file etichette col Terminal di arrivo,
032100180313     C                   when      iUBS715TID  = '1'
032200180313     C* ai dati va aggiunto il terminal
032300180313     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS +
032400180313     C                                                    %editc(wTMA:'X')
032500180313     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS) +
032600180313     C                                                    %size(wTMA)
032700180313     C                   endsl
032800170419     C*
032900170419     C* Valorizzo parametri per driver utilità segnacolli 1
033000170525     C                   eval      UBSGU1IDS.ISGU1ILIN  = iUBS715LIN
033100170419     C                   eval      UBSGU1IDS.ISGU1IFMT  = 'A'
033200170419     C                   eval      UBSGU1IDS.ISGU1IFLAT = UBSGU1DSA
033300170419     C                   eval      UBSGU1IDS.ISGU1IFLEN = %size(UBSGU1DSA)
033400170419     C                   eval      UBSGU1IDS.ISGU1IBRTE = UBBRTETIDS
033500170419     C                   eval      UBSGU1IDS.ISGU1IBLEN = %size(UBBRTETIDS)
033600170419     C*
033700170419     C* Chiamata al driver stampa e reperimento etichetta segnacolli
033800170419     C                   call      'UBSGU1R'
033900170419     C                   parm                    UBSGU1IDS
034000170804     C                   parm                    UBSGU1OLEN
034100170419     C                   parm                    UBSGU1OUT
034200170518     C*
034300171018     C* Se errore => uscita con errore
034400170518     C                   if        UBSGU1IDS.OSGU1IESI <> *zeros
034500170606     C                   seton                                        55
034600170526     C                   eval      wErrIdx = 99
034700170526     C                   eval      wErrMsg = UBSGU1IDS.OSGU1IMSG
034800170526     C                   eval      wErrTag = 'UBSGU1R'
034900170522     C                   exsr      exeErr
035000170803     C                   else
035100170803     C                   eval      oUBS715DATI = UBSGU1OUT
035200170518     C                   endif
035300170419     C*
035400170419     C                   endsr
035500170525
035600170525
035700170525
035800170525     C*******************************
035900170606     C* Etichetta di ERRORE - a livello di SPEDIZIONE
036000170525     C*******************************
036100170525     C*
036200170606     C     exeSGF_ETESPE begsr
036300170606     C*
036400170606     C* Se richiesto nelle personalizzazioni VPC del cliente genero un etichetta segnacollo
036500170606     C* per evidenziare l'errore occorso
036600171018 xxx C***                if        §VPCSGE = 'E'
036700170525     C*
036800170525     C* Inizializzazioni DS di procedure
036900170525     C                   clear                   UBBRTETIDS
037000170525     C                   clear                   UBSGU1IDS
037100170525     C                   clear                   UBSGU1DSA
037200170525     C*
037300170525     C* Valorizzo parametri per stampa segnacollo
037400171018     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
037500171018     C*
037600170525     C                   select
037700170525     C                   when      §VPCSGF = 'P'
037800170525     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
037900170525     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PDF'
038000170525     C                   when      §VPCSGF = 'T'
038100170525     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
038200170525     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TIF'
038300170525     C                   when      §VPCSGF = 'Z'
038400170525     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
038500170525     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
038600171023     C                   when      §VPCSGF = 'L'
038700171023     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
038800171023     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PCL'
038900170525     C                   other
039000170525     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
039100170525     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
039200170525     C                   endsl
039300170525     C*
039400170525     C                   eval      UBBRTETIDS.PIN_TLA   = *blanks
039500170525     C                   eval      UBBRTETIDS.PIN_OFSX  = *zeros
039600170525     C                   eval      UBBRTETIDS.PIN_OFSY  = *zeros
039700170525     C                   eval      UBBRTETIDS.PIN_BORDO = 'N'
039800170525     C                   eval      UBBRTETIDS.PIN_LOGO  = 'S'
039900170829     C                   eval      UBBRTETIDS.PIN_ROW   = 'N'
040000170525     C                   eval      UBBRTETIDS.PIN_TIE   = *blanks
040100170525     C                   eval      UBBRTETIDS.PIN_RST   = *blanks
040200170526     C*
040300170526     C* Traduco in lingua il messaggio di ERRORE da stampare
040400170526     C                   clear                   UBBRTETI_MSG
040500170526     C                   select
040600171018     C                   when      iUBS715LIN = 'IT' AND wErrIdx<=%elem(MSGFLG)
040700170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGITA(wErrIdx)
040800171018     C                   when      iUBS715LIN = 'EN' AND wErrIdx<=%elem(MSGFLG)
040900170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGENG(wErrIdx)
041000171018     C                   when      iUBS715LIN = 'FR' AND wErrIdx<=%elem(MSGFLG)
041100170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGFRA(wErrIdx)
041200171018     C                   when      iUBS715LIN = 'DE' AND wErrIdx<=%elem(MSGFLG)
041300170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGDEU(wErrIdx)
041400171018     C                   when      iUBS715LIN = 'ES' AND wErrIdx<=%elem(MSGFLG)
041500170526     C                   eval      UBBRTETI_MSG.MESSAGE_02 = MSGESP(wErrIdx)
041600170526     C                   other
041700171018     C                   eval      UBBRTETI_MSG.MESSAGE_02 = oUBS715MSG
041800170526     C                   endsl
041900170525     C*
042000170526     C* Valorizzo i parametri per generare etichetta di ERRORE
042100170525     C                   eval      UBBRTETIDS.PIN_SEGERR = 'S'
042200170526     C                   eval      UBBRTETIDS.PIN_MSGERT = 'ERROR'
042300171018     C                   select
042400171018     C                   when      vabRMN  > *zeros
042500171018     C                   eval      UBBRTETI_MSG.SUBTITLE01=
042600171018     C                                      %trim(vabRSD)+' - '+%trim(vabCAD)
042700171018     C                   eval      UBBRTETI_MSG.SUBTITLE02=
042800170526     C                                           'REF: '+%editc(vabRMN:'4')
042900171018     C                   when      vabRMA <> *blanks
043000171018     C                   eval      UBBRTETI_MSG.SUBTITLE01=
043100171018     C                                      %trim(vabRSD)+' - '+%trim(vabCAD)
043200170526     C                   eval      UBBRTETI_MSG.SUBTITLE02=
043300170526     C                                           'REF: '+%trim(vabRMA)
043400171018     C                   endsl
043500171018     C                   if        vabNCD > *zeros OR vabNCA > *zeros
043600170526     C                   eval      UBBRTETI_MSG.SUBTITLE03=
043700170526     C                               'NCD: '+%editc(vabNCD:'4')+'   '+
043800170526     C                               'NCA: '+%editc(vabNCA:'4')
043900171018     C                   endif
044000170526     C*
044100170526     C                   eval      UBBRTETIDS.PIN_MSGERR = UBBRTETI_MSG
044200171018     C*
044300171018     C* Valorizzo parametri per driver utilità segnacolli 1 - Formato "A"
044400171018     C                   eval      UBSGU1DSA.ISGU1AOPZ  = *blanks
044500171018     C                   eval      UBSGU1DSA.ISGU1AKSU  = iUBS715KSU
044600171018     C                   eval      UBSGU1DSA.ISGU1AUNI  = iUBS715UNI
044700171018     C                   eval      UBSGU1DSA.ISGU1ADSTO = wSGU1ADSTO
044800171018     C                   eval      UBSGU1DSA.ISGU1ATIPF = dftTIPF_ETI
044900171018     C                   eval      UBSGU1DSA.ISGU1ATIPT = dftTIP_TRASF
045000171018     C                   eval      UBSGU1DSA.ISGU1AMSKF = §VPCSGN
045100171018     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS
045200171018     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS)
045300180313
045400180313     C                   select
045500180313     C* se è stata chiesta una maschera file etichette col Terminal di arrivo,
045600180313     C                   when      iUBS715TID  = '1'
045700180313     C* ai dati va aggiunto il terminal
045800180313     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS +
045900180313     C                                                    %editc(wTMA:'X')
046000180313     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS) +
046100180313     C                                                    %size(wTMA)
046200180313     C                   endsl
046300171018     C*
046400171018     C* Valorizzo parametri per driver utilità segnacolli 1
046500171018     C                   eval      UBSGU1IDS.ISGU1ILIN  = iUBS715LIN
046600171018     C                   eval      UBSGU1IDS.ISGU1IFMT  = 'A'
046700171018     C                   eval      UBSGU1IDS.ISGU1IFLAT = UBSGU1DSA
046800171018     C                   eval      UBSGU1IDS.ISGU1IFLEN = %size(UBSGU1DSA)
046900171018     C                   eval      UBSGU1IDS.ISGU1IBRTE = UBBRTETIDS
047000171018     C                   eval      UBSGU1IDS.ISGU1IBLEN = %size(UBBRTETIDS)
047100170525     C*
047200170525     C* Chiamata al driver stampa e reperimento etichetta segnacolli
047300170525     C                   call      'UBSGU1R'
047400170525     C                   parm                    UBSGU1IDS
047500170804     C                   parm                    UBSGU1OLEN
047600170525     C                   parm                    UBSGU1OUT
047700170525     C*
047800170525     C* Se errore => segnalazione
047900170525     C                   if        UBSGU1IDS.OSGU1IESI <> *zeros
048000170525     C                   eval      wErrIdx = *zeros
048100170525     C                   eval      wErrMsg = UBSGU1IDS.OSGU1IMSG
048200170525     C                   eval      wErrTag = 'UBSGU1R'
048300170525     C                   exsr      exeErr
048400170525     C                   endif
048500170525     C*
048600171018 xxx C***                endif
048700170606     C*
048800170525     C                   endsr
048900170516
049000170522
049100170516
049200170516     C*
049300170518     C     exeCIS_SI95   begsr
049400170516     C*
049500170516     C                   clear                   UBSI95DS
049600170525     C                   eval      ISI95LIN = iUBS715LIN
049700170522     C                   eval      ISI95TYP = 'F'                               C / V / F / *blnk
049800170522     C                   eval      ISI95CFD = §VPCCFD                           S / *blnk
049900170522     C                   eval      ISI95TLL = §VPCTLL                           S / *blnk
050000170522     C                   eval      ISI95IED = §VPCIED                           S / *blnk
050100170522     C                   eval      ISI95DAT = *zeros
050200170516     C*
050300170516     C                   clear                   RTNsi95esito
050400170516     C                   clear                   RTNsi95opcode
050500170516     C                   clear                   RTNsi95status
050600170516     C*
050700170516     C* Chiamata al driver di Calcolo Instradamento
050800170516     C                   call      'UBSI95R'
050900170516     C                   parm                    UBSI95DS
051000170522     C                   parm                    EDIVABDS
051100170516     C                   parm                    RTNsi95esito
051200170516     C                   parm                    RTNsi95opcode
051300170516     C                   parm                    RTNsi95status
051400170516     C*
051500170516     C* Verifico esito chiamata
051600171018     C                   eval      oUBS715SI95 = UBSI95DS
051700170525     C                   if        RTNsi95esito = 0 AND
051800170525     C                             OSI95LNA > *zeros
051900170522     C                   eval      vabLNA = OSI95LNA
052000170522     C                   eval      vabZNC = OSI95ZNC
052100170518     C                   else
052200171018     C*
052300171018     C* Se errore in calcolo instradamento => gestisco
052400171018     C                   seton                                        55
052500170526     C                   eval      wErrIdx = 99
052600170525     C                   eval      wErrMsg = OSI95MSG
052700170525     C                   eval      wErrTag = 'UBSI95R'
052800170525     C                   exsr      exeErr
052900170516     C                   endif
053000170516     C*
053100170516     C                   endsr
053200170522
053300170522
053400170522
053500170522     C*
053600170522     C     exeErr        begsr
053700170526     C*
053800170526     C* Inizializzo enanling flags del messaggio
053900170526     C                   clear                   MSGFLG_DS
054000170525     C*
054100170525     C* Traduco il messaggio di errore da ritornare
054200170525     C                   select
054300170526     C                   when      wErrIdx = 99 AND
054400170526     C                             wErrMsg <> *blanks
054500171018     C                   if        oUBS715MSG = *blanks
054600170526     C                   eval      oUBS715MSG = wErrMsg
054700171018     C                   endif
054800171018     C***                eval      MSGFLG_DS.FatalError = *on
054900170526     C*
055000170525     C                   when      wErrMsg <> *blanks
055100170525     C                   eval      oUBS715MSG = wErrMsg
055200170525     C*
055300170526     C                   when      wErrIdx  > *zeros AND
055400170526     C                             wErrIdx <= %elem(MSGFLG)
055500170526     C                   eval      MSGFLG_DS = MSGFLG(wErrIdx)
055600170526     C                   select
055700170525     C                   when      iUBS715LIN = 'IT'
055800170525     C                   eval      oUBS715MSG = MSGITA(wErrIdx)
055900170525     C                   when      iUBS715LIN = 'EN'
056000170525     C                   eval      oUBS715MSG = MSGENG(wErrIdx)
056100170525     C                   when      iUBS715LIN = 'FR'
056200170525     C                   eval      oUBS715MSG = MSGFRA(wErrIdx)
056300170525     C                   when      iUBS715LIN = 'DE'
056400170525     C                   eval      oUBS715MSG = MSGDEU(wErrIdx)
056500170525     C                   when      iUBS715LIN = 'ES'
056600170525     C                   eval      oUBS715MSG = MSGESP(wErrIdx)
056700170525     C                   other
056800170525     C                   eval      oUBS715MSG = MSGENG(wErrIdx)
056900170525     C                   endsl
057000170525     C*
057100170525     C                   other
057200170525     C                   eval      oUBS715MSG = *blanks
057300170525     C                   endsl
057400170522     C*
057500170525     C* Se opportuno invio email con dettaglio errore occorso
057600170526     C                   if        MSGFLG_DS.InviaEmail = *on
057700170525     C                   exsr      sndAlert
057800170525     C                   endif
057900170525     C*
058000170526     C* Se errore "locale" valorizzo esito a KO ed esco
058100170526     C                   if        wErrTag = 'UBS715R' OR
058200170526     C                             MSGFLG_DS.FatalError = *on
058300170525     C                   eval      oUBS715ESI = '2'
058400170526     C                   return
058500170525     C                   endif
058600170522     C*
058700170522     C                   endsr
058800170522
058900170526
059000170522
059100170522     C     sndAlert      BEGSR
059200170522     C*
059300170522     C* Inizializzo variabili
059400170522     C                   movel     *blanks       wrkEml          253
059500170522     C                   movel     *blanks       wrkCcEml        253
059600170522     C                   movel     *blanks       wrkEmlMsg      5000
059700170522     C                   movel     *blanks       wrkEmlOgg        44
059800170522     C*
059900170522     C* Valorizzo i campi della e-m@ail
060000170522     C                   eval      wrkEml='cedalert@brt.it'
060100170522     C                   eval      wrkEmlOgg=
060200170522     C                               'VAS - Errore in esecuzione UBS715R'
060300170522     C                   EVAL      wrkEmlMsg = 'Errore in esecuzione UBS715R: '+
060400170522     C                                                                    ':/N'+
060500170522     C                                                                    ':/N'+
060600170525     C                             %trim(wErrTag)                              +
060700170522     C                                                                    ':/N'+
060800170522     C                                                                    ':/N'+
060900170525     C                             %trim(oUBS715MSG)                           +
061000170525     C                                                                    ':/N'+
061100170525     C                                                                    ':/N'+
061200171018     C                             'Verificare il LOG del job:  '              +
061300170522     C                             %editc(psjobnbr:'X')+'/'                    +
061400170522     C                             %trim(psuser)+'/'                           +
061500170522     C                             %trim(psjobname)                      +':/N'+
061600170522     C                             x'00'
061700170522     C*
061800170522     C                   call(e)   'TIS701C1'
061900170522     C                   parm                    wrkEml
062000170522     C                   parm                    wrkCcEml
062100170522     C                   parm                    wrkEmlOgg
062200170522     C                   parm                    wrkEmlMsg
062300170522     C                   parm                    wrkEsito          1
062400170522     C*
062500170522     C                   ENDSR
062600170824
062700170824
062800170824
062900170824     C*
063000170824     C     carTab        begsr
063100170824     C*
063200170824     C* Reperimento TIPO FILE ETICHETTE SEGNACOLLO di DEFAULT "VASWS"
063300170824     C                   eval      iS713TBECOD = 'VPR'
063400170824     C                   eval      iS713TBEKE1 = '*VASWS'
063500170824     C                   eval      iS713TBEKE2 = '*TIPF-ETI'
063600170824     C                   eval      iS713TBELIN = *blanks
063700170824     C                   eval      iS713TBESIF = *blanks
063800170824     C                   call      'TIS713R'
063900170824     C                   parm                    iS713TBECOD
064000170824     C                   parm                    iS713TBEKE1
064100170824     C                   parm                    iS713TBEKE2
064200170824     C                   parm                    iS713TBELIN
064300170824     C                   parm                    iS713TBESIF
064400170824     C                   parm                    oS713TBEUNI
064500170824     C                   parm                    oS713ESITO
064600170824     C*
064700170824     C                   if        oS713ESITO = '1'
064800170824     C                   eval      dftTIPF_ETI = %trim(oS713TBEUNI)
064900170824     C                   else
065000170824     C                   eval      dftTIPF_ETI = 'SG'
065100170824     C                   endif
065200170824     C*
065300170824     C* Reperimento TIPO TRASFERIMENTO OUTPUT di DEFAULT "VASWS"
065400170824     C                   eval      iS713TBECOD = 'VPR'
065500170824     C                   eval      iS713TBEKE1 = '*VASWS'
065600170824     C                   eval      iS713TBEKE2 = '*TIP-TRASF'
065700170824     C                   eval      iS713TBELIN = *blanks
065800170824     C                   eval      iS713TBESIF = *blanks
065900170824     C                   call      'TIS713R'
066000170824     C                   parm                    iS713TBECOD
066100170824     C                   parm                    iS713TBEKE1
066200170824     C                   parm                    iS713TBEKE2
066300170824     C                   parm                    iS713TBELIN
066400170824     C                   parm                    iS713TBESIF
066500170824     C                   parm                    oS713TBEUNI
066600170824     C                   parm                    oS713ESITO
066700170824     C*
066800170824     C                   if        oS713ESITO = '1'
066900170824     C                   eval      dftTIP_TRASF = %trim(oS713TBEUNI)
067000170824     C                   else
067100170824     C                   eval      dftTIP_TRASF = 'BV'
067200170824     C                   endif
067300170824     C*
067400170824     C                   endsr
067500170824     C*
067600000714
067700050920
067800050920
067900000714      /TITLE Routine di *init PGM
068000000714     C*
068100000714     C     *inzsr        begsr
068200060103     C*
068300060103     C     *entry        plist
068400170525     C                   parm                    iUBS715LIN
068500170522     C                   parm                    iUBS715TID
068600170523     C                   parm                    iUBS715ENV
068700170522     C                   parm                    iUBS715OPZ
068800170522     C                   parm                    iUBS715KSU
068900170522     C                   parm                    iUBS715UNI
069000170522     C                   parm                    iUBS715DATI
069100170522     C                   parm                    oUBS715ESI
069200170525     C                   parm                    oUBS715MSG
069300170803     C                   parm                    oUBS715SI95
069400170523     C                   parm                    oUBS715DATI
069500000714     C*
069600000714     C                   endsr
069700170525** MSGFLG - FLAG MESSAGGI
06980017052611
06990017052611
07000017052611
07010017052611
07020017052611
070300170530
070400170525
070500170525
070600170525
070700170525
070800170525** MSGITA - MESSAGGI
070900170525Errore generico
071000170525Parametro non valido
071100170526Segnacollo errato o non consentito
071200170526Segnacollo già utilizzato
071300170526Impossibile prelevare un nuovo ID segnacollo
071400170525
071500170525
071600170525
071700170525
071800170525
071900170525** MSGENG - MESSAGGI
072000170525Generic error
072100170525Invalid parameter
072200170526Parcel number invalid or not allowed
072300170525Parcel number already consumed
072400170526Unable to raise new Parcel number
072500170525
072600170525
072700170525
072800170525
072900170525
073000170525** MSGFRA - MESSAGGI
073100170525Generic error
073200170525Invalid parameter
073300170526Parcel number invalid or not allowed
073400170525Parcel number already consumed
073500170526Unable to raise new Parcel number
073600170525
073700170525
073800170525
073900170525
074000170525
074100170525** MSGDEU - MESSAGGI
074200170525Generic error
074300170525Invalid parameter
074400170526Parcel number invalid or not allowed
074500170525Parcel number already consumed
074600170526Unable to raise new Parcel number
074700170525
074800170525
074900170525
075000170525
075100170525
075200170525** MSGESP - MESSAGGI
075300170525Generic error
075400170525Invalid parameter
075500170526Parcel number invalid or not allowed
075600170525Parcel number already consumed
075700170526Unable to raise new Parcel number
075800170525
075900170525
076000170525
076100170525
076200170525
