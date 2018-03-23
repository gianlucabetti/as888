000100170605     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200170605
000300170605
000400170605      //********************************************************************************************
000500170605      //
000600170605      // Definizione file.
000700170605      //
000800170605      //********************************************************************************************
000900170605     Ftabel00f  IF   E           K DISK
001000170412
001100170412
001200170412      //********************************************************************************************
001300170412      //
001400170412      // Definizione prototipi procedure.
001500170412      //
001600170412      //********************************************************************************************
001700170605     D/COPY GAITRASRC/SRCPROTOPR,UBVCP0R_01
001800170605     D/COPY GAITRASRC/SRCPROTOPR,UBVCP0R_04
001900170412
002000170412
002100170412      //********************************************************************************************
002200170412      //
002300170412      // Definizione interfacce procedure.
002400170412      //
002500170412      //********************************************************************************************
002600170523     D/COPY GAITRASRC/SRCPROTOPI,UBCKSGNR
002700170605     D/COPY GAITRASRC/SRCPROTOPI,UBS716TIS
002800170605     D/COPY GAITRASRC/SRCPROTOPI,UBAASGNR
002900010524
003000170419     D*
003100170605     D  DS3C         e ds                  inz
003200170522     D  DVPC         e ds                  inz
003300170602     D  UBS716DS     e ds                  inz qualified
003400170605     D  wLNP           s                   inz like(aasgn_LNP)
003500170605     D  wNRS           s                   inz like(aasgn_NRS)
003600170605     D  wNCD           s                   inz like(aasgn_NCD)
003700170605     D  wNCA           s                   inz like(aasgn_NCA)
003800170605     D  wLNA           s                   inz like(aasgn_LNA)
003900170605     D  wNCL           s                   inz like(aasgn_NCL)
004000170605     D  wCCM           s                   inz like(aasgn_CCM)
004100170816     D  wCTM           s                   inz like(aasgn_CTM)
004200170602     D  esci_ciclo     s               N   inz
004300170605     D  segnac_OK      s               N   inz
004400170602     D  oVCPEsito      s              2S 0 inz
004500170602     D  oVCPNum        s             15S 0 inz
004600170602     D  oVCPNumDal     s             15S 0 inz
004700170602     D  oVCPIncr       s              3S 0 inz
004800170516
004900170526     D*
005000170605     D MSGITA          S            100    DIM(10) CTDATA PERRCD(1)
005100170605     D MSGENG          S            100    DIM(10) CTDATA PERRCD(1)
005200170605     D MSGFRA          S            100    DIM(10) CTDATA PERRCD(1)
005300170605     D MSGDEU          S            100    DIM(10) CTDATA PERRCD(1)
005400170605     D MSGESP          S            100    DIM(10) CTDATA PERRCD(1)
005500170602
005600170602     D*
005700170605     D VCP_VASWS_DS    ds                  inz qualified
005800170602     D   filler06                     6S 0
005900170602     D   NRS                          2S 0
006000170602     D   CNT                          7S 0
006100170605     D   full                  1     15S 0
006200170605
006300170605     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
006400170605     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
006500010525
006600010525
006700010525     C*-----------------------------------------------------------------------------
006800170412     C*
006900170412     C* Avvio il monitoring del intero flusso
007000170412     C                   monitor
007100170605     C*
007200170605     C* Inizializzazione di work
007300170605     C                   exsr      exeInz
007400170522     C*
007500170522     C* Verifica parametri
007600170522     C                   exsr      chkPar
007700170519     C*
007800170602     C* Eseguo Main
007900170602     C                   exsr      exeMain
008000170602     C*
008100170602     C* Controlli finali
008200170605     C                   exsr      chkFin
008300170412     C*
008400170412     C* Gestisco eventuale errore
008500170412     C                   on-error
008600170602     C
008700170602     C                   eval      oAASGNESI = -1
008800170605     C                   exsr      exeErr
008900170412     C*
009000170412     C* Arresto il monitoring
009100170412     C                   endmon
009200100624     C*
009300170420     C                   return
009400010525     C*
009500010525     C*-----------------------------------------------------------------------------
009600170526
009700170526
009800170526
009900170526     C*
010000170526     C     exeInz        begsr
010100170526     C*
010200170526     C* Inizializzazioni varie
010300170602     C                   eval      esci_ciclo = *off
010400170605     C                   eval      segnac_OK  = *off
010500170605     C                   clear                   wLNP
010600170605     C                   clear                   wNRS
010700170602     C                   clear                   wNCD
010800170602     C                   clear                   wNCA
010900170605     C                   clear                   wLNA
011000170605     C                   clear                   wNCL
011100170605     C                   clear                   wCCM
011200170816     C                   clear                   wCTM
011300170602     C*
011400170602     C* Inizializzo l'esito ad errore generico
011500170605     C                   eval      oAASGNESI = -1
011600170609     C                   clear                   oAASGNMSG
011700170602     C*
011800170602     C* Ridefinisco DS tabella VAS Personalizzazioni Clienti
011900170602     C                   eval      DVPC = iAASGNUNI
012000170526     C*
012100170526     C                   endsr
012200170605
012300170605
012400170605
012500170605     C*
012600170605     C     chkPar        begsr
012700170605     C*
012800170605     C* Se dati tabella VAS Personalizzazioni Clienti vuoti => errore
012900170605     C                   if        iAASGNUNI = *blanks
013000170605     C                   eval      oAASGNESI = -2
013100170605     C                   exsr      exeErr
013200170605     C                   return
013300170605     C                   endif
013400170605     C*
013500170605     C* Se dati minimi necessari per assegnazione segnacaolli mancanti => errore
013600170605     C                   if        aasgn_CCM = *zeros OR
013700170605     C                             aasgn_LNP = *zeros OR
013800170801     C***                          aasgn_NRS = *zeros OR
013900170605     C                             aasgn_NCL = *zeros OR
014000170605     C                             aasgn_LNA = *zeros
014100170605     C                   eval      oAASGNESI = -2
014200170605     C                   exsr      exeErr
014300170605     C                   return
014400170605     C                   endif
014500170605     C*
014600170605     C* Effettuo considerazioni sul ID lingua indicato in input
014700170605     C                   if        iAASGNLIN = *blanks
014800170605     C                   eval      iAASGNLIN = 'IT'
014900170605     C                   else
015000170605     C                   eval      iAASGNLIN =
015100170605     C                             %xlate(Lower:Upper:iAASGNLIN)
015200170605     C                   endif
015300170605     C                   select
015400170605     C                   when      iAASGNLIN = 'IT'
015500170605     C                   when      iAASGNLIN = 'EN'
015600170605     C                   when      iAASGNLIN = 'FR'
015700170605     C                   when      iAASGNLIN = 'DE'
015800170605     C                   when      iAASGNLIN = 'ES'
015900170605     C                   other
016000170605     C                   eval      iAASGNLIN = 'EN'
016100170605     C                   endsl
016200170605     C*
016300170605     C* Innanzitutto reperisco serie correntemente assegnata sul cliente in tab 3C
016400170605     C                   eval      tblKUT = 1
016500170605     C                   eval      tblCOD = '3C'
016600170609     C                   eval      tblKEY = %editc(aasgn_CCM:'X')
016700170605     C     KEYtab_C      chain     tabel00f
016800170605     C                   if        %found(tabel00f) AND tblFLG = *blanks
016900170605     C                   eval      DS3C = tblUNI
017000170605     C                   eval      wNRS = §3CNRS
017100170816     C                   eval      wCTM = §3CCTM
017200170605     C                   else
017300170605     C                   eval      oAASGNESI = -7
017400170605     C                   exsr      exeErr
017500170605     C                   return
017600170605     C                   endif
017700170605     C*
017800170605     C                   endsr
017900170602
018000170602
018100170602
018200170602     C*
018300170605     C     exeMain       begsr
018400170605     C*
018500170605     C* Imposto gli elementi provenienti sempre dai dati spedizione
018600170605     C                   eval      wLNA = aasgn_LNA
018700170605     C                   eval      wNCL = aasgn_NCL
018800170605     C                   eval      wCCM = aasgn_CCM
018900170605     C*
019000170602     C* A seconda del tipo Assegnazione Segnacolli indicata in tabella VAS Personalizzazioni Clienti
019100170602     C                   select
019200170602     C                   when      §VPCASG = 'S' or §VPCASG = 'F'
019300170602     C*
019400170602     C* Se richeista Assegnazione Automatica Segnacolli eseguo ciclo principale di reperimento
019500170605     C                   z-add     1             wNumCiclo         2 0
019600170602     C                   z-add     5             wMaxCicli         2 0
019700170605     C                   dow       not esci_ciclo AND wNumCiclo <= wMaxCicli
019800170801     C*
019900170801     C* Se numero serie non presente su file => considero quello da tabella '3C' corrente
020000170801     C                   if        aasgn_NRS = *zeros
020100170801     C                   eval      aasgn_NRS = wNRS
020200170801     C                   endif
020300170816     C*
020400170816     C* Codice trattamento merce => considero sempre quello da tabella '3C' corrente
020500170816     C                   eval      aasgn_CTM = wCTM
020600170605     C*
020700170605     C* Eseguo operazioni previste/necessarie
020800170602     C                   exsr      exeOpe
020900170605     C*
021000170605     C* Eseguo verifica dati segnacollo
021100170605     C                   exsr      exeChk
021200170605     C*
021300170605     C                   add       1             wNumCiclo
021400170605     C                   enddo
021500170602     C*
021600170602     C* Altrimenti effettuo unicamente controllo dei dati già presenti
021700170605     C                   other
021800170605     C*
021900170605     C* Considero tutti dati segnacollo indicati sui dati dal cliente
022000170605     C                   eval      wLNP = aasgn_LNP
022100170602     C                   eval      wNRS = aasgn_NRS
022200170602     C                   eval      wNCD = aasgn_NCD
022300170602     C                   eval      wNCA = aasgn_NCA
022400170605     C*
022500170605     C* Eseguo verifica dati segnacollo
022600170605     C                   exsr      exeChk
022700170602     C                   endsl
022800170605     C*
022900170602     C                   endsr
023000170526
023100170526
023200170526
023300170602     C*
023400170602     C     exeOpe        begsr
023500170530     C*
023600170602     C* Per prima cosa verifico eventuale assegnazione segnacolli precedente per medesima spedizione
023700170602     C                   eval      iS716DSVAB  = iAASGNDSVAB
023800170530     C                   eval      iS716TIS    = §VPCTIS
023900170530     C                   call      'UBS71601R'
024000170530     C                   parm                    iS716DSVAB
024100170530     C                   parm                    iS716TIS
024200170530     C                   parm                    oS716FOUND
024300170530     C                   parm                    oS716DTA
024400170530     C*
024500170530     C                   select
024600170530     C*
024700170605     C* Se errore in reperimento => errore generale (preferisco non "rischiare")
024800170605     C                   when      oS716FOUND < *zeros
024900170602     C                   eval      oAASGNESI = -1
025000170605     C                   exsr      exeErr
025100170602     C                   return
025200170530     C*
025300170602     C* Se segnacolli già in precedenza assegnati
025400170602     C                   when      oS716FOUND = 1
025500170602     C                   eval      UBS716DS = oS716DTA
025600170605     C                   eval      wLNP = UBS716DS.S716FLS
025700170605     C                   eval      wNRS = UBS716DS.S716NRS
025800170602     C                   eval      wNCD = UBS716DS.S716NCD
025900170602     C                   eval      wNCA = UBS716DS.S716NCA
026000170605     C                   exsr      chkAASGN
026100170605     C                   eval      esci_ciclo = *on
026200170602     C*
026300170602     C* Se segnacolli non ancora assegnati
026400170602     C                   when      oS716FOUND = 0
026500170605     C                   exsr      rtvAASGN
026600170605     C*
026700170605     C                   endsl
026800170605     C*
026900170605     C                   endsr
027000170605
027100170605
027200170605
027300170605     C*
027400170605     C     exeChk        begsr
027500170605     C*
027600170605     C* Inizializzo flag segnac_OK a OK
027700170605     C                   eval      segnac_OK = *on
027800170605     C*
027900170605     C* 1 - Dati mandatori congruenti
028000170605     C                   if        segnac_OK
028100170605     C                   if        wNCD = *zeros AND wNCA > *zeros
028200170605     C                   eval      wNCD = wNCA
028300170605     C                   eval      wNCA = *zeros
028400170605     C                   endif
028500170605     C                   if        wNCA = *zeros AND wNCL = 1
028600170605     C                   eval      wNCA = wNCD
028700170605     C                   endif
028800170605     C                   if        %abs(wNCA - wNCD) + 1 <> wNCL
028900170605     C                   eval      segnac_OK = *off
029000170605     C                   endif
029100170605     C                   endif
029200170605     C*
029300170605     C* 2 - Dati mandatori presenti
029400170605     C                   if        segnac_OK
029500170605     C                   if        wLNP = *zeros OR
029600170605     C                             wNRS = *zeros OR
029700170605     C                             wNCD = *zeros OR
029800170605     C                             wLNA = *zeros
029900170605     C                   eval      segnac_OK = *off
030000170605     C                   endif
030100170605     C                   endif
030200170605     C*
030300170605     C* 3 - Verifica correttezza "operativa" di TUTTI i segnacolli DA/A
030400170605     C                   if        segnac_OK
030500170605     C                   clear                   wCurrSGN          7 0
030600170605     C                   z-add     1             i                 5 0
030700170605     C                   dow       i <= wNCL AND segnac_OK
030800170605     C*
030900170605     C* Imposto segnacollo corrrente
031000170605     C                   eval      wCurrSGN = wNCD + i - 1
031100170605     C*
031200170605     C* Segnacollo non del proprio range serie/segnacollo
031300170605     C                   clear                   oVCPEsito
031400170605                         UBVCP0R_CTL('VASWS'
031500170609                                    :%editc(wCCM:'X')
031600170605                                    :*zeros
031700170605                                    :oVCPEsito
031800170605                           :%dec(%editc(wNRS:'X')+%editc(wCurrSGN:'X'):15:0));
031900170605
032000170605     C*
032100170605     C                   if        oVCPEsito <> *zeros
032200170605     C                   eval      segnac_OK = *off
032300170605     C                   eval      oAASGNESI = -5
032400170605     C                   exsr      exeErr
032500170605     C                   endif
032600170605     C*
032700170605     C* Segnacollo già utilizzato
032800170605     C                   if        segnac_OK
032900170605     C                   eval      iCKSGNFLS   = wLNP
033000170605     C                   eval      iCKSGNLNA   = wLNA
033100170605     C                   eval      iCKSGNNRS   = wNRS
033200170605     C                   eval      iCKSGNNSC   = wCurrSGN
033300170605     C                   call      'UBCKSGNR'
033400170605     C                   parm                    iCKSGNFLS
033500170605     C                   parm                    iCKSGNLNA
033600170605     C                   parm                    iCKSGNNRS
033700170605     C                   parm                    iCKSGNNSC
033800170605     C                   parm                    oCKSGNFOUND
033900170605     C                   parm                    oCKSGNARTDS
034000170605     C*
034100170605     C                   if        oCKSGNFOUND <> *zeros
034200170605     C                   eval      segnac_OK = *off
034300170605     C                   eval      oAASGNESI = -6
034400170605     C                   exsr      exeErr
034500170605     C                   endif
034600170605     C                   endif
034700170605     C*
034800170605     C                   add       1             i
034900170605     C                   enddo
035000170605     C                   endif
035100170605     C*
035200170605     C* In uscita verifico esito del controllo, se OK => tutto è già OK per cui escita ciclo Main
035300170605     C                   if        segnac_OK
035400170609     C                   eval      esci_ciclo = *on
035500170605     C                   endif
035600170605     C*
035700170605     C                   endsr
035800170602
035900170602
036000170602
036100170602     C*
036200170605     C     rtvAASGN      begsr
036300170602     C*
036400170602     C* Stacco i segnacolli DA/A dal proprio range serie/segnacollo
036500170602     C                   clear                   oVCPEsito
036600170602     C                   clear                   oVCPNum
036700170602     C                   clear                   oVCPNumDal
036800170602     C                   clear                   oVCPIncr
036900170602                         UBVCP0R_PRG('VASWS'
037000170609                                    :%editc(wCCM:'X')
037100170602                                    :oVCPEsito
037200170602                                    :oVCPNum
037300170605                                    :wNCL
037400170602                                    :oVCPNumDal
037500170602                                    :oVCPIncr);
037600170602     C*
037700170602     C                   if        oVCPEsito <> *zeros OR oVCPIncr   > 1
037800170602     C*
037900170602     C* Se errore o errata configurazione anagrafica progressivo VASWS
038000170605     C                   eval      oAASGNESI = -8
038100170605     C                   exsr      exeErr
038200170602     C                   return
038300170602     C*
038400170602     C* Se OK valorizzo dati segnacolli
038500170602     C                   else
038600170605     C                   eval      wLNP = aasgn_LNP
038700170605     C                   eval      VCP_VASWS_DS.full = oVCPNumDal
038800170602     C                   eval      wNRS = VCP_VASWS_DS.NRS
038900170602     C                   eval      wNCD = VCP_VASWS_DS.CNT
039000170605     C                   eval      VCP_VASWS_DS.full = oVCPNum
039100170602     C                   eval      wNCA = VCP_VASWS_DS.CNT
039200170602     C*
039300170602     C* Verifico dati segnacollo
039400170605     C                   exsr      chkAASGN
039500170602     C*
039600170602     C* Memorizzo segnacolli assegnati in caso di ri-esecuzione medesima spedizione
039700170602     C                   eval      iS716DSVAB  = iAASGNDSVAB
039800170602     C                   eval      iS716TIS    = §VPCTIS
039900170602     C                   call      'UBS71602R'
040000170602     C                   parm                    iS716DSVAB
040100170602     C                   parm                    iS716TIS
040200170602     C                   parm                    oS716ESI
040300170602     C*
040400170602     C                   endif
040500170602     C*
040600170602     C                   endsr
040700170605
040800170605
040900170605
041000170605     C*
041100170605     C     chkAASGN      begsr
041200170605     C*
041300170605     C                   select
041400170605     C*
041500170605     C* Se Segnacolli Assgnati = dati VAB o dati VAB assenti => OK
041600170605     C                   when      (wLNP = aasgn_LNP OR aasgn_LNP = *zeros) AND
041700170605     C                             (wNRS = aasgn_NRS OR aasgn_NRS = *zeros) AND
041800170605     C                             (wNCD = aasgn_NCD OR aasgn_NCD = *zeros) AND
041900170605     C                             (wNCA = aasgn_NCA OR aasgn_NCA = *zeros)
042000170605     C                   exsr      valOut
042100170605     C*
042200170605     C* Se Segnacolli Assgnati <> dati VAB e richiesta forzatura => OK
042300170605     C                   when      (wLNP <> aasgn_LNP AND §VPCASG = 'F') OR
042400170605     C                             (wNRS <> aasgn_NRS AND §VPCASG = 'F') OR
042500170605     C                             (wNCD <> aasgn_NCD AND §VPCASG = 'F') OR
042600170605     C                             (wNCA <> aasgn_NCA AND §VPCASG = 'F')
042700170605     C                   exsr      valOut
042800170605     C*
042900170605     C* Altrimenti => errore
043000170609     C                   other
043100170605     C                   eval      oAASGNESI = -3
043200170605     C                   exsr      exeErr
043300170605     C*
043400170605     C                   endsl
043500170605     C*
043600170605     C                   endsr
043700170602
043800170602
043900170602
044000170602     C*
044100170602     C     valOut        begsr
044200170602     C*
044300170602     C* Valorizzazione in ouput Segnacolli Assegnati
044400170605     C                   eval      aasgn_LNP = wLNP
044500170602     C                   eval      aasgn_NRS = wNRS
044600170602     C                   eval      aasgn_NCD = wNCD
044700170602     C                   eval      aasgn_NCA = wNCA
044800170602     C                   eval      esci_ciclo = *on
044900170602     C*
045000170602     C                   endsr
045100170605
045200170605
045300170605
045400170605     C*
045500170605     C     chkFin        begsr
045600170605     C*
045700170605     C* Se tutti i dati mandatori per il segnacollo siano valorizzati => OK finale
045800170605     C                   if        aasgn_LNP > *zeros AND
045900170605     C                             aasgn_NRS > *zeros AND
046000170605     C                             aasgn_NCD > *zeros AND
046100170605     C                             aasgn_NCA > *zeros AND
046200170605     C                             aasgn_LNA > *zeros AND
046300170605     C                             aasgn_NCL > *zeros
046400170605     C                   eval      oAASGNESI = *zeros
046500170605     C                   clear                   oAASGNMSG
046600170605     C                   endif
046700170605     C*
046800170605     C                   endsr
046900170605
047000170605
047100170605
047200170605     C*
047300170605     C     exeErr        begsr
047400170605     C*
047500170605     C* Traduco il messaggio di errore da ritornare
047600170605     C                   select
047700170605     C                   when      iAASGNLIN = 'IT'
047800170605     C                   eval      oAASGNMSG = MSGITA(%abs(oAASGNESI))
047900170605     C                   when      iAASGNLIN = 'EN'
048000170605     C                   eval      oAASGNMSG = MSGENG(%abs(oAASGNESI))
048100170605     C                   when      iAASGNLIN = 'FR'
048200170605     C                   eval      oAASGNMSG = MSGFRA(%abs(oAASGNESI))
048300170605     C                   when      iAASGNLIN = 'DE'
048400170605     C                   eval      oAASGNMSG = MSGDEU(%abs(oAASGNESI))
048500170605     C                   when      iAASGNLIN = 'ES'
048600170605     C                   eval      oAASGNMSG = MSGESP(%abs(oAASGNESI))
048700170605     C                   other
048800170605     C                   eval      iAASGNLIN = MSGENG(%abs(oAASGNESI))
048900170605     C                   endsl
049000170605     C*
049100170605     C                   endsr
049200000714
049300050920
049400050920
049500000714      /TITLE Routine di *init PGM
049600000714     C*
049700000714     C     *inzsr        begsr
049800060103     C*
049900060103     C     *entry        plist
050000170609     C                   parm                    iAASGNLIN
050100170602     C                   parm                    iAASGNUNI
050200170602     C                   parm                    iAASGNDSVAB
050300170602     C                   parm                    oAASGNESI
050400170602     C                   parm                    oAASGNMSG
050500170605     C*
050600170605     C* Chiave su TABEL00F - Completa
050700170605     C     KEYtab_C      KLIST
050800170605     C                   KFLD                    tblKUT
050900170605     C                   KFLD                    tblCOD
051000170605     C                   KFLD                    tblKEY
051100000714     C*
051200000714     C                   endsr
051300170525** MSGITA - MESSAGGI
051400170525Errore generico
051500170602Parametri non validi
051600170602Impossibile assegnare segnacolli
051700170602Errore reperimento segnacolli
051800170605Segnacollo errato o non consentito
051900170605Segnacollo già utilizzato
052000170605Cliente non abilitato alla serie (tab 3C)
052100170605Impossibile prelevare un nuovo ID segnacollo
0522001706059
05230017060510
052400170605** MSGENG - MESSAGGI
052500170605Generic error
052600170605Invalid parameter
052700170605Unable to raise new parcel number
052800170605Error addressing parcel number
052900170605Parcel number invalid or not allowed
053000170605Parcel number already consumed
053100170605Customer not enabled to series (tab 3C)
053200170605Unable to raise new parcel number
0533001706059
05340017060510
053500170605** MSGFRA - MESSAGGI
053600170605Generic error
053700170605Invalid parameter
053800170605Unable to raise new parcel number
053900170605Error addressing parcel number
054000170605Parcel number invalid or not allowed
054100170605Parcel number already consumed
054200170605Customer not enabled to series (tab 3C)
054300170605Unable to raise new parcel number
0544001706059
05450017060510
054600170605** MSGDEU - MESSAGGI
054700170605Generic error
054800170605Invalid parameter
054900170605Unable to raise new parcel number
055000170605Error addressing parcel number
055100170605Parcel number invalid or not allowed
055200170605Parcel number already consumed
055300170605Customer not enabled to series (tab 3C)
055400170605Unable to raise new parcel number
0555001706059
05560017060510
055700170605** MSGESP - MESSAGGI
055800170605Generic error
055900170605Invalid parameter
056000170605Unable to raise new parcel number
056100170605Error addressing parcel number
056200170605Parcel number invalid or not allowed
056300170605Parcel number already consumed
056400170605Customer not enabled to series (tab 3C)
056500170605Unable to raise new parcel number
0566001706059
05670017060510
