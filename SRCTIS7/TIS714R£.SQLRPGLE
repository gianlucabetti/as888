000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200990907
000300170801     Fedivabvpc uf   e             disk    usropn prefix(E_)
000400170522     F                                     extdesc('GAITRAGRPS/EDIVABWR')
000500170822     Fedivatvpc uf   e             disk    usropn prefix(E_)
000600170822     F                                     extdesc('GAITRAGRPS/EDIVATWR')
000700170420     Ffivabvpc  uf   e             disk    usropn prefix(F_)
000800170522     F                                     extdesc('GAITRAGRPS/FIVABWWR')
000900170824     Ffivatvpc  uf   e             disk    usropn prefix(F_)
001000170824     F                                     extdesc('GAITRAGRPS/FIVATWWR')
001100170822     Fesyvab0f  o    e             disk    rename(edivab00:esyvab00)
001200170822     Fesyvat0f  o    e             disk    rename(edivat00:esyvat00)
001300170901     Ftivgd00f  o    e             disk    usropn
001400170412
001500170412
001600170412      //********************************************************************************************
001700170412      //
001800170412      // Definizione prototipi procedure.
001900170412      //
002000170412      //********************************************************************************************
002100170412
002200170412
002300170412      //********************************************************************************************
002400170412      //
002500170412      // Definizione interfacce procedure.
002600170412      //
002700170412      //********************************************************************************************
002800170522     D/COPY GAITRASRC/SRCPROTOPI,UBS715R
002900170824     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
003000990907
003100010524
003200170419     D*
003300170803     D  UBSI95DS     e ds                  inz
003400170419     D  FIVABDS_F    e ds                  inz extname(fivabwwr) prefix(F_)
003500170824     D  FIVATDS_F    e ds                  inz extname(fivatwwr) prefix(F_)
003600170824     D  FIVABDS      e ds                  inz extname(fivabwwr) qualified
003700170824     D  FIVATDS      e ds                  inz extname(fivatwwr) qualified
003800170801     D  EDIVABDS_E   e ds                  inz extname(edivabwr) prefix(E_)
003900170822     D  EDIVATDS_E   e ds                  inz extname(edivatwr) prefix(E_)
004000170824     D  EDIVABDS     e ds                  inz extname(edivabwr)
004100170824     D  EDIVATDS     e ds                  inz extname(edivatwr)
004200170821     D  DVPC         e ds                  inz
004300170824     D  TIS7VASDS    e ds                  inz
004400170822     D*
004500170822     D wCCM            s                   inz like(vabCCM)
004600170822     D wSUN            s                   inz like(vabSUN)
004700170823     D sCMR            s                   inz like(vabCMR)
004800170823     D sDATCOR         s                   inz like(vabDTS)
004900170823     D sORACOR         s                   inz like(vabHMS)
005000170824     D dftTIPF         s              2A
005100171021     D fgsANN          s              3A
005200170901     D wBufferOut      s           2048    inz
005300171003     D wCntTot         s              9S 0 inz
005400171003     D wCntErr         s              9S 0 inz
005500171228     D wCaller         s             15    inz
005600171228     D wLenBuff        s              4S 0 inz
005700170901     D*
005800170901     D psds           sds
005900170901     D  procname         *PROC
006000010525
006100010525
006200010525     C*-----------------------------------------------------------------------------
006300170818     C*
006400170818     C* Definisco le opzioni con cui verranno di seguito utilizzate le istruzioni SQL
006500170818     C
006600170818     C/EXEC SQL
006700170818     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006800170818     C/END-EXEC
006900170412     C*
007000170412     C* Avvio il monitoring del intero flusso
007100170412     C                   monitor
007200170824     C*
007300170824     C* Caricamento tabella
007400170824     C                   exsr      carTab
007500170821     C*
007600170821     C* Ridefinisco dati tabella VPC ricevuti in input
007700170901     C                   eval      DVPC = prmI714UNI
007800170901     C*
007900170901     C* Imposto subito alcuni valori costanti
008000170901     C                   eval      sCMR    = %char(%timestamp(*SYS))
008100170901     C                   eval      sDATCOR = %dec(%date() : *iso)
008200170901     C                   eval      sORACOR = %dec(%time() : *iso)
008300170412     C*
008400170412     C* In base al tipo di file indicato in input => leggo
008500170412     C                   select
008600170419     C                   when      prmFmtFil = 'FB'
008700170412     C                   exsr      exeFNVAB
008800170824     C                   exsr      uplTO_EW_FB
008900170822 xxx C***                exsr      uplTO_DL_FB
009000170419     C                   when      prmFmtFil = 'EB'
009100170412     C                   exsr      exeEDIVAB
009200170822     C                   exsr      uplTO_EW_EB
009300170901     C                   exsr      uplTO_DL_EB
009400171127 xxx C                   exsr      uplTO_AN_EB
009500170908     C                   when      prmFmtFil = 'ET'
009600170908 xxx C                   exsr      uplTO_AN_ET
009700170412     C                   other
009800170801     C***                eval      prmO714ESI = '2'
009900170412     C                   endsl
010000170412     C*
010100170412     C* Gestisco eventuale errore
010200170412     C                   on-error
010300170518     C
010400170824     C                   exsr      EXEERR
010500170412     C*
010600170412     C* Arresto il monitoring
010700170412     C                   endmon
010800100624     C*
010900171016     C                   seton                                        lr
011000010525     C*
011100010525     C*-----------------------------------------------------------------------------
011200170412
011300170412
011400170412
011500171228     C     exeFNVAB      begsr
011600171003     C*
011700171003     C* Inizializzazione contatori di esito chiamata
011800171003     C                   clear                   wCntTot
011900171003     C                   clear                   wCntErr
012000170420     C*
012100170420     C* Apertura file
012200170420     C                   if        not %open(fivabvpc)
012300170822     C                   open(e)   fivabvpc
012400170420     C                   endif
012500170412     C*
012600170412     C* Scorro tutto il file
012700170822     C                   if        not %error
012800170419     C     *start        setll     fivabvpc
012900170412     C                   read      fivabvpc
013000170523     C                   dow       not %eof(fivabvpc)
013100170412     C*
013200170821     C* Normalizzo verso formato EDIVAB
013300170824     C                   clear                   EDIVABDS
013400170419     C                   eval      FIVABDS = FIVABDS_F
013500170824     C                   eval-corr EDIVABDS = FIVABDS
013600170419     C*
013700170419     C* Eseguo operazioni
013800170522     C                   exsr      exeOpe
013900170801     C*
014000170801     C* Aggiorno dati modificati
014100170824     C                   eval-corr FIVABDS = EDIVABDS
014200170801     C                   eval      FIVABDS_F = FIVABDS
014300170801     C                   update    fivab000
014400170412     C*
014500170516     C* Proseguo lettura dati spedizioni
014600170412     C                   read      fivabvpc
014700170412     C                   enddo
014800170822     C                   endif
014900170420     C*
015000170420     C* Chiusura file
015100170420     C                   if        %open(fivabvpc)
015200170822     C                   close(e)  fivabvpc
015300170420     C                   endif
015400171003     C*
015500171003     C* Verifica esito esecuzione Opzioni
015600171003     C                   if        wCntErr = wCntTot
015700171003     C                   eval      prmO714ESI = '2'
015800171003     C                   endif
015900170412     C*
016000170412     C                   endsr
016100170412
016200170412
016300170412
016400170412     C     exeEDIVAB     begsr
016500171003     C*
016600171003     C* Inizializzazione contatori di esito chiamata
016700171003     C                   clear                   wCntTot
016800171003     C                   clear                   wCntErr
016900170420     C*
017000170420     C* Apertura file
017100170420     C                   if        not %open(edivabvpc)
017200170822     C                   open(e)   edivabvpc
017300170420     C                   endif
017400170412     C*
017500170412     C* Scorro tutto il file
017600170822     C                   if        not %error
017700170419     C     *start        setll     edivabvpc
017800170412     C                   read      edivabvpc
017900170523     C                   dow       not %eof(edivabvpc)
018000170801     C*
018100170821     C* Normalizzo verso formato EDIVAB
018200170824     C                   clear                   EDIVABDS
018300170824     C                   eval      EDIVABDS = EDIVABDS_E
018400170412     C*
018500170419     C* Eseguo operazioni
018600170412     C                   exsr      exeOpe
018700170801     C*
018800170801     C* Aggiorno dati modificati
018900170824     C                   eval      EDIVABDS_E = EDIVABDS
019000170801     C                   update    edivab00
019100170412     C*
019200170516     C* Proseguo lettura dati spedizioni
019300170419     C                   read      edivabvpc
019400170412     C                   enddo
019500170822     C                   endif
019600170420     C*
019700170420     C* Chiusura file
019800170420     C                   if        %open(edivabvpc)
019900170822     C                   close(e)  edivabvpc
020000170420     C                   endif
020100171003     C*
020200171003     C* Verifica esito esecuzione Opzioni
020300171003     C                   if        wCntErr = wCntTot
020400171003     C                   eval      prmO714ESI = '2'
020500171003     C                   endif
020600170412     C*
020700170412     C                   endsr
020800170824
020900170824
021000170824
021100170824     C     uplTO_EW_FB   begsr
021200170824     C*
021300170824     C* Se richiesto UPLOAD TO ... EasySpedweb
021400170824     C                   if        §VPCUPLTO = 'E'
021500170824     C*
021600170824     C* *** Elaborazione file VAT ***
021700170824     C*
021800170824     C* Apertura file
021900170824     C                   if        not %open(fivabvpc)
022000170824     C                   open(e)   fivatvpc
022100170824     C                   endif
022200170824     C*
022300170824     C* Scorro tutto il file
022400170824     C                   if        not %error
022500170824     C     *start        setll     fivatvpc
022600170824     C                   read      fivatvpc
022700170824     C                   dow       not %eof(fivatvpc)
022800170824     C*
022900170824     C* Normalizzo verso formato EDIVAT
023000170824     C                   clear                   EDIVATDS
023100170824     C                   eval      FIVATDS = FIVATDS_F
023200170824     C                   eval-corr EDIVATDS = FIVATDS
023300170824     C*
023400170824     C* Reperisco lo Strategi User Number del cliente corrente
023500170824     C                   eval      wCCM = vatCCM
023600170824     C                   exsr      rtvSUN
023700170824     C*
023800170824     C* Scrivo file "transitorio EasySpedWeb"
023900170824     C                   exsr      wriEsyVAT
024000170824     C*
024100170824     C* Se OK scrittura file "transitorio EasySpedWeb" => elimino file dati UPLOAD corrente
024200170824     C                   delete    fivatvpc
024300170824     C*
024400170824     C* Proseguo lettura dati spedizioni
024500170824     C                   read      fivatvpc
024600170824     C                   enddo
024700170824     C                   endif
024800170824     C*
024900170824     C* Chiusura file
025000170824     C                   if        %open(fivatvpc)
025100170824     C                   close(e)  fivatvpc
025200170824     C                   endif
025300170824     C*
025400170824     C* *** Elaborazione file VAB ***
025500170824     C*
025600170824     C* Apertura file
025700170824     C                   if        not %open(fivabvpc)
025800170824     C                   open(e)   fivabvpc
025900170824     C                   endif
026000170824     C*
026100170824     C* Scorro tutto il file
026200170824     C                   if        not %error
026300170824     C     *start        setll     fivabvpc
026400170824     C                   read      fivabvpc
026500170824     C                   dow       not %eof(fivabvpc)
026600170824     C*
026700170824     C* Normalizzo verso formato EDIVAB
026800170824     C                   clear                   EDIVABDS
026900170824     C                   eval      FIVABDS = FIVABDS_F
027000170824     C                   eval-corr EDIVABDS = FIVABDS
027100170824     C*
027200170824     C* Reperisco lo Strategi User Number del cliente corrente
027300170824     C                   eval      wCCM = vabCCM
027400170824     C                   exsr      rtvSUN
027500170824     C*
027600170824     C* Scrivo file "transitorio EasySpedWeb"
027700170824     C                   exsr      wriEsyVAB
027800170824     C*
027900170824     C* Se OK scrittura file "transitorio EasySpedWeb" => elimino file dati UPLOAD corrente
028000170824     C                   delete    fivabvpc
028100170824     C*
028200170824     C* Proseguo lettura dati spedizioni
028300170824     C                   read      fivabvpc
028400170824     C                   enddo
028500170824     C                   endif
028600170824     C*
028700170824     C* Chiusura file
028800170824     C                   if        %open(fivabvpc)
028900170824     C                   close(e)  fivabvpc
029000170824     C                   endif
029100170824     C*
029200170824     C                   endif
029300170824     C*
029400170824     C                   endsr
029500170822
029600170822
029700170822
029800170822     C     uplTO_EW_EB   begsr
029900170822     C*
030000170822     C* Se richiesto UPLOAD TO ... EasySpedweb
030100170822     C                   if        §VPCUPLTO = 'E'
030200170822     C*
030300170822     C* *** Elaborazione file VAT ***
030400170822     C*
030500170822     C* Apertura file
030600170822     C                   if        not %open(edivatvpc)
030700170822     C                   open(e)   edivatvpc
030800170822     C                   endif
030900170822     C*
031000170822     C* Scorro tutto il file
031100170822     C                   if        not %error
031200170822     C     *start        setll     edivatvpc
031300170822     C                   read      edivatvpc
031400170822     C                   dow       not %eof(edivatvpc)
031500170822     C*
031600170822     C* Normalizzo verso formato EDIVAT
031700170824     C                   clear                   EDIVATDS
031800170824     C                   eval      EDIVATDS = EDIVATDS_E
031900170822     C*
032000170822     C* Reperisco lo Strategi User Number del cliente corrente
032100170822     C                   eval      wCCM = vatCCM
032200170822     C                   exsr      rtvSUN
032300170822     C*
032400170822     C* Scrivo file "transitorio EasySpedWeb"
032500170823     C                   exsr      wriEsyVAT
032600170822     C*
032700170822     C* Se OK scrittura file "transitorio EasySpedWeb" => elimino file dati UPLOAD corrente
032800170822     C                   delete    edivatvpc
032900170822     C*
033000170822     C* Proseguo lettura dati spedizioni
033100170822     C                   read      edivatvpc
033200170822     C                   enddo
033300170822     C                   endif
033400170822     C*
033500170822     C* Chiusura file
033600170822     C                   if        %open(edivatvpc)
033700170822     C                   close(e)  edivatvpc
033800170822     C                   endif
033900170822     C*
034000170822     C* *** Elaborazione file VAB ***
034100170822     C*
034200170822     C* Apertura file
034300170822     C                   if        not %open(edivabvpc)
034400170822     C                   open(e)   edivabvpc
034500170822     C                   endif
034600170822     C*
034700170822     C* Scorro tutto il file
034800170822     C                   if        not %error
034900170822     C     *start        setll     edivabvpc
035000170822     C                   read      edivabvpc
035100170822     C                   dow       not %eof(edivabvpc)
035200170822     C*
035300170822     C* Normalizzo verso formato EDIVAB
035400170824     C                   clear                   EDIVABDS
035500170824     C                   eval      EDIVABDS = EDIVABDS_E
035600170822     C*
035700170822     C* Reperisco lo Strategi User Number del cliente corrente
035800170822     C                   eval      wCCM = vabCCM
035900170822     C                   exsr      rtvSUN
036000170822     C*
036100170822     C* Scrivo file "transitorio EasySpedWeb"
036200170823     C                   exsr      wriEsyVAB
036300170822     C*
036400170822     C* Se OK scrittura file "transitorio EasySpedWeb" => elimino file dati UPLOAD corrente
036500170822     C                   delete    edivabvpc
036600170822     C*
036700170822     C* Proseguo lettura dati spedizioni
036800170822     C                   read      edivabvpc
036900170822     C                   enddo
037000170822     C                   endif
037100170822     C*
037200170822     C* Chiusura file
037300170822     C                   if        %open(edivabvpc)
037400170822     C                   close(e)  edivabvpc
037500170822     C                   endif
037600170822     C*
037700170822     C                   endif
037800170822     C*
037900170822     C                   endsr
038000170419
038100170908
038200170908
038300170908     C     uplTO_AN_EB   begsr
038400170908     C*
038500170908     C* Se richiesto UPLOAD TO ... Filiale ANNULATO
038600170908     C                   if        §VPCUPLTO = 'A'
038700171127     C                             or §VPCUPLTO = 'B'
038800170908     C*
038900170908     C* *** Elaborazione file VAB ***
039000170908     C*
039100170908     C* Apertura file
039200170908     C                   if        not %open(edivabvpc)
039300170908     C                   open(e)   edivabvpc
039400170908     C                   endif
039500170908     C*
039600170908     C* Scorro tutto il file
039700170908     C                   if        not %error
039800170908     C     *start        setll     edivabvpc
039900170908     C                   read      edivabvpc
040000170908     C                   dow       not %eof(edivabvpc)
040100170908     C*
040200170908     C* Annullo applicativamente il record corrente
040300171021     C                   eval      E_VABFGS = %dec(fgsANN:3:0)
040400170908     C*
040500170908     C* Aggiorno il recordo corrente
040600170908     C                   update    edivab00
040700170908     C*
040800170908     C* Proseguo lettura dati spedizioni
040900170908     C                   read      edivabvpc
041000170908     C                   enddo
041100170908     C                   endif
041200170908     C*
041300170908     C* Chiusura file
041400170908     C                   if        %open(edivabvpc)
041500170908     C                   close(e)  edivabvpc
041600170908     C                   endif
041700170908     C*
041800170908     C                   endif
041900170908     C*
042000170908     C                   endsr
042100170908
042200170908
042300170908
042400170908     C     uplTO_AN_ET   begsr
042500170908     C*
042600170908     C* Se richiesto UPLOAD TO ... Filiale ANNULATO
042700170908     C                   if        §VPCUPLTO = 'A'
042800171127     C                             or §VPCUPLTO = 'B'
042900170908     C*
043000170908     C* *** Elaborazione file VAT ***
043100170908     C*
043200170908     C* Apertura file
043300170908     C                   if        not %open(edivatvpc)
043400170908     C                   open(e)   edivatvpc
043500170908     C                   endif
043600170908     C*
043700170908     C* Scorro tutto il file
043800170908     C                   if        not %error
043900170908     C     *start        setll     edivatvpc
044000170908     C                   read      edivatvpc
044100170908     C                   dow       not %eof(edivatvpc)
044200170908     C*
044300170908     C* Annullo applicativamente il record corrente
044400171021     C                   eval      E_VATFGS = %dec(fgsANN:3:0)
044500170908     C*
044600170908     C* Aggiorno il recordo corrente
044700170908     C                   update    edivat00
044800170908     C*
044900170908     C* Proseguo lettura dati spedizioni
045000170908     C                   read      edivatvpc
045100170908     C                   enddo
045200170908     C                   endif
045300170908     C*
045400170908     C* Chiusura file
045500170908     C                   if        %open(edivatvpc)
045600170908     C                   close(e)  edivatvpc
045700170908     C                   endif
045800170908     C*
045900170908     C                   endif
046000170908     C*
046100170908     C                   endsr
046200170908
046300170419
046400170419
046500170419     C*
046600170522     C     exeOpe        begsr
046700171003     C*
046800171003     C                   add       1             wCntTot
046900170522     C*
047000170522     C* Chiamata al driver di esecuzione operazioni per Personalizzazioni Clienti
047100170526     C                   eval      iUBS715LIN  = 'IT'
047200170522     C                   eval      iUBS715TID  = *blanks
047300170523     C                   eval      iUBS715ENV  = 'VUL'
047400170522     C                   eval      iUBS715OPZ  = prmCMZAZN
047500170522     C                   eval      iUBS715KSU  = prmKSU
047600170522     C                   eval      iUBS715UNI  = prmI714UNI
047700170824     C                   eval      iUBS715DATI = EDIVABDS
047800170522     C*
047900170522     C                   call      'UBS715R'
048000170526     C                   parm                    iUBS715LIN
048100170522     C                   parm                    iUBS715TID
048200170523     C                   parm                    iUBS715ENV
048300170522     C                   parm                    iUBS715OPZ
048400170522     C                   parm                    iUBS715KSU
048500170522     C                   parm                    iUBS715UNI
048600170522     C                   parm                    iUBS715DATI
048700170522     C                   parm                    oUBS715ESI
048800170526     C                   parm                    oUBS715MSG
048900170803     C                   parm                    oUBS715SI95
049000170523     C                   parm                    oUBS715DATI
049100170522     C*
049200170522     C* Se occorso errore => recepisco
049300170522     C                   if        oUBS715ESI <> *blanks
049400171003     C                   add       1             wCntErr
049500170801     C                   else
049600170824     C                   eval      EDIVABDS = iUBS715DATI
049700170522     C                   endif
049800170419     C*
049900170419     C                   endsr
050000170822
050100170822
050200170822
050300170822     C*
050400170822     C     rtvSUN        begsr
050500170822     C*
050600170822     C                   clear                   wSUN
050700170822     C*
050800170822     C/EXEC SQL
050900170822     C+ declare C1 cursor for
051000170822     C+ SELECT T3EW§SUN FROM TABEL3EWV WHERE T3EW§CCM = :wCCM
051100170822     C+ for read only
051200170822     C/END-EXEC
051300170822     C
051400170822     C/EXEC SQL
051500170822     C+ open C1
051600170822     C/END-EXEC
051700170822     C
051800170822     C/EXEC SQL
051900170822     C+ Fetch C1 into :wSUN
052000170822     C/END-EXEC
052100170822     C
052200170822     C/EXEC SQL
052300170822     C+ close C1
052400170822     C/END-EXEC
052500170822     C*
052600170822     C                   endsr
052700170823
052800170823
052900170823
053000170823     C*
053100170823     C     wriEsyVAB     begsr
053200170823     C*
053300170823     C* Verifico/imposto campi "EDI"
053400170823     C                   if        vabCMR = *blanks
053500170823     C                   eval      vabCMR = sCMR
053600170823     C                   endif
053700170823     C*
053800170823     C                   if        vabDCM = *zeros
053900170823     C                   eval      vabDCM = sDATCOR
054000170823     C                   endif
054100170823     C*
054200170823     C                   if        vabDTS = *zeros
054300170823     C                   eval      vabDTS = sDATCOR
054400170823     C                   endif
054500170823     C*
054600170823     C                   if        vabHMS = *zeros
054700170823     C                   eval      vabHMS = sORACOR
054800170823     C                   endif
054900170823     C*
055000170823     C                   if        vabCNT = *zeros
055100170823     C                   eval      vabCNT = 1
055200170823     C                   endif
055300170823     C*
055400170823     C                   eval      vabSUN = wSUN
055500170823     C*
055600170823     C* Infine scarico il buffer di output
055700170823     C                   write     esyvab00
055800170823     C*
055900170823     C                   endsr
056000170823
056100170823
056200170823
056300170823     C*
056400170823     C     wriEsyVAT     begsr
056500170823     C*
056600170823     C* Verifico/imposto campi "EDI"
056700170823     C                   if        vatCMR = *blanks
056800170823     C                   eval      vatCMR = sCMR
056900170823     C                   endif
057000170823     C*
057100170823     C                   if        vatCNT = *zeros
057200170823     C                   eval      vatCNT = 1
057300170823     C                   endif
057400170823     C*
057500170823     C                   eval      vabSUN = wSUN
057600170823     C*
057700170823     C* Infine scarico il buffer di output
057800170823     C                   write     esyvat00
057900170823     C*
058000170823     C                   endsr
058100170821
058200170821
058300170821
058400170821     C*
058500170822 xxx C     uplTO_DL_EB   begsr
058600170824     C*
058700170824     C* Se richiesto UPLOAD TO ... DOWNLOAD
058800170824     C                   if        §VPCUPLTO = 'D'
058900171127     C                             or §VPCUPLTO = 'B'
059000170824     C*
059100170824     C* Inizializzo variabili d wrk
059200170824     C                   movel     'N'           wProcedi          1
059300170824     C*
059400170824     C* Apertura file
059500170824     C                   if        not %open(tivgd00f)
059600170824     C                   open      tivgd00f
059700170824     C                   endif
059800170824     C*
059900170824     C* Stacco progressivo univoco download
060000170824     C                   CLEAR                   TIS7VASDS
060100170824     C                   EVAL      i§7VASOPZ = 'PRG'
060200170824     C                   CALL(e)   'TIS7VASR1'
060300170824     C                   PARM                    TIS7VASDS
060400170824     C*
060500170824     C* Se OK => proseguo
060600170824     C                   if        not %error AND
060700170824     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
060800170824     C                   movel     'S'           wProcedi
060900170824     C                   endif
061000170824     C*
061100170824     C* Se ok a procedere => elaboro
061200170824     C                   if        wProcedi = 'S'
061300170821     C*
061400170821     C/EXEC SQL
061500170822     C+ declare C2 cursor for
061600170901     C+ select edivabvpc.*,
061700170901     C+        ifnull(edivatvpc.vattrc, ' '),
061800170901     C+        ifnull(edivatvpc.vatnot, ' ')
061900170821     C+ from edivabvpc left join edivatvpc
062000170821     C+ on vabcmr=vatcmr and vabcnt=vatcnt and vabccm=vatccm and
062100170821     C+    vabaas=vataas and vablnp=vatlnp and
062200170821     C+    vabnrs=vatnrs and vabnsp=vatnsp
062300170821     C+ order by vabcmr, vabcnt, vabccm,
062400170821     C+          vabaas, vablnp, vabnrs, vabnsp, vattrc
062500170821     C+ for read only
062600170821     C/END-EXEC
062700170821     C
062800170821     C/EXEC SQL
062900170822     C+ open C2
063000170821     C/END-EXEC
063100170821     C
063200170821     C/EXEC SQL
063300170824     C+ Fetch C2 into :EDIVABDS, :VATTRC, :VATNOT
063400170821     C/END-EXEC
063500170821     C*
063600170821     C                   dow       sqlcod = *zeros
063700170901     C*
063800170901     C                   eval      wBufferOut = EDIVABDS +
063900170901     C                                          VATTRC +
064000170901     C                                          VATNOT
064100171228     C*
064200171228     C                   eval      wLenBuff = %size(EDIVABDS) +
064300171228     C                              %size(VATTRC) +
064400171228     C                              %size(VATNOT)
064500171228     C* richiamo User Exit per vedere se c'è qualcosa da aggiungere al buffer per il cliente
064600171228     C                   call      'TIS714UE'
064700171228     C* cliente
064800171228     C                   parm                    VABCCM
064900171228     C* dove sono
065000171228     C                   parm      'UPLTO_DL_EB' wCaller
065100171228     C* lunghezza buffer
065200171228     C                   parm                    wBufferOut
065300171228     C* buffer
065400171228     C                   parm                    wLenBuff
065500171228     C
065600170901     C*
065700170901     C                   exsr      wriVGD
065800170821     C*
065900170821     C/EXEC SQL
066000170824     C+ Fetch C2 into :EDIVABDS, :VATTRC, :VATNOT
066100170821     C/END-EXEC
066200170821     C*
066300170821     C                   enddo
066400170821     C*
066500170821     C/EXEC SQL
066600170822     C+ close C2
066700170821     C/END-EXEC
066800170824     C*
066900170824     C* Chiusura file
067000170824     C                   if        %open(tivgd00f)
067100170824     C                   close(e)  tivgd00f
067200170824     C                   endif
067300170824     C*
067400170824     C* Finalizzo la transazione
067500170824     C                   EVAL      i§7VASOPZ  = 'RLS'
067600170824     C                   EVAL      i§7VASTIP  = dftTIPF
067700170824     C                   EVAL      i§7VASKSU  = '0'+prmKSU
067800170824     C                   EVAL      i§7VASTSC  = 'WW'
067900170824     C                   EVAL      i§7VASSTO  = '?'
068000170824     C                   EVAL      i§7VASSTTO = 'R'
068100170824     C                   EVAL      i§7VASPRG  = o§7VASPRG
068200170824     C                   CALL(e)   'TIS7VASR1'
068300170824     C                   PARM                    TIS7VASDS
068400170824     C*
068500170824     C* Verifico esito chiamata
068600170824     C                   if        %error OR o§7VASOK = *off
068700170824     C                   exsr      EXEERR
068800170824     C                   endif
068900170824     C*
069000170824     C                   endif
069100170821     C*
069200170822     C* Dopo il ridirezionamento output su DOWNLOAD => svuoto file dati UPLOAD corrente
069300171127     C* non per i flag che si aspettano di fare qualcos'altro su EDIVAB
069400171127     C                   if        §VPCUPLTO = 'D'
069500170821     C*
069600170821     C/EXEC SQL
069700170821     C+ delete from edivabvpc
069800170821     C/END-EXEC
069900170821     C*
070000170821     C/EXEC SQL
070100170821     C+ delete from edivatvpc
070200170821     C/END-EXEC
070300170821     C*
070400170824     C                   endif
070500171127     C*
070600171127     C                   endif
070700170824     C*
070800170821     C                   endsr
070900170901
071000170901
071100170901
071200170901     C     wriVGD        BEGSR
071300170901     C*
071400170901     C                   clear                   tivgd000
071500170901     C                   eval      vgdDTA = wBufferOut
071600170901     C                   eval      vgdTIP = dftTIPF
071700170901     C                   eval      vgdKSU = '0'+prmKSU
071800170901     C                   eval      vgdTSC = 'WW'
071900170901     C                   eval      vgdDAT = sDATCOR
072000170901     C                   eval      vgdPRG = o§7VASPRG
072100170901     C                   eval      vgdPGM = procname
072200170901     C                   eval      vgdSTO = '?'
072300170901     C                   write     tivgd000
072400170901     C*
072500170901     C                   ENDSR
072600170824
072700170824
072800170824
072900170824     C     EXEERR        BEGSR
073000170824     C*
073100170824     C                   eval      prmO714ESI = '2'
073200170824     C                   rolbk(e)
073300170824     C                   seton                                        lr
073400170824     C                   return
073500170824     C*
073600170824     C                   ENDSR
073700170824
073800170824
073900170824
074000170824     C*
074100170824     C     carTab        begsr
074200170824     C*
074300170824     C* Reperimento TIPO FILE di DEFAULT "VASWS"
074400170824     C                   eval      iS713TBECOD = 'VPR'
074500170824     C                   eval      iS713TBEKE1 = '*VASWS'
074600170824     C                   eval      iS713TBEKE2 = '*DFTTIPF'
074700170824     C                   eval      iS713TBELIN = *blanks
074800170824     C                   eval      iS713TBESIF = *blanks
074900170824     C                   call      'TIS713R'
075000170824     C                   parm                    iS713TBECOD
075100170824     C                   parm                    iS713TBEKE1
075200170824     C                   parm                    iS713TBEKE2
075300170824     C                   parm                    iS713TBELIN
075400170824     C                   parm                    iS713TBESIF
075500170824     C                   parm                    oS713TBEUNI
075600170824     C                   parm                    oS713ESITO
075700170824     C*
075800170824     C                   if        oS713ESITO = '1'
075900170824     C                   eval      dftTIPF = %trim(oS713TBEUNI)
076000170824     C                   else
076100170824     C                   eval      dftTIPF = 'EB'
076200170824     C                   endif
076300171021     C*
076400171021     C* Reperimento VABFGS di "ANNULLAMENTO"
076500171021     C                   eval      iS713TBECOD = 'VPR'
076600171021     C                   eval      iS713TBEKE1 = '*VAS'
076700171021     C                   eval      iS713TBEKE2 = '*FGSANN'
076800171021     C                   eval      iS713TBELIN = *blanks
076900171021     C                   eval      iS713TBESIF = *blanks
077000171021     C                   call      'TIS713R'
077100171021     C                   parm                    iS713TBECOD
077200171021     C                   parm                    iS713TBEKE1
077300171021     C                   parm                    iS713TBEKE2
077400171021     C                   parm                    iS713TBELIN
077500171021     C                   parm                    iS713TBESIF
077600171021     C                   parm                    oS713TBEUNI
077700171021     C                   parm                    oS713ESITO
077800171021     C*
077900171021     C                   if        oS713ESITO = '1'
078000171021     C                   eval      fgsANN = %trim(oS713TBEUNI)
078100171021     C                   else
078200171021     C                   eval      fgsANN = '998'
078300171021     C                   endif
078400170824     C*
078500170824     C                   endsr
078600170824     C*
078700000714
078800050920
078900050920
079000000714      /TITLE Routine di *init PGM
079100000714     C*
079200000714     C     *inzsr        begsr
079300060103     C*
079400060103     C     *entry        plist
079500170419     C                   parm                    prmFmtFil         2
079600170419     C                   parm                    prmCMZAZN         4
079700170412     C                   parm                    prmKSU            7
079800170412     C                   parm                    prmI714UNI      256
079900170412     C                   parm                    prmO714ESI        1
080000000714     C*
080100000714     C                   endsr
080200000714
