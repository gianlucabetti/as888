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
004400180312     D  FNLV55DS     e ds                  inz
004500170822     D*
004600170822     D wCCM            s                   inz like(vabCCM)
004700170822     D wSUN            s                   inz like(vabSUN)
004800170823     D sCMR            s                   inz like(vabCMR)
004900170823     D sDATCOR         s                   inz like(vabDTS)
005000170823     D sORACOR         s                   inz like(vabHMS)
005100170824     D dftTIPF         s              2A
005200171021     D fgsANN          s              3A
005300170901     D wBufferOut      s           2048    inz
005400171003     D wCntTot         s              9S 0 inz
005500171003     D wCntErr         s              9S 0 inz
005600171228     D wCaller         s             15    inz
005700171228     D wLenBuff        s              4S 0 inz
005800180312     D wTMA            s              3S 0 inz
005900170901     D*
006000170901     D psds           sds
006100170901     D  procname         *PROC
006200010525
006300010525
006400010525     C*-----------------------------------------------------------------------------
006500170818     C*
006600170818     C* Definisco le opzioni con cui verranno di seguito utilizzate le istruzioni SQL
006700170818     C
006800170818     C/EXEC SQL
006900170818     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007000170818     C/END-EXEC
007100170412     C*
007200170412     C* Avvio il monitoring del intero flusso
007300170412     C                   monitor
007400170824     C*
007500170824     C* Caricamento tabella
007600170824     C                   exsr      carTab
007700170821     C*
007800170821     C* Ridefinisco dati tabella VPC ricevuti in input
007900170901     C                   eval      DVPC = prmI714UNI
008000170901     C*
008100170901     C* Imposto subito alcuni valori costanti
008200170901     C                   eval      sCMR    = %char(%timestamp(*SYS))
008300170901     C                   eval      sDATCOR = %dec(%date() : *iso)
008400170901     C                   eval      sORACOR = %dec(%time() : *iso)
008500170412     C*
008600170412     C* In base al tipo di file indicato in input => leggo
008700170412     C                   select
008800170419     C                   when      prmFmtFil = 'FB'
008900170412     C                   exsr      exeFNVAB
009000170824     C                   exsr      uplTO_EW_FB
009100170822 xxx C***                exsr      uplTO_DL_FB
009200170419     C                   when      prmFmtFil = 'EB'
009300170412     C                   exsr      exeEDIVAB
009400170822     C                   exsr      uplTO_EW_EB
009500170901     C                   exsr      uplTO_DL_EB
009600171127 xxx C                   exsr      uplTO_AN_EB
009700170908     C                   when      prmFmtFil = 'ET'
009800170908 xxx C                   exsr      uplTO_AN_ET
009900170412     C                   other
010000170801     C***                eval      prmO714ESI = '2'
010100170412     C                   endsl
010200170412     C*
010300170412     C* Gestisco eventuale errore
010400170412     C                   on-error
010500170518     C
010600170824     C                   exsr      EXEERR
010700170412     C*
010800170412     C* Arresto il monitoring
010900170412     C                   endmon
011000100624     C*
011100171016     C                   seton                                        lr
011200010525     C*
011300010525     C*-----------------------------------------------------------------------------
011400170412
011500170412
011600170412
011700171228     C     exeFNVAB      begsr
011800171003     C*
011900171003     C* Inizializzazione contatori di esito chiamata
012000171003     C                   clear                   wCntTot
012100171003     C                   clear                   wCntErr
012200170420     C*
012300170420     C* Apertura file
012400170420     C                   if        not %open(fivabvpc)
012500170822     C                   open(e)   fivabvpc
012600170420     C                   endif
012700170412     C*
012800170412     C* Scorro tutto il file
012900170822     C                   if        not %error
013000170419     C     *start        setll     fivabvpc
013100170412     C                   read      fivabvpc
013200170523     C                   dow       not %eof(fivabvpc)
013300170412     C*
013400170821     C* Normalizzo verso formato EDIVAB
013500170824     C                   clear                   EDIVABDS
013600170419     C                   eval      FIVABDS = FIVABDS_F
013700170824     C                   eval-corr EDIVABDS = FIVABDS
013800170419     C*
013900170419     C* Eseguo operazioni
014000170522     C                   exsr      exeOpe
014100170801     C*
014200170801     C* Aggiorno dati modificati
014300170824     C                   eval-corr FIVABDS = EDIVABDS
014400170801     C                   eval      FIVABDS_F = FIVABDS
014500170801     C                   update    fivab000
014600170412     C*
014700170516     C* Proseguo lettura dati spedizioni
014800170412     C                   read      fivabvpc
014900170412     C                   enddo
015000170822     C                   endif
015100170420     C*
015200170420     C* Chiusura file
015300170420     C                   if        %open(fivabvpc)
015400170822     C                   close(e)  fivabvpc
015500170420     C                   endif
015600171003     C*
015700171003     C* Verifica esito esecuzione Opzioni
015800171003     C                   if        wCntErr = wCntTot
015900171003     C                   eval      prmO714ESI = '2'
016000171003     C                   endif
016100170412     C*
016200170412     C                   endsr
016300170412
016400170412
016500170412
016600170412     C     exeEDIVAB     begsr
016700171003     C*
016800171003     C* Inizializzazione contatori di esito chiamata
016900171003     C                   clear                   wCntTot
017000171003     C                   clear                   wCntErr
017100170420     C*
017200170420     C* Apertura file
017300170420     C                   if        not %open(edivabvpc)
017400170822     C                   open(e)   edivabvpc
017500170420     C                   endif
017600170412     C*
017700170412     C* Scorro tutto il file
017800170822     C                   if        not %error
017900170419     C     *start        setll     edivabvpc
018000170412     C                   read      edivabvpc
018100170523     C                   dow       not %eof(edivabvpc)
018200170801     C*
018300170821     C* Normalizzo verso formato EDIVAB
018400170824     C                   clear                   EDIVABDS
018500170824     C                   eval      EDIVABDS = EDIVABDS_E
018600170412     C*
018700170419     C* Eseguo operazioni
018800170412     C                   exsr      exeOpe
018900170801     C*
019000170801     C* Aggiorno dati modificati
019100170824     C                   eval      EDIVABDS_E = EDIVABDS
019200170801     C                   update    edivab00
019300170412     C*
019400170516     C* Proseguo lettura dati spedizioni
019500170419     C                   read      edivabvpc
019600170412     C                   enddo
019700170822     C                   endif
019800170420     C*
019900170420     C* Chiusura file
020000170420     C                   if        %open(edivabvpc)
020100170822     C                   close(e)  edivabvpc
020200170420     C                   endif
020300171003     C*
020400171003     C* Verifica esito esecuzione Opzioni
020500171003     C                   if        wCntErr = wCntTot
020600171003     C                   eval      prmO714ESI = '2'
020700171003     C                   endif
020800170412     C*
020900170412     C                   endsr
021000170824
021100170824
021200170824
021300170824     C     uplTO_EW_FB   begsr
021400170824     C*
021500170824     C* Se richiesto UPLOAD TO ... EasySpedweb
021600170824     C                   if        §VPCUPLTO = 'E'
021700170824     C*
021800170824     C* *** Elaborazione file VAT ***
021900170824     C*
022000170824     C* Apertura file
022100170824     C                   if        not %open(fivabvpc)
022200170824     C                   open(e)   fivatvpc
022300170824     C                   endif
022400170824     C*
022500170824     C* Scorro tutto il file
022600170824     C                   if        not %error
022700170824     C     *start        setll     fivatvpc
022800170824     C                   read      fivatvpc
022900170824     C                   dow       not %eof(fivatvpc)
023000170824     C*
023100170824     C* Normalizzo verso formato EDIVAT
023200170824     C                   clear                   EDIVATDS
023300170824     C                   eval      FIVATDS = FIVATDS_F
023400170824     C                   eval-corr EDIVATDS = FIVATDS
023500170824     C*
023600170824     C* Reperisco lo Strategi User Number del cliente corrente
023700170824     C                   eval      wCCM = vatCCM
023800170824     C                   exsr      rtvSUN
023900170824     C*
024000170824     C* Scrivo file "transitorio EasySpedWeb"
024100170824     C                   exsr      wriEsyVAT
024200170824     C*
024300170824     C* Se OK scrittura file "transitorio EasySpedWeb" => elimino file dati UPLOAD corrente
024400170824     C                   delete    fivatvpc
024500170824     C*
024600170824     C* Proseguo lettura dati spedizioni
024700170824     C                   read      fivatvpc
024800170824     C                   enddo
024900170824     C                   endif
025000170824     C*
025100170824     C* Chiusura file
025200170824     C                   if        %open(fivatvpc)
025300170824     C                   close(e)  fivatvpc
025400170824     C                   endif
025500170824     C*
025600170824     C* *** Elaborazione file VAB ***
025700170824     C*
025800170824     C* Apertura file
025900170824     C                   if        not %open(fivabvpc)
026000170824     C                   open(e)   fivabvpc
026100170824     C                   endif
026200170824     C*
026300170824     C* Scorro tutto il file
026400170824     C                   if        not %error
026500170824     C     *start        setll     fivabvpc
026600170824     C                   read      fivabvpc
026700170824     C                   dow       not %eof(fivabvpc)
026800170824     C*
026900170824     C* Normalizzo verso formato EDIVAB
027000170824     C                   clear                   EDIVABDS
027100170824     C                   eval      FIVABDS = FIVABDS_F
027200170824     C                   eval-corr EDIVABDS = FIVABDS
027300170824     C*
027400170824     C* Reperisco lo Strategi User Number del cliente corrente
027500170824     C                   eval      wCCM = vabCCM
027600170824     C                   exsr      rtvSUN
027700170824     C*
027800170824     C* Scrivo file "transitorio EasySpedWeb"
027900170824     C                   exsr      wriEsyVAB
028000170824     C*
028100170824     C* Se OK scrittura file "transitorio EasySpedWeb" => elimino file dati UPLOAD corrente
028200170824     C                   delete    fivabvpc
028300170824     C*
028400170824     C* Proseguo lettura dati spedizioni
028500170824     C                   read      fivabvpc
028600170824     C                   enddo
028700170824     C                   endif
028800170824     C*
028900170824     C* Chiusura file
029000170824     C                   if        %open(fivabvpc)
029100170824     C                   close(e)  fivabvpc
029200170824     C                   endif
029300170824     C*
029400170824     C                   endif
029500170824     C*
029600170824     C                   endsr
029700170822
029800170822
029900170822
030000170822     C     uplTO_EW_EB   begsr
030100170822     C*
030200170822     C* Se richiesto UPLOAD TO ... EasySpedweb
030300170822     C                   if        §VPCUPLTO = 'E'
030400170822     C*
030500170822     C* *** Elaborazione file VAT ***
030600170822     C*
030700170822     C* Apertura file
030800170822     C                   if        not %open(edivatvpc)
030900170822     C                   open(e)   edivatvpc
031000170822     C                   endif
031100170822     C*
031200170822     C* Scorro tutto il file
031300170822     C                   if        not %error
031400170822     C     *start        setll     edivatvpc
031500170822     C                   read      edivatvpc
031600170822     C                   dow       not %eof(edivatvpc)
031700170822     C*
031800170822     C* Normalizzo verso formato EDIVAT
031900170824     C                   clear                   EDIVATDS
032000170824     C                   eval      EDIVATDS = EDIVATDS_E
032100170822     C*
032200170822     C* Reperisco lo Strategi User Number del cliente corrente
032300170822     C                   eval      wCCM = vatCCM
032400170822     C                   exsr      rtvSUN
032500170822     C*
032600170822     C* Scrivo file "transitorio EasySpedWeb"
032700170823     C                   exsr      wriEsyVAT
032800170822     C*
032900170822     C* Se OK scrittura file "transitorio EasySpedWeb" => elimino file dati UPLOAD corrente
033000170822     C                   delete    edivatvpc
033100170822     C*
033200170822     C* Proseguo lettura dati spedizioni
033300170822     C                   read      edivatvpc
033400170822     C                   enddo
033500170822     C                   endif
033600170822     C*
033700170822     C* Chiusura file
033800170822     C                   if        %open(edivatvpc)
033900170822     C                   close(e)  edivatvpc
034000170822     C                   endif
034100170822     C*
034200170822     C* *** Elaborazione file VAB ***
034300170822     C*
034400170822     C* Apertura file
034500170822     C                   if        not %open(edivabvpc)
034600170822     C                   open(e)   edivabvpc
034700170822     C                   endif
034800170822     C*
034900170822     C* Scorro tutto il file
035000170822     C                   if        not %error
035100170822     C     *start        setll     edivabvpc
035200170822     C                   read      edivabvpc
035300170822     C                   dow       not %eof(edivabvpc)
035400170822     C*
035500170822     C* Normalizzo verso formato EDIVAB
035600170824     C                   clear                   EDIVABDS
035700170824     C                   eval      EDIVABDS = EDIVABDS_E
035800170822     C*
035900170822     C* Reperisco lo Strategi User Number del cliente corrente
036000170822     C                   eval      wCCM = vabCCM
036100170822     C                   exsr      rtvSUN
036200170822     C*
036300170822     C* Scrivo file "transitorio EasySpedWeb"
036400170823     C                   exsr      wriEsyVAB
036500170822     C*
036600170822     C* Se OK scrittura file "transitorio EasySpedWeb" => elimino file dati UPLOAD corrente
036700170822     C                   delete    edivabvpc
036800170822     C*
036900170822     C* Proseguo lettura dati spedizioni
037000170822     C                   read      edivabvpc
037100170822     C                   enddo
037200170822     C                   endif
037300170822     C*
037400170822     C* Chiusura file
037500170822     C                   if        %open(edivabvpc)
037600170822     C                   close(e)  edivabvpc
037700170822     C                   endif
037800170822     C*
037900170822     C                   endif
038000170822     C*
038100170822     C                   endsr
038200170419
038300170908
038400170908
038500170908     C     uplTO_AN_EB   begsr
038600170908     C*
038700170908     C* Se richiesto UPLOAD TO ... Filiale ANNULATO
038800170908     C                   if        §VPCUPLTO = 'A'
038900171127     C                             or §VPCUPLTO = 'B'
039000170908     C*
039100170908     C* *** Elaborazione file VAB ***
039200170908     C*
039300170908     C* Apertura file
039400170908     C                   if        not %open(edivabvpc)
039500170908     C                   open(e)   edivabvpc
039600170908     C                   endif
039700170908     C*
039800170908     C* Scorro tutto il file
039900170908     C                   if        not %error
040000170908     C     *start        setll     edivabvpc
040100170908     C                   read      edivabvpc
040200170908     C                   dow       not %eof(edivabvpc)
040300170908     C*
040400170908     C* Annullo applicativamente il record corrente
040500171021     C                   eval      E_VABFGS = %dec(fgsANN:3:0)
040600170908     C*
040700170908     C* Aggiorno il recordo corrente
040800170908     C                   update    edivab00
040900170908     C*
041000170908     C* Proseguo lettura dati spedizioni
041100170908     C                   read      edivabvpc
041200170908     C                   enddo
041300170908     C                   endif
041400170908     C*
041500170908     C* Chiusura file
041600170908     C                   if        %open(edivabvpc)
041700170908     C                   close(e)  edivabvpc
041800170908     C                   endif
041900170908     C*
042000170908     C                   endif
042100170908     C*
042200170908     C                   endsr
042300170908
042400170908
042500170908
042600170908     C     uplTO_AN_ET   begsr
042700170908     C*
042800170908     C* Se richiesto UPLOAD TO ... Filiale ANNULATO
042900170908     C                   if        §VPCUPLTO = 'A'
043000171127     C                             or §VPCUPLTO = 'B'
043100170908     C*
043200170908     C* *** Elaborazione file VAT ***
043300170908     C*
043400170908     C* Apertura file
043500170908     C                   if        not %open(edivatvpc)
043600170908     C                   open(e)   edivatvpc
043700170908     C                   endif
043800170908     C*
043900170908     C* Scorro tutto il file
044000170908     C                   if        not %error
044100170908     C     *start        setll     edivatvpc
044200170908     C                   read      edivatvpc
044300170908     C                   dow       not %eof(edivatvpc)
044400170908     C*
044500170908     C* Annullo applicativamente il record corrente
044600171021     C                   eval      E_VATFGS = %dec(fgsANN:3:0)
044700170908     C*
044800170908     C* Aggiorno il recordo corrente
044900170908     C                   update    edivat00
045000170908     C*
045100170908     C* Proseguo lettura dati spedizioni
045200170908     C                   read      edivatvpc
045300170908     C                   enddo
045400170908     C                   endif
045500170908     C*
045600170908     C* Chiusura file
045700170908     C                   if        %open(edivatvpc)
045800170908     C                   close(e)  edivatvpc
045900170908     C                   endif
046000170908     C*
046100170908     C                   endif
046200170908     C*
046300170908     C                   endsr
046400170908
046500170419
046600170419
046700170419     C*
046800170522     C     exeOpe        begsr
046900171003     C*
047000171003     C                   add       1             wCntTot
047100180312     C*
047200180312     C* se è stata chiesta una maschera file etichette col Terminal di arrivo,
047300180312     C                   if        §VPCSGN = 'TA'
047400180312     C* calcolo il terminal di arrivo
047500180312     C                   clear                   wTMA
047600180312     C                   clear                   fnlv55ds
047700180312     C                   eval      d55LNP = VABLNP
047800180312     C                   eval      d55LIN = VABLNA
047900180312     C                   eval      d55TPT = 'A'
048000180312     C                   eval      d55DRF = VABAAS*10000+
048100180312     C                                      VABMGS
048200180312     C                   call      'FNLV55R'
048300180312     C                   parm                    fnlv55ds
048400180312     C                   eval      wTMA = d55TFA
048500180312     C                   endif
048600170522     C*
048700170522     C* Chiamata al driver di esecuzione operazioni per Personalizzazioni Clienti
048800170526     C                   eval      iUBS715LIN  = 'IT'
048900170522     C                   eval      iUBS715TID  = *blanks
049000180312     C                   eval      iUBS715DATI = EDIVABDS
049100180312     C                   select
049200180312     C* se è stata chiesta una maschera file etichette col Terminal di arrivo,
049300180312     C                   when      §VPCSGN = 'TA'
049400180312     C* aggiungo ai dati il terminal e cambio l'ID
049500180312     C                   eval      iUBS715TID  = '1'
049600180313     C                   eval      iUBS715DATI = EDIVABDS +
049700180313     C                                           %editc(wTMA:'X')
049800180312     C                   endsl
049900170523     C                   eval      iUBS715ENV  = 'VUL'
050000170522     C                   eval      iUBS715OPZ  = prmCMZAZN
050100170522     C                   eval      iUBS715KSU  = prmKSU
050200170522     C                   eval      iUBS715UNI  = prmI714UNI
050300170522     C*
050400170522     C                   call      'UBS715R'
050500170526     C                   parm                    iUBS715LIN
050600170522     C                   parm                    iUBS715TID
050700170523     C                   parm                    iUBS715ENV
050800170522     C                   parm                    iUBS715OPZ
050900170522     C                   parm                    iUBS715KSU
051000170522     C                   parm                    iUBS715UNI
051100170522     C                   parm                    iUBS715DATI
051200170522     C                   parm                    oUBS715ESI
051300170526     C                   parm                    oUBS715MSG
051400170803     C                   parm                    oUBS715SI95
051500170523     C                   parm                    oUBS715DATI
051600170522     C*
051700170522     C* Se occorso errore => recepisco
051800170522     C                   if        oUBS715ESI <> *blanks
051900171003     C                   add       1             wCntErr
052000170801     C                   else
052100170824     C                   eval      EDIVABDS = iUBS715DATI
052200170522     C                   endif
052300170419     C*
052400170419     C                   endsr
052500170822
052600170822
052700170822
052800170822     C*
052900170822     C     rtvSUN        begsr
053000170822     C*
053100170822     C                   clear                   wSUN
053200170822     C*
053300170822     C/EXEC SQL
053400170822     C+ declare C1 cursor for
053500170822     C+ SELECT T3EW§SUN FROM TABEL3EWV WHERE T3EW§CCM = :wCCM
053600170822     C+ for read only
053700170822     C/END-EXEC
053800170822     C
053900170822     C/EXEC SQL
054000170822     C+ open C1
054100170822     C/END-EXEC
054200170822     C
054300170822     C/EXEC SQL
054400170822     C+ Fetch C1 into :wSUN
054500170822     C/END-EXEC
054600170822     C
054700170822     C/EXEC SQL
054800170822     C+ close C1
054900170822     C/END-EXEC
055000170822     C*
055100170822     C                   endsr
055200170823
055300170823
055400170823
055500170823     C*
055600170823     C     wriEsyVAB     begsr
055700170823     C*
055800170823     C* Verifico/imposto campi "EDI"
055900170823     C                   if        vabCMR = *blanks
056000170823     C                   eval      vabCMR = sCMR
056100170823     C                   endif
056200170823     C*
056300170823     C                   if        vabDCM = *zeros
056400170823     C                   eval      vabDCM = sDATCOR
056500170823     C                   endif
056600170823     C*
056700170823     C                   if        vabDTS = *zeros
056800170823     C                   eval      vabDTS = sDATCOR
056900170823     C                   endif
057000170823     C*
057100170823     C                   if        vabHMS = *zeros
057200170823     C                   eval      vabHMS = sORACOR
057300170823     C                   endif
057400170823     C*
057500170823     C                   if        vabCNT = *zeros
057600170823     C                   eval      vabCNT = 1
057700170823     C                   endif
057800170823     C*
057900170823     C                   eval      vabSUN = wSUN
058000170823     C*
058100170823     C* Infine scarico il buffer di output
058200170823     C                   write     esyvab00
058300170823     C*
058400170823     C                   endsr
058500170823
058600170823
058700170823
058800170823     C*
058900170823     C     wriEsyVAT     begsr
059000170823     C*
059100170823     C* Verifico/imposto campi "EDI"
059200170823     C                   if        vatCMR = *blanks
059300170823     C                   eval      vatCMR = sCMR
059400170823     C                   endif
059500170823     C*
059600170823     C                   if        vatCNT = *zeros
059700170823     C                   eval      vatCNT = 1
059800170823     C                   endif
059900170823     C*
060000170823     C                   eval      vabSUN = wSUN
060100170823     C*
060200170823     C* Infine scarico il buffer di output
060300170823     C                   write     esyvat00
060400170823     C*
060500170823     C                   endsr
060600170821
060700170821
060800170821
060900170821     C*
061000170822 xxx C     uplTO_DL_EB   begsr
061100170824     C*
061200170824     C* Se richiesto UPLOAD TO ... DOWNLOAD
061300170824     C                   if        §VPCUPLTO = 'D'
061400171127     C                             or §VPCUPLTO = 'B'
061500170824     C*
061600170824     C* Inizializzo variabili d wrk
061700170824     C                   movel     'N'           wProcedi          1
061800170824     C*
061900170824     C* Apertura file
062000170824     C                   if        not %open(tivgd00f)
062100170824     C                   open      tivgd00f
062200170824     C                   endif
062300170824     C*
062400170824     C* Stacco progressivo univoco download
062500170824     C                   CLEAR                   TIS7VASDS
062600170824     C                   EVAL      i§7VASOPZ = 'PRG'
062700170824     C                   CALL(e)   'TIS7VASR1'
062800170824     C                   PARM                    TIS7VASDS
062900170824     C*
063000170824     C* Se OK => proseguo
063100170824     C                   if        not %error AND
063200170824     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
063300170824     C                   movel     'S'           wProcedi
063400170824     C                   endif
063500170824     C*
063600170824     C* Se ok a procedere => elaboro
063700170824     C                   if        wProcedi = 'S'
063800170821     C*
063900170821     C/EXEC SQL
064000170822     C+ declare C2 cursor for
064100170901     C+ select edivabvpc.*,
064200170901     C+        ifnull(edivatvpc.vattrc, ' '),
064300170901     C+        ifnull(edivatvpc.vatnot, ' ')
064400170821     C+ from edivabvpc left join edivatvpc
064500170821     C+ on vabcmr=vatcmr and vabcnt=vatcnt and vabccm=vatccm and
064600170821     C+    vabaas=vataas and vablnp=vatlnp and
064700170821     C+    vabnrs=vatnrs and vabnsp=vatnsp
064800170821     C+ order by vabcmr, vabcnt, vabccm,
064900170821     C+          vabaas, vablnp, vabnrs, vabnsp, vattrc
065000170821     C+ for read only
065100170821     C/END-EXEC
065200170821     C
065300170821     C/EXEC SQL
065400170822     C+ open C2
065500170821     C/END-EXEC
065600170821     C
065700170821     C/EXEC SQL
065800170824     C+ Fetch C2 into :EDIVABDS, :VATTRC, :VATNOT
065900170821     C/END-EXEC
066000170821     C*
066100170821     C                   dow       sqlcod = *zeros
066200170901     C*
066300180312     C* se è stata chiesta una maschera file etichette col Terminal di arrivo,
066400180312     C* passo il Terminal dopo i dati del VAB
066500180312     C                   if        §VPCSGN = 'TA'
066600170901     C                   eval      wBufferOut = EDIVABDS +
066700180313     C                                          %editc(wTMA:'X') +
066800180312     C                                          VATTRC +
066900170901     C                                          VATNOT
067000180312     C*
067100180312     C                   eval      wLenBuff = %size(EDIVABDS) +
067200180313     C                              %size(wTMA) +
067300180313     C                              %size(VATTRC) +
067400180312     C                              %size(VATNOT)
067500180312     C* altrimenti non lo passo
067600180312     C                   else
067700180312     C                   eval      wBufferOut = EDIVABDS +
067800180312     C                                          VATTRC +
067900180312     C                                          VATNOT
068000171228     C*
068100171228     C                   eval      wLenBuff = %size(EDIVABDS) +
068200171228     C                              %size(VATTRC) +
068300171228     C                              %size(VATNOT)
068400180312     C                   endif
068500180312     C*
068600171228     C* richiamo User Exit per vedere se c'è qualcosa da aggiungere al buffer per il cliente
068700171228     C                   call      'TIS714UE'
068800171228     C* cliente
068900171228     C                   parm                    VABCCM
069000171228     C* dove sono
069100171228     C                   parm      'UPLTO_DL_EB' wCaller
069200171228     C* lunghezza buffer
069300171228     C                   parm                    wBufferOut
069400171228     C* buffer
069500171228     C                   parm                    wLenBuff
069600171228     C
069700170901     C*
069800170901     C                   exsr      wriVGD
069900170821     C*
070000170821     C/EXEC SQL
070100170824     C+ Fetch C2 into :EDIVABDS, :VATTRC, :VATNOT
070200170821     C/END-EXEC
070300170821     C*
070400170821     C                   enddo
070500170821     C*
070600170821     C/EXEC SQL
070700170822     C+ close C2
070800170821     C/END-EXEC
070900170824     C*
071000170824     C* Chiusura file
071100170824     C                   if        %open(tivgd00f)
071200170824     C                   close(e)  tivgd00f
071300170824     C                   endif
071400170824     C*
071500170824     C* Finalizzo la transazione
071600170824     C                   EVAL      i§7VASOPZ  = 'RLS'
071700170824     C                   EVAL      i§7VASTIP  = dftTIPF
071800170824     C                   EVAL      i§7VASKSU  = '0'+prmKSU
071900170824     C                   EVAL      i§7VASTSC  = 'WW'
072000170824     C                   EVAL      i§7VASSTO  = '?'
072100170824     C                   EVAL      i§7VASSTTO = 'R'
072200170824     C                   EVAL      i§7VASPRG  = o§7VASPRG
072300170824     C                   CALL(e)   'TIS7VASR1'
072400170824     C                   PARM                    TIS7VASDS
072500170824     C*
072600170824     C* Verifico esito chiamata
072700170824     C                   if        %error OR o§7VASOK = *off
072800170824     C                   exsr      EXEERR
072900170824     C                   endif
073000170824     C*
073100170824     C                   endif
073200170821     C*
073300170822     C* Dopo il ridirezionamento output su DOWNLOAD => svuoto file dati UPLOAD corrente
073400171127     C* non per i flag che si aspettano di fare qualcos'altro su EDIVAB
073500171127     C                   if        §VPCUPLTO = 'D'
073600170821     C*
073700170821     C/EXEC SQL
073800170821     C+ delete from edivabvpc
073900170821     C/END-EXEC
074000170821     C*
074100170821     C/EXEC SQL
074200170821     C+ delete from edivatvpc
074300170821     C/END-EXEC
074400170821     C*
074500170824     C                   endif
074600171127     C*
074700171127     C                   endif
074800170824     C*
074900170821     C                   endsr
075000170901
075100170901
075200170901
075300170901     C     wriVGD        BEGSR
075400170901     C*
075500170901     C                   clear                   tivgd000
075600170901     C                   eval      vgdDTA = wBufferOut
075700170901     C                   eval      vgdTIP = dftTIPF
075800170901     C                   eval      vgdKSU = '0'+prmKSU
075900170901     C                   eval      vgdTSC = 'WW'
076000170901     C                   eval      vgdDAT = sDATCOR
076100170901     C                   eval      vgdPRG = o§7VASPRG
076200170901     C                   eval      vgdPGM = procname
076300170901     C                   eval      vgdSTO = '?'
076400170901     C                   write     tivgd000
076500170901     C*
076600170901     C                   ENDSR
076700170824
076800170824
076900170824
077000170824     C     EXEERR        BEGSR
077100170824     C*
077200170824     C                   eval      prmO714ESI = '2'
077300170824     C                   rolbk(e)
077400170824     C                   seton                                        lr
077500170824     C                   return
077600170824     C*
077700170824     C                   ENDSR
077800170824
077900170824
078000170824
078100170824     C*
078200170824     C     carTab        begsr
078300170824     C*
078400170824     C* Reperimento TIPO FILE di DEFAULT "VASWS"
078500170824     C                   eval      iS713TBECOD = 'VPR'
078600170824     C                   eval      iS713TBEKE1 = '*VASWS'
078700170824     C                   eval      iS713TBEKE2 = '*DFTTIPF'
078800170824     C                   eval      iS713TBELIN = *blanks
078900170824     C                   eval      iS713TBESIF = *blanks
079000170824     C                   call      'TIS713R'
079100170824     C                   parm                    iS713TBECOD
079200170824     C                   parm                    iS713TBEKE1
079300170824     C                   parm                    iS713TBEKE2
079400170824     C                   parm                    iS713TBELIN
079500170824     C                   parm                    iS713TBESIF
079600170824     C                   parm                    oS713TBEUNI
079700170824     C                   parm                    oS713ESITO
079800170824     C*
079900170824     C                   if        oS713ESITO = '1'
080000170824     C                   eval      dftTIPF = %trim(oS713TBEUNI)
080100170824     C                   else
080200170824     C                   eval      dftTIPF = 'EB'
080300170824     C                   endif
080400171021     C*
080500171021     C* Reperimento VABFGS di "ANNULLAMENTO"
080600171021     C                   eval      iS713TBECOD = 'VPR'
080700171021     C                   eval      iS713TBEKE1 = '*VAS'
080800171021     C                   eval      iS713TBEKE2 = '*FGSANN'
080900171021     C                   eval      iS713TBELIN = *blanks
081000171021     C                   eval      iS713TBESIF = *blanks
081100171021     C                   call      'TIS713R'
081200171021     C                   parm                    iS713TBECOD
081300171021     C                   parm                    iS713TBEKE1
081400171021     C                   parm                    iS713TBEKE2
081500171021     C                   parm                    iS713TBELIN
081600171021     C                   parm                    iS713TBESIF
081700171021     C                   parm                    oS713TBEUNI
081800171021     C                   parm                    oS713ESITO
081900171021     C*
082000171021     C                   if        oS713ESITO = '1'
082100171021     C                   eval      fgsANN = %trim(oS713TBEUNI)
082200171021     C                   else
082300171021     C                   eval      fgsANN = '998'
082400171021     C                   endif
082500170824     C*
082600170824     C                   endsr
082700170824     C*
082800000714
082900050920
083000050920
083100000714      /TITLE Routine di *init PGM
083200000714     C*
083300000714     C     *inzsr        begsr
083400060103     C*
083500060103     C     *entry        plist
083600170419     C                   parm                    prmFmtFil         2
083700170419     C                   parm                    prmCMZAZN         4
083800170412     C                   parm                    prmKSU            7
083900170412     C                   parm                    prmI714UNI      256
084000170412     C                   parm                    prmO714ESI        1
084100000714     C*
084200000714     C                   endsr
084300000714
