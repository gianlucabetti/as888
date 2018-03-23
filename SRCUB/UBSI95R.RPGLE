000100170428     H datedit(*ymd/) dftactgrp(*no) actgrp(*caller)
000200170428
000300170428
000400170428      //********************************************************************************************
000500170428      //
000600170428      // Definizione file.
000700170428      //
000800170428      //********************************************************************************************
000900170428     Fazorg01l  IF   E           K DISK
001000170428     Ftntbe01l  IF   E           K DISK
001100170428
001200170428
001300170428      //********************************************************************************************
001400170428      //
001500170428      // Definizione interfacce procedure.
001600170428      //
001700170428      //********************************************************************************************
001800170428     D/COPY GAITRASRC/SRCPROTOPI,UBSI95R
001900170428     D/COPY GAITRASRC/SRCPROTOPI,TISI95R1
002000170428
002100170428
002200170428      //********************************************************************************************
002300170428      //
002400170428      // Definizione variabili.
002500170428      //
002600170428      //********************************************************************************************
002700170504     D MSGTAG          S             80A   DIM(30) CTDATA PERRCD(1)
002800170428     D*
002900100503     D MSGITA          S             80A   DIM(5) CTDATA PERRCD(1)
003000100503     D MSGENG          S             80A   DIM(5) CTDATA PERRCD(1)
003100100503     D MSGFRA          S             80A   DIM(5) CTDATA PERRCD(1)
003200100503     D MSGDEU          S             80A   DIM(5) CTDATA PERRCD(1)
003300090810     D*
003400170504     D fnlvd1ds      e ds
003500090810     D tisi95ds      e ds
003600090810     D tisit0ds      e ds
003700090810     D tisit0x1ds    e ds
003800090810     D ddstflo       e ds
003900090810     D fnlv55ds      e ds
004000170825     D trulflbds     e ds
004100170428     D dtba          e ds
004200170428     D dlts          e ds
004300170428     D dcli          e ds
004400090810     D*
004500100504     D wDOS            S              1A
004600100504     D wTFP            S              3S 0
004700170908     D wLNP            S              3S 0
004800100504     D wTFA            S              3S 0
004900100504     D wLNA            S              3S 0
005000170512     D sLNA            S              3S 0
005100100504     D wZNC            S              2S 0
005200170831     D wPRD            S                   LIKE(O95PRV)
005300170831     D wCAD            S                   LIKE(O95CAP)
005400170831     D wLOD            S                   LIKE(O95LOC)
005500170831     D wIND            S                   LIKE(VABIND)
005600090810     D*
005700170428     D skGvaB_C        S              2A   DIM(10)
005800170428     D skGvaB_D        S                   DIM(10) LIKE(dtba)
005900170428     D jGva            S              2S 0
006000170428     D skLtsCod        S              1A   DIM(10)
006100170428     D skLtsDat        S                   DIM(10) LIKE(dlts)
006200170428     D jLts            S              2S 0
006300170428     D skOrgCod        S              3S 0 DIM(300)
006400170428     D skOrgDes        S             20A   DIM(300)
006500100504     D jOrg            S              4S 0
006600100504     D jMsg            S              2S 0
006700170428     D datcor          S              8S 0
006800170428     D wTotPKB         S              7S 1
006900170428     D wTotVLB         S              5S 3
007000091027     D*
007100170504     D                 DS                  INZ
007200170504     D  wAAS                   1      4  0
007300170504     D  wMGS                   5      8  0
007400170504     D wData                   1      8  0
007500170428     D*
007600170428     D wVABANT         DS                  INZ
007700170428     D  wTB1_COD                      2A
007800170428     D  wTB1_TOT                      2  0
007900170428     D  wTB2_COD                      2A
008000170428     D  wTB2_TOT                      2  0
008100170428     D  wFILLER                       1A
008200170428     D*
008300170428
008400170428      /COPY GAITRASRC/SRCUB,UBTL05D02
008500170428     D   pInNazione    S              3A
008600170428     D   pInProvincia  S              4A
008700170428     D   pInComune     S            128A
008800170428     D   pInLocalita   S            128A
008900170428     D   pInCAP        S              9A
009000170428     D   pInVia        S            128A
009100170428     D   pInCivico     S             10A
009200170428     D   pInScore      S             10A
009300170428     D   pOutEsito     S              1A
009400170428     D   pOutEsitoDes  S            128A
009500170428     D   pOutTotFound  S              4S 0
009600170428     D   pOutResults   DS                  dim(10) likeds(dsOutUBTL05R02)
009700170428
009800080620
009900170428     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
010000170428     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
010100170428
010200091027
010300091027
010400080620      //********************************************************************************************
010500080620      //
010600080620      // Definizione parametri procedura.
010700080620      //
010800080620      //********************************************************************************************
010900080620     C     *ENTRY        PLIST
011000170504     C                   PARM                    UBSI95DS
011100170522     C                   PARM                    EDIVABDS
011200170504     C                   PARM                    rtnesito
011300170504     C                   PARM                    rtnopcode
011400170504     C                   PARM                    rtnstatus
011500170428     C*
011600170428     C* Avvio il monitoring del intero flusso
011700170428     C                   monitor
011800090810     C*
011900090810     C* Inizializzazione indicatori d procedura
012000170428     C                   setoff                                       5051
012100170504     C                   setoff                                       5253
012200170508     C                   setoff                                       5455
012300090810     C*
012400090810     C* Reperimento data corrente
012500170428     C                   exsr      repDATCOR
012600090810     C*
012700090810     C* Inizializzazione parametri output e variabili wrk
012800170428     C                   exsr      inzPAROUT
012900090810     C*
013000090810     C* Verifico i parametri d input
013100170508     C                   exsr      chkPARIN
013200170428     C*
013300170428     C* Verifico limiti sul tipo servizio
013400170428     C                   exsr      chkTSP
013500100401     C*
013600170428     C* Proseguo solo se finora tutto ok
013700170428     C                   if        OSI95ERR = *blanks
013800170908     C*
013900170908     C* Verifico FGS
014000170908     C                   exsr      chkFGS
014100090810     C*
014200090810     C* Reperisco il terminal di partenza
014300170526     C   51
014400170526     COR 52              exsr      rtvTFP
014500090810     C*
014600090810     C* Verifica destinatari "particolari"
014700170526     C   51
014800170526     COR 52              exsr      chkDEST
014900170428     C*
015000170428     C* Verifico personalizzazioni cliente
015100170428     C   52              exsr      chkCLI
015200090810     C*
015300090810     C* Calcolo instradamento
015400090810     C                   exsr      calINSTR
015500170831     C*
015600170831     C* Se dopo calcolo instradamento ancora tutto OK proseguo con ulteriori considerazioni
015700170831     C                   if        OSI95ERR = *blanks
015800170831     C*
015900170831     C* Controllo indirizzo tramite Tellus
016000170831     C   55              exsr      chkTELLUS
016100170526     C*
016200170526     C* Controllo e correzione dati VAB
016300170526     C   53              exsr      exeChkFix
016400170504     C*
016500170504     C* Forzatura instradamento EEX->DPD
016600170504     C   54              exsr      istrEEXDPD
016700170825     C*
016800170825     C* Fozature FLA e FLB
016900170825     C                   exsr      exeFLX
017000170831     C*
017100170831     C                   endif
017200100401     C*
017300100401     C                   endif
017400100401     C*
017500100401     C*-------------------------
017600091007     C*
017700091007     C* Proseguo solo se finora tutto ok
017800110621     C                   if        wLNA > *zeros
017900170512     C*
018000170512     C* Gestione forzatura FFD/LNA
018100170512     C   56              eval      wLNA = sLNA
018200090810     C*
018300090810     C* Reperisco il terminal di arrivo
018400170504     C   51
018500170504     COR 52              exsr      rtvTFA
018600090811     C*
018700090811     C* Reperisco descrizioni LNA e LNP
018800170504     C   51
018900170504     COR 52              exsr      rtvPODESC
019000091007     C*
019100091007     C                   endif
019200090810     C*
019300090810     C* Valorizzazione finale parametri d output
019400170428     C                   exsr      valPAROUT
019500170428     C*
019600170428     C* Gestisco eventuale errore
019700170428     C                   on-error
019800170428     C                   eval      OSI95ERR = '.'
019900170428     C                   eval      jMsg = 3
020000170428     C                   exsr      rtvMsgLocal
020100170428     C                   eval      RTNesito  = -1
020200170428     C                   eval      RTNopcode = 'EXEC ERROR'
020300170428     C                   eval      RTNstatus = -1
020400170428     C*
020500170428     C* Arresto il monitoring
020600170428     C                   endmon
020700090810     C*
020800090810     C                   return
020900090810
021000100330
021100090810
021200090810      /TITLE Reperimento data corrente
021300090810     C     repDATCOR     BEGSR
021400090810     C*
021500170428     C                   select
021600170428     C                   when      ISI95DAT > *zeros
021700170428     C                   eval      datcor = ISI95DAT
021800170428     C                   when      VABAAS > *zeros AND VABMGS > *zeros
021900170428     C                   eval      wAAS = VABAAS
022000170428     C                   eval      wMGS = VABMGS
022100170428     C                   eval      datcor = wData
022200170428     C                   other
022300170428     C                   eval      datcor = %dec(%date():*ISO)
022400170428     C                   endsl
022500170428     C*
022600170428     C                   eval      ISI95DAT = datcor
022700170428     C                   eval      wData = datcor
022800170428     C                   eval      VABAAS = wAAS
022900170428     C                   eval      VABMGS = wMGS
023000090810     C*
023100090810     C                   ENDSR
023200090810
023300100330
023400090810
023500090810      /TITLE Inizializzazione parametri output
023600090810     C     inzPAROUT     BEGSR
023700090810     C*
023800170504     C                   clear                   rtnesito
023900170504     C                   clear                   rtnopcode
024000170504     C                   clear                   rtnstatus
024100170512     C*
024200170512     C* Forzatura per Fermo Deposito con Filiale Arrivo scelta dal mittente
024300170512     C                   setoff                                       56
024400170512     C                   clear                   sLNA
024500170512     C                   if        VABFFD = 'S' and OSI95LNA > *zeros
024600170512     C                   eval      sLNA = OSI95LNA
024700170512     C                   seton                                        56
024800170512     C                   endif
024900090810     C*
025000170428     C                   clear                   OSI95TFA
025100170428     C                   clear                   OSI95LNA
025200170428     C                   clear                   OSI95ZNC
025300170428     C                   clear                   OSI95LNPD
025400170428     C                   clear                   OSI95LNAD
025500170428     C                   clear                   OSI95ERR
025600170428     C                   clear                   OSI95MSG
025700170428     C                   clear                   OSI95TAG
025800090810     C*
025900090810     C                   clear                   wDOS
026000090810     C                   clear                   wTFP
026100170908     C                   clear                   wLNP
026200090810     C                   clear                   wTFA
026300090810     C                   clear                   wLNA
026400090810     C                   clear                   wZNC
026500170831     C                   clear                   wPRD
026600170831     C                   clear                   wCAD
026700170831     C                   clear                   wLOD
026800170831     C                   clear                   wIND
026900170508     C*
027000170508     C                   clear                   fnlv55ds
027100170508     C                   clear                   tisit0ds
027200170508     C                   clear                   dcli
027300170508     C                   clear                   fnlvd1ds
027400090810     C*
027500090810     C                   ENDSR
027600090810
027700100330
027800090810
027900090810      /TITLE Verifica parametri di input
028000170508     C     chkPARIN      BEGSR
028100100401     C*
028200100401     C                   setoff                                       10
028300170428     C*
028400170428     C* Normalizzo la lingua indicata in input
028500170428     C                   if        ISI95LIN = *blanks
028600170428     C                   eval      ISI95LIN = 'IT'
028700170428     C                   else
028800170428     C                   eval      ISI95LIN = %xlate(Lower:Upper:ISI95LIN)
028900170428     C                   endif
029000170428     C                   if        ISI95LIN = 'IT' OR
029100170428     C                             ISI95LIN = 'EN' OR
029200170428     C                             ISI95LIN = 'FR' OR
029300170428     C                             ISI95LIN = 'DE'
029400170428     C                   else
029500170428     C                   eval      ISI95LIN = 'EN'
029600170428     C                   endif
029700170428     C*
029800170428     C                   select
029900170428     C                   when      ISI95TYP = *blanks
030000170428     C                   eval      ISI95TYP = 'C'
030100170428     C                   when      ISI95TYP <> 'C' AND
030200170428     C                             ISI95TYP <> 'V' AND
030300170516     C                             ISI95TYP <> 'F'
030400170428     C                   seton                                        10
030500170428     C                   endsl
030600100401     C*
030700100401     C* Procedo solo se parametri in ingresso sono consistenti
030800100401     C                   if        *in10
030900170428     C                   eval      OSI95ERR = '.'
031000100401     C                   eval      jMsg = 2
031100100401     C                   exsr      rtvMsgLocal
031200100401     C                   eval      RTNesito  = -1
031300100401     C                   eval      RTNopcode = 'ERR PARAMS'
031400100401     C                   eval      RTNstatus = 95
031500100401     C                   endif
031600170508     C*
031700170508     C* Verifico il tipo lancio =>
031800170508     C*    'C' = calcolo instradamento         'V' = verifica dati indirizzo
031900170508     C*    'F' = calcolo instradamento full
032000170508     C                   select
032100170508     C                   when      ISI95TYP = 'C'
032200170508     C                   seton                                        51
032300170508     C                   when      ISI95TYP = 'F'
032400170508     C                   seton                                        52
032500170508     C                   endsl
032600170508     C*
032700170508     C* Verifico se richiesto controllo e correzzione dati VAB
032800170508     C                   if        ISI95CFD = 'S'
032900170508     C                   seton                                        53
033000170508     C                   endif
033100170508     C*
033200170508     C* Verifico se richiesto forzatura instradamento da EuroExpress a DPD
033300170508     C                   if        ISI95IED = 'S'
033400170508     C                   seton                                        54
033500170508     C                   endif
033600170508     C*
033700170508     C* Verifico se richiesto anche controllo "Tellus"
033800170508     C                   if        ISI95TLL = 'S'
033900170508     C                   seton                                        55
034000170508     C                   endif
034100090810     C*
034200090810     C                   ENDSR
034300170908
034400170908
034500170908
034600170908      /TITLE Verifica FGS
034700170908     C     chkFGS        BEGSR
034800170908     C*
034900170908     C* Se FGS valore "valido" => lo preferisco alla LNP ai fini calcolo instradamento
035000170908     C                   if        VABFGS  > *zeros AND
035100170908     C                             VABFGS <> 789    AND
035200170908     C                             VABFGS <  900
035300170908     C                   eval      wLNP = VABFGS
035400170908     C                   else
035500170908     C                   eval      wLNP = VABLNP
035600170908     C                   endif
035700170908     C*
035800170908     C                   ENDSR
035900170428
036000170428
036100170428
036200170428      /TITLE Verifico limiti sul tipo servizio
036300170428     C     chkTSP        BEGSR
036400170428     C*
036500170428     C                   z-add     1             jLts
036600170801     C*
036700170801     C* Se Tipo Servizio sin qui non valorizzato assumo per default CORRIERE
036800170801     C                   if        VABTSP = *blanks
036900170801     C                   eval      VABTSP = 'C'
037000170801     C                   endif
037100170801     C*
037200170428     C     VABTSP        lookup    skLtsCod(jLts)                         88
037300170428     C                   if        %found
037400170428     C                   eval      dlts = skLtsDat(jLts)
037500170428     C                   if        VABNCL > §LTSLCLM OR
037600170428     C                             VABPKB > §LTSLPKM OR
037700170428     C                             VABVLB > §LTSLMCM
037800170428     C                   eval      OSI95ERR = '.'
037900170428     C                   eval      jMsg = 4
038000170428     C                   exsr      rtvMsgLocal
038100170428     C                   eval      RTNesito  = -1
038200170428     C                   eval      RTNopcode = 'ERR TSP'
038300170428     C                   eval      RTNstatus = 94
038400170428     C                   endif
038500170428     C                   endif
038600170428     C*
038700170428     C                   ENDSR
038800090810
038900100330
039000090810
039100090810      /TITLE Reperisco il terminal di partenza
039200090810     C     rtvTFP        BEGSR
039300090810     C*
039400090811     C                   movel     'P'           d55tpt
039500170908     C                   z-add     wLNP          d55lin
039600170908     C                   z-add     wLNP          d55lnp
039700170428     C                   z-add     ISI95DAT      d55drf
039800090810     C                   call      'FNLV55R'
039900090810     C                   PARM                    fnlv55ds
040000090810     C*
040100090810     C                   if        d55err = *blanks
040200090810     C                   z-add     d55tfp        wTFP
040300090810     C                   else
040400170428     C                   eval      OSI95ERR = '.'
040500100401     C                   eval      jMsg = 3
040600100401     C                   exsr      rtvMsgLocal
040700090810     C                   eval      RTNesito  = -1
040800100128     C                   eval      RTNopcode = 'ERR TFP'
040900100401     C                   eval      RTNstatus = 99
041000090810     C                   return
041100090810     C                   endif
041200090810     C*
041300090810     C                   ENDSR
041400090810
041500100330
041600090810
041700090810      /TITLE Reperisco il terminal di arrivo
041800090810     C     rtvTFA        BEGSR
041900090810     C*
042000090810     C                   clear                   fnlv55ds
042100090811     C                   movel     'A'           d55tpt
042200090810     C                   z-add     wLNA          d55lin
042300170908     C                   z-add     wLNP          d55lnp
042400170428     C                   z-add     ISI95DAT      d55drf
042500090810     C                   call      'FNLV55R'
042600090810     C                   PARM                    fnlv55ds
042700090810     C*
042800090810     C                   if        d55err = *blanks
042900090810     C                   z-add     d55tfa        wTFA
043000090810     C                   else
043100170428     C                   eval      OSI95ERR = '.'
043200100401     C                   eval      jMsg = 3
043300100401     C                   exsr      rtvMsgLocal
043400090810     C                   eval      RTNesito  = -1
043500100128     C                   eval      RTNopcode = 'ERR TFA'
043600100401     C                   eval      RTNstatus = 98
043700090810     C                   return
043800090810     C                   endif
043900090810     C*
044000090810     C                   ENDSR
044100090810
044200100330
044300090810
044400090810      /TITLE Verifica destinatari "particolari"
044500090810     C     chkDEST       BEGSR
044600090810     C*
044700090810     C                   clear                   tisit0ds
044800170428     C                   movel     VABNZD        it0NAZ
044900170428     C                   movel     VABPRD        it0PRV
045000170428     C                   movel     VABCAD        it0CAP
045100170428     C                   movel     VABLOD        it0LOC
045200170428     C                   movel     VABIND        it0IND
045300170428     C                   movel     VABRSD        it0RAG
045400090810     C                   call      'TISIT5R'
045500090810     C                   parm                    tisit0ds
045600170428     C                   parm                    tisit0x1ds
045700090810     C*
045800090810     C* Se trovato destinatario tra i dest. particolari verifico se presenti linea/zona
045900090810     C* instradamento prestabiliti
046000090810     C                   if        ot0ERR = *blanks
046100090810     C                   eval      ddstflo = ot0FLO
046200090811     C                   eval      wDOS    = ot0dos
046300090810     C                   if        §DSTLNA <> *blanks AND §DSTZNC <> *blanks
046400090810     C                   move(p)   §DSTLNA       wLNA
046500090810     C                   move(p)   §DSTZNC       wZNC
046600170428     C                   seton                                        50
046700090810     C                   endif
046800170428     C                   if        OT0X1GC1 <> *blanks
046900170428     C                   select
047000170428     C                   when      VABGC1 = *blanks
047100170428     C                   eval      VABGC1 = OT0X1GC1
047200170428     C                   when      VABGC2 = *blanks
047300170428     C                   eval      VABGC2 = OT0X1GC1
047400170428     C                   endsl
047500170428     C                   endif
047600170428     C                   if        OT0X1GC2 <> *blanks
047700170428     C                   select
047800170428     C                   when      VABGC1 = *blanks
047900170428     C                   eval      VABGC1 = OT0X1GC2
048000170428     C                   when      VABGC2 = *blanks
048100170428     C                   eval      VABGC2 = OT0X1GC2
048200170428     C                   endsl
048300170428     C                   endif
048400090810     C                   endif
048500170428     C*
048600170428     C* Se presente particolarità varia "U" => non considero Supermercato / Appuntamento
048700170428     C                   if        %trim(VABGVA) = 'U'
048800170428     C                   clear                   wDOS
048900170428     C                   endif
049000090810     C*
049100090810     C                   ENDSR
049200170428
049300170428
049400170428
049500170428      /TITLE Verifico personalizzazioni cliente
049600170428     C     chkCLI        BEGSR
049700170428     C*
049800170428     C                   clear                   dcli
049900170428     C                   eval      tbecod = 'CLI'
050000170504     C                   movel     vabCCM        tbeke1
050100170428     C     KEYtbe01_P    chain     tntbe01l
050200170428     C                   if        %found(tntbe01l)
050300170508     C                   if        tbeatb = *blanks
050400170428     C                   eval      dcli = tbeuni
050500170428     C                   endif
050600170428     C                   endif
050700170428     C*
050800170428     C                   ENDSR
050900170825
051000170825
051100170825
051200170825      /TITLE Fozature FLA e FLB
051300170825     C     exeFLX        BEGSR
051400170825     C*
051500170825     C* Forzatura linea di arrivo per codice cliente mittente o per linea di p.
051600170825     C                   clear                   trulflbds
051700170825     C                   movel     wLNA          iflblna
051800170908     C                   move      wLNP          Iflblnp
051900170825     C                   move      vabCCM        iflbccm
052000170825     C                   call      'TRULFLBR'
052100170825     C                   parm                    trulflbds
052200170825     C                   if        Oflblfz > *zeros
052300170825     C                   move      OflbLFZ       wLNA
052400170825     C                   if        oflberr = 'F'
052500170825     C                   z-add     90            wZNC
052600170825     C                   movel     'S'           vabFFD
052700170825     C                   endif
052800170825     C                   endif
052900170825     C*
053000170825     C                   ENDSR
053100170508
053200170508
053300170508
053400170508      /TITLE Controllo e correzione dati VAB
053500170508     C     exeChkFix     BEGSR
053600170508     C*
053700170831     C                   eval      VABPRD = wPRD
053800170831     C                   eval      VABCAD = wCAD
053900170831     C                   eval      VABLOD = wLOD
054000170831     C                   if        wIND <> *blanks
054100170831     C                   eval      VABIND = wIND
054200170831     C                   endif
054300170508     C*
054400170508     C                   ENDSR
054500170428
054600170428
054700170428
054800170428      /TITLE Controllo indirizzo tramite Tellus
054900170428     C     chkTELLUS     BEGSR
055000170428     C*
055100170428     C                   if        VABIND <> *blanks
055200170428     C
055300170428     C                   EVAL      pInNazione   = VABNZD
055400170831     C                   EVAL      pInProvincia = wPRD
055500170831     C                   EVAL      pInComune    = wLOD
055600170428     C                   EVAL      pInLocalita  = *blanks
055700170831     C                   EVAL      pInCAP       = wCAD
055800170428     C                   EVAL      pInVia       = VABIND
055900170428     C                   EVAL      pInCivico    = *blanks
056000170428     C                   EVAL      pInScore     = *blanks
056100170526     C                   CALL(e)   'UBTL05R02'
056200170428     C                   PARM                    pInNazione
056300170428     C                   PARM                    pInProvincia
056400170428     C                   PARM                    pInComune
056500170428     C                   PARM                    pInLocalita
056600170428     C                   PARM                    pInCAP
056700170428     C                   PARM                    pInVia
056800170428     C                   PARM                    pInCivico
056900170428     C                   PARM                    pInScore
057000170428     C                   PARM                    pOutEsito
057100170428     C                   PARM                    pOutEsitoDes
057200170428     C                   PARM                    pOutTotFound
057300170428     C                   PARM                    pOutResults
057400170428     C                   z-add     1             inx               3 0
057500170428     C                   dow       inx<=pOutTotFound
057600170428     C                   if        pOutResults(inx).esatto  = '0' and
057700170428     C                             pOutResults(inx).LivNorm = 'V' and
057800170831     C                             pOutResults(inx).Prov    = wPRD
057900170831     C                   eval      wCAD = pOutResults(inx).CAP
058000170831     C                   if        %trim(pOutResults(inx).Civico) <> *blanks and
058100170831     C                             %trim(pOutResults(inx).Civico) <> '0'
058200170831     C                   eval      wIND = %trim(pOutResults(inx).Via)+', '+
058300170831     C                                    %trim(pOutResults(inx).Civico)
058400170831     C                   endif
058500170428     C                   leave
058600170428     C                   endif
058700170428     C                   add       1             inx
058800170428     C                   enddo
058900170428     C
059000170428     C                   endif
059100170428     C*
059200170428     C                   endsr
059300090810
059400100330
059500090810
059600090810      /TITLE Calcolo instradamento
059700090810     C     calINSTR      BEGSR
059800091027     C*
059900091027     C                   clear                   tisi95ds
060000091027     C                   clear                   tisi95r1ds
060100090810     C                   eval      i95tcn = '7'
060200170504     C                   eval      i95dat = ISI95DAT
060300170504     C                   eval      i95fi1 = VABATB
060400170428     C                   eval      i95nar = VABNZD
060500170428     C                   eval      i95prv = VABPRD
060600170428     C                   eval      i95cap = VABCAD
060700170428     C                   eval      i95loc = VABLOD
060800170428     C                   eval      i95lkg = VABPKB
060900170428     C                   eval      i95lmc = VABVLB
061000170428     C                   eval      i95ffd = VABFFD
061100170428     C                   eval      i95tsp = VABTSP
061200090810     C                   eval      i95tfp = wTFP
061300100330     C   51              eval      i95fre = 'S'
061400100504     C*
061500100504     C                   select
061600170504     C                   when      vabCBO = '1'
061700100504     C                   eval      i95tpo = 'F'
061800170504     C                   when      vabCBO = '2'
061900100504     C                   eval      i95tpo = 'A'
062000170504     C                   when      vabCBO = '4'
062100100504     C                   eval      i95tpo = 'F'
062200100504     C                   eval      i95fca = 'S'
062300170504     C                   when      vabCBO = '6'
062400100504     C                   eval      i95tpo = 'A'
062500100504     C                   eval      i95fca = 'S'
062600170504     C                   other
062700170504     C                   eval      i95tpo = 'F'
062800100504     C                   endsl
062900100504     C*
063000170428     C                   if        wDOS   = 'S' OR
063100170428     C                             VABTC1 = 'S' OR
063200170428     C                             VABTC2 = 'S'
063300170504     C                   if        §clisup <> 'N'
063400170504     C                   eval      %subst(i95fi2:1:1) = 'S'
063500170428     C                   endif
063600170504     C                   endif
063700170428     C*
063800170428     C                   if        wDOS   = 'A' OR
063900170428     C                             VABTC1 = 'A' OR
064000170428     C                             VABTC2 = 'A'
064100170504     C                   eval      %subst(i95fi2:2:1) = 'A'
064200170428     C                   endif
064300170428     C*
064400170428     C                   if        §cliban <> 'S'
064500170505     C                   if        %trim(VABGVA) = 'B' AND VABANT <> *zeros
064600170504     C                   movel     VABANT        wVABANT
064700170428     C                   z-add     1             jGva
064800170428     C     wTB1_COD      lookup    skGvaB_C(jGva)                         88
064900170428     C                   if        %found
065000170428     C                   eval      dtba = skGvaB_D(jGva)
065100170428     C                   eval      i95lkg = i95lkg + (§TBAPKG*wTB1_TOT)
065200170504     C                   if        i95lmc > *zeros
065300170428     C                   eval      i95lmc = i95lmc + (§TBAVOL*wTB1_TOT)
065400170504     C                   endif
065500170428     C                   endif
065600170428     C                   z-add     1             jGva
065700170428     C     wTB2_COD      lookup    skGvaB_C(jGva)                         88
065800170428     C                   if        %found
065900170428     C                   eval      dtba = skGvaB_D(jGva)
066000170428     C                   eval      i95lkg = i95lkg + (§TBAPKG*wTB2_TOT)
066100170504     C                   if        i95lmc > *zeros
066200170428     C                   eval      i95lmc = i95lmc + (§TBAVOL*wTB2_TOT)
066300170504     C                   endif
066400170428     C                   endif
066500170428     C                   endif
066600170428     C                   endif
066700170428     C*
066800170428     C                   if        §clitspf <> *blanks
066900170428     C                   movel     §clitspf      i95tsp
067000170428     C                   endif
067100170428     C*
067200170508     C                   if        §clipsm <> *blanks AND §clipsm <> *zeros
067300170428     C                   eval      i95lkg  = %dec(§clipsm:7:1)
067400170428     C                   endif
067500170508     C                   if        §clivlm <> *blanks AND §clivlm <> *zeros
067600170428     C                   eval      i95lmc  = %dec(§clivlm:5:3)
067700170428     C                   endif
067800170428     C*
067900170516     C                   select
068000170516     C                   when      ISI95TYP = 'V'
068100170516     C                   eval      tisi95r1ds.si95itype = 'V'
068200170516     C                   when      ISI95TYP = 'C' or ISI95TYP = 'F'
068300170516     C                   eval      tisi95r1ds.si95itype = 'C'
068400170516     C                   other
068500170516     C                   eval      tisi95r1ds.si95itype = ISI95TYP
068600170516     C                   endsl
068700170516     C*
068800170428     C                   eval      tisi95r1ds.si95ilang = ISI95LIN
068900170428     C                   eval      tisi95r1ds.si95incl  = VABNCL
069000091027     C                   eval      tisi95r1ds.tisi95ds  = tisi95ds
069100091027     C*
069200091027     C                   call      'TISI95R1'
069300091027     C                   parm                    tisi95r1ds
069400091027     C                   eval      tisi95ds = tisi95r1ds.tisi95ds
069500090810     C*
069600170428     C                   eval      OSI95ERR = o95err
069700091105     C*
069800100330     C                   if        i95nar  = *blanks AND
069900100330     C                             o95lia  < '2'
070000170428     C                   eval      OSI95ERR = '*'
070100100330     C                   eval      jMsg = 1
070200100330     C                   exsr      rtvMsgLocal
070300100330     C                   else
070400090811     C  N50              eval      wLNA = o95lna
070500090811     C  N50              eval      wZNC = o95znc
070600170831     C                   eval      wPRD = o95prv
070700170831     C                   eval      wCAD = o95cap
070800170831     C                   eval      wLOD = o95loc
070900110621     C*
071000110621     C* Forzatura x "finto" errore
071100110801     C                   if        wLNA > *zeros AND wZNC > *zeros           AND
071200110801     C                             (o95lid = '3' OR o95lid = '4')            AND
071300110801     C                             %subst(tisi95r1ds.si95ocmsg:2:1)<>'*'
071400170428     C                   eval      OSI95ERR = *blanks
071500110621     C                   endif
071600110621     C*
071700090810     C                   endif
071800090810     C*
071900090810     C                   ENDSR
072000170504
072100090811
072200170504
072300170504      /TITLE Forzatura instradamento EEX->DPD
072400170504     C     istrEEXDPD    BEGSR
072500170504     C*
072600170504     C                   clear                   fnlvd1ds
072700170908     C                   eval      ilvd1LNP  = wLNP
072800170504     C                   eval      ilvd1NCL  = vabNCL
072900170504     C                   eval      ilvd1PKG  = i95lkg
073000170504     C                   eval      ilvd1VLU  = i95lmc
073100170504     C                   eval      ilvd1CCM  = vabCCM
073200170504     C                   eval      ilvd1ORMF = 'S'
073300170504     C*
073400170504     C                   call      'FNLVD1R'
073500170504     C                   parm                    fnlvd1ds
073600170504     C                   parm                    tisi95ds
073700170504     C*
073800170504     C* Se instradabile su DPD
073900170504     C                   if        olvd1esi = 'S'
074000170505     C  N50              eval      wLNA = elvd1LNAD
074100170505     C  N50              eval      wZNC = olvd1ZNCD
074200170504     C                   endif
074300170504     C*
074400170504     C                   ENDSR
074500170504
074600100330
074700090811
074800090811      /TITLE Reperisco descrizioni LNA e LNP
074900090811     C     rtvPODESC     BEGSR
075000090811     C*
075100090811     C* Reperimento descrizione LNP
075200091027     C                   z-add     1             jOrg
075300170908     C     wLNP          lookup    skOrgCod(jOrg)                         88
075400091027     C                   if        %found
075500170428     C                   eval      OSI95LNPD = skOrgDes(jOrg)
075600090811     C                   else
075700170428     C                   eval      OSI95ERR = '.'
075800100401     C                   eval      jMsg = 3
075900100401     C                   exsr      rtvMsgLocal
076000090811     C                   eval      RTNesito  = -1
076100100128     C                   eval      RTNopcode = 'ERR LNP'
076200100401     C                   eval      RTNstatus = 97
076300090811     C                   return
076400090811     C                   endif
076500090811     C*
076600090811     C* Reperimento descrizione LNA
076700091027     C                   z-add     1             jOrg
076800091027     C     wLNA          lookup    skOrgCod(jOrg)                         88
076900091027     C                   if        %found
077000170428     C                   eval      OSI95LNAD = skOrgDes(jOrg)
077100090811     C                   else
077200170428     C                   eval      OSI95ERR = '.'
077300100401     C                   eval      jMsg = 3
077400100401     C                   exsr      rtvMsgLocal
077500090811     C                   eval      RTNesito  = -1
077600100128     C                   eval      RTNopcode = 'ERR LNA'
077700100401     C                   eval      RTNstatus = 96
077800090811     C                   return
077900090811     C                   endif
078000090811     C*
078100090811     C                   ENDSR
078200100330
078300100330
078400100330
078500100330     C     rtvMsgLocal   BEGSR
078600100330     C*
078700100330     C                   select
078800100330     C* ... Italiano
078900100330     C                   when      tisi95r1ds.si95ilang        = 'ITA' OR
079000100330     C                             %trim(tisi95r1ds.si95ilang) = 'IT'
079100170504     C                   eval      OSI95MSG = MSGITA(jMsg)
079200100330     C* ... Inglese
079300100330     C                   when      tisi95r1ds.si95ilang        = 'ENG' OR
079400100330     C                             %trim(tisi95r1ds.si95ilang) = 'EN'
079500170504     C                   eval      OSI95MSG = MSGENG(jMsg)
079600100330     C* ... Francese
079700100330     C                   when      tisi95r1ds.si95ilang        = 'FRA' OR
079800100330     C                             %trim(tisi95r1ds.si95ilang) = 'FR'
079900170504     C                   eval      OSI95MSG = MSGFRA(jMsg)
080000100330     C* ... Tedesco
080100100330     C                   when      tisi95r1ds.si95ilang        = 'DEU' OR
080200100330     C                             %trim(tisi95r1ds.si95ilang) = 'DE'
080300170504     C                   eval      OSI95MSG = MSGDEU(jMsg)
080400100330     C                   other
080500100330     C                   endsl
080600100330     C*
080700100330     C                   ENDSR
080800100330     C***
080900100128
081000090810
081100090810
081200090810      /TITLE Valorizzazione finale parametri d output
081300090810     C     valPAROUT     BEGSR
081400090810     C*
081500170428     C                   eval      OSI95TFA = wTFA
081600170428     C                   eval      OSI95LNA = wLNA
081700170428     C                   eval      OSI95ZNC = wZNC
081800170825     C*
081900170825     C                   eval      vabLNA = wLNA
082000170825     C                   eval      vabZNC = wZNC
082100091027     C*
082200091027     C                   if        tisi95r1ds.si95ojmsg > *zeros
082300170428     C                   eval      OSI95MSG = tisi95r1ds.si95odmsg
082400170428     C                   eval      OSI95TAG = MSGTAG(tisi95r1ds.si95ojmsg)
082500091027     C                   endif
082600100126     C*
082700100126     C* Se presente errore => gestisco
082800090810     C                   eval      RTNesito  = 0
082900100128     C                   eval      RTNopcode = 'SUCCESS'
083000100128     C                   eval      RTNstatus = 00
083100090810     C*
083200090810     C                   ENDSR
083300170428
083400170428
083500170428
083600170428      /TITLE Inizializzazioni
083700170428     C     *inzSR        BEGSR
083800170428     C*
083900170428     C* Definizione chiavi di lettura
084000170428     C     KEYtbe01_P    klist
084100170428     C                   kfld                    tbeCOD
084200170428     C                   kfld                    tbeKE1
084300170428     C*
084400170428     C* Organigramma
084500170428     C                   z-add     *zeros        jOrg
084600170428     C     *start        setll     azorg01l
084700170428     C                   read      azorg01l
084800170428     C                   dow       not %eof(azorg01l)
084900170428     C                   if        orgfva = *blanks AND
085000170803     C                             (orgfag = 'F' OR orgfag = 'A')
085100170428     C                   add       1             jOrg
085200170428     C                   eval      skOrgCod(jOrg) = orgfil
085300170428     C                   eval      skOrgDes(jOrg) = orgdes
085400170428     C                   endif
085500170428     C                   read      azorg01l
085600170428     C                   enddo
085700170428     C*
085800170428     C* Tabella TBA
085900170428     C                   z-add     *zeros        jGva
086000170428     C                   eval      tbecod = 'TBA'
086100170428     C     tbecod        setll     tntbe01l
086200170428     C     tbecod        reade     tntbe01l
086300170428     C                   dow       not %eof(tntbe01l)
086400170508     C                   if        tbeatb = *blanks
086500170428     C                   eval      dtba = tbeuni
086600170428     C                   if        §TBAUTB = 'S'
086700170428     C                   add       1             jGva
086800170428     C                   eval      skGvaB_C(jGva) = tbeke1
086900170428     C                   eval      skGvaB_D(jGva) = dtba
087000170428     C                   endif
087100170428     C                   endif
087200170428     C     tbecod        reade     tntbe01l
087300170428     C                   enddo
087400170428     C*
087500170428     C* Tabella LTS
087600170428     C                   z-add     *zeros        jLts
087700170428     C                   eval      tbecod = 'LTS'
087800170428     C     tbecod        setll     tntbe01l
087900170428     C     tbecod        reade     tntbe01l
088000170428     C                   dow       not %eof(tntbe01l)
088100170508     C                   if        tbeatb = *blanks
088200170428     C                   eval      dlts = tbeuni
088300170428     C                   add       1             jLts
088400170428     C                   eval      skLtsCod(jLts) = tbeke1
088500170428     C                   eval      skLtsDat(jLts) = dlts
088600170428     C                   endif
088700170428     C     tbecod        reade     tntbe01l
088800170428     C                   enddo
088900170428     C*
089000170428     C                   ENDSR
089100100330     C*--------------------------------------------------------------------
089200091007** MSGTAG - MESSAGGI
089300100504CodNazione                                                                       1
089400100504Cap                                                                              2
089500100504CodTipoServizio                                                                  3
089600100504                                                                                 4
089700100504                                                                                 5
089800100504                                                                                 6
089900100504                                                                                 7
090000100504Cap                                                                              8
090100100504CodProvincia                                                                     9
090200100504                                                                                 A
090300100504Localita                                                                         B
090400100504CodProvincia                                                                     C
090500100504Localita                                                                         D
090600100504CodProvincia                                                                     E
090700100504                                                                                 F
090800100504                                                                                 G
090900100504Localita                                                                         H
091000100504Localita                                                                         I
091100100504                                                                                 L
091200100504                                                                                 M
091300100504CodTipoServizio                                                                  P
091400100504CodTipoServizio                                                                  Q
091500100504CodNetwork                                                                       R
091600100504CodTipoServizio                                                                  S
091700100504CodNetwork                                                                       T
091800100504CodNetwork|CodNazione|Cap                                                        U
091900100504CodNetwork|CodNazione|Cap|TBOCBO                                                 V
092000100504CodNetwork|CodNazione|Cap|TBOCBO                                                 W
092100100504CodNetwork|CodNazione|Cap                                                        X
092200100504                                                                                 Y
092300100330** MSGITA - MESSAGGI
092400100330Impossibile calcolare un esatto instradamento.
092500100401Parametri di input non validi.
092600100401Errore interno imprevisto.
092700170428Valori di colli, peso o volume incompatibili con il tipo servizio.
092800100330
092900100330** MSGENG - MESSAGGI
093000100330Unable to calculate exact routing.
093100100401Invalid input parameters.
093200100401Unexpected internal error.
093300170428Value for parcels, weight or volume not allower related to service type.
093400100330
093500100330** MSGFRA - MESSAGGI
093600100401Impossible de calculer exactement le routage.
093700100401Les paramètres d'entrée ne sont pas valides.
093800100401Inattendue erreur interne.
093900170428Valeur pour colis, poids ou volume non autorisés liés au type de service.
094000170428
094100100330** MSGDEU - MESSAGGI
094200100401Unmöglich zu berechnen exakte Route.
094300100401Input-Parameter ungültig.
094400100401Unerwarteter interner Fehler.
094500170428Wert für Pakete, Gewicht oder Volumen nicht zulässig mit Service-Typ.
094600100330
