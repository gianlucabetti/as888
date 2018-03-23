000100090717     h datedit(*ymd/) dftactgrp(*no) actgrp(*caller)
000200080620      //********************************************************************************************
000300080620      //
000400080620      // Definizione variabili.
000500080620      //
000600080620      //********************************************************************************************
000700090811     Fazorg01l  IF   E           K DISK
000800090811     F*
000900100504     D MSGTAG          S             80A   DIM(30) CTDATA PERRCD(1)
001000100330     F*
001100100503     D MSGITA          S             80A   DIM(5) CTDATA PERRCD(1)
001200100503     D MSGENG          S             80A   DIM(5) CTDATA PERRCD(1)
001300100503     D MSGFRA          S             80A   DIM(5) CTDATA PERRCD(1)
001400100503     D MSGDEU          S             80A   DIM(5) CTDATA PERRCD(1)
001500090810     D*
001600090810     D tisi95ds      e ds
001700090810     D tisit0ds      e ds
001800090810     D tisit0x1ds    e ds
001900090810     D ddstflo       e ds
002000090810     D fnlv55ds      e ds
002100090810     D*
002200100504     D iTYPE           S              1A
002300100504     D iLANG           S              3A
002400100504     D iDATA           S              8S 0
002500100504     D iNTW            S              1A
002600100504     D iLNP            S              3S 0
002700100504     D iNZD            S              3A
002800100504     D iPRD            S              2A
002900100504     D iCAD            S              9A
003000100504     D iLOD            S             35A
003100100504     D iIND            S             35A
003200100504     D iRSD            S             35A
003300100504     D iNCL            S              5S 0
003400100504     D iPKB            S              7S 1
003500100504     D iVLB            S              5S 3
003600100504     D iFFD            S              1A
003700100504     D iTSP            S              1A
003800100504     D iTC1            S              1A
003900100504     D iTC2            S              1A
004000100504     D iCBO            S              2A
004100090810     D*
004200100504     D oTFA            S              3S 0
004300100504     D oLNA            S              3S 0
004400100504     D oZNC            S              2S 0
004500100504     D oLNPD           S             20A
004600100504     D oLNAD           S             20A
004700100504     D oERR            S              1A
004800100504     D oMSG            S            256A
004900100504     D oTAG            S             80A
005000090811     D*
005100090811     D RTNesito        S             10I 0
005200100504     D RTNopcode       S             10A
005300090811     D RTNstatus       S             10I 0
005400090810     D*
005500100504     D wDOS            S              1A
005600100504     D wTFP            S              3S 0
005700100504     D wTFA            S              3S 0
005800100504     D wLNA            S              3S 0
005900170512     D sLNA            S              3S 0
006000100504     D wZNC            S              2S 0
006100090810     D*
006200100504     D skOrgCod        S              3S 0 DIM(999)
006300100504     D skOrgDes        S             20A   DIM(999)
006400100504     D jOrg            S              4S 0
006500100504     D jMsg            S              2S 0
006600100504     D datcor          S              8S 0
006700091027     D*
006800080620
006900091027     D/COPY GAITRASRC/SRCPROTOPI,TISI95R1
007000091027
007100091027
007200080620      //********************************************************************************************
007300080620      //
007400080620      // Definizione parametri procedura.
007500080620      //
007600080620      //********************************************************************************************
007700080620     C     *ENTRY        PLIST
007800100330     C                   PARM                    iTYPE
007900090810     C                   PARM                    iLANG
008000090810     C                   PARM                    iDATA
008100090810     C                   PARM                    iNTW
008200090810     C                   PARM                    iLNP
008300090810     C                   PARM                    iNZD
008400090810     C                   PARM                    iPRD
008500090810     C                   PARM                    iCAD
008600090810     C                   PARM                    iLOD
008700090810     C                   PARM                    iIND
008800090810     C                   PARM                    iRSD
008900100114     C                   PARM                    iNCL
009000090810     C                   PARM                    iPKB
009100090810     C                   PARM                    iVLB
009200090810     C                   PARM                    iFFD
009300090810     C                   PARM                    iTSP
009400090810     C                   PARM                    iTC1
009500090810     C                   PARM                    iTC2
009600100504     C                   PARM                    iCBO
009700090810     C                   PARM                    oTFA
009800090810     C                   PARM                    oLNA
009900090810     C                   PARM                    oZNC
010000090811     C                   PARM                    oLNPD
010100090811     C                   PARM                    oLNAD
010200090810     C                   PARM                    oERR
010300100503     C                   PARM                    oMSG
010400100503     C                   PARM                    oTAG
010500080620     C                   PARM                    rtnesito
010600080620     C                   PARM                    rtnopcode
010700080620     C                   PARM                    rtnstatus
010800090810     C*
010900090810     C* Inizializzazione indicatori d procedura
011000100330     C                   setoff                                       5051
011100090810     C*
011200090810     C* Reperimento data corrente
011300090810     C                   exsr      repDATCOR
011400090810     C*
011500090810     C* Inizializzazione parametri output e variabili wrk
011600090810     C                   exsr      inzPAROUT
011700090810     C*
011800090810     C* Verifico i parametri d input
011900090810     C                   exsr      chkPARIN
012000100401     C*
012100100401     C* Proseguo solo se finora tutto ok
012200100401     C                   if        oERR = *blanks
012300100330     C*
012400100330     C* Verifico il tipo lancio => 'C' = calcolo instradamento  'V' = verifica dati indirizzo
012500100330     C                   if        iTYPE = 'C'
012600100330     C                   seton                                        51
012700100330     C                   endif
012800090810     C*
012900090810     C* Reperisco il terminal di partenza
013000100330     C   51              exsr      rtvTFP
013100090810     C*
013200090810     C* Verifica destinatari "particolari"
013300100330     C   51              exsr      chkDEST
013400090810     C*
013500090810     C* Calcolo instradamento
013600090810     C                   exsr      calINSTR
013700100401     C*
013800100401     C                   endif
013900100401     C*
014000100401     C*-------------------------
014100091007     C*
014200091007     C* Proseguo solo se finora tutto ok
014300170512     C                   if        wLNA > *zeros
014400170512     C*
014500170512     C* Gestione forzatura FFD/LNA
014600170512     C   56              eval      wLNA = sLNA
014700090810     C*
014800090810     C* Reperisco il terminal di arrivo
014900100330     C   51              exsr      rtvTFA
015000090811     C*
015100090811     C* Reperisco descrizioni LNA e LNP
015200100330     C   51              exsr      rtvPODESC
015300091007     C*
015400091007     C                   endif
015500090810     C*
015600090810     C* Valorizzazione finale parametri d output
015700090810     C                   exsr      valPAROUT
015800090810     C*
015900090810     C                   return
016000091027
016100100330
016200091027
016300091027      /TITLE Inizializzazioni
016400091027     C     *inzSR        BEGSR
016500091027     C*
016600091027     C*---
016700091027     C* Organigramma
016800091027     C*---
016900091027     C                   z-add     *zeros        jOrg
017000091027     C     *start        setll     azorg01l
017100091027     C                   read      azorg01l
017200091027     C                   dow       not %eof(azorg01l)
017300091027     C                   if        orgfva = *blanks
017400091027     C                   add       1             jOrg
017500091027     C                   eval      skOrgCod(jOrg) = orgfil
017600091027     C                   eval      skOrgDes(jOrg) = orgdes
017700091027     C                   endif
017800091027     C                   read      azorg01l
017900091027     C                   enddo
018000091027     C*
018100091027     C                   ENDSR
018200090810
018300100330
018400090810
018500090810      /TITLE Reperimento data corrente
018600090810     C     repDATCOR     BEGSR
018700090810     C*
018800091027     C                   eval      datcor = %DEC(%DATE():*ISO)
018900090810     C*
019000090810     C                   ENDSR
019100090810
019200100330
019300090810
019400090810      /TITLE Inizializzazione parametri output
019500090810     C     inzPAROUT     BEGSR
019600090810     C*
019700090810     C                   clear                   rtnesito
019800090810     C                   clear                   rtnopcode
019900100330     C                   clear                   rtnstatus
020000090810     C*
020100170512     C* Forzatura per Fermo Deposito con Filiale Arrivo scelta dal mittente
020200170512     C                   setoff                                       56
020300170512     C                   clear                   sLNA
020400170512     C                   if        iFFD = 'S' and oLNA > *zeros
020500170512     C                   eval      sLNA = oLNA
020600170512     C                   seton                                        56
020700170512     C                   endif
020800170512     C*
020900090810     C                   clear                   oTFA
021000090810     C                   clear                   oLNA
021100090810     C                   clear                   oZNC
021200090811     C                   clear                   oLNPD
021300090811     C                   clear                   oLNAD
021400090810     C                   clear                   oERR
021500100503     C                   clear                   oMSG
021600100503     C                   clear                   oTAG
021700090810     C*
021800090810     C                   clear                   wDOS
021900090810     C                   clear                   wTFP
022000090810     C                   clear                   wTFA
022100090810     C                   clear                   wLNA
022200090810     C                   clear                   wZNC
022300090810     C*
022400090810     C                   ENDSR
022500090810
022600100330
022700090810
022800090810      /TITLE Verifica parametri di input
022900090810     C     chkPARIN      BEGSR
023000100401     C*
023100100401     C                   setoff                                       10
023200090810     C*
023300090810     C                   if        iDATA = *zeros
023400090810     C                   eval      iDATA = datcor
023500090810     C                   endif
023600100330     C*
023700100330     C                   if        iTYPE = *blanks
023800100330     C                   eval      iTYPE = 'C'
023900100330     C                   endif
024000100401     C*
024100100401     C                   if        iTYPE <> *blanks AND
024200100401     C                             iTYPE <> 'V'     AND
024300100401     C                             iTYPE <> 'C'
024400100401     C                   seton                                        51
024500100401     C                   endif
024600100401     C*
024700100401     C* Procedo solo se parametri in ingresso sono consistenti
024800100401     C                   if        *in10
024900100401     C                   eval      oERR = '.'
025000100401     C                   eval      jMsg = 2
025100100401     C                   exsr      rtvMsgLocal
025200100401     C                   eval      RTNesito  = -1
025300100401     C                   eval      RTNopcode = 'ERR PARAMS'
025400100401     C                   eval      RTNstatus = 95
025500100401     C                   endif
025600090810     C*
025700090810     C                   ENDSR
025800090810
025900100330
026000090810
026100090810      /TITLE Reperisco il terminal di partenza
026200090810     C     rtvTFP        BEGSR
026300090810     C*
026400090810     C                   clear                   fnlv55ds
026500090811     C                   movel     'P'           d55tpt
026600090810     C                   z-add     iLNP          d55lin
026700090810     C                   z-add     iLNP          d55lnp
026800090810     C                   z-add     iDATA         d55drf
026900090810     C                   call      'FNLV55R'
027000090810     C                   PARM                    fnlv55ds
027100090810     C*
027200090810     C                   if        d55err = *blanks
027300090810     C                   z-add     d55tfp        wTFP
027400090810     C                   else
027500100401     C                   eval      oERR = '.'
027600100401     C                   eval      jMsg = 3
027700100401     C                   exsr      rtvMsgLocal
027800090810     C                   eval      RTNesito  = -1
027900100128     C                   eval      RTNopcode = 'ERR TFP'
028000100401     C                   eval      RTNstatus = 99
028100090810     C                   return
028200090810     C                   endif
028300090810     C*
028400090810     C                   ENDSR
028500090810
028600100330
028700090810
028800090810      /TITLE Reperisco il terminal di arrivo
028900090810     C     rtvTFA        BEGSR
029000090810     C*
029100090810     C                   clear                   fnlv55ds
029200090811     C                   movel     'A'           d55tpt
029300090810     C                   z-add     wLNA          d55lin
029400090810     C                   z-add     iLNP          d55lnp
029500090810     C                   z-add     iDATA         d55drf
029600090810     C                   call      'FNLV55R'
029700090810     C                   PARM                    fnlv55ds
029800090810     C*
029900090810     C                   if        d55err = *blanks
030000090810     C                   z-add     d55tfa        wTFA
030100090810     C                   else
030200100401     C                   eval      oERR = '.'
030300100401     C                   eval      jMsg = 3
030400100401     C                   exsr      rtvMsgLocal
030500090810     C                   eval      RTNesito  = -1
030600100128     C                   eval      RTNopcode = 'ERR TFA'
030700100401     C                   eval      RTNstatus = 98
030800090810     C                   return
030900090810     C                   endif
031000090810     C*
031100090810     C                   ENDSR
031200090810
031300100330
031400090810
031500090810      /TITLE Verifica destinatari "particolari"
031600090810     C     chkDEST       BEGSR
031700090810     C*
031800090810     C                   clear                   tisit0ds
031900090810     C                   movel     iNZD          it0NAZ
032000090810     C                   movel     iPRD          it0PRV
032100090810     C                   movel     iCAD          it0CAP
032200090810     C                   movel     iLOD          it0LOC
032300090810     C                   movel     iIND          it0IND
032400090810     C                   movel     iRSD          it0RAG
032500090810     C                   call      'TISIT5R'
032600090810     C                   parm                    tisit0ds
032700090810     C*
032800090810     C* Se trovato destinatario tra i dest. particolari verifico se presenti linea/zona
032900090810     C* instradamento prestabiliti
033000090810     C                   if        ot0ERR = *blanks
033100090810     C                   eval      ddstflo = ot0FLO
033200090811     C                   eval      wDOS    = ot0dos
033300090810     C                   if        §DSTLNA <> *blanks AND §DSTZNC <> *blanks
033400090810     C                   move(p)   §DSTLNA       wLNA
033500090810     C                   move(p)   §DSTZNC       wZNC
033600090810     C                   seton                                        50
033700090810     C                   endif
033800090810     C                   endif
033900090810     C*
034000090810     C                   ENDSR
034100090810
034200100330
034300090810
034400090810      /TITLE Calcolo instradamento
034500090810     C     calINSTR      BEGSR
034600091027     C*
034700091027     C                   clear                   tisi95ds
034800091027     C                   clear                   tisi95r1ds
034900090810     C                   eval      i95tcn = '7'
035000090810     C                   eval      i95dat = iDATA
035100090810     C                   eval      i95fi1 = iNTW
035200090810     C                   eval      i95nar = iNZD
035300090810     C                   eval      i95prv = iPRD
035400090810     C                   eval      i95cap = iCAD
035500090810     C                   eval      i95loc = iLOD
035600090810     C                   eval      i95lkg = iPKB
035700090810     C                   eval      i95lmc = iVLB
035800090810     C                   eval      i95ffd = iFFD
035900090810     C                   eval      i95tsp = iTSP
036000090810     C                   eval      i95tfp = wTFP
036100100330     C   51              eval      i95fre = 'S'
036200100504     C*
036300100504     C                   select
036400100504     C                   when      iCBO = '1'
036500100504     C                   eval      i95tpo = 'F'
036600100504     C                   when      iCBO = '2'
036700100504     C                   eval      i95tpo = 'A'
036800100504     C                   when      iCBO = '4'
036900100504     C                   eval      i95tpo = 'F'
037000100504     C                   eval      i95fca = 'S'
037100100504     C                   when      iCBO = '6'
037200100504     C                   eval      i95tpo = 'A'
037300100504     C                   eval      i95fca = 'S'
037400100504     C                   endsl
037500100504     C*
037600090810     C                   if        wDOS = 'S'
037700090810     C                   move      wDOS          i95fi2
037800090810     C                   endif
037900090810     C                   if        iTC1 = 'S'
038000090810     C                   movel     iTC1          i95fi2
038100090810     C                   endif
038200090810     C                   if        iTC2 = 'S'
038300090810     C                   movel     iTC2          i95fi2
038400090810     C                   endif
038500091027     C*
038600100331     C                   eval      tisi95r1ds.si95itype = iTYPE
038700091027     C                   eval      tisi95r1ds.si95ilang = iLANG
038800100115     C                   eval      tisi95r1ds.si95incl  = iNCL
038900091027     C                   eval      tisi95r1ds.tisi95ds  = tisi95ds
039000091027     C*
039100091027     C                   call      'TISI95R1'
039200091027     C                   parm                    tisi95r1ds
039300091027     C                   eval      tisi95ds = tisi95r1ds.tisi95ds
039400090810     C*
039500091105     C                   eval      oERR = o95err
039600091105     C*
039700100330     C                   if        i95nar  = *blanks AND
039800100330     C                             o95lia  < '2'
039900100330     C                   eval      oERR = '*'
040000100330     C                   eval      jMsg = 1
040100100330     C                   exsr      rtvMsgLocal
040200100330     C                   else
040300090811     C  N50              eval      wLNA = o95lna
040400090811     C  N50              eval      wZNC = o95znc
040500110621     C*
040600110621     C* Forzatura x "finto" errore
040700110801     C                   if        wLNA > *zeros AND wZNC > *zeros           AND
040800110801     C                             (o95lid = '3' OR o95lid = '4')            AND
040900110801     C                             %subst(tisi95r1ds.si95ocmsg:2:1)<>'*'
041000110621     C                   eval      oERR = *blanks
041100110621     C                   endif
041200110621     C*
041300090810     C                   endif
041400090810     C*
041500090810     C                   ENDSR
041600090811
041700100330
041800090811
041900090811      /TITLE Reperisco descrizioni LNA e LNP
042000090811     C     rtvPODESC     BEGSR
042100090811     C*
042200090811     C* Reperimento descrizione LNP
042300091027     C                   z-add     1             jOrg
042400091027     C     iLNP          lookup    skOrgCod(jOrg)                         88
042500091027     C                   if        %found
042600091027     C                   eval      oLNPD = skOrgDes(jOrg)
042700090811     C                   else
042800100401     C                   eval      oERR = '.'
042900100401     C                   eval      jMsg = 3
043000100401     C                   exsr      rtvMsgLocal
043100090811     C                   eval      RTNesito  = -1
043200100128     C                   eval      RTNopcode = 'ERR LNP'
043300100401     C                   eval      RTNstatus = 97
043400090811     C                   return
043500090811     C                   endif
043600090811     C*
043700090811     C* Reperimento descrizione LNA
043800091027     C                   z-add     1             jOrg
043900091027     C     wLNA          lookup    skOrgCod(jOrg)                         88
044000091027     C                   if        %found
044100091027     C                   eval      oLNAD = skOrgDes(jOrg)
044200090811     C                   else
044300100401     C                   eval      oERR = '.'
044400100401     C                   eval      jMsg = 3
044500100401     C                   exsr      rtvMsgLocal
044600090811     C                   eval      RTNesito  = -1
044700100128     C                   eval      RTNopcode = 'ERR LNA'
044800100401     C                   eval      RTNstatus = 96
044900090811     C                   return
045000090811     C                   endif
045100090811     C*
045200090811     C                   ENDSR
045300100330
045400100330
045500100330
045600100330     C     rtvMsgLocal   BEGSR
045700100330     C*
045800100330     C                   select
045900100330     C* ... Italiano
046000100330     C                   when      tisi95r1ds.si95ilang        = 'ITA' OR
046100100330     C                             %trim(tisi95r1ds.si95ilang) = 'IT'
046200100330     C                   eval      oMSG = MSGITA(jMsg)
046300100330     C* ... Inglese
046400100330     C                   when      tisi95r1ds.si95ilang        = 'ENG' OR
046500100330     C                             %trim(tisi95r1ds.si95ilang) = 'EN'
046600100330     C                   eval      oMSG = MSGENG(jMsg)
046700100330     C* ... Francese
046800100330     C                   when      tisi95r1ds.si95ilang        = 'FRA' OR
046900100330     C                             %trim(tisi95r1ds.si95ilang) = 'FR'
047000100330     C                   eval      oMSG = MSGFRA(jMsg)
047100100330     C* ... Tedesco
047200100330     C                   when      tisi95r1ds.si95ilang        = 'DEU' OR
047300100330     C                             %trim(tisi95r1ds.si95ilang) = 'DE'
047400100330     C                   eval      oMSG = MSGDEU(jMsg)
047500100330     C                   other
047600100330     C                   endsl
047700100330     C*
047800100330     C                   ENDSR
047900100330     C***
048000100330
048100100128
048200100128
048300100504     C     *pssr         BEGSR
048400100128     C*
048500100504     C                   eval      oERR = '.'
048600100504     C                   eval      jMsg = 3
048700100504     C                   exsr      rtvMsgLocal
048800100504     C                   eval      RTNesito  = -1
048900100504     C                   eval      RTNopcode = 'EXEC ERROR'
049000100504     C                   eval      RTNstatus = -1
049100100128     C*
049200100504     C                   seton                                        LR
049300100128     C*
049400100504     C                   ENDSR     '*CANCL'
049500100128     C***
049600100128
049700090810
049800090810
049900090810      /TITLE Valorizzazione finale parametri d output
050000090810     C     valPAROUT     BEGSR
050100090810     C*
050200090810     C                   eval      oTFA = wTFA
050300170512     C                   eval      oLNA = wLNA
050400090810     C                   eval      oZNC = wZNC
050500091027     C*
050600091027     C                   if        tisi95r1ds.si95ojmsg > *zeros
050700100503     C                   eval      oMSG = tisi95r1ds.si95odmsg
050800100503     C                   eval      oTAG = MSGTAG(tisi95r1ds.si95ojmsg)
050900091027     C                   endif
051000100126     C*
051100100126     C* Se presente errore => gestisco
051200090810     C                   eval      RTNesito  = 0
051300100128     C                   eval      RTNopcode = 'SUCCESS'
051400100128     C                   eval      RTNstatus = 00
051500090810     C*
051600090810     C                   ENDSR
051700100330     C*--------------------------------------------------------------------
051800091007** MSGTAG - MESSAGGI
051900100504CodNazione                                                                       1
052000100504Cap                                                                              2
052100100504CodTipoServizio                                                                  3
052200100504                                                                                 4
052300100504                                                                                 5
052400100504                                                                                 6
052500100504                                                                                 7
052600100504Cap                                                                              8
052700100504CodProvincia                                                                     9
052800100504                                                                                 A
052900100504Localita                                                                         B
053000100504CodProvincia                                                                     C
053100100504Localita                                                                         D
053200100504CodProvincia                                                                     E
053300100504                                                                                 F
053400100504                                                                                 G
053500100504Localita                                                                         H
053600100504Localita                                                                         I
053700100504                                                                                 L
053800100504                                                                                 M
053900100504CodTipoServizio                                                                  P
054000100504CodTipoServizio                                                                  Q
054100100504CodNetwork                                                                       R
054200100504CodTipoServizio                                                                  S
054300100504CodNetwork                                                                       T
054400100504CodNetwork|CodNazione|Cap                                                        U
054500100504CodNetwork|CodNazione|Cap|TBOCBO                                                 V
054600100504CodNetwork|CodNazione|Cap|TBOCBO                                                 W
054700100504CodNetwork|CodNazione|Cap                                                        X
054800100504                                                                                 Y
054900100330** MSGITA - MESSAGGI
055000100330Impossibile calcolare un esatto instradamento.
055100100401Parametri di input non validi.
055200100401Errore interno imprevisto.
055300100330
055400100330
055500100330** MSGENG - MESSAGGI
055600100330Unable to calculate exact routing.
055700100401Invalid input parameters.
055800100401Unexpected internal error.
055900100330
056000100330
056100100330** MSGFRA - MESSAGGI
056200100401Impossible de calculer exactement le routage.
056300100401Les paramètres d'entrée ne sont pas valides.
056400100401Inattendue erreur interne.
056500100330
056600100330
056700100330** MSGDEU - MESSAGGI
056800100401Unmöglich zu berechnen exakte Route.
056900100401Input-Parameter ungültig.
057000100401Unerwarteter interner Fehler.
057100100330
057200100330
