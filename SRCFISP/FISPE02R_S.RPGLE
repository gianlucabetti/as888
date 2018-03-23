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
005900100504     D wZNC            S              2S 0
006000090810     D*
006100100504     D skOrgCod        S              3S 0 DIM(999)
006200100504     D skOrgDes        S             20A   DIM(999)
006300100504     D jOrg            S              4S 0
006400100504     D jMsg            S              2S 0
006500100504     D datcor          S              8S 0
006600091027     D*
006700080620
006800091027     D/COPY GAITRASRC/SRCPROTOPI,TISI95R1
006900091027
007000091027
007100080620      //********************************************************************************************
007200080620      //
007300080620      // Definizione parametri procedura.
007400080620      //
007500080620      //********************************************************************************************
007600080620     C     *ENTRY        PLIST
007700100330     C                   PARM                    iTYPE
007800090810     C                   PARM                    iLANG
007900090810     C                   PARM                    iDATA
008000090810     C                   PARM                    iNTW
008100090810     C                   PARM                    iLNP
008200090810     C                   PARM                    iNZD
008300090810     C                   PARM                    iPRD
008400090810     C                   PARM                    iCAD
008500090810     C                   PARM                    iLOD
008600090810     C                   PARM                    iIND
008700090810     C                   PARM                    iRSD
008800100114     C                   PARM                    iNCL
008900090810     C                   PARM                    iPKB
009000090810     C                   PARM                    iVLB
009100090810     C                   PARM                    iFFD
009200090810     C                   PARM                    iTSP
009300090810     C                   PARM                    iTC1
009400090810     C                   PARM                    iTC2
009500100504     C                   PARM                    iCBO
009600090810     C                   PARM                    oTFA
009700090810     C                   PARM                    oLNA
009800090810     C                   PARM                    oZNC
009900090811     C                   PARM                    oLNPD
010000090811     C                   PARM                    oLNAD
010100090810     C                   PARM                    oERR
010200100503     C                   PARM                    oMSG
010300100503     C                   PARM                    oTAG
010400080620     C                   PARM                    rtnesito
010500080620     C                   PARM                    rtnopcode
010600080620     C                   PARM                    rtnstatus
010700090810     C*
010800090810     C* Inizializzazione indicatori d procedura
010900100330     C                   setoff                                       5051
011000090810     C*
011100090810     C* Reperimento data corrente
011200090810     C                   exsr      repDATCOR
011300090810     C*
011400090810     C* Inizializzazione parametri output e variabili wrk
011500090810     C                   exsr      inzPAROUT
011600090810     C*
011700090810     C* Verifico i parametri d input
011800090810     C                   exsr      chkPARIN
011900100401     C*
012000100401     C* Proseguo solo se finora tutto ok
012100100401     C                   if        oERR = *blanks
012200100330     C*
012300100330     C* Verifico il tipo lancio => 'C' = calcolo instradamento  'V' = verifica dati indirizzo
012400100330     C                   if        iTYPE = 'C'
012500100330     C                   seton                                        51
012600100330     C                   endif
012700090810     C*
012800090810     C* Reperisco il terminal di partenza
012900100330     C   51              exsr      rtvTFP
013000090810     C*
013100090810     C* Verifica destinatari "particolari"
013200100330     C   51              exsr      chkDEST
013300090810     C*
013400090810     C* Calcolo instradamento
013500090810     C                   exsr      calINSTR
013600100401     C*
013700100401     C                   endif
013800100401     C*
013900100401     C*-------------------------
014000091007     C*
014100091007     C* Proseguo solo se finora tutto ok
014200110621     C                   if        wLNA > *zeros
014300090810     C*
014400090810     C* Reperisco il terminal di arrivo
014500100330     C   51              exsr      rtvTFA
014600090811     C*
014700090811     C* Reperisco descrizioni LNA e LNP
014800100330     C   51              exsr      rtvPODESC
014900091007     C*
015000091007     C                   endif
015100090810     C*
015200090810     C* Valorizzazione finale parametri d output
015300090810     C                   exsr      valPAROUT
015400090810     C*
015500090810     C                   return
015600091027
015700100330
015800091027
015900091027      /TITLE Inizializzazioni
016000091027     C     *inzSR        BEGSR
016100091027     C*
016200091027     C*---
016300091027     C* Organigramma
016400091027     C*---
016500091027     C                   z-add     *zeros        jOrg
016600091027     C     *start        setll     azorg01l
016700091027     C                   read      azorg01l
016800091027     C                   dow       not %eof(azorg01l)
016900091027     C                   if        orgfva = *blanks
017000091027     C                   add       1             jOrg
017100091027     C                   eval      skOrgCod(jOrg) = orgfil
017200091027     C                   eval      skOrgDes(jOrg) = orgdes
017300091027     C                   endif
017400091027     C                   read      azorg01l
017500091027     C                   enddo
017600091027     C*
017700091027     C                   ENDSR
017800090810
017900100330
018000090810
018100090810      /TITLE Reperimento data corrente
018200090810     C     repDATCOR     BEGSR
018300090810     C*
018400091027     C                   eval      datcor = %DEC(%DATE():*ISO)
018500090810     C*
018600090810     C                   ENDSR
018700090810
018800100330
018900090810
019000090810      /TITLE Inizializzazione parametri output
019100090810     C     inzPAROUT     BEGSR
019200090810     C*
019300090810     C                   clear                   rtnesito
019400090810     C                   clear                   rtnopcode
019500100330     C                   clear                   rtnstatus
019600090810     C*
019700090810     C                   clear                   oTFA
019800090810     C                   clear                   oLNA
019900090810     C                   clear                   oZNC
020000090811     C                   clear                   oLNPD
020100090811     C                   clear                   oLNAD
020200090810     C                   clear                   oERR
020300100503     C                   clear                   oMSG
020400100503     C                   clear                   oTAG
020500090810     C*
020600090810     C                   clear                   wDOS
020700090810     C                   clear                   wTFP
020800090810     C                   clear                   wTFA
020900090810     C                   clear                   wLNA
021000090810     C                   clear                   wZNC
021100090810     C*
021200090810     C                   ENDSR
021300090810
021400100330
021500090810
021600090810      /TITLE Verifica parametri di input
021700090810     C     chkPARIN      BEGSR
021800100401     C*
021900100401     C                   setoff                                       10
022000090810     C*
022100090810     C                   if        iDATA = *zeros
022200090810     C                   eval      iDATA = datcor
022300090810     C                   endif
022400100330     C*
022500100330     C                   if        iTYPE = *blanks
022600100330     C                   eval      iTYPE = 'C'
022700100330     C                   endif
022800100401     C*
022900100401     C                   if        iTYPE <> *blanks AND
023000100401     C                             iTYPE <> 'V'     AND
023100100401     C                             iTYPE <> 'C'
023200100401     C                   seton                                        51
023300100401     C                   endif
023400100401     C*
023500100401     C* Procedo solo se parametri in ingresso sono consistenti
023600100401     C                   if        *in10
023700100401     C                   eval      oERR = '.'
023800100401     C                   eval      jMsg = 2
023900100401     C                   exsr      rtvMsgLocal
024000100401     C                   eval      RTNesito  = -1
024100100401     C                   eval      RTNopcode = 'ERR PARAMS'
024200100401     C                   eval      RTNstatus = 95
024300100401     C                   endif
024400090810     C*
024500090810     C                   ENDSR
024600090810
024700100330
024800090810
024900090810      /TITLE Reperisco il terminal di partenza
025000090810     C     rtvTFP        BEGSR
025100090810     C*
025200090810     C                   clear                   fnlv55ds
025300090811     C                   movel     'P'           d55tpt
025400090810     C                   z-add     iLNP          d55lin
025500090810     C                   z-add     iLNP          d55lnp
025600090810     C                   z-add     iDATA         d55drf
025700090810     C                   call      'FNLV55R'
025800090810     C                   PARM                    fnlv55ds
025900090810     C*
026000090810     C                   if        d55err = *blanks
026100090810     C                   z-add     d55tfp        wTFP
026200090810     C                   else
026300100401     C                   eval      oERR = '.'
026400100401     C                   eval      jMsg = 3
026500100401     C                   exsr      rtvMsgLocal
026600090810     C                   eval      RTNesito  = -1
026700100128     C                   eval      RTNopcode = 'ERR TFP'
026800100401     C                   eval      RTNstatus = 99
026900090810     C                   return
027000090810     C                   endif
027100090810     C*
027200090810     C                   ENDSR
027300090810
027400100330
027500090810
027600090810      /TITLE Reperisco il terminal di arrivo
027700090810     C     rtvTFA        BEGSR
027800090810     C*
027900090810     C                   clear                   fnlv55ds
028000090811     C                   movel     'A'           d55tpt
028100090810     C                   z-add     wLNA          d55lin
028200090810     C                   z-add     iLNP          d55lnp
028300090810     C                   z-add     iDATA         d55drf
028400090810     C                   call      'FNLV55R'
028500090810     C                   PARM                    fnlv55ds
028600090810     C*
028700090810     C                   if        d55err = *blanks
028800090810     C                   z-add     d55tfa        wTFA
028900090810     C                   else
029000100401     C                   eval      oERR = '.'
029100100401     C                   eval      jMsg = 3
029200100401     C                   exsr      rtvMsgLocal
029300090810     C                   eval      RTNesito  = -1
029400100128     C                   eval      RTNopcode = 'ERR TFA'
029500100401     C                   eval      RTNstatus = 98
029600090810     C                   return
029700090810     C                   endif
029800090810     C*
029900090810     C                   ENDSR
030000090810
030100100330
030200090810
030300090810      /TITLE Verifica destinatari "particolari"
030400090810     C     chkDEST       BEGSR
030500090810     C*
030600090810     C                   clear                   tisit0ds
030700090810     C                   movel     iNZD          it0NAZ
030800090810     C                   movel     iPRD          it0PRV
030900090810     C                   movel     iCAD          it0CAP
031000090810     C                   movel     iLOD          it0LOC
031100090810     C                   movel     iIND          it0IND
031200090810     C                   movel     iRSD          it0RAG
031300090810     C                   call      'TISIT5R'
031400090810     C                   parm                    tisit0ds
031500090810     C*
031600090810     C* Se trovato destinatario tra i dest. particolari verifico se presenti linea/zona
031700090810     C* instradamento prestabiliti
031800090810     C                   if        ot0ERR = *blanks
031900090810     C                   eval      ddstflo = ot0FLO
032000090811     C                   eval      wDOS    = ot0dos
032100090810     C                   if        §DSTLNA <> *blanks AND §DSTZNC <> *blanks
032200090810     C                   move(p)   §DSTLNA       wLNA
032300090810     C                   move(p)   §DSTZNC       wZNC
032400090810     C                   seton                                        50
032500090810     C                   endif
032600090810     C                   endif
032700090810     C*
032800090810     C                   ENDSR
032900090810
033000100330
033100090810
033200090810      /TITLE Calcolo instradamento
033300090810     C     calINSTR      BEGSR
033400091027     C*
033500091027     C                   clear                   tisi95ds
033600091027     C                   clear                   tisi95r1ds
033700090810     C                   eval      i95tcn = '7'
033800090810     C                   eval      i95dat = iDATA
033900090810     C                   eval      i95fi1 = iNTW
034000090810     C                   eval      i95nar = iNZD
034100090810     C                   eval      i95prv = iPRD
034200090810     C                   eval      i95cap = iCAD
034300090810     C                   eval      i95loc = iLOD
034400090810     C                   eval      i95lkg = iPKB
034500090810     C                   eval      i95lmc = iVLB
034600090810     C                   eval      i95ffd = iFFD
034700090810     C                   eval      i95tsp = iTSP
034800090810     C                   eval      i95tfp = wTFP
034900100330     C   51              eval      i95fre = 'S'
035000100504     C*
035100100504     C                   select
035200100504     C                   when      iCBO = '1'
035300100504     C                   eval      i95tpo = 'F'
035400100504     C                   when      iCBO = '2'
035500100504     C                   eval      i95tpo = 'A'
035600100504     C                   when      iCBO = '4'
035700100504     C                   eval      i95tpo = 'F'
035800100504     C                   eval      i95fca = 'S'
035900100504     C                   when      iCBO = '6'
036000100504     C                   eval      i95tpo = 'A'
036100100504     C                   eval      i95fca = 'S'
036200100504     C                   endsl
036300100504     C*
036400090810     C                   if        wDOS = 'S'
036500090810     C                   move      wDOS          i95fi2
036600090810     C                   endif
036700090810     C                   if        iTC1 = 'S'
036800090810     C                   movel     iTC1          i95fi2
036900090810     C                   endif
037000090810     C                   if        iTC2 = 'S'
037100090810     C                   movel     iTC2          i95fi2
037200090810     C                   endif
037300091027     C*
037400100331     C                   eval      tisi95r1ds.si95itype = iTYPE
037500091027     C                   eval      tisi95r1ds.si95ilang = iLANG
037600100115     C                   eval      tisi95r1ds.si95incl  = iNCL
037700091027     C                   eval      tisi95r1ds.tisi95ds  = tisi95ds
037800091027     C*
037900091027     C                   call      'TISI95R1'
038000091027     C                   parm                    tisi95r1ds
038100091027     C                   eval      tisi95ds = tisi95r1ds.tisi95ds
038200090810     C*
038300091105     C                   eval      oERR = o95err
038400091105     C*
038500100330     C                   if        i95nar  = *blanks AND
038600100330     C                             o95lia  < '2'
038700100330     C                   eval      oERR = '*'
038800100330     C                   eval      jMsg = 1
038900100330     C                   exsr      rtvMsgLocal
039000100330     C                   else
039100090811     C  N50              eval      wLNA = o95lna
039200090811     C  N50              eval      wZNC = o95znc
039300110621     C*
039400110621     C* Forzatura x "finto" errore
039500110801     C                   if        wLNA > *zeros AND wZNC > *zeros           AND
039600110801     C                             (o95lid = '3' OR o95lid = '4')            AND
039700110801     C                             %subst(tisi95r1ds.si95ocmsg:2:1)<>'*'
039800110621     C                   eval      oERR = *blanks
039900110621     C                   endif
040000110621     C*
040100090810     C                   endif
040200090810     C*
040300090810     C                   ENDSR
040400090811
040500100330
040600090811
040700090811      /TITLE Reperisco descrizioni LNA e LNP
040800090811     C     rtvPODESC     BEGSR
040900090811     C*
041000090811     C* Reperimento descrizione LNP
041100091027     C                   z-add     1             jOrg
041200091027     C     iLNP          lookup    skOrgCod(jOrg)                         88
041300091027     C                   if        %found
041400091027     C                   eval      oLNPD = skOrgDes(jOrg)
041500090811     C                   else
041600100401     C                   eval      oERR = '.'
041700100401     C                   eval      jMsg = 3
041800100401     C                   exsr      rtvMsgLocal
041900090811     C                   eval      RTNesito  = -1
042000100128     C                   eval      RTNopcode = 'ERR LNP'
042100100401     C                   eval      RTNstatus = 97
042200090811     C                   return
042300090811     C                   endif
042400090811     C*
042500090811     C* Reperimento descrizione LNA
042600091027     C                   z-add     1             jOrg
042700091027     C     wLNA          lookup    skOrgCod(jOrg)                         88
042800091027     C                   if        %found
042900091027     C                   eval      oLNAD = skOrgDes(jOrg)
043000090811     C                   else
043100100401     C                   eval      oERR = '.'
043200100401     C                   eval      jMsg = 3
043300100401     C                   exsr      rtvMsgLocal
043400090811     C                   eval      RTNesito  = -1
043500100128     C                   eval      RTNopcode = 'ERR LNA'
043600100401     C                   eval      RTNstatus = 96
043700090811     C                   return
043800090811     C                   endif
043900090811     C*
044000090811     C                   ENDSR
044100100330
044200100330
044300100330
044400100330     C     rtvMsgLocal   BEGSR
044500100330     C*
044600100330     C                   select
044700100330     C* ... Italiano
044800100330     C                   when      tisi95r1ds.si95ilang        = 'ITA' OR
044900100330     C                             %trim(tisi95r1ds.si95ilang) = 'IT'
045000100330     C                   eval      oMSG = MSGITA(jMsg)
045100100330     C* ... Inglese
045200100330     C                   when      tisi95r1ds.si95ilang        = 'ENG' OR
045300100330     C                             %trim(tisi95r1ds.si95ilang) = 'EN'
045400100330     C                   eval      oMSG = MSGENG(jMsg)
045500100330     C* ... Francese
045600100330     C                   when      tisi95r1ds.si95ilang        = 'FRA' OR
045700100330     C                             %trim(tisi95r1ds.si95ilang) = 'FR'
045800100330     C                   eval      oMSG = MSGFRA(jMsg)
045900100330     C* ... Tedesco
046000100330     C                   when      tisi95r1ds.si95ilang        = 'DEU' OR
046100100330     C                             %trim(tisi95r1ds.si95ilang) = 'DE'
046200100330     C                   eval      oMSG = MSGDEU(jMsg)
046300100330     C                   other
046400100330     C                   endsl
046500100330     C*
046600100330     C                   ENDSR
046700100330     C***
046800100330
046900100128
047000100128
047100100504     C     *pssr         BEGSR
047200100128     C*
047300100504     C                   eval      oERR = '.'
047400100504     C                   eval      jMsg = 3
047500100504     C                   exsr      rtvMsgLocal
047600100504     C                   eval      RTNesito  = -1
047700100504     C                   eval      RTNopcode = 'EXEC ERROR'
047800100504     C                   eval      RTNstatus = -1
047900100128     C*
048000100504     C                   seton                                        LR
048100100128     C*
048200100504     C                   ENDSR     '*CANCL'
048300100128     C***
048400100128
048500090810
048600090810
048700090810      /TITLE Valorizzazione finale parametri d output
048800090810     C     valPAROUT     BEGSR
048900090810     C*
049000090810     C                   eval      oTFA = wTFA
049100090810     C                   eval      oLNA = wLNA
049200090810     C                   eval      oZNC = wZNC
049300091027     C*
049400091027     C                   if        tisi95r1ds.si95ojmsg > *zeros
049500100503     C                   eval      oMSG = tisi95r1ds.si95odmsg
049600100503     C                   eval      oTAG = MSGTAG(tisi95r1ds.si95ojmsg)
049700091027     C                   endif
049800100126     C*
049900100126     C* Se presente errore => gestisco
050000090810     C                   eval      RTNesito  = 0
050100100128     C                   eval      RTNopcode = 'SUCCESS'
050200100128     C                   eval      RTNstatus = 00
050300090810     C*
050400090810     C                   ENDSR
050500100330     C*--------------------------------------------------------------------
050600091007** MSGTAG - MESSAGGI
050700100504CodNazione                                                                       1
050800100504Cap                                                                              2
050900100504CodTipoServizio                                                                  3
051000100504                                                                                 4
051100100504                                                                                 5
051200100504                                                                                 6
051300100504                                                                                 7
051400100504Cap                                                                              8
051500100504CodProvincia                                                                     9
051600100504                                                                                 A
051700100504Localita                                                                         B
051800100504CodProvincia                                                                     C
051900100504Localita                                                                         D
052000100504CodProvincia                                                                     E
052100100504                                                                                 F
052200100504                                                                                 G
052300100504Localita                                                                         H
052400100504Localita                                                                         I
052500100504                                                                                 L
052600100504                                                                                 M
052700100504CodTipoServizio                                                                  P
052800100504CodTipoServizio                                                                  Q
052900100504CodNetwork                                                                       R
053000100504CodTipoServizio                                                                  S
053100100504CodNetwork                                                                       T
053200100504CodNetwork|CodNazione|Cap                                                        U
053300100504CodNetwork|CodNazione|Cap|TBOCBO                                                 V
053400100504CodNetwork|CodNazione|Cap|TBOCBO                                                 W
053500100504CodNetwork|CodNazione|Cap                                                        X
053600100504                                                                                 Y
053700100330** MSGITA - MESSAGGI
053800100330Impossibile calcolare un esatto instradamento.
053900100401Parametri di input non validi.
054000100401Errore interno imprevisto.
054100100330
054200100330
054300100330** MSGENG - MESSAGGI
054400100330Unable to calculate exact routing.
054500100401Invalid input parameters.
054600100401Unexpected internal error.
054700100330
054800100330
054900100330** MSGFRA - MESSAGGI
055000100401Impossible de calculer exactement le routage.
055100100401Les paramètres d'entrée ne sont pas valides.
055200100401Inattendue erreur interne.
055300100330
055400100330
055500100330** MSGDEU - MESSAGGI
055600100401Unmöglich zu berechnen exakte Route.
055700100401Input-Parameter ungültig.
055800100401Unerwarteter interner Fehler.
055900100330
056000100330
