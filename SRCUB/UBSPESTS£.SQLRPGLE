000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200160112      * da "sia *PGM che *MODULE presente nell'omonimo *SRVPGM" è rimasto solo *PGM
000300160112     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*caller)
000400120113
000500120116      * Per compilare aggiungere la lib. dati di filiale (se si è in un SI di sede) FILTRAPRD
000600111216      * o la lib. dati di sede (se si è in un SI di filiale) GAITRAGRPS
000700111216
000800120116     F*-----------
000900120116     FTABEL00F  IF   E           K DISK
001000120116     F*-----------
001100031103
001200041015     D*-----------
001300041015     D* VARIABILI D WRK
001400041015     D*-----------
001500120116     D wCausAnom       s              2A   dim(200)
001600120116     D wDescAnom       s             20A   dim(200)
001700120117     D wCausLAvv       s              3A   dim(20)
001800120113     D* lingua in maiuscolo
001900120113     D wLingua         s                   like(pInLingua)
002000120116     D Inz             s              1
002100120116     D wAAS            s                   like(pInAnno)
002200120116     D wLNP            s                   like(pInLineaPar)
002300120116     D wNRS            s                   like(pInSerie)
002400120116     D wNSP            s                   like(pInNumSped)
002500120116     D wEsito1         s              1
002600120116     D wEsito2         s              1
002700120116     D wrkDCM          s              8s 0
002800120116     D wrkCCA          s              1
002900141106     D wrkFDA          s              1
003000120113     D* indici
003100120113     D wi              s              3s 0
003200120113     D wx              s              3s 0
003300110929
003400111216
003500111216     D*-----------
003600111216     D* PARAMETRI
003700111216     D*-----------
003800120113     D pInAnno         S              4s 0
003900111221     D pInLineaPar     S              3s 0
004000111221     D pInSerie        S              2s 0
004100111221     D pInNumSped      S              7s 0
004200120113     D pInLingua       S              2
004300120113     D pInMaiuscolo    S              1
004400120113     D pOutLblTyp      S              1
004500120116     D pOutRblBO       S              1
004600120113     D pOutStsSped     S              2
004700141106     D pOutDtConsRich  S              8S 0
004800120117     D pOutDtApGiac    s              8s 0
004900120117     D pOutDtLAvviso   s              8s 0
005000120113     D pOutDescSts     S            128
005100120116     D pOutDtSts       S              8S 0
005200120113     D pOutErrore      S              1
005300141106     D pOutCA          S              1
005400141106     D pOutDtPrGiac    S              8s 0
005500141106     D pOutDtChGiac    S              8s 0
005600031103
005700120113     D* parametri UBLBLSPE_GetLblTyp
005800120116     D*pOutLblTyp      S              1A
005900120113     D pOutAnnoBO      S              4S 0
006000120113     D pOutLineaParBO  S              3S 0
006100120113     D pOutSerieBO     S              2S 0
006200120113     D pOutNumSpedBO   S              7S 0
006300120113     D pOutDcmBO       S              8S 0
006400120113     D pOutCcaBO       S              1A
006500120116     D*pOutRblBO       S              1A
006600120113     D* parametri UBLBLSPE_GetLastChild
006700120116     D pOutAnnoFI      S              4S 0
006800120116     D pOutLineaParFI  S              3S 0
006900120116     D pOutSerieFI     S              2S 0
007000120116     D pOutNumSpedFI   S              7S 0
007100120116     D pOutDcmFI       S              8S 0
007200120116     D pOutCcaFI       S              1A
007300120113     D* parametri UBRTVSPE
007400120113     D pInRichiesta    S              3A
007500120113     D pOutTITAS_DS  E DS                  extname(TITAS00F)
007600120113     D pOutFNARB_DS  E DS                  extname(FNARB00F)
007700120113     D pOutFNBLP_DS  E DS                  extname(FNBLP00F)
007800120113     D pOutEsitoSPE    S              3A
007900120116     D* parametri UBRTVGIAC
008000120116     D pInNumProg      S              2S 0
008100120116     D pOutTIGCP_DS  E DS                  extname(TIGCP51L)
008200120116     D pOutEsitoGIA    S              1A
008300120117     D* parametri UBRTVEVB
008400120117     D pInCausale      S              3a
008500120117     D pInDtEvento     S              8s 0
008600120117     D* tipo chiamata:
008700120117     D*  0=prima chiamata
008800120117     D*  1=successive
008900120117     D pInTpChiamEVB   S              1s 0
009000120117     D pOutFNEVB_DS    DS                  occurs(50)
009100120117     D  pOutCausale                   3a
009200120117     D  pOutDtEvento                  8s 0
009300120117     D  pOutOraEvento                 4s 0
009400120117     D  pOutDtImmEv                   8s 0
009500120117     D  pOutOraImmEv                  6s 0
009600120117     D  pOutFilImmEv                  3s 0
009700120117     D* esito reperimento:
009800120117     D*  0=non reperito
009900120117     D*  1=reperito
010000120117     D pOutEsitoEVB    S              1A
010100120117     D pOutNrOccurs    S              3S 0
010200120117     D*  0=non letti tutti i dati (da fare altra chiamata)
010300120117     D*  1=finiti di leggere tutti i dati
010400120117     D pOutEnd         S              1A
010500120113
010600110928     D*------------------
010700120116     D* DS Esterne
010800110928     D*------------------
010900120116     D DS7O          E DS
011000120117     D DS2A          E DS
011100120116
011200120116     D*------------------
011300120116     D* LINKING A DEFINIZIONI ESTERNE
011400120116     D*------------------
011500120113     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
011600120113     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
011700110928
011800030924
011900030325
012000030924     C* MAIN/
012100120113     C*
012200120113     C* Inizializza extra *inzsr
012300120113     C                   IF        Inz <> '1'
012400120117     C                   EXSR      Inizia1Volta
012500120113     C                   EVAL      Inz = '1'
012600120113     C                   ENDIF
012700120117     C*
012800120117     C* Inizializza sempre
012900120117     C                   EXSR      IniziaSempre
013000041015     C*
013100120113     C* Reperisce le info sulla spedizione
013200120113     C                   EXSR      RepInfoSped
013300120113     C*
013400120113     C* Reperisce data consegna richiesta
013500120113     C                   EXSR      RepDtConsRich
013600120113     C*
013700120113     C* Definisce descrizione stato
013800120113     C                   EXSR      DefDescrSts
013900120116     C*
014000120116     C* Restituisco la descrizione in maiuscolo/minuscolo a seconda della richiesta
014100120116      /FREE
014200120116       //porto la lingua in maiuscolo
014300120116       if pInMaiuscolo = '1';
014400120116         exec sql
014500120116           set :pOutDescSts = ucase(:pOutDescSts);
014600120116       else;
014700120116         exec sql
014800120116           set :pOutDescSts = lcase(:pOutDescSts);
014900120116       endif;
015000120116      /END-FREE
015100110929     C*
015200111216     C                   SETON                                        RT
015300111216     C                   RETURN
015400031103
015500031103
015600031103
015700120117     C*------------------------------------------------------------------------*
015800120117     C* Inizializza ogni volta
015900120117     C*------------------------------------------------------------------------*
016000120117     C     IniziaSempre  BEGSR
016100141106     C*
016200141106      /FREE
016300141106       //porto la lingua in maiuscolo
016400141106       exec sql
016500141106         set :wLingua = ucase(:pInLingua);
016600141106      /END-FREE
016700141106     C*
016800141106     C*
016900141106     C* controllo consistenza parametri in ingresso
017000141106     C                   IF        pInAnno = 0 or
017100141106     C                             pInLineaPar = 0 or
017200141106     C                             pInNumSped = 0
017300141106     C* estremi spedizione non corretti
017400141106     C                   EVAL      pOutErrore = '2'
017500141106     C                   RETURN
017600141106     C                   ENDIF
017700141106     C                   IF        wLingua <> 'IT' and
017800141106     C                             wLingua <> 'EN' and
017900141106     C                             wLingua <> 'FR' and
018000141106     C                             wLingua <> 'DE'
018100141106     C* cod. lingua inesistente
018200141106     C                   EVAL      pOutErrore = '3'
018300141106     C                   RETURN
018400141106     C                   ENDIF
018500141106     C* se il flag maiuscolo/minuscolo è vuoto, gli do come default maiuscolo
018600141106     C                   IF        pInMaiuscolo = *blank
018700141106     C                   EVAL      pInMaiuscolo = *on
018800141106     C                   ENDIF
018900141106     C                   IF        pInMaiuscolo <> *on and
019000141106     C                             pInMaiuscolo <> *off
019100141106     C* flag maiuscolo/minuscolo non corretto
019200141106     C                   EVAL      pOutErrore = '4'
019300141106     C                   RETURN
019400141106     C                   ENDIF
019500120117     C*
019600120117     C                   CLEAR                   pOutLblTyp
019700120117     C                   CLEAR                   pOutRblBO
019800120117     C                   CLEAR                   pOutStsSped
019900141106     C                   CLEAR                   pOutDtConsRich
020000120117     C                   CLEAR                   pOutDtApGiac
020100120117     C                   CLEAR                   pOutDtLAvviso
020200120117     C                   CLEAR                   pOutDescSts
020300120117     C                   CLEAR                   pOutDtSts
020400141106     C                   CLEAR                   pOutCA
020500120117     C                   EVAL      pOutErrore = '0'
020600120117     C*
020700120117     C                   ENDSR
020800120113     C*------------------------------------------------------------------------*
020900120113     C* Reperimento info su spedizione
021000120113     C*------------------------------------------------------------------------*
021100120113     C     RepInfoSped   BEGSR
021200120113     C*
021300120113     C                   setoff                                       4041
021400120116     C                   movel     *blanks       wEsito1
021500120116     C                   movel     *blanks       wEsito2
021600120116     C                   z-add     *zeros        wrkDCM
021700120116     C                   movel     *blanks       wrkCCA
021800120113     C*
021900120113     C                   eval      wAAS = pInAnno
022000120113     C                   eval      wLNP = pInLineaPar
022100120113     C                   eval      wNRS = pInSerie
022200120113     C                   eval      wNSP = pInNumSped
022300120113     C*
022400120113     C* Chiamata metodo GetLblTyp
022500120113     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
022600120113     C                                                wAAS
022700120113     C                                               :wLNP
022800120113     C                                               :wNRS
022900120113     C                                               :wNSP
023000120113     C                                               :pOutLblTyp
023100120113     C                                               :pOutAnnoBO
023200120113     C                                               :pOutLineaParBO
023300120113     C                                               :pOutSerieBO
023400120113     C                                               :pOutNumSpedBO
023500120113     C                                               :pOutDcmBO
023600120113     C                                               :pOutCcaBO
023700120113     C                                               :pOutRblBO))
023800120113     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
023900120113     C                   if        pOutLblTyp <> 'O'
024000120113     C                   eval      wAAS = pOutAnnoBO
024100120113     C                   eval      wLNP = pOutLineaParBO
024200120113     C                   eval      wNRS = pOutSerieBO
024300120113     C                   eval      wNSP = pOutNumSpedBO
024400120113     C                   endif
024500120113     C*
024600120113     C* Chiamata metodo GetLastChild
024700120113     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
024800120113     C                                                wAAS
024900120113     C                                               :wLNP
025000120113     C                                               :wNRS
025100120113     C                                               :wNSP
025200120113     C                                               :pOutAnnoFI
025300120113     C                                               :pOutLineaParFI
025400120113     C                                               :pOutSerieFI
025500120113     C                                               :pOutNumSpedFI
025600120113     C                                               :pOutDcmFI
025700120113     C                                               :pOutCcaFI))
025800120113     C*
025900120113     C* Considerazioni sullo stato corrente della bolla
026000120113     C                   IF        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
026100120113     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
026200120113     C                   seton                                        40        * bolla cons.
026300120113     C                   ENDIF
026400120113     C*
026500120113     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
026600120113     C                                            and pOutCcaBO = *blanks) or
026700120113     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
026800120113     C                                            and pOutCcaFI = *blanks)
026900120113     C                   seton                                        41        * bolla cons. OK
027000120113     C                   endif
027100120113     C*
027200120113     C                   SELECT
027300120113     C                   WHEN      *in40 = *off and *in41 = *off
027400120113     C                   EVAL      pOutStsSped = 'NP'
027500120113     C                   WHEN      *in40 = *off and *in41 = *on
027600120113     C* non può esistere perché il 41 si dovrebbe accendere per gli stessi motivi del 40 con in più
027700120113     C* altri test in AND
027800120113     C                   EVAL      pOutErrore = '5'
027900120113     C                   RETURN
028000120113     C                   WHEN      *in40 = *on  and *in41 = *off
028100120113     C                   EVAL      pOutStsSped = 'SK'
028200120113     C                   WHEN      *in40 = *on  and *in41 = *on
028300120113     C                   EVAL      pOutStsSped = 'SO'
028400120116     C                   ENDSL
028500120113     C*
028600120113     C                   ENDSR
028700120113
028800120113
028900120113     C*------------------------------------------------------------------------*
029000120113     C* Reperisce data consegna richiesta
029100120113     C*------------------------------------------------------------------------*
029200120113     C     RepDtConsRich BEGSR
029300120113     C*
029400141106     C                   movel     *blanks       wrkFDA
029500141106     C*
029600120113     C                   call      'UBRTVSPE'
029700120113     C                   parm      'APS'         pInRichiesta
029800120113     C                   parm      wAAS          pInAnno
029900120113     C                   parm      wLNP          pInLineaPar
030000120113     C                   parm      wNRS          pInSerie
030100120113     C                   parm      wNSP          pInNumSped
030200120113     C                   parm                    pOutFNARB_DS
030300120113     C                   parm                    pOutFNBLP_DS
030400120113     C                   parm                    pOutTITAS_DS
030500120113     C                   parm                    pOutEsitoSPE
030600120113     C*
030700120116     C* reperisco la data consegna dal record trovato e, se ne ho trovato più di uno,
030800120116     C* respetto la sequenza APS
030900120116     C                   IF        %subst(pOutEsitoSPE : 1 : 1) = 'A'
031000141106     C                   EVAL      pOutDtConsRich = ARBDCR
031100120116     C                   ELSE
031200120116     C                   IF        %subst(pOutEsitoSPE : 2 : 1) = 'P'
031300141106     C                   EVAL      pOutDtConsRich = BLPDCR
031400120116     C                   ELSE
031500120116     C                   IF        %subst(pOutEsitoSPE : 2 : 1) = 'S'
031600141106     C                   EVAL      pOutDtConsRich = TASDCR
031700120116     C                   ELSE
031800141106     C                   EVAL      pOutDtConsRich = *zeros
031900120116     C                   ENDIF
032000141106     C                   ENDIF
032100120116     C                   ENDIF
032200141106     C*
032300141106     C                   EVAL      wrkFDA = TASFDA
032400141106     C                   EVAL      pOutCA = wrkFDA
032500120116     C*
032600120113     C                   ENDSR
032700120113
032800120113
032900120113     C*------------------------------------------------------------------------*
033000120113     C* Definisce descrizione stato
033100120113     C*------------------------------------------------------------------------*
033200120113     C     DefDescrSts   BEGSR
033300120113     C*
033400120113     C                   if        wEsito2 = '0'
033500120113     C                   eval      wrkDCM = pOutDcmFI
033600120113     C                   eval      wrkCCA = pOutCcaFI
033700120113     C                   else
033800120113     C                   eval      wrkDCM = pOutDcmBO
033900120113     C                   eval      wrkCCA = pOutCcaBO
034000120113     C                   endif
034100120113     C*
034200120116     C                   EVAL      pOutDtSts      = wrkDCM
034300120113     C*
034400120113     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
034500120113     C                   IF        wrkDCM > *zeros AND
034600120113     C                             (wrkCCA = *blanks OR wrkCCA = '1')
034700120116     C                   EVAL      pOutDescSts    = 'Consegnata'
034800120113     C                   ELSE
034900120113     C*
035000120113     C* A questo punto stabilisco la descrizione
035100120113     C                   SELECT
035200120113     C                   WHEN      wrkCCA = '1'
035300120116     C                   EVAL      pOutDescSts    = 'Dirottamento'
035400120113     C                   WHEN      wrkCCA = '2'
035500120116     C                   EVAL      pOutDescSts    = 'Reso al mittente'
035600120116     C                   EVAL      pOutStsSped = 'SR'
035700120113     C                   WHEN      wrkCCA = '3'
035800120116     C                   EVAL      pOutDescSts    = 'Rapinata merce'
035900120113     C                   WHEN      wrkCCA = '4'
036000120116     C                   EVAL      pOutDescSts    = 'Rapinato incasso'
036100120113     C                   WHEN      wrkCCA = '5'
036200120116     C                   EVAL      pOutDescSts    = 'Chiusura con anomalia'
036300120113     C                   WHEN      wrkCCA = '6'
036400120116     C                   EVAL      pOutDescSts    = 'Reso al mittente su avaria'
036500120113     C                   WHEN      wrkCCA = '7'
036600120116     C                   EVAL      pOutDescSts    = 'Merce mai affidata'
036700120113     C                   WHEN      wrkCCA = 'A'
036800120116     C                   EVAL      pOutDescSts    = 'Apertura pratica anomalia'
036900120113     C                   WHEN      wrkCCA = 'C'
037000120116     C                   EVAL      pOutDescSts    = 'In consegna'
037100120113     C                   WHEN      wrkCCA = 'R'
037200120116     C                   EVAL      pOutDescSts    = 'Spedizione ripristinata'
037300120113     C                   WHEN      wrkCCA = 'S'
037400120116     C                   EVAL      pOutDescSts    = 'Merce distrutta'
037500120113     C                   OTHER
037600120116     C                   EVAL      pOutDescSts    = '*****'
037700120113     C                   ENDSL
037800120117     C*
037900120117     C                   ENDIF
038000120116     C*
038100120116     C* se ho il codice anomalia
038200120116     C                   IF        wrkCCA <> *blank
038300120116     C* reperisco la descrizione da TABEL in lingua
038400120117     C                   SELECT
038500120117     C                   WHEN      wLingua = 'IT'
038600120117     C                   EVAL      wi = %lookup('1' + wrkCCA
038700120117     C                                          : wCausAnom)
038800120117     C                   WHEN      wLingua = 'EN'
038900120117     C                   EVAL      wi = %lookup('2' + wrkCCA
039000120117     C                                          : wCausAnom)
039100120117     C                   WHEN      wLingua = 'FR'
039200120117     C                   EVAL      wi = %lookup('3' + wrkCCA
039300120117     C                                          : wCausAnom)
039400120117     C                   WHEN      wLingua = 'DE'
039500120117     C                   EVAL      wi = %lookup('4' + wrkCCA
039600120116     C                                          : wCausAnom)
039700120117     C                   ENDSL
039800120116     C                   IF        wi > 0
039900120117     C                   EVAL      pOutDescSts    = wDescAnom(wi)
040000120116     C                   ENDIF
040100120116     C                   ENDIF
040200120116     C*
040300120116     C*
040400120117     C                   EVAL      pOutDtApGiac = *zeros
040500141106     C                   EVAL      pOutDtPrGiac = *zeros
040600141106     C                   EVAL      pOutDtChGiac = *zeros
040700120116     C* Reperisco l'ultima giacenza
040800120116     C                   call      'UBRTVGIAC'
040900120116     C                   parm      wAAS          pInAnno
041000120116     C                   parm      wLNP          pInLineaPar
041100120116     C                   parm      wNRS          pInSerie
041200120116     C                   parm      wNSP          pInNumSped
041300120116     C                   parm      *zeros        pInNumProg
041400120116     C                   parm                    pOutTIGCP_DS
041500120116     C                   parm                    pOutEsitoGIA
041600120116     C* se ho trovato la giacenza costrisco la data coll'anno e il mese
041700120116     C                   IF        pOutEsitoGIA = '1'
041800141106     C                   EVAL      pOutDtApGiac = GCPDUR
041900141106     C                   EVAL      pOutDtPrGiac = GCPAGC*10000+GCPMGC
042000141106     C                   EVAL      pOutDtChGiac = GCPDCG
042100120116     C                   ENDIF
042200120117     C*
042300120117     C                   EVAL      pOutDtLAvviso = *zeros
042400120117     C*
042500120117     C* Reperisco gli eventi senza porre filtri (al massimo 50 chiamate)
042600120117     C* per ogni eveto reperito controllo se la sua causale è di "lasciato avviso", se lo è
042700120117     C* mi salvo la data evento; l'unltima salvata sarà quella che restituirò al chiamante
042800120117     C* (il driver mi rende i dati con data ASCEND)
042900120117     C                   z-add     *zeros        pInTpChiamEVB
043000120117     C                   dow       pOutEnd <> '1' AND pInTpChiamEVB < 50
043100120117     C                   call      'UBRTVEVB'
043200120117     C                   parm      wAAS          pInAnno
043300120117     C                   parm      wLNP          pInLineaPar
043400120117     C                   parm      wNRS          pInSerie
043500120117     C                   parm      wNSP          pInNumSped
043600120117     C                   parm      *blank        pInCausale
043700120117     C                   parm                    pInDtEvento
043800120117     C                   parm                    pInTpChiamEVB
043900120117     C                   parm                    pOutFNEVB_DS
044000120117     C                   parm                    pOutEsitoEVB
044100120117     C                   parm                    pOutNrOccurs
044200120117     C                   parm                    pOutEnd
044300120117     C*
044400120117     C* Reperisco i dati del buffer FNDCT per ogni occorrenza trovata
044500120117     C                   IF        pOutNrOccurs > *zeros
044600120117     C     1             DO        pOutNrOccurs  wx
044700120117     C     wx            occur     pOutFNEVB_DS
044800120117     C*
044900120117     C* verifico se la causale evento è di lasciato avviso
045000120117     C                   EVAL      wi = %lookup(pOutCausale : wCausLAvv)
045100120117     C                   IF        wi > 0
045200120117     C                   EVAL      pOutDtLAvviso = pOutDtEvento
045300120117     C                   ENDIF
045400120117     C*
045500120117     C                   ENDDO
045600120117     C                   ENDIF
045700120117     C*
045800120117     C* Proseguo con le successive chiamate al driver
045900120117     C                   add       1             pInTpChiamEVB
046000120117     C                   enddo
046100120117     C*
046200120117     C* Ultime verifiche per la data stato e per la descrizione
046300120113     C*
046400120117     C                   IF        (pOutDtLAvviso > wrkDCM AND
046500120117     C                              pOutDtLAvviso >  pOutDtApGiac)
046600120117     C                   EVAL      pOutDtSts      = pOutDtLAvviso
046700120116     C                   EVAL      pOutDescSts    = 'Lasciato avviso'
046800120117     C                   ENDIF
046900120117     C                   IF        (pOutDtApGiac > wrkDCM AND
047000120117     C                              pOutDtApGiac >= pOutDtLAvviso)
047100120117     C                   EVAL      pOutDtSts      = pOutDtApGiac
047200120116     C                   EVAL      pOutDescSts    = 'In giacenza'
047300120113     C                   ENDIF
047400120113     C                   IF        wrkDCM = *zeros AND
047500120117     C                             pOutDtLAvviso = *zeros AND
047600120117     C                             pOutDtApGiac = *zeros AND
047700120113     C                             wrkCCA = *blanks
047800120116     C                   EVAL      pOutDtSts      = %dec(%date() : *ISO)
047900120113     C*
048000120113     C* Verifico la data consegna richiesta
048100141106     C                   IF        pOutDtConsRich > %dec(%date() : *ISO)
048200120116     C                   EVAL      pOutDescSts    = 'Richiesta consegna il ' +
048300120113     C                              UBFMTDATE_Convert(
048400141106     C                                  %editc(pOutDtConsRich:'4') :
048500120113     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
048600120113     C                   ELSE
048700120116     C                   EVAL      pOutDescSts    = 'In lavorazione'
048800120113     C                   ENDIF
048900120113     C                   ENDIF
049000120113     C*
049100120113     C                   ENDSR
049200991027
049300991027      /TITLE Operazioni iniziali.
049400991027     C     *inzsr        BEGSR
049500991027     C*
049600991027     C     *ENTRY        PLIST
049700111221     C                   PARM                    pInAnno
049800111216     C                   PARM                    pInLineaPar
049900120113     C                   PARM                    pInSerie
050000111216     C                   PARM                    pInNumSped
050100120113     C                   PARM                    pInLingua
050200120113     C                   PARM                    pInMaiuscolo
050300120113     C                   PARM                    pOutLblTyp
050400120113     C                   PARM                    pOutRblBO
050500120113     C                   PARM                    pOutStsSped
050600141106     C                   PARM                    pOutDtConsRich
050700120117     C                   PARM                    pOutDtApGiac
050800120117     C                   PARM                    pOutDtLAvviso
050900120113     C                   PARM                    pOutDescSts
051000120116     C                   PARM                    pOutDtSts
051100120113     C                   PARM                    pOutErrore
051200141106     C                   PARM                    pOutCA
051300141106     C                   PARM                    pOutDtPrGiac
051400141106     C                   PARM                    pOutDtChGiac
051500030709     C*
051600030709     C* Definizione chiavi
051700120113     C*
051800120113     C* TABEL
051900120113     C     K02TAB00      KLIST
052000120113     C                   KFLD                    TBLKUt
052100120113     C                   KFLD                    TBLCod
052200110929     C*
052300111216     C* Chiave su FNARB01L / FNBLP01L  Completa / TITAS30C  parziale
052400111216     C     K04TAS30      KLIST
052500111216     C                   KFLD                    pInAnno
052600111216     C                   KFLD                    pInLineaPar
052700111216     C                   KFLD                    pInSerie
052800111216     C                   KFLD                    pInNumSped
052900991027     C*
053000991027     C                   ENDSR
053100120113
053200120113
053300120113     C*------------------------------------------------------------------------*
053400120113     C* Inizializzazioni non in *inzsr
053500120113     C*------------------------------------------------------------------------*
053600120117     C     Inizia1Volta  BEGSR
053700120113     C*
053800120113     C*
053900120113     C* reperisco le causali anomalia nelle varie lingue
054000120113     C* mettendo nella schiera delle anomalie anche la lingua così concatenate: <lingua><causale>
054100120117     C                   Z-ADD     0             wx
054200120116     C     1             DO        4             wi
054300120113     C                   EVAL      TBLKUt = wi
054400120113     C                   MOVEL     '7O'          TBLCod
054500120113     C     K02TAB00      SETLL     TABEL
054600120113     C     K02TAB00      READE     TABEL
054700120113     C                   DOW       not %eof
054800120113     C                   ADD       1             wx
054900120113     C                   EVAL      wCausAnom(wx) = %char(TBLKUt) +
055000120113     C                                             %subst(TBLKey : 1 : 1)
055100120113     C                   MOVEL     TBLUNI        DS7O
055200120113     C                   EVAL      wDescAnom(wx) = §7ODeI
055300120116     C     K02TAB00      READE     TABEL                                  31
055400120113     C                   ENDDO
055500120116     C                   ENDDO
055600120113     C*
055700120117     C* reperisco le causali eventi
055800120117     C                   Z-ADD     0             wx
055900120117     C                   EVAL      TBLKUt = 1
056000120117     C                   MOVEL     '2A'          TBLCod
056100120117     C     K02TAB00      SETLL     TABEL
056200120117     C     K02TAB00      READE     TABEL
056300120117     C                   DOW       not %eof
056400120117     C                   MOVEL     TBLUNI        DS2A
056500120117     C* se tipo evento di mancata consegna è "lasciato avviso"
056600120117     C                   IF        §2AFTC = 'A'
056700120117     C                   ADD       1             wx
056800120117     C                   EVAL      wCausLAvv(wx) = TBLKey
056900120117     C                   ENDIF
057000120117     C     K02TAB00      READE     TABEL                                  31
057100120117     C                   ENDDO
057200120117     C*
057300120113     C                   ENDSR
