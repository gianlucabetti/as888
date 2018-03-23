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
007500180109     D pInAnno_p       S              4p 0
007600180109     D pInLinea_p      S              3p 0
007700180109     D pInSerie_p      S              2p 0
007800180109     D pInNumSped_p    S              7p 0
007900120113     D pOutTITAS_DS  E DS                  extname(TITAS00F)
008000120113     D pOutFNARB_DS  E DS                  extname(FNARB00F)
008100120113     D pOutFNBLP_DS  E DS                  extname(FNBLP00F)
008200120113     D pOutEsitoSPE    S              3A
008300120116     D* parametri UBRTVGIAC
008400120116     D pInNumProg      S              2S 0
008500120116     D pOutTIGCP_DS  E DS                  extname(TIGCP51L)
008600120116     D pOutEsitoGIA    S              1A
008700120117     D* parametri UBRTVEVB
008800120117     D pInCausale      S              3a
008900120117     D pInDtEvento     S              8s 0
009000120117     D* tipo chiamata:
009100120117     D*  0=prima chiamata
009200120117     D*  1=successive
009300120117     D pInTpChiamEVB   S              1s 0
009400120117     D pOutFNEVB_DS    DS                  occurs(50)
009500120117     D  pOutCausale                   3a
009600120117     D  pOutDtEvento                  8s 0
009700120117     D  pOutOraEvento                 4s 0
009800120117     D  pOutDtImmEv                   8s 0
009900120117     D  pOutOraImmEv                  6s 0
010000120117     D  pOutFilImmEv                  3s 0
010100120117     D* esito reperimento:
010200120117     D*  0=non reperito
010300120117     D*  1=reperito
010400120117     D pOutEsitoEVB    S              1A
010500120117     D pOutNrOccurs    S              3S 0
010600120117     D*  0=non letti tutti i dati (da fare altra chiamata)
010700120117     D*  1=finiti di leggere tutti i dati
010800120117     D pOutEnd         S              1A
010900120113
011000110928     D*------------------
011100120116     D* DS Esterne
011200110928     D*------------------
011300120116     D DS7O          E DS
011400120117     D DS2A          E DS
011500120116
011600120116     D*------------------
011700120116     D* LINKING A DEFINIZIONI ESTERNE
011800120116     D*------------------
011900120113     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
012000120113     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
012100110928
012200030924
012300030325
012400030924     C* MAIN/
012500120113     C*
012600120113     C* Inizializza extra *inzsr
012700120113     C                   IF        Inz <> '1'
012800120117     C                   EXSR      Inizia1Volta
012900120113     C                   EVAL      Inz = '1'
013000120113     C                   ENDIF
013100120117     C*
013200120117     C* Inizializza sempre
013300120117     C                   EXSR      IniziaSempre
013400041015     C*
013500120113     C* Reperisce le info sulla spedizione
013600120113     C                   EXSR      RepInfoSped
013700120113     C*
013800120113     C* Reperisce data consegna richiesta
013900120113     C                   EXSR      RepDtConsRich
014000120113     C*
014100120113     C* Definisce descrizione stato
014200120113     C                   EXSR      DefDescrSts
014300120116     C*
014400120116     C* Restituisco la descrizione in maiuscolo/minuscolo a seconda della richiesta
014500120116      /FREE
014600120116       //porto la lingua in maiuscolo
014700120116       if pInMaiuscolo = '1';
014800120116         exec sql
014900120116           set :pOutDescSts = ucase(:pOutDescSts);
015000120116       else;
015100120116         exec sql
015200120116           set :pOutDescSts = lcase(:pOutDescSts);
015300120116       endif;
015400120116      /END-FREE
015500110929     C*
015600111216     C                   SETON                                        RT
015700111216     C                   RETURN
015800031103
015900031103
016000031103
016100120117     C*------------------------------------------------------------------------*
016200120117     C* Inizializza ogni volta
016300120117     C*------------------------------------------------------------------------*
016400120117     C     IniziaSempre  BEGSR
016500141106     C*
016600141106      /FREE
016700141106       //porto la lingua in maiuscolo
016800141106       exec sql
016900141106         set :wLingua = ucase(:pInLingua);
017000141106      /END-FREE
017100141106     C*
017200141106     C*
017300141106     C* controllo consistenza parametri in ingresso
017400141106     C                   IF        pInAnno = 0 or
017500141106     C                             pInLineaPar = 0 or
017600141106     C                             pInNumSped = 0
017700141106     C* estremi spedizione non corretti
017800141106     C                   EVAL      pOutErrore = '2'
017900141106     C                   RETURN
018000141106     C                   ENDIF
018100141106     C                   IF        wLingua <> 'IT' and
018200141106     C                             wLingua <> 'EN' and
018300141106     C                             wLingua <> 'FR' and
018400141106     C                             wLingua <> 'DE'
018500141106     C* cod. lingua inesistente
018600141106     C                   EVAL      pOutErrore = '3'
018700141106     C                   RETURN
018800141106     C                   ENDIF
018900141106     C* se il flag maiuscolo/minuscolo è vuoto, gli do come default maiuscolo
019000141106     C                   IF        pInMaiuscolo = *blank
019100141106     C                   EVAL      pInMaiuscolo = *on
019200141106     C                   ENDIF
019300141106     C                   IF        pInMaiuscolo <> *on and
019400141106     C                             pInMaiuscolo <> *off
019500141106     C* flag maiuscolo/minuscolo non corretto
019600141106     C                   EVAL      pOutErrore = '4'
019700141106     C                   RETURN
019800141106     C                   ENDIF
019900120117     C*
020000120117     C                   CLEAR                   pOutLblTyp
020100120117     C                   CLEAR                   pOutRblBO
020200120117     C                   CLEAR                   pOutStsSped
020300141106     C                   CLEAR                   pOutDtConsRich
020400120117     C                   CLEAR                   pOutDtApGiac
020500120117     C                   CLEAR                   pOutDtLAvviso
020600120117     C                   CLEAR                   pOutDescSts
020700120117     C                   CLEAR                   pOutDtSts
020800141106     C                   CLEAR                   pOutCA
020900120117     C                   EVAL      pOutErrore = '0'
021000120117     C*
021100120117     C                   ENDSR
021200120113     C*------------------------------------------------------------------------*
021300120113     C* Reperimento info su spedizione
021400120113     C*------------------------------------------------------------------------*
021500120113     C     RepInfoSped   BEGSR
021600120113     C*
021700120113     C                   setoff                                       4041
021800120116     C                   movel     *blanks       wEsito1
021900120116     C                   movel     *blanks       wEsito2
022000120116     C                   z-add     *zeros        wrkDCM
022100120116     C                   movel     *blanks       wrkCCA
022200120113     C*
022300120113     C                   eval      wAAS = pInAnno
022400120113     C                   eval      wLNP = pInLineaPar
022500120113     C                   eval      wNRS = pInSerie
022600120113     C                   eval      wNSP = pInNumSped
022700120113     C*
022800120113     C* Chiamata metodo GetLblTyp
022900120113     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
023000120113     C                                                wAAS
023100120113     C                                               :wLNP
023200120113     C                                               :wNRS
023300120113     C                                               :wNSP
023400120113     C                                               :pOutLblTyp
023500120113     C                                               :pOutAnnoBO
023600120113     C                                               :pOutLineaParBO
023700120113     C                                               :pOutSerieBO
023800120113     C                                               :pOutNumSpedBO
023900120113     C                                               :pOutDcmBO
024000120113     C                                               :pOutCcaBO
024100120113     C                                               :pOutRblBO))
024200120113     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
024300120113     C                   if        pOutLblTyp <> 'O'
024400120113     C                   eval      wAAS = pOutAnnoBO
024500120113     C                   eval      wLNP = pOutLineaParBO
024600120113     C                   eval      wNRS = pOutSerieBO
024700120113     C                   eval      wNSP = pOutNumSpedBO
024800120113     C                   endif
024900120113     C*
025000120113     C* Chiamata metodo GetLastChild
025100120113     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
025200120113     C                                                wAAS
025300120113     C                                               :wLNP
025400120113     C                                               :wNRS
025500120113     C                                               :wNSP
025600120113     C                                               :pOutAnnoFI
025700120113     C                                               :pOutLineaParFI
025800120113     C                                               :pOutSerieFI
025900120113     C                                               :pOutNumSpedFI
026000120113     C                                               :pOutDcmFI
026100120113     C                                               :pOutCcaFI))
026200120113     C*
026300120113     C* Considerazioni sullo stato corrente della bolla
026400120113     C                   IF        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
026500120113     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
026600120113     C                   seton                                        40        * bolla cons.
026700120113     C                   ENDIF
026800120113     C*
026900120113     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
027000120113     C                                            and pOutCcaBO = *blanks) or
027100120113     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
027200120113     C                                            and pOutCcaFI = *blanks)
027300120113     C                   seton                                        41        * bolla cons. OK
027400120113     C                   endif
027500120113     C*
027600120113     C                   SELECT
027700120113     C                   WHEN      *in40 = *off and *in41 = *off
027800120113     C                   EVAL      pOutStsSped = 'NP'
027900120113     C                   WHEN      *in40 = *off and *in41 = *on
028000120113     C* non può esistere perché il 41 si dovrebbe accendere per gli stessi motivi del 40 con in più
028100120113     C* altri test in AND
028200120113     C                   EVAL      pOutErrore = '5'
028300120113     C                   RETURN
028400120113     C                   WHEN      *in40 = *on  and *in41 = *off
028500120113     C                   EVAL      pOutStsSped = 'SK'
028600120113     C                   WHEN      *in40 = *on  and *in41 = *on
028700120113     C                   EVAL      pOutStsSped = 'SO'
028800120116     C                   ENDSL
028900120113     C*
029000120113     C                   ENDSR
029100120113
029200120113
029300120113     C*------------------------------------------------------------------------*
029400120113     C* Reperisce data consegna richiesta
029500120113     C*------------------------------------------------------------------------*
029600120113     C     RepDtConsRich BEGSR
029700120113     C*
029800141106     C                   movel     *blanks       wrkFDA
029900141106     C*
030000180109     C                   eval      pInAnno_p   = wAAS
030100180109     C                   eval      pInLinea_p  = wLNP
030200180109     C                   eval      pInSerie_p  = wNRS
030300180109     C                   eval      pInNumSped_p= wNSP
030400180109     C                   call      'UBRTVSPE'
030500120113     C                   parm      'APS'         pInRichiesta
030600180109     C                   parm                    pInAnno_p
030700180109     C                   parm                    pInLinea_p
030800180109     C                   parm                    pInSerie_p
030900180109     C                   parm                    pInNumSped_p
031000180109     C                   parm                    pOutTITAS_DS
031100120113     C                   parm                    pOutFNARB_DS
031200120113     C                   parm                    pOutFNBLP_DS
031300120113     C                   parm                    pOutEsitoSPE
031400120113     C*
031500120116     C* reperisco la data consegna dal record trovato e, se ne ho trovato più di uno,
031600120116     C* respetto la sequenza APS
031700120116     C                   IF        %subst(pOutEsitoSPE : 1 : 1) = 'A'
031800141106     C                   EVAL      pOutDtConsRich = ARBDCR
031900120116     C                   ELSE
032000120116     C                   IF        %subst(pOutEsitoSPE : 2 : 1) = 'P'
032100141106     C                   EVAL      pOutDtConsRich = BLPDCR
032200120116     C                   ELSE
032300120116     C                   IF        %subst(pOutEsitoSPE : 2 : 1) = 'S'
032400141106     C                   EVAL      pOutDtConsRich = TASDCR
032500120116     C                   ELSE
032600141106     C                   EVAL      pOutDtConsRich = *zeros
032700120116     C                   ENDIF
032800141106     C                   ENDIF
032900120116     C                   ENDIF
033000141106     C*
033100141106     C                   EVAL      wrkFDA = TASFDA
033200141106     C                   EVAL      pOutCA = wrkFDA
033300120116     C*
033400120113     C                   ENDSR
033500120113
033600120113
033700120113     C*------------------------------------------------------------------------*
033800120113     C* Definisce descrizione stato
033900120113     C*------------------------------------------------------------------------*
034000120113     C     DefDescrSts   BEGSR
034100120113     C*
034200120113     C                   if        wEsito2 = '0'
034300120113     C                   eval      wrkDCM = pOutDcmFI
034400120113     C                   eval      wrkCCA = pOutCcaFI
034500120113     C                   else
034600120113     C                   eval      wrkDCM = pOutDcmBO
034700120113     C                   eval      wrkCCA = pOutCcaBO
034800120113     C                   endif
034900120113     C*
035000120116     C                   EVAL      pOutDtSts      = wrkDCM
035100120113     C*
035200120113     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
035300120113     C                   IF        wrkDCM > *zeros AND
035400120113     C                             (wrkCCA = *blanks OR wrkCCA = '1')
035500120116     C                   EVAL      pOutDescSts    = 'Consegnata'
035600120113     C                   ELSE
035700120113     C*
035800120113     C* A questo punto stabilisco la descrizione
035900120113     C                   SELECT
036000120113     C                   WHEN      wrkCCA = '1'
036100120116     C                   EVAL      pOutDescSts    = 'Dirottamento'
036200120113     C                   WHEN      wrkCCA = '2'
036300120116     C                   EVAL      pOutDescSts    = 'Reso al mittente'
036400120116     C                   EVAL      pOutStsSped = 'SR'
036500120113     C                   WHEN      wrkCCA = '3'
036600120116     C                   EVAL      pOutDescSts    = 'Rapinata merce'
036700120113     C                   WHEN      wrkCCA = '4'
036800120116     C                   EVAL      pOutDescSts    = 'Rapinato incasso'
036900120113     C                   WHEN      wrkCCA = '5'
037000120116     C                   EVAL      pOutDescSts    = 'Chiusura con anomalia'
037100120113     C                   WHEN      wrkCCA = '6'
037200120116     C                   EVAL      pOutDescSts    = 'Reso al mittente su avaria'
037300120113     C                   WHEN      wrkCCA = '7'
037400120116     C                   EVAL      pOutDescSts    = 'Merce mai affidata'
037500120113     C                   WHEN      wrkCCA = 'A'
037600120116     C                   EVAL      pOutDescSts    = 'Apertura pratica anomalia'
037700120113     C                   WHEN      wrkCCA = 'C'
037800120116     C                   EVAL      pOutDescSts    = 'In consegna'
037900120113     C                   WHEN      wrkCCA = 'R'
038000120116     C                   EVAL      pOutDescSts    = 'Spedizione ripristinata'
038100120113     C                   WHEN      wrkCCA = 'S'
038200120116     C                   EVAL      pOutDescSts    = 'Merce distrutta'
038300120113     C                   OTHER
038400120116     C                   EVAL      pOutDescSts    = '*****'
038500120113     C                   ENDSL
038600120117     C*
038700120117     C                   ENDIF
038800120116     C*
038900120116     C* se ho il codice anomalia
039000120116     C                   IF        wrkCCA <> *blank
039100120116     C* reperisco la descrizione da TABEL in lingua
039200120117     C                   SELECT
039300120117     C                   WHEN      wLingua = 'IT'
039400120117     C                   EVAL      wi = %lookup('1' + wrkCCA
039500120117     C                                          : wCausAnom)
039600120117     C                   WHEN      wLingua = 'EN'
039700120117     C                   EVAL      wi = %lookup('2' + wrkCCA
039800120117     C                                          : wCausAnom)
039900120117     C                   WHEN      wLingua = 'FR'
040000120117     C                   EVAL      wi = %lookup('3' + wrkCCA
040100120117     C                                          : wCausAnom)
040200120117     C                   WHEN      wLingua = 'DE'
040300120117     C                   EVAL      wi = %lookup('4' + wrkCCA
040400120116     C                                          : wCausAnom)
040500120117     C                   ENDSL
040600120116     C                   IF        wi > 0
040700120117     C                   EVAL      pOutDescSts    = wDescAnom(wi)
040800120116     C                   ENDIF
040900120116     C                   ENDIF
041000120116     C*
041100120116     C*
041200120117     C                   EVAL      pOutDtApGiac = *zeros
041300141106     C                   EVAL      pOutDtPrGiac = *zeros
041400141106     C                   EVAL      pOutDtChGiac = *zeros
041500120116     C* Reperisco l'ultima giacenza
041600120116     C                   call      'UBRTVGIAC'
041700120116     C                   parm      wAAS          pInAnno
041800120116     C                   parm      wLNP          pInLineaPar
041900120116     C                   parm      wNRS          pInSerie
042000120116     C                   parm      wNSP          pInNumSped
042100120116     C                   parm      *zeros        pInNumProg
042200120116     C                   parm                    pOutTIGCP_DS
042300120116     C                   parm                    pOutEsitoGIA
042400120116     C* se ho trovato la giacenza costrisco la data coll'anno e il mese
042500120116     C                   IF        pOutEsitoGIA = '1'
042600141106     C                   EVAL      pOutDtApGiac = GCPDUR
042700141106     C                   EVAL      pOutDtPrGiac = GCPAGC*10000+GCPMGC
042800141106     C                   EVAL      pOutDtChGiac = GCPDCG
042900120116     C                   ENDIF
043000120117     C*
043100120117     C                   EVAL      pOutDtLAvviso = *zeros
043200120117     C*
043300120117     C* Reperisco gli eventi senza porre filtri (al massimo 50 chiamate)
043400120117     C* per ogni eveto reperito controllo se la sua causale è di "lasciato avviso", se lo è
043500120117     C* mi salvo la data evento; l'unltima salvata sarà quella che restituirò al chiamante
043600120117     C* (il driver mi rende i dati con data ASCEND)
043700120117     C                   z-add     *zeros        pInTpChiamEVB
043800120117     C                   dow       pOutEnd <> '1' AND pInTpChiamEVB < 50
043900120117     C                   call      'UBRTVEVB'
044000120117     C                   parm      wAAS          pInAnno
044100120117     C                   parm      wLNP          pInLineaPar
044200120117     C                   parm      wNRS          pInSerie
044300120117     C                   parm      wNSP          pInNumSped
044400120117     C                   parm      *blank        pInCausale
044500120117     C                   parm                    pInDtEvento
044600120117     C                   parm                    pInTpChiamEVB
044700120117     C                   parm                    pOutFNEVB_DS
044800120117     C                   parm                    pOutEsitoEVB
044900120117     C                   parm                    pOutNrOccurs
045000120117     C                   parm                    pOutEnd
045100120117     C*
045200120117     C* Reperisco i dati del buffer FNDCT per ogni occorrenza trovata
045300120117     C                   IF        pOutNrOccurs > *zeros
045400120117     C     1             DO        pOutNrOccurs  wx
045500120117     C     wx            occur     pOutFNEVB_DS
045600120117     C*
045700120117     C* verifico se la causale evento è di lasciato avviso
045800120117     C                   EVAL      wi = %lookup(pOutCausale : wCausLAvv)
045900120117     C                   IF        wi > 0
046000120117     C                   EVAL      pOutDtLAvviso = pOutDtEvento
046100120117     C                   ENDIF
046200120117     C*
046300120117     C                   ENDDO
046400120117     C                   ENDIF
046500120117     C*
046600120117     C* Proseguo con le successive chiamate al driver
046700120117     C                   add       1             pInTpChiamEVB
046800120117     C                   enddo
046900120117     C*
047000120117     C* Ultime verifiche per la data stato e per la descrizione
047100120113     C*
047200120117     C                   IF        (pOutDtLAvviso > wrkDCM AND
047300120117     C                              pOutDtLAvviso >  pOutDtApGiac)
047400120117     C                   EVAL      pOutDtSts      = pOutDtLAvviso
047500120116     C                   EVAL      pOutDescSts    = 'Lasciato avviso'
047600120117     C                   ENDIF
047700120117     C                   IF        (pOutDtApGiac > wrkDCM AND
047800120117     C                              pOutDtApGiac >= pOutDtLAvviso)
047900120117     C                   EVAL      pOutDtSts      = pOutDtApGiac
048000120116     C                   EVAL      pOutDescSts    = 'In giacenza'
048100120113     C                   ENDIF
048200120113     C                   IF        wrkDCM = *zeros AND
048300120117     C                             pOutDtLAvviso = *zeros AND
048400120117     C                             pOutDtApGiac = *zeros AND
048500120113     C                             wrkCCA = *blanks
048600120116     C                   EVAL      pOutDtSts      = %dec(%date() : *ISO)
048700120113     C*
048800120113     C* Verifico la data consegna richiesta
048900141106     C                   IF        pOutDtConsRich > %dec(%date() : *ISO)
049000120116     C                   EVAL      pOutDescSts    = 'Richiesta consegna il ' +
049100120113     C                              UBFMTDATE_Convert(
049200141106     C                                  %editc(pOutDtConsRich:'4') :
049300120113     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
049400120113     C                   ELSE
049500120116     C                   EVAL      pOutDescSts    = 'In lavorazione'
049600120113     C                   ENDIF
049700120113     C                   ENDIF
049800120113     C*
049900120113     C                   ENDSR
050000991027
050100991027      /TITLE Operazioni iniziali.
050200991027     C     *inzsr        BEGSR
050300991027     C*
050400991027     C     *ENTRY        PLIST
050500111221     C                   PARM                    pInAnno
050600111216     C                   PARM                    pInLineaPar
050700120113     C                   PARM                    pInSerie
050800111216     C                   PARM                    pInNumSped
050900120113     C                   PARM                    pInLingua
051000120113     C                   PARM                    pInMaiuscolo
051100120113     C                   PARM                    pOutLblTyp
051200120113     C                   PARM                    pOutRblBO
051300120113     C                   PARM                    pOutStsSped
051400141106     C                   PARM                    pOutDtConsRich
051500120117     C                   PARM                    pOutDtApGiac
051600120117     C                   PARM                    pOutDtLAvviso
051700120113     C                   PARM                    pOutDescSts
051800120116     C                   PARM                    pOutDtSts
051900120113     C                   PARM                    pOutErrore
052000141106     C                   PARM                    pOutCA
052100141106     C                   PARM                    pOutDtPrGiac
052200141106     C                   PARM                    pOutDtChGiac
052300030709     C*
052400030709     C* Definizione chiavi
052500120113     C*
052600120113     C* TABEL
052700120113     C     K02TAB00      KLIST
052800120113     C                   KFLD                    TBLKUt
052900120113     C                   KFLD                    TBLCod
053000110929     C*
053100111216     C* Chiave su FNARB01L / FNBLP01L  Completa / TITAS30C  parziale
053200111216     C     K04TAS30      KLIST
053300111216     C                   KFLD                    pInAnno
053400111216     C                   KFLD                    pInLineaPar
053500111216     C                   KFLD                    pInSerie
053600111216     C                   KFLD                    pInNumSped
053700991027     C*
053800991027     C                   ENDSR
053900120113
054000120113
054100120113     C*------------------------------------------------------------------------*
054200120113     C* Inizializzazioni non in *inzsr
054300120113     C*------------------------------------------------------------------------*
054400120117     C     Inizia1Volta  BEGSR
054500120113     C*
054600120113     C*
054700120113     C* reperisco le causali anomalia nelle varie lingue
054800120113     C* mettendo nella schiera delle anomalie anche la lingua così concatenate: <lingua><causale>
054900120117     C                   Z-ADD     0             wx
055000120116     C     1             DO        4             wi
055100120113     C                   EVAL      TBLKUt = wi
055200120113     C                   MOVEL     '7O'          TBLCod
055300120113     C     K02TAB00      SETLL     TABEL
055400120113     C     K02TAB00      READE     TABEL
055500120113     C                   DOW       not %eof
055600120113     C                   ADD       1             wx
055700120113     C                   EVAL      wCausAnom(wx) = %char(TBLKUt) +
055800120113     C                                             %subst(TBLKey : 1 : 1)
055900120113     C                   MOVEL     TBLUNI        DS7O
056000120113     C                   EVAL      wDescAnom(wx) = §7ODeI
056100120116     C     K02TAB00      READE     TABEL                                  31
056200120113     C                   ENDDO
056300120116     C                   ENDDO
056400120113     C*
056500120117     C* reperisco le causali eventi
056600120117     C                   Z-ADD     0             wx
056700120117     C                   EVAL      TBLKUt = 1
056800120117     C                   MOVEL     '2A'          TBLCod
056900120117     C     K02TAB00      SETLL     TABEL
057000120117     C     K02TAB00      READE     TABEL
057100120117     C                   DOW       not %eof
057200120117     C                   MOVEL     TBLUNI        DS2A
057300120117     C* se tipo evento di mancata consegna è "lasciato avviso"
057400120117     C                   IF        §2AFTC = 'A'
057500120117     C                   ADD       1             wx
057600120117     C                   EVAL      wCausLAvv(wx) = TBLKey
057700120117     C                   ENDIF
057800120117     C     K02TAB00      READE     TABEL                                  31
057900120117     C                   ENDDO
058000120117     C*
058100120113     C                   ENDSR
