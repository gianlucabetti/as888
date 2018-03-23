000100000000     H*------------------------------------------------------------------------*
000200000104     H* IMPORTAZIONE CAPPARIO DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500060427     F*------------------------------------------------------------------------*
000600060427     F* DATA BASE
000700060427     F*------------------------------------------------------------------------*
000800060427     FDPCRO01L  UF A E           K DISK
000900060427     FWFDPCROF  IF   E           K DISK
001000060502     FTABEL00F  IF   E           K DISK
001100060503     FTNTBE01L  IF   E           K DISK
001200060502     FDPCDP02L  IF   E           K DISK
001300060427     FPRTF198   O    F  198        PRINTER
001400060502     D*----------------
001500060502     D* DS ESTERNE
001600060502     D*-------------------
001700060502     D DS15          E DS                  INZ
001800060517     D XTOEBCDDS     E DS                  INZ
001900060427     D*----------------
002000060427     D* DS PARAMETRI DI LANCIO PGM
002100060427     D*-------------------
002200060427     D PARAM           DS                  INZ
002300060427     D  PARVER                        5  0
002400060502     D*----------------
002500060502     D* DS RIDEFINIZIONE SERVIZI
002600060502     D*-------------------
002700060502     D DS_SRVC_A       DS                  INZ
002800060502     D  SRVC_S_A                      1
002900060502     D  SRVC_B_A                      3
003000060502     D  SRVC_E_A                      3
003100060502     D*
003200060502     D DS_SRVC         DS                  INZ
003300060502     D  SRVC_S                        1
003400060502     D  SRVC_B                        3  0
003500060502     D  SRVC_E                        3  0
003600060503     D*----------------
003700060503     D* DS RIDEFINIZIONE DEPOT PARTENZA (ROUTE PLACES)
003800060503     D*-------------------
003900060503     D DS_ROUP_C       DS                  INZ
004000060503     D  ROUP_C_T                      1
004100060503     D  ROUP_C_C                      2
004200060503     D*
004300060503     D DS_ROUP_D       DS                  INZ
004400060503     D  ROUP_D_T                      1
004500060503     D  ROUP_D_B                      4
004600060503     D  ROUP_D_E                      4
004700060503     D*
004800060503     D DS_ROUP_G       DS                  INZ
004900060503     D  ROUP_G_T                      1
005000060503     D  ROUP_G_C                      4
005100060503     D*
005200060503     D DS_ROUP         DS                  INZ
005300060503     D  ROUP_T                        1
005400060503     D  ROUP_C                        4
005500060508     D*----------------
005600060508     D* DS RIDEFINIZIONE GRUPPI DEPOT
005700060508     D*-------------------
005800060508     D DS_GRPD         DS                  INZ
005900060508     D  GRPD_G                        4
006000060508     D  GRPD_D                        4
006100000000     D*-------------------
006200000000     D* ARCHITETTURA
006300000000     D*-------------------
006400000000     D KPJBA         E DS
006500060426     D*-------------------
006600060427     D*------------------
006700060427     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006800060427     D*------------------
006900060627     D InStringa       S          32678    VARYING                              (stringa input)
007000060427     D InSepara        S             10                                         (separatore)
007100060428     D skSplitCSV      S           4096    DIM(1000) VARYING
007200060428     D lenInSepara     S              2  0 INZ(*zeros)
007300060428     D posDa           S              5  0 INZ(*zeros)
007400060428     D posA            S              5  0 INZ(*zeros)
007500060502     D i               S              4  0 INZ(1)
007600060428     D wSize           S             10I 0 INZ(*zeros)
007700060427     D*-------------------
007800060427     D* VARIABILI DI WRK
007900060427     D*-------------------
008000060502     D wAlfa           S           4096    INZ VARYING
008100060502     D wElencoServizi  S           4096    INZ VARYING
008200060503     D wElencoDepot    S           4096    INZ VARYING
008300060502     D wRiga           S            198    INZ VARYING
008400060502     D skNaz           S              2    DIM(1000)
008500060502     D jNaz            S              4  0 INZ
008600060502     D skDpt           S              4    DIM(200)
008700060502     D jDpt            S              3  0 INZ
008800060503     D skSrv           S              3  0 DIM(100)
008900060503     D jSrv            S              3  0 INZ
009000060503     D skSrvL          S              3    DIM(1000) DESCEND
009100060503     D jSrvL           S              4  0 INZ
009200060503     D skRouL          S              5    DIM(1000) DESCEND
009300060503     D jRouL           S              4  0 INZ
009400060508     D skDptL          S              8    DIM(1000) DESCEND
009500060508     D jDptL           S              4  0 INZ
009600060621     D wCROPTCB        S                   LIKE(CROPTCB) INZ
009700060621     D wCROPTCE        S                   LIKE(CROPTCE) INZ
009800000000     C*------------------------------------------------------------------------*
009900000000     C* MAIN LINE
010000000000     C*------------------------------------------------------------------------*
010100060502     C*
010200060502     C* Carico le tabelle occorrenti
010300060502     C                   EXSR      CARTAB
010400060502     C*
010500060502     C* Carico i depot della versione correntemente in importazione
010600060503     C                   EXSR      CARDPT
010700060427     C*
010800060427     C* Effettuo elaborazione
010900060427     C                   EXSR      EXECRO
011000000000     C*
011100060517     C* Effettuo chiamate *pgm d utilità solo x chiusure finali
011200060517     C                   CLEAR                   XTOEBCDDS
011300060517     C                   EVAL      §XTETLA  = 'C'
011400060517     C                   CALL      'XTOEBCDR'
011500060517     C                   PARM                    XTOEBCDDS
011600060517     C*
011700000000     C                   SETON                                        LR
011800060427     C*------------------------------------------------------------------------*
011900060427     C*  EXECRO - IMPORTA L' ARCHIVIO "ROUTES"
012000060427     C*------------------------------------------------------------------------*
012100060427     C     EXECRO        BEGSR
012200060427     C*
012300060427     C* Scorro tutto il file d wrk
012400060427     C     *START        SETLL     WFDPCROF
012500060427     C                   READ      WFDPCROF
012600060427     C                   DOW       not %eof(WFDPCROF)
012700060427     C                   IF        %subst(%trim(WDCRODTA):1:1) <> '#'
012800060428     C*
012900060428     C* Eseguo "splittamento" dei dati
013000060428     C                   EVAL      InStringa = WDCRODTA
013100060502     C                   EVAL      InSepara  = '|'
013200060428     C                   EXSR      SPLIT
013300060428     C*
013400060428     C* Se nn reperiti elementi listo in stampa
013500060428     C                   IF        i = *zeros
013600060427     C                   EVAL      wRiga = 'WFDPCROF' + ' ' + WDCRODTA
013700060427     C                   EXCEPT    rigaErr
013800060427     C                   ELSE
013900060427     C*
014000060427     C* Imposto il buffer del file d output
014100060427     C                   CLEAR                   DPCRO000
014200060621     C                   CLEAR                   wCROPTCB
014300060621     C                   CLEAR                   wCROPTCE
014400081008     C*
014500060621     C                   SETOFF                                       50
014600060427     C                   EVAL      CROVER  = PARVER
014700060428     C                   EVAL      wAlfa = %trim(skSplitCSV(1))
014800060427     C                   MOVEL     wAlfa         CROISO2
014900090312     C*
015000090312     C* Eseguo forzatura particolare x le nazioni sovrane:
015100090312     C* - GG (Guernsey)
015200090312     C* - JE (Jersey)
015300090312     C* - IM (Isle of Man)
015400090312     C*
015500090312     C* che sono cmq gestire dalle poste inglesi x cui occorre overraidare tali nazioni
015600090312     C* come GB
015700090312     C                   IF        CROISO2 = 'GG'
015800090312     C                   EVAL      CROISO2 = 'GB'
015900090312     C                   ENDIF
016000090312     C                   IF        CROISO2 = 'JE'
016100090312     C                   EVAL      CROISO2 = 'GB'
016200090312     C                   ENDIF
016300090312     C                   IF        CROISO2 = 'IM'
016400090312     C                   EVAL      CROISO2 = 'GB'
016500090312     C                   ENDIF
016600060502     C*
016700060502     C* Importo unicamente i record riferiti a instradamenti su nazioni servite da DPD
016800060502     C     CROISO2       LOOKUP    skNaz                                  55
016900060515     C                   IF        %equal
017000060502     C*
017100060428     C                   EVAL      wAlfa = %trim(skSplitCSV(2))
017200060621     C                   MOVEL     wAlfa         wCROPTCB
017300060428     C                   EVAL      wAlfa = %trim(skSplitCSV(3))
017400060621     C                   MOVEL     wAlfa         wCROPTCE
017500060502     C*
017600060502     C* Se nn specificato l'estremo finale del range do x assunto che sia uguale al estremo
017700060502     C* iniziale in modo da gestire sempre i cap come range
017800060621     C                   IF        wCROPTCE = *blanks
017900060621     C                   EVAL      wCROPTCE = wCROPTCB
018000060502     C                   ENDIF
018100060621     C*
018200060621     C* Eseguo forzatura x Portogallo
018300060621     C                   IF        CROISO2 = 'PT'
018400060629     C                   IF        %len(%trim(wCROPTCB)) = 7
018500060629     C                   EVAL      wCROPTCB = %subst(wCROPTCB:1:4)
018600060621     C                   ELSE
018700060621     C                   SETON                                        50
018800060621     C                   EVAL      wRiga = 'CROPTCB'+' '+CROISO2+' '+wCROPTCB
018900060621     C                   EXCEPT    rigaErr
019000060621     C                   ENDIF
019100060621     C                   ENDIF
019200060621     C*
019300060621     C* Se anomalia skippo tutto il resto e nn scrivo i record relativi
019400060621     C                   IF        *IN50 = *off
019500060621     C*
019600060621     C* Eseguo controlli x Nazioni previste con CAP a 4 byte
019700060621     C                   IF        CROISO2 = 'NL' OR
019800060621     C                             CROISO2 = 'BE' OR
019900060621     C                             CROISO2 = 'AT' OR
020000060621     C                             CROISO2 = 'DK' OR
020100060621     C                             CROISO2 = 'LU' OR
020200060621     C                             CROISO2 = 'LV' OR
020300060621     C                             CROISO2 = 'SI' OR
020400060621     C                             CROISO2 = 'HU' OR
020500060621     C                             CROISO2 = 'AD'
020600060621     C                   IF        %len(%trim(wCROPTCB)) = 4
020700060621     C                   ELSE
020800060621     C                   EVAL      wRiga = 'CROPTCB'+' '+CROISO2+' '+wCROPTCB
020900060621     C                   EXCEPT    rigaErr
021000060621     C                   ENDIF
021100060621     C*
021200060621     C                   IF        %len(%trim(wCROPTCE)) = 4
021300060621     C                   ELSE
021400060621     C                   EVAL      wRiga = 'CROPTCE'+' '+CROISO2+' '+wCROPTCE
021500060621     C                   EXCEPT    rigaErr
021600060621     C                   ENDIF
021700060621     C                   ENDIF
021800060621     C*
021900060621     C* Eseguo controlli x Nazioni previste con CAP a 5 byte
022000060621     C                   IF        CROISO2 = 'CZ' OR
022100060621     C                             CROISO2 = 'DE' OR
022200060621     C                             CROISO2 = 'EE' OR
022300060621     C                             CROISO2 = 'ES' OR
022400060621     C                             CROISO2 = 'FI' OR
022500060621     C                             CROISO2 = 'FR' OR
022600060621     C                             CROISO2 = 'LT' OR
022700060621     C                             CROISO2 = 'PL' OR
022800060621     C                             CROISO2 = 'SE' OR
022900130925     C                             CROISO2 = 'SK' OR
023000130925     C                             CROISO2 = 'HR'
023100060621     C                   IF        %len(%trim(wCROPTCB)) = 5
023200060621     C                   ELSE
023300060621     C                   EVAL      wRiga = 'CROPTCB'+' '+CROISO2+' '+wCROPTCB
023400060621     C                   EXCEPT    rigaErr
023500060621     C                   ENDIF
023600060621     C*
023700060621     C                   IF        %len(%trim(wCROPTCE)) = 5
023800060621     C                   ELSE
023900060621     C                   EVAL      wRiga = 'CROPTCE'+' '+CROISO2+' '+wCROPTCE
024000060621     C                   EXCEPT    rigaErr
024100060621     C                   ENDIF
024200060621     C                   ENDIF
024300060621     C*
024400060621     C* Eseguo riempimento "a 9" del CAP "A"
024500060621     C                   EVAL      CROPTCB = wCROPTCB
024600060621     C                   MOVEL     *all'9'       CROPTCE
024700060621     C                   EVAL      wAlfa = %trim(wCROPTCE)
024800060621     C                   MOVEL     wAlfa         CROPTCE
024900060621     C*
025000060428     C                   EVAL      wAlfa = %trim(skSplitCSV(4))
025100060502     C                   EVAL      wElencoServizi = wAlfa
025200060502     C*
025300060428     C                   EVAL      wAlfa = %trim(skSplitCSV(5))
025400060503     C                   EVAL      wElencoDepot = wAlfa
025500060503     C*
025600060428     C                   EVAL      wAlfa = %trim(skSplitCSV(6))
025700060508     C                   EVAL      CROSNDD = wAlfa
025800060428     C                   EVAL      wAlfa = %trim(skSplitCSV(7))
025900060427     C                   MOVEL     wAlfa         CROOSRT
026000060428     C                   EVAL      wAlfa = %trim(skSplitCSV(8))
026100060427     C                   MOVEL     wAlfa         CRODDEP
026200060428     C                   EVAL      wAlfa = %trim(skSplitCSV(9))
026300060427     C                   MOVEL     wAlfa         CROPTYG
026400060428     C                   EVAL      wAlfa = %trim(skSplitCSV(10))
026500060427     C                   MOVE      wAlfa         CRODSRT
026600060428     C                   EVAL      wAlfa = %trim(skSplitCSV(11))
026700060427     C                   MOVE      wAlfa         CROBID
026800060516     C*
026900060517     C* Effettuo forzatura fino a diversa indicazione da DPD GeoPost
027000060517     C                   IF        CROBID = 037
027100060517     C                   EVAL      CROBIDP = '%'
027200060517     C                   ELSE
027300060517     C                   IF        CROBID = *zeros
027400060517     C                   EVAL      CROBIDP = *blanks
027500060517     C                   ELSE
027600060517     C* Se carattere nn "consueto" eseguo la conversione da ASCII Decimal a EBCDIC Char
027700060517     C                   CLEAR                   XTOEBCDDS
027800060517     C                   EVAL      §XTETLA  = 'E'
027900060517     C                   MOVE(P)   CROBID        §XTEADEC
028000060517     C                   CALL      'XTOEBCDR'
028100060517     C                   PARM                    XTOEBCDDS
028200060517     C                   IF        §XTEERR <> *blanks
028300060517     C                   EVAL      wRiga = 'WFDPCROF' + ' BID = ' +
028400060517     C                                     %trim(%editw(CROBID:'0   '))
028500060517     C                   EXCEPT    rigaErr
028600060517     C                   ELSE
028700060517     C                   EVAL      CROBIDP = §XTEACHR
028800060517     C                   ENDIF
028900060516     C                   ENDIF
029000060517     C                   ENDIF
029100060516     C*
029200060427     C                   EVAL      CROATB  = *blanks
029300081009     C*
029400081009     C* Eseguo sub-split del campo elenco eccezioni servizi x poter così verticalizzare gli
029500081009     C* instradamenti relativi
029600081009     C                   EXSR      VERTSRV
029700081009     C*
029800081009     C* Proseguo solamente se il record corrente è riferito a servizi da noi gestiti
029900081009     C                   IF        *IN30 = *OFF
030000060503     C*
030100060503     C* Eseguo sub-split del campo elenco eccezioni depot i x poter così verticalizzare gli
030200060503     C* instradamenti relativi
030300060508     C                   EXSR      VERTROU
030400060503     C*
030500060503     C* Ordino in maniera discendente le schiere con la lista delle eccezzioni x servizio
030600060503     C* o depot partenza
030700060503     C                   SORTA     skSrvL
030800060508     C                   SORTA     skRouL
030900060503     C*
031000060503     C* A questo punto effettuo la combinazione lineare delle 2 schiere eccezioni x servizi e
031100060503     C* eccezioni x depot partenza x verticalizzare tutte le combinate risultanti
031200060503     C                   Z-ADD     1             jSrvL
031300060503     C* ..ciclo sui servizi
031400060503     C                   DOW       jSrvL <= %elem(skSrvL)
031500060503     C                   IF        skSrvL(jSrvL) = *blanks
031600060503     C                   LEAVE
031700060503     C                   ELSE
031800060503     C                   Z-ADD     1             jRouL
031900060503     C* ..ciclo sui depot partenza
032000060503     C                   DOW       jRouL <= %elem(skRouL)
032100060503     C                   IF        skRouL(jRouL) = *blanks
032200060503     C                   LEAVE
032300060503     C                   ELSE
032400060503     C                   MOVE(P)   skSrvL(jSrvL) CROSRVC
032500060503     C                   EVAL      DS_ROUP  = skRouL(jRouL)
032600060503     C                   EVAL      CROROUPT = ROUP_T
032700060503     C                   EVAL      CROROUP  = ROUP_C
032800060530     C* Verifico nn già presenza record corrente
032900060530     C     KEYcro01_C    SETLL     DPCRO01L                               54
033000060530     C                   IF        not %equal(DPCRO01L)
033100060529     C                   WRITE(e)  DPCRO000
033200060530     C                   ENDIF
033300060503     C                   ENDIF
033400060503     C                   ADD       1             jRouL
033500060503     C                   ENDDO
033600060503     C*
033700060503     C                   ENDIF
033800060503     C                   ADD       1             jSrvL
033900060503     C                   ENDDO
034000060621     C*
034100060621     C* Presente anomalia formato CAP
034200060621     C                   ENDIF
034300081009     C*
034400081009     C* Servizio nn gestito da Bartolini
034500081009     C                   ENDIF
034600060502     C*
034700060502     C* Nazione nn servita da DPD
034800060502     C                   ENDIF
034900060502     C*
035000060502     C* Record vuoto
035100060427     C                   ENDIF
035200060502     C*
035300060502     C* Record di intestazione (1° byte = #)
035400060427     C                   ENDIF
035500060427     C                   READ      WFDPCROF
035600060427     C                   ENDDO
035700060427     C*
035800060427     C                   ENDSR
035900060503     C*------------------------------------------------------------------------*
036000060503     C* VERTSRV - Routine di verticalizzazione eccezioni x servizi DPD
036100060503     C*------------------------------------------------------------------------*
036200060503     C     VERTSRV       BEGSR
036300081008     C*
036400081009     C                   SETOFF                                       303132
036500060503     C*
036600060503     C* Inizializzo schiera e indice
036700060503     C                   Z-ADD     *zeros        jSrvL
036800060503     C                   CLEAR                   skSrvL
036900060503     C*
037000060503     C* Se presenti eccezioni x servizi
037100060503     C                   IF        wElencoServizi <> *blanks
037200081009     C                   SETON                                        31
037300060503     C*
037400060503     C* Eseguo "splittamento" dei dati
037500060503     C                   EVAL      InStringa = wElencoServizi
037600060503     C                   EVAL      InSepara  = ','
037700060503     C                   EXSR      SPLIT
037800060503     C*
037900060503     C* Se nn reperiti elementi listo in stampa
038000060503     C                   IF        i = *zeros
038100060503     C                   EVAL      wRiga = 'WFDPCROF' + ' ' + wElencoServizi
038200060503     C                   EXCEPT    rigaErr
038300060503     C                   ELSE
038400060503     C                   Z-ADD     1             jSk               4 0
038500060503     C                   DOW       jSk <= i
038600060503     C                   EVAL      DS_SRVC_A = %trim(skSplitCSV(jSk))
038700060503     C*
038800060503     C* Se nn specificato l'estremo finale del range do x assunto che sia uguale al estremo
038900060503     C* iniziale in modo da gestire sempre i servizi come range
039000060503     C                   IF        SRVC_E_A > '000' AND SRVC_E_A <= '999'
039100060503     C                   ELSE
039200060503     C                   EVAL      SRVC_E_A = SRVC_B_A
039300060503     C                   ENDIF
039400060503     C*
039500060503     C                   EVAL      DS_SRVC = DS_SRVC_A
039600060503     C*
039700060503     C* Verticalizzo tutti i servizi indicati anche come range
039800060503     C                   DOW       SRVC_B <= SRVC_E
039900060503     C*
040000060503     C* Importo unicamente i record riferiti a servizi DPD da noi gestiti
040100060503     C     SRVC_B        LOOKUP    skSrv                                  55
040200060515     C                   IF        %equal
040300081009     C                   SETON                                        32
040400060530     C                   ADD       1             jSrvL
040500060503     C                   MOVE(P)   SRVC_B        skSrvL(jSrvL)
040600060503     C                   ENDIF
040700060503     C                   ADD       1             SRVC_B
040800060503     C                   ENDDO
040900060503     C*
041000060503     C                   ADD       1             jSk
041100060503     C                   ENDDO
041200060503     C*
041300060503     C                   ENDIF
041400060530     C***                ENDIF
041500060503     C*
041600060503     C* Se nn presenti eccezioni x servizi
041700060530     C***                IF        jSrvL = *zeros
041800060530     C                   ELSE
041900060503     C                   ADD       1             jSrvL
042000060503     C                   MOVE(P)   *all'0'       skSrvL(jSrvL)
042100060503     C                   ENDIF
042200081009     C*
042300081009     C* Verifico presenza di eccezioni x servizi gestiti da Bartolini
042400081009     C                   IF        *IN31 = *ON  AND
042500081009     C                             *IN32 = *OFF
042600081009     C                   SETON                                        30
042700081009     C                   ENDIF
042800060503     C*
042900060503     C                   ENDSR
043000060503     C*------------------------------------------------------------------------*
043100060503     C* VERTROU - Routine di verticalizzazione eccezioni x depot partenza
043200060503     C*------------------------------------------------------------------------*
043300060503     C     VERTROU       BEGSR
043400060503     C*
043500060503     C* Inizializzo schiera e indice
043600060503     C                   Z-ADD     *zeros        jRouL
043700060503     C                   CLEAR                   skRouL
043800060503     C*
043900060503     C* Se presenti eccezioni x depot di partenza (solo Italia)
044000060503     C                   IF        wElencoDepot <> *blanks
044100060503     C*
044200060503     C* Eseguo "splittamento" dei dati
044300060503     C                   EVAL      InStringa = wElencoDepot
044400060503     C                   EVAL      InSepara  = ','
044500060503     C                   EXSR      SPLIT
044600060503     C*
044700060503     C* Se nn reperiti elementi listo in stampa
044800060503     C                   IF        i = *zeros
044900060503     C                   EVAL      wRiga = 'WFDPCROF' + ' ' + wElencoDepot
045000060503     C                   EXCEPT    rigaErr
045100060503     C                   ELSE
045200060503     C                   Z-ADD     1             jSk               4 0
045300060503     C                   DOW       jSk <= i
045400060503     C*
045500060503     C* Gestisco indicazione x intera nazione (C=Country)
045600060503     C                   IF        %subst(skSplitCSV(jSk):1:1) = 'C'
045700060503     C                   EVAL      DS_ROUP_C = skSplitCSV(jSk)
045800060503     C                   IF        ROUP_C_C = 'IT'
045900060503     C                   DOW       jRouL <= %elem(skDpt)
046000060530     C                   IF        skDpt(jRouL+1) <> *blanks
046100060530     C                   ADD       1             jRouL
046200060503     C                   EVAL      ROUP_T = ROUP_C_T
046300060503     C                   EVAL      ROUP_C = skDpt(jRouL)
046400060503     C                   EVAL      skRouL(jRouL) = DS_ROUP
046500060503     C                   ELSE
046600060503     C                   LEAVE
046700060503     C                   ENDIF
046800060503     C                   ENDDO
046900060503     C                   ENDIF
047000060503     C                   ENDIF
047100060503     C*
047200060503     C* Gestisco indicazione x depot (D=Depot)
047300060503     C                   IF        %subst(skSplitCSV(jSk):1:1) = 'D'
047400060503     C                   EVAL      DS_ROUP_D = skSplitCSV(jSk)
047500060503     C                   IF        ROUP_D_E > '0000' AND ROUP_D_E <= '9999'
047600060503     C                   ELSE
047700060503     C                   EVAL      ROUP_D_E = ROUP_D_B
047800060503     C                   ENDIF
047900060503     C*
048000060503     C                   MOVE(P)   ROUP_D_B      wROUP_B           4 0
048100060503     C                   MOVE(P)   ROUP_D_E      wROUP_E           4 0
048200060503     C                   MOVEL(P)  *blanks       wROUP             4
048300060503     C                   DOW       wROUP_B <= wROUP_E
048400060503     C                   MOVE(P)   wROUP_B       wROUP
048500060503     C     wROUP         LOOKUP    skDpt                                  55
048600060515     C                   IF        %equal
048700060503     C                   ADD       1             jRouL
048800060503     C                   EVAL      ROUP_T = ROUP_D_T
048900060503     C                   EVAL      ROUP_C = wROUP
049000060503     C                   EVAL      skRouL(jRouL) = DS_ROUP
049100060503     C                   ENDIF
049200060503     C                   ADD       1             wROUP_B
049300060503     C                   ENDDO
049400060503     C                   ENDIF
049500060503     C*
049600060508     C* Gestisco indicazione x gruppo depot (D=Depot)
049700060503     C                   IF        %subst(skSplitCSV(jSk):1:1) = 'G'
049800060503     C                   EVAL      DS_ROUP_G = skSplitCSV(jSk)
049900060508     C                   SORTA     skDptL
050000060508     C                   Z-ADD     1             jDptL
050100060508     C                   DOW       jDptL <= %elem(skDptL)
050200060508     C                   IF        skDptL(jDptL) = *blanks
050300060508     C                   LEAVE
050400060508     C                   ELSE
050500060508     C                   EVAL      DS_GRPD = skDptL(jDptL)
050600060508     C                   IF        GRPD_G = ROUP_G_C
050700060508     C                   ADD       1             jRouL
050800060508     C                   EVAL      ROUP_T = 'G'
050900060508     C                   EVAL      ROUP_C = GRPD_D
051000060508     C                   EVAL      skRouL(jRouL) = DS_ROUP
051100060508     C                   ENDIF
051200060508     C                   ENDIF
051300060508     C                   ADD       1             jDptL
051400060508     C                   ENDDO
051500060508     C                   ENDIF
051600060503     C*
051700060503     C                   ADD       1             jSk
051800060503     C                   ENDDO
051900060503     C*
052000060503     C                   ENDIF
052100060530     C***                ENDIF
052200060503     C*
052300060503     C* Se nn presenti eccezioni x depot di partenza (solo Italia)
052400060530     C***                IF        jRouL = *zeros
052500060530     C                   ELSE
052600060503     C                   ADD       1             jRouL
052700060503     C                   EVAL      ROUP_T = *blanks
052800060503     C                   EVAL      ROUP_C = *all'0'
052900060503     C                   EVAL      skRouL(jRouL) = DS_ROUP
053000060503     C                   ENDIF
053100060503     C*
053200060503     C                   ENDSR
053300060428     C*------------------------------------------------------------------------*
053400060428     C* SPLIT - Routine di reperimento PORTO e VARIE fatturate
053500060428     C*------------------------------------------------------------------------*
053600060428     C     SPLIT         BEGSR
053700060428     C*
053800060428     C* Innanzitutto inizializzo i parametri d OUT
053900060428     C                   RESET                   lenInSepara
054000060428     C                   RESET                   posDa
054100060428     C                   RESET                   posA
054200060428     C                   RESET                   i
054300060428     C                   RESET                   wSize
054400060502     C                   CLEAR                   SkSplitCSV
054500060428     C*
054600060428     C* Determino subito la lunghezza del "separatore"
054700060428     C                   EVAL      lenInSepara = %len(%trim(InSepara))
054800060428     C**********
054900060428     C* Effettuo lo split della stringa dati d input
055000060428     C**********
055100060428     C*
055200060428     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
055300060428     C                   DOW       posDa <= %len(%trim(InStringa))
055400060428     C*
055500060428     C* Gestisco il 1° elemento
055600060428     C                   IF        i = 1
055700060428     C*
055800060428     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemen
055900060428     C                   EVAL      posDa = 1
056000060428     C*
056100060428     C* Eseguo lo scan x trovare la fine del primo campo
056200060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
056300060428     C                                     posDa)
056400060428     C*
056500060428     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
056600060428     C* l'intero contenuto della stringa è l'unico elemento presente
056700060428     C                   IF        posA = *zeros
056800060428     C                   CLEAR                   SkSplitCSV
056900060428     C                   EVAL      SkSplitCSV(i) = %trim(InStringa)
057000060428     C                   LEAVE
057100060428     C                   ELSE
057200060428     C*
057300060428     C* A questo punto "estraggo" l'elemento corrente
057400060428     C                   IF        posA = 1
057500060428     C                   EVAL      SkSplitCSV(i) = *blanks
057600060428     C                   ELSE
057700060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
057800060428     C                                                    (posA-posDa))
057900060428     C                   ENDIF
058000060428     C                   ENDIF
058100060428     C*
058200060428     C* X gli elementi successivi al 1°
058300060428     C                   ELSE
058400060428     C*
058500060428     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
058600060428     C                   EVAL      posDa = posA + lenInSepara
058700060428     C*
058800060428     C* Verifico che nn vi sia il campo nn valorizzato
058900060428     C                   IF        %subst(InStringa:posDa:lenInSepara) =
059000060428     C                             %trim(InSepara)
059100060428     C*
059200060428     C* Se elemento nn valorizzato skippo al prossimo
059300060428     C                   EVAL      SkSplitCSV(i) = *blanks
059400060428     C                   EVAL      posA  = posA + 1
059500060428     C                   ELSE
059600060428     C*
059700060428     C* Eseguo lo scan x trovare la fine dell'elemento corrente
059800060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
059900060428     C                                     posDa+1)
060000060428     C*
060100060428     C* Gestisco l'ultimo campo
060200060428     C                   IF        posA = *zeros
060300060428     C                   EVAL      posA = %len(%trim(InStringa)) + 1
060400060428     C*
060500060428     C* A questo punto "estraggo" l'elemento corrente
060600060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
060700060428     C                                                    (posA-posDa))
060800060428     C                   LEAVE
060900060428     C                   ELSE
061000060428     C*
061100060428     C* A questo punto "estraggo" l'elemento corrente
061200060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
061300060428     C                                                    (posA-posDa))
061400060428     C                   ENDIF
061500060428     C                   ENDIF
061600060428     C                   ENDIF
061700060428     C*
061800060428     C* Incremento il contatore d campo
061900060428     C                   EVAL      i = i +1
062000060428     C                   ENDDO
062100060428     C*
062200060428     C                   ENDSR
062300060502     C*------------------------------------------------------------------------*
062400060502     C* CARTAB - CARICAMENTO DATI TABELLATI
062500060502     C*------------------------------------------------------------------------*
062600060502     C     cartab        BEGSR
062700060502     C*
062800060502     C* Reperisco tutte le nazioni (ISO2) servite da DPD
062900060502     C                   Z-ADD     *zeros        jNaz
063000060503     C                   CLEAR                   skNaz
063100060502     C                   CLEAR                   DS15
063200060502     C                   EVAL      tblKUT = 1
063300060502     C                   EVAL      tblCOD = '15'
063400060502     C     KEYtabel_P    SETLL     TABEL00F
063500060502     C                   IF        %found(TABEL00F)
063600060502     C     KEYtabel_P    READE     TABEL00F
063700060502     C                   DOW       not %eof(TABEL00F)
063800060502     C                   IF        tblFLG = *blanks
063900060502     C                   EVAL      DS15 = tblUNI
064000060921     C                   IF        §15LAD > *zeros OR
064100060921     C                             §15RED = 'S'
064200060502     C                   ADD       1             jNaz
064300060502     C                   MOVEL(P)  §15COD        skNaz(jNaz)
064400060502     C                   ENDIF
064500060502     C                   ENDIF
064600060502     C     KEYtabel_P    READE     TABEL00F
064700060502     C                   ENDDO
064800060502     C                   ENDIF
064900060503     C*
065000060503     C* Reperisco tutti i servizi DPD da noi gestiti
065100060503     C                   Z-ADD     *zeros        jSrv
065200060503     C                   CLEAR                   skSrv
065300060503     C                   EVAL      tbeCOD = 'SRV'
065400060503     C     KEYtbe01_P    SETLL     TNTBE01L
065500060503     C                   IF        %found(TNTBE01L)
065600060503     C     KEYtbe01_P    READE     TNTBE01L
065700060503     C                   DOW       not %eof(TNTBE01L)
065800060503     C                   IF        tbeATB = *blanks
065900060503     C                   ADD       1             jSrv
066000060503     C                   MOVEL(P)  tbeKE1        skSrv(jSrv)
066100060503     C                   ENDIF
066200060503     C     KEYtbe01_P    READE     TNTBE01L
066300060503     C                   ENDDO
066400060503     C                   ENDIF
066500060502     C*
066600060502     C                   ENDSR
066700060502     C*------------------------------------------------------------------------*
066800060502     C* CARDPT - CARICAMENTO ANAGRAFICA DEPOT DPD
066900060502     C*------------------------------------------------------------------------*
067000060502     C     cardpt        BEGSR
067100060502     C*
067200060502     C* Carico solo i depot italia (quindi i ns P.O. che servono DPD)
067300060502     C                   Z-ADD     *zeros        jDpt
067400060508     C                   Z-ADD     *zeros        jDptL
067500060508     C                   CLEAR                   skDpt
067600060508     C                   CLEAR                   skDptL
067700060502     C                   EVAL      cdpVER  = PARVER
067800060502     C                   EVAL      cdpISO2 = 'IT'
067900060502     C     KEYcdp02_P    SETLL     DPCDP02L
068000060502     C                   IF        %found(DPCDP02L)
068100060502     C     KEYcdp02_P    READE     DPCDP02L
068200060502     C                   DOW       not %eof(DPCDP02L)
068300060502     C                   IF        cdpATB = *blanks
068400060502     C                   ADD       1             jDpt
068500060502     C                   MOVEL(P)  cdpDPC        skDpt(jDpt)
068600060508     C*
068700060508     C* Verifico il raggruppamento del depot
068800060508     C                   IF        cdpGID <> *blanks
068900060508     C*
069000060508     C* Eseguo "splittamento" dei dati
069100060508     C                   EVAL      InStringa = cdpGID
069200060508     C                   EVAL      InSepara  = ','
069300060508     C                   EXSR      SPLIT
069400060508     C*
069500060508     C* Se nn reperiti elementi listo in stampa
069600060508     C                   IF        i = *zeros
069700060508     C                   EVAL      wRiga = 'WFDPCROF' + ' ' + cdpGID
069800060508     C                   EXCEPT    rigaErr
069900060508     C                   ELSE
070000060508     C                   Z-ADD     1             jSk               4 0
070100060508     C                   DOW       jSk <= i
070200060508     C                   ADD       1             jDptL
070300060508     C                   CLEAR                   DS_GRPD
070400060508     C                   EVAL      GRPD_G = skSplitCSV(jSk)
070500060508     C                   EVAL      GRPD_D = cdpDPC
070600060508     C                   EVAL      skDptL(jDptL) = DS_GRPD
070700060508     C                   ADD       1             jSk
070800060508     C                   ENDDO
070900060508     C                   ENDIF
071000060508     C                   ENDIF
071100060508     C*
071200060502     C                   ENDIF
071300060502     C     KEYcdp02_P    READE     DPCDP02L
071400060502     C                   ENDDO
071500060502     C                   ENDIF
071600060502     C*
071700060502     C                   ENDSR
071800000000     C*------------------------------------------------------------------------*
071900980403     C* *INZSR - OPERAZIONI INIZIALI
072000000000     C*------------------------------------------------------------------------*
072100980403     C     *INZSR        BEGSR
072200000000     C*
072300000000     C* RICEVIMENTO PARAMETRI
072400000000     C     *ENTRY        PLIST
072500000000     C                   PARM                    KPJBA
072600060427     C                   MOVEL     KPJBU         PARAM
072700060502     C*
072800060502     C* Definizione chiavi di lettura
072900060502     C*
073000060502     C* TABEL00F - Parziale
073100060502     C     KEYtabel_P    KLIST
073200060502     C                   KFLD                    tblKUT
073300060502     C                   KFLD                    tblCOD
073400060503     C*
073500060503     C* TNTBE01L - Parziale
073600060503     C     KEYtbe01_P    KLIST
073700060503     C                   KFLD                    tbeCOD
073800060502     C*
073900060502     C* DPCDP02L - Parziale
074000060502     C     KEYcdp02_P    KLIST
074100060502     C                   KFLD                    cdpVER
074200060502     C                   KFLD                    cdpISO2
074300060530     C*
074400060530     C* DPCRO01L - Completa
074500060530     C     KEYcro01_C    KLIST
074600060530     C                   KFLD                    croVER
074700060530     C                   KFLD                    croISO2
074800060608     C                   KFLD                    croSRVC
074900060608     C                   KFLD                    croROUP
075000060530     C                   KFLD                    croPTCB
075100060530     C                   KFLD                    croPTCE
075200000000     C*
075300000000     C                   ENDSR
075400060427     C*------------------------------------------------------------------------*
075500060427     OPRTF198   E            rigaErr     1
075600060427     O                       wRiga
