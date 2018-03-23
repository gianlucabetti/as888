000100000000     H*------------------------------------------------------------------------*
000200000104     H* IMPORTAZIONE CAPPARIO DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500060427     F*------------------------------------------------------------------------*
000600060427     F* DATA BASE
000700060427     F*------------------------------------------------------------------------*
000800060530     FDPCDY01L  UF A E           K DISK
000900060530     FWFDPCDYF  IF   E           K DISK
001000060529     FDPCDP02L  IF   E           K DISK
001100060529     FTNTBE01L  IF   E           K DISK
001200060427     FPRTF198   O    F  198        PRINTER
001300060427     D*----------------
001400060427     D* DS PARAMETRI DI LANCIO PGM
001500060427     D*-------------------
001600060427     D PARAM           DS                  INZ
001700060427     D  PARVER                        5  0
001800060529     D*----------------
001900060529     D* DS ESTERNE
002000060529     D*-------------------
002100060529     D DS15          E DS                  INZ
002200060529     D*----------------
002300060529     D* DS RIDEFINIZIONE SERVIZI
002400060529     D*-------------------
002500060529     D DS_SRVC_A       DS                  INZ
002600060529     D  SRVC_S_A                      1
002700060529     D  SRVC_B_A                      3
002800060529     D  SRVC_E_A                      3
002900060529     D*
003000060529     D DS_SRVC         DS                  INZ
003100060529     D  SRVC_S                        1
003200060529     D  SRVC_B                        3  0
003300060529     D  SRVC_E                        3  0
003400060529     D*----------------
003500060529     D* DS RIDEFINIZIONE DEPOT PARTENZA (ROUTE PLACES)
003600060529     D*-------------------
003700060529     D DS_ROUP_C       DS                  INZ
003800060529     D  ROUP_C_T                      1
003900060529     D  ROUP_C_C                      2
004000060529     D*
004100060529     D DS_ROUP_D       DS                  INZ
004200060529     D  ROUP_D_T                      1
004300060529     D  ROUP_D_B                      4
004400060529     D  ROUP_D_E                      4
004500060529     D*
004600060529     D DS_ROUP_G       DS                  INZ
004700060529     D  ROUP_G_T                      1
004800060529     D  ROUP_G_C                      4
004900060529     D*
005000060529     D DS_ROUP         DS                  INZ
005100060529     D  ROUP_T                        1
005200060529     D  ROUP_C                        4
005300060529     D*----------------
005400060529     D* DS RIDEFINIZIONE GRUPPI DEPOT
005500060529     D*-------------------
005600060529     D DS_GRPD         DS                  INZ
005700060529     D  GRPD_G                        4
005800060529     D  GRPD_D                        4
005900060530     D*----------------
006000060530     D* DS RIDEFINIZIONE RANGE CAP
006100060530     D*-------------------
006200060530     D DS_PTC          DS                  INZ
006300060530     D  PTC_T                         1
006400060530     D  PTC_ISO2                      2
006500060530     D  PTC_PTCB                      9
006600060530     D  PTC_PTCE                      9
006700000000     D*-------------------
006800000000     D* ARCHITETTURA
006900000000     D*-------------------
007000000000     D KPJBA         E DS
007100060426     D*-------------------
007200060427     D*------------------
007300060427     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
007400060427     D*------------------
007500060627     D InStringa       S          32768    VARYING                              (stringa input)
007600060427     D InSepara        S             10                                         (separatore)
007700060628     D skSplitCSV      S          32768    DIM(200)  VARYING
007800060628     D skSplitCSVs     S          35768    DIM(200)  VARYING
007900060628     D skSpli2CSV      S            256    DIM(5000) VARYING
008000060628     D skSpli2CSVs     S            256    DIM(5000) VARYING
008100060428     D lenInSepara     S              2  0 INZ(*zeros)
008200060428     D posDa           S              5  0 INZ(*zeros)
008300060428     D posA            S              5  0 INZ(*zeros)
008400060428     D i               s              4  0 INZ(1)
008500060529     D is              s              4  0 INZ(1)
008600060428     D wSize           S             10I 0 INZ(*zeros)
008700060427     D*-------------------
008800060427     D* VARIABILI DI WRK
008900060427     D*-------------------
009000060529     D wNaz            s                   LIKE(cdpISO2)
009100060529     D wCharCSV        s              8    INZ('|')
009200060627     D wAlfa           s          32768    INZ varying
009300060529     D wRiga           s            198    INZ
009400060627     D wElencoServizi  S          32768    INZ VARYING
009500060627     D wElencoRuleFr   S          32768    INZ VARYING
009600060627     D wElencoRuleTo   S          32768    INZ VARYING
009700060627     D skDptI          S              4    DIM(200)
009800060529     D jDptI           S              3  0 INZ
009900060529     D skDpt           S              4    DIM(1000)
010000060529     D jDpt            S              4  0 INZ
010100060529     D skSrv           S              3  0 DIM(100)
010200060529     D jSrv            S              3  0 INZ
010300060627     D skSrvL          S              3    DIM(2000) DESCEND
010400060529     D jSrvL           S              4  0 INZ
010500060627     D skRulF          S              5    DIM(2000) DESCEND
010600060529     D jRulF           S              4  0 INZ
010700060627     D skRulT          S              5    DIM(2000) DESCEND
010800060529     D jRulT           S              4  0 INZ
010900060627     D skCapT          S                   LIKE(DS_PTC) DIM(2000) DESCEND
011000060530     D jCapT           S              4  0 INZ
011100060627     D skDptLI         S              8    DIM(2000) DESCEND
011200060529     D jDptLI          S              4  0 INZ
011300060627     D skDptL          S              8    DIM(2000) DESCEND
011400060529     D jDptL           S              4  0 INZ
011500060622     D wPTCE           S              9    INZ VARYING
011600000000     C*------------------------------------------------------------------------*
011700000000     C* MAIN LINE
011800000000     C*------------------------------------------------------------------------*
011900060529     C*
012000060529     C* Carico le tabelle occorrenti
012100060529     C                   EXSR      CARTAB
012200060529     C*
012300060529     C* Carico i depot Italia della versione correntemente in importazione
012400060529     C                   EXSR      CARDPTI
012500060427     C*
012600060427     C* Effettuo elaborazione
012700060530     C                   EXSR      EXECDY
012800000000     C*
012900000000     C                   SETON                                        LR
013000060427     C*------------------------------------------------------------------------*
013100060530     C*  EXECDY - IMPORTA L' ARCHIVIO "DENY"
013200060427     C*------------------------------------------------------------------------*
013300060530     C     EXECDY        BEGSR
013400060427     C*
013500060427     C* Scorro tutto il file d wrk
013600060530     C     *START        SETLL     WFDPCDYF
013700060530     C                   READ      WFDPCDYF
013800060530     C                   DOW       not %eof(WFDPCDYF)
013900060530     C                   IF        %subst(%trim(WDCDYDTA):1:1) <> '#'
014000060428     C*
014100060428     C* Eseguo "splittamento" dei dati
014200060530     C                   EVAL      InStringa = WDCDYDTA
014300060428     C                   EVAL      InSepara  = wCharCSV
014400060628     C                   EXSR      SPLIT
014500060428     C*
014600060428     C* Se nn reperiti elementi listo in stampa
014700060428     C                   IF        i = *zeros
014800060530     C                   EVAL      wRiga = 'WFDPCDYF' + ' ' + WDCDYDTA
014900060427     C                   EXCEPT    rigaErr
015000060427     C                   ELSE
015100060427     C*
015200060427     C* Imposto il buffer del file d output
015300060530     C                   CLEAR                   DPCDY000
015400060530     C                   EVAL      CDYVER  = PARVER
015500060529     C*
015600060530     C                   EVAL      wAlfa = %trim(skSplitCSV(1))
015700060530     C                   MOVE      wAlfa         CDYID
015800060530     C                   EVAL      CDYATB  = *blanks
015900060529     C*
016000060530     C                   EVAL      wAlfa = %trim(skSplitCSV(2))
016100060529     C                   EVAL      wElencoRuleFr = wAlfa
016200060529     C*
016300060530     C                   EVAL      wAlfa = %trim(skSplitCSV(3))
016400060529     C                   EVAL      wElencoServizi = wAlfa
016500060529     C*
016600060530     C                   EVAL      wAlfa = %trim(skSplitCSV(4))
016700060529     C                   EVAL      wElencoRuleTo = wAlfa
016800081008     C*
016900081008     C* Eseguo sub-split del campo elenco eccezioni servizi x poter così verticalizzare gli
017000081008     C* instradamenti relativi
017100081008     C                   EXSR      VERTSRV
017200081008     C*
017300081008     C* Proseguo solamente se il record corrente è riferito a servizi da noi gestiti
017400081008     C                   IF        *IN30 = *OFF
017500060529     C*
017600060529     C* Eseguo sub-split del campo elenco eccezioni depot i x poter così verticalizzare gli
017700060529     C* instradamenti relativi
017800060529     C                   EXSR      VERTRULF
017900060529     C*
018000060529     C* Eseguo sub-split del campo elenco eccezioni depot i x poter così verticalizzare gli
018100060529     C* instradamenti relativi
018200060529     C                   EXSR      VERTRULT
018300060529     C*
018400060529     C* Ordino in maniera discendente le schiere con la lista delle eccezzioni x servizio
018500060529     C* o depot partenza
018600081008     C                   SORTA     skSrvL
018700060529     C                   SORTA     skRulF
018800060529     C                   SORTA     skRulT
018900060530     C                   SORTA     skCapT
019000060529     C*
019100060529     C* A questo punto effettuo la combinazione lineare delle 3 schiere eccezioni x eccezioni depot
019200060529     C* partenza/servizi/eccezioni arrivi x verticalizzare tutte le combinate risultanti
019300060529     C                   Z-ADD     1             jSrvL
019400060529     C* ..ciclo sui servizi
019500060529     C                   DOW       jSrvL <= %elem(skSrvL)
019600060529     C                   IF        skSrvL(jSrvL) = *blanks
019700060529     C                   LEAVE
019800060529     C                   ELSE
019900060529     C                   Z-ADD     1             jRulF
020000060529     C* ..ciclo sui depot partenza
020100060529     C                   DOW       jRulF <= %elem(skRulF)
020200060529     C                   IF        skRulF(jRulF) = *blanks
020300060529     C                   LEAVE
020400060529     C                   ELSE
020500060529     C                   Z-ADD     1             jRulT
020600060530     C* ..ciclo sui depot arrivo
020700060529     C                   DOW       jRulT <= %elem(skRulT)
020800060529     C                   IF        skRulT(jRulT) = *blanks
020900060529     C                   LEAVE
021000060529     C                   ELSE
021100060530     C                   Z-ADD     1             jCapT
021200060530     C* ..ciclo sul range cap arrivo
021300060530     C                   DOW       jCapT <= %elem(skCapT)
021400060530     C                   IF        skCapT(jCapT) = *blanks
021500060530     C                   LEAVE
021600060530     C                   ELSE
021700060530     C                   MOVE(P)   skSrvL(jSrvL) CDYSRVC
021800060529     C                   EVAL      DS_ROUP  = skRulF(jRulF)
021900060530     C                   EVAL      CDYRULFT = ROUP_T
022000060530     C                   EVAL      CDYRULF  = ROUP_C
022100060529     C                   EVAL      DS_ROUP  = skRulT(jRulT)
022200060530     C                   EVAL      CDYRULTT = ROUP_T
022300060530     C                   EVAL      CDYRULT  = ROUP_C
022400060628     C                   EVAL      DS_PTC   = skCapT(jCapT)
022500060530     C                   IF        CDYRULTT = *blanks
022600060530     C                   EVAL      CDYRULTT = PTC_T
022700060530     C                   ENDIF
022800060530     C                   EVAL      CDYISO2  = PTC_ISO2
022900060530     C                   EVAL      CDYPTCB  = PTC_PTCB
023000060530     C                   EVAL      CDYPTCE  = PTC_PTCE
023100060622     C* Se presenti divieti => scrivo il record
023200060530     C                   IF        CDYRULF <> *blanks OR
023300060530     C                             CDYSRVC <> *zeros  OR
023400060530     C                             CDYRULT <> *blanks OR
023500060530     C                             CDYISO2 <> *blanks
023600060622     C*
023700060622     C* Eseguo forzatura x Portogallo
023800060622     C                   SETOFF                                       50
023900060622     C                   IF        CDYISO2 = 'PT'
024000060629     C                   IF        %len(%trim(CDYPTCB)) = 7
024100060629     C                   EVAL      CDYPTCB = %subst(CDYPTCB:1:4)
024200060622     C                   ELSE
024300060622     C                   SETON                                        50
024400060622     C                   EVAL      wRiga = 'CDYPTCB'+' '+CDYISO2+' '+CDYPTCB
024500060622     C                   EXCEPT    rigaErr
024600060622     C                   ENDIF
024700060622     C                   ENDIF
024800060622     C*
024900060622     C  N50              WRITE(e)  DPCDY000
025000060529     C                   ENDIF
025100060530     C*
025200060530     C                   ENDIF
025300060530     C                   ADD       1             jCapT
025400060530     C                   ENDDO
025500060529     C*
025600060529     C                   ENDIF
025700060529     C                   ADD       1             jRulT
025800060529     C                   ENDDO
025900060529     C*
026000060529     C                   ENDIF
026100060529     C                   ADD       1             jRulF
026200060529     C                   ENDDO
026300060529     C*
026400060529     C                   ENDIF
026500060529     C                   ADD       1             jSrvL
026600060529     C                   ENDDO
026700060529     C*
026800081008     C                   ENDIF
026900081008     C*
027000060427     C                   ENDIF
027100060427     C                   ENDIF
027200060530     C                   READ      WFDPCDYF
027300060427     C                   ENDDO
027400060427     C*
027500060427     C                   ENDSR
027600060529     C*------------------------------------------------------------------------*
027700060529     C* VERTSRV - Routine di verticalizzazione eccezioni x servizi DPD
027800060529     C*------------------------------------------------------------------------*
027900060529     C     VERTSRV       BEGSR
028000081008     C*
028100081009     C                   SETOFF                                       303132
028200060529     C*
028300060529     C* Inizializzo schiera e indice
028400060529     C                   Z-ADD     *zeros        jSrvL
028500060529     C                   CLEAR                   skSrvL
028600060529     C*
028700060529     C* Se presenti eccezioni x servizi
028800060529     C                   IF        wElencoServizi <> *blanks
028900081009     C                   SETON                                        31
029000060529     C*
029100060529     C* Eseguo "splittamento" dei dati
029200060529     C                   EVAL      InStringa = wElencoServizi
029300060529     C                   EVAL      InSepara  = ','
029400060628     C                   EXSR      SPLIT2
029500060529     C*
029600060529     C* Se nn reperiti elementi listo in stampa
029700060529     C                   IF        i = *zeros
029800060530     C                   EVAL      wRiga = 'WFDPCDYF' + ' ' + wElencoServizi
029900060529     C                   EXCEPT    rigaErr
030000060529     C                   ELSE
030100060529     C                   Z-ADD     1             jSk               4 0
030200060529     C                   DOW       jSk <= i
030300060628     C                   EVAL      DS_SRVC_A = %trim(skSpli2CSV(jSk))
030400060529     C*
030500060529     C* Se nn specificato l'estremo finale del range do x assunto che sia uguale al estremo
030600060529     C* iniziale in modo da gestire sempre i servizi come range
030700060529     C                   IF        SRVC_E_A > '000' AND SRVC_E_A <= '999'
030800060529     C                   ELSE
030900060529     C                   EVAL      SRVC_E_A = SRVC_B_A
031000060529     C                   ENDIF
031100060529     C*
031200060529     C                   EVAL      DS_SRVC = DS_SRVC_A
031300060529     C*
031400060529     C* Verticalizzo tutti i servizi indicati anche come range
031500060529     C                   DOW       SRVC_B <= SRVC_E
031600060529     C*
031700060529     C* Importo unicamente i record riferiti a servizi DPD da noi gestiti
031800060529     C     SRVC_B        LOOKUP    skSrv                                  55
031900060529     C                   IF        %equal
032000081009     C                   SETON                                        32
032100060530     C                   ADD       1             jSrvL
032200060529     C                   MOVE(P)   SRVC_B        skSrvL(jSrvL)
032300060529     C                   ENDIF
032400060529     C                   ADD       1             SRVC_B
032500060529     C                   ENDDO
032600060529     C*
032700060529     C                   ADD       1             jSk
032800060529     C                   ENDDO
032900060529     C*
033000060529     C                   ENDIF
033100060530     C***                ENDIF
033200060529     C*
033300060529     C* Se nn presenti eccezioni x servizi
033400060530     C***                IF        jSrvL = *zeros
033500060530     C                   ELSE
033600060529     C                   ADD       1             jSrvL
033700060529     C                   MOVE(P)   *all'0'       skSrvL(jSrvL)
033800060529     C                   ENDIF
033900081009     C*
034000081009     C* Verifico presenza di eccezioni x servizi gestiti da Bartolini
034100081009     C                   IF        *IN31 = *ON  AND
034200081009     C                             *IN32 = *OFF
034300081009     C                   SETON                                        30
034400081009     C                   ENDIF
034500060529     C*
034600060529     C                   ENDSR
034700060529     C*------------------------------------------------------------------------*
034800060529     C* VERTRULF - Routine di verticalizzazione eccezioni x rule from (praticamente depot partenza)
034900060529     C*------------------------------------------------------------------------*
035000060529     C     VERTRULF      BEGSR
035100060529     C*
035200060529     C* Inizializzo schiera e indice
035300060529     C                   Z-ADD     *zeros        jRulF
035400060529     C                   CLEAR                   skRulF
035500060529     C*
035600060529     C* Se presenti eccezioni x depot di partenza (solo Italia)
035700060529     C                   IF        wElencoRuleFr <> *blanks
035800060529     C*
035900060529     C* Eseguo "splittamento" dei dati
036000060529     C                   EVAL      InStringa = wElencoRuleFr
036100060529     C                   EVAL      InSepara  = ','
036200060628     C                   EXSR      SPLIT2
036300060530     C*
036400060530     C* Salvo i valori ritornati dalla routine in quanto in corso d routine corrente dovrò
036500060530     C* necessariamente richiamarla
036600060530     C                   EVAL      is = i
036700060628     C                   EVAL      skSpli2CSVs = skSpli2CSV
036800060529     C*
036900060529     C* Se nn reperiti elementi listo in stampa
037000060530     C                   IF        is = *zeros
037100060530     C                   EVAL      wRiga = 'WFDPCDYF' + ' ' + wElencoRuleFr
037200060529     C                   EXCEPT    rigaErr
037300060529     C                   ELSE
037400060530     C                   Z-ADD     1             jSks              4 0
037500060530     C                   DOW       jSks <= is
037600060529     C*
037700060529     C* Gestisco indicazione x intera nazione (C=Country)
037800060628     C                   IF        %subst(skSpli2CSVs(jSks):1:1) = 'C'
037900060628     C                   EVAL      DS_ROUP_C = skSpli2CSVs(jSks)
038000060529     C                   IF        ROUP_C_C = 'IT'
038100060529     C                   DOW       jRulF <= %elem(skDptI)
038200060530     C                   IF        skDptI(jRulF+1) <> *blanks
038300060530     C                   ADD       1             jRulF
038400060529     C                   EVAL      ROUP_T = ROUP_C_T
038500060529     C                   EVAL      ROUP_C = skDptI(jRulF)
038600060529     C                   EVAL      skRulF(jRulF) = DS_ROUP
038700060529     C                   ELSE
038800060529     C                   LEAVE
038900060529     C                   ENDIF
039000060529     C                   ENDDO
039100060529     C                   ENDIF
039200060529     C                   ENDIF
039300060529     C*
039400060529     C* Gestisco indicazione x depot (D=Depot)
039500060628     C                   IF        %subst(skSpli2CSVs(jSks):1:1) = 'D'
039600060628     C                   EVAL      DS_ROUP_D = skSpli2CSVs(jSks)
039700060529     C                   IF        ROUP_D_E > '0000' AND ROUP_D_E <= '9999'
039800060529     C                   ELSE
039900060529     C                   EVAL      ROUP_D_E = ROUP_D_B
040000060529     C                   ENDIF
040100060529     C*
040200060529     C                   MOVE(P)   ROUP_D_B      wROUP_B           4 0
040300060529     C                   MOVE(P)   ROUP_D_E      wROUP_E           4 0
040400060529     C                   MOVEL(P)  *blanks       wROUP             4
040500060529     C                   DOW       wROUP_B <= wROUP_E
040600060529     C                   MOVE(P)   wROUP_B       wROUP
040700060529     C     wROUP         LOOKUP    skDptI                                 55
040800060529     C                   IF        %equal
040900060529     C                   ADD       1             jRulF
041000060529     C                   EVAL      ROUP_T = ROUP_D_T
041100060529     C                   EVAL      ROUP_C = wROUP
041200060529     C                   EVAL      skRulF(jRulF) = DS_ROUP
041300060529     C                   ENDIF
041400060529     C                   ADD       1             wROUP_B
041500060529     C                   ENDDO
041600060529     C                   ENDIF
041700060529     C*
041800060529     C* Gestisco indicazione x gruppo depot (D=Depot)
041900060628     C                   IF        %subst(skSpli2CSVs(jSks):1:1) = 'G'
042000060628     C                   EVAL      DS_ROUP_G = skSpli2CSVs(jSks)
042100060529     C                   SORTA     skDptLI
042200060529     C                   Z-ADD     1             jDptLI
042300060529     C                   DOW       jDptLI <= %elem(skDptLI)
042400060529     C                   IF        skDptLI(jDptLI) = *blanks
042500060529     C                   LEAVE
042600060529     C                   ELSE
042700060529     C                   EVAL      DS_GRPD = skDptLI(jDptLI)
042800060529     C                   IF        GRPD_G = ROUP_G_C
042900060529     C                   ADD       1             jRulF
043000060529     C                   EVAL      ROUP_T = 'G'
043100060529     C                   EVAL      ROUP_C = GRPD_D
043200060529     C                   EVAL      skRulF(jRulF) = DS_ROUP
043300060529     C                   ENDIF
043400060529     C                   ENDIF
043500060529     C                   ADD       1             jDptLI
043600060529     C                   ENDDO
043700060529     C                   ENDIF
043800060529     C*
043900060530     C                   ADD       1             jSks
044000060529     C                   ENDDO
044100060529     C*
044200060529     C                   ENDIF
044300060530     C***                ENDIF
044400060529     C*
044500060529     C* Se nn presenti eccezioni x depot di partenza (solo Italia)
044600060530     C***                IF        jRulF = *zeros
044700060530     C                   ELSE
044800060529     C                   ADD       1             jRulF
044900060529     C                   EVAL      ROUP_T = *blanks
045000060529     C                   EVAL      ROUP_C = *all'0'
045100060529     C                   EVAL      skRulF(jRulF) = DS_ROUP
045200060529     C                   ENDIF
045300060529     C*
045400060529     C                   ENDSR
045500060529     C*------------------------------------------------------------------------*
045600060529     C* VERTRULT - Routine di verticalizzazione eccezioni x rule to
045700060529     C*------------------------------------------------------------------------*
045800060529     C     VERTRULT      BEGSR
045900060529     C*
046000060529     C* Inizializzo schiera e indice
046100060529     C                   Z-ADD     *zeros        jRulT
046200060529     C                   CLEAR                   skRulT
046300060530     C                   Z-ADD     *zeros        jCapT
046400060530     C                   CLEAR                   skCapT
046500060529     C*
046600060529     C* Se presenti eccezioni x depot di partenza (solo Italia)
046700060529     C                   IF        wElencoRuleTo <> *blanks
046800060529     C*
046900060529     C* Eseguo "splittamento" dei dati
047000060529     C                   EVAL      InStringa = wElencoRuleTo
047100060529     C                   EVAL      InSepara  = ','
047200060628     C                   EXSR      SPLIT2
047300060529     C*
047400060529     C* Salvo i valori ritornati dalla routine in quanto in corso d routine corrente dovrò
047500060529     C* necessariamente richiamarla
047600060529     C                   EVAL      is = i
047700060628     C                   EVAL      skSpli2CSVs = skSpli2CSV
047800060529     C*
047900060529     C* Se nn reperiti elementi listo in stampa
048000060529     C                   IF        is = *zeros
048100060530     C                   EVAL      wRiga = 'WFDPCDYF' + ' ' + wElencoRuleTo
048200060529     C                   EXCEPT    rigaErr
048300060529     C                   ELSE
048400060529     C                   Z-ADD     1             jSks              4 0
048500060529     C                   DOW       jSks <= is
048600060529     C*
048700060529     C* Gestisco indicazione x intera nazione (C=Country)
048800060628     C                   IF        %subst(skSpli2CSVs(jSks):1:1) = 'C'
048900060628     C                   EVAL      DS_ROUP_C = skSpli2CSVs(jSks)
049000060529     C* Carico i depot della nazione corrente
049100060529     C                   EVAL      wNaz = ROUP_C_C
049200060529     C                   EXSR      CARDPT
049300060529     C*
049400060529     C                   DOW       jRulT <= %elem(skDpt)
049500060530     C                   IF        skDpt(jRulT+1) <> *blanks
049600060530     C                   ADD       1             jRulT
049700060529     C                   EVAL      ROUP_T = ROUP_C_T
049800060529     C                   EVAL      ROUP_C = skDpt(jRulT)
049900060529     C                   EVAL      skRulT(jRulT) = DS_ROUP
050000060529     C                   ELSE
050100060529     C                   LEAVE
050200060529     C                   ENDIF
050300060529     C                   ENDDO
050400060529     C                   ENDIF
050500060529     C*
050600060529     C* Gestisco indicazione x depot (D=Depot)
050700060628     C                   IF        %subst(skSpli2CSVs(jSks):1:1) = 'D'
050800060628     C                   EVAL      DS_ROUP_D = skSpli2CSVs(jSks)
050900060529     C                   IF        ROUP_D_E > '0000' AND ROUP_D_E <= '9999'
051000060529     C                   ELSE
051100060529     C                   EVAL      ROUP_D_E = ROUP_D_B
051200060529     C                   ENDIF
051300060529     C*
051400060529     C                   MOVE(P)   ROUP_D_B      wROUP_B           4 0
051500060529     C                   MOVE(P)   ROUP_D_E      wROUP_E           4 0
051600060529     C                   MOVEL(P)  *blanks       wROUP             4
051700060529     C                   DOW       wROUP_B <= wROUP_E
051800060529     C                   MOVE(P)   wROUP_B       wROUP
051900060529     C     wROUP         LOOKUP    skDpt                                  55
052000060529     C                   IF        %equal
052100060529     C                   ADD       1             jRulT
052200060529     C                   EVAL      ROUP_T = ROUP_D_T
052300060529     C                   EVAL      ROUP_C = wROUP
052400060529     C                   EVAL      skRulT(jRulT) = DS_ROUP
052500060529     C                   ENDIF
052600060529     C                   ADD       1             wROUP_B
052700060529     C                   ENDDO
052800060529     C                   ENDIF
052900060529     C*
053000060529     C* Gestisco indicazione x gruppo depot (D=Depot)
053100060628     C                   IF        %subst(skSpli2CSVs(jSks):1:1) = 'G'
053200060628     C                   EVAL      DS_ROUP_G = skSpli2CSVs(jSks)
053300060529     C                   SORTA     skDptL
053400060529     C                   Z-ADD     1             jDptL
053500060529     C                   DOW       jDptL <= %elem(skDptL)
053600060529     C                   IF        skDptL(jDptL) = *blanks
053700060529     C                   LEAVE
053800060529     C                   ELSE
053900060529     C                   EVAL      DS_GRPD = skDptL(jDptL)
054000060529     C                   IF        GRPD_G = ROUP_G_C
054100060529     C                   ADD       1             jRulT
054200060529     C                   EVAL      ROUP_T = 'G'
054300060529     C                   EVAL      ROUP_C = GRPD_D
054400060529     C                   EVAL      skRulT(jRulT) = DS_ROUP
054500060529     C                   ENDIF
054600060529     C                   ENDIF
054700060529     C                   ADD       1             jDptL
054800060529     C                   ENDDO
054900060529     C                   ENDIF
055000060530     C*
055100060530     C* Gestisco indicazione x range cap
055200060628     C                   IF        %subst(skSpli2CSVs(jSks):1:1) = 'Z'
055300060530     C                   CLEAR                   DS_PTC
055400060530     C                   EVAL      PTC_T = 'Z'
055500060628     C                   EVAL      PTC_ISO2 = %subst(skSpli2CSVs(jSks):2:2)
055600060530     C                   Z-ADD     *zeros        wPosMeno          4 0
055700060628     C                   EVAL      wPosMeno = %scan('-':skSpli2CSVs(jSks))
055800060530     C                   IF        wPosMeno > 0
055900060530     C                   EVAL      PTC_PTCB =
056000060628     C                             %subst(skSpli2CSVs(jSks):4:wPosMeno-4)
056100060530     C                   EVAL      PTC_PTCE =
056200060628     C                             %subst(skSpli2CSVs(jSks):wPosMeno+1)
056300060530     C                   ELSE
056400060530     C                   EVAL      PTC_PTCB =
056500060628     C                             %subst(skSpli2CSVs(jSks):4)
056600060530     C                   EVAL      PTC_PTCE = PTC_PTCB
056700060530     C                   ENDIF
056800060628     C*
056900060628     C* Eseguo riempimento "a 9" del CAP "A"
057000060628     C                   EVAL      wPTCE = %trim(PTC_PTCE)
057100060628     C                   MOVEL     *all'9'       PTC_PTCE
057200060628     C                   MOVEL     wPTCE         PTC_PTCE
057300060628     C*
057400060530     C                   ADD       1             jCapT
057500060628     C                   EVAL      skCapT(jCapT) = DS_PTC
057600060530     C                   ENDIF
057700060529     C*
057800060529     C                   ADD       1             jSks
057900060529     C                   ENDDO
058000060529     C*
058100060529     C                   ENDIF
058200060530     C                   ENDIF
058300060529     C*
058400060530     C* Queste schiere nn devono essere discriminanti ai fini della scrittura x cui se nn
058500060530     C* specificatamente valorizzate le inizializzo a valori d default (=*all)
058600060530     C                   IF        jRulT = *zero
058700060529     C                   ADD       1             jRulT
058800060529     C                   EVAL      ROUP_T = *blanks
058900060529     C                   EVAL      ROUP_C = *all'0'
059000060529     C                   EVAL      skRulT(jRulT) = DS_ROUP
059100060529     C                   ENDIF
059200060530     C                   IF        jCapT = *zero
059300060530     C                   ADD       1             jCapT
059400060530     C                   EVAL      PTC_T     = 'Z'
059500060530     C                   EVAL      PTC_ISO2  = *blanks
059600060530     C                   EVAL      PTC_PTCB  = *all'0'
059700060622     C                   EVAL      PTC_PTCE  = *all'9'
059800060628     C                   EVAL      skCapT(jCapT) = DS_PTC
059900060530     C                   ENDIF
060000060529     C*
060100060529     C                   ENDSR
060200060428     C*------------------------------------------------------------------------*
060300060628     C* SPLIT - Routine di reperimento elementi separati da carattere separatore
060400060428     C*------------------------------------------------------------------------*
060500060428     C     SPLIT         BEGSR
060600060428     C*
060700060428     C* Innanzitutto inizializzo i parametri d OUT
060800060428     C                   RESET                   lenInSepara
060900060428     C                   RESET                   posDa
061000060428     C                   RESET                   posA
061100060428     C                   RESET                   i
061200060428     C                   RESET                   wSize
061300060428     C*
061400060428     C* Determino subito la lunghezza del "separatore"
061500060428     C                   EVAL      lenInSepara = %len(%trim(InSepara))
061600060428     C**********
061700060428     C* Effettuo lo split della stringa dati d input
061800060428     C**********
061900060428     C*
062000060428     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
062100060627     C                   DOW       posDa <= %len(%trim(InStringa))
062200060428     C*
062300060428     C* Gestisco il 1° elemento
062400060428     C                   IF        i = 1
062500060428     C*
062600060428     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemen
062700060428     C                   EVAL      posDa = 1
062800060428     C*
062900060428     C* Eseguo lo scan x trovare la fine del primo campo
063000060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
063100060428     C                                     posDa)
063200060428     C*
063300060428     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
063400060428     C* l'intero contenuto della stringa è l'unico elemento presente
063500060428     C                   IF        posA = *zeros
063600060428     C                   CLEAR                   SkSplitCSV
063700060428     C                   EVAL      SkSplitCSV(i) = %trim(InStringa)
063800060428     C                   LEAVE
063900060428     C                   ELSE
064000060428     C*
064100060428     C* A questo punto "estraggo" l'elemento corrente
064200060428     C                   IF        posA = 1
064300060428     C                   EVAL      SkSplitCSV(i) = *blanks
064400060428     C                   ELSE
064500060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
064600060428     C                                                    (posA-posDa))
064700060428     C                   ENDIF
064800060428     C                   ENDIF
064900060428     C*
065000060428     C* X gli elementi successivi al 1°
065100060428     C                   ELSE
065200060428     C*
065300060428     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
065400060428     C                   EVAL      posDa = posA + lenInSepara
065500060428     C*
065600060428     C* Verifico che nn vi sia il campo nn valorizzato
065700060428     C                   IF        %subst(InStringa:posDa:lenInSepara) =
065800060428     C                             %trim(InSepara)
065900060428     C*
066000060428     C* Se elemento nn valorizzato skippo al prossimo
066100060428     C                   EVAL      SkSplitCSV(i) = *blanks
066200060428     C                   EVAL      posA  = posA + 1
066300060428     C                   ELSE
066400060428     C*
066500060428     C* Eseguo lo scan x trovare la fine dell'elemento corrente
066600060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
066700060428     C                                     posDa+1)
066800060428     C*
066900060428     C* Gestisco l'ultimo campo
067000060428     C                   IF        posA = *zeros
067100060428     C                   EVAL      posA = %len(%trim(InStringa)) + 1
067200060428     C*
067300060428     C* A questo punto "estraggo" l'elemento corrente
067400060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
067500060428     C                                                    (posA-posDa))
067600060428     C                   LEAVE
067700060428     C                   ELSE
067800060428     C*
067900060428     C* A questo punto "estraggo" l'elemento corrente
068000060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
068100060428     C                                                    (posA-posDa))
068200060428     C                   ENDIF
068300060428     C                   ENDIF
068400060428     C                   ENDIF
068500060428     C*
068600060428     C* Incremento il contatore d campo
068700060428     C                   EVAL      i = i +1
068800060428     C                   ENDDO
068900060428     C*
069000060428     C                   ENDSR
069100060628     C*------------------------------------------------------------------------*
069200060628     C* SPLIT2 - Routine di reperimento elementi separati da carattere separatore
069300060628     C*------------------------------------------------------------------------*
069400060628     C     SPLIT2        BEGSR
069500060628     C*
069600060628     C* Innanzitutto inizializzo i parametri d OUT
069700060628     C                   RESET                   lenInSepara
069800060628     C                   RESET                   posDa
069900060628     C                   RESET                   posA
070000060628     C                   RESET                   i
070100060628     C                   RESET                   wSize
070200060628     C*
070300060628     C* Determino subito la lunghezza del "separatore"
070400060628     C                   EVAL      lenInSepara = %len(%trim(InSepara))
070500060628     C**********
070600060628     C* Effettuo lo split della stringa dati d input
070700060628     C**********
070800060628     C*
070900060628     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
071000060628     C                   DOW       posDa <= %len(%trim(InStringa))
071100060628     C*
071200060628     C* Gestisco il 1° elemento
071300060628     C                   IF        i = 1
071400060628     C*
071500060628     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemen
071600060628     C                   EVAL      posDa = 1
071700060628     C*
071800060628     C* Eseguo lo scan x trovare la fine del primo campo
071900060628     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
072000060628     C                                     posDa)
072100060628     C*
072200060628     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
072300060628     C* l'intero contenuto della stringa è l'unico elemento presente
072400060628     C                   IF        posA = *zeros
072500060628     C                   CLEAR                   SkSpli2CSV
072600060628     C                   EVAL      SkSpli2CSV(i) = %trim(InStringa)
072700060628     C                   LEAVE
072800060628     C                   ELSE
072900060628     C*
073000060628     C* A questo punto "estraggo" l'elemento corrente
073100060628     C                   IF        posA = 1
073200060628     C                   EVAL      SkSpli2CSV(i) = *blanks
073300060628     C                   ELSE
073400060628     C                   EVAL      SkSpli2CSV(i) = %subst(InStringa:posDa:
073500060628     C                                                    (posA-posDa))
073600060628     C                   ENDIF
073700060628     C                   ENDIF
073800060628     C*
073900060628     C* X gli elementi successivi al 1°
074000060628     C                   ELSE
074100060628     C*
074200060628     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
074300060628     C                   EVAL      posDa = posA + lenInSepara
074400060628     C*
074500060628     C* Verifico che nn vi sia il campo nn valorizzato
074600060628     C                   IF        %subst(InStringa:posDa:lenInSepara) =
074700060628     C                             %trim(InSepara)
074800060628     C*
074900060628     C* Se elemento nn valorizzato skippo al prossimo
075000060628     C                   EVAL      SkSpli2CSV(i) = *blanks
075100060628     C                   EVAL      posA  = posA + 1
075200060628     C                   ELSE
075300060628     C*
075400060628     C* Eseguo lo scan x trovare la fine dell'elemento corrente
075500060628     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
075600060628     C                                     posDa+1)
075700060628     C*
075800060628     C* Gestisco l'ultimo campo
075900060628     C                   IF        posA = *zeros
076000060628     C                   EVAL      posA = %len(%trim(InStringa)) + 1
076100060628     C*
076200060628     C* A questo punto "estraggo" l'elemento corrente
076300060628     C                   EVAL      SkSpli2CSV(i) = %subst(InStringa:posDa:
076400060628     C                                                    (posA-posDa))
076500060628     C                   LEAVE
076600060628     C                   ELSE
076700060628     C*
076800060628     C* A questo punto "estraggo" l'elemento corrente
076900060628     C                   EVAL      SkSpli2CSV(i) = %subst(InStringa:posDa:
077000060628     C                                                    (posA-posDa))
077100060628     C                   ENDIF
077200060628     C                   ENDIF
077300060628     C                   ENDIF
077400060628     C*
077500060628     C* Incremento il contatore d campo
077600060628     C                   EVAL      i = i +1
077700060628     C                   ENDDO
077800060628     C*
077900060628     C                   ENDSR
078000060529     C*------------------------------------------------------------------------*
078100060529     C* CARTAB - CARICAMENTO DATI TABELLATI
078200060529     C*------------------------------------------------------------------------*
078300060529     C     cartab        BEGSR
078400060529     C*
078500060529     C* Reperisco tutti i servizi DPD da noi gestiti
078600060529     C                   Z-ADD     *zeros        jSrv
078700060529     C                   CLEAR                   skSrv
078800060529     C                   EVAL      tbeCOD = 'SRV'
078900060529     C     KEYtbe01_P    SETLL     TNTBE01L
079000060529     C                   IF        %found(TNTBE01L)
079100060529     C     KEYtbe01_P    READE     TNTBE01L
079200060529     C                   DOW       not %eof(TNTBE01L)
079300060529     C                   IF        tbeATB = *blanks
079400060529     C                   ADD       1             jSrv
079500060529     C                   MOVEL(P)  tbeKE1        skSrv(jSrv)
079600060529     C                   ENDIF
079700060529     C     KEYtbe01_P    READE     TNTBE01L
079800060529     C                   ENDDO
079900060529     C                   ENDIF
080000060529     C*
080100060529     C                   ENDSR
080200060529     C*------------------------------------------------------------------------*
080300060529     C* CARDPTI - CARICAMENTO ANAGRAFICA DEPOT DPD ITALIA
080400060529     C*------------------------------------------------------------------------*
080500060529     C     cardptI       BEGSR
080600060529     C*
080700060529     C* Carico solo i depot italia (quindi i ns P.O. che servono DPD)
080800060529     C                   Z-ADD     *zeros        jDptI
080900060529     C                   Z-ADD     *zeros        jDptLI
081000060529     C                   CLEAR                   skDptI
081100060529     C                   CLEAR                   skDptLI
081200060529     C                   EVAL      cdpVER  = PARVER
081300060529     C                   EVAL      cdpISO2 = 'IT'
081400060529     C     KEYcdp02_P    SETLL     DPCDP02L
081500060529     C                   IF        %found(DPCDP02L)
081600060529     C     KEYcdp02_P    READE     DPCDP02L
081700060529     C                   DOW       not %eof(DPCDP02L)
081800060529     C                   IF        cdpATB = *blanks
081900060529     C                   ADD       1             jDptI
082000060529     C                   MOVEL(P)  cdpDPC        skDptI(jDptI)
082100060529     C*
082200060529     C* Verifico il raggruppamento del depot
082300060529     C                   IF        cdpGID <> *blanks
082400060529     C*
082500060529     C* Eseguo "splittamento" dei dati
082600060529     C                   EVAL      InStringa = cdpGID
082700060529     C                   EVAL      InSepara  = ','
082800060529     C                   EXSR      SPLIT
082900060529     C*
083000060529     C* Se nn reperiti elementi listo in stampa
083100060529     C                   IF        i = *zeros
083200060530     C                   EVAL      wRiga = 'WFDPCDYF' + ' ' + cdpGID
083300060529     C                   EXCEPT    rigaErr
083400060529     C                   ELSE
083500060529     C                   Z-ADD     1             jSk               4 0
083600060529     C                   DOW       jSk <= i
083700060529     C                   ADD       1             jDptLI
083800060529     C                   CLEAR                   DS_GRPD
083900060529     C                   EVAL      GRPD_G = skSplitCSV(jSk)
084000060529     C                   EVAL      GRPD_D = cdpDPC
084100060529     C                   EVAL      skDptLI(jDptLI) = DS_GRPD
084200060529     C                   ADD       1             jSk
084300060529     C                   ENDDO
084400060529     C                   ENDIF
084500060529     C                   ENDIF
084600060529     C*
084700060529     C                   ENDIF
084800060529     C     KEYcdp02_P    READE     DPCDP02L
084900060529     C                   ENDDO
085000060529     C                   ENDIF
085100060529     C*
085200060529     C                   ENDSR
085300060529     C*------------------------------------------------------------------------*
085400060529     C* CARDPT - CARICAMENTO ANAGRAFICA DEPOT DPD
085500060529     C*------------------------------------------------------------------------*
085600060529     C     cardpt        BEGSR
085700060529     C*
085800060529     C* Carico solo i depot italia (quindi i ns P.O. che servono DPD)
085900060529     C                   Z-ADD     *zeros        jDpt
086000060529     C                   Z-ADD     *zeros        jDptL
086100060529     C                   CLEAR                   skDpt
086200060529     C                   CLEAR                   skDptL
086300060529     C                   EVAL      cdpVER  = PARVER
086400060529     C                   EVAL      cdpISO2 = wNaz
086500060529     C     KEYcdp02_P    SETLL     DPCDP02L
086600060529     C                   IF        %found(DPCDP02L)
086700060529     C     KEYcdp02_P    READE     DPCDP02L
086800060529     C                   DOW       not %eof(DPCDP02L)
086900060529     C                   IF        cdpATB = *blanks
087000060529     C                   ADD       1             jDpt
087100060529     C                   MOVEL(P)  cdpDPC        skDpt(jDpt)
087200060529     C*
087300060529     C* Verifico il raggruppamento del depot
087400060529     C                   IF        cdpGID <> *blanks
087500060529     C*
087600060529     C* Eseguo "splittamento" dei dati
087700060529     C                   EVAL      InStringa = cdpGID
087800060529     C                   EVAL      InSepara  = ','
087900060529     C                   EXSR      SPLIT
088000060529     C*
088100060529     C* Se nn reperiti elementi listo in stampa
088200060529     C                   IF        i = *zeros
088300060530     C                   EVAL      wRiga = 'WFDPCDYF' + ' ' + cdpGID
088400060529     C                   EXCEPT    rigaErr
088500060529     C                   ELSE
088600060529     C                   Z-ADD     1             jSk               4 0
088700060529     C                   DOW       jSk <= i
088800060529     C                   ADD       1             jDptL
088900060529     C                   CLEAR                   DS_GRPD
089000060529     C                   EVAL      GRPD_G = skSplitCSV(jSk)
089100060529     C                   EVAL      GRPD_D = cdpDPC
089200060529     C                   EVAL      skDptL(jDptL) = DS_GRPD
089300060529     C                   ADD       1             jSk
089400060529     C                   ENDDO
089500060529     C                   ENDIF
089600060529     C*
089700060529     C                   ENDIF
089800060529     C                   ENDIF
089900060529     C     KEYcdp02_P    READE     DPCDP02L
090000060529     C                   ENDDO
090100060529     C                   ENDIF
090200060529     C*
090300060529     C                   ENDSR
090400000000     C*------------------------------------------------------------------------*
090500980403     C* *INZSR - OPERAZIONI INIZIALI
090600000000     C*------------------------------------------------------------------------*
090700980403     C     *INZSR        BEGSR
090800000000     C*
090900000000     C* RICEVIMENTO PARAMETRI
091000000000     C     *ENTRY        PLIST
091100000000     C                   PARM                    KPJBA
091200060427     C                   MOVEL     KPJBU         PARAM
091300060529     C*
091400060529     C* Definizione chiavi di lettura
091500060529     C*
091600060529     C* TNTBE01L - Parziale
091700060529     C     KEYtbe01_P    KLIST
091800060529     C                   KFLD                    tbeCOD
091900060529     C*
092000060529     C* DPCDP02L - Parziale
092100060529     C     KEYcdp02_P    KLIST
092200060529     C                   KFLD                    cdpVER
092300060529     C                   KFLD                    cdpISO2
092400000000     C*
092500000000     C                   ENDSR
092600060427     C*------------------------------------------------------------------------*
092700060427     OPRTF198   E            rigaErr     1
092800060427     O                       wRiga
