000100000000     H*------------------------------------------------------------------------*
000200000104     H* IMPORTAZIONE CAPPARIO DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500060427     F*------------------------------------------------------------------------*
000600060427     F* DATA BASE
000700060427     F*------------------------------------------------------------------------*
000800060427     FDPCAL01L  UF A E           K DISK
000900060427     FWFDPCALF  IF   E           K DISK
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
007700060627     D skSplitCSV      S           8192    DIM(1000) VARYING
007800060627     D skSplitCSVs     S           8192    DIM(1000) VARYING
007900060428     D lenInSepara     S              2  0 INZ(*zeros)
008000060428     D posDa           S              5  0 INZ(*zeros)
008100060428     D posA            S              5  0 INZ(*zeros)
008200060428     D i               s              4  0 INZ(1)
008300060529     D is              s              4  0 INZ(1)
008400060428     D wSize           S             10I 0 INZ(*zeros)
008500060427     D*-------------------
008600060427     D* VARIABILI DI WRK
008700060427     D*-------------------
008800060529     D wNaz            s                   LIKE(cdpISO2)
008900060529     D wCharCSV        s              8    INZ('|')
009000060627     D wAlfa           s           8192    INZ varying
009100060529     D wRiga           s            198    INZ
009200060627     D wElencoServizi  S           8192    INZ VARYING
009300060627     D wElencoRuleFr   S           8192    INZ VARYING
009400060627     D wElencoRuleTo   S           8192    INZ VARYING
009500060529     D skDptI          S              4    DIM(200)
009600060529     D jDptI           S              3  0 INZ
009700060529     D skDpt           S              4    DIM(1000)
009800060529     D jDpt            S              4  0 INZ
009900060529     D skSrv           S              3  0 DIM(100)
010000060529     D jSrv            S              3  0 INZ
010100060529     D skSrvL          S              3    DIM(1000) DESCEND
010200060529     D jSrvL           S              4  0 INZ
010300060529     D skRulF          S              5    DIM(1000) DESCEND
010400060529     D jRulF           S              4  0 INZ
010500060529     D skRulT          S              5    DIM(1000) DESCEND
010600060529     D jRulT           S              4  0 INZ
010700060530     D skCapT          S                   LIKE(DS_PTC) DIM(1000) DESCEND
010800060530     D jCapT           S              4  0 INZ
010900060529     D skDptLI         S              8    DIM(1000) DESCEND
011000060529     D jDptLI          S              4  0 INZ
011100060529     D skDptL          S              8    DIM(1000) DESCEND
011200060529     D jDptL           S              4  0 INZ
011300060622     D wPTCE           S              9    INZ VARYING
011400000000     C*------------------------------------------------------------------------*
011500000000     C* MAIN LINE
011600000000     C*------------------------------------------------------------------------*
011700060529     C*
011800060529     C* Carico le tabelle occorrenti
011900060529     C                   EXSR      CARTAB
012000060529     C*
012100060529     C* Carico i depot Italia della versione correntemente in importazione
012200060529     C                   EXSR      CARDPTI
012300060427     C*
012400060427     C* Effettuo elaborazione
012500060427     C                   EXSR      EXECAL
012600000000     C*
012700000000     C                   SETON                                        LR
012800060427     C*------------------------------------------------------------------------*
012900060427     C*  EXECAL - IMPORTA L' ARCHIVIO "ALLOW"
013000060427     C*------------------------------------------------------------------------*
013100060427     C     EXECAL        BEGSR
013200060427     C*
013300060427     C* Scorro tutto il file d wrk
013400060427     C     *START        SETLL     WFDPCALF
013500060427     C                   READ      WFDPCALF
013600060427     C                   DOW       not %eof(WFDPCALF)
013700060427     C                   IF        %subst(%trim(WDCALDTA):1:1) <> '#'
013800170109     C* Aggiungo filtro unicamente per ottimizzare performance elaborazione
013900170109     C                             AND
014000170109     C                             (%subst(%trim(WDCALDTA):1:3)  = 'CIT' OR
014100170109     C                              %subst(%trim(WDCALDTA):1:1) <> 'C')
014200060428     C*
014300060428     C* Eseguo "splittamento" dei dati
014400060428     C                   EVAL      InStringa = WDCALDTA
014500060428     C                   EVAL      InSepara  = wCharCSV
014600060428     C                   EXSR      SPLIT
014700060428     C*
014800060428     C* Se nn reperiti elementi listo in stampa
014900060428     C                   IF        i = *zeros
015000060427     C                   EVAL      wRiga = 'WFDPCALF' + ' ' + WDCALDTA
015100060427     C                   EXCEPT    rigaErr
015200060427     C                   ELSE
015300060427     C*
015400060427     C* Imposto il buffer del file d output
015500060427     C                   CLEAR                   DPCAL000
015600060427     C                   EVAL      CALVER  = PARVER
015700060529     C*
015800060428     C                   EVAL      wAlfa = %trim(skSplitCSV(4))
015900060427     C                   MOVE      wAlfa         CALID
016000060427     C                   EVAL      CALATB  = *blanks
016100060529     C*
016200060529     C                   EVAL      wAlfa = %trim(skSplitCSV(1))
016300060529     C                   EVAL      wElencoRuleFr = wAlfa
016400060529     C*
016500060529     C                   EVAL      wAlfa = %trim(skSplitCSV(2))
016600060529     C                   EVAL      wElencoServizi = wAlfa
016700060529     C*
016800060529     C                   EVAL      wAlfa = %trim(skSplitCSV(3))
016900060529     C                   EVAL      wElencoRuleTo = wAlfa
017000081008     C*
017100081008     C* Eseguo sub-split del campo elenco eccezioni servizi x poter così verticalizzare gli
017200081008     C* instradamenti relativi
017300081008     C                   EXSR      VERTSRV
017400081008     C*
017500081008     C* Proseguo solamente se il record corrente è riferito a servizi da noi gestiti
017600081008     C                   IF        *IN30 = *OFF
017700060529     C*
017800060529     C* Eseguo sub-split del campo elenco eccezioni depot i x poter così verticalizzare gli
017900060529     C* instradamenti relativi
018000060529     C                   EXSR      VERTRULF
018100060529     C*
018200060529     C* Eseguo sub-split del campo elenco eccezioni depot i x poter così verticalizzare gli
018300060529     C* instradamenti relativi
018400060529     C                   EXSR      VERTRULT
018500060529     C*
018600060529     C* Ordino in maniera discendente le schiere con la lista delle eccezzioni x servizio
018700060529     C* o depot partenza
018800081008     C                   SORTA     skSrvL
018900060529     C                   SORTA     skRulF
019000060529     C                   SORTA     skRulT
019100060530     C                   SORTA     skCapT
019200060529     C*
019300060529     C* A questo punto effettuo la combinazione lineare delle 3 schiere eccezioni x eccezioni depot
019400060529     C* partenza/servizi/eccezioni arrivi x verticalizzare tutte le combinate risultanti
019500060529     C                   Z-ADD     1             jSrvL
019600060529     C* ..ciclo sui servizi
019700060529     C                   DOW       jSrvL <= %elem(skSrvL)
019800060529     C                   IF        skSrvL(jSrvL) = *blanks
019900060529     C                   LEAVE
020000060529     C                   ELSE
020100060529     C                   Z-ADD     1             jRulF
020200060529     C* ..ciclo sui depot partenza
020300060529     C                   DOW       jRulF <= %elem(skRulF)
020400060529     C                   IF        skRulF(jRulF) = *blanks
020500060529     C                   LEAVE
020600060529     C                   ELSE
020700060529     C                   Z-ADD     1             jRulT
020800060530     C* ..ciclo sui depot arrivo
020900060529     C                   DOW       jRulT <= %elem(skRulT)
021000060529     C                   IF        skRulT(jRulT) = *blanks
021100060529     C                   LEAVE
021200060529     C                   ELSE
021300060530     C                   Z-ADD     1             jCapT
021400060530     C* ..ciclo sul range cap arrivo
021500060530     C                   DOW       jCapT <= %elem(skCapT)
021600060530     C                   IF        skCapT(jCapT) = *blanks
021700060530     C                   LEAVE
021800060530     C                   ELSE
021900060529     C                   MOVE(P)   skSrvL(jSrvL) CALSRVC
022000060529     C                   EVAL      DS_ROUP  = skRulF(jRulF)
022100060529     C                   EVAL      CALRULFT = ROUP_T
022200060529     C                   EVAL      CALRULF  = ROUP_C
022300060529     C                   EVAL      DS_ROUP  = skRulT(jRulT)
022400060529     C                   EVAL      CALRULTT = ROUP_T
022500060529     C                   EVAL      CALRULT  = ROUP_C
022600060530     C                   EVAL      DS_PTC   = skCapT(jCapT)
022700060530     C                   IF        CALRULTT = *blanks
022800060530     C                   EVAL      CALRULTT = PTC_T
022900060530     C                   ENDIF
023000060530     C                   EVAL      CALISO2  = PTC_ISO2
023100060530     C                   EVAL      CALPTCB  = PTC_PTCB
023200060530     C                   EVAL      CALPTCE  = PTC_PTCE
023300060622     C* Se presenti permessi => scrivo il record
023400060529     C                   IF        CALRULF <> *blanks OR
023500060529     C                             CALSRVC <> *zeros  OR
023600060529     C                             CALRULT <> *blanks OR
023700060530     C                             CALISO2 <> *blanks
023800060622     C*
023900060622     C* Eseguo forzatura x Portogallo
024000060622     C                   SETOFF                                       50
024100060622     C                   IF        CALISO2 = 'PT'
024200060629     C                   IF        %len(%trim(CALPTCB)) = 7
024300060629     C                   EVAL      CALPTCB = %subst(CALPTCB:1:4)
024400060622     C                   ELSE
024500060622     C                   SETON                                        50
024600060622     C                   EVAL      wRiga = 'CALPTCB'+' '+CALISO2+' '+CALPTCB
024700060622     C                   EXCEPT    rigaErr
024800060622     C                   ENDIF
024900060622     C                   ENDIF
025000060622     C*
025100060622     C  N50              WRITE(e)  DPCAL000
025200060529     C                   ENDIF
025300060530     C*
025400060530     C                   ENDIF
025500060530     C                   ADD       1             jCapT
025600060530     C                   ENDDO
025700060529     C*
025800060529     C                   ENDIF
025900060529     C                   ADD       1             jRulT
026000060529     C                   ENDDO
026100060529     C*
026200060529     C                   ENDIF
026300060529     C                   ADD       1             jRulF
026400060529     C                   ENDDO
026500060529     C*
026600060529     C                   ENDIF
026700060529     C                   ADD       1             jSrvL
026800060529     C                   ENDDO
026900060529     C*
027000081008     C                   ENDIF
027100081008     C*
027200060427     C                   ENDIF
027300060427     C                   ENDIF
027400060427     C                   READ      WFDPCALF
027500060427     C                   ENDDO
027600060427     C*
027700060427     C                   ENDSR
027800060529     C*------------------------------------------------------------------------*
027900060529     C* VERTSRV - Routine di verticalizzazione eccezioni x servizi DPD
028000060529     C*------------------------------------------------------------------------*
028100060529     C     VERTSRV       BEGSR
028200081008     C*
028300081009     C                   SETOFF                                       303132
028400060529     C*
028500060529     C* Inizializzo schiera e indice
028600060529     C                   Z-ADD     *zeros        jSrvL
028700060529     C                   CLEAR                   skSrvL
028800060529     C*
028900060529     C* Se presenti eccezioni x servizi
029000060529     C                   IF        wElencoServizi <> *blanks
029100081009     C                   SETON                                        31
029200060529     C*
029300060529     C* Eseguo "splittamento" dei dati
029400060529     C                   EVAL      InStringa = wElencoServizi
029500060529     C                   EVAL      InSepara  = ','
029600060529     C                   EXSR      SPLIT
029700060529     C*
029800060529     C* Se nn reperiti elementi listo in stampa
029900060529     C                   IF        i = *zeros
030000060529     C                   EVAL      wRiga = 'WFDPCALF' + ' ' + wElencoServizi
030100060529     C                   EXCEPT    rigaErr
030200060529     C                   ELSE
030300060529     C                   Z-ADD     1             jSk               4 0
030400060529     C                   DOW       jSk <= i
030500060529     C                   EVAL      DS_SRVC_A = %trim(skSplitCSV(jSk))
030600060529     C*
030700060529     C* Se nn specificato l'estremo finale del range do x assunto che sia uguale al estremo
030800060529     C* iniziale in modo da gestire sempre i servizi come range
030900060529     C                   IF        SRVC_E_A > '000' AND SRVC_E_A <= '999'
031000060529     C                   ELSE
031100060529     C                   EVAL      SRVC_E_A = SRVC_B_A
031200060529     C                   ENDIF
031300060529     C*
031400060529     C                   EVAL      DS_SRVC = DS_SRVC_A
031500060529     C*
031600060529     C* Verticalizzo tutti i servizi indicati anche come range
031700060529     C                   DOW       SRVC_B <= SRVC_E
031800060529     C*
031900060529     C* Importo unicamente i record riferiti a servizi DPD da noi gestiti
032000060529     C     SRVC_B        LOOKUP    skSrv                                  55
032100060529     C                   IF        %equal
032200081009     C                   SETON                                        32
032300060530     C                   ADD       1             jSrvL
032400060529     C                   MOVE(P)   SRVC_B        skSrvL(jSrvL)
032500060529     C                   ENDIF
032600060529     C                   ADD       1             SRVC_B
032700060529     C                   ENDDO
032800060529     C*
032900060529     C                   ADD       1             jSk
033000060529     C                   ENDDO
033100060529     C*
033200060529     C                   ENDIF
033300060530     C***                ENDIF
033400060529     C*
033500060529     C* Se nn presenti eccezioni x servizi
033600060530     C***                IF        jSrvL = *zeros
033700060530     C                   ELSE
033800060529     C                   ADD       1             jSrvL
033900060529     C                   MOVE(P)   *all'0'       skSrvL(jSrvL)
034000060529     C                   ENDIF
034100081009     C*
034200081009     C* Verifico presenza di eccezioni x servizi gestiti da Bartolini
034300081009     C                   IF        *IN31 = *ON  AND
034400081009     C                             *IN32 = *OFF
034500081009     C                   SETON                                        30
034600081009     C                   ENDIF
034700060529     C*
034800060529     C                   ENDSR
034900060529     C*------------------------------------------------------------------------*
035000060529     C* VERTRULF - Routine di verticalizzazione eccezioni x rule from (praticamente depot partenza)
035100060529     C*------------------------------------------------------------------------*
035200060529     C     VERTRULF      BEGSR
035300060529     C*
035400060529     C* Inizializzo schiera e indice
035500060529     C                   Z-ADD     *zeros        jRulF
035600060529     C                   CLEAR                   skRulF
035700060529     C*
035800060529     C* Se presenti eccezioni x depot di partenza (solo Italia)
035900060529     C                   IF        wElencoRuleFr <> *blanks
036000060529     C*
036100060529     C* Eseguo "splittamento" dei dati
036200060529     C                   EVAL      InStringa = wElencoRuleFr
036300060529     C                   EVAL      InSepara  = ','
036400060529     C                   EXSR      SPLIT
036500060530     C*
036600060530     C* Salvo i valori ritornati dalla routine in quanto in corso d routine corrente dovrò
036700060530     C* necessariamente richiamarla
036800060530     C                   EVAL      is = i
036900060530     C                   EVAL      skSplitCSVs = skSplitCSV
037000060529     C*
037100060529     C* Se nn reperiti elementi listo in stampa
037200060530     C                   IF        is = *zeros
037300060529     C                   EVAL      wRiga = 'WFDPCALF' + ' ' + wElencoRuleFr
037400060529     C                   EXCEPT    rigaErr
037500060529     C                   ELSE
037600060530     C                   Z-ADD     1             jSks              4 0
037700060530     C                   DOW       jSks <= is
037800060529     C*
037900060529     C* Gestisco indicazione x intera nazione (C=Country)
038000060530     C                   IF        %subst(skSplitCSVs(jSks):1:1) = 'C'
038100060530     C                   EVAL      DS_ROUP_C = skSplitCSVs(jSks)
038200060529     C                   IF        ROUP_C_C = 'IT'
038300060529     C                   DOW       jRulF <= %elem(skDptI)
038400060530     C                   IF        skDptI(jRulF+1) <> *blanks
038500060530     C                   ADD       1             jRulF
038600060529     C                   EVAL      ROUP_T = ROUP_C_T
038700060529     C                   EVAL      ROUP_C = skDptI(jRulF)
038800060529     C                   EVAL      skRulF(jRulF) = DS_ROUP
038900060529     C                   ELSE
039000060529     C                   LEAVE
039100060529     C                   ENDIF
039200060529     C                   ENDDO
039300060529     C                   ENDIF
039400060529     C                   ENDIF
039500060529     C*
039600060529     C* Gestisco indicazione x depot (D=Depot)
039700060530     C                   IF        %subst(skSplitCSVs(jSks):1:1) = 'D'
039800060530     C                   EVAL      DS_ROUP_D = skSplitCSVs(jSks)
039900060529     C                   IF        ROUP_D_E > '0000' AND ROUP_D_E <= '9999'
040000060529     C                   ELSE
040100060529     C                   EVAL      ROUP_D_E = ROUP_D_B
040200060529     C                   ENDIF
040300060529     C*
040400060529     C                   MOVE(P)   ROUP_D_B      wROUP_B           4 0
040500060529     C                   MOVE(P)   ROUP_D_E      wROUP_E           4 0
040600060529     C                   MOVEL(P)  *blanks       wROUP             4
040700060529     C                   DOW       wROUP_B <= wROUP_E
040800060529     C                   MOVE(P)   wROUP_B       wROUP
040900060529     C     wROUP         LOOKUP    skDptI                                 55
041000060529     C                   IF        %equal
041100060529     C                   ADD       1             jRulF
041200060529     C                   EVAL      ROUP_T = ROUP_D_T
041300060529     C                   EVAL      ROUP_C = wROUP
041400060529     C                   EVAL      skRulF(jRulF) = DS_ROUP
041500060529     C                   ENDIF
041600060529     C                   ADD       1             wROUP_B
041700060529     C                   ENDDO
041800060529     C                   ENDIF
041900060529     C*
042000060529     C* Gestisco indicazione x gruppo depot (D=Depot)
042100060530     C                   IF        %subst(skSplitCSVs(jSks):1:1) = 'G'
042200060530     C                   EVAL      DS_ROUP_G = skSplitCSVs(jSks)
042300060529     C                   SORTA     skDptLI
042400060529     C                   Z-ADD     1             jDptLI
042500060529     C                   DOW       jDptLI <= %elem(skDptLI)
042600060529     C                   IF        skDptLI(jDptLI) = *blanks
042700060529     C                   LEAVE
042800060529     C                   ELSE
042900060529     C                   EVAL      DS_GRPD = skDptLI(jDptLI)
043000060529     C                   IF        GRPD_G = ROUP_G_C
043100060529     C                   ADD       1             jRulF
043200060529     C                   EVAL      ROUP_T = 'G'
043300060529     C                   EVAL      ROUP_C = GRPD_D
043400060529     C                   EVAL      skRulF(jRulF) = DS_ROUP
043500060529     C                   ENDIF
043600060529     C                   ENDIF
043700060529     C                   ADD       1             jDptLI
043800060529     C                   ENDDO
043900060529     C                   ENDIF
044000060529     C*
044100060530     C                   ADD       1             jSks
044200060529     C                   ENDDO
044300060529     C*
044400060529     C                   ENDIF
044500060530     C***                ENDIF
044600060529     C*
044700060529     C* Se nn presenti eccezioni x depot di partenza (solo Italia)
044800060530     C***                IF        jRulF = *zeros
044900060530     C                   ELSE
045000060529     C                   ADD       1             jRulF
045100060529     C                   EVAL      ROUP_T = *blanks
045200060529     C                   EVAL      ROUP_C = *all'0'
045300060529     C                   EVAL      skRulF(jRulF) = DS_ROUP
045400060529     C                   ENDIF
045500060529     C*
045600060529     C                   ENDSR
045700060529     C*------------------------------------------------------------------------*
045800060529     C* VERTRULT - Routine di verticalizzazione eccezioni x rule to
045900060529     C*------------------------------------------------------------------------*
046000060529     C     VERTRULT      BEGSR
046100060529     C*
046200060529     C* Inizializzo schiera e indice
046300060529     C                   Z-ADD     *zeros        jRulT
046400060529     C                   CLEAR                   skRulT
046500060530     C                   Z-ADD     *zeros        jCapT
046600060530     C                   CLEAR                   skCapT
046700060529     C*
046800060529     C* Se presenti eccezioni x depot di partenza (solo Italia)
046900060529     C                   IF        wElencoRuleTo <> *blanks
047000060529     C*
047100060529     C* Eseguo "splittamento" dei dati
047200060529     C                   EVAL      InStringa = wElencoRuleTo
047300060529     C                   EVAL      InSepara  = ','
047400060529     C                   EXSR      SPLIT
047500060529     C*
047600060529     C* Salvo i valori ritornati dalla routine in quanto in corso d routine corrente dovrò
047700060529     C* necessariamente richiamarla
047800060529     C                   EVAL      is = i
047900060529     C                   EVAL      skSplitCSVs = skSplitCSV
048000060529     C*
048100060529     C* Se nn reperiti elementi listo in stampa
048200060529     C                   IF        is = *zeros
048300060529     C                   EVAL      wRiga = 'WFDPCALF' + ' ' + wElencoRuleTo
048400060529     C                   EXCEPT    rigaErr
048500060529     C                   ELSE
048600060529     C                   Z-ADD     1             jSks              4 0
048700060529     C                   DOW       jSks <= is
048800060529     C*
048900060529     C* Gestisco indicazione x intera nazione (C=Country)
049000060529     C                   IF        %subst(skSplitCSVs(jSks):1:1) = 'C'
049100060529     C                   EVAL      DS_ROUP_C = skSplitCSVs(jSks)
049200060529     C* Carico i depot della nazione corrente
049300060529     C                   EVAL      wNaz = ROUP_C_C
049400060529     C                   EXSR      CARDPT
049500060529     C*
049600060529     C                   DOW       jRulT <= %elem(skDpt)
049700060530     C                   IF        skDpt(jRulT+1) <> *blanks
049800060530     C                   ADD       1             jRulT
049900060529     C                   EVAL      ROUP_T = ROUP_C_T
050000060529     C                   EVAL      ROUP_C = skDpt(jRulT)
050100060529     C                   EVAL      skRulT(jRulT) = DS_ROUP
050200060529     C                   ELSE
050300060529     C                   LEAVE
050400060529     C                   ENDIF
050500060529     C                   ENDDO
050600060529     C                   ENDIF
050700060529     C*
050800060529     C* Gestisco indicazione x depot (D=Depot)
050900060529     C                   IF        %subst(skSplitCSVs(jSks):1:1) = 'D'
051000060529     C                   EVAL      DS_ROUP_D = skSplitCSVs(jSks)
051100060529     C                   IF        ROUP_D_E > '0000' AND ROUP_D_E <= '9999'
051200060529     C                   ELSE
051300060529     C                   EVAL      ROUP_D_E = ROUP_D_B
051400060529     C                   ENDIF
051500060529     C*
051600060529     C                   MOVE(P)   ROUP_D_B      wROUP_B           4 0
051700060529     C                   MOVE(P)   ROUP_D_E      wROUP_E           4 0
051800060529     C                   MOVEL(P)  *blanks       wROUP             4
051900060529     C                   DOW       wROUP_B <= wROUP_E
052000060529     C                   MOVE(P)   wROUP_B       wROUP
052100060529     C     wROUP         LOOKUP    skDpt                                  55
052200060529     C                   IF        %equal
052300060529     C                   ADD       1             jRulT
052400060529     C                   EVAL      ROUP_T = ROUP_D_T
052500060529     C                   EVAL      ROUP_C = wROUP
052600060529     C                   EVAL      skRulT(jRulT) = DS_ROUP
052700060529     C                   ENDIF
052800060529     C                   ADD       1             wROUP_B
052900060529     C                   ENDDO
053000060529     C                   ENDIF
053100060529     C*
053200060529     C* Gestisco indicazione x gruppo depot (D=Depot)
053300060529     C                   IF        %subst(skSplitCSVs(jSks):1:1) = 'G'
053400060529     C                   EVAL      DS_ROUP_G = skSplitCSVs(jSks)
053500060529     C                   SORTA     skDptL
053600060529     C                   Z-ADD     1             jDptL
053700060529     C                   DOW       jDptL <= %elem(skDptL)
053800060529     C                   IF        skDptL(jDptL) = *blanks
053900060529     C                   LEAVE
054000060529     C                   ELSE
054100060529     C                   EVAL      DS_GRPD = skDptL(jDptL)
054200060529     C                   IF        GRPD_G = ROUP_G_C
054300060529     C                   ADD       1             jRulT
054400060529     C                   EVAL      ROUP_T = 'G'
054500060529     C                   EVAL      ROUP_C = GRPD_D
054600060529     C                   EVAL      skRulT(jRulT) = DS_ROUP
054700060529     C                   ENDIF
054800060529     C                   ENDIF
054900060529     C                   ADD       1             jDptL
055000060529     C                   ENDDO
055100060529     C                   ENDIF
055200060530     C*
055300060530     C* Gestisco indicazione x range cap
055400060530     C                   IF        %subst(skSplitCSVs(jSks):1:1) = 'Z'
055500060530     C                   CLEAR                   DS_PTC
055600060530     C                   EVAL      PTC_T = 'Z'
055700060530     C                   EVAL      PTC_ISO2 = %subst(skSplitCSVs(jSks):2:2)
055800060530     C                   Z-ADD     *zeros        wPosMeno          4 0
055900060530     C                   EVAL      wPosMeno = %scan('-':skSplitCSVs(jSks))
056000060530     C                   IF        wPosMeno > 0
056100060530     C                   EVAL      PTC_PTCB =
056200060530     C                             %subst(skSplitCSVs(jSks):4:wPosMeno-4)
056300060622     C                   EVAL      PTC_PTCE =
056400060530     C                             %subst(skSplitCSVs(jSks):wPosMeno+1)
056500060530     C                   ELSE
056600060530     C                   EVAL      PTC_PTCB =
056700060530     C                             %subst(skSplitCSVs(jSks):4)
056800060530     C                   EVAL      PTC_PTCE = PTC_PTCB
056900060530     C                   ENDIF
057000060622     C*
057100060622     C* Eseguo riempimento "a 9" del CAP "A"
057200060622     C                   EVAL      wPTCE = %trim(PTC_PTCE)
057300060622     C                   MOVEL     *all'9'       PTC_PTCE
057400060622     C                   MOVEL     wPTCE         PTC_PTCE
057500060622     C*
057600060530     C                   ADD       1             jCapT
057700060530     C                   EVAL      skCapT(jCapT) = DS_PTC
057800060530     C                   ENDIF
057900060529     C*
058000060529     C                   ADD       1             jSks
058100060529     C                   ENDDO
058200060529     C*
058300060529     C                   ENDIF
058400060530     C                   ENDIF
058500060529     C*
058600060530     C* Queste schiere nn devono essere discriminanti ai fini della scrittura x cui se nn
058700060530     C* specificatamente valorizzate le inizializzo a valori d default (=*all)
058800060530     C                   IF        jRulT = *zero
058900060529     C                   ADD       1             jRulT
059000060529     C                   EVAL      ROUP_T = *blanks
059100060529     C                   EVAL      ROUP_C = *all'0'
059200060529     C                   EVAL      skRulT(jRulT) = DS_ROUP
059300060529     C                   ENDIF
059400060530     C                   IF        jCapT = *zero
059500060530     C                   ADD       1             jCapT
059600060530     C                   EVAL      PTC_T     = 'Z'
059700060530     C                   EVAL      PTC_ISO2  = *blanks
059800060530     C                   EVAL      PTC_PTCB  = *all'0'
059900060622     C                   EVAL      PTC_PTCE  = *all'9'
060000060530     C                   EVAL      skCapT(jCapT) = DS_PTC
060100060530     C                   ENDIF
060200060529     C*
060300060529     C                   ENDSR
060400060428     C*------------------------------------------------------------------------*
060500060428     C* SPLIT - Routine di reperimento PORTO e VARIE fatturate
060600060428     C*------------------------------------------------------------------------*
060700060428     C     SPLIT         BEGSR
060800060428     C*
060900060428     C* Innanzitutto inizializzo i parametri d OUT
061000060428     C                   RESET                   lenInSepara
061100060428     C                   RESET                   posDa
061200060428     C                   RESET                   posA
061300060428     C                   RESET                   i
061400060428     C                   RESET                   wSize
061500060428     C*
061600060428     C* Determino subito la lunghezza del "separatore"
061700060428     C                   EVAL      lenInSepara = %len(%trim(InSepara))
061800060428     C**********
061900060428     C* Effettuo lo split della stringa dati d input
062000060428     C**********
062100060428     C*
062200060428     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
062300060428     C                   DOW       posDa <= %len(%trim(InStringa))
062400060428     C*
062500060428     C* Gestisco il 1° elemento
062600060428     C                   IF        i = 1
062700060428     C*
062800060428     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemen
062900060428     C                   EVAL      posDa = 1
063000060428     C*
063100060428     C* Eseguo lo scan x trovare la fine del primo campo
063200060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
063300060428     C                                     posDa)
063400060428     C*
063500060428     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
063600060428     C* l'intero contenuto della stringa è l'unico elemento presente
063700060428     C                   IF        posA = *zeros
063800060428     C                   CLEAR                   SkSplitCSV
063900060428     C                   EVAL      SkSplitCSV(i) = %trim(InStringa)
064000060428     C                   LEAVE
064100060428     C                   ELSE
064200060428     C*
064300060428     C* A questo punto "estraggo" l'elemento corrente
064400060428     C                   IF        posA = 1
064500060428     C                   EVAL      SkSplitCSV(i) = *blanks
064600060428     C                   ELSE
064700060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
064800060428     C                                                    (posA-posDa))
064900060428     C                   ENDIF
065000060428     C                   ENDIF
065100060428     C*
065200060428     C* X gli elementi successivi al 1°
065300060428     C                   ELSE
065400060428     C*
065500060428     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
065600060428     C                   EVAL      posDa = posA + lenInSepara
065700060428     C*
065800060428     C* Verifico che nn vi sia il campo nn valorizzato
065900060428     C                   IF        %subst(InStringa:posDa:lenInSepara) =
066000060428     C                             %trim(InSepara)
066100060428     C*
066200060428     C* Se elemento nn valorizzato skippo al prossimo
066300060428     C                   EVAL      SkSplitCSV(i) = *blanks
066400060428     C                   EVAL      posA  = posA + 1
066500060428     C                   ELSE
066600060428     C*
066700060428     C* Eseguo lo scan x trovare la fine dell'elemento corrente
066800060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
066900060428     C                                     posDa+1)
067000060428     C*
067100060428     C* Gestisco l'ultimo campo
067200060428     C                   IF        posA = *zeros
067300060428     C                   EVAL      posA = %len(%trim(InStringa)) + 1
067400060428     C*
067500060428     C* A questo punto "estraggo" l'elemento corrente
067600060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
067700060428     C                                                    (posA-posDa))
067800060428     C                   LEAVE
067900060428     C                   ELSE
068000060428     C*
068100060428     C* A questo punto "estraggo" l'elemento corrente
068200060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
068300060428     C                                                    (posA-posDa))
068400060428     C                   ENDIF
068500060428     C                   ENDIF
068600060428     C                   ENDIF
068700060428     C*
068800060428     C* Incremento il contatore d campo
068900060428     C                   EVAL      i = i +1
069000060428     C                   ENDDO
069100060428     C*
069200060428     C                   ENDSR
069300060529     C*------------------------------------------------------------------------*
069400060529     C* CARTAB - CARICAMENTO DATI TABELLATI
069500060529     C*------------------------------------------------------------------------*
069600060529     C     cartab        BEGSR
069700060529     C*
069800060529     C* Reperisco tutti i servizi DPD da noi gestiti
069900060529     C                   Z-ADD     *zeros        jSrv
070000060529     C                   CLEAR                   skSrv
070100060529     C                   EVAL      tbeCOD = 'SRV'
070200060529     C     KEYtbe01_P    SETLL     TNTBE01L
070300060529     C                   IF        %found(TNTBE01L)
070400060529     C     KEYtbe01_P    READE     TNTBE01L
070500060529     C                   DOW       not %eof(TNTBE01L)
070600060529     C                   IF        tbeATB = *blanks
070700060529     C                   ADD       1             jSrv
070800060529     C                   MOVEL(P)  tbeKE1        skSrv(jSrv)
070900060529     C                   ENDIF
071000060529     C     KEYtbe01_P    READE     TNTBE01L
071100060529     C                   ENDDO
071200060529     C                   ENDIF
071300060529     C*
071400060529     C                   ENDSR
071500060529     C*------------------------------------------------------------------------*
071600060529     C* CARDPTI - CARICAMENTO ANAGRAFICA DEPOT DPD ITALIA
071700060529     C*------------------------------------------------------------------------*
071800060529     C     cardptI       BEGSR
071900060529     C*
072000060529     C* Carico solo i depot italia (quindi i ns P.O. che servono DPD)
072100060529     C                   Z-ADD     *zeros        jDptI
072200060529     C                   Z-ADD     *zeros        jDptLI
072300060529     C                   CLEAR                   skDptI
072400060529     C                   CLEAR                   skDptLI
072500060529     C                   EVAL      cdpVER  = PARVER
072600060529     C                   EVAL      cdpISO2 = 'IT'
072700060529     C     KEYcdp02_P    SETLL     DPCDP02L
072800060529     C                   IF        %found(DPCDP02L)
072900060529     C     KEYcdp02_P    READE     DPCDP02L
073000060529     C                   DOW       not %eof(DPCDP02L)
073100060529     C                   IF        cdpATB = *blanks
073200060529     C                   ADD       1             jDptI
073300060529     C                   MOVEL(P)  cdpDPC        skDptI(jDptI)
073400060529     C*
073500060529     C* Verifico il raggruppamento del depot
073600060529     C                   IF        cdpGID <> *blanks
073700060529     C*
073800060529     C* Eseguo "splittamento" dei dati
073900060529     C                   EVAL      InStringa = cdpGID
074000060529     C                   EVAL      InSepara  = ','
074100060529     C                   EXSR      SPLIT
074200060529     C*
074300060529     C* Se nn reperiti elementi listo in stampa
074400060529     C                   IF        i = *zeros
074500060529     C                   EVAL      wRiga = 'WFDPCALF' + ' ' + cdpGID
074600060529     C                   EXCEPT    rigaErr
074700060529     C                   ELSE
074800060529     C                   Z-ADD     1             jSk               4 0
074900060529     C                   DOW       jSk <= i
075000060529     C                   ADD       1             jDptLI
075100060529     C                   CLEAR                   DS_GRPD
075200060529     C                   EVAL      GRPD_G = skSplitCSV(jSk)
075300060529     C                   EVAL      GRPD_D = cdpDPC
075400060529     C                   EVAL      skDptLI(jDptLI) = DS_GRPD
075500060529     C                   ADD       1             jSk
075600060529     C                   ENDDO
075700060529     C                   ENDIF
075800060529     C                   ENDIF
075900060529     C*
076000060529     C                   ENDIF
076100060529     C     KEYcdp02_P    READE     DPCDP02L
076200060529     C                   ENDDO
076300060529     C                   ENDIF
076400060529     C*
076500060529     C                   ENDSR
076600060529     C*------------------------------------------------------------------------*
076700060529     C* CARDPT - CARICAMENTO ANAGRAFICA DEPOT DPD
076800060529     C*------------------------------------------------------------------------*
076900060529     C     cardpt        BEGSR
077000060529     C*
077100060529     C* Carico solo i depot italia (quindi i ns P.O. che servono DPD)
077200060529     C                   Z-ADD     *zeros        jDpt
077300060529     C                   Z-ADD     *zeros        jDptL
077400060529     C                   CLEAR                   skDpt
077500060529     C                   CLEAR                   skDptL
077600060529     C                   EVAL      cdpVER  = PARVER
077700060529     C                   EVAL      cdpISO2 = wNaz
077800060529     C     KEYcdp02_P    SETLL     DPCDP02L
077900060529     C                   IF        %found(DPCDP02L)
078000060529     C     KEYcdp02_P    READE     DPCDP02L
078100060529     C                   DOW       not %eof(DPCDP02L)
078200060529     C                   IF        cdpATB = *blanks
078300060529     C                   ADD       1             jDpt
078400060529     C                   MOVEL(P)  cdpDPC        skDpt(jDpt)
078500060529     C*
078600060529     C* Verifico il raggruppamento del depot
078700060529     C                   IF        cdpGID <> *blanks
078800060529     C*
078900060529     C* Eseguo "splittamento" dei dati
079000060529     C                   EVAL      InStringa = cdpGID
079100060529     C                   EVAL      InSepara  = ','
079200060529     C                   EXSR      SPLIT
079300060529     C*
079400060529     C* Se nn reperiti elementi listo in stampa
079500060529     C                   IF        i = *zeros
079600060529     C                   EVAL      wRiga = 'WFDPCALF' + ' ' + cdpGID
079700060529     C                   EXCEPT    rigaErr
079800060529     C                   ELSE
079900060529     C                   Z-ADD     1             jSk               4 0
080000060529     C                   DOW       jSk <= i
080100060529     C                   ADD       1             jDptL
080200060529     C                   CLEAR                   DS_GRPD
080300060529     C                   EVAL      GRPD_G = skSplitCSV(jSk)
080400060529     C                   EVAL      GRPD_D = cdpDPC
080500060529     C                   EVAL      skDptL(jDptL) = DS_GRPD
080600060529     C                   ADD       1             jSk
080700060529     C                   ENDDO
080800060529     C                   ENDIF
080900060529     C*
081000060529     C                   ENDIF
081100060529     C                   ENDIF
081200060529     C     KEYcdp02_P    READE     DPCDP02L
081300060529     C                   ENDDO
081400060529     C                   ENDIF
081500060529     C*
081600060529     C                   ENDSR
081700000000     C*------------------------------------------------------------------------*
081800980403     C* *INZSR - OPERAZIONI INIZIALI
081900000000     C*------------------------------------------------------------------------*
082000980403     C     *INZSR        BEGSR
082100000000     C*
082200000000     C* RICEVIMENTO PARAMETRI
082300000000     C     *ENTRY        PLIST
082400000000     C                   PARM                    KPJBA
082500060427     C                   MOVEL     KPJBU         PARAM
082600060529     C*
082700060529     C* Definizione chiavi di lettura
082800060529     C*
082900060529     C* TNTBE01L - Parziale
083000060529     C     KEYtbe01_P    KLIST
083100060529     C                   KFLD                    tbeCOD
083200060529     C*
083300060529     C* DPCDP02L - Parziale
083400060529     C     KEYcdp02_P    KLIST
083500060529     C                   KFLD                    cdpVER
083600060529     C                   KFLD                    cdpISO2
083700000000     C*
083800000000     C                   ENDSR
083900060427     C*------------------------------------------------------------------------*
084000060427     OPRTF198   E            rigaErr     1
084100060427     O                       wRiga
