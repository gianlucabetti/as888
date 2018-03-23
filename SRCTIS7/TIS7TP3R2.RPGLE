000100170117     H DECEDIT('0.') DATEDIT(*DMY.)
000200170117     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400170117     Ftivgdtmp  if   E             DISK
000500170117     Ftivawwwt  uf a E             DISK    commit
000600090123     Ffnorn02l  IF   E           K DISK
000700150416     Ffnorm01l  IF   E           K DISK
000800170117     Ffnore01l  IF   E           K DISK
000900991027
001000090123     D fnvapds       e ds                  extname(fnvap00t)
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400170117     D pCSV            s              1    inz
001500170117     D pDLM            s              1    inz varying
001600170117     D pFAR            s                   inz like(vapFAR)
001700170117
001800170117     D wDATA           s             10    inz varying
001900170117
002000170117     D iINTI           s               N   inz
002100170117     D iINTD           s               N   inz
002200170117     D iINTE           s               N   inz
002300170117     D iDAT            s               N   inz
002400170117     D iTORM           s              1A   inz
002500170117     D iTFAR           s              1A   inz
002600170117     D iNOT            s               N   inz
002700170117     D iNPR            s               N   inz
002800170117
002900090123
003000991027
003100170118     C*
003200170118     C* Avvio il monitoring del intero flusso
003300170118     C                   monitor
003400170118     C*
003500170118     C* Verifica esistenza record nel membro di output
003600030908     C                   EXSR      chkRECFILMBR
003700170118     C*
003800170118     C* Effettuo traduzione
003900991027     C                   EXSR      traduci
004000170118     C*
004100170118     C* Gestisco eventuale errore
004200170118     C                   on-error
004300170118     C*
004400170118     C* Imposto esito transazione a errore
004500170118     C                   EVAL      wrkesito = '2'
004600170118     C*
004700170118     C* Se errore => emetto operazione d ROLLBACK
004800170118     C                   ROLBK
004900170118     C*
005000170118     C* Arresto il monitoring
005100170118     C                   endmon
005200170118     C*
005300921023     C                   SETON                                        LR
005400170118
005500030908
005600090123
005700030908     C     chkRECFILMBR  BEGSR
005800030908     C*
005900090123     C                   READ      TIVAWWWT                               55
006000030908     C*
006100030908     C                   ENDSR
006200991027
006300170118
006400090123
006500991027     C     traduci       BEGSR
006600170117     C*
006700170117     C* Inizializzazione indicatori di procedure per parametri input
006800170117     C                   eval      iINTI = *off
006900170117     C                   eval      iINTD = *off
007000170117     C                   eval      iINTE = *off
007100170117     C                   eval      iDAT  = *off
007200170117     C                   eval      iTORM = *blanks
007300170117     C                   eval      iTFAR = *blanks
007400170117     C                   eval      iNOT  = *off
007500170117     C                   eval      iNPR  = *off
007600170117     C*
007700170117     C* Se richiesta intestazione
007800170117     C                   select
007900170117     C                   when      %subst(prmppt:1:1) = 'I'
008000170117     C                   eval      iINTI = *on
008100170117     C                   when      %subst(prmppt:1:1) = 'D'
008200170117     C                   eval      iINTD = *on
008300170117     C                   when      %subst(prmppt:1:1) = 'E'
008400170117     C                   eval      iINTE = *on
008500170117     C                   endsl
008600170117     C*
008700170117     C* Verifico se richiesta formatttazione data (con barre)
008800170117     C                   if        %subst(prmppt:2:1) = 'D'
008900170117     C                   eval      iDAT = *on
009000170117     C                   else
009100170117     C                   eval      iDAT = *off
009200170117     C                   endif
009300170117     C*
009400170117     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
009500170117     C                   eval      pCSV = %subst(prmppt:3:1)
009600170117     C                   if        pCSV = *blanks
009700170117     C                   eval      pCSV = ','
009800170117     C                   endif
009900170117     C*
010000170117     C* Verifico se richiesto un particolare carattere d separatore decimale
010100170117     C                   if        %subst(prmppt:4:1) = ','
010200170117     C                   setoff                                       60
010300170117     C                   else
010400170117     C                   seton                                        60
010500170117     C                   endif
010600170117     C*
010700170117     C* Verifico il carattere delimitatore test indicato nei parametri del cliente/tipo file
010800170117     C                   select
010900170117     C                   when      %subst(prmppt:5:1) = *blanks
011000170117     C                   eval      pDLM = '"'
011100170117     C                   when      %subst(prmppt:5:1) = 'N'
011200170117     C                   eval      pDLM = ''
011300170117     C                   other
011400170117     C                   eval      pDLM = %subst(prmppt:5:1)
011500170117     C                   endsl
011600170117     C*
011700170117     C* Se richiesti solamente dati relativi a ORM OK/KO
011800170117     C                   select
011900170117     C                   when      %subst(prmppt:6:1) = 'B'
012000170117     C                   eval      iTORM = 'B'
012100170117     C                   when      %subst(prmppt:6:1) = 'A'
012200170117     C                   eval      iTORM = 'A'
012300170117     C                   other
012400170117     C                   eval      iTORM = *blanks
012500170117     C                   endsl
012600170117     C*
012700170117     C* Se richiesta considerazione su specifica FASE di ritiro
012800170117     C                   if        %subst(prmppt:7:4) <> *blanks
012900170117     C                   select
013000170117     C                   when      %subst(prmppt:7:1) = 'S'
013100170117     C                   eval      iTFAR = 'S'
013200170117     C                   when      %subst(prmppt:7:1) = 'E'
013300170117     C                   eval      iTFAR = 'E'
013400170117     C                   endsl
013500170117     C                   eval      pFAR = %dec(%subst(prmppt:8:3):3:0)
013600170117     C                   endif
013700170117     C*
013800170117     C* Se richiesti dati di estensione relativi alle NOTE
013900170117     C                   if        %subst(prmppt:11:1) = 'S'
014000170117     C                   eval      iNOT = *on
014100170117     C                   endif
014200170117     C*
014300170117     C* Se richiesti dati di estensione relativi al Numero Prenotazione Ritiro
014400170117     C                   if        %subst(prmppt:12:1) = 'S'
014500170117     C                   eval      iNPR = *on
014600170117     C                   endif
014700170117     C*
014800170117     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
014900170117     C                   if        (iINTI OR iINTD OR iINTE) AND *in55 = *on
015000170117     C                   if        iINTI
015100170117     C                   eval      VAWDTA  =
015200170117     C                             pDLM+'VAPIDC'                     +pDLM+pCSV+
015300170117     C                             pDLM+'VAPPOE'                     +pDLM+pCSV+
015400170117     C                             pDLM+'VAPDPE'                     +pDLM+pCSV+
015500170117     C                             pDLM+'VAPNSR'                     +pDLM+pCSV+
015600170117     C                             pDLM+'VAPNOR'                     +pDLM+pCSV+
015700170117     C                             pDLM+'VAPNRV'                     +pDLM+pCSV+
015800170117     C                             pDLM+'VAPRFA'                     +pDLM+pCSV+
015900170117     C                             pDLM+'VAPPOG'                     +pDLM+pCSV+
016000170117     C                             pDLM+'VAPDPG'                     +pDLM+pCSV+
016100170117     C                             pDLM+'VAPDAE'                     +pDLM+pCSV+
016200170117     C                             pDLM+'VAPORE'                     +pDLM+pCSV+
016300170117     C                             pDLM+'VAPFAR'                     +pDLM+pCSV+
016400170117     C                             pDLM+'VAPDFA'                     +pDLM+pCSV+
016500170117     C                             pDLM+'VAPCAR'                     +pDLM+pCSV+
016600170117     C                             pDLM+'VAPDCA'                     +pDLM+pCSV+
016700170117     C                             pDLM+'VAPNOT'                     +pDLM
016800170117     C*
016900170117     C                   if        iNOT
017000170117     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
017100170117     C                             pDLM+'VAPNO1'                     +pDLM+pCSV+
017200170117     C                             pDLM+'VAPNO2'                     +pDLM+pCSV+
017300170117     C                             pDLM+'FARNOT'                     +pDLM
017400170117     C                   endif
017500170117     C*
017600170117     C                   if        iNPR
017700170117     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
017800170117     C                             pDLM+'VAPNPR'                     +pDLM
017900170117     C                   endif
018000170117     C                   endif
018100170117     C*
018200170117     C                   if        iINTD
018300170117     C                   eval      VAWDTA  =
018400170117     C                             pDLM+'IDENTIFICATIVO CLIENTE'     +pDLM+pCSV+
018500170117     C                             pDLM+'FILIALE EMITTENTE'          +pDLM+pCSV+
018600170117     C                             pDLM+'DECODIFICA FIL. EMITTENTE'  +pDLM+pCSV+
018700170117     C                             pDLM+'NUMERO SERIE'               +pDLM+pCSV+
018800170117     C                             pDLM+'NUMERO RITIRO'              +pDLM+pCSV+
018900170117     C                             pDLM+'NUMERO VIAGGIO'             +pDLM+pCSV+
019000170117     C                             pDLM+'RIFERIMENTO ALFANUMERICO'   +pDLM+pCSV+
019100170117     C                             pDLM+'FIL. ESECUZIONE FASE'       +pDLM+pCSV+
019200170117     C                             pDLM+'DESCRIZ. FIL. ESECUZ. FASE' +pDLM+pCSV+
019300170117     C                             pDLM+'DATA ESECUZIONE FASE'       +pDLM+pCSV+
019400170117     C                             pDLM+'ORA VARIAZIONE'             +pDLM+pCSV+
019500170117     C                             pDLM+'CODICE FASE ESEGUITA'       +pDLM+pCSV+
019600170117     C                             pDLM+'DECODIFICA FASE'            +pDLM+pCSV+
019700170117     C                             pDLM+'CAUSALE MANCATO RITIRO'     +pDLM+pCSV+
019800170117     C                             pDLM+'DECODIFICA CAUSALE'         +pDLM+pCSV+
019900170117     C                             pDLM+'NOTE RITIRO'                +pDLM
020000170117     C*
020100170117     C                   if        iNOT
020200170117     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
020300170117     C                             pDLM+'NOTE ORM 1'                 +pDLM+pCSV+
020400170117     C                             pDLM+'NOTE ORM 2'                 +pDLM+pCSV+
020500170117     C                             pDLM+'NOTE FASE'                  +pDLM
020600170117     C                   endif
020700170117     C*
020800170117     C                   if        iNPR
020900170117     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
021000170117     C                             pDLM+'NUM. PRENOTAZIONE RITIRO'   +pDLM
021100170117     C                   endif
021200170117     C                   endif
021300170117     C*
021400170117     C                   if        iINTE
021500170117     C                   eval      VAWDTA  =
021600170117     C                             pDLM+'CUSTOMER USER CODE'         +pDLM+pCSV+
021700170117     C                             pDLM+'ISSUER DEPOT'               +pDLM+pCSV+
021800170117     C                             pDLM+'ISSUER DEPOT DESCRIPTION'   +pDLM+pCSV+
021900170117     C                             pDLM+'SERIES NBR.'                +pDLM+pCSV+
022000170117     C                             pDLM+'PICK-UP NBR.'               +pDLM+pCSV+
022100170117     C                             pDLM+'TRIP NBR.'                  +pDLM+pCSV+
022200170117     C                             pDLM+'SENDER PICK-IP REFERENCE'   +pDLM+pCSV+
022300170117     C                             pDLM+'EXEC STEP DEPOT'            +pDLM+pCSV+
022400170117     C                             pDLM+'EXEC STEP DEPOT DESCRIPTION'+pDLM+pCSV+
022500170117     C                             pDLM+'EXEC STEP DATE'             +pDLM+pCSV+
022600170117     C                             pDLM+'EXEC STEP TIME'             +pDLM+pCSV+
022700170117     C                             pDLM+'EXEC STEP CODE'             +pDLM+pCSV+
022800170117     C                             pDLM+'EXEC STEP DESCRIPTION'      +pDLM+pCSV+
022900170117     C                             pDLM+'PICK-UP FAIL. CAUSE'        +pDLM+pCSV+
023000170117     C                             pDLM+'PICK-UP FAIL. CAUSE DESCR.' +pDLM+pCSV+
023100170117     C                             pDLM+'PICK-UP NOTES'              +pDLM
023200170117     C*
023300170117     C                   if        iNOT
023400170117     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
023500170117     C                             pDLM+'ADD. NOTE 1'                +pDLM+pCSV+
023600170117     C                             pDLM+'ADD. NOTE 2'                +pDLM+pCSV+
023700170117     C                             pDLM+'STEP NOTES'                 +pDLM
023800170117     C                   endif
023900170117     C*
024000170117     C                   if        iNPR
024100170117     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
024200170117     C                             pDLM+'PICK-UP ARRANGEMENT NBR.'   +pDLM
024300170117     C                   endif
024400170117     C                   endif
024500170117     C*
024600170123     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV
024700090123     C                   WRITE     TIVAW000
024800170117     C                   endif
024900991027     C*
025000170117     C* Scorro tutto il file
025100170117     C                   read      tivgdtmp
025200170117     C                   dow       not %eof(tivgdtmp)
025300170119     C*
025400170119     C* Inizializzo buffer output
025500170119     C                   clear                   TIVAW000
025600170118     C*
025700170118     C* Valorizzo dati "correnti"
025800170118     C                   movel     vgddta        FNVAPDS
025900170117     C*
026000170117     C                   setoff                                       40
026100170117     C*
026200170117     C* Verifico eventuali filtri sulla FASE richiesti nei parametri di input
026300170117     C                   if        pFAR <> *zeros
026400170117     C                   if        iTFAR = 'S' and
026500170117     C                             VAPFAR <> pFAR
026600170117     C                   seton                                        40
026700170117     C                   endif
026800170117     C                   if        iTFAR = 'E' and
026900170117     C                             VAPFAR  = pFAR
027000170117     C                   seton                                        40
027100170117     C                   endif
027200170117     C                   endif
027300170117     C*
027400170117     C                   if        iTORM = 'B' and
027500170117     C                             VAPFAR <> 900 and
027600170117     C                             VAPFAR <> 910
027700170117     C                   seton                                        40
027800170117     C                   endif
027900170117     C                   if        iTORM = 'A' and
028000170117     C                             VAPFAR <> 999
028100170117     C                   seton                                        40
028200170117     C                   endif
028300170117     C*
028400170117     C* Restituisco i dati solo se rispondono ai requisiti richiesti nei parametri
028500170117     C                   if        not *in40
028600170117     C*
028700170117     C* Inizializzazioni campi "dati"
028800170117     C                   exsr      inzVAR
028900150416     C*
029000170117     C* Reperisco le note del ORM corrente e FASE corrente
029100170117     C                   if        iNOT
029200150416     C                   exsr      reporm
029300170117     C                   exsr      reporn
029400170117     C                   endif
029500170117     C*
029600170117     C* Reperisco il Numero Prenotazione Ritiro
029700170117     C                   if        iNPR
029800170117     C                   eval      oreTRC = 'NP'
029900170117     C                   exsr      repore
030000170117     C                   endif
030100051007     C*
030200051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
030300170117     C                   if        iDAT
030400170117     C                   eval      wDATA = %trim(%editw(VAPDAE:'    /  /  '))
030500170117     C                   else
030600170117     C                   eval      wDATA = %trim(%editc(VAPDAE:'X'))
030700170117     C                   endif
030800170117     C*
030900170117     C                   EVAL      VAWDTA  =
031000170117     C                             pDLM+           %trim(VAPIDC)     +pDLM+pCSV+
031100170117     C                                      %trim(%editc(VAPPOE:'Q'))     +pCSV+
031200170117     C                             pDLM+           %trim(VAPDPE)     +pDLM+pCSV+
031300170117     C                                      %trim(%editc(VAPNSR:'Q'))     +pCSV+
031400170117     C                                      %trim(%editc(VAPNOR:'Q'))     +pCSV+
031500170117     C                                      %trim(%editc(VAPNRV:'Q'))     +pCSV+
031600170117     C                             pDLM+           %trim(VAPRFA)     +pDLM+pCSV+
031700170117     C                                      %trim(%editc(VAPPOG:'Q'))     +pCSV+
031800170117     C                             pDLM+           %trim(VAPDPG)     +pDLM+pCSV+
031900170117     C                                              %trim(wDATA)          +pCSV+
032000170117     C                                      %trim(%editc(VAPORE:'Q'))     +pCSV+
032100170117     C                             pDLM+    %trim(%editc(VAPFAR:'Q'))+pDLM+pCSV+
032200170117     C                             pDLM+           %trim(VAPDFA)     +pDLM+pCSV+
032300170117     C                             pDLM+           %trim(VAPCAR)     +pDLM+pCSV+
032400170117     C                             pDLM+           %trim(VAPDCA)     +pDLM+pCSV+
032500170117     C                             pDLM+           %trim(VAPNOT)     +pDLM
032600170117     C*
032700170117     C                   if        iNOT
032800170117     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
032900170117     C                             pDLM+           %trim(wNote1)     +pDLM+pCSV+
033000170117     C                             pDLM+           %trim(wNote2)     +pDLM+pCSV+
033100170117     C                             pDLM+           %trim(wNoteFase)  +pDLM
033200170117     C                   endif
033300170117     C*
033400170117     C                   if        iNPR
033500170117     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
033600170117     C                             pDLM+           %trim(wOreDati)   +pDLM
033700170117     C                   endif
033800930409     C*
033900170123     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV
034000090123     C                   WRITE     TIVAW000
034100991027     C*
034200170117     C                   endif
034300170117     C*
034400170117     C                   read      tivgdtmp
034500170117     C                   enddo
034600991027     C*
034700030325     C                   EVAL      wrkesito = '0'
034800991027     C*
034900910830     C                   ENDSR
035000170117
035100170117
035200170117
035300170117     C     inzVAR        BEGSR
035400170117     C*
035500170117     C* Inizializzazioni campi "dati" di wrk
035600170117     C                   clear                   wDATA
035700170117     C*
035800170117     C                   ENDSR
035900150416
036000150416
036100150416
036200150416     C     REPORM        BEGSR
036300150416     C*
036400150416     C                   MOVEL     *blanks       wNote1           35
036500150416     C                   MOVEL     *blanks       wNote2           35
036600150416     C*
036700150416     C* Aggancio il file FNORM x reperire i 2 campi note
036800150416     C     KEYorm1C      CHAIN     fnorm01l
036900150416     C                   IF        %found(fnorm01l)
037000150416     C                   EVAL      wNote1 = ormNO1
037100150416     C                   EVAL      wNote2 = ormNO2
037200150416     C                   ENDIF
037300150416     C*
037400150416     C                   ENDSR
037500170117
037600170117
037700170117
037800170117     C     REPORN        BEGSR
037900170117     C*
038000170117     C                   MOVEL     *blanks       wNoteFase       150
038100170117     C*
038200170117     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
038300170117     C     KEYorn2P      SETLL     fnorn02l
038400170117     C                   IF        %equal(fnorn02l)
038500170117     C*
038600170117     C* Lettura note
038700170117     C     KEYorn2P      READE     fnorn02l
038800170117     C                   DOW       not %eof(fnorn02l) AND
038900170117     C                             %len(%trim(wNoteFase)) <=
039000170117     C                             %size(wNoteFase) - 35
039100170117     C*
039200170117     C* Solo note con data/ora > data/ora esecuzione fase relativa
039300170117     C                   IF        orndai*1000000+ornori >=
039400170117     C                             vapdae*1000000+vapore
039500170117     C*
039600170117     C* Aggiungo al buffer del VAP la nota fase ORM corrente
039700170117     C                   EVAL      wNoteFase = %trim(wNoteFase) + ' ' +
039800170117     C                             %trim(ornnot)
039900170117     C                   ENDIF
040000170117     C*
040100170117     C     KEYorn2P      READE     fnorn02l
040200170117     C                   ENDDO
040300170117     C                   ENDIF
040400170117     C*
040500170117     C                   ENDSR
040600170117
040700170117
040800170117
040900170117     C     REPORE        BEGSR
041000170117     C*
041100170117     C                   MOVEL     *blanks       wOreDati         60
041200170117     C*
041300170117     C* Aggancio il file FNORE per reperire il Numero Prenotazione Ritiro
041400170117     C     KEYore1C      CHAIN     fnore01l
041500170117     C                   IF        %found(fnore01l)
041600170117     C                   EVAL      wOreDati = oreDATI
041700170117     C                   ENDIF
041800170117     C*
041900170117     C                   ENDSR
042000090123
042100090123
042200090123
042300991027     C     *inzsr        BEGSR
042400991027     C*
042500991027     C     *ENTRY        PLIST
042600991027     C                   parm                    prmppt
042700991027     C     wrkesito      parm      wrkesito      prmesito
042800090123     C*
042900090123     C* Definizione chiavi
043000090123     C*
043100090123     C* File: FNORN02L
043200090123     C     KEYorn2P      KLIST
043300090123     C                   KFLD                    VAPPOE
043400090123     C                   KFLD                    VAPNSR
043500090123     C                   KFLD                    VAPNOR
043600090123     C                   KFLD                    VAPNRV
043700090123     C                   KFLD                    VAPFAR
043800150416     C*
043900150416     C* File: FNORM01L
044000150416     C     KEYorm1C      KLIST
044100150416     C                   KFLD                    VAPPOE
044200150416     C                   KFLD                    VAPNSR
044300150416     C                   KFLD                    VAPNOR
044400150416     C                   KFLD                    VAPNRV
044500170117     C*
044600170117     C* File: FNORE01L
044700170117     C     KEYore1C      KLIST
044800170117     C                   KFLD                    VAPPOE
044900170117     C                   KFLD                    VAPNSR
045000170117     C                   KFLD                    VAPNOR
045100170117     C                   KFLD                    VAPNRV
045200170117     C                   KFLD                    ORETRC
045300991027     C*
045400991027     C                   ENDSR
