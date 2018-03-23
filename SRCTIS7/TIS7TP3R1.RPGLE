000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400170117     Ftivgdtmp  if   E             DISK
000500170117     Ffnore01l  IF   E           K DISK
000600170117     FTIVAWWWT  O    E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100170117     D fnvapDS       e ds                  extname(fnvap00t)
001200170117     D fnvapAsciDS   e ds                  extname(fnvapwwt) prefix(A_)
001300170117     D fnvapEbcdDS   e ds                  extname(fnvap1wt) prefix(E_)
001400170119     D l_VP_A          s              4  0 inz(%size(fnvapAsciDS))
001500170119     D l_VP_E          s              4  0 inz(%size(fnvapEbcdDS))
001600170117     D iASCI           s               N   inz
001700170117     D iEBCD           s               N   inz
001800170117     D iTORM           s              1A   inz
001900170117     D iTFAR           s              1A   inz
002000170117     D iNPR            s               N   inz
002100170117     D pFAR            s                   inz like(vapFAR)
002200170117     D wNPR            s             10A   inz
002300991027
002400170117
002500170117
002600170118     C*
002700170118     C* Avvio il monitoring del intero flusso
002800170118     C                   monitor
002900170118     C*
003000170118     C* Effettuo traduzione
003100991027     C                   EXSR      traduci
003200170118     C*
003300170118     C* Gestisco eventuale errore
003400170118     C                   on-error
003500170118     C*
003600170118     C* Imposto esito transazione a errore
003700170118     C                   EVAL      wrkesito = '2'
003800170118     C*
003900170118     C* Se errore => emetto operazione d ROLLBACK
004000170118     C                   ROLBK
004100170118     C*
004200170118     C* Arresto il monitoring
004300170118     C                   endmon
004400170118     C*
004500921023     C                   SETON                                        LR
004600991027
004700170117
004800170117
004900991027     C     traduci       BEGSR
005000170117     C*
005100170117     C* Inizializzazione indicatori di procedure per parametri input
005200170117     C                   eval      iASCI = *off
005300170117     C                   eval      iEBCD = *off
005400170117     C                   eval      iTORM = *blanks
005500170117     C                   eval      iTFAR = *blanks
005600170117     C                   eval      iNPR  = *off
005700170117     C*
005800170117     C* Se richiesta considerazione su specifica FASE di ritiro
005900170117     C                   if        %subst(prmppt:1:4) <> *blanks
006000170117     C                   select
006100170117     C                   when      %subst(prmppt:1:1) = 'S'
006200170117     C                   eval      iTFAR = 'S'
006300170117     C                   when      %subst(prmppt:1:1) = 'E'
006400170117     C                   eval      iTFAR = 'E'
006500170117     C                   endsl
006600170117     C                   eval      pFAR = %dec(%subst(prmppt:2:3):3:0)
006700170117     C                   endif
006800170117     C*
006900170117     C* Se richiesti solamente dati relativi a ORM OK/KO
007000170117     C                   select
007100170117     C                   when      %subst(prmppt:5:1) = 'B'
007200170117     C                   eval      iTORM = 'B'
007300170117     C                   when      %subst(prmppt:5:1) = 'A'
007400170117     C                   eval      iTORM = 'A'
007500170117     C                   other
007600170117     C                   eval      iTORM = *blanks
007700170117     C                   endsl
007800170117     C*
007900170117     C* Se richiesto ritorno dati in formato EBCDIC altrimenti ASCII
008000170117     C                   if        %subst(prmppt:6:1) = 'S'
008100170117     C                   eval      iEBCD = *on
008200170117     C                   else
008300170117     C                   eval      iASCI = *on
008400170117     C                   endif
008500170117     C*
008600170117     C* Se richiesti dati di estensione relativi al Numero Prenotazione Ritiro
008700170117     C                   if        %subst(prmppt:7:1) = 'S'
008800170117     C                   eval      iNPR = *on
008900170117     C                   endif
009000170117     C*
009100170117     C* Scorro tutto il file
009200170117     C                   read      tivgdtmp
009300170117     C                   dow       not %eof(tivgdtmp)
009400170117     C*
009500170117     C* Inizializzo DS di output
009600170119     C                   clear                   TIVAW000
009700170117     C                   clear                   fnvapAsciDS
009800170117     C                   clear                   fnvapEbcdDS
009900170117     C                   clear                   wNPR
010000170117     C*
010100170117     C                   eval      fnvapDS = vgdDTA
010200170117     C*
010300170117     C                   setoff                                       40
010400170117     C*
010500170117     C* Verifico eventuali filtri sulla FASE richiesti nei parametri di input
010600170117     C                   if        pFAR <> *zeros
010700170117     C                   if        iTFAR = 'S' and
010800170117     C                             VAPFAR <> pFAR
010900170117     C                   seton                                        40
011000170117     C                   endif
011100170117     C                   if        iTFAR = 'E' and
011200170117     C                             VAPFAR  = pFAR
011300170117     C                   seton                                        40
011400170117     C                   endif
011500170117     C                   endif
011600170117     C*
011700170117     C                   if        iTORM = 'B' and
011800170117     C                             VAPFAR <> 900 and
011900170117     C                             VAPFAR <> 910
012000170117     C                   seton                                        40
012100170117     C                   endif
012200170117     C                   if        iTORM = 'A' and
012300170117     C                             VAPFAR <> 999
012400170117     C                   seton                                        40
012500170117     C                   endif
012600170117     C*
012700170117     C* Restituisco i dati solo se rispondono ai requisiti richiesti nei parametri
012800170117     C                   if        not *in40
012900170117     C*
013000170117     C* Gestisco formato richiesto ASCII/EBCDIC
013100170117     C                   select
013200170117     C                   when      iASCI
013300170117     C                   exsr      fmtVAP_ASCI
013400170117     C                   when      iEBCD
013500170117     C                   exsr      fmtVAP_EBCD
013600170117     C                   other
013700170117     C                   eval      wrkesito = '2'
013800170117     C                   endsl
013900170117     C*
014000170117     C* Se tutto ok => procedo
014100170117     C                   if        wrkesito = *blanks
014200170117     C*
014300170117     C* Se richiesto Reperisco il Numero Prenotazione Ritiro
014400170117     C                   if        iNPR
014500170117     C                   eval      oreTRC = 'NP'
014600170117     C                   exsr      repore
014700170117     C                   eval      wNPR = %trim(wOreDati)
014800170117     C                   endif
014900170117     C*
015000170117     C* Scarico il buffer di output formato richiesto ASCII/EBCDIC
015100170117     C                   select
015200170117     C                   when      iASCI
015300170117     C                   eval      VAWDTA = fnvapAsciDS
015400170117     C                   when      iEBCD
015500170117     C                   eval      VAWDTA = fnvapEbcdDS
015600170117     C                   endsl
015700170117     C*
015800170117     C                   if        iNPR
015900170117     C                   eval      VAWDTA = %trim(VAWDTA) + wNPR
016000170117     C                   endif
016100930409     C*
016200170117     C                   write     TIVAW000
016300170117     C                   endif
016400120112     C                   endif
016500170117     C*
016600170117     C* Proseguo con il record successivo
016700170117     C                   read      tivgdtmp
016800170117     C                   enddo
016900170117     C*
017000170117     C                   if        wrkesito <> '2'
017100170117     C                   eval      wrkesito  = '0'
017200170117     C                   endif
017300991027     C*
017400910830     C                   ENDSR
017500170117
017600170117
017700170117
017800170117     C*
017900170117     C     fmtVAP_ASCI   BEGSR
018000170117     C*
018100170117     C                   MOVE      vapIDC        A_vapIDC
018200170117     C                   MOVE      vapPOE        A_vapPOE
018300170117     C                   MOVE      vapDPE        A_vapDPE
018400170117     C                   MOVE      vapNSR        A_vapNSR
018500170117     C                   MOVE      vapNOR        A_vapNOR
018600170117     C                   MOVE      vapNRV        A_vapNRV
018700170117     C                   MOVE      vapRFA        A_vapRFA
018800170117     C                   MOVE      vapPOG        A_vapPOG
018900170117     C                   MOVE      vapDPG        A_vapDPG
019000170117     C                   MOVE      vapDAE        A_vapDAE
019100170117     C                   MOVE      vapORE        A_vapORE
019200170117     C                   MOVE      vapFAR        A_vapFAR
019300170117     C                   MOVE      vapDFA        A_vapDFA
019400170117     C                   MOVE      vapCAR        A_vapCAR
019500170117     C                   MOVE      vapDCA        A_vapDCA
019600170117     C                   MOVE      vapNOT        A_vapNOT
019700170117     C*
019800170117     C* A fine campo forzo il carattere "." a fine campo x delimitare esattamente la fine del record
019900170117     C                   MOVE      '.'           A_vapNOT
020000170117     C*
020100170117     C                   ENDSR
020200170117
020300170117
020400170117
020500170117     C*
020600170117     C     fmtVAP_EBCD   BEGSR
020700170117     C*
020800170117     C                   MOVE      vapIDC        E_vapIDC
020900170117     C                   MOVE      vapPOE        E_vapPOE
021000170117     C                   MOVE      vapDPE        E_vapDPE
021100170117     C                   MOVE      vapNSR        E_vapNSR
021200170117     C                   MOVE      vapNOR        E_vapNOR
021300170117     C                   MOVE      vapNRV        E_vapNRV
021400170117     C                   MOVE      vapRFA        E_vapRFA
021500170117     C                   MOVE      vapPOG        E_vapPOG
021600170117     C                   MOVE      vapDPG        E_vapDPG
021700170117     C                   MOVE      vapDAE        E_vapDAE
021800170117     C                   MOVE      vapORE        E_vapORE
021900170117     C                   MOVE      vapFAR        E_vapFAR
022000170117     C                   MOVE      vapDFA        E_vapDFA
022100170117     C                   MOVE      vapCAR        E_vapCAR
022200170117     C                   MOVE      vapDCA        E_vapDCA
022300170117     C                   MOVE      vapNOT        E_vapNOT
022400170117     C*
022500170117     C* A fine campo forzo il carattere "." a fine campo x delimitare esattamente la fine del record
022600170117     C                   MOVE      '.'           E_vapNOT
022700170117     C*
022800170117     C                   ENDSR
022900170117
023000170117
023100170117
023200170117     C     REPORE        BEGSR
023300170117     C*
023400170117     C                   MOVEL     *blanks       wOreDati         60
023500170117     C*
023600170117     C* Aggancio il file FNORE per reperire il Numero Prenotazione Ritiro
023700170117     C     KEYore1C      CHAIN     fnore01l
023800170117     C                   IF        %found(fnore01l)
023900170117     C                   EVAL      wOreDati = oreDATI
024000170117     C                   ENDIF
024100170117     C*
024200170117     C                   ENDSR
024300170117
024400170117
024500991027
024600991027      /TITLE Operazioni iniziali.
024700991027     C     *inzsr        BEGSR
024800991027     C*
024900991027     C     *ENTRY        PLIST
025000991027     C                   parm                    prmppt
025100991027     C     wrkesito      parm      wrkesito      prmesito
025200991027     C*
025300170117     C* File: FNORE01L
025400170117     C     KEYore1C      KLIST
025500170117     C                   KFLD                    VAPPOE
025600170117     C                   KFLD                    VAPNSR
025700170117     C                   KFLD                    VAPNOR
025800170117     C                   KFLD                    VAPNRV
025900170117     C                   KFLD                    ORETRC
026000170117     C*
026100991027     C                   ENDSR
