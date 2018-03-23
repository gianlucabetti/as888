000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300171016     H DECEDIT('0,') DATEDIT(*YMD.)
000400990908
000500160801     Ftabel00f  if   e           k disk
000600171102     FWFDPDRDBF uF   E             DISK    usropn
000700171016     FDPCAS11I  uF a E           k DISK           commit
000800171016     FDPCBU11I  uF a E           k DISK           commit
000900171016     FDPCCC11I  uF a E           k DISK           commit
001000171016     FDPCDP11I  uF a E           k DISK           commit
001100171016     FDPCEL11I  uF a E           k DISK           commit
001200171016     FDPCIC11I  uF a E           k DISK           commit
001300171016     FDPCNP11I  uF a E           k DISK           commit
001400171016     FDPCNT11I  uF a E           k DISK           commit
001500171016     FDPCPC11I  uF a E           k DISK           commit
001600171016     FDPCRE11I  uF a E           k DISK           commit
001700171016     FDPCSO11I  uF a E           k DISK           commit
001800171016     FDPCST11I  uF a E           k DISK           commit
001900171016     FDPCVE11I  uF a E           k DISK           commit
002000171016     FDPCWA11I  uF a E           k DISK           commit
002100171016     FDPCWS11I  uF a E           k DISK           commit
002200171016     FDPCR111I  uF a E           k DISK           commit
002300171016     FDPCR211I  uF a E           k DISK           commit
002400171016     FDPCR311I  uF a E           k DISK           commit
002500171016     FDPCR411I  uF a E           k DISK           commit
002600171016     FDPCR511I  uF a E           k DISK           commit
002700170616
002800990908
002900000801     D*----------------------------------------------------
003000000801     D* DICHIARAZIOINE VARIABILI DI WRK
003100000801     D*----------------------------------------------------
003200160801     D ds15          e ds
003300170621     D wWDPDRDB        s                   LIKE(WDPDRDB) INZ
003400170616     D MAX_VERSION     s                   LIKE(CVEVER) INZ
003500170616     D NEW_VERSION     s                   LIKE(CVEVER) INZ
003600171016     D Decor_DAY       s                   LIKE(CVEDDE) INZ
003700171016     D Scade_DAY       s                   LIKE(CVEDSC) INZ
003800171016     D DATA_OGGI       s              8  0 INZ
003900170616     D SkSplitFLD      S            128    DIM(512) INZ
004000170616     D currMAP         S            128    VARYING INZ
004100170622     D currRCD         S              9S 0 INZ
004200170621     D wDESxx          S            128    VARYING INZ
004300170621     D wCSTSRTC        S                   LIKE(CSTSRTC)  INZ
004400170621     D w2048_01        S           2048    VARYING INZ
004500170621     D w2048_02        S           2048    VARYING INZ
004600170621     D w2048_03        S           2048    VARYING INZ
004700170621     D w2048_04        S           2048    VARYING INZ
004800170621     D sOutIdxMax_w1   S                   LIKE(OutIdxMax) INZ
004900170621     D sOutIdxMax_w2   S                   LIKE(OutIdxMax) INZ
005000170621     D sOutIdxMax_w3   S                   LIKE(OutIdxMax) INZ
005100170621     D sOutIdxMax_w4   S                   LIKE(OutIdxMax) INZ
005200170621     D wSORC           S              3    INZ
005300170621     D wSkCSV_03_w1    S             64    DIM(1024) INZ
005400170621     D wSkCSV_03_w2    S             64    DIM(1024) INZ
005500170621     D wSkCSV_03_w3    S             64    DIM(1024) INZ
005600170621     D wSkCSV_03_w4    S             64    DIM(1024) INZ
005700170616
005800170616     D*------------
005900170616     D skDEFKEY        S             64    DIM(128)
006000170620     D DS_DEF          DS
006100170620     D   skDEF                       64    DIM(128)
006200170620     D skDEFS          S                   DIM(128) LIKE(DS_DEF)
006300170616     D jDEFS           S              3  0 INZ
006400160606
006500160801     D*------------
006600170620     D jNAZ            s              4  0 INZ(*zeros)
006700170620     D skNAZISO        S              2    DIM(1000)
006800160801     D skNAZBAR        S              3    DIM(1000)
006900160606
007000171024     d dataiso         s               d   datfmt(*iso)
007100141028
007200141028     D*------------------
007300141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
007400141028     D*------------------
007500170621     D CharCSV         S              1    INZ(';')
007600170621     D CharNUM         S              1    INZ(',')
007700141028
007800141028
007900141028     D*------------------
008000141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
008100141028     D*------------------
008200170616     D InStringa       S         131072A   VARYING                              (stringa input)
008300170616     D InSepara        S             10A                                        (separatore)
008400170616     D InTypeOut       S              2A                                        (tipo output)
008500170616     D wSkParam        S         131072A                                        (output)
008600170620     D OutIdxMax       S              4S 0                                      (max indice valoriz)
008700170616     D OutErrore       S              1A                                        (flag errore)
008800170616     D OutMsg          S             80A                                        (messaggio errore)
008900170620     D SkSplitCSV_3    S             64    DIM(1024)
009000170616     D SkSplitCSV_4    S            128    DIM(512)
009100170616     D SkSplitCSV_9    S           2048    DIM(64)
009200990908
009300141028
009400141028     D*------------------
009500141028     D* LINKING A DEFINIZIONI ESTERNE
009600141028     D*------------------
009700141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009800141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009900141028
010000171016     c     *entry        PLIST
010100171016     c                   parm                    ErrFlag           1
010200171016     c                   parm                    ErrMsg           70
010300171016
010400171016     c                   clear                   ErrFlag
010500171016     c                   clear                   ErrMsg
010600171016
010700171016     C                   seton                                        LR
010800171016     C*  AAAAMMGG
010900171016     c                   eval      DATA_OGGI = *date
011000171016     C*
011100160801     C                   EXSR      CARTAB                                       CARICA TABELLE
011200170616     C                   EXSR      RWFILE                                       ELABORAZIONE
011300171016
011400171016      ** x nessun problema riscontrato CONFERMA le scritture
011500171016     c                   if        ErrFlag = *blank
011600171016     c                   COMMIT
011700171016      **
011800171016      * x Errore esegue il rollback
011900171016     c                   elseif    ErrFlag = '1'
012000171016     c                   ROLBK
012100171016     c                   end
012200171016      **
012300171016     c                   RETURN
012400171016     C*----------------------------------------------------*
012500171016      **   Legge il WORKFILE (TXT) e lo importa
012600171016     C*----------------------------------------------------*
012700040526     C     RWFILE        BEGSR
012800170620     C*
012900170621     C                   if        not %open(WFDPDRDBF)
013000170621     C                   open      WFDPDRDBF
013100170620     C                   endif
013200150127     C*
013300170621     C                   READ      WFDPDRDBF
013400170621     C                   DOW       not %eof(WFDPDRDBF)
013500000801     C*
013600170621     C                   if        WDPDRDB <> *blanks
013700141028     C*
013800170616     C* "normalizzo" il dato di input corrente
013900170621     C                   eval      wWDPDRDB = %trim(%subst(WDPDRDB:1:
014000170621     C                                        %len(%trim(WDPDRDB))-1))
014100170622     C*
014200170622     C* Verifico rischio dati troncati
014300170622     C                   add       1             currRCD
014400170622     C                   if        %subst(WDPDRDB:1000) <> *blanks
014500170622     C                   exsr      sndAlert
014600170622     C                   endif
014700040910     C*
014800170616     C* Eseguo routine di traduzione
014900170616     C                   exsr      exeTrad
015000171016      *
015100171016      **  per qualsiasi tipo di errore ROLLBACK e torna al chiamante
015200171016     c                   if        ErrFlag ='1'
015300171016     C                   LEAVE
015400171016     C                   endif
015500040802     C*
015600000905     C                   endif
015700000905     C*
015800170621     C***                DELETE    WFDPDRDB0
015900170621     C                   READ      WFDPDRDBF
016000170621     C                   ENDDO
016100170620     C*
016200170621     C                   if        %open(WFDPDRDBF)
016300170621     C                   close     WFDPDRDBF
016400170620     C                   endif
016500990920     C*
016600910830     C                   ENDSR
016700000613     C***
016800141028     C*----------------------------------------------------*
016900141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017000141028     C*----------------------------------------------------*
017100141028     C     INZVAR        BEGSR
017200141028     C*
017300141028     C* Inizializzo variabili di wrk
017400170620     C                   SETOFF                                       131419
017500170620     C                   CLEAR                   SkSplitCSV_3
017600170616     C                   CLEAR                   SkSplitCSV_4
017700170616     C                   CLEAR                   SkSplitCSV_9
017800170621     C                   CLEAR                   wDESxx
017900170620     C                   CLEAR                   wCSTSRTC
018000170620     C                   CLEAR                   w2048_01
018100170621     C                   CLEAR                   w2048_02
018200170621     C                   CLEAR                   w2048_03
018300170621     C                   CLEAR                   w2048_04
018400141028     C*
018500141028     C                   ENDSR
018600170616     C***
018700170616
018800000801     C*----------------------------------------------------*
018900170616     C*  TRADUZIONE DATI
019000000801     C*----------------------------------------------------*
019100170616     C     EXETRAD       BEGSR
019200170616     C*
019300170616     C* Inizializzazioni a liverro di singolo record
019400170616     C                   EXSR      INZVAR
019500170616     C*
019600170616     C* Ignoro i record relativi al "file"
019700170616     C                   SELECT
019800170621     C                   WHEN      %subst(wWDPDRDB:1:1)  = '#' AND
019900170621     C                             %subst(wWDPDRDB:1:4) <> '#DEF'
020000170616     C*
020100170616     C* Carico innanzitutto tutti i record di DEFinizione
020200170621     C                   WHEN      %subst(wWDPDRDB:1:16) = '#DEF:GEOROUTING:'
020300170621     C                   EVAL      InStringa = %subst(wWDPDRDB:16+1)
020400170616     C                   EVAL      InSepara = CharCSV
020500170620     C                   SETOFF                                       131419
020600170616     C                   SETON                                        14
020700170616     C                   EXSR      SR_SPLIT
020800170616     C                   EVAL      skDEF = %subarr(SkSplitCSV_4:1)
020900170616     C                   ADD       1             jDEFS
021000170616     C                   EVAL      skDEFKEY(jDEFS) = skDEF(1)
021100170620     C                   EVAL      skDEFS(jDEFS)   = DS_DEF
021200170620     C*
021300170616     C* Elaboro i record di dati
021400170616     C                   OTHER
021500170616     C*
021600170616     C* Splitto il record di dati corrrente
021700170621     C                   EVAL      InStringa = %trim(wWDPDRDB)
021800170616     C                   EVAL      InSepara = CharCSV
021900170620     C                   SETOFF                                       131419
022000170616     C                   SETON                                        19
022100170616     C                   EXSR      SR_SPLIT
022200170616     C*
022300170616     C* Ogni record di dati lo "mappo" tramite la propria struttura dati
022400170616     C                   EXSR      MAPPADATI
022500170616     C*
022600170616     C                   ENDSL
022700170616     C*
022800170616     C                   ENDSR
022900170616     C***
023000170616
023100170616
023200170616     C*----------------------------------------------------*
023300170616     C*  GESTIONE MAPPATURE DATI
023400170616     C*----------------------------------------------------*
023500170616     C     MAPPADATI     BEGSR
023600170616     C*
023700170616     C* Verifico che il tipo record dati corrente risulti già mappato
023800170616     C                   EVAL      currMAP = %trim(SkSplitCSV_9(1))
023900170616     C                   EVAL      jDEFS = %lookup(currMAP:skDEFKEY:1)
024000170616     C*
024100170616     C* Se tipo record dati corrente già mappato => carico relativa mappatura
024200170616     C                   IF        jDEFS > *zeros
024300170620     C                   EVAL      DS_DEF = skDEFS(jDEFS)
024400170616     C                   EVAL      SkSplitFLD = %subarr(skDEF:1)
024500170616     C*
024600170616     C* Rispetto ad ogni tipo record dati => gestisco con relativa mappatura
024700170616     C                   SELECT
024800170616     C                   WHEN      currMAP ='VERSION'
024900170616     C                   EXSR      gesCVE
025000170616     C*
025100170616     C                   WHEN      currMAP ='COUNTRY'
025200170616     C                   EXSR      gesCCC
025300170616     C*
025400170620     C***                WHEN      currMAP ='COUNTRYTRANSLATION'
025500170619     C***                EXSR      gesXXX
025600170616     C*
025700170616     C                   WHEN      currMAP ='COUNTRY.IT'
025800170616     C                   EXSR      gesCCC_IT
025900170616     C*
026000170616     C                   WHEN      currMAP ='COUNTRY.DE'
026100170616     C                   EXSR      gesCCC_DE
026200170616     C*
026300170616     C                   WHEN      currMAP ='COUNTRY.FR'
026400170616     C                   EXSR      gesCCC_FR
026500170616     C*
026600170616     C                   WHEN      currMAP ='COUNTRY.EN'
026700170616     C                   EXSR      gesCCC_EN
026800170616     C*
026900170616     C                   WHEN      currMAP ='BUSINESSUNIT'
027000170616     C                   EXSR      gesCBU
027100170616     C*
027200170616     C                   WHEN      currMAP ='POSTALCODE'
027300170616     C                   EXSR      gesCPC
027400170616     C*
027500170620     C***                WHEN      currMAP ='STATE'
027600170619     C***                EXSR      gesCST
027700170616     C*
027800170620     C***                WHEN      currMAP ='STATE.IT'
027900170619     C***                EXSR      gesCST_IT
028000170616     C*
028100170620     C***                WHEN      currMAP ='STATE.DE'
028200170619     C***                EXSR      gesCST_DE
028300170616     C*
028400170620     C***                WHEN      currMAP ='STATE.FR'
028500170619     C***                EXSR      gesCST_FR
028600170616     C*
028700170620     C***                WHEN      currMAP ='STATE.EN'
028800170619     C***                EXSR      gesCST_EN
028900170616     C*
029000170616     C                   WHEN      currMAP ='DEPOTS'
029100170619     C                   EXSR      gesCDP
029200170616     C*
029300170620     C***                WHEN      currMAP ='ELEMENTS'
029400170619     C***                EXSR      gesCEL
029500170616     C*
029600170620     C***                WHEN      currMAP ='ELEMENTS.IT'
029700170619     C***                EXSR      gesCEL_IT
029800170616     C*
029900170620     C***                WHEN      currMAP ='ELEMENTS.EN'
030000170619     C***                EXSR      gesCEL_EN
030100170616     C*
030200170620     C***                WHEN      currMAP ='ELEMENTS.DE'
030300170619     C***                EXSR      gesCEL_DE
030400170616     C*
030500170620     C***                WHEN      currMAP ='ELEMENTS.FR'
030600170619     C***                EXSR      gesCEL_FR
030700170616     C*
030800170620     C***                WHEN      currMAP ='ELEMENTS.PT'
030900170619     C***                EXSR      gesCEL_PT
031000170619     C*
031100170616     C                   WHEN      currMAP ='SOCODE'
031200170616     C                   EXSR      gesCSO
031300170616     C*
031400170616     C                   WHEN      currMAP ='ASCODE'
031500170619     C                   EXSR      gesCAS
031600170616     C*
031700170616     C                   WHEN      currMAP ='IC'
031800170619     C                   EXSR      gesCIC
031900170616     C*
032000170616     C                   WHEN      currMAP ='NETWORK'
032100170619     C                   EXSR      gesCNT
032200170616     C*
032300170616     C                   WHEN      currMAP ='NETWORKPLAN'
032400170616     C                   EXSR      gesCNP
032500170616     C*
032600170620     C***                WHEN      currMAP ='RCODE'
032700170620     C***                EXSR      gesXXX
032800170616     C*
032900170616     C                   WHEN      currMAP ='ALLOWSO'
033000170619     C                   EXSR      gesCWS
033100170616     C*
033200170616     C                   WHEN      currMAP ='ALLOWAS'
033300170619     C                   EXSR      gesCWA
033400170619     C*
033500170619     C                   WHEN      currMAP ='R1DBU'
033600170619     C                   EXSR      gesCR1
033700170619     C*
033800170619     C                   WHEN      currMAP ='R2ROUTE'
033900170619     C                   EXSR      gesCR2
034000170619     C*
034100170619     C                   WHEN      currMAP ='R3OSORT'
034200170619     C                   EXSR      gesCR3
034300170619     C*
034400170619     C                   WHEN      currMAP ='R4CSORT'
034500170620     C                   EXSR      gesCR4
034600170619     C*
034700170619     C                   WHEN      currMAP ='R5DSORT'
034800170619     C                   EXSR      gesCR5
034900170616     C*
035000170620     C***                WHEN      currMAP ='DEPARTURE'
035100170619     C***                EXSR      gesXXX
035200170616     C*
035300170616     C                   WHEN      currMAP ='RELATION'
035400170619     C                   EXSR      gesCRE
035500170616     C*
035600170616     C                   WHEN      currMAP ='SOSERVICEINFO.IT'
035700170616     C                   EXSR      gesCSO_IT
035800170616     C*
035900170616     C                   WHEN      currMAP ='SOSERVICEINFO.EN'
036000170616     C                   EXSR      gesCSO_EN
036100170616     C*
036200170616     C                   WHEN      currMAP ='SOSERVICEINFO.DE'
036300170616     C                   EXSR      gesCSO_DE
036400170616     C*
036500170616     C                   WHEN      currMAP ='SOSERVICEINFO.FR'
036600170616     C                   EXSR      gesCSO_FR
036700170616     C*
036800170616     C                   WHEN      currMAP ='SOSERVICEINFO.ET'
036900170616     C                   EXSR      gesCSO_ET
037000170616     C*
037100170616     C                   WHEN      currMAP ='SOSERVICEINFO.CS'
037200170616     C                   EXSR      gesCSO_CS
037300170616     C*
037400170616     C                   ENDSL
037500170616     C*
037600170616     C                   ENDIF
037700170616     C*
037800170616     C                   ENDSR
037900170616     C***
038000170616
038100170616
038200170616     C*----------------------------------------------------*
038300170616     C*  GESTIONE MAPPATURA "VERSION"
038400170616     C*----------------------------------------------------*
038500170616     C     GESCVE        BEGSR
038600170616     C*
038700170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
038800170616     C                   CLEAR                   DPCVE100
038900170616     C*
039000170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
039100170616     C                   Z-ADD     2             i                 4 0
039200170620     C                   DOW       i <= %elem(SkSplitFLD)
039300170620     C                             AND i <= OutIdxMax
039400170616     C*
039500170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
039600170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
039700170616     C                             SkSplitCSV_9(i) <> *blanks
039800170616     C*
039900170616     C*** VERD
040000170616     C                   IF        %trim(SkSplitFLD(i)) = 'VERSIONNUMBER'
040100170616     C                   EVAL      CVEVERD = SkSplitCSV_9(i)
040200170616     C                   ENDIF
040300170616     C*** DDE
040400170616     C                   IF        %trim(SkSplitFLD(i)) = 'VALIDFROM'
040500170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
040600170616     C                   EXSR      CHKNUM
040700170616     C                   IF        PiInt=*on
040800170616     C                   Z-ADD     PiVal         CVEDDE
040900170616     C                   ELSE
041000170616     C                   dump(A)
041100170616     C                   return
041200170616     C                   ENDIF
041300170616     C                   ENDIF
041400171016     C*** DSC
041500171016     C                   IF        %trim(SkSplitFLD(i)) = 'VALIDTO'
041600171016     C                   EVAL      PiStr = SkSplitCSV_9(i)
041700171016     C                   EXSR      CHKNUM
041800171016     C                   IF        PiInt=*on
041900171016     C                   Z-ADD     PiVal         CVEDSC
042000171016     C                   ELSE
042100171016     C                   dump(A)
042200171016     C                   return
042300171016     C                   ENDIF
042400171016     C                   ENDIF
042500170929      *
042600171016      *  SE IL CAPPARIO che sta caricando ha una DATA SCADENZA antecedente
042700171016      *   alla DATA di OGGI - Blocca TUTTO
042800171016      *  Esce con ERRORE eseguendo un ROLLBACK di ciò che aveva fatto.
042900171017     c                   If        cveDSC < data_OGGI and cveDSC >0
043000171016     c                   eval         ErrFlag = '1'
043100171016     c                   eval         ErrMsg  = 'Data Scadenza Cappario -
043200171016     c                             ANTECEDENTE oggi. Cappario NON TRADUCIBILE!!'
043300171016     c                   leaveSR
043400171016     c                   End
043500171016     C*
043600170929     C*
043700170616     C*** BCID
043800170616     C                   IF        %trim(SkSplitFLD(i)) = 'BARCODEID'
043900170627     C                   EVAL      CVEBCID = SkSplitCSV_9(i)
044000170616     C                   ENDIF
044100170616     C*
044200170616     C                   ENDIF
044300170616     C                   EVAL      i = i + 1
044400170616     C                   ENDDO
044500171017      *------------
044600171017     C*** DSC
044700171017     C*** Forzo la data di scadenza al 20391231
044800171017     C                   EVAL      CVEDSC = 20391231
044900170616     C*
045000170616     C*** DIM
045100170616     C                   EVAL      CVEDIM = oggi
045200170616     C*
045300170616     C* Reperisco la versione più alta corrente
045400170616     C*
045500170616     C/EXEC SQL
045600170616     C+ SET :MAX_VERSION = (SELECT MAX(CVEVER) FROM DPCVE10F)
045700170616     C/END-EXEC
045800170616     C*
045900170616     C* Stacco il nuovo numero versione BRT come incremento della versione più alta presente
046000170616     C                   EVAL      NEW_VERSION = MAX_VERSION + 1
046100170616     C*
046200170616     C* Effetttuo considerazioni si "sicurezza" sulle date di validità della versione DPD
046300170616     C                   IF        CVEDDE < domani
046400170929     C                   EVAL      CVEDDE = domani
046500171024     C                   END
046600171024     C*
046700171024     C*  pulisce la data Scadenza
046800171024     C                   clear                   DataScad          8 0
046900171024     C                   move      CVEDDE        DataIso
047000171024     c                   subdur    1:*d          DataIso
047100171024     C                   move      DataIso       DataScad
047200170616     C*
047300170616     C* Faccio scadere a oggi tutte le versioni aventi data scadenza
047400170616     C* nel range di validità della versione DPD corrente
047500170616     C*
047600170616     C/EXEC SQL
047700171024     C+ UPDATE DPCVE10F SET CVEDSC = :DataScad
047800170616     C+ WHERE CVEDSC BETWEEN :CVEDDE AND :CVEDSC
047900170616     C/END-EXEC
048000170616     C*
048100170620     C* Quindi lancio driver di "allineamento validità versioni DPD GeoRDB 2017"
048200170616     C*
048300170620     C                   CALL      'TISID3R1'
048400170620     C*
048500170620     C*** VER
048600170620     C                   EVAL      CVEVER = NEW_VERSION
048700170928     C*
048800170616     C* Infine inserisco il record corrente
048900170616     C                   WRITE     DPCVE100
049000170616     C*
049100170616     C                   ENDSR
049200170616     C***
049300170616
049400170616
049500170616     C*----------------------------------------------------*
049600170616     C*  GESTIONE MAPPATURA "COUNTRY"
049700170616     C*----------------------------------------------------*
049800170616     C     GESCCC        BEGSR
049900170616     C*
050000170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
050100170616     C                   CLEAR                   DPCCC100
050200170616     C*
050300170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
050400170616     C                   Z-ADD     2             i                 4 0
050500170620     C                   DOW       i <= %elem(SkSplitFLD)
050600170620     C                             AND i <= OutIdxMax
050700170616     C*
050800170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
050900170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
051000170616     C                             SkSplitCSV_9(i) <> *blanks
051100170616     C*
051200170616     C*** ISO2
051300170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
051400170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
051500170616     C                   ENDIF
051600170616     C*** ISON
051700170616     C                   IF        %trim(SkSplitFLD(i)) = 'ISONUMCOUNTRYCODE'
051800170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
051900170616     C                   EXSR      CHKNUM
052000170616     C                   IF        PiInt=*on
052100170616     C                   Z-ADD     PiVal         CCCISON
052200170616     C                   ENDIF
052300170616     C                   ENDIF
052400170616     C*** ISO3
052500170616     C                   IF        %trim(SkSplitFLD(i)) = 'ISOALPHA3COUNTRYCODE'
052600170616     C                   EVAL      CCCISO3 = SkSplitCSV_9(i)
052700170616     C                   ENDIF
052800170629     C*** PSYS
052900170616     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGPOSTCODENO'
053000170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
053100170616     C                   EXSR      CHKNUM
053200170616     C                   IF        PiInt=*on
053300170629     C                   Z-ADD     PiVal         CCCPSYS
053400170616     C                   ENDIF
053500170616     C                   ENDIF
053600170629     C*** PATTL
053700170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODEPATTERN'
053800170620     C                   EVAL      CCCPATTL = SkSplitCSV_9(i)
053900170616     C                   ENDIF
054000170616     C*** FNS
054100170616     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGNEEDSTATE'
054200170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
054300170616     C                   EXSR      CHKNUM
054400170616     C                   IF        PiInt=*on
054500170616     C                   Z-ADD     PiVal         CCCFNS
054600170616     C                   ENDIF
054700170616     C                   ENDIF
054800170616     C*** PVAL
054900170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODEVALIDATION'
055000170616     C                   EVAL      CCCPVAL = SkSplitCSV_9(i)
055100170616     C                   ENDIF
055200170616     C*** CVAL
055300170616     C                   IF        %trim(SkSplitFLD(i)) = 'CITYVALIDATION'
055400170616     C                   EVAL      CCCCVAL = SkSplitCSV_9(i)
055500170616     C                   ENDIF
055600170616     C*** FREE
055700170616     C                   IF        %trim(SkSplitFLD(i)) = 'FREETRADEAREA'
055800170616     C                   EVAL      CCCFREE = SkSplitCSV_9(i)
055900170616     C                   ENDIF
056000170616     C*** DUMY
056100170616     C                   IF        %trim(SkSplitFLD(i)) = 'DUMMYCOUNTRYCODE'
056200170616     C                   EVAL      CCCDUMY = SkSplitCSV_9(i)
056300170616     C                   ENDIF
056400170616     C*
056500170616     C                   ENDIF
056600170616     C                   EVAL      i = i + 1
056700170616     C                   ENDDO
056800170616     C*
056900170616     C*** VER
057000170616     C                   EVAL      CCCVER = NEW_VERSION
057100170616     C*** BRT
057200170620     C                   Z-ADD     1             jNAZ
057300170620     C     CCCISO2       LOOKUP    skNAZISO(jNAZ)                         55
057400170616     C                   IF        %found
057500170616     C                   EVAL      CCCBRT = skNAZBAR(jNAZ)
057600170622     C                   ELSE
057700170622     C                   EVAL      CCCBRT = *all'*'
057800170616     C                   ENDIF
057900170616     C*
058000170616     C* Infine inserisco il record corrente
058100170616     C                   WRITE     DPCCC100
058200170616     C*
058300170616     C                   ENDSR
058400170616     C***
058500170616
058600170616
058700170616     C*----------------------------------------------------*
058800170616     C*  GESTIONE MAPPATURA "COUNTRY.IT"
058900170616     C*----------------------------------------------------*
059000170616     C     GESCCC_IT     BEGSR
059100170616     C*
059200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
059300170616     C                   Z-ADD     2             i                 4 0
059400170620     C                   DOW       i <= %elem(SkSplitFLD)
059500170620     C                             AND i <= OutIdxMax
059600170616     C*
059700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
059800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
059900170616     C                             SkSplitCSV_9(i) <> *blanks
060000170616     C*
060100170616     C*** ISO2
060200170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
060300170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
060400170616     C                   ENDIF
060500170616     C*** NAME
060600170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
060700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
060800170616     C                   ENDIF
060900170616     C*
061000170616     C                   ENDIF
061100170616     C                   EVAL      i = i + 1
061200170616     C                   ENDDO
061300170616     C*
061400170616     C*** ATB
061500170616     C                   EVAL      CCCATB = *blanks
061600170616     C*** VER
061700170616     C                   EVAL      CCCVER = NEW_VERSION
061800170616     C*
061900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
062000170621     C                   IF        wDESxx <> *blanks
062100170616     C     KEYccc_11C    CHAIN     dpccc11i
062200170616     C                   IF        %found(dpccc11i)
062300170621     C                   EVAL      CCCDESIT = wDESxx
062400170616     C                   UPDATE    DPCCC100
062500170616     C                   ENDIF
062600170620     C                   ENDIF
062700170616     C*
062800170616     C                   ENDSR
062900170616     C***
063000170616
063100170616
063200170616     C*----------------------------------------------------*
063300170616     C*  GESTIONE MAPPATURA "COUNTRY.DE"
063400170616     C*----------------------------------------------------*
063500170616     C     GESCCC_DE     BEGSR
063600170616     C*
063700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
063800170616     C                   Z-ADD     2             i                 4 0
063900170620     C                   DOW       i <= %elem(SkSplitFLD)
064000170620     C                             AND i <= OutIdxMax
064100170616     C*
064200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
064300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
064400170616     C                             SkSplitCSV_9(i) <> *blanks
064500170616     C*
064600170616     C*** ISO2
064700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
064800170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
064900170616     C                   ENDIF
065000170616     C*** NAME
065100170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
065200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
065300170616     C                   ENDIF
065400170616     C*
065500170616     C                   ENDIF
065600170616     C                   EVAL      i = i + 1
065700170616     C                   ENDDO
065800170616     C*
065900170616     C*** ATB
066000170616     C                   EVAL      CCCATB = *blanks
066100170616     C*** VER
066200170616     C                   EVAL      CCCVER = NEW_VERSION
066300170616     C*
066400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
066500170621     C                   IF        wDESxx <> *blanks
066600170616     C     KEYccc_11C    CHAIN     dpccc11i
066700170616     C                   IF        %found(dpccc11i)
066800170621     C                   EVAL      CCCDESDE = wDESxx
066900170616     C                   UPDATE    DPCCC100
067000170620     C                   ENDIF
067100170616     C                   ENDIF
067200170616     C*
067300170616     C                   ENDSR
067400170616     C***
067500170616
067600170616
067700170616     C*----------------------------------------------------*
067800170616     C*  GESTIONE MAPPATURA "COUNTRY.FR"
067900170616     C*----------------------------------------------------*
068000170616     C     GESCCC_FR     BEGSR
068100170616     C*
068200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
068300170616     C                   Z-ADD     2             i                 4 0
068400170620     C                   DOW       i <= %elem(SkSplitFLD)
068500170620     C                             AND i <= OutIdxMax
068600170616     C*
068700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
068800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
068900170616     C                             SkSplitCSV_9(i) <> *blanks
069000170616     C*
069100170616     C*** ISO2
069200170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
069300170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
069400170616     C                   ENDIF
069500170616     C*** NAME
069600170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
069700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
069800170616     C                   ENDIF
069900170616     C*
070000170616     C                   ENDIF
070100170616     C                   EVAL      i = i + 1
070200170616     C                   ENDDO
070300170616     C*
070400170616     C*** ATB
070500170616     C                   EVAL      CCCATB = *blanks
070600170616     C*** VER
070700170616     C                   EVAL      CCCVER = NEW_VERSION
070800170616     C*
070900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
071000170621     C                   IF        wDESxx <> *blanks
071100170616     C     KEYccc_11C    CHAIN     dpccc11i
071200170616     C                   IF        %found(dpccc11i)
071300170621     C                   EVAL      CCCDESFR = wDESxx
071400170616     C                   UPDATE    DPCCC100
071500170616     C                   ENDIF
071600170620     C                   ENDIF
071700170616     C*
071800170616     C                   ENDSR
071900170616     C***
072000170616
072100170616
072200170616     C*----------------------------------------------------*
072300170616     C*  GESTIONE MAPPATURA "COUNTRY.EN"
072400170616     C*----------------------------------------------------*
072500170616     C     GESCCC_EN     BEGSR
072600170616     C*
072700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
072800170616     C                   Z-ADD     2             i                 4 0
072900170620     C                   DOW       i <= %elem(SkSplitFLD)
073000170620     C                             AND i <= OutIdxMax
073100170616     C*
073200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
073300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
073400170616     C                             SkSplitCSV_9(i) <> *blanks
073500170616     C*
073600170616     C*** ISO2
073700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
073800170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
073900170616     C                   ENDIF
074000170616     C*** NAME
074100170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
074200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
074300170616     C                   ENDIF
074400170616     C*
074500170616     C                   ENDIF
074600170616     C                   EVAL      i = i + 1
074700170616     C                   ENDDO
074800170616     C*
074900170616     C*** ATB
075000170616     C                   EVAL      CCCATB = *blanks
075100170616     C*** VER
075200170616     C                   EVAL      CCCVER = NEW_VERSION
075300170616     C*
075400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
075500170621     C                   IF        wDESxx <> *blanks
075600170616     C     KEYccc_11C    CHAIN     dpccc11i
075700170616     C                   IF        %found(dpccc11i)
075800170621     C                   EVAL      CCCDESEN = wDESxx
075900170616     C                   UPDATE    DPCCC100
076000170616     C                   ENDIF
076100170620     C                   ENDIF
076200170616     C*
076300170616     C                   ENDSR
076400170616     C***
076500170616
076600170616
076700170616     C*----------------------------------------------------*
076800170616     C*  GESTIONE MAPPATURA "BUSINESSUNIT"
076900170616     C*----------------------------------------------------*
077000170616     C     GESCBU        BEGSR
077100170616     C*
077200170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
077300170616     C                   CLEAR                   DPCBU100
077400170616     C*
077500170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
077600170616     C                   Z-ADD     2             i                 4 0
077700170620     C                   DOW       i <= %elem(SkSplitFLD)
077800170620     C                             AND i <= OutIdxMax
077900170616     C*
078000170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
078100170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
078200170616     C                             SkSplitCSV_9(i) <> *blanks
078300170616     C*
078400170616     C*** BUCA
078500170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUALPHASTR'
078600170616     C                   EVAL      CBUBUCA = SkSplitCSV_9(i)
078700170616     C                   ENDIF
078800170616     C*** BUCN
078900170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
079000170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
079100170616     C                   EXSR      CHKNUM
079200170616     C                   IF        PiInt=*on
079300170616     C                   Z-ADD     PiVal         CBUBUCN
079400170616     C                   ENDIF
079500170616     C                   ENDIF
079600170616     C*** NAME
079700170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUNAME'
079800170616     C                   EVAL      CBUNAME = SkSplitCSV_9(i)
079900170616     C                   ENDIF
080000170616     C*** ISO2
080100170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUDEFAULTCOUNTRY'
080200170616     C                   EVAL      CBUISO2 = SkSplitCSV_9(i)
080300170616     C                   ENDIF
080400170616     C*** TYPE
080500170616     C                   IF        %trim(SkSplitFLD(i)) = 'TYPEOFBU'
080600170616     C                   EVAL      CBUTYPE = SkSplitCSV_9(i)
080700170616     C                   ENDIF
080800170616     C*** GRPF
080900170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUFINANCIALGROUP'
081000170616     C                   EVAL      CBUGRPF = SkSplitCSV_9(i)
081100170616     C                   ENDIF
081200170616     C*** GRPG
081300170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUGEOGRAPHGROUP'
081400170616     C                   EVAL      CBUGRPG = SkSplitCSV_9(i)
081500170616     C                   ENDIF
081600170616     C*** GRPT
081700170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUTECHNICALGROUP'
081800170616     C                   EVAL      CBUGRPT = SkSplitCSV_9(i)
081900170616     C                   ENDIF
082000170616     C*** DPDC
082100170616     C                   IF        %trim(SkSplitFLD(i)) = 'DPDCONTRACT'
082200170616     C                   EVAL      CBUDPDC = SkSplitCSV_9(i)
082300170616     C                   ENDIF
082400170616     C*
082500170616     C                   ENDIF
082600170616     C                   EVAL      i = i + 1
082700170616     C                   ENDDO
082800170616     C*
082900170616     C*** VER
083000170616     C                   EVAL      CBUVER = NEW_VERSION
083100170616     C*
083200170616     C* Infine inserisco il record corrente
083300170616     C                   WRITE     DPCBU100
083400170616     C*
083500170616     C                   ENDSR
083600170616     C***
083700170616
083800170616
083900170616     C*----------------------------------------------------*
084000170616     C*  GESTIONE MAPPATURA "POSTALCODE"
084100170616     C*----------------------------------------------------*
084200170616     C     GESCPC        BEGSR
084300170616     C*
084400170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
084500170616     C                   CLEAR                   DPCPC100
084600170616     C*
084700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
084800170616     C                   Z-ADD     2             i                 4 0
084900170620     C                   DOW       i <= %elem(SkSplitFLD)
085000170620     C                             AND i <= OutIdxMax
085100170616     C*
085200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
085300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
085400170616     C                             SkSplitCSV_9(i) <> *blanks
085500170616     C*
085600170616     C*** ISO2
085700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
085800170616     C                   EVAL      CPCISO2 = SkSplitCSV_9(i)
085900170616     C                   ENDIF
086000170616     C*** PTCB
086100170616     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
086200170616     C                   EVAL      CPCPTCB = SkSplitCSV_9(i)
086300170616     C                   ENDIF
086400170616     C*** PTCE
086500170616     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
086600170616     C                   EVAL      CPCPTCE = SkSplitCSV_9(i)
086700170616     C                   ENDIF
086800170616     C*** PTCT
086900170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODETYPE'
087000170620     C                   EVAL      CPCPTCT = SkSplitCSV_9(i)
087100170616     C                   ENDIF
087200170616     C*
087300170616     C                   ENDIF
087400170616     C                   EVAL      i = i + 1
087500170616     C                   ENDDO
087600170619     C*
087700170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
087800170619     C                   IF        CPCPTCE = *blanks
087900170619     C                   EVAL      CPCPTCE = CPCPTCB
088000170619     C                   ENDIF
088100170616     C*
088200170616     C*** VER
088300170616     C                   EVAL      CPCVER = NEW_VERSION
088400170616     C*
088500170616     C* Infine inserisco il record corrente
088600170616     C                   WRITE     DPCPC100
088700170616     C*
088800170616     C                   ENDSR
088900170616     C***
089000170616
089100170616
089200170616     C*----------------------------------------------------*
089300170616     C*  GESTIONE MAPPATURA "STATE"
089400170616     C*----------------------------------------------------*
089500170616     C     GESCST        BEGSR
089600170616     C*
089700170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
089800170616     C                   CLEAR                   DPCST100
089900170616     C*
090000170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090100170616     C                   Z-ADD     2             i                 4 0
090200170620     C                   DOW       i <= %elem(SkSplitFLD)
090300170620     C                             AND i <= OutIdxMax
090400170616     C*
090500170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090600170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
090700170616     C                             SkSplitCSV_9(i) <> *blanks
090800170616     C*
090900170616     C*** ISO2
091000170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
091100170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
091200170616     C                   ENDIF
091300170616     C*** REGC
091400170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODE'
091500170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
091600170616     C                   ENDIF
091700170616     C*** STAC
091800170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
091900170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
092000170616     C                   ENDIF
092100170616     C*
092200170616     C                   ENDIF
092300170616     C                   EVAL      i = i + 1
092400170616     C                   ENDDO
092500170616     C*
092600170616     C*** VER
092700170616     C                   EVAL      CSTVER = NEW_VERSION
092800170616     C*
092900170616     C* Infine inserisco il record corrente
093000170616     C                   WRITE     DPCST100
093100170616     C*
093200170616     C                   ENDSR
093300170616     C***
093400170616
093500170616
093600170616     C*----------------------------------------------------*
093700170616     C*  GESTIONE MAPPATURA "STATE.IT"
093800170616     C*----------------------------------------------------*
093900170616     C     GESCST_IT     BEGSR
094000170616     C*
094100170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094200170616     C                   Z-ADD     2             i                 4 0
094300170620     C                   DOW       i <= %elem(SkSplitFLD)
094400170620     C                             AND i <= OutIdxMax
094500170616     C*
094600170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094700170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
094800170616     C                             SkSplitCSV_9(i) <> *blanks
094900170616     C*
095000170616     C*** ISO2
095100170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
095200170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
095300170616     C                   ENDIF
095400170616     C*** REGC
095500170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
095600170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
095700170616     C                   ENDIF
095800170616     C*** STAC
095900170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
096000170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
096100170616     C                   ENDIF
096200170616     C*** SRTC
096300170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
096400170616     C                   ENDIF
096500170616     C*** DESxx
096600170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
096700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
096800170616     C                   ENDIF
096900170616     C*
097000170616     C                   ENDIF
097100170616     C                   EVAL      i = i + 1
097200170616     C                   ENDDO
097300170616     C*
097400170616     C*** ATB
097500170616     C                   EVAL      CSTATB = *blanks
097600170616     C*** VER
097700170616     C                   EVAL      CSTVER = NEW_VERSION
097800170616     C*
097900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
098000170621     C                   IF        wDESxx <> *blanks
098100170616     C     KEYcst_11C    CHAIN     dpCST11i
098200170616     C                   IF        %found(dpCST11i)
098300170621     C                   EVAL      CSTDESIT = wDESxx
098400170616     C                   UPDATE    DPCST100
098500170616     C                   ENDIF
098600170620     C                   ENDIF
098700170616     C*
098800170616     C                   ENDSR
098900170616     C***
099000170616
099100170616
099200170616     C*----------------------------------------------------*
099300170616     C*  GESTIONE MAPPATURA "STATE.DE"
099400170616     C*----------------------------------------------------*
099500170616     C     GESCST_DE     BEGSR
099600170616     C*
099700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099800170616     C                   Z-ADD     2             i                 4 0
099900170620     C                   DOW       i <= %elem(SkSplitFLD)
100000170620     C                             AND i <= OutIdxMax
100100170616     C*
100200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
100400170616     C                             SkSplitCSV_9(i) <> *blanks
100500170616     C*
100600170616     C*** ISO2
100700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
100800170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
100900170616     C                   ENDIF
101000170616     C*** REGC
101100170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
101200170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
101300170616     C                   ENDIF
101400170616     C*** STAC
101500170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
101600170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
101700170616     C                   ENDIF
101800170616     C*** SRTC
101900170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
102000170616     C                   ENDIF
102100170616     C*** DESxx
102200170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
102300170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
102400170616     C                   ENDIF
102500170616     C*
102600170616     C                   ENDIF
102700170616     C                   EVAL      i = i + 1
102800170616     C                   ENDDO
102900170616     C*
103000170616     C*** ATB
103100170616     C                   EVAL      CSTATB = *blanks
103200170616     C*** VER
103300170616     C                   EVAL      CSTVER = NEW_VERSION
103400170616     C*
103500170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
103600170621     C                   IF        wDESxx <> *blanks
103700170616     C     KEYcst_11C    CHAIN     dpCST11i
103800170616     C                   IF        %found(dpCST11i)
103900170621     C                   EVAL      CSTDESDE = wDESxx
104000170616     C                   UPDATE    DPCST100
104100170616     C                   ENDIF
104200170620     C                   ENDIF
104300170616     C*
104400170616     C                   ENDSR
104500170616     C***
104600170616
104700170616
104800170616     C*----------------------------------------------------*
104900170616     C*  GESTIONE MAPPATURA "STATE.FR"
105000170616     C*----------------------------------------------------*
105100170616     C     GESCST_FR     BEGSR
105200170616     C*
105300170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
105400170616     C                   Z-ADD     2             i                 4 0
105500170620     C                   DOW       i <= %elem(SkSplitFLD)
105600170620     C                             AND i <= OutIdxMax
105700170616     C*
105800170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
105900170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
106000170616     C                             SkSplitCSV_9(i) <> *blanks
106100170616     C*
106200170616     C*** ISO2
106300170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
106400170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
106500170616     C                   ENDIF
106600170616     C*** REGC
106700170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
106800170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
106900170616     C                   ENDIF
107000170616     C*** STAC
107100170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
107200170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
107300170616     C                   ENDIF
107400170616     C*** SRTC
107500170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
107600170616     C                   ENDIF
107700170616     C*** DESxx
107800170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
107900170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
108000170616     C                   ENDIF
108100170616     C*
108200170616     C                   ENDIF
108300170616     C                   EVAL      i = i + 1
108400170616     C                   ENDDO
108500170616     C*
108600170616     C*** ATB
108700170616     C                   EVAL      CSTATB = *blanks
108800170616     C*** VER
108900170616     C                   EVAL      CSTVER = NEW_VERSION
109000170616     C*
109100170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
109200170621     C                   IF        wDESxx <> *blanks
109300170616     C     KEYcst_11C    CHAIN     dpCST11i
109400170616     C                   IF        %found(dpCST11i)
109500170621     C                   EVAL      CSTDESFR = wDESxx
109600170616     C                   UPDATE    DPCST100
109700170620     C                   ENDIF
109800170616     C                   ENDIF
109900170616     C*
110000170616     C                   ENDSR
110100170616     C***
110200170616
110300170616
110400170616     C*----------------------------------------------------*
110500170616     C*  GESTIONE MAPPATURA "STATE.EN"
110600170616     C*----------------------------------------------------*
110700170616     C     GESCST_EN     BEGSR
110800170616     C*
110900170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
111000170616     C                   Z-ADD     2             i                 4 0
111100170620     C                   DOW       i <= %elem(SkSplitFLD)
111200170620     C                             AND i <= OutIdxMax
111300170616     C*
111400170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
111500170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
111600170616     C                             SkSplitCSV_9(i) <> *blanks
111700170616     C*
111800170616     C*** ISO2
111900170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
112000170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
112100170616     C                   ENDIF
112200170616     C*** REGC
112300170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
112400170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
112500170616     C                   ENDIF
112600170616     C*** STAC
112700170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
112800170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
112900170616     C                   ENDIF
113000170616     C*** SRTC
113100170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
113200170616     C                   ENDIF
113300170616     C*** DESxx
113400170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
113500170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
113600170616     C                   ENDIF
113700170616     C*
113800170616     C                   ENDIF
113900170616     C                   EVAL      i = i + 1
114000170616     C                   ENDDO
114100170616     C*
114200170616     C*** ATB
114300170616     C                   EVAL      CSTATB = *blanks
114400170616     C*** VER
114500170616     C                   EVAL      CSTVER = NEW_VERSION
114600170616     C*
114700170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
114800170621     C                   IF        wDESxx <> *blanks
114900170616     C     KEYcst_11C    CHAIN     dpCST11i
115000170616     C                   IF        %found(dpCST11i)
115100170621     C                   EVAL      CSTDESEN = wDESxx
115200170616     C                   UPDATE    DPCST100
115300170620     C                   ENDIF
115400170616     C                   ENDIF
115500170616     C*
115600170616     C                   ENDSR
115700170616     C***
115800170616
115900170616
116000170616     C*----------------------------------------------------*
116100170616     C*  GESTIONE MAPPATURA "ELEMENTS"
116200170616     C*----------------------------------------------------*
116300170616     C     GESCEL        BEGSR
116400170616     C*
116500170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
116600170616     C                   CLEAR                   DPCEL100
116700170616     C*
116800170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
116900170616     C                   Z-ADD     2             i                 4 0
117000170620     C                   DOW       i <= %elem(SkSplitFLD)
117100170620     C                             AND i <= OutIdxMax
117200170616     C*
117300170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
117400170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
117500170616     C                             SkSplitCSV_9(i) <> *blanks
117600170616     C*
117700170620     C*** ELMC
117800170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
117900170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
118000170620     C                   EXSR      CHKNUM
118100170620     C                   IF        PiInt=*on
118200170620     C                   Z-ADD     PiVal         CELELMC
118300170620     C                   ENDIF
118400170620     C                   ENDIF
118500170616     C*** SRTC
118600170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTSHORTCUT'
118700170616     C                   EVAL      CELSRTC = SkSplitCSV_9(i)
118800170616     C                   ENDIF
118900170616     C*** ELMT
119000170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTTYPE'
119100170616     C                   EVAL      CELELMT = SkSplitCSV_9(i)
119200170616     C                   ENDIF
119300170616     C*
119400170616     C                   ENDIF
119500170616     C                   EVAL      i = i + 1
119600170616     C                   ENDDO
119700170616     C*
119800170616     C*** VER
119900170616     C                   EVAL      CELVER = NEW_VERSION
120000170616     C*
120100170616     C* Infine inserisco il record corrente
120200170616     C                   WRITE     DPCEL100
120300170616     C*
120400170616     C                   ENDSR
120500170616     C***
120600170616
120700170616
120800170616     C*----------------------------------------------------*
120900170616     C*  GESTIONE MAPPATURA "ELEMENTS.IT"
121000170616     C*----------------------------------------------------*
121100170616     C     GESCEL_IT     BEGSR
121200170616     C*
121300170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
121400170616     C                   Z-ADD     2             i                 4 0
121500170620     C                   DOW       i <= %elem(SkSplitFLD)
121600170620     C                             AND i <= OutIdxMax
121700170616     C*
121800170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
121900170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
122000170616     C                             SkSplitCSV_9(i) <> *blanks
122100170616     C*
122200170620     C*** ELMC
122300170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
122400170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
122500170620     C                   EXSR      CHKNUM
122600170620     C                   IF        PiInt=*on
122700170620     C                   Z-ADD     PiVal         CELELMC
122800170620     C                   ENDIF
122900170620     C                   ENDIF
123000170616     C*** DESxx
123100170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
123200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
123300170616     C                   ENDIF
123400170616     C*
123500170616     C                   ENDIF
123600170616     C                   EVAL      i = i + 1
123700170616     C                   ENDDO
123800170616     C*
123900170616     C*** ATB
124000170616     C                   EVAL      CELATB = *blanks
124100170616     C*** VER
124200170616     C                   EVAL      CELVER = NEW_VERSION
124300170616     C*
124400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
124500170621     C                   IF        wDESxx <> *blanks
124600170616     C     KEYcel_11C    CHAIN     dpCEL11i
124700170616     C                   IF        %found(dpCEL11i)
124800170621     C                   EVAL      CELDESIT = wDESxx
124900170616     C                   UPDATE    DPCEL100
125000170620     C                   ENDIF
125100170616     C                   ENDIF
125200170616     C*
125300170616     C                   ENDSR
125400170616     C***
125500170616
125600170616
125700170616     C*----------------------------------------------------*
125800170616     C*  GESTIONE MAPPATURA "ELEMENTS.DE"
125900170616     C*----------------------------------------------------*
126000170616     C     GESCEL_DE     BEGSR
126100170616     C*
126200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
126300170616     C                   Z-ADD     2             i                 4 0
126400170620     C                   DOW       i <= %elem(SkSplitFLD)
126500170620     C                             AND i <= OutIdxMax
126600170616     C*
126700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
126800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
126900170616     C                             SkSplitCSV_9(i) <> *blanks
127000170616     C*
127100170620     C*** ELMC
127200170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
127300170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
127400170620     C                   EXSR      CHKNUM
127500170620     C                   IF        PiInt=*on
127600170620     C                   Z-ADD     PiVal         CELELMC
127700170620     C                   ENDIF
127800170620     C                   ENDIF
127900170616     C*** DESxx
128000170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
128100170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
128200170616     C                   ENDIF
128300170616     C*
128400170616     C                   ENDIF
128500170616     C                   EVAL      i = i + 1
128600170616     C                   ENDDO
128700170616     C*
128800170616     C*** ATB
128900170616     C                   EVAL      CELATB = *blanks
129000170616     C*** VER
129100170616     C                   EVAL      CELVER = NEW_VERSION
129200170616     C*
129300170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
129400170621     C                   IF        wDESxx <> *blanks
129500170616     C     KEYcel_11C    CHAIN     dpCEL11i
129600170616     C                   IF        %found(dpCEL11i)
129700170621     C                   EVAL      CELDESDE = wDESxx
129800170616     C                   UPDATE    DPCEL100
129900170620     C                   ENDIF
130000170616     C                   ENDIF
130100170616     C*
130200170616     C                   ENDSR
130300170616     C***
130400170616
130500170616
130600170616     C*----------------------------------------------------*
130700170616     C*  GESTIONE MAPPATURA "ELEMENTS.FR"
130800170616     C*----------------------------------------------------*
130900170616     C     GESCEL_FR     BEGSR
131000170616     C*
131100170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
131200170616     C                   Z-ADD     2             i                 4 0
131300170620     C                   DOW       i <= %elem(SkSplitFLD)
131400170620     C                             AND i <= OutIdxMax
131500170616     C*
131600170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
131700170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
131800170616     C                             SkSplitCSV_9(i) <> *blanks
131900170616     C*
132000170620     C*** ELMC
132100170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
132200170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
132300170620     C                   EXSR      CHKNUM
132400170620     C                   IF        PiInt=*on
132500170620     C                   Z-ADD     PiVal         CELELMC
132600170620     C                   ENDIF
132700170620     C                   ENDIF
132800170616     C*** DESxx
132900170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
133000170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
133100170616     C                   ENDIF
133200170616     C*
133300170616     C                   ENDIF
133400170616     C                   EVAL      i = i + 1
133500170616     C                   ENDDO
133600170616     C*
133700170616     C*** ATB
133800170616     C                   EVAL      CELATB = *blanks
133900170616     C*** VER
134000170616     C                   EVAL      CELVER = NEW_VERSION
134100170616     C*
134200170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
134300170621     C                   IF        wDESxx <> *blanks
134400170616     C     KEYcel_11C    CHAIN     dpCEL11i
134500170616     C                   IF        %found(dpCEL11i)
134600170621     C                   EVAL      CELDESFR = wDESxx
134700170616     C                   UPDATE    DPCEL100
134800170620     C                   ENDIF
134900170616     C                   ENDIF
135000170616     C*
135100170616     C                   ENDSR
135200170616     C***
135300170616
135400170616
135500170616     C*----------------------------------------------------*
135600170616     C*  GESTIONE MAPPATURA "ELEMENTS.EN"
135700170616     C*----------------------------------------------------*
135800170616     C     GESCEL_EN     BEGSR
135900170616     C*
136000170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
136100170616     C                   Z-ADD     2             i                 4 0
136200170620     C                   DOW       i <= %elem(SkSplitFLD)
136300170620     C                             AND i <= OutIdxMax
136400170616     C*
136500170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
136600170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
136700170616     C                             SkSplitCSV_9(i) <> *blanks
136800170616     C*
136900170620     C*** ELMC
137000170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
137100170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
137200170620     C                   EXSR      CHKNUM
137300170620     C                   IF        PiInt=*on
137400170620     C                   Z-ADD     PiVal         CELELMC
137500170620     C                   ENDIF
137600170620     C                   ENDIF
137700170616     C*** DESxx
137800170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
137900170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
138000170616     C                   ENDIF
138100170616     C*
138200170616     C                   ENDIF
138300170616     C                   EVAL      i = i + 1
138400170616     C                   ENDDO
138500170616     C*
138600170616     C*** ATB
138700170616     C                   EVAL      CELATB = *blanks
138800170616     C*** VER
138900170616     C                   EVAL      CELVER = NEW_VERSION
139000170616     C*
139100170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
139200170621     C                   IF        wDESxx <> *blanks
139300170616     C     KEYcel_11C    CHAIN     dpCEL11i
139400170616     C                   IF        %found(dpCEL11i)
139500170621     C                   EVAL      CELDESEN = wDESxx
139600170616     C                   UPDATE    DPCEL100
139700170620     C                   ENDIF
139800170616     C                   ENDIF
139900170616     C*
140000170616     C                   ENDSR
140100170616     C***
140200170616
140300170616
140400170616     C*----------------------------------------------------*
140500170616     C*  GESTIONE MAPPATURA "ELEMENTS.PT"
140600170616     C*----------------------------------------------------*
140700170616     C     GESCEL_PT     BEGSR
140800170616     C*
140900170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
141000170616     C                   Z-ADD     2             i                 4 0
141100170620     C                   DOW       i <= %elem(SkSplitFLD)
141200170620     C                             AND i <= OutIdxMax
141300170616     C*
141400170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
141500170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
141600170616     C                             SkSplitCSV_9(i) <> *blanks
141700170616     C*
141800170620     C*** ELMC
141900170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
142000170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
142100170620     C                   EXSR      CHKNUM
142200170620     C                   IF        PiInt=*on
142300170620     C                   Z-ADD     PiVal         CELELMC
142400170620     C                   ENDIF
142500170620     C                   ENDIF
142600170616     C*** DESxx
142700170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
142800170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
142900170616     C                   ENDIF
143000170616     C*
143100170616     C                   ENDIF
143200170616     C                   EVAL      i = i + 1
143300170616     C                   ENDDO
143400170616     C*
143500170616     C*** ATB
143600170616     C                   EVAL      CELATB = *blanks
143700170616     C*** VER
143800170616     C                   EVAL      CELVER = NEW_VERSION
143900170616     C*
144000170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
144100170621     C                   IF        wDESxx <> *blanks
144200170616     C     KEYcel_11C    CHAIN     dpCEL11i
144300170616     C                   IF        %found(dpCEL11i)
144400170621     C                   EVAL      CELDESPT = wDESxx
144500170616     C                   UPDATE    DPCEL100
144600170620     C                   ENDIF
144700170616     C                   ENDIF
144800170616     C*
144900170616     C                   ENDSR
145000170619     C***
145100170619
145200170619
145300170619     C*----------------------------------------------------*
145400170619     C*  GESTIONE MAPPATURA "SOCODE"
145500170619     C*----------------------------------------------------*
145600170619     C     GESCSO        BEGSR
145700170619     C*
145800170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
145900170619     C                   CLEAR                   DPCSO100
146000170619     C*
146100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
146200170619     C                   Z-ADD     2             i                 4 0
146300170620     C                   DOW       i <= %elem(SkSplitFLD)
146400170620     C                             AND i <= OutIdxMax
146500170619     C*
146600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
146700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
146800170619     C                             SkSplitCSV_9(i) <> *blanks
146900170619     C*
147000170619     C*** SORC
147100170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
147200170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
147300170619     C                   EXSR      CHKNUM
147400170619     C                   IF        PiInt=*on
147500170619     C                   Z-ADD     PiVal         CSOSORC
147600170619     C                   ENDIF
147700170619     C                   ENDIF
147800170619     C*** STXT
147900170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICETEXT'
148000170620     C                   EVAL      CSOSTXT  = SkSplitCSV_9(i)
148100170619     C                   ENDIF
148200170619     C*** SMRK
148300170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICEMARK'
148400170620     C                   EVAL      CSOSMRK  = SkSplitCSV_9(i)
148500170619     C                   ENDIF
148600170620     C*** ELMC
148700170619     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODES'
148800170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
148900170619     C                   ENDIF
149000170619     C*** ASCO
149100170619     C                   IF        %trim(SkSplitFLD(i)) = 'ASCODENEEDED'
149200170620     C                   EVAL      CSOASCO  = SkSplitCSV_9(i)
149300170619     C                   ENDIF
149400170619     C*
149500170619     C                   ENDIF
149600170619     C                   EVAL      i = i + 1
149700170619     C                   ENDDO
149800170619     C*
149900170619     C*** VER
150000170619     C                   EVAL      CSOVER = NEW_VERSION
150100170620     C*
150200170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
150300170620     C                   EVAL      InStringa = w2048_01
150400170620     C                   EVAL      InSepara = ','
150500170620     C                   SETOFF                                       131419
150600170620     C                   SETON                                        13
150700170620     C                   EXSR      SR_SPLIT
150800170620     C*
150900170620     C* Se nessun elemento => scrivo comunque un record singolo
151000170620     C                   IF        OutIdxMax = *zeros
151100170620     C                   WRITE     DPCSO100
151200170620     C                   ELSE
151300170620     C*
151400170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
151500170620     C                   Z-ADD     1             j                 4 0
151600170620     C                   DOW       j <= %elem(SkSplitCSV_3)
151700170620     C                             AND j <= OutIdxMax
151800170620     C*** ELMC
151900170620     C                   EVAL      PiStr = SkSplitCSV_3(j)
152000170620     C                   EXSR      CHKNUM
152100170620     C                   IF        PiInt=*on
152200170620     C                   Z-ADD     PiVal         CSOELMC
152300170620     C                   ENDIF
152400170620     C*
152500170620     C* Infine inserisco il record corrente
152600170620     C                   WRITE     DPCSO100
152700170620     C*
152800170620     C                   EVAL      j = j + 1
152900170620     C                   ENDDO
153000170620     C                   ENDIF
153100170619     C*
153200170619     C                   ENDSR
153300170619     C***
153400170619
153500170619
153600170619     C*----------------------------------------------------*
153700170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.IT"
153800170619     C*----------------------------------------------------*
153900170620     C     GESCSO_IT     BEGSR
154000170619     C*
154100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
154200170619     C                   Z-ADD     2             i                 4 0
154300170620     C                   DOW       i <= %elem(SkSplitFLD)
154400170620     C                             AND i <= OutIdxMax
154500170620     C*
154600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
154700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
154800170619     C                             SkSplitCSV_9(i) <> *blanks
154900170619     C*
155000170620     C*** SORC
155100170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
155200170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
155300170620     C                   EXSR      CHKNUM
155400170620     C                   IF        PiInt=*on
155500170620     C                   Z-ADD     PiVal         CSOSORC
155600170620     C                   ENDIF
155700170620     C                   ENDIF
155800170619     C*** DESxx
155900170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
156000170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
156100170619     C                   ENDIF
156200170619     C*
156300170619     C                   ENDIF
156400170620     C                   EVAL      i = i + 1
156500170619     C                   ENDDO
156600170619     C*
156700170619     C*** ATB
156800170619     C                   EVAL      CSOATB = *blanks
156900170619     C*** VER
157000170619     C                   EVAL      CSOVER = NEW_VERSION
157100170619     C*
157200170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
157300170621     C                   IF        wDESxx <> *blanks
157400170620     C     KEYcso_11C    CHAIN     dpCSO11i
157500170619     C                   IF        %found(dpCSO11i)
157600170621     C                   EVAL      CSODESIT = wDESxx
157700170619     C                   UPDATE    DPCSO100
157800170620     C                   ENDIF
157900170619     C                   ENDIF
158000170619     C*
158100170619     C                   ENDSR
158200170619     C***
158300170619
158400170619
158500170619     C*----------------------------------------------------*
158600170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.EN"
158700170619     C*----------------------------------------------------*
158800170619     C     GESCSO_EN     BEGSR
158900170619     C*
159000170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
159100170619     C                   Z-ADD     2             i                 4 0
159200170620     C                   DOW       i <= %elem(SkSplitFLD)
159300170620     C                             AND i <= OutIdxMax
159400170619     C*
159500170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
159600170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
159700170619     C                             SkSplitCSV_9(i) <> *blanks
159800170619     C*
159900170620     C*** SORC
160000170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
160100170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
160200170620     C                   EXSR      CHKNUM
160300170620     C                   IF        PiInt=*on
160400170620     C                   Z-ADD     PiVal         CSOSORC
160500170620     C                   ENDIF
160600170620     C                   ENDIF
160700170619     C*** DESxx
160800170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
160900170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
161000170619     C                   ENDIF
161100170619     C*
161200170619     C                   ENDIF
161300170620     C                   EVAL      i = i + 1
161400170619     C                   ENDDO
161500170619     C*
161600170619     C*** ATB
161700170619     C                   EVAL      CSOATB = *blanks
161800170619     C*** VER
161900170619     C                   EVAL      CSOVER = NEW_VERSION
162000170619     C*
162100170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
162200170621     C                   IF        wDESxx <> *blanks
162300170619     C     KEYcso_11C    CHAIN     dpCSO11i
162400170619     C                   IF        %found(dpCSO11i)
162500170621     C                   EVAL      CSODESEN = wDESxx
162600170619     C                   UPDATE    DPCSO100
162700170620     C                   ENDIF
162800170619     C                   ENDIF
162900170619     C*
163000170619     C                   ENDSR
163100170619     C***
163200170619
163300170619
163400170619     C*----------------------------------------------------*
163500170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.DE"
163600170619     C*----------------------------------------------------*
163700170619     C     GESCSO_DE     BEGSR
163800170619     C*
163900170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
164000170619     C                   Z-ADD     2             i                 4 0
164100170620     C                   DOW       i <= %elem(SkSplitFLD)
164200170620     C                             AND i <= OutIdxMax
164300170619     C*
164400170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
164500170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
164600170619     C                             SkSplitCSV_9(i) <> *blanks
164700170619     C*
164800170620     C*** SORC
164900170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
165000170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
165100170620     C                   EXSR      CHKNUM
165200170620     C                   IF        PiInt=*on
165300170620     C                   Z-ADD     PiVal         CSOSORC
165400170620     C                   ENDIF
165500170620     C                   ENDIF
165600170619     C*** DESxx
165700170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
165800170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
165900170619     C                   ENDIF
166000170619     C*
166100170619     C                   ENDIF
166200170620     C                   EVAL      i = i + 1
166300170619     C                   ENDDO
166400170619     C*
166500170619     C*** ATB
166600170619     C                   EVAL      CSOATB = *blanks
166700170619     C*** VER
166800170619     C                   EVAL      CSOVER = NEW_VERSION
166900170619     C*
167000170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
167100170621     C                   IF        wDESxx <> *blanks
167200170619     C     KEYcso_11C    CHAIN     dpCSO11i
167300170619     C                   IF        %found(dpCSO11i)
167400170621     C                   EVAL      CSODESDE = wDESxx
167500170619     C                   UPDATE    DPCSO100
167600170620     C                   ENDIF
167700170619     C                   ENDIF
167800170619     C*
167900170619     C                   ENDSR
168000170619     C***
168100170619
168200170619
168300170619     C*----------------------------------------------------*
168400170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.FR"
168500170619     C*----------------------------------------------------*
168600170619     C     GESCSO_FR     BEGSR
168700170619     C*
168800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
168900170619     C                   Z-ADD     2             i                 4 0
169000170620     C                   DOW       i <= %elem(SkSplitFLD)
169100170620     C                             AND i <= OutIdxMax
169200170619     C*
169300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
169400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
169500170619     C                             SkSplitCSV_9(i) <> *blanks
169600170619     C*
169700170620     C*** SORC
169800170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
169900170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
170000170620     C                   EXSR      CHKNUM
170100170620     C                   IF        PiInt=*on
170200170620     C                   Z-ADD     PiVal         CSOSORC
170300170620     C                   ENDIF
170400170620     C                   ENDIF
170500170619     C*** DESxx
170600170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
170700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
170800170619     C                   ENDIF
170900170619     C*
171000170619     C                   ENDIF
171100170620     C                   EVAL      i = i + 1
171200170619     C                   ENDDO
171300170619     C*
171400170619     C*** ATB
171500170619     C                   EVAL      CSOATB = *blanks
171600170619     C*** VER
171700170619     C                   EVAL      CSOVER = NEW_VERSION
171800170619     C*
171900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
172000170621     C                   IF        wDESxx <> *blanks
172100170619     C     KEYcso_11C    CHAIN     dpCSO11i
172200170619     C                   IF        %found(dpCSO11i)
172300170621     C                   EVAL      CSODESFR = wDESxx
172400170619     C                   UPDATE    DPCSO100
172500170620     C                   ENDIF
172600170619     C                   ENDIF
172700170619     C*
172800170619     C                   ENDSR
172900170619     C***
173000170619
173100170619
173200170619     C*----------------------------------------------------*
173300170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.ET"
173400170619     C*----------------------------------------------------*
173500170619     C     GESCSO_ET     BEGSR
173600170619     C*
173700170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
173800170619     C                   Z-ADD     2             i                 4 0
173900170620     C                   DOW       i <= %elem(SkSplitFLD)
174000170620     C                             AND i <= OutIdxMax
174100170619     C*
174200170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
174300170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
174400170619     C                             SkSplitCSV_9(i) <> *blanks
174500170619     C*
174600170620     C*** SORC
174700170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
174800170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
174900170620     C                   EXSR      CHKNUM
175000170620     C                   IF        PiInt=*on
175100170620     C                   Z-ADD     PiVal         CSOSORC
175200170620     C                   ENDIF
175300170620     C                   ENDIF
175400170619     C*** DESxx
175500170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
175600170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
175700170619     C                   ENDIF
175800170619     C*
175900170619     C                   ENDIF
176000170620     C                   EVAL      i = i + 1
176100170619     C                   ENDDO
176200170619     C*
176300170619     C*** ATB
176400170619     C                   EVAL      CSOATB = *blanks
176500170619     C*** VER
176600170619     C                   EVAL      CSOVER = NEW_VERSION
176700170619     C*
176800170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
176900170621     C                   IF        wDESxx <> *blanks
177000170619     C     KEYcso_11C    CHAIN     dpCSO11i
177100170619     C                   IF        %found(dpCSO11i)
177200170621     C                   EVAL      CSODESET = wDESxx
177300170619     C                   UPDATE    DPCSO100
177400170620     C                   ENDIF
177500170619     C                   ENDIF
177600170619     C*
177700170619     C                   ENDSR
177800170619     C***
177900170619
178000170619
178100170619     C*----------------------------------------------------*
178200170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.CS"
178300170619     C*----------------------------------------------------*
178400170619     C     GESCSO_CS     BEGSR
178500170619     C*
178600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
178700170619     C                   Z-ADD     2             i                 4 0
178800170620     C                   DOW       i <= %elem(SkSplitFLD)
178900170620     C                             AND i <= OutIdxMax
179000170619     C*
179100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
179200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
179300170619     C                             SkSplitCSV_9(i) <> *blanks
179400170619     C*
179500170620     C*** SORC
179600170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
179700170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
179800170620     C                   EXSR      CHKNUM
179900170620     C                   IF        PiInt=*on
180000170620     C                   Z-ADD     PiVal         CSOSORC
180100170620     C                   ENDIF
180200170620     C                   ENDIF
180300170619     C*** DESxx
180400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
180500170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
180600170619     C                   ENDIF
180700170619     C*
180800170619     C                   ENDIF
180900170620     C                   EVAL      i = i + 1
181000170619     C                   ENDDO
181100170619     C*
181200170619     C*** ATB
181300170619     C                   EVAL      CSOATB = *blanks
181400170619     C*** VER
181500170619     C                   EVAL      CSOVER = NEW_VERSION
181600170619     C*
181700170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
181800170621     C                   IF        wDESxx <> *blanks
181900170619     C     KEYcso_11C    CHAIN     dpCSO11i
182000170619     C                   IF        %found(dpCSO11i)
182100170621     C                   EVAL      CSODESCS = wDESxx
182200170619     C                   UPDATE    DPCSO100
182300170620     C                   ENDIF
182400170619     C                   ENDIF
182500170619     C*
182600170619     C                   ENDSR
182700170619     C***
182800170619
182900170619
183000170619     C*----------------------------------------------------*
183100170619     C*  GESTIONE MAPPATURA "ASCODE"
183200170619     C*----------------------------------------------------*
183300170619     C     GESCAS        BEGSR
183400170619     C*
183500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
183600170619     C                   CLEAR                   DPCAS100
183700170619     C*
183800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
183900170619     C                   Z-ADD     2             i                 4 0
184000170620     C                   DOW       i <= %elem(SkSplitFLD)
184100170620     C                             AND i <= OutIdxMax
184200170619     C*
184300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
184400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
184500170619     C                             SkSplitCSV_9(i) <> *blanks
184600170619     C*
184700170619     C*** ADSC
184800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ASCODE'
184900170620     C                   EVAL      CASADSC = SkSplitCSV_9(i)
185000170619     C                   ENDIF
185100170619     C*** STXT
185200170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICETEXT'
185300170620     C                   EVAL      CASSTXT = SkSplitCSV_9(i)
185400170619     C                   ENDIF
185500170620     C*** ELMC
185600170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODES'
185700170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
185800170620     C                   ENDIF
185900170619     C*
186000170619     C                   ENDIF
186100170620     C                   EVAL      i = i + 1
186200170619     C                   ENDDO
186300170619     C*
186400170619     C*** VER
186500170619     C                   EVAL      CASVER = NEW_VERSION
186600170620     C*
186700170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
186800170620     C                   EVAL      InStringa = w2048_01
186900170620     C                   EVAL      InSepara = ','
187000170620     C                   SETOFF                                       131419
187100170620     C                   SETON                                        13
187200170620     C                   EXSR      SR_SPLIT
187300170620     C*
187400170620     C* Se nessun elemento => scrivo comunque un record singolo
187500170620     C                   IF        OutIdxMax = *zeros
187600170620     C                   WRITE     DPCAS100
187700170620     C                   ELSE
187800170620     C*
187900170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
188000170620     C                   Z-ADD     1             j                 4 0
188100170620     C                   DOW       j <= %elem(SkSplitCSV_3)
188200170620     C                             AND j <= OutIdxMax
188300170620     C*** ELMC
188400170620     C                   EVAL      PiStr = SkSplitCSV_3(j)
188500170620     C                   EXSR      CHKNUM
188600170620     C                   IF        PiInt=*on
188700170620     C                   Z-ADD     PiVal         CASELMC
188800170620     C                   ENDIF
188900170620     C*
189000170620     C* Infine inserisco il record corrente
189100170620     C                   WRITE     DPCAS100
189200170620     C*
189300170620     C                   EVAL      j = j + 1
189400170620     C                   ENDDO
189500170620     C                   ENDIF
189600170619     C*
189700170619     C                   ENDSR
189800170619     C***
189900170619
190000170619
190100170619     C*----------------------------------------------------*
190200170619     C*  GESTIONE MAPPATURA "IC"
190300170619     C*----------------------------------------------------*
190400170619     C     GESCIC        BEGSR
190500170619     C*
190600170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
190700170619     C                   CLEAR                   DPCIC100
190800170619     C*
190900170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
191000170619     C                   Z-ADD     2             i                 4 0
191100170620     C                   DOW       i <= %elem(SkSplitFLD)
191200170620     C                             AND i <= OutIdxMax
191300170619     C*
191400170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
191500170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
191600170619     C                             SkSplitCSV_9(i) <> *blanks
191700170619     C*
191800170619     C*** ICC
191900170619     C                   IF        %trim(SkSplitFLD(i)) = 'INJECTIONCODE'
192000170619     C                   EVAL      CICICC  = SkSplitCSV_9(i)
192100170619     C                   ENDIF
192200170619     C*** DPTN
192300170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
192400170619     C                   EVAL      CICDPTN = SkSplitCSV_9(i)
192500170619     C                   ENDIF
192600170619     C*** ICD
192700170619     C                   IF        %trim(SkSplitFLD(i)) = 'DESCRIPTION'
192800170619     C                   EVAL      CICICD  = SkSplitCSV_9(i)
192900170619     C                   ENDIF
193000170619     C*
193100170619     C                   ENDIF
193200170620     C                   EVAL      i = i + 1
193300170619     C                   ENDDO
193400170619     C*
193500170619     C*** VER
193600170619     C                   EVAL      CICVER = NEW_VERSION
193700170619     C*
193800170619     C* Infine inserisco il record corrente
193900170619     C                   WRITE     DPCIC100
194000170619     C*
194100170619     C                   ENDSR
194200170619     C***
194300170619
194400170619
194500170619     C*----------------------------------------------------*
194600170619     C*  GESTIONE MAPPATURA "NETWORK"
194700170619     C*----------------------------------------------------*
194800170619     C     GESCNT        BEGSR
194900170619     C*
195000170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
195100170619     C                   CLEAR                   DPCNT100
195200170619     C*
195300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
195400170619     C                   Z-ADD     2             i                 4 0
195500170620     C                   DOW       i <= %elem(SkSplitFLD)
195600170620     C                             AND i <= OutIdxMax
195700170619     C*
195800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
195900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
196000170619     C                             SkSplitCSV_9(i) <> *blanks
196100170619     C*
196200170619     C*** NTWC
196300170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKCODE'
196400170619     C                   EVAL      CNTNTWC = SkSplitCSV_9(i)
196500170619     C                   ENDIF
196600170619     C*** BUCN
196700170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
196800170619     C                   EVAL      CNTBUCN = SkSplitCSV_9(i)
196900170619     C                   ENDIF
197000170619     C*** ISO2
197100170619     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
197200170619     C                   EVAL      CNTISO2 = SkSplitCSV_9(i)
197300170619     C                   ENDIF
197400170619     C*** NTWN
197500170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKNAME'
197600170619     C                   EVAL      CNTNTWN = SkSplitCSV_9(i)
197700170619     C                   ENDIF
197800170619     C*
197900170619     C                   ENDIF
198000170619     C                   EVAL      i = i + 1
198100170619     C                   ENDDO
198200170619     C*
198300170619     C*** VER
198400170619     C                   EVAL      CNTVER = NEW_VERSION
198500170619     C*
198600170619     C* Infine inserisco il record corrente
198700170619     C                   WRITE     DPCNT100
198800170619     C*
198900170619     C                   ENDSR
199000170619     C***
199100170619
199200170619
199300170619     C*----------------------------------------------------*
199400170621     C*  GESTIONE MAPPATURA "NETWORKPLAN"
199500170619     C*----------------------------------------------------*
199600170619     C     GESCNP        BEGSR
199700170619     C*
199800170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
199900170619     C                   CLEAR                   DPCNP100
200000170619     C*
200100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
200200170619     C                   Z-ADD     2             i                 4 0
200300170620     C                   DOW       i <= %elem(SkSplitFLD)
200400170620     C                             AND i <= OutIdxMax
200500170619     C*
200600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
200700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
200800170619     C                             SkSplitCSV_9(i) <> *blanks
200900170619     C*
201000170619     C*** NTWP
201100170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
201200170619     C                   EVAL      CNPNTWP  = SkSplitCSV_9(i)
201300170619     C                   ENDIF
201400170621     C*** SORCB / SORCE
201500170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODELIST'
201600170621     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
201700170619     C                   ENDIF
201800170619     C*** ASCOL
201900170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOASCODELIST'
202000170619     C                   EVAL      CNPASCOL = SkSplitCSV_9(i)
202100170619     C                   ENDIF
202200170619     C*** FULL
202300170619     C                   IF        %trim(SkSplitFLD(i)) = 'FULLNETWORKPLAN'
202400170619     C                   EVAL      CNPFULL  = SkSplitCSV_9(i)
202500170619     C                   ENDIF
202600170619     C*** PTY
202700170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
202800170619     C                   EVAL      CNPPTY   = SkSplitCSV_9(i)
202900170619     C                   ENDIF
203000170619     C*
203100170619     C                   ENDIF
203200170619     C                   EVAL      i = i + 1
203300170619     C                   ENDDO
203400170619     C*
203500170619     C*** VER
203600170621     C                   EVAL      CNPVER = NEW_VERSION
203700170621     C*
203800170621     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
203900170621     C                   EVAL      InStringa = w2048_01
204000170621     C                   EVAL      InSepara = ','
204100170621     C                   SETOFF                                       131419
204200170621     C                   SETON                                        13
204300170621     C                   EXSR      SR_SPLIT
204400170621     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
204500170621     C*
204600170621     C* Se nessun elemento => scrivo comunque un record singolo
204700170621     C                   IF        sOutIdxMax_w1 = *zeros
204800170621     C                   WRITE     DPCNP100
204900170621     C                   ELSE
205000170621     C*
205100170621     C* Sviluppo tutti gli elementi della lista dei Routing Places
205200170621     C                   Z-ADD     1             j                 4 0
205300170621     C                   DOW       j <= %elem(SkSplitCSV_3)
205400170621     C                             AND j <= sOutIdxMax_w1
205500170621     C*
205600170621     C* Determino il tipo di Routing Place
205700170621     C                   SELECT
205800170621     C* SOCODE - Single
205900170621     C                   WHEN      %len(%trim((SkSplitCSV_3(j)))) = 4
206000170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):2:3)
206100170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
206200170621     C                   EVAL      CNPSORCE = CNPSORCB
206300170621     C                   WRITE     DPCNP100
206400170621     C* SOCODE - Range
206500170621     C                   WHEN      %scan('-':SkSplitCSV_3(j)) > *zeros
206600170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):2:3)
206700170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
206800170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):6:3)
206900170621     C                   EVAL      CNPSORCE = %dec(wSORC:3:0)
207000170621     C                   WRITE     DPCNP100
207100170621     C* SOCODE - List
207200170621     C                   WHEN      %scan(':':SkSplitCSV_3(j)) > *zeros
207300170621     C                   EVAL      InStringa = %subst(SkSplitCSV_3(j):2)
207400170621     C                   EVAL      InSepara = ':'
207500170621     C                   SETOFF                                       131419
207600170621     C                   SETON                                        14
207700170621     C                   EXSR      SR_SPLIT
207800170621     C*
207900170621     C* Sviluppo tutti gli elementi della lista dei Routing Places
208000170621     C                   Z-ADD     1             z                 4 0
208100170621     C                   DOW       z <= %elem(SkSplitCSV_4)
208200170621     C                             AND z <= OutIdxMax
208300170621     C                   EVAL      wSORC    = SkSplitCSV_4(z)
208400170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
208500170621     C                   EVAL      CNPSORCE = CNPSORCB
208600170621     C                   WRITE     DPCNP100
208700170621     C                   EVAL      z = z + 1
208800170621     C                   ENDDO
208900170621     C*
209000170621     C                   ENDSL
209100170621     C*
209200170621     C                   EVAL      j = j + 1
209300170621     C                   ENDDO
209400170621     C                   ENDIF
209500170619     C*
209600170619     C                   ENDSR
209700170619     C***
209800170619
209900170619
210000170619     C*----------------------------------------------------*
210100170619     C*  GESTIONE MAPPATURA "ALLOWSO"
210200170619     C*----------------------------------------------------*
210300170621     C     GESCWS        BEGSR
210400170619     C*
210500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
210600170619     C                   CLEAR                   DPCWS100
210700170619     C*
210800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
210900170619     C                   Z-ADD     2             i                 4 0
211000170620     C                   DOW       i <= %elem(SkSplitFLD)
211100170620     C                             AND i <= OutIdxMax
211200170619     C*
211300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
211400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
211500170619     C                             SkSplitCSV_9(i) <> *blanks
211600170619     C*
211700170621     C*** RULFT / RULFB / RULFE
211800170619     C                   IF        %trim(SkSplitFLD(i)) = 'RULEFROM'
211900170621     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
212000170621     C                   ENDIF
212100170621     C*** RULSB / RULSE
212200170621     C                   IF        %trim(SkSplitFLD(i)) = 'RULESERVICE'
212300170621     C                   EVAL      w2048_02 = SkSplitCSV_9(i)
212400170621     C                   ENDIF
212500170621     C*** RULTT / RULTB / RULTE
212600170621     C                   IF        %trim(SkSplitFLD(i)) = 'RULETO'
212700170621     C                   EVAL      w2048_03 = SkSplitCSV_9(i)
212800170619     C                   ENDIF
212900170621     C*** ZONTB / ZONTE
213000170619     C                   IF        %trim(SkSplitFLD(i)) = 'ZONETO'
213100170621     C                   EVAL      w2048_04 = SkSplitCSV_9(i)
213200170619     C                   ENDIF
213300170619     C*** ALWID
213400170619     C                   IF        %trim(SkSplitFLD(i)) = 'UNIQUEALLOWID'
213500170628     C                   EVAL      PiStr = SkSplitCSV_9(i)
213600170628     C                   EXSR      CHKNUM
213700170628     C                   IF        PiInt=*on
213800170628     C                   Z-ADD     PiVal         CWSALWID
213900170628     C                   ENDIF
214000170628     C                   ENDIF
214100170619     C*
214200170619     C                   ENDIF
214300170619     C                   EVAL      i = i + 1
214400170619     C                   ENDDO
214500170619     C*
214600170619     C*** VER
214700170619     C                   EVAL      CWSVER = NEW_VERSION
214800170621     C*
214900170621     C* Splitto il 1° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
215000170621     C                   EVAL      InStringa = w2048_01
215100170621     C                   EVAL      InSepara = ','
215200170621     C                   SETOFF                                       131419
215300170621     C                   SETON                                        13
215400170621     C                   EXSR      SR_SPLIT
215500170621     C                   EVAL      wSkCSV_03_w1  = %subarr(SkSplitCSV_3:1)
215600170621     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
215700170621     C*
215800170621     C* Splitto il 2° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
215900170621     C                   EVAL      InStringa = w2048_02
216000170621     C                   EVAL      InSepara = ','
216100170621     C                   SETOFF                                       131419
216200170621     C                   SETON                                        13
216300170621     C                   EXSR      SR_SPLIT
216400170621     C                   EVAL      wSkCSV_03_w2  = %subarr(SkSplitCSV_3:1)
216500170621     C                   EVAL      sOutIdxMax_w2 = OutIdxMax
216600170621     C*
216700170621     C* Splitto il 3° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
216800170621     C                   EVAL      InStringa = w2048_03
216900170621     C                   EVAL      InSepara = ','
217000170621     C                   SETOFF                                       131419
217100170621     C                   SETON                                        13
217200170621     C                   EXSR      SR_SPLIT
217300170621     C                   EVAL      wSkCSV_03_w3  = %subarr(SkSplitCSV_3:1)
217400170621     C                   EVAL      sOutIdxMax_w3 = OutIdxMax
217500170621     C*
217600170621     C* Splitto il 4° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
217700170621     C                   EVAL      InStringa = w2048_04
217800170621     C                   EVAL      InSepara = ','
217900170621     C                   SETOFF                                       131419
218000170621     C                   SETON                                        13
218100170621     C                   EXSR      SR_SPLIT
218200170621     C                   EVAL      wSkCSV_03_w4  = %subarr(SkSplitCSV_3:1)
218300170621     C                   EVAL      sOutIdxMax_w4 = OutIdxMax
218400170621     C*
218500170621     C* Verifico eventuale assenza di elementi che in tal caso forzo a 1
218600170621     C                   IF        sOutIdxMax_w1 = *zeros
218700170621     C                   EVAL      sOutIdxMax_w1 = 1
218800170621     C                   ENDIF
218900170621     C                   IF        sOutIdxMax_w2 = *zeros
219000170621     C                   EVAL      sOutIdxMax_w2 = 1
219100170621     C                   ENDIF
219200170621     C                   IF        sOutIdxMax_w3 = *zeros
219300170621     C                   EVAL      sOutIdxMax_w3 = 1
219400170621     C                   ENDIF
219500170621     C                   IF        sOutIdxMax_w4 = *zeros
219600170621     C                   EVAL      sOutIdxMax_w4 = 1
219700170621     C                   ENDIF
219800170621     C*
219900170621     C* Sviluppo tutti gli elementi "moltiplicando" le liste delle RULE FROM/SOCODE/TO
220000170621     C*
220100170621     C* 1° lista
220200170621     C                   Z-ADD     1             i                 4 0
220300170621     C                   DOW       i <= %elem(wSkCSV_03_w1)
220400170621     C                             AND i <= sOutIdxMax_w1
220500170621     C*
220600170621     C* Determino il tipo di RULE FROM
220700170621     C                   SELECT
220800170621     C* DEPOT - Single
220900170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'D' AND
221000170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 8
221100170621     C                   EVAL      CWSRULFT = 'D'
221200170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:7)
221300170621     C                   EVAL      CWSRULFE = CWSRULFB
221400170621     C* DEPOT - Range
221500170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'D' AND
221600170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 15
221700170621     C                   EVAL      CWSRULFT = 'D'
221800170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:7)
221900170621     C                   EVAL      CWSRULFE = %subst(wSkCSV_03_w1(i):9:7)
222000170621     C* DEPOT GROUP - Single
222100170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'G'
222200170621     C                   EVAL      CWSRULFT = 'G'
222300170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:4)
222400170621     C                   EVAL      CWSRULFE = CWSRULFB
222500170621     C* COUNTRY - Single
222600170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'C'
222700170621     C                   EVAL      CWSRULFT = 'C'
222800170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:2)
222900170621     C                   EVAL      CWSRULFE = CWSRULFB
223000170621     C* NETWORK - Single
223100170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'N'
223200170621     C                   EVAL      CWSRULFT = 'N'
223300170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:3)
223400170621     C                   EVAL      CWSRULFE = CWSRULFB
223500170621     C* IC - Single
223600170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'I'
223700170621     C                   EVAL      CWSRULFT = 'I'
223800170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:9)
223900170621     C                   EVAL      CWSRULFE = CWSRULFB
224000170621     C* BU - Single
224100170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'B' AND
224200170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 4
224300170621     C                   EVAL      CWSRULFT = 'B'
224400170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:3)
224500170621     C                   EVAL      CWSRULFE = CWSRULFB
224600170621     C* BU+COUNTRY - Single
224700170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'B' AND
224800170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 6
224900170621     C                   EVAL      CWSRULFT = 'B'
225000170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:5)
225100170621     C                   EVAL      CWSRULFE = CWSRULFB
225200170621     C*
225300170621     C                   ENDSL
225400170621     C*
225500170621     C* 2° lista
225600170621     C                   Z-ADD     1             j                 4 0
225700170621     C                   DOW       j <= %elem(wSkCSV_03_w2)
225800170621     C                             AND j <= sOutIdxMax_w2
225900170621     C*
226000170621     C* Determino il tipo di RULE SERVICE
226100170621     C                   SELECT
226200170621     C* SOCODE - Single
226300170621     C                   WHEN      %subst(wSkCSV_03_w2(j):1:2) = 'SO' AND
226400170621     C                             %len(%trim((wSkCSV_03_w2(j)))) = 5
226500170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):3:3)
226600170621     C                   EVAL      CWSRULSB = %dec(wSORC:3:0)
226700170621     C                   EVAL      CWSRULSE = CWSRULSB
226800170621     C* SOCODE - Range
226900170621     C                   WHEN      %subst(wSkCSV_03_w2(j):1:2) = 'SO' AND
227000170621     C                             %len(%trim((wSkCSV_03_w2(j)))) = 8
227100170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):3:3)
227200170621     C                   EVAL      CWSRULSB = %dec(wSORC:3:0)
227300170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):6:3)
227400170621     C                   EVAL      CWSRULSE = %dec(wSORC:3:0)
227500170621     C*
227600170621     C                   ENDSL
227700170621     C*
227800170621     C* 3° lista
227900170621     C                   Z-ADD     1             z                 4 0
228000170621     C                   DOW       z <= %elem(wSkCSV_03_w3)
228100170621     C                             AND z <= sOutIdxMax_w3
228200170621     C*
228300170621     C* Determino il tipo di RULE TO
228400170621     C                   SELECT
228500170621     C* DEPOT - Single
228600170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'D' AND
228700170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 8
228800170621     C                   EVAL      CWSRULTT = 'D'
228900170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:7)
229000170621     C                   EVAL      CWSRULTE = CWSRULTB
229100170621     C* DEPOT - Range
229200170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'D' AND
229300170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 15
229400170621     C                   EVAL      CWSRULTT = 'D'
229500170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:7)
229600170621     C                   EVAL      CWSRULTE = %subst(wSkCSV_03_w3(z):9:7)
229700170621     C* DEPOT GROUP - Single
229800170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'G'
229900170621     C                   EVAL      CWSRULTT = 'G'
230000170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:4)
230100170621     C                   EVAL      CWSRULTE = CWSRULTB
230200170621     C* COUNTRY - Single
230300170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'C'
230400170621     C                   EVAL      CWSRULTT = 'C'
230500170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:2)
230600170621     C                   EVAL      CWSRULTE = CWSRULTB
230700170621     C* NETWORK - Single
230800170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'N'
230900170621     C                   EVAL      CWSRULTT = 'N'
231000170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:3)
231100170621     C                   EVAL      CWSRULTE = CWSRULTB
231200170621     C* COUNTRY+ZIP CODE - Single
231300170629     C***                WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'Z' AND
231400170629     C***                          %scan('-':wSkCSV_03_w3(z)) = *zeros
231500170629     C***                EVAL      CWSRULTT = 'Z'
231600170629     C***                EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:11)
231700170629     C***                EVAL      CWSRULTE = CWSRULTB
231800170621     C* COUNTRY+ZIP CODE - Range
231900170629     C***                WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'Z' AND
232000170629     C***                          %scan('-':wSkCSV_03_w3(z)) > *zeros
232100170629     C***                EVAL      CWSRULTT = 'Z'
232200170629     C***                EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:
232300170629     C***                                     %scan('-':wSkCSV_03_w3(z))-2)
232400170629     C***                EVAL      CWSRULTE = %subst(wSkCSV_03_w3(z):2:2)+
232500170629     C***                                     %subst(wSkCSV_03_w3(z):
232600170629     C***                                     %scan('-':wSkCSV_03_w3(z))+1:9)
232700170621     C* BU+COUNTRY - Single
232800170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'B' AND
232900170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 6
233000170621     C                   EVAL      CWSRULTT = 'B'
233100170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w1(z):2:5)
233200170621     C                   EVAL      CWSRULTE = CWSRULTB
233300170621     C*
233400170621     C                   ENDSL
233500170621     C*
233600170621     C* 4° lista
233700170621     C                   Z-ADD     1             x                 4 0
233800170621     C                   DOW       x <= %elem(wSkCSV_03_w4)
233900170621     C                             AND x <= sOutIdxMax_w4
234000170621     C*
234100170629     C* Determino il tipo di ZONE TO
234200170621     C                   SELECT
234300170621     C* COUNTRY+ZIP CODE - Single
234400170621     C                   WHEN      %subst(wSkCSV_03_w4(x):1:1) = 'Z' AND
234500170621     C                             %scan('-':wSkCSV_03_w4(x)) = *zeros
234600170621     C                   EVAL      CWSZONTB = %subst(wSkCSV_03_w4(x):2:11)
234700170621     C                   EVAL      CWSZONTE = CWSZONTB
234800170621     C* COUNTRY+ZIP CODE - Range
234900170621     C                   WHEN      %subst(wSkCSV_03_w4(x):1:1) = 'Z' AND
235000170621     C                             %scan('-':wSkCSV_03_w4(x)) > *zeros
235100170621     C                   EVAL      CWSZONTB = %subst(wSkCSV_03_w4(x):2:
235200170621     C                                        %scan('-':wSkCSV_03_w4(x))-2)
235300170621     C                   EVAL      CWSZONTE = %subst(wSkCSV_03_w4(x):2:2)+
235400170621     C                                        %subst(wSkCSV_03_w4(x):
235500170621     C                                        %scan('-':wSkCSV_03_w4(x))+1:9)
235600170621     C*
235700170621     C                   ENDSL
235800170621     C*
235900170621     C* Inserisco il record corrente
236000170621     C                   WRITE     DPCWS100
236100170621     C*
236200170621     C                   EVAL      x = x + 1
236300170621     C                   ENDDO
236400170621     C*
236500170621     C                   EVAL      z = z + 1
236600170621     C                   ENDDO
236700170621     C*
236800170621     C                   EVAL      j = j + 1
236900170621     C                   ENDDO
237000170621     C*
237100170621     C                   EVAL      i = i + 1
237200170621     C                   ENDDO
237300170619     C*
237400170619     C                   ENDSR
237500170619     C***
237600170629
237700170629
237800170629     C*----------------------------------------------------*
237900170629     C*  GESTIONE MAPPATURA "ALLOWAS"
238000170629     C*----------------------------------------------------*
238100170629     C     GESCWA        BEGSR
238200170629     C*
238300170629     C* Inizializzo il formato record del file relativo alla mappatura corrente
238400170629     C                   CLEAR                   DPCWA100
238500170629     C*
238600170629     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
238700170629     C                   Z-ADD     2             i                 4 0
238800170629     C                   DOW       i <= %elem(SkSplitFLD)
238900170629     C                             AND i <= OutIdxMax
239000170629     C*
239100170629     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
239200170629     C                   IF        SkSplitFLD(i)   <> *blanks AND
239300170629     C                             SkSplitCSV_9(i) <> *blanks
239400170629     C*
239500170629     C*** ALWID
239600170629     C                   IF        %trim(SkSplitFLD(i)) = 'UNIQUEALLOWID'
239700170629     C                   EVAL      PiStr = SkSplitCSV_9(i)
239800170629     C                   EXSR      CHKNUM
239900170629     C                   IF        PiInt=*on
240000170629     C                   Z-ADD     PiVal         CWAALWID
240100170629     C                   ENDIF
240200170629     C                   ENDIF
240300170629     C*** RULSB / RULSE
240400170629     C                   IF        %trim(SkSplitFLD(i)) = 'RULESERVICE'
240500170629     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
240600170629     C                   ENDIF
240700170629     C*** ZONTB / ZONTE
240800170629     C                   IF        %trim(SkSplitFLD(i)) = 'ZONETO'
240900170629     C                   EVAL      w2048_02 = SkSplitCSV_9(i)
241000170629     C                   ENDIF
241100170629     C*
241200170629     C                   ENDIF
241300170629     C                   EVAL      i = i + 1
241400170629     C                   ENDDO
241500170629     C*
241600170629     C*** VER
241700170629     C                   EVAL      CWAVER = NEW_VERSION
241800170629     C*
241900170629     C* Splitto il 1° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
242000170629     C                   EVAL      InStringa = w2048_01
242100170629     C                   EVAL      InSepara = ','
242200170629     C                   SETOFF                                       131419
242300170629     C                   SETON                                        13
242400170629     C                   EXSR      SR_SPLIT
242500170629     C                   EVAL      wSkCSV_03_w1  = %subarr(SkSplitCSV_3:1)
242600170629     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
242700170629     C*
242800170629     C* Splitto il 2° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
242900170629     C                   EVAL      InStringa = w2048_02
243000170629     C                   EVAL      InSepara = ','
243100170629     C                   SETOFF                                       131419
243200170629     C                   SETON                                        13
243300170629     C                   EXSR      SR_SPLIT
243400170629     C                   EVAL      wSkCSV_03_w2  = %subarr(SkSplitCSV_3:1)
243500170629     C                   EVAL      sOutIdxMax_w2 = OutIdxMax
243600170629     C*
243700170629     C* Verifico eventuale assenza di elementi che in tal caso forzo a 1
243800170629     C                   IF        sOutIdxMax_w1 = *zeros
243900170629     C                   EVAL      sOutIdxMax_w1 = 1
244000170629     C                   ENDIF
244100170629     C                   IF        sOutIdxMax_w2 = *zeros
244200170629     C                   EVAL      sOutIdxMax_w2 = 1
244300170629     C                   ENDIF
244400170629     C*
244500170629     C* Sviluppo tutti gli elementi "moltiplicando" le liste delle RULE SERVICE/ZONE
244600170629     C*
244700170629     C* 1° lista
244800170629     C                   Z-ADD     1             i                 4 0
244900170629     C                   DOW       i <= %elem(wSkCSV_03_w1)
245000170629     C                             AND i <= sOutIdxMax_w1
245100170629     C*
245200170629     C* Determino il tipo di RULE SERVICE
245300170629     C                   SELECT
245400170724     C* ASCODE - Single
245500170629     C                   WHEN      %subst(wSkCSV_03_w1(j):1:2) = 'AS' AND
245600170629     C                             %len(%trim((wSkCSV_03_w1(j)))) = 5
245700170629     C                   EVAL      CWARULSB = %subst(wSkCSV_03_w1(j):3:3)
245800170629     C                   EVAL      CWARULSE = CWARULSB
245900170724     C* ASCODE - Range
246000170629     C                   WHEN      %subst(wSkCSV_03_w1(j):1:2) = 'AS' AND
246100170629     C                             %len(%trim((wSkCSV_03_w1(j)))) = 8
246200170629     C                   EVAL      CWARULSB = %subst(wSkCSV_03_w1(j):3:3)
246300170629     C                   EVAL      CWARULSE = %subst(wSkCSV_03_w1(j):6:3)
246400170629     C*
246500170629     C                   ENDSL
246600170629     C*
246700170629     C* 2° lista
246800170629     C                   Z-ADD     1             j                 4 0
246900170629     C                   DOW       j <= %elem(wSkCSV_03_w2)
247000170629     C                             AND j <= sOutIdxMax_w2
247100170629     C*
247200170629     C* Determino il tipo di ZONE TO
247300170629     C                   SELECT
247400170629     C* COUNTRY+ZIP CODE - Single
247500170629     C                   WHEN      %subst(wSkCSV_03_w2(x):1:1) = 'Z' AND
247600170629     C                             %scan('-':wSkCSV_03_w2(x)) = *zeros
247700170629     C                   EVAL      CWAZONTB = %subst(wSkCSV_03_w2(x):2:11)
247800170629     C                   EVAL      CWAZONTE = CWAZONTB
247900170629     C* COUNTRY+ZIP CODE - Range
248000170629     C                   WHEN      %subst(wSkCSV_03_w2(x):1:1) = 'Z' AND
248100170629     C                             %scan('-':wSkCSV_03_w2(x)) > *zeros
248200170629     C                   EVAL      CWAZONTB = %subst(wSkCSV_03_w2(x):2:
248300170629     C                                        %scan('-':wSkCSV_03_w2(x))-2)
248400170629     C                   EVAL      CWAZONTE = %subst(wSkCSV_03_w2(x):2:2)+
248500170629     C                                        %subst(wSkCSV_03_w2(x):
248600170629     C                                        %scan('-':wSkCSV_03_w2(x))+1:9)
248700170629     C*
248800170629     C                   ENDSL
248900170629     C*
249000170629     C* Inserisco il record corrente
249100170629     C                   WRITE     DPCWA100
249200170629     C*
249300170629     C                   EVAL      j = j + 1
249400170629     C                   ENDDO
249500170629     C*
249600170629     C                   EVAL      i = i + 1
249700170629     C                   ENDDO
249800170629     C*
249900170629     C                   ENDSR
250000170629     C***
250100170619
250200170619
250300170619     C*----------------------------------------------------*
250400170619     C*  GESTIONE MAPPATURA "RELATION"
250500170619     C*----------------------------------------------------*
250600170619     C     GESCRE        BEGSR
250700170619     C*
250800170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
250900170619     C                   CLEAR                   DPCRE100
251000170619     C*
251100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
251200170619     C                   Z-ADD     2             i                 4 0
251300170620     C                   DOW       i <= %elem(SkSplitFLD)
251400170620     C                             AND i <= OutIdxMax
251500170619     C*
251600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
251700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
251800170619     C                             SkSplitCSV_9(i) <> *blanks
251900170619     C*
252000170619     C*** DPTN
252100170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
252200170619     C                   EVAL      CREDPTN  = SkSplitCSV_9(i)
252300170619     C                   ENDIF
252400170619     C*** ROUT
252500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTE'
252600170619     C                   EVAL      CREROUT  = SkSplitCSV_9(i)
252700170619     C                   ENDIF
252800170619     C*** RELA
252900170619     C                   IF        %trim(SkSplitFLD(i)) = 'RELATION'
253000170619     C                   EVAL      CRERELA  = SkSplitCSV_9(i)
253100170619     C                   ENDIF
253200170619     C*
253300170619     C                   ENDIF
253400170619     C                   EVAL      i = i + 1
253500170619     C                   ENDDO
253600170619     C*
253700170619     C*** VER
253800170619     C                   EVAL      CREVER = NEW_VERSION
253900170619     C*
254000170619     C* Infine inserisco il record corrente
254100170619     C                   WRITE     DPCRE100
254200170619     C*
254300170619     C                   ENDSR
254400170619     C***
254500170619
254600170619
254700170619     C*----------------------------------------------------*
254800170619     C*  GESTIONE MAPPATURA "DEPOTS"
254900170619     C*----------------------------------------------------*
255000170619     C     GESCDP        BEGSR
255100170619     C*
255200170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
255300170619     C                   CLEAR                   DPCDP100
255400170619     C*
255500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
255600170619     C                   Z-ADD     2             i                 4 0
255700170620     C                   DOW       i <= %elem(SkSplitFLD)
255800170620     C                             AND i <= OutIdxMax
255900170619     C*
256000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
256100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
256200170619     C                             SkSplitCSV_9(i) <> *blanks
256300170619     C*
256400170619     C*** DPTN
256500170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
256600170619     C                   EVAL      CDPDPTN  = SkSplitCSV_9(i)
256700170619     C                   ENDIF
256800170619     C*** DSTR
256900170619     C                   IF        %trim(SkSplitFLD(i)) = 'DEPOTSTR'
257000170619     C                   EVAL      CDPDSTR  = SkSplitCSV_9(i)
257100170619     C                   ENDIF
257200170619     C*** IATA
257300170619     C                   IF        %trim(SkSplitFLD(i)) = 'IATALIKECODE'
257400170619     C                   EVAL      CDPIATA  = SkSplitCSV_9(i)
257500170619     C                   ENDIF
257600170619     C*** GRPID
257700170619     C                   IF        %trim(SkSplitFLD(i)) = 'GROUPID'
257800170619     C                   EVAL      CDPGRPID = SkSplitCSV_9(i)
257900170619     C                   ENDIF
258000170619     C*** NAME1
258100170619     C                   IF        %trim(SkSplitFLD(i)) = 'COMPANYNAME'
258200170619     C                   EVAL      CDPNAME1 = SkSplitCSV_9(i)
258300170619     C                   ENDIF
258400170619     C*** NAME2
258500170619     C                   IF        %trim(SkSplitFLD(i)) = 'COMPANYNAME2'
258600170619     C                   EVAL      CDPNAME2 = SkSplitCSV_9(i)
258700170619     C                   ENDIF
258800170619     C*** STRET
258900170619     C                   IF        %trim(SkSplitFLD(i)) = 'STREET'
259000170619     C                   EVAL      CDPSTRET = SkSplitCSV_9(i)
259100170619     C                   ENDIF
259200170619     C*** STNBR
259300170619     C                   IF        %trim(SkSplitFLD(i)) = 'PROPNUM'
259400170619     C                   EVAL      CDPSTNBR = SkSplitCSV_9(i)
259500170619     C                   ENDIF
259600170619     C*** ADDR1
259700170619     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS2'
259800170619     C                   EVAL      CDPADDR1 = SkSplitCSV_9(i)
259900170619     C                   ENDIF
260000170619     C*** ADDR2
260100170619     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS3'
260200170619     C                   EVAL      CDPADDR2 = SkSplitCSV_9(i)
260300170619     C                   ENDIF
260400170619     C*** FLOOR
260500170619     C                   IF        %trim(SkSplitFLD(i)) = 'FLOOR'
260600170619     C                   EVAL      CDPFLOOR = SkSplitCSV_9(i)
260700170619     C                   ENDIF
260800170619     C*** BUILD
260900170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUILDING'
261000170619     C                   EVAL      CDPBUILD = SkSplitCSV_9(i)
261100170619     C                   ENDIF
261200170619     C*** ISO2
261300170619     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
261400170619     C                   EVAL      CDPISO2  = SkSplitCSV_9(i)
261500170619     C                   ENDIF
261600170619     C*** STATE
261700170619     C                   IF        %trim(SkSplitFLD(i)) = 'STATE'
261800170619     C                   EVAL      CDPSTATE = SkSplitCSV_9(i)
261900170619     C                   ENDIF
262000170619     C*** PTC
262100170619     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODE'
262200170619     C                   EVAL      CDPPTC   = SkSplitCSV_9(i)
262300170619     C                   ENDIF
262400170619     C*** CITY
262500170619     C                   IF        %trim(SkSplitFLD(i)) = 'CITYNAME'
262600170619     C                   EVAL      CDPCITY  = SkSplitCSV_9(i)
262700170619     C                   ENDIF
262800170619     C*** TEL
262900170619     C                   IF        %trim(SkSplitFLD(i)) = 'PHONE'
263000170619     C                   EVAL      CDPTEL   = SkSplitCSV_9(i)
263100170619     C                   ENDIF
263200170619     C*** FAX
263300170619     C                   IF        %trim(SkSplitFLD(i)) = 'FAX'
263400170619     C                   EVAL      CDPFAX   = SkSplitCSV_9(i)
263500170619     C                   ENDIF
263600170619     C*** EMAIL
263700170619     C                   IF        %trim(SkSplitFLD(i)) = 'MAIL'
263800170619     C                   EVAL      CDPEMAIL = SkSplitCSV_9(i)
263900170619     C                   ENDIF
264000170619     C*** WEB
264100170619     C                   IF        %trim(SkSplitFLD(i)) = 'WEB'
264200170619     C                   EVAL      CDPWEB   = SkSplitCSV_9(i)
264300170619     C                   ENDIF
264400170619     C*** BUCN
264500170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
264600170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
264700170619     C                   EXSR      CHKNUM
264800170619     C                   IF        PiInt=*on
264900170619     C                   Z-ADD     PiVal         CDPBUCN
265000170619     C                   ENDIF
265100170619     C                   ENDIF
265200170619     C*** GEOX
265300170619     C                   IF        %trim(SkSplitFLD(i)) = 'SGPSLAT'
265400170619     C                   EVAL      CDPGEOX  = SkSplitCSV_9(i)
265500170619     C                   ENDIF
265600170619     C*** GEOY
265700170619     C                   IF        %trim(SkSplitFLD(i)) = 'SGPSLONG'
265800170619     C                   EVAL      CDPGEOY  = SkSplitCSV_9(i)
265900170619     C                   ENDIF
266000170619     C*** DUMY
266100170619     C                   IF        %trim(SkSplitFLD(i)) = 'DUMMYDEPOT'
266200170619     C                   EVAL      CDPDUMY  = SkSplitCSV_9(i)
266300170619     C                   ENDIF
266400170619     C*
266500170619     C                   ENDIF
266600170619     C                   EVAL      i = i + 1
266700170619     C                   ENDDO
266800170619     C*
266900170619     C*** VER
267000170706     C                   EVAL      CDPVER  = NEW_VERSION
267100170619     C*
267200170920     C** caricamento file LEGAMI depot DPD Linea/Cliente
267300170920     c                   if          CDPISO2 <> *blank
267400170920     c                   call      'TISIDLEGR'
267500170920     c                   parm                    CDPDPTN
267600170920     c                   parm                    CDPISO2
267700170929     c                   parm                    CVEDDE
267800170929     c                   parm                    CVEDSC
267900170920     c                   end
268000170920     C*
268100170619     C* Infine inserisco il record corrente
268200170619     C                   WRITE     DPCDP100
268300170619     C*
268400170619     C                   ENDSR
268500170619     C***
268600170619
268700170619
268800170619     C*----------------------------------------------------*
268900170619     C*  GESTIONE MAPPATURA "R1DBU"
269000170619     C*----------------------------------------------------*
269100170619     C     GESCR1        BEGSR
269200170619     C*
269300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
269400170619     C                   CLEAR                   DPCR1100
269500170619     C*
269600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
269700170619     C                   Z-ADD     2             i                 4 0
269800170620     C                   DOW       i <= %elem(SkSplitFLD)
269900170620     C                             AND i <= OutIdxMax
270000170619     C*
270100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
270200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
270300170619     C                             SkSplitCSV_9(i) <> *blanks
270400170621     C*
270500170621     C*** ROUT / ROUPB / ROUPE
270600170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
270700170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
270800170619     C                   ENDIF
270900170619     C*** TRADS
271000170619     C                   IF        %trim(SkSplitFLD(i)) = 'TRADESTATUS'
271100170619     C                   EVAL      CR1TRADS = SkSplitCSV_9(i)
271200170619     C                   ENDIF
271300170619     C*** ISO2
271400170619     C                   IF        %trim(SkSplitFLD(i)) =
271500170619     C                                        'DESTINATIONCOUNTRYCODE'
271600170619     C                   EVAL      CR1ISO2  = SkSplitCSV_9(i)
271700170619     C                   ENDIF
271800170619     C*** PTCB
271900170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
272000170619     C                   EVAL      CR1PTCB  = SkSplitCSV_9(i)
272100170619     C                   ENDIF
272200170619     C*** PTCE
272300170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
272400170619     C                   EVAL      CR1PTCE  = SkSplitCSV_9(i)
272500170619     C                   ENDIF
272600170619     C*** NTWP
272700170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
272800170619     C                   EVAL      CR1NTWP  = SkSplitCSV_9(i)
272900170619     C                   ENDIF
273000170619     C*** PTY
273100170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
273200170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
273300170619     C                   EXSR      CHKNUM
273400170619     C                   IF        PiInt=*on
273500170619     C                   Z-ADD     PiVal         CR1PTY
273600170619     C                   ENDIF
273700170619     C                   ENDIF
273800170619     C*** BUCN
273900170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
274000170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
274100170619     C                   EXSR      CHKNUM
274200170619     C                   IF        PiInt=*on
274300170619     C                   Z-ADD     PiVal         CR1BUCN
274400170619     C                   ENDIF
274500170619     C                   ENDIF
274600170619     C*
274700170619     C                   ENDIF
274800170619     C                   EVAL      i = i + 1
274900170619     C                   ENDDO
275000170619     C*
275100170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
275200170619     C                   IF        CR1PTCE = *blanks
275300170619     C                   EVAL      CR1PTCE = CR1PTCB
275400170619     C                   ENDIF
275500170619     C*
275600170619     C*** VER
275700170619     C                   EVAL      CR1VER = NEW_VERSION
275800170620     C*
275900170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
276000170620     C                   EVAL      InStringa = w2048_01
276100170620     C                   EVAL      InSepara = ','
276200170620     C                   SETOFF                                       131419
276300170620     C                   SETON                                        13
276400170620     C                   EXSR      SR_SPLIT
276500170620     C*
276600170620     C* Se nessun elemento => scrivo comunque un record singolo
276700170620     C                   IF        OutIdxMax = *zeros
276800170620     C                   WRITE     DPCR1100
276900170620     C                   ELSE
277000170620     C*
277100170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
277200170620     C                   Z-ADD     1             j                 4 0
277300170620     C                   DOW       j <= %elem(SkSplitCSV_3)
277400170620     C                             AND j <= OutIdxMax
277500170620     C*
277600170620     C* Determino il tipo di Routing Place
277700170620     C                   SELECT
277800170620     C* Injection Code - Range
277900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
278000170620     C                   EVAL      CR1ROUT  = 'I'
278100170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):3:9)
278200170620     C                   EVAL      CR1ROUPE = %subst(SkSplitCSV_3(j):12:9)
278300170620     C* Injection Code - Single
278400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
278500170620     C                   EVAL      CR1ROUT  = 'I'
278600170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:9)
278700170620     C                   EVAL      CR1ROUPE = CR1ROUPB
278800170620     C* Country Code
278900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
279000170620     C                   EVAL      CR1ROUT  = 'C'
279100170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:2)
279200170620     C                   EVAL      CR1ROUPE = CR1ROUPB
279300170620     C* Depot Code - Range
279400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
279500170620     C                   EVAL      CR1ROUT  = 'D'
279600170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):3:7)
279700170620     C                   EVAL      CR1ROUPE = %subst(SkSplitCSV_3(j):10:7)
279800170620     C* Depot Code - Single
279900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
280000170620     C                   EVAL      CR1ROUT  = 'D'
280100170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:7)
280200170620     C                   EVAL      CR1ROUPE = CR1ROUPB
280300170620     C* Depot Group
280400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
280500170620     C                   EVAL      CR1ROUT  = 'G'
280600170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:4)
280700170620     C                   EVAL      CR1ROUPE = CR1ROUPB
280800170620     C* Business Unit
280900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
281000170620     C                   EVAL      CR1ROUT  = 'B'
281100170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:3)
281200170620     C                   EVAL      CR1ROUPE = CR1ROUPB
281300170620     C*
281400170620     C                   ENDSL
281500170619     C*
281600170619     C* Infine inserisco il record corrente
281700170619     C                   WRITE     DPCR1100
281800170620     C*
281900170620     C                   EVAL      j = j + 1
282000170620     C                   ENDDO
282100170620     C                   ENDIF
282200170619     C*
282300170619     C                   ENDSR
282400170619     C***
282500170619
282600170619
282700170619     C*----------------------------------------------------*
282800170619     C*  GESTIONE MAPPATURA "R2ROUTE"
282900170619     C*----------------------------------------------------*
283000170619     C     GESCR2        BEGSR
283100170619     C*
283200170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
283300170619     C                   CLEAR                   DPCR2100
283400170619     C*
283500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
283600170619     C                   Z-ADD     2             i                 4 0
283700170620     C                   DOW       i <= %elem(SkSplitFLD)
283800170620     C                             AND i <= OutIdxMax
283900170619     C*
284000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
284100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
284200170619     C                             SkSplitCSV_9(i) <> *blanks
284300170619     C*
284400170621     C*** ROUT / ROUPB / ROUPE
284500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
284600170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
284700170619     C                   ENDIF
284800170619     C*** BUCN
284900170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
285000170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
285100170619     C                   EXSR      CHKNUM
285200170619     C                   IF        PiInt=*on
285300170619     C                   Z-ADD     PiVal         CR2BUCN
285400170619     C                   ENDIF
285500170619     C                   ENDIF
285600170619     C*** ISO2
285700170619     C                   IF        %trim(SkSplitFLD(i)) =
285800170619     C                                        'DESTINATIONCOUNTRYCODE'
285900170619     C                   EVAL      CR2ISO2  = SkSplitCSV_9(i)
286000170619     C                   ENDIF
286100170619     C*** TRADS
286200170619     C                   IF        %trim(SkSplitFLD(i)) = 'TRADESTATUS'
286300170619     C                   EVAL      CR2TRADS = SkSplitCSV_9(i)
286400170619     C                   ENDIF
286500170619     C*** PTCB
286600170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
286700170619     C                   EVAL      CR2PTCB  = SkSplitCSV_9(i)
286800170619     C                   ENDIF
286900170619     C*** PTCE
287000170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
287100170619     C                   EVAL      CR2PTCE  = SkSplitCSV_9(i)
287200170619     C                   ENDIF
287300170619     C*** NTWP
287400170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
287500170619     C                   EVAL      CR2NTWP  = SkSplitCSV_9(i)
287600170619     C                   ENDIF
287700170619     C*** PTY
287800170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
287900170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
288000170619     C                   EXSR      CHKNUM
288100170619     C                   IF        PiInt=*on
288200170619     C                   Z-ADD     PiVal         CR2PTY
288300170619     C                   ENDIF
288400170619     C                   ENDIF
288500170619     C*** DDPT
288600170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
288700170619     C                   EVAL      CR2DDPT  = SkSplitCSV_9(i)
288800170619     C                   ENDIF
288900170619     C*** SSORT
289000170619     C                   IF        %trim(SkSplitFLD(i)) = 'SSORT'
289100170619     C                   EVAL      CR2SSORT = SkSplitCSV_9(i)
289200170619     C                   ENDIF
289300170619     C*** PTNC
289400170619     C                   IF        %trim(SkSplitFLD(i)) = 'PARTNERCODE'
289500170619     C                   EVAL      CR2PTNC  = SkSplitCSV_9(i)
289600170619     C                   ENDIF
289700170619     C*
289800170619     C                   ENDIF
289900170619     C                   EVAL      i = i + 1
290000170619     C                   ENDDO
290100170619     C*
290200170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
290300170619     C                   IF        CR2PTCE = *blanks
290400170619     C                   EVAL      CR2PTCE = CR2PTCB
290500170619     C                   ENDIF
290600170619     C*
290700170619     C*** VER
290800170619     C                   EVAL      CR2VER = NEW_VERSION
290900170620     C*
291000170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
291100170620     C                   EVAL      InStringa = w2048_01
291200170620     C                   EVAL      InSepara = ','
291300170620     C                   SETOFF                                       131419
291400170620     C                   SETON                                        13
291500170620     C                   EXSR      SR_SPLIT
291600170620     C*
291700170620     C* Se nessun elemento => scrivo comunque un record singolo
291800170620     C                   IF        OutIdxMax = *zeros
291900170620     C                   WRITE     DPCR2100
292000170620     C                   ELSE
292100170620     C*
292200170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
292300170620     C                   Z-ADD     1             j                 4 0
292400170620     C                   DOW       j <= %elem(SkSplitCSV_3)
292500170620     C                             AND j <= OutIdxMax
292600170620     C*
292700170620     C* Determino il tipo di Routing Place
292800170620     C                   SELECT
292900170620     C* Injection Code - Range
293000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
293100170620     C                   EVAL      CR2ROUT  = 'I'
293200170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):3:9)
293300170620     C                   EVAL      CR2ROUPE = %subst(SkSplitCSV_3(j):12:9)
293400170620     C* Injection Code - Single
293500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
293600170620     C                   EVAL      CR2ROUT  = 'I'
293700170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:9)
293800170620     C                   EVAL      CR2ROUPE = CR2ROUPB
293900170620     C* Country Code
294000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
294100170620     C                   EVAL      CR2ROUT  = 'C'
294200170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:2)
294300170620     C                   EVAL      CR2ROUPE = CR2ROUPB
294400170620     C* Depot Code - Range
294500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
294600170620     C                   EVAL      CR2ROUT  = 'D'
294700170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):3:7)
294800170620     C                   EVAL      CR2ROUPE = %subst(SkSplitCSV_3(j):10:7)
294900170620     C* Depot Code - Single
295000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
295100170620     C                   EVAL      CR2ROUT  = 'D'
295200170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:7)
295300170620     C                   EVAL      CR2ROUPE = CR2ROUPB
295400170620     C* Depot Group
295500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
295600170620     C                   EVAL      CR2ROUT  = 'G'
295700170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:4)
295800170620     C                   EVAL      CR2ROUPE = CR2ROUPB
295900170620     C* Business Unit
296000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
296100170620     C                   EVAL      CR2ROUT  = 'B'
296200170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:3)
296300170620     C                   EVAL      CR2ROUPE = CR2ROUPB
296400170620     C*
296500170620     C                   ENDSL
296600170620     C*
296700170620     C* Infine inserisco il record corrente
296800170620     C                   WRITE     DPCR2100
296900170620     C*
297000170620     C                   EVAL      j = j + 1
297100170620     C                   ENDDO
297200170620     C                   ENDIF
297300170619     C*
297400170619     C                   ENDSR
297500170619     C***
297600170619
297700170619
297800170619     C*----------------------------------------------------*
297900170619     C*  GESTIONE MAPPATURA "R3OSORT"
298000170619     C*----------------------------------------------------*
298100170619     C     GESCR3        BEGSR
298200170619     C*
298300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
298400170619     C                   CLEAR                   DPCR3100
298500170619     C*
298600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
298700170619     C                   Z-ADD     2             i                 4 0
298800170620     C                   DOW       i <= %elem(SkSplitFLD)
298900170620     C                             AND i <= OutIdxMax
299000170619     C*
299100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
299200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
299300170619     C                             SkSplitCSV_9(i) <> *blanks
299400170619     C*
299500170621     C*** ROUT / ROUPB / ROUPE
299600170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
299700170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
299800170619     C                   ENDIF
299900170619     C*** ISO2
300000170619     C                   IF        %trim(SkSplitFLD(i)) =
300100170619     C                                        'DESTINATIONCOUNTRYCODE'
300200170619     C                   EVAL      CR3ISO2  = SkSplitCSV_9(i)
300300170619     C                   ENDIF
300400170619     C*** DDPT
300500170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
300600170619     C                   EVAL      CR3DDPT  = SkSplitCSV_9(i)
300700170619     C                   ENDIF
300800170619     C*** PTCB
300900170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
301000170619     C                   EVAL      CR3PTCB  = SkSplitCSV_9(i)
301100170619     C                   ENDIF
301200170619     C*** PTCE
301300170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
301400170619     C                   EVAL      CR3PTCE  = SkSplitCSV_9(i)
301500170619     C                   ENDIF
301600170619     C*** NTWP
301700170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
301800170619     C                   EVAL      CR3NTWP  = SkSplitCSV_9(i)
301900170619     C                   ENDIF
302000170619     C*** PTY
302100170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
302200170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
302300170619     C                   EXSR      CHKNUM
302400170619     C                   IF        PiInt=*on
302500170619     C                   Z-ADD     PiVal         CR3PTY
302600170619     C                   ENDIF
302700170619     C                   ENDIF
302800170619     C*** OSORT
302900170619     C                   IF        %trim(SkSplitFLD(i)) = 'OSORT'
303000170620     C                   EVAL      CR3OSORT = SkSplitCSV_9(i)
303100170619     C                   ENDIF
303200170619     C*
303300170619     C                   ENDIF
303400170619     C                   EVAL      i = i + 1
303500170619     C                   ENDDO
303600170619     C*
303700170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
303800170619     C                   IF        CR3PTCE = *blanks
303900170619     C                   EVAL      CR3PTCE = CR3PTCB
304000170619     C                   ENDIF
304100170619     C*
304200170619     C*** VER
304300170619     C                   EVAL      CR3VER = NEW_VERSION
304400170620     C*
304500170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
304600170620     C                   EVAL      InStringa = w2048_01
304700170620     C                   EVAL      InSepara = ','
304800170620     C                   SETOFF                                       131419
304900170620     C                   SETON                                        13
305000170620     C                   EXSR      SR_SPLIT
305100170620     C*
305200170620     C* Se nessun elemento => scrivo comunque un record singolo
305300170620     C                   IF        OutIdxMax = *zeros
305400170620     C                   WRITE     DPCR3100
305500170620     C                   ELSE
305600170620     C*
305700170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
305800170620     C                   Z-ADD     1             j                 4 0
305900170620     C                   DOW       j <= %elem(SkSplitCSV_3)
306000170620     C                             AND j <= OutIdxMax
306100170620     C*
306200170620     C* Determino il tipo di Routing Place
306300170620     C                   SELECT
306400170620     C* Injection Code - Range
306500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
306600170620     C                   EVAL      CR3ROUT  = 'I'
306700170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):3:9)
306800170620     C                   EVAL      CR3ROUPE = %subst(SkSplitCSV_3(j):12:9)
306900170620     C* Injection Code - Single
307000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
307100170620     C                   EVAL      CR3ROUT  = 'I'
307200170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:9)
307300170620     C                   EVAL      CR3ROUPE = CR3ROUPB
307400170620     C* Country Code
307500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
307600170620     C                   EVAL      CR3ROUT  = 'C'
307700170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:2)
307800170620     C                   EVAL      CR3ROUPE = CR3ROUPB
307900170620     C* Depot Code - Range
308000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
308100170620     C                   EVAL      CR3ROUT  = 'D'
308200170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):3:7)
308300170620     C                   EVAL      CR3ROUPE = %subst(SkSplitCSV_3(j):10:7)
308400170620     C* Depot Code - Single
308500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
308600170620     C                   EVAL      CR3ROUT  = 'D'
308700170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:7)
308800170620     C                   EVAL      CR3ROUPE = CR3ROUPB
308900170620     C* Depot Group
309000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
309100170620     C                   EVAL      CR3ROUT  = 'G'
309200170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:4)
309300170620     C                   EVAL      CR3ROUPE = CR3ROUPB
309400170620     C* Business Unit
309500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
309600170620     C                   EVAL      CR3ROUT  = 'B'
309700170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:3)
309800170620     C                   EVAL      CR3ROUPE = CR3ROUPB
309900170620     C*
310000170620     C                   ENDSL
310100170620     C*
310200170620     C* Infine inserisco il record corrente
310300170620     C                   WRITE     DPCR3100
310400170620     C*
310500170620     C                   EVAL      j = j + 1
310600170620     C                   ENDDO
310700170620     C                   ENDIF
310800170619     C*
310900170619     C                   ENDSR
311000170619     C***
311100170619
311200170619
311300170619     C*----------------------------------------------------*
311400170619     C*  GESTIONE MAPPATURA "R4CSORT"
311500170619     C*----------------------------------------------------*
311600170619     C     GESCR4        BEGSR
311700170619     C*
311800170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
311900170619     C                   CLEAR                   DPCR4100
312000170619     C*
312100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
312200170619     C                   Z-ADD     2             i                 4 0
312300170620     C                   DOW       i <= %elem(SkSplitFLD)
312400170620     C                             AND i <= OutIdxMax
312500170619     C*
312600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
312700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
312800170619     C                             SkSplitCSV_9(i) <> *blanks
312900170619     C*
313000170621     C*** ROUT / ROUPB / ROUPE
313100170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
313200170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
313300170619     C                   ENDIF
313400170619     C*** ISO2
313500170619     C                   IF        %trim(SkSplitFLD(i)) =
313600170619     C                                        'DESTINATIONCOUNTRYCODE'
313700170619     C                   EVAL      CR4ISO2  = SkSplitCSV_9(i)
313800170619     C                   ENDIF
313900170619     C*** DDPT
314000170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
314100170619     C                   EVAL      CR4DDPT  = SkSplitCSV_9(i)
314200170619     C                   ENDIF
314300170619     C*** NTWP
314400170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
314500170619     C                   EVAL      CR4NTWP  = SkSplitCSV_9(i)
314600170619     C                   ENDIF
314700170619     C*** PTY
314800170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
314900170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
315000170619     C                   EXSR      CHKNUM
315100170619     C                   IF        PiInt=*on
315200170619     C                   Z-ADD     PiVal         CR4PTY
315300170619     C                   ENDIF
315400170619     C                   ENDIF
315500170619     C*** CSORT
315600170619     C                   IF        %trim(SkSplitFLD(i)) = 'CSORT'
315700170620     C                   EVAL      CR4CSORT = SkSplitCSV_9(i)
315800170619     C                   ENDIF
315900170619     C*
316000170619     C                   ENDIF
316100170619     C                   EVAL      i = i + 1
316200170619     C                   ENDDO
316300170619     C*
316400170619     C*** VER
316500170619     C                   EVAL      CR4VER = NEW_VERSION
316600170620     C*
316700170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
316800170620     C                   EVAL      InStringa = w2048_01
316900170620     C                   EVAL      InSepara = ','
317000170620     C                   SETOFF                                       131419
317100170620     C                   SETON                                        13
317200170620     C                   EXSR      SR_SPLIT
317300170620     C*
317400170620     C* Se nessun elemento => scrivo comunque un record singolo
317500170620     C                   IF        OutIdxMax = *zeros
317600170620     C                   WRITE     DPCR4100
317700170620     C                   ELSE
317800170620     C*
317900170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
318000170620     C                   Z-ADD     1             j                 4 0
318100170620     C                   DOW       j <= %elem(SkSplitCSV_3)
318200170620     C                             AND j <= OutIdxMax
318300170620     C*
318400170620     C* Determino il tipo di Routing Place
318500170620     C                   SELECT
318600170620     C* Injection Code - Range
318700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
318800170620     C                   EVAL      CR4ROUT  = 'I'
318900170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):3:9)
319000170620     C                   EVAL      CR4ROUPE = %subst(SkSplitCSV_3(j):12:9)
319100170620     C* Injection Code - Single
319200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
319300170620     C                   EVAL      CR4ROUT  = 'I'
319400170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:9)
319500170620     C                   EVAL      CR4ROUPE = CR4ROUPB
319600170620     C* Country Code
319700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
319800170620     C                   EVAL      CR4ROUT  = 'C'
319900170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:2)
320000170620     C                   EVAL      CR4ROUPE = CR4ROUPB
320100170620     C* Depot Code - Range
320200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
320300170620     C                   EVAL      CR4ROUT  = 'D'
320400170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):3:7)
320500170620     C                   EVAL      CR4ROUPE = %subst(SkSplitCSV_3(j):10:7)
320600170620     C* Depot Code - Single
320700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
320800170620     C                   EVAL      CR4ROUT  = 'D'
320900170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:7)
321000170620     C                   EVAL      CR4ROUPE = CR4ROUPB
321100170620     C* Depot Group
321200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
321300170620     C                   EVAL      CR4ROUT  = 'G'
321400170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:4)
321500170620     C                   EVAL      CR4ROUPE = CR4ROUPB
321600170620     C* Business Unit
321700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
321800170620     C                   EVAL      CR4ROUT  = 'B'
321900170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:3)
322000170620     C                   EVAL      CR4ROUPE = CR4ROUPB
322100170620     C*
322200170620     C                   ENDSL
322300170620     C*
322400170620     C* Infine inserisco il record corrente
322500170620     C                   WRITE     DPCR4100
322600170620     C*
322700170620     C                   EVAL      j = j + 1
322800170620     C                   ENDDO
322900170620     C                   ENDIF
323000170619     C*
323100170619     C                   ENDSR
323200170619     C***
323300170619
323400170619
323500170619     C*----------------------------------------------------*
323600170619     C*  GESTIONE MAPPATURA "R5DSORT"
323700170619     C*----------------------------------------------------*
323800170619     C     GESCR5        BEGSR
323900170619     C*
324000170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
324100170619     C                   CLEAR                   DPCR5100
324200170619     C*
324300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
324400170619     C                   Z-ADD     2             i                 4 0
324500170620     C                   DOW       i <= %elem(SkSplitFLD)
324600170620     C                             AND i <= OutIdxMax
324700170619     C*
324800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
324900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
325000170619     C                             SkSplitCSV_9(i) <> *blanks
325100170619     C*
325200170621     C*** ROUT / ROUPB / ROUPE
325300170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
325400170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
325500170619     C                   ENDIF
325600170619     C*** DDPT
325700170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
325800170619     C                   EVAL      CR5DDPT  = SkSplitCSV_9(i)
325900170619     C                   ENDIF
326000170619     C*** ISO2
326100170619     C                   IF        %trim(SkSplitFLD(i)) =
326200170619     C                                        'DESTINATIONCOUNTRYCODE'
326300170619     C                   EVAL      CR5ISO2  = SkSplitCSV_9(i)
326400170619     C                   ENDIF
326500170619     C*** PTCB
326600170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
326700170619     C                   EVAL      CR5PTCB  = SkSplitCSV_9(i)
326800170619     C                   ENDIF
326900170619     C*** PTCE
327000170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
327100170619     C                   EVAL      CR5PTCE  = SkSplitCSV_9(i)
327200170619     C                   ENDIF
327300170619     C*** NTWP
327400170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
327500170619     C                   EVAL      CR5NTWP  = SkSplitCSV_9(i)
327600170619     C                   ENDIF
327700170619     C*** PTY
327800170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
327900170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
328000170619     C                   EXSR      CHKNUM
328100170619     C                   IF        PiInt=*on
328200170619     C                   Z-ADD     PiVal         CR5PTY
328300170619     C                   ENDIF
328400170619     C                   ENDIF
328500170619     C*** DSORT
328600170619     C                   IF        %trim(SkSplitFLD(i)) = 'DSORT'
328700170629     C                   EVAL      CR5DSORT = %trim(SkSplitCSV_9(i))
328800170619     C                   ENDIF
328900170619     C*
329000170619     C                   ENDIF
329100170619     C                   EVAL      i = i + 1
329200170619     C                   ENDDO
329300170619     C*
329400170619     C*** VER
329500170621     C                   EVAL      CR5VER = NEW_VERSION
329600170620     C*
329700170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
329800170620     C                   EVAL      InStringa = w2048_01
329900170620     C                   EVAL      InSepara = ','
330000170620     C                   SETOFF                                       131419
330100170620     C                   SETON                                        13
330200170620     C                   EXSR      SR_SPLIT
330300170620     C*
330400170620     C* Se nessun elemento => scrivo comunque un record singolo
330500170620     C                   IF        OutIdxMax = *zeros
330600170620     C                   WRITE     DPCR5100
330700170620     C                   ELSE
330800170620     C*
330900170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
331000170620     C                   Z-ADD     1             j                 4 0
331100170620     C                   DOW       j <= %elem(SkSplitCSV_3)
331200170620     C                             AND j <= OutIdxMax
331300170620     C*
331400170620     C* Determino il tipo di Routing Place
331500170620     C                   SELECT
331600170620     C* Injection Code - Range
331700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
331800170620     C                   EVAL      CR5ROUT  = 'I'
331900170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):3:9)
332000170620     C                   EVAL      CR5ROUPE = %subst(SkSplitCSV_3(j):12:9)
332100170620     C* Injection Code - Single
332200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
332300170620     C                   EVAL      CR5ROUT  = 'I'
332400170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:9)
332500170620     C                   EVAL      CR5ROUPE = CR5ROUPB
332600170620     C* Country Code
332700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
332800170620     C                   EVAL      CR5ROUT  = 'C'
332900170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:2)
333000170620     C                   EVAL      CR5ROUPE = CR5ROUPB
333100170620     C* Depot Code - Range
333200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
333300170620     C                   EVAL      CR5ROUT  = 'D'
333400170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):3:7)
333500170620     C                   EVAL      CR5ROUPE = %subst(SkSplitCSV_3(j):10:7)
333600170620     C* Depot Code - Single
333700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
333800170620     C                   EVAL      CR5ROUT  = 'D'
333900170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:7)
334000170620     C                   EVAL      CR5ROUPE = CR5ROUPB
334100170620     C* Depot Group
334200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
334300170620     C                   EVAL      CR5ROUT  = 'G'
334400170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:4)
334500170620     C                   EVAL      CR5ROUPE = CR5ROUPB
334600170620     C* Business Unit
334700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
334800170620     C                   EVAL      CR5ROUT  = 'B'
334900170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:3)
335000170620     C                   EVAL      CR5ROUPE = CR5ROUPB
335100170620     C*
335200170620     C                   ENDSL
335300170620     C*
335400170620     C* Infine inserisco il record corrente
335500170620     C                   WRITE     DPCR5100
335600170620     C*
335700170620     C                   EVAL      j = j + 1
335800170620     C                   ENDDO
335900170620     C                   ENDIF
336000170619     C*
336100170619     C                   ENDSR
336200170620     C***
336300170616
336400170616
336500141028     C*----------------------------------------------------*
336600141028     C*  CONTROLLO NUMERICITA' CAMPI
336700141028     C*----------------------------------------------------*
336800141028     C     CHKNUM        BEGSR
336900141028     C*
337000141028     C                   IF        PiDecChr = *blanks
337100141028     C                   EVAL      PiDecChr = CharNUM
337200141028     C                   ENDIF
337300141028     C*
337400141028     C                   callp     UBISNUM_Check(PiStr
337500141028     C                                          :PiDecChr
337600141028     C                                          :PiVal
337700141028     C                                          :PiNum
337800141028     C                                          :PiInt)
337900141028     C*
338000141028     C                   ENDSR
338100141028     C***
338200141028
338300141028
338400141028     C*----------------------------------------------------*
338500141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
338600141028     C*----------------------------------------------------*
338700141028     C     SR_SPLIT      BEGSR
338800141028     C*
338900141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
339000170620     C   13              EVAL      InTypeOut = '3'
339100170620     C   14              EVAL      InTypeOut = '4'
339200170620     C   19              EVAL      InTypeOut = '9'
339300170616     C                   CALL      'XSPLIT3'
339400141028     C                   PARM                    InStringa
339500141028     C                   PARM                    InSepara
339600170616     C                   PARM                    InTypeOut
339700141028     C                   PARM      ''            wSkParam
339800170620     C                   PARM                    OutIdxMax
339900141028     C                   PARM                    OutErrore
340000141028     C                   PARM                    OutMsg
340100170620     C   13              MOVEA     wSkParam      SkSplitCSV_3
340200170620     C   14              MOVEA     wSkParam      SkSplitCSV_4
340300170620     C   19              MOVEA     wSkParam      SkSplitCSV_9
340400141028     C*
340500141028     C                   ENDSR
340600160801
340700160801
340800160801     C*--------------------------------------------------------
340900160801     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
341000160801     C*--------------------------------------------------------
341100160801     C     CARTAB        BEGSR
341200160801     C*
341300160801     C* TABELLA '15' - NAZIONI
341400160801     C                   clear                   skNAZISO
341500160801     C                   clear                   skNAZBAR
341600160801     C                   eval      tblKUT = 1
341700160801     C                   eval      tblCOD = '15'
341800170616     C     KEYtab_P      setll     tabel00f
341900170616     C     KEYtab_P      reade     tabel00f
342000160801     C                   dow       not %eof(tabel00f)
342100160801     C                   if        tblFLG = *blanks
342200160801     C                   movel(p)  tblUNI        ds15
342300160801     C                   add       1             jNAZ
342400160801     C                   eval      skNAZBAR(jNAZ) = tblKEY
342500170620     C                   eval      skNAZISO(jNAZ) = %trim(§15COD)
342600160801     C                   endif
342700170616     C     KEYtab_P      reade     tabel00f
342800160801     C                   enddo
342900160801     C*
343000160801     C                   ENDSR
343100160801     C***
343200170622
343300170622
343400170622     C*--------------------------------------------------------------------------------------------*
343500170622     C* SNDALERT - Invio email di alert al assistenza IT
343600170622     C*--------------------------------------------------------------------------------------------*
343700170622     C     sndAlert      BEGSR
343800170622     C*
343900170622     C* Inizializzo variabili
344000170622     C                   movel     *blanks       wrkEml          253
344100170622     C                   movel     *blanks       wrkCcEml        253
344200170622     C                   movel     *blanks       wrkEmlMsg      5000
344300170622     C                   movel     *blanks       wrkEmlOgg        44
344400170622     C*
344500170622     C* Valorizzo i campi della e-m@ail
344600170622     C                   eval      wrkEml='cedalert@brt.it'
344700171025     C                   eval      wrkCcEml='andrea.bertocchi@brt.it'
344800170622     C                   eval      wrkEmlOgg=
344900170622     C                               'DPD GeoRDB 2017 - Importazione '         +
345000170622     C                               'VERSIONE: ' + %editc(NEW_VERSION:'X')
345100170622     C                   EVAL      wrkEmlMsg = 'Rilevata possibilità '         +
345200170622     C                             'troncamento dati al record: '              +
345300170622     C                                         %editc(currRCD:'4')             +
345400170622     C                                                                    ':/N'+
345500170622     C                                                                    ':/N'
345600170622     C*
345700170622     C                   call(e)   'TIS701C1'
345800170622     C                   parm                    wrkEml
345900170622     C                   parm                    wrkCcEml
346000170622     C                   parm                    wrkEmlOgg
346100170622     C                   parm                    wrkEmlMsg
346200170622     C                   parm                    wrkEsito          1
346300170622     C*
346400170622     C                   ENDSR
346500070411
346600990910
346700000613     C     *inzsr        BEGSR
346800000613     C*
346900170616     C* CALCOLA LA VARIE DATE DI RIFERIMENTO
347000170616     C                   z-add     *zeros        oggi              8 0
347100170616     C                   z-add     *zeros        domani            8 0
347200170616     C                   eval      oggi   = %dec(%date() : *ISO)
347300170616     C                   eval      domani = %dec(%date()+%days(1) : *ISO)
347400160801     C*
347500170616     C* Chiave su TABEL00F - Parziale
347600170616     C     KEYtab_P      KLIST
347700160801     C                   KFLD                    tblKUT
347800160801     C                   KFLD                    tblCOD
347900170616     C*
348000170616     C* Chiave su DPCCC00F - Completa
348100170616     C     KEYccc_11C    KLIST
348200170616     C                   KFLD                    cccATB
348300170616     C                   KFLD                    cccVER
348400170616     C                   KFLD                    cccISO2
348500170616     C*
348600170616     C* Chiave su DPCST00F - Completa
348700170616     C     KEYcst_11C    KLIST
348800170616     C                   KFLD                    cstATB
348900170616     C                   KFLD                    cstVER
349000170616     C                   KFLD                    cstISO2
349100170616     C                   KFLD                    cstREGC
349200170616     C                   KFLD                    cstSTAC
349300170616     C*
349400170616     C* Chiave su DPCEL00F - Completa
349500170616     C     KEYcel_11C    KLIST
349600170616     C                   KFLD                    celATB
349700170616     C                   KFLD                    celVER
349800170616     C                   KFLD                    celELMC
349900170619     C*
350000170619     C* Chiave su DPCSO00F - Completa
350100170619     C     KEYcso_11C    KLIST
350200170619     C                   KFLD                    csoATB
350300170619     C                   KFLD                    csoVER
350400170619     C                   KFLD                    csoSORC
350500000830     C*
350600000613     C                   ENDSR
350700000613     C***
