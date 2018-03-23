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
012300171109     C*  Pulizia dei depot dei legati
012400171109     C                   movel     NEW_VERSION   parver
012500171109     C                   CALL      'TISID0R9C'
012600171109     C                   parm                    parver            5
012700171109      **
012800171016     c                   RETURN
012900171016     C*----------------------------------------------------*
013000171016      **   Legge il WORKFILE (TXT) e lo importa
013100171016     C*----------------------------------------------------*
013200040526     C     RWFILE        BEGSR
013300170620     C*
013400170621     C                   if        not %open(WFDPDRDBF)
013500170621     C                   open      WFDPDRDBF
013600170620     C                   endif
013700150127     C*
013800170621     C                   READ      WFDPDRDBF
013900170621     C                   DOW       not %eof(WFDPDRDBF)
014000000801     C*
014100170621     C                   if        WDPDRDB <> *blanks
014200141028     C*
014300170616     C* "normalizzo" il dato di input corrente
014400170621     C                   eval      wWDPDRDB = %trim(%subst(WDPDRDB:1:
014500170621     C                                        %len(%trim(WDPDRDB))-1))
014600170622     C*
014700170622     C* Verifico rischio dati troncati
014800170622     C                   add       1             currRCD
014900170622     C                   if        %subst(WDPDRDB:1000) <> *blanks
015000170622     C                   exsr      sndAlert
015100170622     C                   endif
015200040910     C*
015300170616     C* Eseguo routine di traduzione
015400170616     C                   exsr      exeTrad
015500171016      *
015600171016      **  per qualsiasi tipo di errore ROLLBACK e torna al chiamante
015700171016     c                   if        ErrFlag ='1'
015800171016     C                   LEAVE
015900171016     C                   endif
016000040802     C*
016100000905     C                   endif
016200000905     C*
016300170621     C***                DELETE    WFDPDRDB0
016400170621     C                   READ      WFDPDRDBF
016500170621     C                   ENDDO
016600170620     C*
016700170621     C                   if        %open(WFDPDRDBF)
016800170621     C                   close     WFDPDRDBF
016900170620     C                   endif
017000990920     C*
017100910830     C                   ENDSR
017200000613     C***
017300141028     C*----------------------------------------------------*
017400141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017500141028     C*----------------------------------------------------*
017600141028     C     INZVAR        BEGSR
017700141028     C*
017800141028     C* Inizializzo variabili di wrk
017900170620     C                   SETOFF                                       131419
018000170620     C                   CLEAR                   SkSplitCSV_3
018100170616     C                   CLEAR                   SkSplitCSV_4
018200170616     C                   CLEAR                   SkSplitCSV_9
018300170621     C                   CLEAR                   wDESxx
018400170620     C                   CLEAR                   wCSTSRTC
018500170620     C                   CLEAR                   w2048_01
018600170621     C                   CLEAR                   w2048_02
018700170621     C                   CLEAR                   w2048_03
018800170621     C                   CLEAR                   w2048_04
018900141028     C*
019000141028     C                   ENDSR
019100170616     C***
019200170616
019300000801     C*----------------------------------------------------*
019400170616     C*  TRADUZIONE DATI
019500000801     C*----------------------------------------------------*
019600170616     C     EXETRAD       BEGSR
019700170616     C*
019800170616     C* Inizializzazioni a liverro di singolo record
019900170616     C                   EXSR      INZVAR
020000170616     C*
020100170616     C* Ignoro i record relativi al "file"
020200170616     C                   SELECT
020300170621     C                   WHEN      %subst(wWDPDRDB:1:1)  = '#' AND
020400170621     C                             %subst(wWDPDRDB:1:4) <> '#DEF'
020500170616     C*
020600170616     C* Carico innanzitutto tutti i record di DEFinizione
020700170621     C                   WHEN      %subst(wWDPDRDB:1:16) = '#DEF:GEOROUTING:'
020800170621     C                   EVAL      InStringa = %subst(wWDPDRDB:16+1)
020900170616     C                   EVAL      InSepara = CharCSV
021000170620     C                   SETOFF                                       131419
021100170616     C                   SETON                                        14
021200170616     C                   EXSR      SR_SPLIT
021300170616     C                   EVAL      skDEF = %subarr(SkSplitCSV_4:1)
021400170616     C                   ADD       1             jDEFS
021500170616     C                   EVAL      skDEFKEY(jDEFS) = skDEF(1)
021600170620     C                   EVAL      skDEFS(jDEFS)   = DS_DEF
021700170620     C*
021800170616     C* Elaboro i record di dati
021900170616     C                   OTHER
022000170616     C*
022100170616     C* Splitto il record di dati corrrente
022200170621     C                   EVAL      InStringa = %trim(wWDPDRDB)
022300170616     C                   EVAL      InSepara = CharCSV
022400170620     C                   SETOFF                                       131419
022500170616     C                   SETON                                        19
022600170616     C                   EXSR      SR_SPLIT
022700170616     C*
022800170616     C* Ogni record di dati lo "mappo" tramite la propria struttura dati
022900170616     C                   EXSR      MAPPADATI
023000170616     C*
023100170616     C                   ENDSL
023200170616     C*
023300170616     C                   ENDSR
023400170616     C***
023500170616
023600170616
023700170616     C*----------------------------------------------------*
023800170616     C*  GESTIONE MAPPATURE DATI
023900170616     C*----------------------------------------------------*
024000170616     C     MAPPADATI     BEGSR
024100170616     C*
024200170616     C* Verifico che il tipo record dati corrente risulti già mappato
024300170616     C                   EVAL      currMAP = %trim(SkSplitCSV_9(1))
024400170616     C                   EVAL      jDEFS = %lookup(currMAP:skDEFKEY:1)
024500170616     C*
024600170616     C* Se tipo record dati corrente già mappato => carico relativa mappatura
024700170616     C                   IF        jDEFS > *zeros
024800170620     C                   EVAL      DS_DEF = skDEFS(jDEFS)
024900170616     C                   EVAL      SkSplitFLD = %subarr(skDEF:1)
025000170616     C*
025100170616     C* Rispetto ad ogni tipo record dati => gestisco con relativa mappatura
025200170616     C                   SELECT
025300170616     C                   WHEN      currMAP ='VERSION'
025400170616     C                   EXSR      gesCVE
025500170616     C*
025600170616     C                   WHEN      currMAP ='COUNTRY'
025700170616     C                   EXSR      gesCCC
025800170616     C*
025900170620     C***                WHEN      currMAP ='COUNTRYTRANSLATION'
026000170619     C***                EXSR      gesXXX
026100170616     C*
026200170616     C                   WHEN      currMAP ='COUNTRY.IT'
026300170616     C                   EXSR      gesCCC_IT
026400170616     C*
026500170616     C                   WHEN      currMAP ='COUNTRY.DE'
026600170616     C                   EXSR      gesCCC_DE
026700170616     C*
026800170616     C                   WHEN      currMAP ='COUNTRY.FR'
026900170616     C                   EXSR      gesCCC_FR
027000170616     C*
027100170616     C                   WHEN      currMAP ='COUNTRY.EN'
027200170616     C                   EXSR      gesCCC_EN
027300170616     C*
027400170616     C                   WHEN      currMAP ='BUSINESSUNIT'
027500170616     C                   EXSR      gesCBU
027600170616     C*
027700170616     C                   WHEN      currMAP ='POSTALCODE'
027800170616     C                   EXSR      gesCPC
027900170616     C*
028000170620     C***                WHEN      currMAP ='STATE'
028100170619     C***                EXSR      gesCST
028200170616     C*
028300170620     C***                WHEN      currMAP ='STATE.IT'
028400170619     C***                EXSR      gesCST_IT
028500170616     C*
028600170620     C***                WHEN      currMAP ='STATE.DE'
028700170619     C***                EXSR      gesCST_DE
028800170616     C*
028900170620     C***                WHEN      currMAP ='STATE.FR'
029000170619     C***                EXSR      gesCST_FR
029100170616     C*
029200170620     C***                WHEN      currMAP ='STATE.EN'
029300170619     C***                EXSR      gesCST_EN
029400170616     C*
029500170616     C                   WHEN      currMAP ='DEPOTS'
029600170619     C                   EXSR      gesCDP
029700170616     C*
029800170620     C***                WHEN      currMAP ='ELEMENTS'
029900170619     C***                EXSR      gesCEL
030000170616     C*
030100170620     C***                WHEN      currMAP ='ELEMENTS.IT'
030200170619     C***                EXSR      gesCEL_IT
030300170616     C*
030400170620     C***                WHEN      currMAP ='ELEMENTS.EN'
030500170619     C***                EXSR      gesCEL_EN
030600170616     C*
030700170620     C***                WHEN      currMAP ='ELEMENTS.DE'
030800170619     C***                EXSR      gesCEL_DE
030900170616     C*
031000170620     C***                WHEN      currMAP ='ELEMENTS.FR'
031100170619     C***                EXSR      gesCEL_FR
031200170616     C*
031300170620     C***                WHEN      currMAP ='ELEMENTS.PT'
031400170619     C***                EXSR      gesCEL_PT
031500170619     C*
031600170616     C                   WHEN      currMAP ='SOCODE'
031700170616     C                   EXSR      gesCSO
031800170616     C*
031900170616     C                   WHEN      currMAP ='ASCODE'
032000170619     C                   EXSR      gesCAS
032100170616     C*
032200170616     C                   WHEN      currMAP ='IC'
032300170619     C                   EXSR      gesCIC
032400170616     C*
032500170616     C                   WHEN      currMAP ='NETWORK'
032600170619     C                   EXSR      gesCNT
032700170616     C*
032800170616     C                   WHEN      currMAP ='NETWORKPLAN'
032900170616     C                   EXSR      gesCNP
033000170616     C*
033100170620     C***                WHEN      currMAP ='RCODE'
033200170620     C***                EXSR      gesXXX
033300170616     C*
033400170616     C                   WHEN      currMAP ='ALLOWSO'
033500170619     C                   EXSR      gesCWS
033600170616     C*
033700170616     C                   WHEN      currMAP ='ALLOWAS'
033800170619     C                   EXSR      gesCWA
033900170619     C*
034000170619     C                   WHEN      currMAP ='R1DBU'
034100170619     C                   EXSR      gesCR1
034200170619     C*
034300170619     C                   WHEN      currMAP ='R2ROUTE'
034400170619     C                   EXSR      gesCR2
034500170619     C*
034600170619     C                   WHEN      currMAP ='R3OSORT'
034700170619     C                   EXSR      gesCR3
034800170619     C*
034900170619     C                   WHEN      currMAP ='R4CSORT'
035000170620     C                   EXSR      gesCR4
035100170619     C*
035200170619     C                   WHEN      currMAP ='R5DSORT'
035300170619     C                   EXSR      gesCR5
035400170616     C*
035500170620     C***                WHEN      currMAP ='DEPARTURE'
035600170619     C***                EXSR      gesXXX
035700170616     C*
035800170616     C                   WHEN      currMAP ='RELATION'
035900170619     C                   EXSR      gesCRE
036000170616     C*
036100170616     C                   WHEN      currMAP ='SOSERVICEINFO.IT'
036200170616     C                   EXSR      gesCSO_IT
036300170616     C*
036400170616     C                   WHEN      currMAP ='SOSERVICEINFO.EN'
036500170616     C                   EXSR      gesCSO_EN
036600170616     C*
036700170616     C                   WHEN      currMAP ='SOSERVICEINFO.DE'
036800170616     C                   EXSR      gesCSO_DE
036900170616     C*
037000170616     C                   WHEN      currMAP ='SOSERVICEINFO.FR'
037100170616     C                   EXSR      gesCSO_FR
037200170616     C*
037300170616     C                   WHEN      currMAP ='SOSERVICEINFO.ET'
037400170616     C                   EXSR      gesCSO_ET
037500170616     C*
037600170616     C                   WHEN      currMAP ='SOSERVICEINFO.CS'
037700170616     C                   EXSR      gesCSO_CS
037800170616     C*
037900170616     C                   ENDSL
038000170616     C*
038100170616     C                   ENDIF
038200170616     C*
038300170616     C                   ENDSR
038400170616     C***
038500170616
038600170616
038700170616     C*----------------------------------------------------*
038800170616     C*  GESTIONE MAPPATURA "VERSION"
038900170616     C*----------------------------------------------------*
039000170616     C     GESCVE        BEGSR
039100170616     C*
039200170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
039300170616     C                   CLEAR                   DPCVE100
039400170616     C*
039500170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
039600170616     C                   Z-ADD     2             i                 4 0
039700170620     C                   DOW       i <= %elem(SkSplitFLD)
039800170620     C                             AND i <= OutIdxMax
039900170616     C*
040000170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
040100170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
040200170616     C                             SkSplitCSV_9(i) <> *blanks
040300170616     C*
040400170616     C*** VERD
040500170616     C                   IF        %trim(SkSplitFLD(i)) = 'VERSIONNUMBER'
040600170616     C                   EVAL      CVEVERD = SkSplitCSV_9(i)
040700170616     C                   ENDIF
040800170616     C*** DDE
040900170616     C                   IF        %trim(SkSplitFLD(i)) = 'VALIDFROM'
041000170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
041100170616     C                   EXSR      CHKNUM
041200170616     C                   IF        PiInt=*on
041300170616     C                   Z-ADD     PiVal         CVEDDE
041400170616     C                   ELSE
041500170616     C                   dump(A)
041600170616     C                   return
041700170616     C                   ENDIF
041800170616     C                   ENDIF
041900171016     C*** DSC
042000171016     C                   IF        %trim(SkSplitFLD(i)) = 'VALIDTO'
042100171016     C                   EVAL      PiStr = SkSplitCSV_9(i)
042200171016     C                   EXSR      CHKNUM
042300171016     C                   IF        PiInt=*on
042400171016     C                   Z-ADD     PiVal         CVEDSC
042500171016     C                   ELSE
042600171016     C                   dump(A)
042700171016     C                   return
042800171016     C                   ENDIF
042900171016     C                   ENDIF
043000170929      *
043100171016      *  SE IL CAPPARIO che sta caricando ha una DATA SCADENZA antecedente
043200171016      *   alla DATA di OGGI - Blocca TUTTO
043300171016      *  Esce con ERRORE eseguendo un ROLLBACK di ciò che aveva fatto.
043400171017     c                   If        cveDSC < data_OGGI and cveDSC >0
043500171016     c                   eval         ErrFlag = '1'
043600171016     c                   eval         ErrMsg  = 'Data Scadenza Cappario -
043700171016     c                             ANTECEDENTE oggi. Cappario NON TRADUCIBILE!!'
043800171016     c                   leaveSR
043900171016     c                   End
044000171016     C*
044100170929     C*
044200170616     C*** BCID
044300170616     C                   IF        %trim(SkSplitFLD(i)) = 'BARCODEID'
044400170627     C                   EVAL      CVEBCID = SkSplitCSV_9(i)
044500170616     C                   ENDIF
044600170616     C*
044700170616     C                   ENDIF
044800170616     C                   EVAL      i = i + 1
044900170616     C                   ENDDO
045000171017      *------------
045100171017     C*** DSC
045200171017     C*** Forzo la data di scadenza al 20391231
045300171017     C                   EVAL      CVEDSC = 20391231
045400170616     C*
045500170616     C*** DIM
045600170616     C                   EVAL      CVEDIM = oggi
045700170616     C*
045800170616     C* Reperisco la versione più alta corrente
045900170616     C*
046000170616     C/EXEC SQL
046100170616     C+ SET :MAX_VERSION = (SELECT MAX(CVEVER) FROM DPCVE10F)
046200170616     C/END-EXEC
046300170616     C*
046400170616     C* Stacco il nuovo numero versione BRT come incremento della versione più alta presente
046500170616     C                   EVAL      NEW_VERSION = MAX_VERSION + 1
046600170616     C*
046700170616     C* Effetttuo considerazioni si "sicurezza" sulle date di validità della versione DPD
046800170616     C                   IF        CVEDDE < domani
046900170929     C                   EVAL      CVEDDE = domani
047000171024     C                   END
047100171024     C*
047200171024     C*  pulisce la data Scadenza
047300171024     C                   clear                   DataScad          8 0
047400171024     C                   move      CVEDDE        DataIso
047500171024     c                   subdur    1:*d          DataIso
047600171024     C                   move      DataIso       DataScad
047700170616     C*
047800170616     C* Faccio scadere a oggi tutte le versioni aventi data scadenza
047900170616     C* nel range di validità della versione DPD corrente
048000170616     C*
048100170616     C/EXEC SQL
048200171024     C+ UPDATE DPCVE10F SET CVEDSC = :DataScad
048300170616     C+ WHERE CVEDSC BETWEEN :CVEDDE AND :CVEDSC
048400170616     C/END-EXEC
048500170616     C*
048600170620     C* Quindi lancio driver di "allineamento validità versioni DPD GeoRDB 2017"
048700170616     C*
048800170620     C                   CALL      'TISID3R1'
048900170620     C*
049000170620     C*** VER
049100170620     C                   EVAL      CVEVER = NEW_VERSION
049200170928     C*
049300170616     C* Infine inserisco il record corrente
049400170616     C                   WRITE     DPCVE100
049500170616     C*
049600170616     C                   ENDSR
049700170616     C***
049800170616
049900170616
050000170616     C*----------------------------------------------------*
050100170616     C*  GESTIONE MAPPATURA "COUNTRY"
050200170616     C*----------------------------------------------------*
050300170616     C     GESCCC        BEGSR
050400170616     C*
050500170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
050600170616     C                   CLEAR                   DPCCC100
050700170616     C*
050800170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
050900170616     C                   Z-ADD     2             i                 4 0
051000170620     C                   DOW       i <= %elem(SkSplitFLD)
051100170620     C                             AND i <= OutIdxMax
051200170616     C*
051300170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
051400170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
051500170616     C                             SkSplitCSV_9(i) <> *blanks
051600170616     C*
051700170616     C*** ISO2
051800170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
051900170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
052000170616     C                   ENDIF
052100170616     C*** ISON
052200170616     C                   IF        %trim(SkSplitFLD(i)) = 'ISONUMCOUNTRYCODE'
052300170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
052400170616     C                   EXSR      CHKNUM
052500170616     C                   IF        PiInt=*on
052600170616     C                   Z-ADD     PiVal         CCCISON
052700170616     C                   ENDIF
052800170616     C                   ENDIF
052900170616     C*** ISO3
053000170616     C                   IF        %trim(SkSplitFLD(i)) = 'ISOALPHA3COUNTRYCODE'
053100170616     C                   EVAL      CCCISO3 = SkSplitCSV_9(i)
053200170616     C                   ENDIF
053300170629     C*** PSYS
053400170616     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGPOSTCODENO'
053500170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
053600170616     C                   EXSR      CHKNUM
053700170616     C                   IF        PiInt=*on
053800170629     C                   Z-ADD     PiVal         CCCPSYS
053900170616     C                   ENDIF
054000170616     C                   ENDIF
054100170629     C*** PATTL
054200170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODEPATTERN'
054300170620     C                   EVAL      CCCPATTL = SkSplitCSV_9(i)
054400170616     C                   ENDIF
054500170616     C*** FNS
054600170616     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGNEEDSTATE'
054700170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
054800170616     C                   EXSR      CHKNUM
054900170616     C                   IF        PiInt=*on
055000170616     C                   Z-ADD     PiVal         CCCFNS
055100170616     C                   ENDIF
055200170616     C                   ENDIF
055300170616     C*** PVAL
055400170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODEVALIDATION'
055500170616     C                   EVAL      CCCPVAL = SkSplitCSV_9(i)
055600170616     C                   ENDIF
055700170616     C*** CVAL
055800170616     C                   IF        %trim(SkSplitFLD(i)) = 'CITYVALIDATION'
055900170616     C                   EVAL      CCCCVAL = SkSplitCSV_9(i)
056000170616     C                   ENDIF
056100170616     C*** FREE
056200170616     C                   IF        %trim(SkSplitFLD(i)) = 'FREETRADEAREA'
056300170616     C                   EVAL      CCCFREE = SkSplitCSV_9(i)
056400170616     C                   ENDIF
056500170616     C*** DUMY
056600170616     C                   IF        %trim(SkSplitFLD(i)) = 'DUMMYCOUNTRYCODE'
056700170616     C                   EVAL      CCCDUMY = SkSplitCSV_9(i)
056800170616     C                   ENDIF
056900170616     C*
057000170616     C                   ENDIF
057100170616     C                   EVAL      i = i + 1
057200170616     C                   ENDDO
057300170616     C*
057400170616     C*** VER
057500170616     C                   EVAL      CCCVER = NEW_VERSION
057600170616     C*** BRT
057700170620     C                   Z-ADD     1             jNAZ
057800170620     C     CCCISO2       LOOKUP    skNAZISO(jNAZ)                         55
057900170616     C                   IF        %found
058000170616     C                   EVAL      CCCBRT = skNAZBAR(jNAZ)
058100170622     C                   ELSE
058200170622     C                   EVAL      CCCBRT = *all'*'
058300170616     C                   ENDIF
058400170616     C*
058500170616     C* Infine inserisco il record corrente
058600170616     C                   WRITE     DPCCC100
058700170616     C*
058800170616     C                   ENDSR
058900170616     C***
059000170616
059100170616
059200170616     C*----------------------------------------------------*
059300170616     C*  GESTIONE MAPPATURA "COUNTRY.IT"
059400170616     C*----------------------------------------------------*
059500170616     C     GESCCC_IT     BEGSR
059600170616     C*
059700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
059800170616     C                   Z-ADD     2             i                 4 0
059900170620     C                   DOW       i <= %elem(SkSplitFLD)
060000170620     C                             AND i <= OutIdxMax
060100170616     C*
060200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
060300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
060400170616     C                             SkSplitCSV_9(i) <> *blanks
060500170616     C*
060600170616     C*** ISO2
060700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
060800170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
060900170616     C                   ENDIF
061000170616     C*** NAME
061100170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
061200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
061300170616     C                   ENDIF
061400170616     C*
061500170616     C                   ENDIF
061600170616     C                   EVAL      i = i + 1
061700170616     C                   ENDDO
061800170616     C*
061900170616     C*** ATB
062000170616     C                   EVAL      CCCATB = *blanks
062100170616     C*** VER
062200170616     C                   EVAL      CCCVER = NEW_VERSION
062300170616     C*
062400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
062500170621     C                   IF        wDESxx <> *blanks
062600170616     C     KEYccc_11C    CHAIN     dpccc11i
062700170616     C                   IF        %found(dpccc11i)
062800170621     C                   EVAL      CCCDESIT = wDESxx
062900170616     C                   UPDATE    DPCCC100
063000170616     C                   ENDIF
063100170620     C                   ENDIF
063200170616     C*
063300170616     C                   ENDSR
063400170616     C***
063500170616
063600170616
063700170616     C*----------------------------------------------------*
063800170616     C*  GESTIONE MAPPATURA "COUNTRY.DE"
063900170616     C*----------------------------------------------------*
064000170616     C     GESCCC_DE     BEGSR
064100170616     C*
064200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
064300170616     C                   Z-ADD     2             i                 4 0
064400170620     C                   DOW       i <= %elem(SkSplitFLD)
064500170620     C                             AND i <= OutIdxMax
064600170616     C*
064700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
064800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
064900170616     C                             SkSplitCSV_9(i) <> *blanks
065000170616     C*
065100170616     C*** ISO2
065200170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
065300170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
065400170616     C                   ENDIF
065500170616     C*** NAME
065600170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
065700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
065800170616     C                   ENDIF
065900170616     C*
066000170616     C                   ENDIF
066100170616     C                   EVAL      i = i + 1
066200170616     C                   ENDDO
066300170616     C*
066400170616     C*** ATB
066500170616     C                   EVAL      CCCATB = *blanks
066600170616     C*** VER
066700170616     C                   EVAL      CCCVER = NEW_VERSION
066800170616     C*
066900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
067000170621     C                   IF        wDESxx <> *blanks
067100170616     C     KEYccc_11C    CHAIN     dpccc11i
067200170616     C                   IF        %found(dpccc11i)
067300170621     C                   EVAL      CCCDESDE = wDESxx
067400170616     C                   UPDATE    DPCCC100
067500170620     C                   ENDIF
067600170616     C                   ENDIF
067700170616     C*
067800170616     C                   ENDSR
067900170616     C***
068000170616
068100170616
068200170616     C*----------------------------------------------------*
068300170616     C*  GESTIONE MAPPATURA "COUNTRY.FR"
068400170616     C*----------------------------------------------------*
068500170616     C     GESCCC_FR     BEGSR
068600170616     C*
068700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
068800170616     C                   Z-ADD     2             i                 4 0
068900170620     C                   DOW       i <= %elem(SkSplitFLD)
069000170620     C                             AND i <= OutIdxMax
069100170616     C*
069200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
069300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
069400170616     C                             SkSplitCSV_9(i) <> *blanks
069500170616     C*
069600170616     C*** ISO2
069700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
069800170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
069900170616     C                   ENDIF
070000170616     C*** NAME
070100170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
070200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
070300170616     C                   ENDIF
070400170616     C*
070500170616     C                   ENDIF
070600170616     C                   EVAL      i = i + 1
070700170616     C                   ENDDO
070800170616     C*
070900170616     C*** ATB
071000170616     C                   EVAL      CCCATB = *blanks
071100170616     C*** VER
071200170616     C                   EVAL      CCCVER = NEW_VERSION
071300170616     C*
071400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
071500170621     C                   IF        wDESxx <> *blanks
071600170616     C     KEYccc_11C    CHAIN     dpccc11i
071700170616     C                   IF        %found(dpccc11i)
071800170621     C                   EVAL      CCCDESFR = wDESxx
071900170616     C                   UPDATE    DPCCC100
072000170616     C                   ENDIF
072100170620     C                   ENDIF
072200170616     C*
072300170616     C                   ENDSR
072400170616     C***
072500170616
072600170616
072700170616     C*----------------------------------------------------*
072800170616     C*  GESTIONE MAPPATURA "COUNTRY.EN"
072900170616     C*----------------------------------------------------*
073000170616     C     GESCCC_EN     BEGSR
073100170616     C*
073200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
073300170616     C                   Z-ADD     2             i                 4 0
073400170620     C                   DOW       i <= %elem(SkSplitFLD)
073500170620     C                             AND i <= OutIdxMax
073600170616     C*
073700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
073800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
073900170616     C                             SkSplitCSV_9(i) <> *blanks
074000170616     C*
074100170616     C*** ISO2
074200170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
074300170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
074400170616     C                   ENDIF
074500170616     C*** NAME
074600170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
074700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
074800170616     C                   ENDIF
074900170616     C*
075000170616     C                   ENDIF
075100170616     C                   EVAL      i = i + 1
075200170616     C                   ENDDO
075300170616     C*
075400170616     C*** ATB
075500170616     C                   EVAL      CCCATB = *blanks
075600170616     C*** VER
075700170616     C                   EVAL      CCCVER = NEW_VERSION
075800170616     C*
075900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
076000170621     C                   IF        wDESxx <> *blanks
076100170616     C     KEYccc_11C    CHAIN     dpccc11i
076200170616     C                   IF        %found(dpccc11i)
076300170621     C                   EVAL      CCCDESEN = wDESxx
076400170616     C                   UPDATE    DPCCC100
076500170616     C                   ENDIF
076600170620     C                   ENDIF
076700170616     C*
076800170616     C                   ENDSR
076900170616     C***
077000170616
077100170616
077200170616     C*----------------------------------------------------*
077300170616     C*  GESTIONE MAPPATURA "BUSINESSUNIT"
077400170616     C*----------------------------------------------------*
077500170616     C     GESCBU        BEGSR
077600170616     C*
077700170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
077800170616     C                   CLEAR                   DPCBU100
077900170616     C*
078000170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
078100170616     C                   Z-ADD     2             i                 4 0
078200170620     C                   DOW       i <= %elem(SkSplitFLD)
078300170620     C                             AND i <= OutIdxMax
078400170616     C*
078500170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
078600170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
078700170616     C                             SkSplitCSV_9(i) <> *blanks
078800170616     C*
078900170616     C*** BUCA
079000170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUALPHASTR'
079100170616     C                   EVAL      CBUBUCA = SkSplitCSV_9(i)
079200170616     C                   ENDIF
079300170616     C*** BUCN
079400170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
079500170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
079600170616     C                   EXSR      CHKNUM
079700170616     C                   IF        PiInt=*on
079800170616     C                   Z-ADD     PiVal         CBUBUCN
079900170616     C                   ENDIF
080000170616     C                   ENDIF
080100170616     C*** NAME
080200170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUNAME'
080300170616     C                   EVAL      CBUNAME = SkSplitCSV_9(i)
080400170616     C                   ENDIF
080500170616     C*** ISO2
080600170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUDEFAULTCOUNTRY'
080700170616     C                   EVAL      CBUISO2 = SkSplitCSV_9(i)
080800170616     C                   ENDIF
080900170616     C*** TYPE
081000170616     C                   IF        %trim(SkSplitFLD(i)) = 'TYPEOFBU'
081100170616     C                   EVAL      CBUTYPE = SkSplitCSV_9(i)
081200170616     C                   ENDIF
081300170616     C*** GRPF
081400170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUFINANCIALGROUP'
081500170616     C                   EVAL      CBUGRPF = SkSplitCSV_9(i)
081600170616     C                   ENDIF
081700170616     C*** GRPG
081800170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUGEOGRAPHGROUP'
081900170616     C                   EVAL      CBUGRPG = SkSplitCSV_9(i)
082000170616     C                   ENDIF
082100170616     C*** GRPT
082200170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUTECHNICALGROUP'
082300170616     C                   EVAL      CBUGRPT = SkSplitCSV_9(i)
082400170616     C                   ENDIF
082500170616     C*** DPDC
082600170616     C                   IF        %trim(SkSplitFLD(i)) = 'DPDCONTRACT'
082700170616     C                   EVAL      CBUDPDC = SkSplitCSV_9(i)
082800170616     C                   ENDIF
082900170616     C*
083000170616     C                   ENDIF
083100170616     C                   EVAL      i = i + 1
083200170616     C                   ENDDO
083300170616     C*
083400170616     C*** VER
083500170616     C                   EVAL      CBUVER = NEW_VERSION
083600170616     C*
083700170616     C* Infine inserisco il record corrente
083800170616     C                   WRITE     DPCBU100
083900170616     C*
084000170616     C                   ENDSR
084100170616     C***
084200170616
084300170616
084400170616     C*----------------------------------------------------*
084500170616     C*  GESTIONE MAPPATURA "POSTALCODE"
084600170616     C*----------------------------------------------------*
084700170616     C     GESCPC        BEGSR
084800170616     C*
084900170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
085000170616     C                   CLEAR                   DPCPC100
085100170616     C*
085200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
085300170616     C                   Z-ADD     2             i                 4 0
085400170620     C                   DOW       i <= %elem(SkSplitFLD)
085500170620     C                             AND i <= OutIdxMax
085600170616     C*
085700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
085800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
085900170616     C                             SkSplitCSV_9(i) <> *blanks
086000170616     C*
086100170616     C*** ISO2
086200170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
086300170616     C                   EVAL      CPCISO2 = SkSplitCSV_9(i)
086400170616     C                   ENDIF
086500170616     C*** PTCB
086600170616     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
086700170616     C                   EVAL      CPCPTCB = SkSplitCSV_9(i)
086800170616     C                   ENDIF
086900170616     C*** PTCE
087000170616     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
087100170616     C                   EVAL      CPCPTCE = SkSplitCSV_9(i)
087200170616     C                   ENDIF
087300170616     C*** PTCT
087400170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODETYPE'
087500170620     C                   EVAL      CPCPTCT = SkSplitCSV_9(i)
087600170616     C                   ENDIF
087700170616     C*
087800170616     C                   ENDIF
087900170616     C                   EVAL      i = i + 1
088000170616     C                   ENDDO
088100170619     C*
088200170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
088300170619     C                   IF        CPCPTCE = *blanks
088400170619     C                   EVAL      CPCPTCE = CPCPTCB
088500170619     C                   ENDIF
088600170616     C*
088700170616     C*** VER
088800170616     C                   EVAL      CPCVER = NEW_VERSION
088900170616     C*
089000170616     C* Infine inserisco il record corrente
089100170616     C                   WRITE     DPCPC100
089200170616     C*
089300170616     C                   ENDSR
089400170616     C***
089500170616
089600170616
089700170616     C*----------------------------------------------------*
089800170616     C*  GESTIONE MAPPATURA "STATE"
089900170616     C*----------------------------------------------------*
090000170616     C     GESCST        BEGSR
090100170616     C*
090200170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
090300170616     C                   CLEAR                   DPCST100
090400170616     C*
090500170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090600170616     C                   Z-ADD     2             i                 4 0
090700170620     C                   DOW       i <= %elem(SkSplitFLD)
090800170620     C                             AND i <= OutIdxMax
090900170616     C*
091000170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
091100170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
091200170616     C                             SkSplitCSV_9(i) <> *blanks
091300170616     C*
091400170616     C*** ISO2
091500170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
091600170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
091700170616     C                   ENDIF
091800170616     C*** REGC
091900170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODE'
092000170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
092100170616     C                   ENDIF
092200170616     C*** STAC
092300170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
092400170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
092500170616     C                   ENDIF
092600170616     C*
092700170616     C                   ENDIF
092800170616     C                   EVAL      i = i + 1
092900170616     C                   ENDDO
093000170616     C*
093100170616     C*** VER
093200170616     C                   EVAL      CSTVER = NEW_VERSION
093300170616     C*
093400170616     C* Infine inserisco il record corrente
093500170616     C                   WRITE     DPCST100
093600170616     C*
093700170616     C                   ENDSR
093800170616     C***
093900170616
094000170616
094100170616     C*----------------------------------------------------*
094200170616     C*  GESTIONE MAPPATURA "STATE.IT"
094300170616     C*----------------------------------------------------*
094400170616     C     GESCST_IT     BEGSR
094500170616     C*
094600170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094700170616     C                   Z-ADD     2             i                 4 0
094800170620     C                   DOW       i <= %elem(SkSplitFLD)
094900170620     C                             AND i <= OutIdxMax
095000170616     C*
095100170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095200170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
095300170616     C                             SkSplitCSV_9(i) <> *blanks
095400170616     C*
095500170616     C*** ISO2
095600170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
095700170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
095800170616     C                   ENDIF
095900170616     C*** REGC
096000170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
096100170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
096200170616     C                   ENDIF
096300170616     C*** STAC
096400170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
096500170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
096600170616     C                   ENDIF
096700170616     C*** SRTC
096800170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
096900170616     C                   ENDIF
097000170616     C*** DESxx
097100170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
097200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
097300170616     C                   ENDIF
097400170616     C*
097500170616     C                   ENDIF
097600170616     C                   EVAL      i = i + 1
097700170616     C                   ENDDO
097800170616     C*
097900170616     C*** ATB
098000170616     C                   EVAL      CSTATB = *blanks
098100170616     C*** VER
098200170616     C                   EVAL      CSTVER = NEW_VERSION
098300170616     C*
098400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
098500170621     C                   IF        wDESxx <> *blanks
098600170616     C     KEYcst_11C    CHAIN     dpCST11i
098700170616     C                   IF        %found(dpCST11i)
098800170621     C                   EVAL      CSTDESIT = wDESxx
098900170616     C                   UPDATE    DPCST100
099000170616     C                   ENDIF
099100170620     C                   ENDIF
099200170616     C*
099300170616     C                   ENDSR
099400170616     C***
099500170616
099600170616
099700170616     C*----------------------------------------------------*
099800170616     C*  GESTIONE MAPPATURA "STATE.DE"
099900170616     C*----------------------------------------------------*
100000170616     C     GESCST_DE     BEGSR
100100170616     C*
100200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100300170616     C                   Z-ADD     2             i                 4 0
100400170620     C                   DOW       i <= %elem(SkSplitFLD)
100500170620     C                             AND i <= OutIdxMax
100600170616     C*
100700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
100900170616     C                             SkSplitCSV_9(i) <> *blanks
101000170616     C*
101100170616     C*** ISO2
101200170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
101300170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
101400170616     C                   ENDIF
101500170616     C*** REGC
101600170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
101700170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
101800170616     C                   ENDIF
101900170616     C*** STAC
102000170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
102100170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
102200170616     C                   ENDIF
102300170616     C*** SRTC
102400170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
102500170616     C                   ENDIF
102600170616     C*** DESxx
102700170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
102800170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
102900170616     C                   ENDIF
103000170616     C*
103100170616     C                   ENDIF
103200170616     C                   EVAL      i = i + 1
103300170616     C                   ENDDO
103400170616     C*
103500170616     C*** ATB
103600170616     C                   EVAL      CSTATB = *blanks
103700170616     C*** VER
103800170616     C                   EVAL      CSTVER = NEW_VERSION
103900170616     C*
104000170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
104100170621     C                   IF        wDESxx <> *blanks
104200170616     C     KEYcst_11C    CHAIN     dpCST11i
104300170616     C                   IF        %found(dpCST11i)
104400170621     C                   EVAL      CSTDESDE = wDESxx
104500170616     C                   UPDATE    DPCST100
104600170616     C                   ENDIF
104700170620     C                   ENDIF
104800170616     C*
104900170616     C                   ENDSR
105000170616     C***
105100170616
105200170616
105300170616     C*----------------------------------------------------*
105400170616     C*  GESTIONE MAPPATURA "STATE.FR"
105500170616     C*----------------------------------------------------*
105600170616     C     GESCST_FR     BEGSR
105700170616     C*
105800170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
105900170616     C                   Z-ADD     2             i                 4 0
106000170620     C                   DOW       i <= %elem(SkSplitFLD)
106100170620     C                             AND i <= OutIdxMax
106200170616     C*
106300170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
106400170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
106500170616     C                             SkSplitCSV_9(i) <> *blanks
106600170616     C*
106700170616     C*** ISO2
106800170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
106900170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
107000170616     C                   ENDIF
107100170616     C*** REGC
107200170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
107300170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
107400170616     C                   ENDIF
107500170616     C*** STAC
107600170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
107700170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
107800170616     C                   ENDIF
107900170616     C*** SRTC
108000170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
108100170616     C                   ENDIF
108200170616     C*** DESxx
108300170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
108400170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
108500170616     C                   ENDIF
108600170616     C*
108700170616     C                   ENDIF
108800170616     C                   EVAL      i = i + 1
108900170616     C                   ENDDO
109000170616     C*
109100170616     C*** ATB
109200170616     C                   EVAL      CSTATB = *blanks
109300170616     C*** VER
109400170616     C                   EVAL      CSTVER = NEW_VERSION
109500170616     C*
109600170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
109700170621     C                   IF        wDESxx <> *blanks
109800170616     C     KEYcst_11C    CHAIN     dpCST11i
109900170616     C                   IF        %found(dpCST11i)
110000170621     C                   EVAL      CSTDESFR = wDESxx
110100170616     C                   UPDATE    DPCST100
110200170620     C                   ENDIF
110300170616     C                   ENDIF
110400170616     C*
110500170616     C                   ENDSR
110600170616     C***
110700170616
110800170616
110900170616     C*----------------------------------------------------*
111000170616     C*  GESTIONE MAPPATURA "STATE.EN"
111100170616     C*----------------------------------------------------*
111200170616     C     GESCST_EN     BEGSR
111300170616     C*
111400170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
111500170616     C                   Z-ADD     2             i                 4 0
111600170620     C                   DOW       i <= %elem(SkSplitFLD)
111700170620     C                             AND i <= OutIdxMax
111800170616     C*
111900170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
112000170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
112100170616     C                             SkSplitCSV_9(i) <> *blanks
112200170616     C*
112300170616     C*** ISO2
112400170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
112500170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
112600170616     C                   ENDIF
112700170616     C*** REGC
112800170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
112900170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
113000170616     C                   ENDIF
113100170616     C*** STAC
113200170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
113300170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
113400170616     C                   ENDIF
113500170616     C*** SRTC
113600170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
113700170616     C                   ENDIF
113800170616     C*** DESxx
113900170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
114000170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
114100170616     C                   ENDIF
114200170616     C*
114300170616     C                   ENDIF
114400170616     C                   EVAL      i = i + 1
114500170616     C                   ENDDO
114600170616     C*
114700170616     C*** ATB
114800170616     C                   EVAL      CSTATB = *blanks
114900170616     C*** VER
115000170616     C                   EVAL      CSTVER = NEW_VERSION
115100170616     C*
115200170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
115300170621     C                   IF        wDESxx <> *blanks
115400170616     C     KEYcst_11C    CHAIN     dpCST11i
115500170616     C                   IF        %found(dpCST11i)
115600170621     C                   EVAL      CSTDESEN = wDESxx
115700170616     C                   UPDATE    DPCST100
115800170620     C                   ENDIF
115900170616     C                   ENDIF
116000170616     C*
116100170616     C                   ENDSR
116200170616     C***
116300170616
116400170616
116500170616     C*----------------------------------------------------*
116600170616     C*  GESTIONE MAPPATURA "ELEMENTS"
116700170616     C*----------------------------------------------------*
116800170616     C     GESCEL        BEGSR
116900170616     C*
117000170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
117100170616     C                   CLEAR                   DPCEL100
117200170616     C*
117300170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
117400170616     C                   Z-ADD     2             i                 4 0
117500170620     C                   DOW       i <= %elem(SkSplitFLD)
117600170620     C                             AND i <= OutIdxMax
117700170616     C*
117800170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
117900170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
118000170616     C                             SkSplitCSV_9(i) <> *blanks
118100170616     C*
118200170620     C*** ELMC
118300170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
118400170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
118500170620     C                   EXSR      CHKNUM
118600170620     C                   IF        PiInt=*on
118700170620     C                   Z-ADD     PiVal         CELELMC
118800170620     C                   ENDIF
118900170620     C                   ENDIF
119000170616     C*** SRTC
119100170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTSHORTCUT'
119200170616     C                   EVAL      CELSRTC = SkSplitCSV_9(i)
119300170616     C                   ENDIF
119400170616     C*** ELMT
119500170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTTYPE'
119600170616     C                   EVAL      CELELMT = SkSplitCSV_9(i)
119700170616     C                   ENDIF
119800170616     C*
119900170616     C                   ENDIF
120000170616     C                   EVAL      i = i + 1
120100170616     C                   ENDDO
120200170616     C*
120300170616     C*** VER
120400170616     C                   EVAL      CELVER = NEW_VERSION
120500170616     C*
120600170616     C* Infine inserisco il record corrente
120700170616     C                   WRITE     DPCEL100
120800170616     C*
120900170616     C                   ENDSR
121000170616     C***
121100170616
121200170616
121300170616     C*----------------------------------------------------*
121400170616     C*  GESTIONE MAPPATURA "ELEMENTS.IT"
121500170616     C*----------------------------------------------------*
121600170616     C     GESCEL_IT     BEGSR
121700170616     C*
121800170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
121900170616     C                   Z-ADD     2             i                 4 0
122000170620     C                   DOW       i <= %elem(SkSplitFLD)
122100170620     C                             AND i <= OutIdxMax
122200170616     C*
122300170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
122400170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
122500170616     C                             SkSplitCSV_9(i) <> *blanks
122600170616     C*
122700170620     C*** ELMC
122800170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
122900170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
123000170620     C                   EXSR      CHKNUM
123100170620     C                   IF        PiInt=*on
123200170620     C                   Z-ADD     PiVal         CELELMC
123300170620     C                   ENDIF
123400170620     C                   ENDIF
123500170616     C*** DESxx
123600170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
123700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
123800170616     C                   ENDIF
123900170616     C*
124000170616     C                   ENDIF
124100170616     C                   EVAL      i = i + 1
124200170616     C                   ENDDO
124300170616     C*
124400170616     C*** ATB
124500170616     C                   EVAL      CELATB = *blanks
124600170616     C*** VER
124700170616     C                   EVAL      CELVER = NEW_VERSION
124800170616     C*
124900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
125000170621     C                   IF        wDESxx <> *blanks
125100170616     C     KEYcel_11C    CHAIN     dpCEL11i
125200170616     C                   IF        %found(dpCEL11i)
125300170621     C                   EVAL      CELDESIT = wDESxx
125400170616     C                   UPDATE    DPCEL100
125500170620     C                   ENDIF
125600170616     C                   ENDIF
125700170616     C*
125800170616     C                   ENDSR
125900170616     C***
126000170616
126100170616
126200170616     C*----------------------------------------------------*
126300170616     C*  GESTIONE MAPPATURA "ELEMENTS.DE"
126400170616     C*----------------------------------------------------*
126500170616     C     GESCEL_DE     BEGSR
126600170616     C*
126700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
126800170616     C                   Z-ADD     2             i                 4 0
126900170620     C                   DOW       i <= %elem(SkSplitFLD)
127000170620     C                             AND i <= OutIdxMax
127100170616     C*
127200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
127300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
127400170616     C                             SkSplitCSV_9(i) <> *blanks
127500170616     C*
127600170620     C*** ELMC
127700170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
127800170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
127900170620     C                   EXSR      CHKNUM
128000170620     C                   IF        PiInt=*on
128100170620     C                   Z-ADD     PiVal         CELELMC
128200170620     C                   ENDIF
128300170620     C                   ENDIF
128400170616     C*** DESxx
128500170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
128600170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
128700170616     C                   ENDIF
128800170616     C*
128900170616     C                   ENDIF
129000170616     C                   EVAL      i = i + 1
129100170616     C                   ENDDO
129200170616     C*
129300170616     C*** ATB
129400170616     C                   EVAL      CELATB = *blanks
129500170616     C*** VER
129600170616     C                   EVAL      CELVER = NEW_VERSION
129700170616     C*
129800170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
129900170621     C                   IF        wDESxx <> *blanks
130000170616     C     KEYcel_11C    CHAIN     dpCEL11i
130100170616     C                   IF        %found(dpCEL11i)
130200170621     C                   EVAL      CELDESDE = wDESxx
130300170616     C                   UPDATE    DPCEL100
130400170620     C                   ENDIF
130500170616     C                   ENDIF
130600170616     C*
130700170616     C                   ENDSR
130800170616     C***
130900170616
131000170616
131100170616     C*----------------------------------------------------*
131200170616     C*  GESTIONE MAPPATURA "ELEMENTS.FR"
131300170616     C*----------------------------------------------------*
131400170616     C     GESCEL_FR     BEGSR
131500170616     C*
131600170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
131700170616     C                   Z-ADD     2             i                 4 0
131800170620     C                   DOW       i <= %elem(SkSplitFLD)
131900170620     C                             AND i <= OutIdxMax
132000170616     C*
132100170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
132200170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
132300170616     C                             SkSplitCSV_9(i) <> *blanks
132400170616     C*
132500170620     C*** ELMC
132600170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
132700170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
132800170620     C                   EXSR      CHKNUM
132900170620     C                   IF        PiInt=*on
133000170620     C                   Z-ADD     PiVal         CELELMC
133100170620     C                   ENDIF
133200170620     C                   ENDIF
133300170616     C*** DESxx
133400170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
133500170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
133600170616     C                   ENDIF
133700170616     C*
133800170616     C                   ENDIF
133900170616     C                   EVAL      i = i + 1
134000170616     C                   ENDDO
134100170616     C*
134200170616     C*** ATB
134300170616     C                   EVAL      CELATB = *blanks
134400170616     C*** VER
134500170616     C                   EVAL      CELVER = NEW_VERSION
134600170616     C*
134700170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
134800170621     C                   IF        wDESxx <> *blanks
134900170616     C     KEYcel_11C    CHAIN     dpCEL11i
135000170616     C                   IF        %found(dpCEL11i)
135100170621     C                   EVAL      CELDESFR = wDESxx
135200170616     C                   UPDATE    DPCEL100
135300170620     C                   ENDIF
135400170616     C                   ENDIF
135500170616     C*
135600170616     C                   ENDSR
135700170616     C***
135800170616
135900170616
136000170616     C*----------------------------------------------------*
136100170616     C*  GESTIONE MAPPATURA "ELEMENTS.EN"
136200170616     C*----------------------------------------------------*
136300170616     C     GESCEL_EN     BEGSR
136400170616     C*
136500170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
136600170616     C                   Z-ADD     2             i                 4 0
136700170620     C                   DOW       i <= %elem(SkSplitFLD)
136800170620     C                             AND i <= OutIdxMax
136900170616     C*
137000170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
137100170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
137200170616     C                             SkSplitCSV_9(i) <> *blanks
137300170616     C*
137400170620     C*** ELMC
137500170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
137600170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
137700170620     C                   EXSR      CHKNUM
137800170620     C                   IF        PiInt=*on
137900170620     C                   Z-ADD     PiVal         CELELMC
138000170620     C                   ENDIF
138100170620     C                   ENDIF
138200170616     C*** DESxx
138300170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
138400170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
138500170616     C                   ENDIF
138600170616     C*
138700170616     C                   ENDIF
138800170616     C                   EVAL      i = i + 1
138900170616     C                   ENDDO
139000170616     C*
139100170616     C*** ATB
139200170616     C                   EVAL      CELATB = *blanks
139300170616     C*** VER
139400170616     C                   EVAL      CELVER = NEW_VERSION
139500170616     C*
139600170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
139700170621     C                   IF        wDESxx <> *blanks
139800170616     C     KEYcel_11C    CHAIN     dpCEL11i
139900170616     C                   IF        %found(dpCEL11i)
140000170621     C                   EVAL      CELDESEN = wDESxx
140100170616     C                   UPDATE    DPCEL100
140200170620     C                   ENDIF
140300170616     C                   ENDIF
140400170616     C*
140500170616     C                   ENDSR
140600170616     C***
140700170616
140800170616
140900170616     C*----------------------------------------------------*
141000170616     C*  GESTIONE MAPPATURA "ELEMENTS.PT"
141100170616     C*----------------------------------------------------*
141200170616     C     GESCEL_PT     BEGSR
141300170616     C*
141400170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
141500170616     C                   Z-ADD     2             i                 4 0
141600170620     C                   DOW       i <= %elem(SkSplitFLD)
141700170620     C                             AND i <= OutIdxMax
141800170616     C*
141900170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
142000170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
142100170616     C                             SkSplitCSV_9(i) <> *blanks
142200170616     C*
142300170620     C*** ELMC
142400170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
142500170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
142600170620     C                   EXSR      CHKNUM
142700170620     C                   IF        PiInt=*on
142800170620     C                   Z-ADD     PiVal         CELELMC
142900170620     C                   ENDIF
143000170620     C                   ENDIF
143100170616     C*** DESxx
143200170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
143300170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
143400170616     C                   ENDIF
143500170616     C*
143600170616     C                   ENDIF
143700170616     C                   EVAL      i = i + 1
143800170616     C                   ENDDO
143900170616     C*
144000170616     C*** ATB
144100170616     C                   EVAL      CELATB = *blanks
144200170616     C*** VER
144300170616     C                   EVAL      CELVER = NEW_VERSION
144400170616     C*
144500170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
144600170621     C                   IF        wDESxx <> *blanks
144700170616     C     KEYcel_11C    CHAIN     dpCEL11i
144800170616     C                   IF        %found(dpCEL11i)
144900170621     C                   EVAL      CELDESPT = wDESxx
145000170616     C                   UPDATE    DPCEL100
145100170620     C                   ENDIF
145200170616     C                   ENDIF
145300170616     C*
145400170616     C                   ENDSR
145500170619     C***
145600170619
145700170619
145800170619     C*----------------------------------------------------*
145900170619     C*  GESTIONE MAPPATURA "SOCODE"
146000170619     C*----------------------------------------------------*
146100170619     C     GESCSO        BEGSR
146200170619     C*
146300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
146400170619     C                   CLEAR                   DPCSO100
146500170619     C*
146600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
146700170619     C                   Z-ADD     2             i                 4 0
146800170620     C                   DOW       i <= %elem(SkSplitFLD)
146900170620     C                             AND i <= OutIdxMax
147000170619     C*
147100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
147200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
147300170619     C                             SkSplitCSV_9(i) <> *blanks
147400170619     C*
147500170619     C*** SORC
147600170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
147700170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
147800170619     C                   EXSR      CHKNUM
147900170619     C                   IF        PiInt=*on
148000170619     C                   Z-ADD     PiVal         CSOSORC
148100170619     C                   ENDIF
148200170619     C                   ENDIF
148300170619     C*** STXT
148400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICETEXT'
148500170620     C                   EVAL      CSOSTXT  = SkSplitCSV_9(i)
148600170619     C                   ENDIF
148700170619     C*** SMRK
148800170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICEMARK'
148900170620     C                   EVAL      CSOSMRK  = SkSplitCSV_9(i)
149000170619     C                   ENDIF
149100170620     C*** ELMC
149200170619     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODES'
149300170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
149400170619     C                   ENDIF
149500170619     C*** ASCO
149600170619     C                   IF        %trim(SkSplitFLD(i)) = 'ASCODENEEDED'
149700170620     C                   EVAL      CSOASCO  = SkSplitCSV_9(i)
149800170619     C                   ENDIF
149900170619     C*
150000170619     C                   ENDIF
150100170619     C                   EVAL      i = i + 1
150200170619     C                   ENDDO
150300170619     C*
150400170619     C*** VER
150500170619     C                   EVAL      CSOVER = NEW_VERSION
150600170620     C*
150700170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
150800170620     C                   EVAL      InStringa = w2048_01
150900170620     C                   EVAL      InSepara = ','
151000170620     C                   SETOFF                                       131419
151100170620     C                   SETON                                        13
151200170620     C                   EXSR      SR_SPLIT
151300170620     C*
151400170620     C* Se nessun elemento => scrivo comunque un record singolo
151500170620     C                   IF        OutIdxMax = *zeros
151600170620     C                   WRITE     DPCSO100
151700170620     C                   ELSE
151800170620     C*
151900170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
152000170620     C                   Z-ADD     1             j                 4 0
152100170620     C                   DOW       j <= %elem(SkSplitCSV_3)
152200170620     C                             AND j <= OutIdxMax
152300170620     C*** ELMC
152400170620     C                   EVAL      PiStr = SkSplitCSV_3(j)
152500170620     C                   EXSR      CHKNUM
152600170620     C                   IF        PiInt=*on
152700170620     C                   Z-ADD     PiVal         CSOELMC
152800170620     C                   ENDIF
152900170620     C*
153000170620     C* Infine inserisco il record corrente
153100170620     C                   WRITE     DPCSO100
153200170620     C*
153300170620     C                   EVAL      j = j + 1
153400170620     C                   ENDDO
153500170620     C                   ENDIF
153600170619     C*
153700170619     C                   ENDSR
153800170619     C***
153900170619
154000170619
154100170619     C*----------------------------------------------------*
154200170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.IT"
154300170619     C*----------------------------------------------------*
154400170620     C     GESCSO_IT     BEGSR
154500170619     C*
154600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
154700170619     C                   Z-ADD     2             i                 4 0
154800170620     C                   DOW       i <= %elem(SkSplitFLD)
154900170620     C                             AND i <= OutIdxMax
155000170620     C*
155100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
155200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
155300170619     C                             SkSplitCSV_9(i) <> *blanks
155400170619     C*
155500170620     C*** SORC
155600170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
155700170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
155800170620     C                   EXSR      CHKNUM
155900170620     C                   IF        PiInt=*on
156000170620     C                   Z-ADD     PiVal         CSOSORC
156100170620     C                   ENDIF
156200170620     C                   ENDIF
156300170619     C*** DESxx
156400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
156500170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
156600170619     C                   ENDIF
156700170619     C*
156800170619     C                   ENDIF
156900170620     C                   EVAL      i = i + 1
157000170619     C                   ENDDO
157100170619     C*
157200170619     C*** ATB
157300170619     C                   EVAL      CSOATB = *blanks
157400170619     C*** VER
157500170619     C                   EVAL      CSOVER = NEW_VERSION
157600170619     C*
157700170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
157800170621     C                   IF        wDESxx <> *blanks
157900170620     C     KEYcso_11C    CHAIN     dpCSO11i
158000170619     C                   IF        %found(dpCSO11i)
158100170621     C                   EVAL      CSODESIT = wDESxx
158200170619     C                   UPDATE    DPCSO100
158300170620     C                   ENDIF
158400170619     C                   ENDIF
158500170619     C*
158600170619     C                   ENDSR
158700170619     C***
158800170619
158900170619
159000170619     C*----------------------------------------------------*
159100170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.EN"
159200170619     C*----------------------------------------------------*
159300170619     C     GESCSO_EN     BEGSR
159400170619     C*
159500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
159600170619     C                   Z-ADD     2             i                 4 0
159700170620     C                   DOW       i <= %elem(SkSplitFLD)
159800170620     C                             AND i <= OutIdxMax
159900170619     C*
160000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
160100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
160200170619     C                             SkSplitCSV_9(i) <> *blanks
160300170619     C*
160400170620     C*** SORC
160500170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
160600170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
160700170620     C                   EXSR      CHKNUM
160800170620     C                   IF        PiInt=*on
160900170620     C                   Z-ADD     PiVal         CSOSORC
161000170620     C                   ENDIF
161100170620     C                   ENDIF
161200170619     C*** DESxx
161300170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
161400170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
161500170619     C                   ENDIF
161600170619     C*
161700170619     C                   ENDIF
161800170620     C                   EVAL      i = i + 1
161900170619     C                   ENDDO
162000170619     C*
162100170619     C*** ATB
162200170619     C                   EVAL      CSOATB = *blanks
162300170619     C*** VER
162400170619     C                   EVAL      CSOVER = NEW_VERSION
162500170619     C*
162600170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
162700170621     C                   IF        wDESxx <> *blanks
162800170619     C     KEYcso_11C    CHAIN     dpCSO11i
162900170619     C                   IF        %found(dpCSO11i)
163000170621     C                   EVAL      CSODESEN = wDESxx
163100170619     C                   UPDATE    DPCSO100
163200170620     C                   ENDIF
163300170619     C                   ENDIF
163400170619     C*
163500170619     C                   ENDSR
163600170619     C***
163700170619
163800170619
163900170619     C*----------------------------------------------------*
164000170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.DE"
164100170619     C*----------------------------------------------------*
164200170619     C     GESCSO_DE     BEGSR
164300170619     C*
164400170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
164500170619     C                   Z-ADD     2             i                 4 0
164600170620     C                   DOW       i <= %elem(SkSplitFLD)
164700170620     C                             AND i <= OutIdxMax
164800170619     C*
164900170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
165000170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
165100170619     C                             SkSplitCSV_9(i) <> *blanks
165200170619     C*
165300170620     C*** SORC
165400170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
165500170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
165600170620     C                   EXSR      CHKNUM
165700170620     C                   IF        PiInt=*on
165800170620     C                   Z-ADD     PiVal         CSOSORC
165900170620     C                   ENDIF
166000170620     C                   ENDIF
166100170619     C*** DESxx
166200170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
166300170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
166400170619     C                   ENDIF
166500170619     C*
166600170619     C                   ENDIF
166700170620     C                   EVAL      i = i + 1
166800170619     C                   ENDDO
166900170619     C*
167000170619     C*** ATB
167100170619     C                   EVAL      CSOATB = *blanks
167200170619     C*** VER
167300170619     C                   EVAL      CSOVER = NEW_VERSION
167400170619     C*
167500170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
167600170621     C                   IF        wDESxx <> *blanks
167700170619     C     KEYcso_11C    CHAIN     dpCSO11i
167800170619     C                   IF        %found(dpCSO11i)
167900170621     C                   EVAL      CSODESDE = wDESxx
168000170619     C                   UPDATE    DPCSO100
168100170620     C                   ENDIF
168200170619     C                   ENDIF
168300170619     C*
168400170619     C                   ENDSR
168500170619     C***
168600170619
168700170619
168800170619     C*----------------------------------------------------*
168900170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.FR"
169000170619     C*----------------------------------------------------*
169100170619     C     GESCSO_FR     BEGSR
169200170619     C*
169300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
169400170619     C                   Z-ADD     2             i                 4 0
169500170620     C                   DOW       i <= %elem(SkSplitFLD)
169600170620     C                             AND i <= OutIdxMax
169700170619     C*
169800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
169900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
170000170619     C                             SkSplitCSV_9(i) <> *blanks
170100170619     C*
170200170620     C*** SORC
170300170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
170400170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
170500170620     C                   EXSR      CHKNUM
170600170620     C                   IF        PiInt=*on
170700170620     C                   Z-ADD     PiVal         CSOSORC
170800170620     C                   ENDIF
170900170620     C                   ENDIF
171000170619     C*** DESxx
171100170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
171200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
171300170619     C                   ENDIF
171400170619     C*
171500170619     C                   ENDIF
171600170620     C                   EVAL      i = i + 1
171700170619     C                   ENDDO
171800170619     C*
171900170619     C*** ATB
172000170619     C                   EVAL      CSOATB = *blanks
172100170619     C*** VER
172200170619     C                   EVAL      CSOVER = NEW_VERSION
172300170619     C*
172400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
172500170621     C                   IF        wDESxx <> *blanks
172600170619     C     KEYcso_11C    CHAIN     dpCSO11i
172700170619     C                   IF        %found(dpCSO11i)
172800170621     C                   EVAL      CSODESFR = wDESxx
172900170619     C                   UPDATE    DPCSO100
173000170620     C                   ENDIF
173100170619     C                   ENDIF
173200170619     C*
173300170619     C                   ENDSR
173400170619     C***
173500170619
173600170619
173700170619     C*----------------------------------------------------*
173800170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.ET"
173900170619     C*----------------------------------------------------*
174000170619     C     GESCSO_ET     BEGSR
174100170619     C*
174200170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
174300170619     C                   Z-ADD     2             i                 4 0
174400170620     C                   DOW       i <= %elem(SkSplitFLD)
174500170620     C                             AND i <= OutIdxMax
174600170619     C*
174700170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
174800170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
174900170619     C                             SkSplitCSV_9(i) <> *blanks
175000170619     C*
175100170620     C*** SORC
175200170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
175300170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
175400170620     C                   EXSR      CHKNUM
175500170620     C                   IF        PiInt=*on
175600170620     C                   Z-ADD     PiVal         CSOSORC
175700170620     C                   ENDIF
175800170620     C                   ENDIF
175900170619     C*** DESxx
176000170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
176100170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
176200170619     C                   ENDIF
176300170619     C*
176400170619     C                   ENDIF
176500170620     C                   EVAL      i = i + 1
176600170619     C                   ENDDO
176700170619     C*
176800170619     C*** ATB
176900170619     C                   EVAL      CSOATB = *blanks
177000170619     C*** VER
177100170619     C                   EVAL      CSOVER = NEW_VERSION
177200170619     C*
177300170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
177400170621     C                   IF        wDESxx <> *blanks
177500170619     C     KEYcso_11C    CHAIN     dpCSO11i
177600170619     C                   IF        %found(dpCSO11i)
177700170621     C                   EVAL      CSODESET = wDESxx
177800170619     C                   UPDATE    DPCSO100
177900170620     C                   ENDIF
178000170619     C                   ENDIF
178100170619     C*
178200170619     C                   ENDSR
178300170619     C***
178400170619
178500170619
178600170619     C*----------------------------------------------------*
178700170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.CS"
178800170619     C*----------------------------------------------------*
178900170619     C     GESCSO_CS     BEGSR
179000170619     C*
179100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
179200170619     C                   Z-ADD     2             i                 4 0
179300170620     C                   DOW       i <= %elem(SkSplitFLD)
179400170620     C                             AND i <= OutIdxMax
179500170619     C*
179600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
179700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
179800170619     C                             SkSplitCSV_9(i) <> *blanks
179900170619     C*
180000170620     C*** SORC
180100170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
180200170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
180300170620     C                   EXSR      CHKNUM
180400170620     C                   IF        PiInt=*on
180500170620     C                   Z-ADD     PiVal         CSOSORC
180600170620     C                   ENDIF
180700170620     C                   ENDIF
180800170619     C*** DESxx
180900170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
181000170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
181100170619     C                   ENDIF
181200170619     C*
181300170619     C                   ENDIF
181400170620     C                   EVAL      i = i + 1
181500170619     C                   ENDDO
181600170619     C*
181700170619     C*** ATB
181800170619     C                   EVAL      CSOATB = *blanks
181900170619     C*** VER
182000170619     C                   EVAL      CSOVER = NEW_VERSION
182100170619     C*
182200170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
182300170621     C                   IF        wDESxx <> *blanks
182400170619     C     KEYcso_11C    CHAIN     dpCSO11i
182500170619     C                   IF        %found(dpCSO11i)
182600170621     C                   EVAL      CSODESCS = wDESxx
182700170619     C                   UPDATE    DPCSO100
182800170620     C                   ENDIF
182900170619     C                   ENDIF
183000170619     C*
183100170619     C                   ENDSR
183200170619     C***
183300170619
183400170619
183500170619     C*----------------------------------------------------*
183600170619     C*  GESTIONE MAPPATURA "ASCODE"
183700170619     C*----------------------------------------------------*
183800170619     C     GESCAS        BEGSR
183900170619     C*
184000170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
184100170619     C                   CLEAR                   DPCAS100
184200170619     C*
184300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
184400170619     C                   Z-ADD     2             i                 4 0
184500170620     C                   DOW       i <= %elem(SkSplitFLD)
184600170620     C                             AND i <= OutIdxMax
184700170619     C*
184800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
184900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
185000170619     C                             SkSplitCSV_9(i) <> *blanks
185100170619     C*
185200170619     C*** ADSC
185300170619     C                   IF        %trim(SkSplitFLD(i)) = 'ASCODE'
185400170620     C                   EVAL      CASADSC = SkSplitCSV_9(i)
185500170619     C                   ENDIF
185600170619     C*** STXT
185700170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICETEXT'
185800170620     C                   EVAL      CASSTXT = SkSplitCSV_9(i)
185900170619     C                   ENDIF
186000170620     C*** ELMC
186100170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODES'
186200170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
186300170620     C                   ENDIF
186400170619     C*
186500170619     C                   ENDIF
186600170620     C                   EVAL      i = i + 1
186700170619     C                   ENDDO
186800170619     C*
186900170619     C*** VER
187000170619     C                   EVAL      CASVER = NEW_VERSION
187100170620     C*
187200170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
187300170620     C                   EVAL      InStringa = w2048_01
187400170620     C                   EVAL      InSepara = ','
187500170620     C                   SETOFF                                       131419
187600170620     C                   SETON                                        13
187700170620     C                   EXSR      SR_SPLIT
187800170620     C*
187900170620     C* Se nessun elemento => scrivo comunque un record singolo
188000170620     C                   IF        OutIdxMax = *zeros
188100170620     C                   WRITE     DPCAS100
188200170620     C                   ELSE
188300170620     C*
188400170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
188500170620     C                   Z-ADD     1             j                 4 0
188600170620     C                   DOW       j <= %elem(SkSplitCSV_3)
188700170620     C                             AND j <= OutIdxMax
188800170620     C*** ELMC
188900170620     C                   EVAL      PiStr = SkSplitCSV_3(j)
189000170620     C                   EXSR      CHKNUM
189100170620     C                   IF        PiInt=*on
189200170620     C                   Z-ADD     PiVal         CASELMC
189300170620     C                   ENDIF
189400170620     C*
189500170620     C* Infine inserisco il record corrente
189600170620     C                   WRITE     DPCAS100
189700170620     C*
189800170620     C                   EVAL      j = j + 1
189900170620     C                   ENDDO
190000170620     C                   ENDIF
190100170619     C*
190200170619     C                   ENDSR
190300170619     C***
190400170619
190500170619
190600170619     C*----------------------------------------------------*
190700170619     C*  GESTIONE MAPPATURA "IC"
190800170619     C*----------------------------------------------------*
190900170619     C     GESCIC        BEGSR
191000170619     C*
191100170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
191200170619     C                   CLEAR                   DPCIC100
191300170619     C*
191400170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
191500170619     C                   Z-ADD     2             i                 4 0
191600170620     C                   DOW       i <= %elem(SkSplitFLD)
191700170620     C                             AND i <= OutIdxMax
191800170619     C*
191900170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
192000170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
192100170619     C                             SkSplitCSV_9(i) <> *blanks
192200170619     C*
192300170619     C*** ICC
192400170619     C                   IF        %trim(SkSplitFLD(i)) = 'INJECTIONCODE'
192500170619     C                   EVAL      CICICC  = SkSplitCSV_9(i)
192600170619     C                   ENDIF
192700170619     C*** DPTN
192800170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
192900170619     C                   EVAL      CICDPTN = SkSplitCSV_9(i)
193000170619     C                   ENDIF
193100170619     C*** ICD
193200170619     C                   IF        %trim(SkSplitFLD(i)) = 'DESCRIPTION'
193300170619     C                   EVAL      CICICD  = SkSplitCSV_9(i)
193400170619     C                   ENDIF
193500170619     C*
193600170619     C                   ENDIF
193700170620     C                   EVAL      i = i + 1
193800170619     C                   ENDDO
193900170619     C*
194000170619     C*** VER
194100170619     C                   EVAL      CICVER = NEW_VERSION
194200170619     C*
194300170619     C* Infine inserisco il record corrente
194400170619     C                   WRITE     DPCIC100
194500170619     C*
194600170619     C                   ENDSR
194700170619     C***
194800170619
194900170619
195000170619     C*----------------------------------------------------*
195100170619     C*  GESTIONE MAPPATURA "NETWORK"
195200170619     C*----------------------------------------------------*
195300170619     C     GESCNT        BEGSR
195400170619     C*
195500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
195600170619     C                   CLEAR                   DPCNT100
195700170619     C*
195800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
195900170619     C                   Z-ADD     2             i                 4 0
196000170620     C                   DOW       i <= %elem(SkSplitFLD)
196100170620     C                             AND i <= OutIdxMax
196200170619     C*
196300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
196400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
196500170619     C                             SkSplitCSV_9(i) <> *blanks
196600170619     C*
196700170619     C*** NTWC
196800170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKCODE'
196900170619     C                   EVAL      CNTNTWC = SkSplitCSV_9(i)
197000170619     C                   ENDIF
197100170619     C*** BUCN
197200170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
197300170619     C                   EVAL      CNTBUCN = SkSplitCSV_9(i)
197400170619     C                   ENDIF
197500170619     C*** ISO2
197600170619     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
197700170619     C                   EVAL      CNTISO2 = SkSplitCSV_9(i)
197800170619     C                   ENDIF
197900170619     C*** NTWN
198000170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKNAME'
198100170619     C                   EVAL      CNTNTWN = SkSplitCSV_9(i)
198200170619     C                   ENDIF
198300170619     C*
198400170619     C                   ENDIF
198500170619     C                   EVAL      i = i + 1
198600170619     C                   ENDDO
198700170619     C*
198800170619     C*** VER
198900170619     C                   EVAL      CNTVER = NEW_VERSION
199000170619     C*
199100170619     C* Infine inserisco il record corrente
199200170619     C                   WRITE     DPCNT100
199300170619     C*
199400170619     C                   ENDSR
199500170619     C***
199600170619
199700170619
199800170619     C*----------------------------------------------------*
199900170621     C*  GESTIONE MAPPATURA "NETWORKPLAN"
200000170619     C*----------------------------------------------------*
200100170619     C     GESCNP        BEGSR
200200170619     C*
200300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
200400170619     C                   CLEAR                   DPCNP100
200500170619     C*
200600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
200700170619     C                   Z-ADD     2             i                 4 0
200800170620     C                   DOW       i <= %elem(SkSplitFLD)
200900170620     C                             AND i <= OutIdxMax
201000170619     C*
201100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
201200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
201300170619     C                             SkSplitCSV_9(i) <> *blanks
201400170619     C*
201500170619     C*** NTWP
201600170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
201700170619     C                   EVAL      CNPNTWP  = SkSplitCSV_9(i)
201800170619     C                   ENDIF
201900170621     C*** SORCB / SORCE
202000170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODELIST'
202100170621     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
202200170619     C                   ENDIF
202300170619     C*** ASCOL
202400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOASCODELIST'
202500170619     C                   EVAL      CNPASCOL = SkSplitCSV_9(i)
202600170619     C                   ENDIF
202700170619     C*** FULL
202800170619     C                   IF        %trim(SkSplitFLD(i)) = 'FULLNETWORKPLAN'
202900170619     C                   EVAL      CNPFULL  = SkSplitCSV_9(i)
203000170619     C                   ENDIF
203100170619     C*** PTY
203200170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
203300170619     C                   EVAL      CNPPTY   = SkSplitCSV_9(i)
203400170619     C                   ENDIF
203500170619     C*
203600170619     C                   ENDIF
203700170619     C                   EVAL      i = i + 1
203800170619     C                   ENDDO
203900170619     C*
204000170619     C*** VER
204100170621     C                   EVAL      CNPVER = NEW_VERSION
204200170621     C*
204300170621     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
204400170621     C                   EVAL      InStringa = w2048_01
204500170621     C                   EVAL      InSepara = ','
204600170621     C                   SETOFF                                       131419
204700170621     C                   SETON                                        13
204800170621     C                   EXSR      SR_SPLIT
204900170621     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
205000170621     C*
205100170621     C* Se nessun elemento => scrivo comunque un record singolo
205200170621     C                   IF        sOutIdxMax_w1 = *zeros
205300170621     C                   WRITE     DPCNP100
205400170621     C                   ELSE
205500170621     C*
205600170621     C* Sviluppo tutti gli elementi della lista dei Routing Places
205700170621     C                   Z-ADD     1             j                 4 0
205800170621     C                   DOW       j <= %elem(SkSplitCSV_3)
205900170621     C                             AND j <= sOutIdxMax_w1
206000170621     C*
206100170621     C* Determino il tipo di Routing Place
206200170621     C                   SELECT
206300170621     C* SOCODE - Single
206400170621     C                   WHEN      %len(%trim((SkSplitCSV_3(j)))) = 4
206500170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):2:3)
206600170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
206700170621     C                   EVAL      CNPSORCE = CNPSORCB
206800170621     C                   WRITE     DPCNP100
206900170621     C* SOCODE - Range
207000170621     C                   WHEN      %scan('-':SkSplitCSV_3(j)) > *zeros
207100170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):2:3)
207200170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
207300170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):6:3)
207400170621     C                   EVAL      CNPSORCE = %dec(wSORC:3:0)
207500170621     C                   WRITE     DPCNP100
207600170621     C* SOCODE - List
207700170621     C                   WHEN      %scan(':':SkSplitCSV_3(j)) > *zeros
207800170621     C                   EVAL      InStringa = %subst(SkSplitCSV_3(j):2)
207900170621     C                   EVAL      InSepara = ':'
208000170621     C                   SETOFF                                       131419
208100170621     C                   SETON                                        14
208200170621     C                   EXSR      SR_SPLIT
208300170621     C*
208400170621     C* Sviluppo tutti gli elementi della lista dei Routing Places
208500170621     C                   Z-ADD     1             z                 4 0
208600170621     C                   DOW       z <= %elem(SkSplitCSV_4)
208700170621     C                             AND z <= OutIdxMax
208800170621     C                   EVAL      wSORC    = SkSplitCSV_4(z)
208900170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
209000170621     C                   EVAL      CNPSORCE = CNPSORCB
209100170621     C                   WRITE     DPCNP100
209200170621     C                   EVAL      z = z + 1
209300170621     C                   ENDDO
209400170621     C*
209500170621     C                   ENDSL
209600170621     C*
209700170621     C                   EVAL      j = j + 1
209800170621     C                   ENDDO
209900170621     C                   ENDIF
210000170619     C*
210100170619     C                   ENDSR
210200170619     C***
210300170619
210400170619
210500170619     C*----------------------------------------------------*
210600170619     C*  GESTIONE MAPPATURA "ALLOWSO"
210700170619     C*----------------------------------------------------*
210800170621     C     GESCWS        BEGSR
210900170619     C*
211000170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
211100170619     C                   CLEAR                   DPCWS100
211200170619     C*
211300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
211400170619     C                   Z-ADD     2             i                 4 0
211500170620     C                   DOW       i <= %elem(SkSplitFLD)
211600170620     C                             AND i <= OutIdxMax
211700170619     C*
211800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
211900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
212000170619     C                             SkSplitCSV_9(i) <> *blanks
212100170619     C*
212200170621     C*** RULFT / RULFB / RULFE
212300170619     C                   IF        %trim(SkSplitFLD(i)) = 'RULEFROM'
212400170621     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
212500170621     C                   ENDIF
212600170621     C*** RULSB / RULSE
212700170621     C                   IF        %trim(SkSplitFLD(i)) = 'RULESERVICE'
212800170621     C                   EVAL      w2048_02 = SkSplitCSV_9(i)
212900170621     C                   ENDIF
213000170621     C*** RULTT / RULTB / RULTE
213100170621     C                   IF        %trim(SkSplitFLD(i)) = 'RULETO'
213200170621     C                   EVAL      w2048_03 = SkSplitCSV_9(i)
213300170619     C                   ENDIF
213400170621     C*** ZONTB / ZONTE
213500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ZONETO'
213600170621     C                   EVAL      w2048_04 = SkSplitCSV_9(i)
213700170619     C                   ENDIF
213800170619     C*** ALWID
213900170619     C                   IF        %trim(SkSplitFLD(i)) = 'UNIQUEALLOWID'
214000170628     C                   EVAL      PiStr = SkSplitCSV_9(i)
214100170628     C                   EXSR      CHKNUM
214200170628     C                   IF        PiInt=*on
214300170628     C                   Z-ADD     PiVal         CWSALWID
214400170628     C                   ENDIF
214500170628     C                   ENDIF
214600170619     C*
214700170619     C                   ENDIF
214800170619     C                   EVAL      i = i + 1
214900170619     C                   ENDDO
215000170619     C*
215100170619     C*** VER
215200170619     C                   EVAL      CWSVER = NEW_VERSION
215300170621     C*
215400170621     C* Splitto il 1° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
215500170621     C                   EVAL      InStringa = w2048_01
215600170621     C                   EVAL      InSepara = ','
215700170621     C                   SETOFF                                       131419
215800170621     C                   SETON                                        13
215900170621     C                   EXSR      SR_SPLIT
216000170621     C                   EVAL      wSkCSV_03_w1  = %subarr(SkSplitCSV_3:1)
216100170621     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
216200170621     C*
216300170621     C* Splitto il 2° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
216400170621     C                   EVAL      InStringa = w2048_02
216500170621     C                   EVAL      InSepara = ','
216600170621     C                   SETOFF                                       131419
216700170621     C                   SETON                                        13
216800170621     C                   EXSR      SR_SPLIT
216900170621     C                   EVAL      wSkCSV_03_w2  = %subarr(SkSplitCSV_3:1)
217000170621     C                   EVAL      sOutIdxMax_w2 = OutIdxMax
217100170621     C*
217200170621     C* Splitto il 3° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
217300170621     C                   EVAL      InStringa = w2048_03
217400170621     C                   EVAL      InSepara = ','
217500170621     C                   SETOFF                                       131419
217600170621     C                   SETON                                        13
217700170621     C                   EXSR      SR_SPLIT
217800170621     C                   EVAL      wSkCSV_03_w3  = %subarr(SkSplitCSV_3:1)
217900170621     C                   EVAL      sOutIdxMax_w3 = OutIdxMax
218000170621     C*
218100170621     C* Splitto il 4° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
218200170621     C                   EVAL      InStringa = w2048_04
218300170621     C                   EVAL      InSepara = ','
218400170621     C                   SETOFF                                       131419
218500170621     C                   SETON                                        13
218600170621     C                   EXSR      SR_SPLIT
218700170621     C                   EVAL      wSkCSV_03_w4  = %subarr(SkSplitCSV_3:1)
218800170621     C                   EVAL      sOutIdxMax_w4 = OutIdxMax
218900170621     C*
219000170621     C* Verifico eventuale assenza di elementi che in tal caso forzo a 1
219100170621     C                   IF        sOutIdxMax_w1 = *zeros
219200170621     C                   EVAL      sOutIdxMax_w1 = 1
219300170621     C                   ENDIF
219400170621     C                   IF        sOutIdxMax_w2 = *zeros
219500170621     C                   EVAL      sOutIdxMax_w2 = 1
219600170621     C                   ENDIF
219700170621     C                   IF        sOutIdxMax_w3 = *zeros
219800170621     C                   EVAL      sOutIdxMax_w3 = 1
219900170621     C                   ENDIF
220000170621     C                   IF        sOutIdxMax_w4 = *zeros
220100170621     C                   EVAL      sOutIdxMax_w4 = 1
220200170621     C                   ENDIF
220300170621     C*
220400170621     C* Sviluppo tutti gli elementi "moltiplicando" le liste delle RULE FROM/SOCODE/TO
220500170621     C*
220600170621     C* 1° lista
220700170621     C                   Z-ADD     1             i                 4 0
220800170621     C                   DOW       i <= %elem(wSkCSV_03_w1)
220900170621     C                             AND i <= sOutIdxMax_w1
221000170621     C*
221100170621     C* Determino il tipo di RULE FROM
221200170621     C                   SELECT
221300170621     C* DEPOT - Single
221400170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'D' AND
221500170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 8
221600170621     C                   EVAL      CWSRULFT = 'D'
221700170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:7)
221800170621     C                   EVAL      CWSRULFE = CWSRULFB
221900170621     C* DEPOT - Range
222000170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'D' AND
222100170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 15
222200170621     C                   EVAL      CWSRULFT = 'D'
222300170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:7)
222400170621     C                   EVAL      CWSRULFE = %subst(wSkCSV_03_w1(i):9:7)
222500170621     C* DEPOT GROUP - Single
222600170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'G'
222700170621     C                   EVAL      CWSRULFT = 'G'
222800170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:4)
222900170621     C                   EVAL      CWSRULFE = CWSRULFB
223000170621     C* COUNTRY - Single
223100170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'C'
223200170621     C                   EVAL      CWSRULFT = 'C'
223300170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:2)
223400170621     C                   EVAL      CWSRULFE = CWSRULFB
223500170621     C* NETWORK - Single
223600170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'N'
223700170621     C                   EVAL      CWSRULFT = 'N'
223800170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:3)
223900170621     C                   EVAL      CWSRULFE = CWSRULFB
224000170621     C* IC - Single
224100170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'I'
224200170621     C                   EVAL      CWSRULFT = 'I'
224300170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:9)
224400170621     C                   EVAL      CWSRULFE = CWSRULFB
224500170621     C* BU - Single
224600170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'B' AND
224700170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 4
224800170621     C                   EVAL      CWSRULFT = 'B'
224900170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:3)
225000170621     C                   EVAL      CWSRULFE = CWSRULFB
225100170621     C* BU+COUNTRY - Single
225200170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'B' AND
225300170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 6
225400170621     C                   EVAL      CWSRULFT = 'B'
225500170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:5)
225600170621     C                   EVAL      CWSRULFE = CWSRULFB
225700170621     C*
225800170621     C                   ENDSL
225900170621     C*
226000170621     C* 2° lista
226100170621     C                   Z-ADD     1             j                 4 0
226200170621     C                   DOW       j <= %elem(wSkCSV_03_w2)
226300170621     C                             AND j <= sOutIdxMax_w2
226400170621     C*
226500170621     C* Determino il tipo di RULE SERVICE
226600170621     C                   SELECT
226700170621     C* SOCODE - Single
226800170621     C                   WHEN      %subst(wSkCSV_03_w2(j):1:2) = 'SO' AND
226900170621     C                             %len(%trim((wSkCSV_03_w2(j)))) = 5
227000170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):3:3)
227100170621     C                   EVAL      CWSRULSB = %dec(wSORC:3:0)
227200170621     C                   EVAL      CWSRULSE = CWSRULSB
227300170621     C* SOCODE - Range
227400170621     C                   WHEN      %subst(wSkCSV_03_w2(j):1:2) = 'SO' AND
227500170621     C                             %len(%trim((wSkCSV_03_w2(j)))) = 8
227600170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):3:3)
227700170621     C                   EVAL      CWSRULSB = %dec(wSORC:3:0)
227800170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):6:3)
227900170621     C                   EVAL      CWSRULSE = %dec(wSORC:3:0)
228000170621     C*
228100170621     C                   ENDSL
228200170621     C*
228300170621     C* 3° lista
228400170621     C                   Z-ADD     1             z                 4 0
228500170621     C                   DOW       z <= %elem(wSkCSV_03_w3)
228600170621     C                             AND z <= sOutIdxMax_w3
228700170621     C*
228800170621     C* Determino il tipo di RULE TO
228900170621     C                   SELECT
229000170621     C* DEPOT - Single
229100170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'D' AND
229200170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 8
229300170621     C                   EVAL      CWSRULTT = 'D'
229400170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:7)
229500170621     C                   EVAL      CWSRULTE = CWSRULTB
229600170621     C* DEPOT - Range
229700170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'D' AND
229800170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 15
229900170621     C                   EVAL      CWSRULTT = 'D'
230000170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:7)
230100170621     C                   EVAL      CWSRULTE = %subst(wSkCSV_03_w3(z):9:7)
230200170621     C* DEPOT GROUP - Single
230300170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'G'
230400170621     C                   EVAL      CWSRULTT = 'G'
230500170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:4)
230600170621     C                   EVAL      CWSRULTE = CWSRULTB
230700170621     C* COUNTRY - Single
230800170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'C'
230900170621     C                   EVAL      CWSRULTT = 'C'
231000170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:2)
231100170621     C                   EVAL      CWSRULTE = CWSRULTB
231200170621     C* NETWORK - Single
231300170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'N'
231400170621     C                   EVAL      CWSRULTT = 'N'
231500170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:3)
231600170621     C                   EVAL      CWSRULTE = CWSRULTB
231700170621     C* COUNTRY+ZIP CODE - Single
231800170629     C***                WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'Z' AND
231900170629     C***                          %scan('-':wSkCSV_03_w3(z)) = *zeros
232000170629     C***                EVAL      CWSRULTT = 'Z'
232100170629     C***                EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:11)
232200170629     C***                EVAL      CWSRULTE = CWSRULTB
232300170621     C* COUNTRY+ZIP CODE - Range
232400170629     C***                WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'Z' AND
232500170629     C***                          %scan('-':wSkCSV_03_w3(z)) > *zeros
232600170629     C***                EVAL      CWSRULTT = 'Z'
232700170629     C***                EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:
232800170629     C***                                     %scan('-':wSkCSV_03_w3(z))-2)
232900170629     C***                EVAL      CWSRULTE = %subst(wSkCSV_03_w3(z):2:2)+
233000170629     C***                                     %subst(wSkCSV_03_w3(z):
233100170629     C***                                     %scan('-':wSkCSV_03_w3(z))+1:9)
233200170621     C* BU+COUNTRY - Single
233300170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'B' AND
233400170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 6
233500170621     C                   EVAL      CWSRULTT = 'B'
233600170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w1(z):2:5)
233700170621     C                   EVAL      CWSRULTE = CWSRULTB
233800170621     C*
233900170621     C                   ENDSL
234000170621     C*
234100170621     C* 4° lista
234200170621     C                   Z-ADD     1             x                 4 0
234300170621     C                   DOW       x <= %elem(wSkCSV_03_w4)
234400170621     C                             AND x <= sOutIdxMax_w4
234500170621     C*
234600170629     C* Determino il tipo di ZONE TO
234700170621     C                   SELECT
234800170621     C* COUNTRY+ZIP CODE - Single
234900170621     C                   WHEN      %subst(wSkCSV_03_w4(x):1:1) = 'Z' AND
235000170621     C                             %scan('-':wSkCSV_03_w4(x)) = *zeros
235100170621     C                   EVAL      CWSZONTB = %subst(wSkCSV_03_w4(x):2:11)
235200170621     C                   EVAL      CWSZONTE = CWSZONTB
235300170621     C* COUNTRY+ZIP CODE - Range
235400170621     C                   WHEN      %subst(wSkCSV_03_w4(x):1:1) = 'Z' AND
235500170621     C                             %scan('-':wSkCSV_03_w4(x)) > *zeros
235600170621     C                   EVAL      CWSZONTB = %subst(wSkCSV_03_w4(x):2:
235700170621     C                                        %scan('-':wSkCSV_03_w4(x))-2)
235800170621     C                   EVAL      CWSZONTE = %subst(wSkCSV_03_w4(x):2:2)+
235900170621     C                                        %subst(wSkCSV_03_w4(x):
236000170621     C                                        %scan('-':wSkCSV_03_w4(x))+1:9)
236100170621     C*
236200170621     C                   ENDSL
236300170621     C*
236400170621     C* Inserisco il record corrente
236500170621     C                   WRITE     DPCWS100
236600170621     C*
236700170621     C                   EVAL      x = x + 1
236800170621     C                   ENDDO
236900170621     C*
237000170621     C                   EVAL      z = z + 1
237100170621     C                   ENDDO
237200170621     C*
237300170621     C                   EVAL      j = j + 1
237400170621     C                   ENDDO
237500170621     C*
237600170621     C                   EVAL      i = i + 1
237700170621     C                   ENDDO
237800170619     C*
237900170619     C                   ENDSR
238000170619     C***
238100170629
238200170629
238300170629     C*----------------------------------------------------*
238400170629     C*  GESTIONE MAPPATURA "ALLOWAS"
238500170629     C*----------------------------------------------------*
238600170629     C     GESCWA        BEGSR
238700170629     C*
238800170629     C* Inizializzo il formato record del file relativo alla mappatura corrente
238900170629     C                   CLEAR                   DPCWA100
239000170629     C*
239100170629     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
239200170629     C                   Z-ADD     2             i                 4 0
239300170629     C                   DOW       i <= %elem(SkSplitFLD)
239400170629     C                             AND i <= OutIdxMax
239500170629     C*
239600170629     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
239700170629     C                   IF        SkSplitFLD(i)   <> *blanks AND
239800170629     C                             SkSplitCSV_9(i) <> *blanks
239900170629     C*
240000170629     C*** ALWID
240100170629     C                   IF        %trim(SkSplitFLD(i)) = 'UNIQUEALLOWID'
240200170629     C                   EVAL      PiStr = SkSplitCSV_9(i)
240300170629     C                   EXSR      CHKNUM
240400170629     C                   IF        PiInt=*on
240500170629     C                   Z-ADD     PiVal         CWAALWID
240600170629     C                   ENDIF
240700170629     C                   ENDIF
240800170629     C*** RULSB / RULSE
240900170629     C                   IF        %trim(SkSplitFLD(i)) = 'RULESERVICE'
241000170629     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
241100170629     C                   ENDIF
241200170629     C*** ZONTB / ZONTE
241300170629     C                   IF        %trim(SkSplitFLD(i)) = 'ZONETO'
241400170629     C                   EVAL      w2048_02 = SkSplitCSV_9(i)
241500170629     C                   ENDIF
241600170629     C*
241700170629     C                   ENDIF
241800170629     C                   EVAL      i = i + 1
241900170629     C                   ENDDO
242000170629     C*
242100170629     C*** VER
242200170629     C                   EVAL      CWAVER = NEW_VERSION
242300170629     C*
242400170629     C* Splitto il 1° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
242500170629     C                   EVAL      InStringa = w2048_01
242600170629     C                   EVAL      InSepara = ','
242700170629     C                   SETOFF                                       131419
242800170629     C                   SETON                                        13
242900170629     C                   EXSR      SR_SPLIT
243000170629     C                   EVAL      wSkCSV_03_w1  = %subarr(SkSplitCSV_3:1)
243100170629     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
243200170629     C*
243300170629     C* Splitto il 2° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
243400170629     C                   EVAL      InStringa = w2048_02
243500170629     C                   EVAL      InSepara = ','
243600170629     C                   SETOFF                                       131419
243700170629     C                   SETON                                        13
243800170629     C                   EXSR      SR_SPLIT
243900170629     C                   EVAL      wSkCSV_03_w2  = %subarr(SkSplitCSV_3:1)
244000170629     C                   EVAL      sOutIdxMax_w2 = OutIdxMax
244100170629     C*
244200170629     C* Verifico eventuale assenza di elementi che in tal caso forzo a 1
244300170629     C                   IF        sOutIdxMax_w1 = *zeros
244400170629     C                   EVAL      sOutIdxMax_w1 = 1
244500170629     C                   ENDIF
244600170629     C                   IF        sOutIdxMax_w2 = *zeros
244700170629     C                   EVAL      sOutIdxMax_w2 = 1
244800170629     C                   ENDIF
244900170629     C*
245000170629     C* Sviluppo tutti gli elementi "moltiplicando" le liste delle RULE SERVICE/ZONE
245100170629     C*
245200170629     C* 1° lista
245300170629     C                   Z-ADD     1             i                 4 0
245400170629     C                   DOW       i <= %elem(wSkCSV_03_w1)
245500170629     C                             AND i <= sOutIdxMax_w1
245600170629     C*
245700170629     C* Determino il tipo di RULE SERVICE
245800170629     C                   SELECT
245900170724     C* ASCODE - Single
246000170629     C                   WHEN      %subst(wSkCSV_03_w1(j):1:2) = 'AS' AND
246100170629     C                             %len(%trim((wSkCSV_03_w1(j)))) = 5
246200170629     C                   EVAL      CWARULSB = %subst(wSkCSV_03_w1(j):3:3)
246300170629     C                   EVAL      CWARULSE = CWARULSB
246400170724     C* ASCODE - Range
246500170629     C                   WHEN      %subst(wSkCSV_03_w1(j):1:2) = 'AS' AND
246600170629     C                             %len(%trim((wSkCSV_03_w1(j)))) = 8
246700170629     C                   EVAL      CWARULSB = %subst(wSkCSV_03_w1(j):3:3)
246800170629     C                   EVAL      CWARULSE = %subst(wSkCSV_03_w1(j):6:3)
246900170629     C*
247000170629     C                   ENDSL
247100170629     C*
247200170629     C* 2° lista
247300170629     C                   Z-ADD     1             j                 4 0
247400170629     C                   DOW       j <= %elem(wSkCSV_03_w2)
247500170629     C                             AND j <= sOutIdxMax_w2
247600170629     C*
247700170629     C* Determino il tipo di ZONE TO
247800170629     C                   SELECT
247900170629     C* COUNTRY+ZIP CODE - Single
248000170629     C                   WHEN      %subst(wSkCSV_03_w2(x):1:1) = 'Z' AND
248100170629     C                             %scan('-':wSkCSV_03_w2(x)) = *zeros
248200170629     C                   EVAL      CWAZONTB = %subst(wSkCSV_03_w2(x):2:11)
248300170629     C                   EVAL      CWAZONTE = CWAZONTB
248400170629     C* COUNTRY+ZIP CODE - Range
248500170629     C                   WHEN      %subst(wSkCSV_03_w2(x):1:1) = 'Z' AND
248600170629     C                             %scan('-':wSkCSV_03_w2(x)) > *zeros
248700170629     C                   EVAL      CWAZONTB = %subst(wSkCSV_03_w2(x):2:
248800170629     C                                        %scan('-':wSkCSV_03_w2(x))-2)
248900170629     C                   EVAL      CWAZONTE = %subst(wSkCSV_03_w2(x):2:2)+
249000170629     C                                        %subst(wSkCSV_03_w2(x):
249100170629     C                                        %scan('-':wSkCSV_03_w2(x))+1:9)
249200170629     C*
249300170629     C                   ENDSL
249400170629     C*
249500170629     C* Inserisco il record corrente
249600170629     C                   WRITE     DPCWA100
249700170629     C*
249800170629     C                   EVAL      j = j + 1
249900170629     C                   ENDDO
250000170629     C*
250100170629     C                   EVAL      i = i + 1
250200170629     C                   ENDDO
250300170629     C*
250400170629     C                   ENDSR
250500170629     C***
250600170619
250700170619
250800170619     C*----------------------------------------------------*
250900170619     C*  GESTIONE MAPPATURA "RELATION"
251000170619     C*----------------------------------------------------*
251100170619     C     GESCRE        BEGSR
251200170619     C*
251300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
251400170619     C                   CLEAR                   DPCRE100
251500170619     C*
251600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
251700170619     C                   Z-ADD     2             i                 4 0
251800170620     C                   DOW       i <= %elem(SkSplitFLD)
251900170620     C                             AND i <= OutIdxMax
252000170619     C*
252100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
252200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
252300170619     C                             SkSplitCSV_9(i) <> *blanks
252400170619     C*
252500170619     C*** DPTN
252600170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
252700170619     C                   EVAL      CREDPTN  = SkSplitCSV_9(i)
252800170619     C                   ENDIF
252900170619     C*** ROUT
253000170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTE'
253100170619     C                   EVAL      CREROUT  = SkSplitCSV_9(i)
253200170619     C                   ENDIF
253300170619     C*** RELA
253400170619     C                   IF        %trim(SkSplitFLD(i)) = 'RELATION'
253500170619     C                   EVAL      CRERELA  = SkSplitCSV_9(i)
253600170619     C                   ENDIF
253700170619     C*
253800170619     C                   ENDIF
253900170619     C                   EVAL      i = i + 1
254000170619     C                   ENDDO
254100170619     C*
254200170619     C*** VER
254300170619     C                   EVAL      CREVER = NEW_VERSION
254400170619     C*
254500170619     C* Infine inserisco il record corrente
254600170619     C                   WRITE     DPCRE100
254700170619     C*
254800170619     C                   ENDSR
254900170619     C***
255000170619
255100170619
255200170619     C*----------------------------------------------------*
255300170619     C*  GESTIONE MAPPATURA "DEPOTS"
255400170619     C*----------------------------------------------------*
255500170619     C     GESCDP        BEGSR
255600170619     C*
255700170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
255800170619     C                   CLEAR                   DPCDP100
255900170619     C*
256000170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
256100170619     C                   Z-ADD     2             i                 4 0
256200170620     C                   DOW       i <= %elem(SkSplitFLD)
256300170620     C                             AND i <= OutIdxMax
256400170619     C*
256500170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
256600170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
256700170619     C                             SkSplitCSV_9(i) <> *blanks
256800170619     C*
256900170619     C*** DPTN
257000170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
257100170619     C                   EVAL      CDPDPTN  = SkSplitCSV_9(i)
257200170619     C                   ENDIF
257300170619     C*** DSTR
257400170619     C                   IF        %trim(SkSplitFLD(i)) = 'DEPOTSTR'
257500170619     C                   EVAL      CDPDSTR  = SkSplitCSV_9(i)
257600170619     C                   ENDIF
257700170619     C*** IATA
257800170619     C                   IF        %trim(SkSplitFLD(i)) = 'IATALIKECODE'
257900170619     C                   EVAL      CDPIATA  = SkSplitCSV_9(i)
258000170619     C                   ENDIF
258100170619     C*** GRPID
258200170619     C                   IF        %trim(SkSplitFLD(i)) = 'GROUPID'
258300170619     C                   EVAL      CDPGRPID = SkSplitCSV_9(i)
258400170619     C                   ENDIF
258500170619     C*** NAME1
258600170619     C                   IF        %trim(SkSplitFLD(i)) = 'COMPANYNAME'
258700170619     C                   EVAL      CDPNAME1 = SkSplitCSV_9(i)
258800170619     C                   ENDIF
258900170619     C*** NAME2
259000170619     C                   IF        %trim(SkSplitFLD(i)) = 'COMPANYNAME2'
259100170619     C                   EVAL      CDPNAME2 = SkSplitCSV_9(i)
259200170619     C                   ENDIF
259300170619     C*** STRET
259400170619     C                   IF        %trim(SkSplitFLD(i)) = 'STREET'
259500170619     C                   EVAL      CDPSTRET = SkSplitCSV_9(i)
259600170619     C                   ENDIF
259700170619     C*** STNBR
259800170619     C                   IF        %trim(SkSplitFLD(i)) = 'PROPNUM'
259900170619     C                   EVAL      CDPSTNBR = SkSplitCSV_9(i)
260000170619     C                   ENDIF
260100170619     C*** ADDR1
260200170619     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS2'
260300170619     C                   EVAL      CDPADDR1 = SkSplitCSV_9(i)
260400170619     C                   ENDIF
260500170619     C*** ADDR2
260600170619     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS3'
260700170619     C                   EVAL      CDPADDR2 = SkSplitCSV_9(i)
260800170619     C                   ENDIF
260900170619     C*** FLOOR
261000170619     C                   IF        %trim(SkSplitFLD(i)) = 'FLOOR'
261100170619     C                   EVAL      CDPFLOOR = SkSplitCSV_9(i)
261200170619     C                   ENDIF
261300170619     C*** BUILD
261400170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUILDING'
261500170619     C                   EVAL      CDPBUILD = SkSplitCSV_9(i)
261600170619     C                   ENDIF
261700170619     C*** ISO2
261800170619     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
261900170619     C                   EVAL      CDPISO2  = SkSplitCSV_9(i)
262000170619     C                   ENDIF
262100170619     C*** STATE
262200170619     C                   IF        %trim(SkSplitFLD(i)) = 'STATE'
262300170619     C                   EVAL      CDPSTATE = SkSplitCSV_9(i)
262400170619     C                   ENDIF
262500170619     C*** PTC
262600170619     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODE'
262700170619     C                   EVAL      CDPPTC   = SkSplitCSV_9(i)
262800170619     C                   ENDIF
262900170619     C*** CITY
263000170619     C                   IF        %trim(SkSplitFLD(i)) = 'CITYNAME'
263100170619     C                   EVAL      CDPCITY  = SkSplitCSV_9(i)
263200170619     C                   ENDIF
263300170619     C*** TEL
263400170619     C                   IF        %trim(SkSplitFLD(i)) = 'PHONE'
263500170619     C                   EVAL      CDPTEL   = SkSplitCSV_9(i)
263600170619     C                   ENDIF
263700170619     C*** FAX
263800170619     C                   IF        %trim(SkSplitFLD(i)) = 'FAX'
263900170619     C                   EVAL      CDPFAX   = SkSplitCSV_9(i)
264000170619     C                   ENDIF
264100170619     C*** EMAIL
264200170619     C                   IF        %trim(SkSplitFLD(i)) = 'MAIL'
264300170619     C                   EVAL      CDPEMAIL = SkSplitCSV_9(i)
264400170619     C                   ENDIF
264500170619     C*** WEB
264600170619     C                   IF        %trim(SkSplitFLD(i)) = 'WEB'
264700170619     C                   EVAL      CDPWEB   = SkSplitCSV_9(i)
264800170619     C                   ENDIF
264900170619     C*** BUCN
265000170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
265100170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
265200170619     C                   EXSR      CHKNUM
265300170619     C                   IF        PiInt=*on
265400170619     C                   Z-ADD     PiVal         CDPBUCN
265500170619     C                   ENDIF
265600170619     C                   ENDIF
265700170619     C*** GEOX
265800170619     C                   IF        %trim(SkSplitFLD(i)) = 'SGPSLAT'
265900170619     C                   EVAL      CDPGEOX  = SkSplitCSV_9(i)
266000170619     C                   ENDIF
266100170619     C*** GEOY
266200170619     C                   IF        %trim(SkSplitFLD(i)) = 'SGPSLONG'
266300170619     C                   EVAL      CDPGEOY  = SkSplitCSV_9(i)
266400170619     C                   ENDIF
266500170619     C*** DUMY
266600170619     C                   IF        %trim(SkSplitFLD(i)) = 'DUMMYDEPOT'
266700170619     C                   EVAL      CDPDUMY  = SkSplitCSV_9(i)
266800170619     C                   ENDIF
266900170619     C*
267000170619     C                   ENDIF
267100170619     C                   EVAL      i = i + 1
267200170619     C                   ENDDO
267300170619     C*
267400170619     C*** VER
267500170706     C                   EVAL      CDPVER  = NEW_VERSION
267600170619     C*
267700170920     C** caricamento file LEGAMI depot DPD Linea/Cliente
267800170920     c                   if          CDPISO2 <> *blank
267900170920     c                   call      'TISIDLEGR'
268000170920     c                   parm                    CDPDPTN
268100170920     c                   parm                    CDPISO2
268200170929     c                   parm                    CVEDDE
268300170929     c                   parm                    CVEDSC
268400170920     c                   end
268500170920     C*
268600170619     C* Infine inserisco il record corrente
268700170619     C                   WRITE     DPCDP100
268800170619     C*
268900170619     C                   ENDSR
269000170619     C***
269100170619
269200170619
269300170619     C*----------------------------------------------------*
269400170619     C*  GESTIONE MAPPATURA "R1DBU"
269500170619     C*----------------------------------------------------*
269600170619     C     GESCR1        BEGSR
269700170619     C*
269800170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
269900170619     C                   CLEAR                   DPCR1100
270000170619     C*
270100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
270200170619     C                   Z-ADD     2             i                 4 0
270300170620     C                   DOW       i <= %elem(SkSplitFLD)
270400170620     C                             AND i <= OutIdxMax
270500170619     C*
270600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
270700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
270800170619     C                             SkSplitCSV_9(i) <> *blanks
270900170621     C*
271000170621     C*** ROUT / ROUPB / ROUPE
271100170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
271200170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
271300170619     C                   ENDIF
271400170619     C*** TRADS
271500170619     C                   IF        %trim(SkSplitFLD(i)) = 'TRADESTATUS'
271600170619     C                   EVAL      CR1TRADS = SkSplitCSV_9(i)
271700170619     C                   ENDIF
271800170619     C*** ISO2
271900170619     C                   IF        %trim(SkSplitFLD(i)) =
272000170619     C                                        'DESTINATIONCOUNTRYCODE'
272100170619     C                   EVAL      CR1ISO2  = SkSplitCSV_9(i)
272200170619     C                   ENDIF
272300170619     C*** PTCB
272400170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
272500170619     C                   EVAL      CR1PTCB  = SkSplitCSV_9(i)
272600170619     C                   ENDIF
272700170619     C*** PTCE
272800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
272900170619     C                   EVAL      CR1PTCE  = SkSplitCSV_9(i)
273000170619     C                   ENDIF
273100170619     C*** NTWP
273200170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
273300170619     C                   EVAL      CR1NTWP  = SkSplitCSV_9(i)
273400170619     C                   ENDIF
273500170619     C*** PTY
273600170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
273700170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
273800170619     C                   EXSR      CHKNUM
273900170619     C                   IF        PiInt=*on
274000170619     C                   Z-ADD     PiVal         CR1PTY
274100170619     C                   ENDIF
274200170619     C                   ENDIF
274300170619     C*** BUCN
274400170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
274500170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
274600170619     C                   EXSR      CHKNUM
274700170619     C                   IF        PiInt=*on
274800170619     C                   Z-ADD     PiVal         CR1BUCN
274900170619     C                   ENDIF
275000170619     C                   ENDIF
275100170619     C*
275200170619     C                   ENDIF
275300170619     C                   EVAL      i = i + 1
275400170619     C                   ENDDO
275500170619     C*
275600170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
275700170619     C                   IF        CR1PTCE = *blanks
275800170619     C                   EVAL      CR1PTCE = CR1PTCB
275900170619     C                   ENDIF
276000170619     C*
276100170619     C*** VER
276200170619     C                   EVAL      CR1VER = NEW_VERSION
276300170620     C*
276400170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
276500170620     C                   EVAL      InStringa = w2048_01
276600170620     C                   EVAL      InSepara = ','
276700170620     C                   SETOFF                                       131419
276800170620     C                   SETON                                        13
276900170620     C                   EXSR      SR_SPLIT
277000170620     C*
277100170620     C* Se nessun elemento => scrivo comunque un record singolo
277200170620     C                   IF        OutIdxMax = *zeros
277300170620     C                   WRITE     DPCR1100
277400170620     C                   ELSE
277500170620     C*
277600170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
277700170620     C                   Z-ADD     1             j                 4 0
277800170620     C                   DOW       j <= %elem(SkSplitCSV_3)
277900170620     C                             AND j <= OutIdxMax
278000170620     C*
278100170620     C* Determino il tipo di Routing Place
278200170620     C                   SELECT
278300170620     C* Injection Code - Range
278400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
278500170620     C                   EVAL      CR1ROUT  = 'I'
278600170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):3:9)
278700170620     C                   EVAL      CR1ROUPE = %subst(SkSplitCSV_3(j):12:9)
278800170620     C* Injection Code - Single
278900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
279000170620     C                   EVAL      CR1ROUT  = 'I'
279100170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:9)
279200170620     C                   EVAL      CR1ROUPE = CR1ROUPB
279300170620     C* Country Code
279400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
279500170620     C                   EVAL      CR1ROUT  = 'C'
279600170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:2)
279700170620     C                   EVAL      CR1ROUPE = CR1ROUPB
279800170620     C* Depot Code - Range
279900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
280000170620     C                   EVAL      CR1ROUT  = 'D'
280100170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):3:7)
280200170620     C                   EVAL      CR1ROUPE = %subst(SkSplitCSV_3(j):10:7)
280300170620     C* Depot Code - Single
280400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
280500170620     C                   EVAL      CR1ROUT  = 'D'
280600170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:7)
280700170620     C                   EVAL      CR1ROUPE = CR1ROUPB
280800170620     C* Depot Group
280900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
281000170620     C                   EVAL      CR1ROUT  = 'G'
281100170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:4)
281200170620     C                   EVAL      CR1ROUPE = CR1ROUPB
281300170620     C* Business Unit
281400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
281500170620     C                   EVAL      CR1ROUT  = 'B'
281600170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:3)
281700170620     C                   EVAL      CR1ROUPE = CR1ROUPB
281800170620     C*
281900170620     C                   ENDSL
282000170619     C*
282100170619     C* Infine inserisco il record corrente
282200170619     C                   WRITE     DPCR1100
282300170620     C*
282400170620     C                   EVAL      j = j + 1
282500170620     C                   ENDDO
282600170620     C                   ENDIF
282700170619     C*
282800170619     C                   ENDSR
282900170619     C***
283000170619
283100170619
283200170619     C*----------------------------------------------------*
283300170619     C*  GESTIONE MAPPATURA "R2ROUTE"
283400170619     C*----------------------------------------------------*
283500170619     C     GESCR2        BEGSR
283600170619     C*
283700170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
283800170619     C                   CLEAR                   DPCR2100
283900170619     C*
284000170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
284100170619     C                   Z-ADD     2             i                 4 0
284200170620     C                   DOW       i <= %elem(SkSplitFLD)
284300170620     C                             AND i <= OutIdxMax
284400170619     C*
284500170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
284600170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
284700170619     C                             SkSplitCSV_9(i) <> *blanks
284800170619     C*
284900170621     C*** ROUT / ROUPB / ROUPE
285000170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
285100170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
285200170619     C                   ENDIF
285300170619     C*** BUCN
285400170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
285500170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
285600170619     C                   EXSR      CHKNUM
285700170619     C                   IF        PiInt=*on
285800170619     C                   Z-ADD     PiVal         CR2BUCN
285900170619     C                   ENDIF
286000170619     C                   ENDIF
286100170619     C*** ISO2
286200170619     C                   IF        %trim(SkSplitFLD(i)) =
286300170619     C                                        'DESTINATIONCOUNTRYCODE'
286400170619     C                   EVAL      CR2ISO2  = SkSplitCSV_9(i)
286500170619     C                   ENDIF
286600170619     C*** TRADS
286700170619     C                   IF        %trim(SkSplitFLD(i)) = 'TRADESTATUS'
286800170619     C                   EVAL      CR2TRADS = SkSplitCSV_9(i)
286900170619     C                   ENDIF
287000170619     C*** PTCB
287100170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
287200170619     C                   EVAL      CR2PTCB  = SkSplitCSV_9(i)
287300170619     C                   ENDIF
287400170619     C*** PTCE
287500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
287600170619     C                   EVAL      CR2PTCE  = SkSplitCSV_9(i)
287700170619     C                   ENDIF
287800170619     C*** NTWP
287900170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
288000170619     C                   EVAL      CR2NTWP  = SkSplitCSV_9(i)
288100170619     C                   ENDIF
288200170619     C*** PTY
288300170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
288400170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
288500170619     C                   EXSR      CHKNUM
288600170619     C                   IF        PiInt=*on
288700170619     C                   Z-ADD     PiVal         CR2PTY
288800170619     C                   ENDIF
288900170619     C                   ENDIF
289000170619     C*** DDPT
289100170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
289200170619     C                   EVAL      CR2DDPT  = SkSplitCSV_9(i)
289300170619     C                   ENDIF
289400170619     C*** SSORT
289500170619     C                   IF        %trim(SkSplitFLD(i)) = 'SSORT'
289600170619     C                   EVAL      CR2SSORT = SkSplitCSV_9(i)
289700170619     C                   ENDIF
289800170619     C*** PTNC
289900170619     C                   IF        %trim(SkSplitFLD(i)) = 'PARTNERCODE'
290000170619     C                   EVAL      CR2PTNC  = SkSplitCSV_9(i)
290100170619     C                   ENDIF
290200170619     C*
290300170619     C                   ENDIF
290400170619     C                   EVAL      i = i + 1
290500170619     C                   ENDDO
290600170619     C*
290700170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
290800170619     C                   IF        CR2PTCE = *blanks
290900170619     C                   EVAL      CR2PTCE = CR2PTCB
291000170619     C                   ENDIF
291100170619     C*
291200170619     C*** VER
291300170619     C                   EVAL      CR2VER = NEW_VERSION
291400170620     C*
291500170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
291600170620     C                   EVAL      InStringa = w2048_01
291700170620     C                   EVAL      InSepara = ','
291800170620     C                   SETOFF                                       131419
291900170620     C                   SETON                                        13
292000170620     C                   EXSR      SR_SPLIT
292100170620     C*
292200170620     C* Se nessun elemento => scrivo comunque un record singolo
292300170620     C                   IF        OutIdxMax = *zeros
292400170620     C                   WRITE     DPCR2100
292500170620     C                   ELSE
292600170620     C*
292700170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
292800170620     C                   Z-ADD     1             j                 4 0
292900170620     C                   DOW       j <= %elem(SkSplitCSV_3)
293000170620     C                             AND j <= OutIdxMax
293100170620     C*
293200170620     C* Determino il tipo di Routing Place
293300170620     C                   SELECT
293400170620     C* Injection Code - Range
293500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
293600170620     C                   EVAL      CR2ROUT  = 'I'
293700170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):3:9)
293800170620     C                   EVAL      CR2ROUPE = %subst(SkSplitCSV_3(j):12:9)
293900170620     C* Injection Code - Single
294000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
294100170620     C                   EVAL      CR2ROUT  = 'I'
294200170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:9)
294300170620     C                   EVAL      CR2ROUPE = CR2ROUPB
294400170620     C* Country Code
294500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
294600170620     C                   EVAL      CR2ROUT  = 'C'
294700170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:2)
294800170620     C                   EVAL      CR2ROUPE = CR2ROUPB
294900170620     C* Depot Code - Range
295000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
295100170620     C                   EVAL      CR2ROUT  = 'D'
295200170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):3:7)
295300170620     C                   EVAL      CR2ROUPE = %subst(SkSplitCSV_3(j):10:7)
295400170620     C* Depot Code - Single
295500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
295600170620     C                   EVAL      CR2ROUT  = 'D'
295700170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:7)
295800170620     C                   EVAL      CR2ROUPE = CR2ROUPB
295900170620     C* Depot Group
296000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
296100170620     C                   EVAL      CR2ROUT  = 'G'
296200170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:4)
296300170620     C                   EVAL      CR2ROUPE = CR2ROUPB
296400170620     C* Business Unit
296500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
296600170620     C                   EVAL      CR2ROUT  = 'B'
296700170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:3)
296800170620     C                   EVAL      CR2ROUPE = CR2ROUPB
296900170620     C*
297000170620     C                   ENDSL
297100170620     C*
297200170620     C* Infine inserisco il record corrente
297300170620     C                   WRITE     DPCR2100
297400170620     C*
297500170620     C                   EVAL      j = j + 1
297600170620     C                   ENDDO
297700170620     C                   ENDIF
297800170619     C*
297900170619     C                   ENDSR
298000170619     C***
298100170619
298200170619
298300170619     C*----------------------------------------------------*
298400170619     C*  GESTIONE MAPPATURA "R3OSORT"
298500170619     C*----------------------------------------------------*
298600170619     C     GESCR3        BEGSR
298700170619     C*
298800170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
298900170619     C                   CLEAR                   DPCR3100
299000170619     C*
299100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
299200170619     C                   Z-ADD     2             i                 4 0
299300170620     C                   DOW       i <= %elem(SkSplitFLD)
299400170620     C                             AND i <= OutIdxMax
299500170619     C*
299600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
299700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
299800170619     C                             SkSplitCSV_9(i) <> *blanks
299900170619     C*
300000170621     C*** ROUT / ROUPB / ROUPE
300100170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
300200170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
300300170619     C                   ENDIF
300400170619     C*** ISO2
300500170619     C                   IF        %trim(SkSplitFLD(i)) =
300600170619     C                                        'DESTINATIONCOUNTRYCODE'
300700170619     C                   EVAL      CR3ISO2  = SkSplitCSV_9(i)
300800170619     C                   ENDIF
300900170619     C*** DDPT
301000170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
301100170619     C                   EVAL      CR3DDPT  = SkSplitCSV_9(i)
301200170619     C                   ENDIF
301300170619     C*** PTCB
301400170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
301500170619     C                   EVAL      CR3PTCB  = SkSplitCSV_9(i)
301600170619     C                   ENDIF
301700170619     C*** PTCE
301800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
301900170619     C                   EVAL      CR3PTCE  = SkSplitCSV_9(i)
302000170619     C                   ENDIF
302100170619     C*** NTWP
302200170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
302300170619     C                   EVAL      CR3NTWP  = SkSplitCSV_9(i)
302400170619     C                   ENDIF
302500170619     C*** PTY
302600170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
302700170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
302800170619     C                   EXSR      CHKNUM
302900170619     C                   IF        PiInt=*on
303000170619     C                   Z-ADD     PiVal         CR3PTY
303100170619     C                   ENDIF
303200170619     C                   ENDIF
303300170619     C*** OSORT
303400170619     C                   IF        %trim(SkSplitFLD(i)) = 'OSORT'
303500170620     C                   EVAL      CR3OSORT = SkSplitCSV_9(i)
303600170619     C                   ENDIF
303700170619     C*
303800170619     C                   ENDIF
303900170619     C                   EVAL      i = i + 1
304000170619     C                   ENDDO
304100170619     C*
304200170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
304300170619     C                   IF        CR3PTCE = *blanks
304400170619     C                   EVAL      CR3PTCE = CR3PTCB
304500170619     C                   ENDIF
304600170619     C*
304700170619     C*** VER
304800170619     C                   EVAL      CR3VER = NEW_VERSION
304900170620     C*
305000170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
305100170620     C                   EVAL      InStringa = w2048_01
305200170620     C                   EVAL      InSepara = ','
305300170620     C                   SETOFF                                       131419
305400170620     C                   SETON                                        13
305500170620     C                   EXSR      SR_SPLIT
305600170620     C*
305700170620     C* Se nessun elemento => scrivo comunque un record singolo
305800170620     C                   IF        OutIdxMax = *zeros
305900170620     C                   WRITE     DPCR3100
306000170620     C                   ELSE
306100170620     C*
306200170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
306300170620     C                   Z-ADD     1             j                 4 0
306400170620     C                   DOW       j <= %elem(SkSplitCSV_3)
306500170620     C                             AND j <= OutIdxMax
306600170620     C*
306700170620     C* Determino il tipo di Routing Place
306800170620     C                   SELECT
306900170620     C* Injection Code - Range
307000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
307100170620     C                   EVAL      CR3ROUT  = 'I'
307200170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):3:9)
307300170620     C                   EVAL      CR3ROUPE = %subst(SkSplitCSV_3(j):12:9)
307400170620     C* Injection Code - Single
307500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
307600170620     C                   EVAL      CR3ROUT  = 'I'
307700170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:9)
307800170620     C                   EVAL      CR3ROUPE = CR3ROUPB
307900170620     C* Country Code
308000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
308100170620     C                   EVAL      CR3ROUT  = 'C'
308200170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:2)
308300170620     C                   EVAL      CR3ROUPE = CR3ROUPB
308400170620     C* Depot Code - Range
308500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
308600170620     C                   EVAL      CR3ROUT  = 'D'
308700170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):3:7)
308800170620     C                   EVAL      CR3ROUPE = %subst(SkSplitCSV_3(j):10:7)
308900170620     C* Depot Code - Single
309000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
309100170620     C                   EVAL      CR3ROUT  = 'D'
309200170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:7)
309300170620     C                   EVAL      CR3ROUPE = CR3ROUPB
309400170620     C* Depot Group
309500170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
309600170620     C                   EVAL      CR3ROUT  = 'G'
309700170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:4)
309800170620     C                   EVAL      CR3ROUPE = CR3ROUPB
309900170620     C* Business Unit
310000170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
310100170620     C                   EVAL      CR3ROUT  = 'B'
310200170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:3)
310300170620     C                   EVAL      CR3ROUPE = CR3ROUPB
310400170620     C*
310500170620     C                   ENDSL
310600170620     C*
310700170620     C* Infine inserisco il record corrente
310800170620     C                   WRITE     DPCR3100
310900170620     C*
311000170620     C                   EVAL      j = j + 1
311100170620     C                   ENDDO
311200170620     C                   ENDIF
311300170619     C*
311400170619     C                   ENDSR
311500170619     C***
311600170619
311700170619
311800170619     C*----------------------------------------------------*
311900170619     C*  GESTIONE MAPPATURA "R4CSORT"
312000170619     C*----------------------------------------------------*
312100170619     C     GESCR4        BEGSR
312200170619     C*
312300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
312400170619     C                   CLEAR                   DPCR4100
312500170619     C*
312600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
312700170619     C                   Z-ADD     2             i                 4 0
312800170620     C                   DOW       i <= %elem(SkSplitFLD)
312900170620     C                             AND i <= OutIdxMax
313000170619     C*
313100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
313200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
313300170619     C                             SkSplitCSV_9(i) <> *blanks
313400170619     C*
313500170621     C*** ROUT / ROUPB / ROUPE
313600170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
313700170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
313800170619     C                   ENDIF
313900170619     C*** ISO2
314000170619     C                   IF        %trim(SkSplitFLD(i)) =
314100170619     C                                        'DESTINATIONCOUNTRYCODE'
314200170619     C                   EVAL      CR4ISO2  = SkSplitCSV_9(i)
314300170619     C                   ENDIF
314400170619     C*** DDPT
314500170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
314600170619     C                   EVAL      CR4DDPT  = SkSplitCSV_9(i)
314700170619     C                   ENDIF
314800170619     C*** NTWP
314900170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
315000170619     C                   EVAL      CR4NTWP  = SkSplitCSV_9(i)
315100170619     C                   ENDIF
315200170619     C*** PTY
315300170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
315400170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
315500170619     C                   EXSR      CHKNUM
315600170619     C                   IF        PiInt=*on
315700170619     C                   Z-ADD     PiVal         CR4PTY
315800170619     C                   ENDIF
315900170619     C                   ENDIF
316000170619     C*** CSORT
316100170619     C                   IF        %trim(SkSplitFLD(i)) = 'CSORT'
316200170620     C                   EVAL      CR4CSORT = SkSplitCSV_9(i)
316300170619     C                   ENDIF
316400170619     C*
316500170619     C                   ENDIF
316600170619     C                   EVAL      i = i + 1
316700170619     C                   ENDDO
316800170619     C*
316900170619     C*** VER
317000170619     C                   EVAL      CR4VER = NEW_VERSION
317100170620     C*
317200170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
317300170620     C                   EVAL      InStringa = w2048_01
317400170620     C                   EVAL      InSepara = ','
317500170620     C                   SETOFF                                       131419
317600170620     C                   SETON                                        13
317700170620     C                   EXSR      SR_SPLIT
317800170620     C*
317900170620     C* Se nessun elemento => scrivo comunque un record singolo
318000170620     C                   IF        OutIdxMax = *zeros
318100170620     C                   WRITE     DPCR4100
318200170620     C                   ELSE
318300170620     C*
318400170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
318500170620     C                   Z-ADD     1             j                 4 0
318600170620     C                   DOW       j <= %elem(SkSplitCSV_3)
318700170620     C                             AND j <= OutIdxMax
318800170620     C*
318900170620     C* Determino il tipo di Routing Place
319000170620     C                   SELECT
319100170620     C* Injection Code - Range
319200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
319300170620     C                   EVAL      CR4ROUT  = 'I'
319400170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):3:9)
319500170620     C                   EVAL      CR4ROUPE = %subst(SkSplitCSV_3(j):12:9)
319600170620     C* Injection Code - Single
319700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
319800170620     C                   EVAL      CR4ROUT  = 'I'
319900170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:9)
320000170620     C                   EVAL      CR4ROUPE = CR4ROUPB
320100170620     C* Country Code
320200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
320300170620     C                   EVAL      CR4ROUT  = 'C'
320400170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:2)
320500170620     C                   EVAL      CR4ROUPE = CR4ROUPB
320600170620     C* Depot Code - Range
320700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
320800170620     C                   EVAL      CR4ROUT  = 'D'
320900170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):3:7)
321000170620     C                   EVAL      CR4ROUPE = %subst(SkSplitCSV_3(j):10:7)
321100170620     C* Depot Code - Single
321200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
321300170620     C                   EVAL      CR4ROUT  = 'D'
321400170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:7)
321500170620     C                   EVAL      CR4ROUPE = CR4ROUPB
321600170620     C* Depot Group
321700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
321800170620     C                   EVAL      CR4ROUT  = 'G'
321900170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:4)
322000170620     C                   EVAL      CR4ROUPE = CR4ROUPB
322100170620     C* Business Unit
322200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
322300170620     C                   EVAL      CR4ROUT  = 'B'
322400170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:3)
322500170620     C                   EVAL      CR4ROUPE = CR4ROUPB
322600170620     C*
322700170620     C                   ENDSL
322800170620     C*
322900170620     C* Infine inserisco il record corrente
323000170620     C                   WRITE     DPCR4100
323100170620     C*
323200170620     C                   EVAL      j = j + 1
323300170620     C                   ENDDO
323400170620     C                   ENDIF
323500170619     C*
323600170619     C                   ENDSR
323700170619     C***
323800170619
323900170619
324000170619     C*----------------------------------------------------*
324100170619     C*  GESTIONE MAPPATURA "R5DSORT"
324200170619     C*----------------------------------------------------*
324300170619     C     GESCR5        BEGSR
324400170619     C*
324500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
324600170619     C                   CLEAR                   DPCR5100
324700170619     C*
324800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
324900170619     C                   Z-ADD     2             i                 4 0
325000170620     C                   DOW       i <= %elem(SkSplitFLD)
325100170620     C                             AND i <= OutIdxMax
325200170619     C*
325300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
325400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
325500170619     C                             SkSplitCSV_9(i) <> *blanks
325600170619     C*
325700170621     C*** ROUT / ROUPB / ROUPE
325800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
325900170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
326000170619     C                   ENDIF
326100170619     C*** DDPT
326200170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
326300170619     C                   EVAL      CR5DDPT  = SkSplitCSV_9(i)
326400170619     C                   ENDIF
326500170619     C*** ISO2
326600170619     C                   IF        %trim(SkSplitFLD(i)) =
326700170619     C                                        'DESTINATIONCOUNTRYCODE'
326800170619     C                   EVAL      CR5ISO2  = SkSplitCSV_9(i)
326900170619     C                   ENDIF
327000170619     C*** PTCB
327100170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
327200170619     C                   EVAL      CR5PTCB  = SkSplitCSV_9(i)
327300170619     C                   ENDIF
327400170619     C*** PTCE
327500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
327600170619     C                   EVAL      CR5PTCE  = SkSplitCSV_9(i)
327700170619     C                   ENDIF
327800170619     C*** NTWP
327900170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
328000170619     C                   EVAL      CR5NTWP  = SkSplitCSV_9(i)
328100170619     C                   ENDIF
328200170619     C*** PTY
328300170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
328400170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
328500170619     C                   EXSR      CHKNUM
328600170619     C                   IF        PiInt=*on
328700170619     C                   Z-ADD     PiVal         CR5PTY
328800170619     C                   ENDIF
328900170619     C                   ENDIF
329000170619     C*** DSORT
329100170619     C                   IF        %trim(SkSplitFLD(i)) = 'DSORT'
329200170629     C                   EVAL      CR5DSORT = %trim(SkSplitCSV_9(i))
329300170619     C                   ENDIF
329400170619     C*
329500170619     C                   ENDIF
329600170619     C                   EVAL      i = i + 1
329700170619     C                   ENDDO
329800170619     C*
329900170619     C*** VER
330000170621     C                   EVAL      CR5VER = NEW_VERSION
330100170620     C*
330200170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
330300170620     C                   EVAL      InStringa = w2048_01
330400170620     C                   EVAL      InSepara = ','
330500170620     C                   SETOFF                                       131419
330600170620     C                   SETON                                        13
330700170620     C                   EXSR      SR_SPLIT
330800170620     C*
330900170620     C* Se nessun elemento => scrivo comunque un record singolo
331000170620     C                   IF        OutIdxMax = *zeros
331100170620     C                   WRITE     DPCR5100
331200170620     C                   ELSE
331300170620     C*
331400170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
331500170620     C                   Z-ADD     1             j                 4 0
331600170620     C                   DOW       j <= %elem(SkSplitCSV_3)
331700170620     C                             AND j <= OutIdxMax
331800170620     C*
331900170620     C* Determino il tipo di Routing Place
332000170620     C                   SELECT
332100170620     C* Injection Code - Range
332200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
332300170620     C                   EVAL      CR5ROUT  = 'I'
332400170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):3:9)
332500170620     C                   EVAL      CR5ROUPE = %subst(SkSplitCSV_3(j):12:9)
332600170620     C* Injection Code - Single
332700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
332800170620     C                   EVAL      CR5ROUT  = 'I'
332900170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:9)
333000170620     C                   EVAL      CR5ROUPE = CR5ROUPB
333100170620     C* Country Code
333200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
333300170620     C                   EVAL      CR5ROUT  = 'C'
333400170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:2)
333500170620     C                   EVAL      CR5ROUPE = CR5ROUPB
333600170620     C* Depot Code - Range
333700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
333800170620     C                   EVAL      CR5ROUT  = 'D'
333900170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):3:7)
334000170620     C                   EVAL      CR5ROUPE = %subst(SkSplitCSV_3(j):10:7)
334100170620     C* Depot Code - Single
334200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
334300170620     C                   EVAL      CR5ROUT  = 'D'
334400170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:7)
334500170620     C                   EVAL      CR5ROUPE = CR5ROUPB
334600170620     C* Depot Group
334700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
334800170620     C                   EVAL      CR5ROUT  = 'G'
334900170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:4)
335000170620     C                   EVAL      CR5ROUPE = CR5ROUPB
335100170620     C* Business Unit
335200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
335300170620     C                   EVAL      CR5ROUT  = 'B'
335400170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:3)
335500170620     C                   EVAL      CR5ROUPE = CR5ROUPB
335600170620     C*
335700170620     C                   ENDSL
335800170620     C*
335900170620     C* Infine inserisco il record corrente
336000170620     C                   WRITE     DPCR5100
336100170620     C*
336200170620     C                   EVAL      j = j + 1
336300170620     C                   ENDDO
336400170620     C                   ENDIF
336500170619     C*
336600170619     C                   ENDSR
336700170620     C***
336800170616
336900170616
337000141028     C*----------------------------------------------------*
337100141028     C*  CONTROLLO NUMERICITA' CAMPI
337200141028     C*----------------------------------------------------*
337300141028     C     CHKNUM        BEGSR
337400141028     C*
337500141028     C                   IF        PiDecChr = *blanks
337600141028     C                   EVAL      PiDecChr = CharNUM
337700141028     C                   ENDIF
337800141028     C*
337900141028     C                   callp     UBISNUM_Check(PiStr
338000141028     C                                          :PiDecChr
338100141028     C                                          :PiVal
338200141028     C                                          :PiNum
338300141028     C                                          :PiInt)
338400141028     C*
338500141028     C                   ENDSR
338600141028     C***
338700141028
338800141028
338900141028     C*----------------------------------------------------*
339000141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
339100141028     C*----------------------------------------------------*
339200141028     C     SR_SPLIT      BEGSR
339300141028     C*
339400141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
339500170620     C   13              EVAL      InTypeOut = '3'
339600170620     C   14              EVAL      InTypeOut = '4'
339700170620     C   19              EVAL      InTypeOut = '9'
339800170616     C                   CALL      'XSPLIT3'
339900141028     C                   PARM                    InStringa
340000141028     C                   PARM                    InSepara
340100170616     C                   PARM                    InTypeOut
340200141028     C                   PARM      ''            wSkParam
340300170620     C                   PARM                    OutIdxMax
340400141028     C                   PARM                    OutErrore
340500141028     C                   PARM                    OutMsg
340600170620     C   13              MOVEA     wSkParam      SkSplitCSV_3
340700170620     C   14              MOVEA     wSkParam      SkSplitCSV_4
340800170620     C   19              MOVEA     wSkParam      SkSplitCSV_9
340900141028     C*
341000141028     C                   ENDSR
341100160801
341200160801
341300160801     C*--------------------------------------------------------
341400160801     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
341500160801     C*--------------------------------------------------------
341600160801     C     CARTAB        BEGSR
341700160801     C*
341800160801     C* TABELLA '15' - NAZIONI
341900160801     C                   clear                   skNAZISO
342000160801     C                   clear                   skNAZBAR
342100160801     C                   eval      tblKUT = 1
342200160801     C                   eval      tblCOD = '15'
342300170616     C     KEYtab_P      setll     tabel00f
342400170616     C     KEYtab_P      reade     tabel00f
342500160801     C                   dow       not %eof(tabel00f)
342600160801     C                   if        tblFLG = *blanks
342700160801     C                   movel(p)  tblUNI        ds15
342800160801     C                   add       1             jNAZ
342900160801     C                   eval      skNAZBAR(jNAZ) = tblKEY
343000170620     C                   eval      skNAZISO(jNAZ) = %trim(§15COD)
343100160801     C                   endif
343200170616     C     KEYtab_P      reade     tabel00f
343300160801     C                   enddo
343400160801     C*
343500160801     C                   ENDSR
343600160801     C***
343700170622
343800170622
343900170622     C*--------------------------------------------------------------------------------------------*
344000170622     C* SNDALERT - Invio email di alert al assistenza IT
344100170622     C*--------------------------------------------------------------------------------------------*
344200170622     C     sndAlert      BEGSR
344300170622     C*
344400170622     C* Inizializzo variabili
344500170622     C                   movel     *blanks       wrkEml          253
344600170622     C                   movel     *blanks       wrkCcEml        253
344700170622     C                   movel     *blanks       wrkEmlMsg      5000
344800170622     C                   movel     *blanks       wrkEmlOgg        44
344900170622     C*
345000170622     C* Valorizzo i campi della e-m@ail
345100170622     C                   eval      wrkEml='cedalert@brt.it'
345200171025     C                   eval      wrkCcEml='andrea.bertocchi@brt.it'
345300170622     C                   eval      wrkEmlOgg=
345400170622     C                               'DPD GeoRDB 2017 - Importazione '         +
345500170622     C                               'VERSIONE: ' + %editc(NEW_VERSION:'X')
345600170622     C                   EVAL      wrkEmlMsg = 'Rilevata possibilità '         +
345700170622     C                             'troncamento dati al record: '              +
345800170622     C                                         %editc(currRCD:'4')             +
345900170622     C                                                                    ':/N'+
346000170622     C                                                                    ':/N'
346100170622     C*
346200170622     C                   call(e)   'TIS701C1'
346300170622     C                   parm                    wrkEml
346400170622     C                   parm                    wrkCcEml
346500170622     C                   parm                    wrkEmlOgg
346600170622     C                   parm                    wrkEmlMsg
346700170622     C                   parm                    wrkEsito          1
346800170622     C*
346900170622     C                   ENDSR
347000070411
347100990910
347200000613     C     *inzsr        BEGSR
347300000613     C*
347400170616     C* CALCOLA LA VARIE DATE DI RIFERIMENTO
347500170616     C                   z-add     *zeros        oggi              8 0
347600170616     C                   z-add     *zeros        domani            8 0
347700170616     C                   eval      oggi   = %dec(%date() : *ISO)
347800170616     C                   eval      domani = %dec(%date()+%days(1) : *ISO)
347900160801     C*
348000170616     C* Chiave su TABEL00F - Parziale
348100170616     C     KEYtab_P      KLIST
348200160801     C                   KFLD                    tblKUT
348300160801     C                   KFLD                    tblCOD
348400170616     C*
348500170616     C* Chiave su DPCCC00F - Completa
348600170616     C     KEYccc_11C    KLIST
348700170616     C                   KFLD                    cccATB
348800170616     C                   KFLD                    cccVER
348900170616     C                   KFLD                    cccISO2
349000170616     C*
349100170616     C* Chiave su DPCST00F - Completa
349200170616     C     KEYcst_11C    KLIST
349300170616     C                   KFLD                    cstATB
349400170616     C                   KFLD                    cstVER
349500170616     C                   KFLD                    cstISO2
349600170616     C                   KFLD                    cstREGC
349700170616     C                   KFLD                    cstSTAC
349800170616     C*
349900170616     C* Chiave su DPCEL00F - Completa
350000170616     C     KEYcel_11C    KLIST
350100170616     C                   KFLD                    celATB
350200170616     C                   KFLD                    celVER
350300170616     C                   KFLD                    celELMC
350400170619     C*
350500170619     C* Chiave su DPCSO00F - Completa
350600170619     C     KEYcso_11C    KLIST
350700170619     C                   KFLD                    csoATB
350800170619     C                   KFLD                    csoVER
350900170619     C                   KFLD                    csoSORC
351000000830     C*
351100000613     C                   ENDSR
351200000613     C***
