000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400160801     Ftabel00f  if   e           k disk
000500170621     FWFDPDRDBF uF   E             DISK    usropn
000600170620     FDPCAS11I  uF a E           k DISK
000700170620     FDPCBU11I  uF a E           k DISK
000800170620     FDPCCC11I  uF a E           k DISK
000900170620     FDPCDP11I  uF a E           k DISK
001000170620     FDPCEL11I  uF a E           k DISK
001100170620     FDPCIC11I  uF a E           k DISK
001200170620     FDPCNP11I  uF a E           k DISK
001300170620     FDPCNT11I  uF a E           k DISK
001400170620     FDPCPC11I  uF a E           k DISK
001500170620     FDPCRE11I  uF a E           k DISK
001600170620     FDPCSO11I  uF a E           k DISK
001700170620     FDPCST11I  uF a E           k DISK
001800170620     FDPCVE11I  uF a E           k DISK
001900170620     FDPCWA11I  uF a E           k DISK
002000170620     FDPCWS11I  uF a E           k DISK
002100170620     FDPCR111I  uF a E           k DISK
002200170620     FDPCR211I  uF a E           k DISK
002300170620     FDPCR311I  uF a E           k DISK
002400170620     FDPCR411I  uF a E           k DISK
002500170620     FDPCR511I  uF a E           k DISK
002600170616
002700990908
002800000801     D*----------------------------------------------------
002900000801     D* DICHIARAZIOINE VARIABILI DI WRK
003000000801     D*----------------------------------------------------
003100160801     D ds15          e ds
003200170621     D wWDPDRDB        s                   LIKE(WDPDRDB) INZ
003300170616     D MAX_VERSION     s                   LIKE(CVEVER) INZ
003400170616     D NEW_VERSION     s                   LIKE(CVEVER) INZ
003500170616     D SkSplitFLD      S            128    DIM(512) INZ
003600170616     D currMAP         S            128    VARYING INZ
003700170622     D currRCD         S              9S 0 INZ
003800170621     D wDESxx          S            128    VARYING INZ
003900170621     D wCSTSRTC        S                   LIKE(CSTSRTC)  INZ
004000170621     D w2048_01        S           2048    VARYING INZ
004100170621     D w2048_02        S           2048    VARYING INZ
004200170621     D w2048_03        S           2048    VARYING INZ
004300170621     D w2048_04        S           2048    VARYING INZ
004400170621     D sOutIdxMax_w1   S                   LIKE(OutIdxMax) INZ
004500170621     D sOutIdxMax_w2   S                   LIKE(OutIdxMax) INZ
004600170621     D sOutIdxMax_w3   S                   LIKE(OutIdxMax) INZ
004700170621     D sOutIdxMax_w4   S                   LIKE(OutIdxMax) INZ
004800170621     D wSORC           S              3    INZ
004900170621     D wSkCSV_03_w1    S             64    DIM(1024) INZ
005000170621     D wSkCSV_03_w2    S             64    DIM(1024) INZ
005100170621     D wSkCSV_03_w3    S             64    DIM(1024) INZ
005200170621     D wSkCSV_03_w4    S             64    DIM(1024) INZ
005300170616
005400170616     D*------------
005500170616     D skDEFKEY        S             64    DIM(128)
005600170620     D DS_DEF          DS
005700170620     D   skDEF                       64    DIM(128)
005800170620     D skDEFS          S                   DIM(128) LIKE(DS_DEF)
005900170616     D jDEFS           S              3  0 INZ
006000160606
006100160801     D*------------
006200170620     D jNAZ            s              4  0 INZ(*zeros)
006300170620     D skNAZISO        S              2    DIM(1000)
006400160801     D skNAZBAR        S              3    DIM(1000)
006500160606
006600141028
006700141028     D*------------------
006800141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006900141028     D*------------------
007000170621     D CharCSV         S              1    INZ(';')
007100170621     D CharNUM         S              1    INZ(',')
007200141028
007300141028
007400141028     D*------------------
007500141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
007600141028     D*------------------
007700170616     D InStringa       S         131072A   VARYING                              (stringa input)
007800170616     D InSepara        S             10A                                        (separatore)
007900170616     D InTypeOut       S              2A                                        (tipo output)
008000170616     D wSkParam        S         131072A                                        (output)
008100170620     D OutIdxMax       S              4S 0                                      (max indice valoriz)
008200170616     D OutErrore       S              1A                                        (flag errore)
008300170616     D OutMsg          S             80A                                        (messaggio errore)
008400170620     D SkSplitCSV_3    S             64    DIM(1024)
008500170616     D SkSplitCSV_4    S            128    DIM(512)
008600170616     D SkSplitCSV_9    S           2048    DIM(64)
008700990908
008800141028
008900141028     D*------------------
009000141028     D* LINKING A DEFINIZIONI ESTERNE
009100141028     D*------------------
009200141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009300141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009400141028
009500141028
009600010201
009700000613     C*
009800160801     C                   EXSR      CARTAB                                       CARICA TABELLE
009900170616     C                   EXSR      RWFILE                                       ELABORAZIONE
010000000616     C*
010100010201     C                   seton                                        LR
010200990908
010300000801
010400040526     C     RWFILE        BEGSR
010500170620     C*
010600170621     C                   if        not %open(WFDPDRDBF)
010700170621     C                   open      WFDPDRDBF
010800170620     C                   endif
010900150127     C*
011000170621     C                   READ      WFDPDRDBF
011100170621     C                   DOW       not %eof(WFDPDRDBF)
011200000801     C*
011300170621     C                   if        WDPDRDB <> *blanks
011400141028     C*
011500170616     C* "normalizzo" il dato di input corrente
011600170621     C                   eval      wWDPDRDB = %trim(%subst(WDPDRDB:1:
011700170621     C                                        %len(%trim(WDPDRDB))-1))
011800170622     C*
011900170622     C* Verifico rischio dati troncati
012000170622     C                   add       1             currRCD
012100170622     C                   if        %subst(WDPDRDB:1000) <> *blanks
012200170622     C                   exsr      sndAlert
012300170622     C                   endif
012400040910     C*
012500170616     C* Eseguo routine di traduzione
012600170616     C                   exsr      exeTrad
012700040802     C*
012800000905     C                   endif
012900000905     C*
013000170621     C***                DELETE    WFDPDRDB0
013100170621     C                   READ      WFDPDRDBF
013200170621     C                   ENDDO
013300170620     C*
013400170621     C                   if        %open(WFDPDRDBF)
013500170621     C                   close     WFDPDRDBF
013600170620     C                   endif
013700990920     C*
013800910830     C                   ENDSR
013900000613     C***
014000170616
014100140922
014200141028     C*----------------------------------------------------*
014300141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
014400141028     C*----------------------------------------------------*
014500141028     C     INZVAR        BEGSR
014600141028     C*
014700141028     C* Inizializzo variabili di wrk
014800170620     C                   SETOFF                                       131419
014900170620     C                   CLEAR                   SkSplitCSV_3
015000170616     C                   CLEAR                   SkSplitCSV_4
015100170616     C                   CLEAR                   SkSplitCSV_9
015200170621     C                   CLEAR                   wDESxx
015300170620     C                   CLEAR                   wCSTSRTC
015400170620     C                   CLEAR                   w2048_01
015500170621     C                   CLEAR                   w2048_02
015600170621     C                   CLEAR                   w2048_03
015700170621     C                   CLEAR                   w2048_04
015800141028     C*
015900141028     C                   ENDSR
016000170616     C***
016100170616
016200170616
016300000801     C*----------------------------------------------------*
016400170616     C*  TRADUZIONE DATI
016500000801     C*----------------------------------------------------*
016600170616     C     EXETRAD       BEGSR
016700170616     C*
016800170616     C* Inizializzazioni a liverro di singolo record
016900170616     C                   EXSR      INZVAR
017000170616     C*
017100170616     C* Ignoro i record relativi al "file"
017200170616     C                   SELECT
017300170621     C                   WHEN      %subst(wWDPDRDB:1:1)  = '#' AND
017400170621     C                             %subst(wWDPDRDB:1:4) <> '#DEF'
017500170616     C*
017600170616     C* Carico innanzitutto tutti i record di DEFinizione
017700170621     C                   WHEN      %subst(wWDPDRDB:1:16) = '#DEF:GEOROUTING:'
017800170621     C                   EVAL      InStringa = %subst(wWDPDRDB:16+1)
017900170616     C                   EVAL      InSepara = CharCSV
018000170620     C                   SETOFF                                       131419
018100170616     C                   SETON                                        14
018200170616     C                   EXSR      SR_SPLIT
018300170616     C                   EVAL      skDEF = %subarr(SkSplitCSV_4:1)
018400170616     C                   ADD       1             jDEFS
018500170616     C                   EVAL      skDEFKEY(jDEFS) = skDEF(1)
018600170620     C                   EVAL      skDEFS(jDEFS)   = DS_DEF
018700170620     C*
018800170616     C* Elaboro i record di dati
018900170616     C                   OTHER
019000170616     C*
019100170616     C* Splitto il record di dati corrrente
019200170621     C                   EVAL      InStringa = %trim(wWDPDRDB)
019300170616     C                   EVAL      InSepara = CharCSV
019400170620     C                   SETOFF                                       131419
019500170616     C                   SETON                                        19
019600170616     C                   EXSR      SR_SPLIT
019700170616     C*
019800170616     C* Ogni record di dati lo "mappo" tramite la propria struttura dati
019900170616     C                   EXSR      MAPPADATI
020000170616     C*
020100170616     C                   ENDSL
020200170616     C*
020300170616     C                   ENDSR
020400170616     C***
020500170616
020600170616
020700170616     C*----------------------------------------------------*
020800170616     C*  GESTIONE MAPPATURE DATI
020900170616     C*----------------------------------------------------*
021000170616     C     MAPPADATI     BEGSR
021100170616     C*
021200170616     C* Verifico che il tipo record dati corrente risulti già mappato
021300170616     C                   EVAL      currMAP = %trim(SkSplitCSV_9(1))
021400170616     C                   EVAL      jDEFS = %lookup(currMAP:skDEFKEY:1)
021500170616     C*
021600170616     C* Se tipo record dati corrente già mappato => carico relativa mappatura
021700170616     C                   IF        jDEFS > *zeros
021800170620     C                   EVAL      DS_DEF = skDEFS(jDEFS)
021900170616     C                   EVAL      SkSplitFLD = %subarr(skDEF:1)
022000170616     C*
022100170616     C* Rispetto ad ogni tipo record dati => gestisco con relativa mappatura
022200170616     C                   SELECT
022300170616     C                   WHEN      currMAP ='VERSION'
022400170616     C                   EXSR      gesCVE
022500170616     C*
022600170616     C                   WHEN      currMAP ='COUNTRY'
022700170616     C                   EXSR      gesCCC
022800170616     C*
022900170620     C***                WHEN      currMAP ='COUNTRYTRANSLATION'
023000170619     C***                EXSR      gesXXX
023100170616     C*
023200170616     C                   WHEN      currMAP ='COUNTRY.IT'
023300170616     C                   EXSR      gesCCC_IT
023400170616     C*
023500170616     C                   WHEN      currMAP ='COUNTRY.DE'
023600170616     C                   EXSR      gesCCC_DE
023700170616     C*
023800170616     C                   WHEN      currMAP ='COUNTRY.FR'
023900170616     C                   EXSR      gesCCC_FR
024000170616     C*
024100170616     C                   WHEN      currMAP ='COUNTRY.EN'
024200170616     C                   EXSR      gesCCC_EN
024300170616     C*
024400170616     C                   WHEN      currMAP ='BUSINESSUNIT'
024500170616     C                   EXSR      gesCBU
024600170616     C*
024700170616     C                   WHEN      currMAP ='POSTALCODE'
024800170616     C                   EXSR      gesCPC
024900170616     C*
025000170620     C***                WHEN      currMAP ='STATE'
025100170619     C***                EXSR      gesCST
025200170616     C*
025300170620     C***                WHEN      currMAP ='STATE.IT'
025400170619     C***                EXSR      gesCST_IT
025500170616     C*
025600170620     C***                WHEN      currMAP ='STATE.DE'
025700170619     C***                EXSR      gesCST_DE
025800170616     C*
025900170620     C***                WHEN      currMAP ='STATE.FR'
026000170619     C***                EXSR      gesCST_FR
026100170616     C*
026200170620     C***                WHEN      currMAP ='STATE.EN'
026300170619     C***                EXSR      gesCST_EN
026400170616     C*
026500170616     C                   WHEN      currMAP ='DEPOTS'
026600170619     C                   EXSR      gesCDP
026700170616     C*
026800170620     C***                WHEN      currMAP ='ELEMENTS'
026900170619     C***                EXSR      gesCEL
027000170616     C*
027100170620     C***                WHEN      currMAP ='ELEMENTS.IT'
027200170619     C***                EXSR      gesCEL_IT
027300170616     C*
027400170620     C***                WHEN      currMAP ='ELEMENTS.EN'
027500170619     C***                EXSR      gesCEL_EN
027600170616     C*
027700170620     C***                WHEN      currMAP ='ELEMENTS.DE'
027800170619     C***                EXSR      gesCEL_DE
027900170616     C*
028000170620     C***                WHEN      currMAP ='ELEMENTS.FR'
028100170619     C***                EXSR      gesCEL_FR
028200170616     C*
028300170620     C***                WHEN      currMAP ='ELEMENTS.PT'
028400170619     C***                EXSR      gesCEL_PT
028500170619     C*
028600170616     C                   WHEN      currMAP ='SOCODE'
028700170616     C                   EXSR      gesCSO
028800170616     C*
028900170616     C                   WHEN      currMAP ='ASCODE'
029000170619     C                   EXSR      gesCAS
029100170616     C*
029200170616     C                   WHEN      currMAP ='IC'
029300170619     C                   EXSR      gesCIC
029400170616     C*
029500170616     C                   WHEN      currMAP ='NETWORK'
029600170619     C                   EXSR      gesCNT
029700170616     C*
029800170616     C                   WHEN      currMAP ='NETWORKPLAN'
029900170616     C                   EXSR      gesCNP
030000170616     C*
030100170620     C***                WHEN      currMAP ='RCODE'
030200170620     C***                EXSR      gesXXX
030300170616     C*
030400170616     C                   WHEN      currMAP ='ALLOWSO'
030500170619     C                   EXSR      gesCWS
030600170616     C*
030700170616     C                   WHEN      currMAP ='ALLOWAS'
030800170619     C                   EXSR      gesCWA
030900170619     C*
031000170619     C                   WHEN      currMAP ='R1DBU'
031100170619     C                   EXSR      gesCR1
031200170619     C*
031300170619     C                   WHEN      currMAP ='R2ROUTE'
031400170619     C                   EXSR      gesCR2
031500170619     C*
031600170619     C                   WHEN      currMAP ='R3OSORT'
031700170619     C                   EXSR      gesCR3
031800170619     C*
031900170619     C                   WHEN      currMAP ='R4CSORT'
032000170620     C                   EXSR      gesCR4
032100170619     C*
032200170619     C                   WHEN      currMAP ='R5DSORT'
032300170619     C                   EXSR      gesCR5
032400170616     C*
032500170620     C***                WHEN      currMAP ='DEPARTURE'
032600170619     C***                EXSR      gesXXX
032700170616     C*
032800170616     C                   WHEN      currMAP ='RELATION'
032900170619     C                   EXSR      gesCRE
033000170616     C*
033100170616     C                   WHEN      currMAP ='SOSERVICEINFO.IT'
033200170616     C                   EXSR      gesCSO_IT
033300170616     C*
033400170616     C                   WHEN      currMAP ='SOSERVICEINFO.EN'
033500170616     C                   EXSR      gesCSO_EN
033600170616     C*
033700170616     C                   WHEN      currMAP ='SOSERVICEINFO.DE'
033800170616     C                   EXSR      gesCSO_DE
033900170616     C*
034000170616     C                   WHEN      currMAP ='SOSERVICEINFO.FR'
034100170616     C                   EXSR      gesCSO_FR
034200170616     C*
034300170616     C                   WHEN      currMAP ='SOSERVICEINFO.ET'
034400170616     C                   EXSR      gesCSO_ET
034500170616     C*
034600170616     C                   WHEN      currMAP ='SOSERVICEINFO.CS'
034700170616     C                   EXSR      gesCSO_CS
034800170616     C*
034900170616     C                   ENDSL
035000170616     C*
035100170616     C                   ENDIF
035200170616     C*
035300170616     C                   ENDSR
035400170616     C***
035500170616
035600170616
035700170616     C*----------------------------------------------------*
035800170616     C*  GESTIONE MAPPATURA "VERSION"
035900170616     C*----------------------------------------------------*
036000170616     C     GESCVE        BEGSR
036100170616     C*
036200170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
036300170616     C                   CLEAR                   DPCVE100
036400170616     C*
036500170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
036600170616     C                   Z-ADD     2             i                 4 0
036700170620     C                   DOW       i <= %elem(SkSplitFLD)
036800170620     C                             AND i <= OutIdxMax
036900170616     C*
037000170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
037100170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
037200170616     C                             SkSplitCSV_9(i) <> *blanks
037300170616     C*
037400170616     C*** VERD
037500170616     C                   IF        %trim(SkSplitFLD(i)) = 'VERSIONNUMBER'
037600170616     C                   EVAL      CVEVERD = SkSplitCSV_9(i)
037700170616     C                   ENDIF
037800170616     C*** DDE
037900170616     C                   IF        %trim(SkSplitFLD(i)) = 'VALIDFROM'
038000170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
038100170616     C                   EXSR      CHKNUM
038200170616     C                   IF        PiInt=*on
038300170616     C                   Z-ADD     PiVal         CVEDDE
038400170616     C                   ELSE
038500170616     C                   dump(A)
038600170616     C                   return
038700170616     C                   ENDIF
038800170616     C                   ENDIF
038900170616     C*** DSC
039000170616     C                   IF        %trim(SkSplitFLD(i)) = 'VALIDTO'
039100170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
039200170616     C                   EXSR      CHKNUM
039300170616     C                   IF        PiInt=*on
039400170616     C                   Z-ADD     PiVal         CVEDSC
039500170616     C                   ELSE
039600170616     C                   dump(A)
039700170616     C                   return
039800170616     C                   ENDIF
039900170616     C                   ENDIF
040000170616     C*** BCID
040100170616     C                   IF        %trim(SkSplitFLD(i)) = 'BARCODEID'
040200170627     C                   EVAL      CVEBCID = SkSplitCSV_9(i)
040300170616     C                   ENDIF
040400170616     C*
040500170616     C                   ENDIF
040600170616     C                   EVAL      i = i + 1
040700170616     C                   ENDDO
040800170616     C*
040900170616     C*** DIM
041000170616     C                   EVAL      CVEDIM = oggi
041100170616     C*
041200170616     C* Reperisco la versione più alta corrente
041300170616     C*
041400170616     C/EXEC SQL
041500170616     C+ SET :MAX_VERSION = (SELECT MAX(CVEVER) FROM DPCVE10F)
041600170616     C/END-EXEC
041700170616     C*
041800170616     C* Stacco il nuovo numero versione BRT come incremento della versione più alta presente
041900170616     C                   EVAL      NEW_VERSION = MAX_VERSION + 1
042000170616     C*
042100170616     C* Effetttuo considerazioni si "sicurezza" sulle date di validità della versione DPD
042200170616     C                   IF        CVEDDE < domani
042300170616     C                   EVAL      CVEDDE = domani
042400170616     C                   ENDIF
042500170616     C                   IF        CVEDSC < domani
042600170616     C                   EVAL      CVEDSC = domani
042700170616     C                   ENDIF
042800170616     C*
042900170616     C* Annullo tutte le versioni che decorrono
043000170616     C* nel range di validità della versione DPD corrente
043100170616     C*
043200170616     C/EXEC SQL
043300170616     C+ UPDATE DPCVE10F SET CVEATB = 'A'
043400170616     C+ WHERE CVEDDE BETWEEN :CVEDDE AND :CVEDSC
043500170616     C/END-EXEC
043600170616     C*
043700170616     C* Faccio scadere a oggi tutte le versioni aventi data scadenza
043800170616     C* nel range di validità della versione DPD corrente
043900170616     C*
044000170616     C/EXEC SQL
044100170616     C+ UPDATE DPCVE10F SET CVEDSC = :oggi
044200170616     C+ WHERE CVEDSC BETWEEN :CVEDDE AND :CVEDSC
044300170616     C/END-EXEC
044400170616     C*
044500170620     C* Quindi lancio driver di "allineamento validità versioni DPD GeoRDB 2017"
044600170616     C*
044700170620     C                   CALL      'TISID3R1'
044800170620     C*
044900170620     C*** VER
045000170620     C                   EVAL      CVEVER = NEW_VERSION
045100170616     C*
045200170616     C* Infine inserisco il record corrente
045300170616     C                   WRITE     DPCVE100
045400170616     C*
045500170616     C                   ENDSR
045600170616     C***
045700170616
045800170616
045900170616     C*----------------------------------------------------*
046000170616     C*  GESTIONE MAPPATURA "COUNTRY"
046100170616     C*----------------------------------------------------*
046200170616     C     GESCCC        BEGSR
046300170616     C*
046400170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
046500170616     C                   CLEAR                   DPCCC100
046600170616     C*
046700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
046800170616     C                   Z-ADD     2             i                 4 0
046900170620     C                   DOW       i <= %elem(SkSplitFLD)
047000170620     C                             AND i <= OutIdxMax
047100170616     C*
047200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
047300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
047400170616     C                             SkSplitCSV_9(i) <> *blanks
047500170616     C*
047600170616     C*** ISO2
047700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
047800170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
047900170616     C                   ENDIF
048000170616     C*** ISON
048100170616     C                   IF        %trim(SkSplitFLD(i)) = 'ISONUMCOUNTRYCODE'
048200170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
048300170616     C                   EXSR      CHKNUM
048400170616     C                   IF        PiInt=*on
048500170616     C                   Z-ADD     PiVal         CCCISON
048600170616     C                   ENDIF
048700170616     C                   ENDIF
048800170616     C*** ISO3
048900170616     C                   IF        %trim(SkSplitFLD(i)) = 'ISOALPHA3COUNTRYCODE'
049000170616     C                   EVAL      CCCISO3 = SkSplitCSV_9(i)
049100170616     C                   ENDIF
049200170629     C*** PSYS
049300170616     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGPOSTCODENO'
049400170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
049500170616     C                   EXSR      CHKNUM
049600170616     C                   IF        PiInt=*on
049700170629     C                   Z-ADD     PiVal         CCCPSYS
049800170616     C                   ENDIF
049900170616     C                   ENDIF
050000170629     C*** PATTL
050100170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODEPATTERN'
050200170620     C                   EVAL      CCCPATTL = SkSplitCSV_9(i)
050300170616     C                   ENDIF
050400170616     C*** FNS
050500170616     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGNEEDSTATE'
050600170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
050700170616     C                   EXSR      CHKNUM
050800170616     C                   IF        PiInt=*on
050900170616     C                   Z-ADD     PiVal         CCCFNS
051000170616     C                   ENDIF
051100170616     C                   ENDIF
051200170616     C*** PVAL
051300170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODEVALIDATION'
051400170616     C                   EVAL      CCCPVAL = SkSplitCSV_9(i)
051500170616     C                   ENDIF
051600170616     C*** CVAL
051700170616     C                   IF        %trim(SkSplitFLD(i)) = 'CITYVALIDATION'
051800170616     C                   EVAL      CCCCVAL = SkSplitCSV_9(i)
051900170616     C                   ENDIF
052000170616     C*** FREE
052100170616     C                   IF        %trim(SkSplitFLD(i)) = 'FREETRADEAREA'
052200170616     C                   EVAL      CCCFREE = SkSplitCSV_9(i)
052300170616     C                   ENDIF
052400170616     C*** DUMY
052500170616     C                   IF        %trim(SkSplitFLD(i)) = 'DUMMYCOUNTRYCODE'
052600170616     C                   EVAL      CCCDUMY = SkSplitCSV_9(i)
052700170616     C                   ENDIF
052800170616     C*
052900170616     C                   ENDIF
053000170616     C                   EVAL      i = i + 1
053100170616     C                   ENDDO
053200170616     C*
053300170616     C*** VER
053400170616     C                   EVAL      CCCVER = NEW_VERSION
053500170616     C*** BRT
053600170620     C                   Z-ADD     1             jNAZ
053700170620     C     CCCISO2       LOOKUP    skNAZISO(jNAZ)                         55
053800170616     C                   IF        %found
053900170616     C                   EVAL      CCCBRT = skNAZBAR(jNAZ)
054000170622     C                   ELSE
054100170622     C                   EVAL      CCCBRT = *all'*'
054200170616     C                   ENDIF
054300170616     C*
054400170616     C* Infine inserisco il record corrente
054500170616     C                   WRITE     DPCCC100
054600170616     C*
054700170616     C                   ENDSR
054800170616     C***
054900170616
055000170616
055100170616     C*----------------------------------------------------*
055200170616     C*  GESTIONE MAPPATURA "COUNTRY.IT"
055300170616     C*----------------------------------------------------*
055400170616     C     GESCCC_IT     BEGSR
055500170616     C*
055600170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
055700170616     C                   Z-ADD     2             i                 4 0
055800170620     C                   DOW       i <= %elem(SkSplitFLD)
055900170620     C                             AND i <= OutIdxMax
056000170616     C*
056100170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
056200170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
056300170616     C                             SkSplitCSV_9(i) <> *blanks
056400170616     C*
056500170616     C*** ISO2
056600170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
056700170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
056800170616     C                   ENDIF
056900170616     C*** NAME
057000170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
057100170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
057200170616     C                   ENDIF
057300170616     C*
057400170616     C                   ENDIF
057500170616     C                   EVAL      i = i + 1
057600170616     C                   ENDDO
057700170616     C*
057800170616     C*** ATB
057900170616     C                   EVAL      CCCATB = *blanks
058000170616     C*** VER
058100170616     C                   EVAL      CCCVER = NEW_VERSION
058200170616     C*
058300170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
058400170621     C                   IF        wDESxx <> *blanks
058500170616     C     KEYccc_11C    CHAIN     dpccc11i
058600170616     C                   IF        %found(dpccc11i)
058700170621     C                   EVAL      CCCDESIT = wDESxx
058800170616     C                   UPDATE    DPCCC100
058900170616     C                   ENDIF
059000170620     C                   ENDIF
059100170616     C*
059200170616     C                   ENDSR
059300170616     C***
059400170616
059500170616
059600170616     C*----------------------------------------------------*
059700170616     C*  GESTIONE MAPPATURA "COUNTRY.DE"
059800170616     C*----------------------------------------------------*
059900170616     C     GESCCC_DE     BEGSR
060000170616     C*
060100170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
060200170616     C                   Z-ADD     2             i                 4 0
060300170620     C                   DOW       i <= %elem(SkSplitFLD)
060400170620     C                             AND i <= OutIdxMax
060500170616     C*
060600170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
060700170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
060800170616     C                             SkSplitCSV_9(i) <> *blanks
060900170616     C*
061000170616     C*** ISO2
061100170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
061200170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
061300170616     C                   ENDIF
061400170616     C*** NAME
061500170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
061600170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
061700170616     C                   ENDIF
061800170616     C*
061900170616     C                   ENDIF
062000170616     C                   EVAL      i = i + 1
062100170616     C                   ENDDO
062200170616     C*
062300170616     C*** ATB
062400170616     C                   EVAL      CCCATB = *blanks
062500170616     C*** VER
062600170616     C                   EVAL      CCCVER = NEW_VERSION
062700170616     C*
062800170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
062900170621     C                   IF        wDESxx <> *blanks
063000170616     C     KEYccc_11C    CHAIN     dpccc11i
063100170616     C                   IF        %found(dpccc11i)
063200170621     C                   EVAL      CCCDESDE = wDESxx
063300170616     C                   UPDATE    DPCCC100
063400170620     C                   ENDIF
063500170616     C                   ENDIF
063600170616     C*
063700170616     C                   ENDSR
063800170616     C***
063900170616
064000170616
064100170616     C*----------------------------------------------------*
064200170616     C*  GESTIONE MAPPATURA "COUNTRY.FR"
064300170616     C*----------------------------------------------------*
064400170616     C     GESCCC_FR     BEGSR
064500170616     C*
064600170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
064700170616     C                   Z-ADD     2             i                 4 0
064800170620     C                   DOW       i <= %elem(SkSplitFLD)
064900170620     C                             AND i <= OutIdxMax
065000170616     C*
065100170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
065200170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
065300170616     C                             SkSplitCSV_9(i) <> *blanks
065400170616     C*
065500170616     C*** ISO2
065600170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
065700170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
065800170616     C                   ENDIF
065900170616     C*** NAME
066000170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
066100170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
066200170616     C                   ENDIF
066300170616     C*
066400170616     C                   ENDIF
066500170616     C                   EVAL      i = i + 1
066600170616     C                   ENDDO
066700170616     C*
066800170616     C*** ATB
066900170616     C                   EVAL      CCCATB = *blanks
067000170616     C*** VER
067100170616     C                   EVAL      CCCVER = NEW_VERSION
067200170616     C*
067300170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
067400170621     C                   IF        wDESxx <> *blanks
067500170616     C     KEYccc_11C    CHAIN     dpccc11i
067600170616     C                   IF        %found(dpccc11i)
067700170621     C                   EVAL      CCCDESFR = wDESxx
067800170616     C                   UPDATE    DPCCC100
067900170616     C                   ENDIF
068000170620     C                   ENDIF
068100170616     C*
068200170616     C                   ENDSR
068300170616     C***
068400170616
068500170616
068600170616     C*----------------------------------------------------*
068700170616     C*  GESTIONE MAPPATURA "COUNTRY.EN"
068800170616     C*----------------------------------------------------*
068900170616     C     GESCCC_EN     BEGSR
069000170616     C*
069100170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
069200170616     C                   Z-ADD     2             i                 4 0
069300170620     C                   DOW       i <= %elem(SkSplitFLD)
069400170620     C                             AND i <= OutIdxMax
069500170616     C*
069600170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
069700170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
069800170616     C                             SkSplitCSV_9(i) <> *blanks
069900170616     C*
070000170616     C*** ISO2
070100170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
070200170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
070300170616     C                   ENDIF
070400170616     C*** NAME
070500170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
070600170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
070700170616     C                   ENDIF
070800170616     C*
070900170616     C                   ENDIF
071000170616     C                   EVAL      i = i + 1
071100170616     C                   ENDDO
071200170616     C*
071300170616     C*** ATB
071400170616     C                   EVAL      CCCATB = *blanks
071500170616     C*** VER
071600170616     C                   EVAL      CCCVER = NEW_VERSION
071700170616     C*
071800170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
071900170621     C                   IF        wDESxx <> *blanks
072000170616     C     KEYccc_11C    CHAIN     dpccc11i
072100170616     C                   IF        %found(dpccc11i)
072200170621     C                   EVAL      CCCDESEN = wDESxx
072300170616     C                   UPDATE    DPCCC100
072400170616     C                   ENDIF
072500170620     C                   ENDIF
072600170616     C*
072700170616     C                   ENDSR
072800170616     C***
072900170616
073000170616
073100170616     C*----------------------------------------------------*
073200170616     C*  GESTIONE MAPPATURA "BUSINESSUNIT"
073300170616     C*----------------------------------------------------*
073400170616     C     GESCBU        BEGSR
073500170616     C*
073600170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
073700170616     C                   CLEAR                   DPCBU100
073800170616     C*
073900170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
074000170616     C                   Z-ADD     2             i                 4 0
074100170620     C                   DOW       i <= %elem(SkSplitFLD)
074200170620     C                             AND i <= OutIdxMax
074300170616     C*
074400170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
074500170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
074600170616     C                             SkSplitCSV_9(i) <> *blanks
074700170616     C*
074800170616     C*** BUCA
074900170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUALPHASTR'
075000170616     C                   EVAL      CBUBUCA = SkSplitCSV_9(i)
075100170616     C                   ENDIF
075200170616     C*** BUCN
075300170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
075400170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
075500170616     C                   EXSR      CHKNUM
075600170616     C                   IF        PiInt=*on
075700170616     C                   Z-ADD     PiVal         CBUBUCN
075800170616     C                   ENDIF
075900170616     C                   ENDIF
076000170616     C*** NAME
076100170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUNAME'
076200170616     C                   EVAL      CBUNAME = SkSplitCSV_9(i)
076300170616     C                   ENDIF
076400170616     C*** ISO2
076500170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUDEFAULTCOUNTRY'
076600170616     C                   EVAL      CBUISO2 = SkSplitCSV_9(i)
076700170616     C                   ENDIF
076800170616     C*** TYPE
076900170616     C                   IF        %trim(SkSplitFLD(i)) = 'TYPEOFBU'
077000170616     C                   EVAL      CBUTYPE = SkSplitCSV_9(i)
077100170616     C                   ENDIF
077200170616     C*** GRPF
077300170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUFINANCIALGROUP'
077400170616     C                   EVAL      CBUGRPF = SkSplitCSV_9(i)
077500170616     C                   ENDIF
077600170616     C*** GRPG
077700170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUGEOGRAPHGROUP'
077800170616     C                   EVAL      CBUGRPG = SkSplitCSV_9(i)
077900170616     C                   ENDIF
078000170616     C*** GRPT
078100170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUTECHNICALGROUP'
078200170616     C                   EVAL      CBUGRPT = SkSplitCSV_9(i)
078300170616     C                   ENDIF
078400170616     C*** DPDC
078500170616     C                   IF        %trim(SkSplitFLD(i)) = 'DPDCONTRACT'
078600170616     C                   EVAL      CBUDPDC = SkSplitCSV_9(i)
078700170616     C                   ENDIF
078800170616     C*
078900170616     C                   ENDIF
079000170616     C                   EVAL      i = i + 1
079100170616     C                   ENDDO
079200170616     C*
079300170616     C*** VER
079400170616     C                   EVAL      CBUVER = NEW_VERSION
079500170616     C*
079600170616     C* Infine inserisco il record corrente
079700170616     C                   WRITE     DPCBU100
079800170616     C*
079900170616     C                   ENDSR
080000170616     C***
080100170616
080200170616
080300170616     C*----------------------------------------------------*
080400170616     C*  GESTIONE MAPPATURA "POSTALCODE"
080500170616     C*----------------------------------------------------*
080600170616     C     GESCPC        BEGSR
080700170616     C*
080800170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
080900170616     C                   CLEAR                   DPCPC100
081000170616     C*
081100170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
081200170616     C                   Z-ADD     2             i                 4 0
081300170620     C                   DOW       i <= %elem(SkSplitFLD)
081400170620     C                             AND i <= OutIdxMax
081500170616     C*
081600170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
081700170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
081800170616     C                             SkSplitCSV_9(i) <> *blanks
081900170616     C*
082000170616     C*** ISO2
082100170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
082200170616     C                   EVAL      CPCISO2 = SkSplitCSV_9(i)
082300170616     C                   ENDIF
082400170616     C*** PTCB
082500170616     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
082600170616     C                   EVAL      CPCPTCB = SkSplitCSV_9(i)
082700170616     C                   ENDIF
082800170616     C*** PTCE
082900170616     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
083000170616     C                   EVAL      CPCPTCE = SkSplitCSV_9(i)
083100170616     C                   ENDIF
083200170616     C*** PTCT
083300170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODETYPE'
083400170620     C                   EVAL      CPCPTCT = SkSplitCSV_9(i)
083500170616     C                   ENDIF
083600170616     C*
083700170616     C                   ENDIF
083800170616     C                   EVAL      i = i + 1
083900170616     C                   ENDDO
084000170619     C*
084100170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
084200170619     C                   IF        CPCPTCE = *blanks
084300170619     C                   EVAL      CPCPTCE = CPCPTCB
084400170619     C                   ENDIF
084500170616     C*
084600170616     C*** VER
084700170616     C                   EVAL      CPCVER = NEW_VERSION
084800170616     C*
084900170616     C* Infine inserisco il record corrente
085000170616     C                   WRITE     DPCPC100
085100170616     C*
085200170616     C                   ENDSR
085300170616     C***
085400170616
085500170616
085600170616     C*----------------------------------------------------*
085700170616     C*  GESTIONE MAPPATURA "STATE"
085800170616     C*----------------------------------------------------*
085900170616     C     GESCST        BEGSR
086000170616     C*
086100170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
086200170616     C                   CLEAR                   DPCST100
086300170616     C*
086400170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086500170616     C                   Z-ADD     2             i                 4 0
086600170620     C                   DOW       i <= %elem(SkSplitFLD)
086700170620     C                             AND i <= OutIdxMax
086800170616     C*
086900170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
087000170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
087100170616     C                             SkSplitCSV_9(i) <> *blanks
087200170616     C*
087300170616     C*** ISO2
087400170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
087500170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
087600170616     C                   ENDIF
087700170616     C*** REGC
087800170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODE'
087900170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
088000170616     C                   ENDIF
088100170616     C*** STAC
088200170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
088300170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
088400170616     C                   ENDIF
088500170616     C*
088600170616     C                   ENDIF
088700170616     C                   EVAL      i = i + 1
088800170616     C                   ENDDO
088900170616     C*
089000170616     C*** VER
089100170616     C                   EVAL      CSTVER = NEW_VERSION
089200170616     C*
089300170616     C* Infine inserisco il record corrente
089400170616     C                   WRITE     DPCST100
089500170616     C*
089600170616     C                   ENDSR
089700170616     C***
089800170616
089900170616
090000170616     C*----------------------------------------------------*
090100170616     C*  GESTIONE MAPPATURA "STATE.IT"
090200170616     C*----------------------------------------------------*
090300170616     C     GESCST_IT     BEGSR
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
091900170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
092000170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
092100170616     C                   ENDIF
092200170616     C*** STAC
092300170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
092400170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
092500170616     C                   ENDIF
092600170616     C*** SRTC
092700170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
092800170616     C                   ENDIF
092900170616     C*** DESxx
093000170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
093100170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
093200170616     C                   ENDIF
093300170616     C*
093400170616     C                   ENDIF
093500170616     C                   EVAL      i = i + 1
093600170616     C                   ENDDO
093700170616     C*
093800170616     C*** ATB
093900170616     C                   EVAL      CSTATB = *blanks
094000170616     C*** VER
094100170616     C                   EVAL      CSTVER = NEW_VERSION
094200170616     C*
094300170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
094400170621     C                   IF        wDESxx <> *blanks
094500170616     C     KEYcst_11C    CHAIN     dpCST11i
094600170616     C                   IF        %found(dpCST11i)
094700170621     C                   EVAL      CSTDESIT = wDESxx
094800170616     C                   UPDATE    DPCST100
094900170616     C                   ENDIF
095000170620     C                   ENDIF
095100170616     C*
095200170616     C                   ENDSR
095300170616     C***
095400170616
095500170616
095600170616     C*----------------------------------------------------*
095700170616     C*  GESTIONE MAPPATURA "STATE.DE"
095800170616     C*----------------------------------------------------*
095900170616     C     GESCST_DE     BEGSR
096000170616     C*
096100170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096200170616     C                   Z-ADD     2             i                 4 0
096300170620     C                   DOW       i <= %elem(SkSplitFLD)
096400170620     C                             AND i <= OutIdxMax
096500170616     C*
096600170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096700170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
096800170616     C                             SkSplitCSV_9(i) <> *blanks
096900170616     C*
097000170616     C*** ISO2
097100170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
097200170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
097300170616     C                   ENDIF
097400170616     C*** REGC
097500170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
097600170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
097700170616     C                   ENDIF
097800170616     C*** STAC
097900170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
098000170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
098100170616     C                   ENDIF
098200170616     C*** SRTC
098300170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
098400170616     C                   ENDIF
098500170616     C*** DESxx
098600170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
098700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
098800170616     C                   ENDIF
098900170616     C*
099000170616     C                   ENDIF
099100170616     C                   EVAL      i = i + 1
099200170616     C                   ENDDO
099300170616     C*
099400170616     C*** ATB
099500170616     C                   EVAL      CSTATB = *blanks
099600170616     C*** VER
099700170616     C                   EVAL      CSTVER = NEW_VERSION
099800170616     C*
099900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
100000170621     C                   IF        wDESxx <> *blanks
100100170616     C     KEYcst_11C    CHAIN     dpCST11i
100200170616     C                   IF        %found(dpCST11i)
100300170621     C                   EVAL      CSTDESDE = wDESxx
100400170616     C                   UPDATE    DPCST100
100500170616     C                   ENDIF
100600170620     C                   ENDIF
100700170616     C*
100800170616     C                   ENDSR
100900170616     C***
101000170616
101100170616
101200170616     C*----------------------------------------------------*
101300170616     C*  GESTIONE MAPPATURA "STATE.FR"
101400170616     C*----------------------------------------------------*
101500170616     C     GESCST_FR     BEGSR
101600170616     C*
101700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101800170616     C                   Z-ADD     2             i                 4 0
101900170620     C                   DOW       i <= %elem(SkSplitFLD)
102000170620     C                             AND i <= OutIdxMax
102100170616     C*
102200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
102300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
102400170616     C                             SkSplitCSV_9(i) <> *blanks
102500170616     C*
102600170616     C*** ISO2
102700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
102800170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
102900170616     C                   ENDIF
103000170616     C*** REGC
103100170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
103200170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
103300170616     C                   ENDIF
103400170616     C*** STAC
103500170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
103600170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
103700170616     C                   ENDIF
103800170616     C*** SRTC
103900170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
104000170616     C                   ENDIF
104100170616     C*** DESxx
104200170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
104300170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
104400170616     C                   ENDIF
104500170616     C*
104600170616     C                   ENDIF
104700170616     C                   EVAL      i = i + 1
104800170616     C                   ENDDO
104900170616     C*
105000170616     C*** ATB
105100170616     C                   EVAL      CSTATB = *blanks
105200170616     C*** VER
105300170616     C                   EVAL      CSTVER = NEW_VERSION
105400170616     C*
105500170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
105600170621     C                   IF        wDESxx <> *blanks
105700170616     C     KEYcst_11C    CHAIN     dpCST11i
105800170616     C                   IF        %found(dpCST11i)
105900170621     C                   EVAL      CSTDESFR = wDESxx
106000170616     C                   UPDATE    DPCST100
106100170620     C                   ENDIF
106200170616     C                   ENDIF
106300170616     C*
106400170616     C                   ENDSR
106500170616     C***
106600170616
106700170616
106800170616     C*----------------------------------------------------*
106900170616     C*  GESTIONE MAPPATURA "STATE.EN"
107000170616     C*----------------------------------------------------*
107100170616     C     GESCST_EN     BEGSR
107200170616     C*
107300170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
107400170616     C                   Z-ADD     2             i                 4 0
107500170620     C                   DOW       i <= %elem(SkSplitFLD)
107600170620     C                             AND i <= OutIdxMax
107700170616     C*
107800170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
107900170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
108000170616     C                             SkSplitCSV_9(i) <> *blanks
108100170616     C*
108200170616     C*** ISO2
108300170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
108400170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
108500170616     C                   ENDIF
108600170616     C*** REGC
108700170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
108800170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
108900170616     C                   ENDIF
109000170616     C*** STAC
109100170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
109200170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
109300170616     C                   ENDIF
109400170616     C*** SRTC
109500170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
109600170616     C                   ENDIF
109700170616     C*** DESxx
109800170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
109900170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
110000170616     C                   ENDIF
110100170616     C*
110200170616     C                   ENDIF
110300170616     C                   EVAL      i = i + 1
110400170616     C                   ENDDO
110500170616     C*
110600170616     C*** ATB
110700170616     C                   EVAL      CSTATB = *blanks
110800170616     C*** VER
110900170616     C                   EVAL      CSTVER = NEW_VERSION
111000170616     C*
111100170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
111200170621     C                   IF        wDESxx <> *blanks
111300170616     C     KEYcst_11C    CHAIN     dpCST11i
111400170616     C                   IF        %found(dpCST11i)
111500170621     C                   EVAL      CSTDESEN = wDESxx
111600170616     C                   UPDATE    DPCST100
111700170620     C                   ENDIF
111800170616     C                   ENDIF
111900170616     C*
112000170616     C                   ENDSR
112100170616     C***
112200170616
112300170616
112400170616     C*----------------------------------------------------*
112500170616     C*  GESTIONE MAPPATURA "ELEMENTS"
112600170616     C*----------------------------------------------------*
112700170616     C     GESCEL        BEGSR
112800170616     C*
112900170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
113000170616     C                   CLEAR                   DPCEL100
113100170616     C*
113200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
113300170616     C                   Z-ADD     2             i                 4 0
113400170620     C                   DOW       i <= %elem(SkSplitFLD)
113500170620     C                             AND i <= OutIdxMax
113600170616     C*
113700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
113800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
113900170616     C                             SkSplitCSV_9(i) <> *blanks
114000170616     C*
114100170620     C*** ELMC
114200170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
114300170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
114400170620     C                   EXSR      CHKNUM
114500170620     C                   IF        PiInt=*on
114600170620     C                   Z-ADD     PiVal         CELELMC
114700170620     C                   ENDIF
114800170620     C                   ENDIF
114900170616     C*** SRTC
115000170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTSHORTCUT'
115100170616     C                   EVAL      CELSRTC = SkSplitCSV_9(i)
115200170616     C                   ENDIF
115300170616     C*** ELMT
115400170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTTYPE'
115500170616     C                   EVAL      CELELMT = SkSplitCSV_9(i)
115600170616     C                   ENDIF
115700170616     C*
115800170616     C                   ENDIF
115900170616     C                   EVAL      i = i + 1
116000170616     C                   ENDDO
116100170616     C*
116200170616     C*** VER
116300170616     C                   EVAL      CELVER = NEW_VERSION
116400170616     C*
116500170616     C* Infine inserisco il record corrente
116600170616     C                   WRITE     DPCEL100
116700170616     C*
116800170616     C                   ENDSR
116900170616     C***
117000170616
117100170616
117200170616     C*----------------------------------------------------*
117300170616     C*  GESTIONE MAPPATURA "ELEMENTS.IT"
117400170616     C*----------------------------------------------------*
117500170616     C     GESCEL_IT     BEGSR
117600170616     C*
117700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
117800170616     C                   Z-ADD     2             i                 4 0
117900170620     C                   DOW       i <= %elem(SkSplitFLD)
118000170620     C                             AND i <= OutIdxMax
118100170616     C*
118200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
118300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
118400170616     C                             SkSplitCSV_9(i) <> *blanks
118500170616     C*
118600170620     C*** ELMC
118700170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
118800170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
118900170620     C                   EXSR      CHKNUM
119000170620     C                   IF        PiInt=*on
119100170620     C                   Z-ADD     PiVal         CELELMC
119200170620     C                   ENDIF
119300170620     C                   ENDIF
119400170616     C*** DESxx
119500170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
119600170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
119700170616     C                   ENDIF
119800170616     C*
119900170616     C                   ENDIF
120000170616     C                   EVAL      i = i + 1
120100170616     C                   ENDDO
120200170616     C*
120300170616     C*** ATB
120400170616     C                   EVAL      CELATB = *blanks
120500170616     C*** VER
120600170616     C                   EVAL      CELVER = NEW_VERSION
120700170616     C*
120800170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
120900170621     C                   IF        wDESxx <> *blanks
121000170616     C     KEYcel_11C    CHAIN     dpCEL11i
121100170616     C                   IF        %found(dpCEL11i)
121200170621     C                   EVAL      CELDESIT = wDESxx
121300170616     C                   UPDATE    DPCEL100
121400170620     C                   ENDIF
121500170616     C                   ENDIF
121600170616     C*
121700170616     C                   ENDSR
121800170616     C***
121900170616
122000170616
122100170616     C*----------------------------------------------------*
122200170616     C*  GESTIONE MAPPATURA "ELEMENTS.DE"
122300170616     C*----------------------------------------------------*
122400170616     C     GESCEL_DE     BEGSR
122500170616     C*
122600170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
122700170616     C                   Z-ADD     2             i                 4 0
122800170620     C                   DOW       i <= %elem(SkSplitFLD)
122900170620     C                             AND i <= OutIdxMax
123000170616     C*
123100170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
123200170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
123300170616     C                             SkSplitCSV_9(i) <> *blanks
123400170616     C*
123500170620     C*** ELMC
123600170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
123700170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
123800170620     C                   EXSR      CHKNUM
123900170620     C                   IF        PiInt=*on
124000170620     C                   Z-ADD     PiVal         CELELMC
124100170620     C                   ENDIF
124200170620     C                   ENDIF
124300170616     C*** DESxx
124400170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
124500170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
124600170616     C                   ENDIF
124700170616     C*
124800170616     C                   ENDIF
124900170616     C                   EVAL      i = i + 1
125000170616     C                   ENDDO
125100170616     C*
125200170616     C*** ATB
125300170616     C                   EVAL      CELATB = *blanks
125400170616     C*** VER
125500170616     C                   EVAL      CELVER = NEW_VERSION
125600170616     C*
125700170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
125800170621     C                   IF        wDESxx <> *blanks
125900170616     C     KEYcel_11C    CHAIN     dpCEL11i
126000170616     C                   IF        %found(dpCEL11i)
126100170621     C                   EVAL      CELDESDE = wDESxx
126200170616     C                   UPDATE    DPCEL100
126300170620     C                   ENDIF
126400170616     C                   ENDIF
126500170616     C*
126600170616     C                   ENDSR
126700170616     C***
126800170616
126900170616
127000170616     C*----------------------------------------------------*
127100170616     C*  GESTIONE MAPPATURA "ELEMENTS.FR"
127200170616     C*----------------------------------------------------*
127300170616     C     GESCEL_FR     BEGSR
127400170616     C*
127500170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
127600170616     C                   Z-ADD     2             i                 4 0
127700170620     C                   DOW       i <= %elem(SkSplitFLD)
127800170620     C                             AND i <= OutIdxMax
127900170616     C*
128000170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
128100170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
128200170616     C                             SkSplitCSV_9(i) <> *blanks
128300170616     C*
128400170620     C*** ELMC
128500170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
128600170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
128700170620     C                   EXSR      CHKNUM
128800170620     C                   IF        PiInt=*on
128900170620     C                   Z-ADD     PiVal         CELELMC
129000170620     C                   ENDIF
129100170620     C                   ENDIF
129200170616     C*** DESxx
129300170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
129400170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
129500170616     C                   ENDIF
129600170616     C*
129700170616     C                   ENDIF
129800170616     C                   EVAL      i = i + 1
129900170616     C                   ENDDO
130000170616     C*
130100170616     C*** ATB
130200170616     C                   EVAL      CELATB = *blanks
130300170616     C*** VER
130400170616     C                   EVAL      CELVER = NEW_VERSION
130500170616     C*
130600170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
130700170621     C                   IF        wDESxx <> *blanks
130800170616     C     KEYcel_11C    CHAIN     dpCEL11i
130900170616     C                   IF        %found(dpCEL11i)
131000170621     C                   EVAL      CELDESFR = wDESxx
131100170616     C                   UPDATE    DPCEL100
131200170620     C                   ENDIF
131300170616     C                   ENDIF
131400170616     C*
131500170616     C                   ENDSR
131600170616     C***
131700170616
131800170616
131900170616     C*----------------------------------------------------*
132000170616     C*  GESTIONE MAPPATURA "ELEMENTS.EN"
132100170616     C*----------------------------------------------------*
132200170616     C     GESCEL_EN     BEGSR
132300170616     C*
132400170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
132500170616     C                   Z-ADD     2             i                 4 0
132600170620     C                   DOW       i <= %elem(SkSplitFLD)
132700170620     C                             AND i <= OutIdxMax
132800170616     C*
132900170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
133000170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
133100170616     C                             SkSplitCSV_9(i) <> *blanks
133200170616     C*
133300170620     C*** ELMC
133400170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
133500170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
133600170620     C                   EXSR      CHKNUM
133700170620     C                   IF        PiInt=*on
133800170620     C                   Z-ADD     PiVal         CELELMC
133900170620     C                   ENDIF
134000170620     C                   ENDIF
134100170616     C*** DESxx
134200170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
134300170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
134400170616     C                   ENDIF
134500170616     C*
134600170616     C                   ENDIF
134700170616     C                   EVAL      i = i + 1
134800170616     C                   ENDDO
134900170616     C*
135000170616     C*** ATB
135100170616     C                   EVAL      CELATB = *blanks
135200170616     C*** VER
135300170616     C                   EVAL      CELVER = NEW_VERSION
135400170616     C*
135500170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
135600170621     C                   IF        wDESxx <> *blanks
135700170616     C     KEYcel_11C    CHAIN     dpCEL11i
135800170616     C                   IF        %found(dpCEL11i)
135900170621     C                   EVAL      CELDESEN = wDESxx
136000170616     C                   UPDATE    DPCEL100
136100170620     C                   ENDIF
136200170616     C                   ENDIF
136300170616     C*
136400170616     C                   ENDSR
136500170616     C***
136600170616
136700170616
136800170616     C*----------------------------------------------------*
136900170616     C*  GESTIONE MAPPATURA "ELEMENTS.PT"
137000170616     C*----------------------------------------------------*
137100170616     C     GESCEL_PT     BEGSR
137200170616     C*
137300170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
137400170616     C                   Z-ADD     2             i                 4 0
137500170620     C                   DOW       i <= %elem(SkSplitFLD)
137600170620     C                             AND i <= OutIdxMax
137700170616     C*
137800170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
137900170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
138000170616     C                             SkSplitCSV_9(i) <> *blanks
138100170616     C*
138200170620     C*** ELMC
138300170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
138400170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
138500170620     C                   EXSR      CHKNUM
138600170620     C                   IF        PiInt=*on
138700170620     C                   Z-ADD     PiVal         CELELMC
138800170620     C                   ENDIF
138900170620     C                   ENDIF
139000170616     C*** DESxx
139100170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
139200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
139300170616     C                   ENDIF
139400170616     C*
139500170616     C                   ENDIF
139600170616     C                   EVAL      i = i + 1
139700170616     C                   ENDDO
139800170616     C*
139900170616     C*** ATB
140000170616     C                   EVAL      CELATB = *blanks
140100170616     C*** VER
140200170616     C                   EVAL      CELVER = NEW_VERSION
140300170616     C*
140400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
140500170621     C                   IF        wDESxx <> *blanks
140600170616     C     KEYcel_11C    CHAIN     dpCEL11i
140700170616     C                   IF        %found(dpCEL11i)
140800170621     C                   EVAL      CELDESPT = wDESxx
140900170616     C                   UPDATE    DPCEL100
141000170620     C                   ENDIF
141100170616     C                   ENDIF
141200170616     C*
141300170616     C                   ENDSR
141400170619     C***
141500170619
141600170619
141700170619     C*----------------------------------------------------*
141800170619     C*  GESTIONE MAPPATURA "SOCODE"
141900170619     C*----------------------------------------------------*
142000170619     C     GESCSO        BEGSR
142100170619     C*
142200170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
142300170619     C                   CLEAR                   DPCSO100
142400170619     C*
142500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
142600170619     C                   Z-ADD     2             i                 4 0
142700170620     C                   DOW       i <= %elem(SkSplitFLD)
142800170620     C                             AND i <= OutIdxMax
142900170619     C*
143000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
143100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
143200170619     C                             SkSplitCSV_9(i) <> *blanks
143300170619     C*
143400170619     C*** SORC
143500170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
143600170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
143700170619     C                   EXSR      CHKNUM
143800170619     C                   IF        PiInt=*on
143900170619     C                   Z-ADD     PiVal         CSOSORC
144000170619     C                   ENDIF
144100170619     C                   ENDIF
144200170619     C*** STXT
144300170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICETEXT'
144400170620     C                   EVAL      CSOSTXT  = SkSplitCSV_9(i)
144500170619     C                   ENDIF
144600170619     C*** SMRK
144700170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICEMARK'
144800170620     C                   EVAL      CSOSMRK  = SkSplitCSV_9(i)
144900170619     C                   ENDIF
145000170620     C*** ELMC
145100170619     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODES'
145200170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
145300170619     C                   ENDIF
145400170619     C*** ASCO
145500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ASCODENEEDED'
145600170620     C                   EVAL      CSOASCO  = SkSplitCSV_9(i)
145700170619     C                   ENDIF
145800170619     C*
145900170619     C                   ENDIF
146000170619     C                   EVAL      i = i + 1
146100170619     C                   ENDDO
146200170619     C*
146300170619     C*** VER
146400170619     C                   EVAL      CSOVER = NEW_VERSION
146500170620     C*
146600170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
146700170620     C                   EVAL      InStringa = w2048_01
146800170620     C                   EVAL      InSepara = ','
146900170620     C                   SETOFF                                       131419
147000170620     C                   SETON                                        13
147100170620     C                   EXSR      SR_SPLIT
147200170620     C*
147300170620     C* Se nessun elemento => scrivo comunque un record singolo
147400170620     C                   IF        OutIdxMax = *zeros
147500170620     C                   WRITE     DPCSO100
147600170620     C                   ELSE
147700170620     C*
147800170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
147900170620     C                   Z-ADD     1             j                 4 0
148000170620     C                   DOW       j <= %elem(SkSplitCSV_3)
148100170620     C                             AND j <= OutIdxMax
148200170620     C*** ELMC
148300170620     C                   EVAL      PiStr = SkSplitCSV_3(j)
148400170620     C                   EXSR      CHKNUM
148500170620     C                   IF        PiInt=*on
148600170620     C                   Z-ADD     PiVal         CSOELMC
148700170620     C                   ENDIF
148800170620     C*
148900170620     C* Infine inserisco il record corrente
149000170620     C                   WRITE     DPCSO100
149100170620     C*
149200170620     C                   EVAL      j = j + 1
149300170620     C                   ENDDO
149400170620     C                   ENDIF
149500170619     C*
149600170619     C                   ENDSR
149700170619     C***
149800170619
149900170619
150000170619     C*----------------------------------------------------*
150100170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.IT"
150200170619     C*----------------------------------------------------*
150300170620     C     GESCSO_IT     BEGSR
150400170619     C*
150500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
150600170619     C                   Z-ADD     2             i                 4 0
150700170620     C                   DOW       i <= %elem(SkSplitFLD)
150800170620     C                             AND i <= OutIdxMax
150900170620     C*
151000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
151100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
151200170619     C                             SkSplitCSV_9(i) <> *blanks
151300170619     C*
151400170620     C*** SORC
151500170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
151600170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
151700170620     C                   EXSR      CHKNUM
151800170620     C                   IF        PiInt=*on
151900170620     C                   Z-ADD     PiVal         CSOSORC
152000170620     C                   ENDIF
152100170620     C                   ENDIF
152200170619     C*** DESxx
152300170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
152400170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
152500170619     C                   ENDIF
152600170619     C*
152700170619     C                   ENDIF
152800170620     C                   EVAL      i = i + 1
152900170619     C                   ENDDO
153000170619     C*
153100170619     C*** ATB
153200170619     C                   EVAL      CSOATB = *blanks
153300170619     C*** VER
153400170619     C                   EVAL      CSOVER = NEW_VERSION
153500170619     C*
153600170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
153700170621     C                   IF        wDESxx <> *blanks
153800170620     C     KEYcso_11C    CHAIN     dpCSO11i
153900170619     C                   IF        %found(dpCSO11i)
154000170621     C                   EVAL      CSODESIT = wDESxx
154100170619     C                   UPDATE    DPCSO100
154200170620     C                   ENDIF
154300170619     C                   ENDIF
154400170619     C*
154500170619     C                   ENDSR
154600170619     C***
154700170619
154800170619
154900170619     C*----------------------------------------------------*
155000170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.EN"
155100170619     C*----------------------------------------------------*
155200170619     C     GESCSO_EN     BEGSR
155300170619     C*
155400170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
155500170619     C                   Z-ADD     2             i                 4 0
155600170620     C                   DOW       i <= %elem(SkSplitFLD)
155700170620     C                             AND i <= OutIdxMax
155800170619     C*
155900170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
156000170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
156100170619     C                             SkSplitCSV_9(i) <> *blanks
156200170619     C*
156300170620     C*** SORC
156400170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
156500170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
156600170620     C                   EXSR      CHKNUM
156700170620     C                   IF        PiInt=*on
156800170620     C                   Z-ADD     PiVal         CSOSORC
156900170620     C                   ENDIF
157000170620     C                   ENDIF
157100170619     C*** DESxx
157200170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
157300170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
157400170619     C                   ENDIF
157500170619     C*
157600170619     C                   ENDIF
157700170620     C                   EVAL      i = i + 1
157800170619     C                   ENDDO
157900170619     C*
158000170619     C*** ATB
158100170619     C                   EVAL      CSOATB = *blanks
158200170619     C*** VER
158300170619     C                   EVAL      CSOVER = NEW_VERSION
158400170619     C*
158500170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
158600170621     C                   IF        wDESxx <> *blanks
158700170619     C     KEYcso_11C    CHAIN     dpCSO11i
158800170619     C                   IF        %found(dpCSO11i)
158900170621     C                   EVAL      CSODESEN = wDESxx
159000170619     C                   UPDATE    DPCSO100
159100170620     C                   ENDIF
159200170619     C                   ENDIF
159300170619     C*
159400170619     C                   ENDSR
159500170619     C***
159600170619
159700170619
159800170619     C*----------------------------------------------------*
159900170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.DE"
160000170619     C*----------------------------------------------------*
160100170619     C     GESCSO_DE     BEGSR
160200170619     C*
160300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
160400170619     C                   Z-ADD     2             i                 4 0
160500170620     C                   DOW       i <= %elem(SkSplitFLD)
160600170620     C                             AND i <= OutIdxMax
160700170619     C*
160800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
160900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
161000170619     C                             SkSplitCSV_9(i) <> *blanks
161100170619     C*
161200170620     C*** SORC
161300170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
161400170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
161500170620     C                   EXSR      CHKNUM
161600170620     C                   IF        PiInt=*on
161700170620     C                   Z-ADD     PiVal         CSOSORC
161800170620     C                   ENDIF
161900170620     C                   ENDIF
162000170619     C*** DESxx
162100170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
162200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
162300170619     C                   ENDIF
162400170619     C*
162500170619     C                   ENDIF
162600170620     C                   EVAL      i = i + 1
162700170619     C                   ENDDO
162800170619     C*
162900170619     C*** ATB
163000170619     C                   EVAL      CSOATB = *blanks
163100170619     C*** VER
163200170619     C                   EVAL      CSOVER = NEW_VERSION
163300170619     C*
163400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
163500170621     C                   IF        wDESxx <> *blanks
163600170619     C     KEYcso_11C    CHAIN     dpCSO11i
163700170619     C                   IF        %found(dpCSO11i)
163800170621     C                   EVAL      CSODESDE = wDESxx
163900170619     C                   UPDATE    DPCSO100
164000170620     C                   ENDIF
164100170619     C                   ENDIF
164200170619     C*
164300170619     C                   ENDSR
164400170619     C***
164500170619
164600170619
164700170619     C*----------------------------------------------------*
164800170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.FR"
164900170619     C*----------------------------------------------------*
165000170619     C     GESCSO_FR     BEGSR
165100170619     C*
165200170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
165300170619     C                   Z-ADD     2             i                 4 0
165400170620     C                   DOW       i <= %elem(SkSplitFLD)
165500170620     C                             AND i <= OutIdxMax
165600170619     C*
165700170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
165800170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
165900170619     C                             SkSplitCSV_9(i) <> *blanks
166000170619     C*
166100170620     C*** SORC
166200170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
166300170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
166400170620     C                   EXSR      CHKNUM
166500170620     C                   IF        PiInt=*on
166600170620     C                   Z-ADD     PiVal         CSOSORC
166700170620     C                   ENDIF
166800170620     C                   ENDIF
166900170619     C*** DESxx
167000170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
167100170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
167200170619     C                   ENDIF
167300170619     C*
167400170619     C                   ENDIF
167500170620     C                   EVAL      i = i + 1
167600170619     C                   ENDDO
167700170619     C*
167800170619     C*** ATB
167900170619     C                   EVAL      CSOATB = *blanks
168000170619     C*** VER
168100170619     C                   EVAL      CSOVER = NEW_VERSION
168200170619     C*
168300170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
168400170621     C                   IF        wDESxx <> *blanks
168500170619     C     KEYcso_11C    CHAIN     dpCSO11i
168600170619     C                   IF        %found(dpCSO11i)
168700170621     C                   EVAL      CSODESFR = wDESxx
168800170619     C                   UPDATE    DPCSO100
168900170620     C                   ENDIF
169000170619     C                   ENDIF
169100170619     C*
169200170619     C                   ENDSR
169300170619     C***
169400170619
169500170619
169600170619     C*----------------------------------------------------*
169700170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.ET"
169800170619     C*----------------------------------------------------*
169900170619     C     GESCSO_ET     BEGSR
170000170619     C*
170100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
170200170619     C                   Z-ADD     2             i                 4 0
170300170620     C                   DOW       i <= %elem(SkSplitFLD)
170400170620     C                             AND i <= OutIdxMax
170500170619     C*
170600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
170700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
170800170619     C                             SkSplitCSV_9(i) <> *blanks
170900170619     C*
171000170620     C*** SORC
171100170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
171200170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
171300170620     C                   EXSR      CHKNUM
171400170620     C                   IF        PiInt=*on
171500170620     C                   Z-ADD     PiVal         CSOSORC
171600170620     C                   ENDIF
171700170620     C                   ENDIF
171800170619     C*** DESxx
171900170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
172000170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
172100170619     C                   ENDIF
172200170619     C*
172300170619     C                   ENDIF
172400170620     C                   EVAL      i = i + 1
172500170619     C                   ENDDO
172600170619     C*
172700170619     C*** ATB
172800170619     C                   EVAL      CSOATB = *blanks
172900170619     C*** VER
173000170619     C                   EVAL      CSOVER = NEW_VERSION
173100170619     C*
173200170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
173300170621     C                   IF        wDESxx <> *blanks
173400170619     C     KEYcso_11C    CHAIN     dpCSO11i
173500170619     C                   IF        %found(dpCSO11i)
173600170621     C                   EVAL      CSODESET = wDESxx
173700170619     C                   UPDATE    DPCSO100
173800170620     C                   ENDIF
173900170619     C                   ENDIF
174000170619     C*
174100170619     C                   ENDSR
174200170619     C***
174300170619
174400170619
174500170619     C*----------------------------------------------------*
174600170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.CS"
174700170619     C*----------------------------------------------------*
174800170619     C     GESCSO_CS     BEGSR
174900170619     C*
175000170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
175100170619     C                   Z-ADD     2             i                 4 0
175200170620     C                   DOW       i <= %elem(SkSplitFLD)
175300170620     C                             AND i <= OutIdxMax
175400170619     C*
175500170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
175600170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
175700170619     C                             SkSplitCSV_9(i) <> *blanks
175800170619     C*
175900170620     C*** SORC
176000170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
176100170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
176200170620     C                   EXSR      CHKNUM
176300170620     C                   IF        PiInt=*on
176400170620     C                   Z-ADD     PiVal         CSOSORC
176500170620     C                   ENDIF
176600170620     C                   ENDIF
176700170619     C*** DESxx
176800170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
176900170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
177000170619     C                   ENDIF
177100170619     C*
177200170619     C                   ENDIF
177300170620     C                   EVAL      i = i + 1
177400170619     C                   ENDDO
177500170619     C*
177600170619     C*** ATB
177700170619     C                   EVAL      CSOATB = *blanks
177800170619     C*** VER
177900170619     C                   EVAL      CSOVER = NEW_VERSION
178000170619     C*
178100170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
178200170621     C                   IF        wDESxx <> *blanks
178300170619     C     KEYcso_11C    CHAIN     dpCSO11i
178400170619     C                   IF        %found(dpCSO11i)
178500170621     C                   EVAL      CSODESCS = wDESxx
178600170619     C                   UPDATE    DPCSO100
178700170620     C                   ENDIF
178800170619     C                   ENDIF
178900170619     C*
179000170619     C                   ENDSR
179100170619     C***
179200170619
179300170619
179400170619     C*----------------------------------------------------*
179500170619     C*  GESTIONE MAPPATURA "ASCODE"
179600170619     C*----------------------------------------------------*
179700170619     C     GESCAS        BEGSR
179800170619     C*
179900170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
180000170619     C                   CLEAR                   DPCAS100
180100170619     C*
180200170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
180300170619     C                   Z-ADD     2             i                 4 0
180400170620     C                   DOW       i <= %elem(SkSplitFLD)
180500170620     C                             AND i <= OutIdxMax
180600170619     C*
180700170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
180800170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
180900170619     C                             SkSplitCSV_9(i) <> *blanks
181000170619     C*
181100170619     C*** ADSC
181200170619     C                   IF        %trim(SkSplitFLD(i)) = 'ASCODE'
181300170620     C                   EVAL      CASADSC = SkSplitCSV_9(i)
181400170619     C                   ENDIF
181500170619     C*** STXT
181600170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICETEXT'
181700170620     C                   EVAL      CASSTXT = SkSplitCSV_9(i)
181800170619     C                   ENDIF
181900170620     C*** ELMC
182000170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODES'
182100170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
182200170620     C                   ENDIF
182300170619     C*
182400170619     C                   ENDIF
182500170620     C                   EVAL      i = i + 1
182600170619     C                   ENDDO
182700170619     C*
182800170619     C*** VER
182900170619     C                   EVAL      CASVER = NEW_VERSION
183000170620     C*
183100170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
183200170620     C                   EVAL      InStringa = w2048_01
183300170620     C                   EVAL      InSepara = ','
183400170620     C                   SETOFF                                       131419
183500170620     C                   SETON                                        13
183600170620     C                   EXSR      SR_SPLIT
183700170620     C*
183800170620     C* Se nessun elemento => scrivo comunque un record singolo
183900170620     C                   IF        OutIdxMax = *zeros
184000170620     C                   WRITE     DPCAS100
184100170620     C                   ELSE
184200170620     C*
184300170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
184400170620     C                   Z-ADD     1             j                 4 0
184500170620     C                   DOW       j <= %elem(SkSplitCSV_3)
184600170620     C                             AND j <= OutIdxMax
184700170620     C*** ELMC
184800170620     C                   EVAL      PiStr = SkSplitCSV_3(j)
184900170620     C                   EXSR      CHKNUM
185000170620     C                   IF        PiInt=*on
185100170620     C                   Z-ADD     PiVal         CASELMC
185200170620     C                   ENDIF
185300170620     C*
185400170620     C* Infine inserisco il record corrente
185500170620     C                   WRITE     DPCAS100
185600170620     C*
185700170620     C                   EVAL      j = j + 1
185800170620     C                   ENDDO
185900170620     C                   ENDIF
186000170619     C*
186100170619     C                   ENDSR
186200170619     C***
186300170619
186400170619
186500170619     C*----------------------------------------------------*
186600170619     C*  GESTIONE MAPPATURA "IC"
186700170619     C*----------------------------------------------------*
186800170619     C     GESCIC        BEGSR
186900170619     C*
187000170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
187100170619     C                   CLEAR                   DPCIC100
187200170619     C*
187300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
187400170619     C                   Z-ADD     2             i                 4 0
187500170620     C                   DOW       i <= %elem(SkSplitFLD)
187600170620     C                             AND i <= OutIdxMax
187700170619     C*
187800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
187900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
188000170619     C                             SkSplitCSV_9(i) <> *blanks
188100170619     C*
188200170619     C*** ICC
188300170619     C                   IF        %trim(SkSplitFLD(i)) = 'INJECTIONCODE'
188400170619     C                   EVAL      CICICC  = SkSplitCSV_9(i)
188500170619     C                   ENDIF
188600170619     C*** DPTN
188700170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
188800170619     C                   EVAL      CICDPTN = SkSplitCSV_9(i)
188900170619     C                   ENDIF
189000170619     C*** ICD
189100170619     C                   IF        %trim(SkSplitFLD(i)) = 'DESCRIPTION'
189200170619     C                   EVAL      CICICD  = SkSplitCSV_9(i)
189300170619     C                   ENDIF
189400170619     C*
189500170619     C                   ENDIF
189600170620     C                   EVAL      i = i + 1
189700170619     C                   ENDDO
189800170619     C*
189900170619     C*** VER
190000170619     C                   EVAL      CICVER = NEW_VERSION
190100170619     C*
190200170619     C* Infine inserisco il record corrente
190300170619     C                   WRITE     DPCIC100
190400170619     C*
190500170619     C                   ENDSR
190600170619     C***
190700170619
190800170619
190900170619     C*----------------------------------------------------*
191000170619     C*  GESTIONE MAPPATURA "NETWORK"
191100170619     C*----------------------------------------------------*
191200170619     C     GESCNT        BEGSR
191300170619     C*
191400170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
191500170619     C                   CLEAR                   DPCNT100
191600170619     C*
191700170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
191800170619     C                   Z-ADD     2             i                 4 0
191900170620     C                   DOW       i <= %elem(SkSplitFLD)
192000170620     C                             AND i <= OutIdxMax
192100170619     C*
192200170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
192300170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
192400170619     C                             SkSplitCSV_9(i) <> *blanks
192500170619     C*
192600170619     C*** NTWC
192700170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKCODE'
192800170619     C                   EVAL      CNTNTWC = SkSplitCSV_9(i)
192900170619     C                   ENDIF
193000170619     C*** BUCN
193100170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
193200170619     C                   EVAL      CNTBUCN = SkSplitCSV_9(i)
193300170619     C                   ENDIF
193400170619     C*** ISO2
193500170619     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
193600170619     C                   EVAL      CNTISO2 = SkSplitCSV_9(i)
193700170619     C                   ENDIF
193800170619     C*** NTWN
193900170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKNAME'
194000170619     C                   EVAL      CNTNTWN = SkSplitCSV_9(i)
194100170619     C                   ENDIF
194200170619     C*
194300170619     C                   ENDIF
194400170619     C                   EVAL      i = i + 1
194500170619     C                   ENDDO
194600170619     C*
194700170619     C*** VER
194800170619     C                   EVAL      CNTVER = NEW_VERSION
194900170619     C*
195000170619     C* Infine inserisco il record corrente
195100170619     C                   WRITE     DPCNT100
195200170619     C*
195300170619     C                   ENDSR
195400170619     C***
195500170619
195600170619
195700170619     C*----------------------------------------------------*
195800170621     C*  GESTIONE MAPPATURA "NETWORKPLAN"
195900170619     C*----------------------------------------------------*
196000170619     C     GESCNP        BEGSR
196100170619     C*
196200170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
196300170619     C                   CLEAR                   DPCNP100
196400170619     C*
196500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
196600170619     C                   Z-ADD     2             i                 4 0
196700170620     C                   DOW       i <= %elem(SkSplitFLD)
196800170620     C                             AND i <= OutIdxMax
196900170619     C*
197000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
197100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
197200170619     C                             SkSplitCSV_9(i) <> *blanks
197300170619     C*
197400170619     C*** NTWP
197500170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
197600170619     C                   EVAL      CNPNTWP  = SkSplitCSV_9(i)
197700170619     C                   ENDIF
197800170621     C*** SORCB / SORCE
197900170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODELIST'
198000170621     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
198100170619     C                   ENDIF
198200170619     C*** ASCOL
198300170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOASCODELIST'
198400170619     C                   EVAL      CNPASCOL = SkSplitCSV_9(i)
198500170619     C                   ENDIF
198600170619     C*** FULL
198700170619     C                   IF        %trim(SkSplitFLD(i)) = 'FULLNETWORKPLAN'
198800170619     C                   EVAL      CNPFULL  = SkSplitCSV_9(i)
198900170619     C                   ENDIF
199000170619     C*** PTY
199100170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
199200170619     C                   EVAL      CNPPTY   = SkSplitCSV_9(i)
199300170619     C                   ENDIF
199400170619     C*
199500170619     C                   ENDIF
199600170619     C                   EVAL      i = i + 1
199700170619     C                   ENDDO
199800170619     C*
199900170619     C*** VER
200000170621     C                   EVAL      CNPVER = NEW_VERSION
200100170621     C*
200200170621     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
200300170621     C                   EVAL      InStringa = w2048_01
200400170621     C                   EVAL      InSepara = ','
200500170621     C                   SETOFF                                       131419
200600170621     C                   SETON                                        13
200700170621     C                   EXSR      SR_SPLIT
200800170621     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
200900170621     C*
201000170621     C* Se nessun elemento => scrivo comunque un record singolo
201100170621     C                   IF        sOutIdxMax_w1 = *zeros
201200170621     C                   WRITE     DPCNP100
201300170621     C                   ELSE
201400170621     C*
201500170621     C* Sviluppo tutti gli elementi della lista dei Routing Places
201600170621     C                   Z-ADD     1             j                 4 0
201700170621     C                   DOW       j <= %elem(SkSplitCSV_3)
201800170621     C                             AND j <= sOutIdxMax_w1
201900170621     C*
202000170621     C* Determino il tipo di Routing Place
202100170621     C                   SELECT
202200170621     C* SOCODE - Single
202300170621     C                   WHEN      %len(%trim((SkSplitCSV_3(j)))) = 4
202400170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):2:3)
202500170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
202600170621     C                   EVAL      CNPSORCE = CNPSORCB
202700170621     C                   WRITE     DPCNP100
202800170621     C* SOCODE - Range
202900170621     C                   WHEN      %scan('-':SkSplitCSV_3(j)) > *zeros
203000170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):2:3)
203100170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
203200170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):6:3)
203300170621     C                   EVAL      CNPSORCE = %dec(wSORC:3:0)
203400170621     C                   WRITE     DPCNP100
203500170621     C* SOCODE - List
203600170621     C                   WHEN      %scan(':':SkSplitCSV_3(j)) > *zeros
203700170621     C                   EVAL      InStringa = %subst(SkSplitCSV_3(j):2)
203800170621     C                   EVAL      InSepara = ':'
203900170621     C                   SETOFF                                       131419
204000170621     C                   SETON                                        14
204100170621     C                   EXSR      SR_SPLIT
204200170621     C*
204300170621     C* Sviluppo tutti gli elementi della lista dei Routing Places
204400170621     C                   Z-ADD     1             z                 4 0
204500170621     C                   DOW       z <= %elem(SkSplitCSV_4)
204600170621     C                             AND z <= OutIdxMax
204700170621     C                   EVAL      wSORC    = SkSplitCSV_4(z)
204800170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
204900170621     C                   EVAL      CNPSORCE = CNPSORCB
205000170621     C                   WRITE     DPCNP100
205100170621     C                   EVAL      z = z + 1
205200170621     C                   ENDDO
205300170621     C*
205400170621     C                   ENDSL
205500170621     C*
205600170621     C                   EVAL      j = j + 1
205700170621     C                   ENDDO
205800170621     C                   ENDIF
205900170619     C*
206000170619     C                   ENDSR
206100170619     C***
206200170619
206300170619
206400170619     C*----------------------------------------------------*
206500170619     C*  GESTIONE MAPPATURA "ALLOWSO"
206600170619     C*----------------------------------------------------*
206700170621     C     GESCWS        BEGSR
206800170619     C*
206900170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
207000170619     C                   CLEAR                   DPCWS100
207100170619     C*
207200170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
207300170619     C                   Z-ADD     2             i                 4 0
207400170620     C                   DOW       i <= %elem(SkSplitFLD)
207500170620     C                             AND i <= OutIdxMax
207600170619     C*
207700170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
207800170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
207900170619     C                             SkSplitCSV_9(i) <> *blanks
208000170619     C*
208100170621     C*** RULFT / RULFB / RULFE
208200170619     C                   IF        %trim(SkSplitFLD(i)) = 'RULEFROM'
208300170621     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
208400170621     C                   ENDIF
208500170621     C*** RULSB / RULSE
208600170621     C                   IF        %trim(SkSplitFLD(i)) = 'RULESERVICE'
208700170621     C                   EVAL      w2048_02 = SkSplitCSV_9(i)
208800170621     C                   ENDIF
208900170621     C*** RULTT / RULTB / RULTE
209000170621     C                   IF        %trim(SkSplitFLD(i)) = 'RULETO'
209100170621     C                   EVAL      w2048_03 = SkSplitCSV_9(i)
209200170619     C                   ENDIF
209300170621     C*** ZONTB / ZONTE
209400170619     C                   IF        %trim(SkSplitFLD(i)) = 'ZONETO'
209500170621     C                   EVAL      w2048_04 = SkSplitCSV_9(i)
209600170619     C                   ENDIF
209700170619     C*** ALWID
209800170619     C                   IF        %trim(SkSplitFLD(i)) = 'UNIQUEALLOWID'
209900170628     C                   EVAL      PiStr = SkSplitCSV_9(i)
210000170628     C                   EXSR      CHKNUM
210100170628     C                   IF        PiInt=*on
210200170628     C                   Z-ADD     PiVal         CWSALWID
210300170628     C                   ENDIF
210400170628     C                   ENDIF
210500170619     C*
210600170619     C                   ENDIF
210700170619     C                   EVAL      i = i + 1
210800170619     C                   ENDDO
210900170619     C*
211000170619     C*** VER
211100170619     C                   EVAL      CWSVER = NEW_VERSION
211200170621     C*
211300170621     C* Splitto il 1° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
211400170621     C                   EVAL      InStringa = w2048_01
211500170621     C                   EVAL      InSepara = ','
211600170621     C                   SETOFF                                       131419
211700170621     C                   SETON                                        13
211800170621     C                   EXSR      SR_SPLIT
211900170621     C                   EVAL      wSkCSV_03_w1  = %subarr(SkSplitCSV_3:1)
212000170621     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
212100170621     C*
212200170621     C* Splitto il 2° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
212300170621     C                   EVAL      InStringa = w2048_02
212400170621     C                   EVAL      InSepara = ','
212500170621     C                   SETOFF                                       131419
212600170621     C                   SETON                                        13
212700170621     C                   EXSR      SR_SPLIT
212800170621     C                   EVAL      wSkCSV_03_w2  = %subarr(SkSplitCSV_3:1)
212900170621     C                   EVAL      sOutIdxMax_w2 = OutIdxMax
213000170621     C*
213100170621     C* Splitto il 3° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
213200170621     C                   EVAL      InStringa = w2048_03
213300170621     C                   EVAL      InSepara = ','
213400170621     C                   SETOFF                                       131419
213500170621     C                   SETON                                        13
213600170621     C                   EXSR      SR_SPLIT
213700170621     C                   EVAL      wSkCSV_03_w3  = %subarr(SkSplitCSV_3:1)
213800170621     C                   EVAL      sOutIdxMax_w3 = OutIdxMax
213900170621     C*
214000170621     C* Splitto il 4° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
214100170621     C                   EVAL      InStringa = w2048_04
214200170621     C                   EVAL      InSepara = ','
214300170621     C                   SETOFF                                       131419
214400170621     C                   SETON                                        13
214500170621     C                   EXSR      SR_SPLIT
214600170621     C                   EVAL      wSkCSV_03_w4  = %subarr(SkSplitCSV_3:1)
214700170621     C                   EVAL      sOutIdxMax_w4 = OutIdxMax
214800170621     C*
214900170621     C* Verifico eventuale assenza di elementi che in tal caso forzo a 1
215000170621     C                   IF        sOutIdxMax_w1 = *zeros
215100170621     C                   EVAL      sOutIdxMax_w1 = 1
215200170621     C                   ENDIF
215300170621     C                   IF        sOutIdxMax_w2 = *zeros
215400170621     C                   EVAL      sOutIdxMax_w2 = 1
215500170621     C                   ENDIF
215600170621     C                   IF        sOutIdxMax_w3 = *zeros
215700170621     C                   EVAL      sOutIdxMax_w3 = 1
215800170621     C                   ENDIF
215900170621     C                   IF        sOutIdxMax_w4 = *zeros
216000170621     C                   EVAL      sOutIdxMax_w4 = 1
216100170621     C                   ENDIF
216200170621     C*
216300170621     C* Sviluppo tutti gli elementi "moltiplicando" le liste delle RULE FROM/SOCODE/TO
216400170621     C*
216500170621     C* 1° lista
216600170621     C                   Z-ADD     1             i                 4 0
216700170621     C                   DOW       i <= %elem(wSkCSV_03_w1)
216800170621     C                             AND i <= sOutIdxMax_w1
216900170621     C*
217000170621     C* Determino il tipo di RULE FROM
217100170621     C                   SELECT
217200170621     C* DEPOT - Single
217300170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'D' AND
217400170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 8
217500170621     C                   EVAL      CWSRULFT = 'D'
217600170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:7)
217700170621     C                   EVAL      CWSRULFE = CWSRULFB
217800170621     C* DEPOT - Range
217900170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'D' AND
218000170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 15
218100170621     C                   EVAL      CWSRULFT = 'D'
218200170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:7)
218300170621     C                   EVAL      CWSRULFE = %subst(wSkCSV_03_w1(i):9:7)
218400170621     C* DEPOT GROUP - Single
218500170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'G'
218600170621     C                   EVAL      CWSRULFT = 'G'
218700170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:4)
218800170621     C                   EVAL      CWSRULFE = CWSRULFB
218900170621     C* COUNTRY - Single
219000170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'C'
219100170621     C                   EVAL      CWSRULFT = 'C'
219200170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:2)
219300170621     C                   EVAL      CWSRULFE = CWSRULFB
219400170621     C* NETWORK - Single
219500170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'N'
219600170621     C                   EVAL      CWSRULFT = 'N'
219700170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:3)
219800170621     C                   EVAL      CWSRULFE = CWSRULFB
219900170621     C* IC - Single
220000170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'I'
220100170621     C                   EVAL      CWSRULFT = 'I'
220200170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:9)
220300170621     C                   EVAL      CWSRULFE = CWSRULFB
220400170621     C* BU - Single
220500170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'B' AND
220600170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 4
220700170621     C                   EVAL      CWSRULFT = 'B'
220800170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:3)
220900170621     C                   EVAL      CWSRULFE = CWSRULFB
221000170621     C* BU+COUNTRY - Single
221100170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'B' AND
221200170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 6
221300170621     C                   EVAL      CWSRULFT = 'B'
221400170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:5)
221500170621     C                   EVAL      CWSRULFE = CWSRULFB
221600170621     C*
221700170621     C                   ENDSL
221800170621     C*
221900170621     C* 2° lista
222000170621     C                   Z-ADD     1             j                 4 0
222100170621     C                   DOW       j <= %elem(wSkCSV_03_w2)
222200170621     C                             AND j <= sOutIdxMax_w2
222300170621     C*
222400170621     C* Determino il tipo di RULE SERVICE
222500170621     C                   SELECT
222600170621     C* SOCODE - Single
222700170621     C                   WHEN      %subst(wSkCSV_03_w2(j):1:2) = 'SO' AND
222800170621     C                             %len(%trim((wSkCSV_03_w2(j)))) = 5
222900170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):3:3)
223000170621     C                   EVAL      CWSRULSB = %dec(wSORC:3:0)
223100170621     C                   EVAL      CWSRULSE = CWSRULSB
223200170621     C* SOCODE - Range
223300170621     C                   WHEN      %subst(wSkCSV_03_w2(j):1:2) = 'SO' AND
223400170621     C                             %len(%trim((wSkCSV_03_w2(j)))) = 8
223500170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):3:3)
223600170621     C                   EVAL      CWSRULSB = %dec(wSORC:3:0)
223700170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):6:3)
223800170621     C                   EVAL      CWSRULSE = %dec(wSORC:3:0)
223900170621     C*
224000170621     C                   ENDSL
224100170621     C*
224200170621     C* 3° lista
224300170621     C                   Z-ADD     1             z                 4 0
224400170621     C                   DOW       z <= %elem(wSkCSV_03_w3)
224500170621     C                             AND z <= sOutIdxMax_w3
224600170621     C*
224700170621     C* Determino il tipo di RULE TO
224800170621     C                   SELECT
224900170621     C* DEPOT - Single
225000170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'D' AND
225100170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 8
225200170621     C                   EVAL      CWSRULTT = 'D'
225300170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:7)
225400170621     C                   EVAL      CWSRULTE = CWSRULTB
225500170621     C* DEPOT - Range
225600170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'D' AND
225700170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 15
225800170621     C                   EVAL      CWSRULTT = 'D'
225900170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:7)
226000170621     C                   EVAL      CWSRULTE = %subst(wSkCSV_03_w3(z):9:7)
226100170621     C* DEPOT GROUP - Single
226200170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'G'
226300170621     C                   EVAL      CWSRULTT = 'G'
226400170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:4)
226500170621     C                   EVAL      CWSRULTE = CWSRULTB
226600170621     C* COUNTRY - Single
226700170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'C'
226800170621     C                   EVAL      CWSRULTT = 'C'
226900170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:2)
227000170621     C                   EVAL      CWSRULTE = CWSRULTB
227100170621     C* NETWORK - Single
227200170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'N'
227300170621     C                   EVAL      CWSRULTT = 'N'
227400170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:3)
227500170621     C                   EVAL      CWSRULTE = CWSRULTB
227600170621     C* COUNTRY+ZIP CODE - Single
227700170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'Z' AND
227800170621     C                             %scan('-':wSkCSV_03_w3(z)) = *zeros
227900170621     C                   EVAL      CWSRULTT = 'Z'
228000170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:11)
228100170621     C                   EVAL      CWSRULTE = CWSRULTB
228200170621     C* COUNTRY+ZIP CODE - Range
228300170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'Z' AND
228400170621     C                             %scan('-':wSkCSV_03_w3(z)) > *zeros
228500170621     C                   EVAL      CWSRULTT = 'Z'
228600170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:
228700170621     C                                        %scan('-':wSkCSV_03_w3(z))-2)
228800170621     C                   EVAL      CWSRULTE = %subst(wSkCSV_03_w3(z):2:2)+
228900170621     C                                        %subst(wSkCSV_03_w3(z):
229000170621     C                                        %scan('-':wSkCSV_03_w3(z))+1:9)
229100170621     C* BU+COUNTRY - Single
229200170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'B' AND
229300170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 6
229400170621     C                   EVAL      CWSRULTT = 'B'
229500170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w1(z):2:5)
229600170621     C                   EVAL      CWSRULTE = CWSRULTB
229700170621     C*
229800170621     C                   ENDSL
229900170621     C*
230000170621     C* 4° lista
230100170621     C                   Z-ADD     1             x                 4 0
230200170621     C                   DOW       x <= %elem(wSkCSV_03_w4)
230300170621     C                             AND x <= sOutIdxMax_w4
230400170621     C*
230500170621     C* Determino il tipo di RULE SERVICE
230600170621     C                   SELECT
230700170621     C* COUNTRY+ZIP CODE - Single
230800170621     C                   WHEN      %subst(wSkCSV_03_w4(x):1:1) = 'Z' AND
230900170621     C                             %scan('-':wSkCSV_03_w4(x)) = *zeros
231000170621     C                   EVAL      CWSZONTB = %subst(wSkCSV_03_w4(x):2:11)
231100170621     C                   EVAL      CWSZONTE = CWSZONTB
231200170621     C* COUNTRY+ZIP CODE - Range
231300170621     C                   WHEN      %subst(wSkCSV_03_w4(x):1:1) = 'Z' AND
231400170621     C                             %scan('-':wSkCSV_03_w4(x)) > *zeros
231500170621     C                   EVAL      CWSZONTB = %subst(wSkCSV_03_w4(x):2:
231600170621     C                                        %scan('-':wSkCSV_03_w4(x))-2)
231700170621     C                   EVAL      CWSZONTE = %subst(wSkCSV_03_w4(x):2:2)+
231800170621     C                                        %subst(wSkCSV_03_w4(x):
231900170621     C                                        %scan('-':wSkCSV_03_w4(x))+1:9)
232000170621     C*
232100170621     C                   ENDSL
232200170621     C*
232300170621     C* Inserisco il record corrente
232400170621     C                   WRITE     DPCWS100
232500170621     C*
232600170621     C                   EVAL      x = x + 1
232700170621     C                   ENDDO
232800170621     C*
232900170621     C                   EVAL      z = z + 1
233000170621     C                   ENDDO
233100170621     C*
233200170621     C                   EVAL      j = j + 1
233300170621     C                   ENDDO
233400170621     C*
233500170621     C                   EVAL      i = i + 1
233600170621     C                   ENDDO
233700170619     C*
233800170619     C                   ENDSR
233900170619     C***
234000170619
234100170619
234200170619     C*----------------------------------------------------*
234300170619     C*  GESTIONE MAPPATURA "ALLOWAS"
234400170619     C*----------------------------------------------------*
234500170619     C     GESCWA        BEGSR
234600170619     C*
234700170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
234800170619     C                   CLEAR                   DPCWA100
234900170619     C*
235000170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
235100170619     C                   Z-ADD     2             i                 4 0
235200170620     C                   DOW       i <= %elem(SkSplitFLD)
235300170620     C                             AND i <= OutIdxMax
235400170619     C*
235500170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
235600170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
235700170619     C                             SkSplitCSV_9(i) <> *blanks
235800170619     C*
235900170619     C*** ALWID
236000170619     C                   IF        %trim(SkSplitFLD(i)) = 'UNIQUEALLOWID'
236100170619     C                   EVAL      CWAALWID = SkSplitCSV_9(i)
236200170619     C                   ENDIF
236300170619     C*** RULS
236400170619     C                   IF        %trim(SkSplitFLD(i)) = 'RULESERVICE'
236500170619     C                   EVAL      CWARULS  = SkSplitCSV_9(i)
236600170619     C                   ENDIF
236700170619     C*** ZONT
236800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ZONETO'
236900170619     C                   EVAL      CWAZONT  = SkSplitCSV_9(i)
237000170619     C                   ENDIF
237100170619     C*
237200170619     C                   ENDIF
237300170619     C                   EVAL      i = i + 1
237400170619     C                   ENDDO
237500170619     C*
237600170619     C*** VER
237700170619     C                   EVAL      CWAVER = NEW_VERSION
237800170619     C*
237900170619     C* Infine inserisco il record corrente
238000170619     C                   WRITE     DPCWA100
238100170619     C*
238200170619     C                   ENDSR
238300170619     C***
238400170619
238500170619
238600170619     C*----------------------------------------------------*
238700170619     C*  GESTIONE MAPPATURA "RELATION"
238800170619     C*----------------------------------------------------*
238900170619     C     GESCRE        BEGSR
239000170619     C*
239100170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
239200170619     C                   CLEAR                   DPCRE100
239300170619     C*
239400170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
239500170619     C                   Z-ADD     2             i                 4 0
239600170620     C                   DOW       i <= %elem(SkSplitFLD)
239700170620     C                             AND i <= OutIdxMax
239800170619     C*
239900170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
240000170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
240100170619     C                             SkSplitCSV_9(i) <> *blanks
240200170619     C*
240300170619     C*** DPTN
240400170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
240500170619     C                   EVAL      CREDPTN  = SkSplitCSV_9(i)
240600170619     C                   ENDIF
240700170619     C*** ROUT
240800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTE'
240900170619     C                   EVAL      CREROUT  = SkSplitCSV_9(i)
241000170619     C                   ENDIF
241100170619     C*** RELA
241200170619     C                   IF        %trim(SkSplitFLD(i)) = 'RELATION'
241300170619     C                   EVAL      CRERELA  = SkSplitCSV_9(i)
241400170619     C                   ENDIF
241500170619     C*
241600170619     C                   ENDIF
241700170619     C                   EVAL      i = i + 1
241800170619     C                   ENDDO
241900170619     C*
242000170619     C*** VER
242100170619     C                   EVAL      CREVER = NEW_VERSION
242200170619     C*
242300170619     C* Infine inserisco il record corrente
242400170619     C                   WRITE     DPCRE100
242500170619     C*
242600170619     C                   ENDSR
242700170619     C***
242800170619
242900170619
243000170619     C*----------------------------------------------------*
243100170619     C*  GESTIONE MAPPATURA "DEPOTS"
243200170619     C*----------------------------------------------------*
243300170619     C     GESCDP        BEGSR
243400170619     C*
243500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
243600170619     C                   CLEAR                   DPCDP100
243700170619     C*
243800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
243900170619     C                   Z-ADD     2             i                 4 0
244000170620     C                   DOW       i <= %elem(SkSplitFLD)
244100170620     C                             AND i <= OutIdxMax
244200170619     C*
244300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
244400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
244500170619     C                             SkSplitCSV_9(i) <> *blanks
244600170619     C*
244700170619     C*** DPTN
244800170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
244900170619     C                   EVAL      CDPDPTN  = SkSplitCSV_9(i)
245000170619     C                   ENDIF
245100170619     C*** DSTR
245200170619     C                   IF        %trim(SkSplitFLD(i)) = 'DEPOTSTR'
245300170619     C                   EVAL      CDPDSTR  = SkSplitCSV_9(i)
245400170619     C                   ENDIF
245500170619     C*** IATA
245600170619     C                   IF        %trim(SkSplitFLD(i)) = 'IATALIKECODE'
245700170619     C                   EVAL      CDPIATA  = SkSplitCSV_9(i)
245800170619     C                   ENDIF
245900170619     C*** GRPID
246000170619     C                   IF        %trim(SkSplitFLD(i)) = 'GROUPID'
246100170619     C                   EVAL      CDPGRPID = SkSplitCSV_9(i)
246200170619     C                   ENDIF
246300170619     C*** NAME1
246400170619     C                   IF        %trim(SkSplitFLD(i)) = 'COMPANYNAME'
246500170619     C                   EVAL      CDPNAME1 = SkSplitCSV_9(i)
246600170619     C                   ENDIF
246700170619     C*** NAME2
246800170619     C                   IF        %trim(SkSplitFLD(i)) = 'COMPANYNAME2'
246900170619     C                   EVAL      CDPNAME2 = SkSplitCSV_9(i)
247000170619     C                   ENDIF
247100170619     C*** STRET
247200170619     C                   IF        %trim(SkSplitFLD(i)) = 'STREET'
247300170619     C                   EVAL      CDPSTRET = SkSplitCSV_9(i)
247400170619     C                   ENDIF
247500170619     C*** STNBR
247600170619     C                   IF        %trim(SkSplitFLD(i)) = 'PROPNUM'
247700170619     C                   EVAL      CDPSTNBR = SkSplitCSV_9(i)
247800170619     C                   ENDIF
247900170619     C*** ADDR1
248000170619     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS2'
248100170619     C                   EVAL      CDPADDR1 = SkSplitCSV_9(i)
248200170619     C                   ENDIF
248300170619     C*** ADDR2
248400170619     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS3'
248500170619     C                   EVAL      CDPADDR2 = SkSplitCSV_9(i)
248600170619     C                   ENDIF
248700170619     C*** FLOOR
248800170619     C                   IF        %trim(SkSplitFLD(i)) = 'FLOOR'
248900170619     C                   EVAL      CDPFLOOR = SkSplitCSV_9(i)
249000170619     C                   ENDIF
249100170619     C*** BUILD
249200170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUILDING'
249300170619     C                   EVAL      CDPBUILD = SkSplitCSV_9(i)
249400170619     C                   ENDIF
249500170619     C*** ISO2
249600170619     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
249700170619     C                   EVAL      CDPISO2  = SkSplitCSV_9(i)
249800170619     C                   ENDIF
249900170619     C*** STATE
250000170619     C                   IF        %trim(SkSplitFLD(i)) = 'STATE'
250100170619     C                   EVAL      CDPSTATE = SkSplitCSV_9(i)
250200170619     C                   ENDIF
250300170619     C*** PTC
250400170619     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODE'
250500170619     C                   EVAL      CDPPTC   = SkSplitCSV_9(i)
250600170619     C                   ENDIF
250700170619     C*** CITY
250800170619     C                   IF        %trim(SkSplitFLD(i)) = 'CITYNAME'
250900170619     C                   EVAL      CDPCITY  = SkSplitCSV_9(i)
251000170619     C                   ENDIF
251100170619     C*** TEL
251200170619     C                   IF        %trim(SkSplitFLD(i)) = 'PHONE'
251300170619     C                   EVAL      CDPTEL   = SkSplitCSV_9(i)
251400170619     C                   ENDIF
251500170619     C*** FAX
251600170619     C                   IF        %trim(SkSplitFLD(i)) = 'FAX'
251700170619     C                   EVAL      CDPFAX   = SkSplitCSV_9(i)
251800170619     C                   ENDIF
251900170619     C*** EMAIL
252000170619     C                   IF        %trim(SkSplitFLD(i)) = 'MAIL'
252100170619     C                   EVAL      CDPEMAIL = SkSplitCSV_9(i)
252200170619     C                   ENDIF
252300170619     C*** WEB
252400170619     C                   IF        %trim(SkSplitFLD(i)) = 'WEB'
252500170619     C                   EVAL      CDPWEB   = SkSplitCSV_9(i)
252600170619     C                   ENDIF
252700170619     C*** BUCN
252800170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
252900170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
253000170619     C                   EXSR      CHKNUM
253100170619     C                   IF        PiInt=*on
253200170619     C                   Z-ADD     PiVal         CDPBUCN
253300170619     C                   ENDIF
253400170619     C                   ENDIF
253500170619     C*** GEOX
253600170619     C                   IF        %trim(SkSplitFLD(i)) = 'SGPSLAT'
253700170619     C                   EVAL      CDPGEOX  = SkSplitCSV_9(i)
253800170619     C                   ENDIF
253900170619     C*** GEOY
254000170619     C                   IF        %trim(SkSplitFLD(i)) = 'SGPSLONG'
254100170619     C                   EVAL      CDPGEOY  = SkSplitCSV_9(i)
254200170619     C                   ENDIF
254300170619     C*** DUMY
254400170619     C                   IF        %trim(SkSplitFLD(i)) = 'DUMMYDEPOT'
254500170619     C                   EVAL      CDPDUMY  = SkSplitCSV_9(i)
254600170619     C                   ENDIF
254700170619     C*
254800170619     C                   ENDIF
254900170619     C                   EVAL      i = i + 1
255000170619     C                   ENDDO
255100170619     C*
255200170619     C*** VER
255300170619     C                   EVAL      CDPVER = NEW_VERSION
255400170619     C*
255500170619     C* Infine inserisco il record corrente
255600170619     C                   WRITE     DPCDP100
255700170619     C*
255800170619     C                   ENDSR
255900170619     C***
256000170619
256100170619
256200170619     C*----------------------------------------------------*
256300170619     C*  GESTIONE MAPPATURA "R1DBU"
256400170619     C*----------------------------------------------------*
256500170619     C     GESCR1        BEGSR
256600170619     C*
256700170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
256800170619     C                   CLEAR                   DPCR1100
256900170619     C*
257000170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
257100170619     C                   Z-ADD     2             i                 4 0
257200170620     C                   DOW       i <= %elem(SkSplitFLD)
257300170620     C                             AND i <= OutIdxMax
257400170619     C*
257500170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
257600170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
257700170619     C                             SkSplitCSV_9(i) <> *blanks
257800170621     C*
257900170621     C*** ROUT / ROUPB / ROUPE
258000170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
258100170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
258200170619     C                   ENDIF
258300170619     C*** TRADS
258400170619     C                   IF        %trim(SkSplitFLD(i)) = 'TRADESTATUS'
258500170619     C                   EVAL      CR1TRADS = SkSplitCSV_9(i)
258600170619     C                   ENDIF
258700170619     C*** ISO2
258800170619     C                   IF        %trim(SkSplitFLD(i)) =
258900170619     C                                        'DESTINATIONCOUNTRYCODE'
259000170619     C                   EVAL      CR1ISO2  = SkSplitCSV_9(i)
259100170619     C                   ENDIF
259200170619     C*** PTCB
259300170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
259400170619     C                   EVAL      CR1PTCB  = SkSplitCSV_9(i)
259500170619     C                   ENDIF
259600170619     C*** PTCE
259700170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
259800170619     C                   EVAL      CR1PTCE  = SkSplitCSV_9(i)
259900170619     C                   ENDIF
260000170619     C*** NTWP
260100170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
260200170619     C                   EVAL      CR1NTWP  = SkSplitCSV_9(i)
260300170619     C                   ENDIF
260400170619     C*** PTY
260500170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
260600170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
260700170619     C                   EXSR      CHKNUM
260800170619     C                   IF        PiInt=*on
260900170619     C                   Z-ADD     PiVal         CR1PTY
261000170619     C                   ENDIF
261100170619     C                   ENDIF
261200170619     C*** BUCN
261300170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
261400170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
261500170619     C                   EXSR      CHKNUM
261600170619     C                   IF        PiInt=*on
261700170619     C                   Z-ADD     PiVal         CR1BUCN
261800170619     C                   ENDIF
261900170619     C                   ENDIF
262000170619     C*
262100170619     C                   ENDIF
262200170619     C                   EVAL      i = i + 1
262300170619     C                   ENDDO
262400170619     C*
262500170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
262600170619     C                   IF        CR1PTCE = *blanks
262700170619     C                   EVAL      CR1PTCE = CR1PTCB
262800170619     C                   ENDIF
262900170619     C*
263000170619     C*** VER
263100170619     C                   EVAL      CR1VER = NEW_VERSION
263200170620     C*
263300170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
263400170620     C                   EVAL      InStringa = w2048_01
263500170620     C                   EVAL      InSepara = ','
263600170620     C                   SETOFF                                       131419
263700170620     C                   SETON                                        13
263800170620     C                   EXSR      SR_SPLIT
263900170620     C*
264000170620     C* Se nessun elemento => scrivo comunque un record singolo
264100170620     C                   IF        OutIdxMax = *zeros
264200170620     C                   WRITE     DPCR1100
264300170620     C                   ELSE
264400170620     C*
264500170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
264600170620     C                   Z-ADD     1             j                 4 0
264700170620     C                   DOW       j <= %elem(SkSplitCSV_3)
264800170620     C                             AND j <= OutIdxMax
264900170620     C*
265000170620     C* Determino il tipo di Routing Place
265100170620     C                   SELECT
265200170620     C* Injection Code - Range
265300170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
265400170620     C                   EVAL      CR1ROUT  = 'I'
265500170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):3:9)
265600170620     C                   EVAL      CR1ROUPE = %subst(SkSplitCSV_3(j):12:9)
265700170620     C* Injection Code - Single
265800170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
265900170620     C                   EVAL      CR1ROUT  = 'I'
266000170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:9)
266100170620     C                   EVAL      CR1ROUPE = CR1ROUPB
266200170620     C* Country Code
266300170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
266400170620     C                   EVAL      CR1ROUT  = 'C'
266500170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:2)
266600170620     C                   EVAL      CR1ROUPE = CR1ROUPB
266700170620     C* Depot Code - Range
266800170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
266900170620     C                   EVAL      CR1ROUT  = 'D'
267000170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):3:7)
267100170620     C                   EVAL      CR1ROUPE = %subst(SkSplitCSV_3(j):10:7)
267200170620     C* Depot Code - Single
267300170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
267400170620     C                   EVAL      CR1ROUT  = 'D'
267500170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:7)
267600170620     C                   EVAL      CR1ROUPE = CR1ROUPB
267700170620     C* Depot Group
267800170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
267900170620     C                   EVAL      CR1ROUT  = 'G'
268000170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:4)
268100170620     C                   EVAL      CR1ROUPE = CR1ROUPB
268200170620     C* Business Unit
268300170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
268400170620     C                   EVAL      CR1ROUT  = 'B'
268500170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:3)
268600170620     C                   EVAL      CR1ROUPE = CR1ROUPB
268700170620     C*
268800170620     C                   ENDSL
268900170619     C*
269000170619     C* Infine inserisco il record corrente
269100170619     C                   WRITE     DPCR1100
269200170620     C*
269300170620     C                   EVAL      j = j + 1
269400170620     C                   ENDDO
269500170620     C                   ENDIF
269600170619     C*
269700170619     C                   ENDSR
269800170619     C***
269900170619
270000170619
270100170619     C*----------------------------------------------------*
270200170619     C*  GESTIONE MAPPATURA "R2ROUTE"
270300170619     C*----------------------------------------------------*
270400170619     C     GESCR2        BEGSR
270500170619     C*
270600170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
270700170619     C                   CLEAR                   DPCR2100
270800170619     C*
270900170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
271000170619     C                   Z-ADD     2             i                 4 0
271100170620     C                   DOW       i <= %elem(SkSplitFLD)
271200170620     C                             AND i <= OutIdxMax
271300170619     C*
271400170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
271500170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
271600170619     C                             SkSplitCSV_9(i) <> *blanks
271700170619     C*
271800170621     C*** ROUT / ROUPB / ROUPE
271900170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
272000170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
272100170619     C                   ENDIF
272200170619     C*** BUCN
272300170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
272400170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
272500170619     C                   EXSR      CHKNUM
272600170619     C                   IF        PiInt=*on
272700170619     C                   Z-ADD     PiVal         CR2BUCN
272800170619     C                   ENDIF
272900170619     C                   ENDIF
273000170619     C*** ISO2
273100170619     C                   IF        %trim(SkSplitFLD(i)) =
273200170619     C                                        'DESTINATIONCOUNTRYCODE'
273300170619     C                   EVAL      CR2ISO2  = SkSplitCSV_9(i)
273400170619     C                   ENDIF
273500170619     C*** TRADS
273600170619     C                   IF        %trim(SkSplitFLD(i)) = 'TRADESTATUS'
273700170619     C                   EVAL      CR2TRADS = SkSplitCSV_9(i)
273800170619     C                   ENDIF
273900170619     C*** PTCB
274000170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
274100170619     C                   EVAL      CR2PTCB  = SkSplitCSV_9(i)
274200170619     C                   ENDIF
274300170619     C*** PTCE
274400170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
274500170619     C                   EVAL      CR2PTCE  = SkSplitCSV_9(i)
274600170619     C                   ENDIF
274700170619     C*** NTWP
274800170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
274900170619     C                   EVAL      CR2NTWP  = SkSplitCSV_9(i)
275000170619     C                   ENDIF
275100170619     C*** PTY
275200170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
275300170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
275400170619     C                   EXSR      CHKNUM
275500170619     C                   IF        PiInt=*on
275600170619     C                   Z-ADD     PiVal         CR2PTY
275700170619     C                   ENDIF
275800170619     C                   ENDIF
275900170619     C*** DDPT
276000170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
276100170619     C                   EVAL      CR2DDPT  = SkSplitCSV_9(i)
276200170619     C                   ENDIF
276300170619     C*** SSORT
276400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SSORT'
276500170619     C                   EVAL      CR2SSORT = SkSplitCSV_9(i)
276600170619     C                   ENDIF
276700170619     C*** PTNC
276800170619     C                   IF        %trim(SkSplitFLD(i)) = 'PARTNERCODE'
276900170619     C                   EVAL      CR2PTNC  = SkSplitCSV_9(i)
277000170619     C                   ENDIF
277100170619     C*
277200170619     C                   ENDIF
277300170619     C                   EVAL      i = i + 1
277400170619     C                   ENDDO
277500170619     C*
277600170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
277700170619     C                   IF        CR2PTCE = *blanks
277800170619     C                   EVAL      CR2PTCE = CR2PTCB
277900170619     C                   ENDIF
278000170619     C*
278100170619     C*** VER
278200170619     C                   EVAL      CR2VER = NEW_VERSION
278300170620     C*
278400170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
278500170620     C                   EVAL      InStringa = w2048_01
278600170620     C                   EVAL      InSepara = ','
278700170620     C                   SETOFF                                       131419
278800170620     C                   SETON                                        13
278900170620     C                   EXSR      SR_SPLIT
279000170620     C*
279100170620     C* Se nessun elemento => scrivo comunque un record singolo
279200170620     C                   IF        OutIdxMax = *zeros
279300170620     C                   WRITE     DPCR2100
279400170620     C                   ELSE
279500170620     C*
279600170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
279700170620     C                   Z-ADD     1             j                 4 0
279800170620     C                   DOW       j <= %elem(SkSplitCSV_3)
279900170620     C                             AND j <= OutIdxMax
280000170620     C*
280100170620     C* Determino il tipo di Routing Place
280200170620     C                   SELECT
280300170620     C* Injection Code - Range
280400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
280500170620     C                   EVAL      CR2ROUT  = 'I'
280600170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):3:9)
280700170620     C                   EVAL      CR2ROUPE = %subst(SkSplitCSV_3(j):12:9)
280800170620     C* Injection Code - Single
280900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
281000170620     C                   EVAL      CR2ROUT  = 'I'
281100170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:9)
281200170620     C                   EVAL      CR2ROUPE = CR2ROUPB
281300170620     C* Country Code
281400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
281500170620     C                   EVAL      CR2ROUT  = 'C'
281600170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:2)
281700170620     C                   EVAL      CR2ROUPE = CR2ROUPB
281800170620     C* Depot Code - Range
281900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
282000170620     C                   EVAL      CR2ROUT  = 'D'
282100170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):3:7)
282200170620     C                   EVAL      CR2ROUPE = %subst(SkSplitCSV_3(j):10:7)
282300170620     C* Depot Code - Single
282400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
282500170620     C                   EVAL      CR2ROUT  = 'D'
282600170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:7)
282700170620     C                   EVAL      CR2ROUPE = CR2ROUPB
282800170620     C* Depot Group
282900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
283000170620     C                   EVAL      CR2ROUT  = 'G'
283100170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:4)
283200170620     C                   EVAL      CR2ROUPE = CR2ROUPB
283300170620     C* Business Unit
283400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
283500170620     C                   EVAL      CR2ROUT  = 'B'
283600170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:3)
283700170620     C                   EVAL      CR2ROUPE = CR2ROUPB
283800170620     C*
283900170620     C                   ENDSL
284000170620     C*
284100170620     C* Infine inserisco il record corrente
284200170620     C                   WRITE     DPCR2100
284300170620     C*
284400170620     C                   EVAL      j = j + 1
284500170620     C                   ENDDO
284600170620     C                   ENDIF
284700170619     C*
284800170619     C                   ENDSR
284900170619     C***
285000170619
285100170619
285200170619     C*----------------------------------------------------*
285300170619     C*  GESTIONE MAPPATURA "R3OSORT"
285400170619     C*----------------------------------------------------*
285500170619     C     GESCR3        BEGSR
285600170619     C*
285700170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
285800170619     C                   CLEAR                   DPCR3100
285900170619     C*
286000170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
286100170619     C                   Z-ADD     2             i                 4 0
286200170620     C                   DOW       i <= %elem(SkSplitFLD)
286300170620     C                             AND i <= OutIdxMax
286400170619     C*
286500170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
286600170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
286700170619     C                             SkSplitCSV_9(i) <> *blanks
286800170619     C*
286900170621     C*** ROUT / ROUPB / ROUPE
287000170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
287100170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
287200170619     C                   ENDIF
287300170619     C*** ISO2
287400170619     C                   IF        %trim(SkSplitFLD(i)) =
287500170619     C                                        'DESTINATIONCOUNTRYCODE'
287600170619     C                   EVAL      CR3ISO2  = SkSplitCSV_9(i)
287700170619     C                   ENDIF
287800170619     C*** DDPT
287900170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
288000170619     C                   EVAL      CR3DDPT  = SkSplitCSV_9(i)
288100170619     C                   ENDIF
288200170619     C*** PTCB
288300170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
288400170619     C                   EVAL      CR3PTCB  = SkSplitCSV_9(i)
288500170619     C                   ENDIF
288600170619     C*** PTCE
288700170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
288800170619     C                   EVAL      CR3PTCE  = SkSplitCSV_9(i)
288900170619     C                   ENDIF
289000170619     C*** NTWP
289100170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
289200170619     C                   EVAL      CR3NTWP  = SkSplitCSV_9(i)
289300170619     C                   ENDIF
289400170619     C*** PTY
289500170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
289600170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
289700170619     C                   EXSR      CHKNUM
289800170619     C                   IF        PiInt=*on
289900170619     C                   Z-ADD     PiVal         CR3PTY
290000170619     C                   ENDIF
290100170619     C                   ENDIF
290200170619     C*** OSORT
290300170619     C                   IF        %trim(SkSplitFLD(i)) = 'OSORT'
290400170620     C                   EVAL      CR3OSORT = SkSplitCSV_9(i)
290500170619     C                   ENDIF
290600170619     C*
290700170619     C                   ENDIF
290800170619     C                   EVAL      i = i + 1
290900170619     C                   ENDDO
291000170619     C*
291100170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
291200170619     C                   IF        CR3PTCE = *blanks
291300170619     C                   EVAL      CR3PTCE = CR3PTCB
291400170619     C                   ENDIF
291500170619     C*
291600170619     C*** VER
291700170619     C                   EVAL      CR3VER = NEW_VERSION
291800170620     C*
291900170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
292000170620     C                   EVAL      InStringa = w2048_01
292100170620     C                   EVAL      InSepara = ','
292200170620     C                   SETOFF                                       131419
292300170620     C                   SETON                                        13
292400170620     C                   EXSR      SR_SPLIT
292500170620     C*
292600170620     C* Se nessun elemento => scrivo comunque un record singolo
292700170620     C                   IF        OutIdxMax = *zeros
292800170620     C                   WRITE     DPCR3100
292900170620     C                   ELSE
293000170620     C*
293100170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
293200170620     C                   Z-ADD     1             j                 4 0
293300170620     C                   DOW       j <= %elem(SkSplitCSV_3)
293400170620     C                             AND j <= OutIdxMax
293500170620     C*
293600170620     C* Determino il tipo di Routing Place
293700170620     C                   SELECT
293800170620     C* Injection Code - Range
293900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
294000170620     C                   EVAL      CR3ROUT  = 'I'
294100170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):3:9)
294200170620     C                   EVAL      CR3ROUPE = %subst(SkSplitCSV_3(j):12:9)
294300170620     C* Injection Code - Single
294400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
294500170620     C                   EVAL      CR3ROUT  = 'I'
294600170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:9)
294700170620     C                   EVAL      CR3ROUPE = CR3ROUPB
294800170620     C* Country Code
294900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
295000170620     C                   EVAL      CR3ROUT  = 'C'
295100170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:2)
295200170620     C                   EVAL      CR3ROUPE = CR3ROUPB
295300170620     C* Depot Code - Range
295400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
295500170620     C                   EVAL      CR3ROUT  = 'D'
295600170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):3:7)
295700170620     C                   EVAL      CR3ROUPE = %subst(SkSplitCSV_3(j):10:7)
295800170620     C* Depot Code - Single
295900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
296000170620     C                   EVAL      CR3ROUT  = 'D'
296100170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:7)
296200170620     C                   EVAL      CR3ROUPE = CR3ROUPB
296300170620     C* Depot Group
296400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
296500170620     C                   EVAL      CR3ROUT  = 'G'
296600170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:4)
296700170620     C                   EVAL      CR3ROUPE = CR3ROUPB
296800170620     C* Business Unit
296900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
297000170620     C                   EVAL      CR3ROUT  = 'B'
297100170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:3)
297200170620     C                   EVAL      CR3ROUPE = CR3ROUPB
297300170620     C*
297400170620     C                   ENDSL
297500170620     C*
297600170620     C* Infine inserisco il record corrente
297700170620     C                   WRITE     DPCR3100
297800170620     C*
297900170620     C                   EVAL      j = j + 1
298000170620     C                   ENDDO
298100170620     C                   ENDIF
298200170619     C*
298300170619     C                   ENDSR
298400170619     C***
298500170619
298600170619
298700170619     C*----------------------------------------------------*
298800170619     C*  GESTIONE MAPPATURA "R4CSORT"
298900170619     C*----------------------------------------------------*
299000170619     C     GESCR4        BEGSR
299100170619     C*
299200170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
299300170619     C                   CLEAR                   DPCR4100
299400170619     C*
299500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
299600170619     C                   Z-ADD     2             i                 4 0
299700170620     C                   DOW       i <= %elem(SkSplitFLD)
299800170620     C                             AND i <= OutIdxMax
299900170619     C*
300000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
300100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
300200170619     C                             SkSplitCSV_9(i) <> *blanks
300300170619     C*
300400170621     C*** ROUT / ROUPB / ROUPE
300500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
300600170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
300700170619     C                   ENDIF
300800170619     C*** ISO2
300900170619     C                   IF        %trim(SkSplitFLD(i)) =
301000170619     C                                        'DESTINATIONCOUNTRYCODE'
301100170619     C                   EVAL      CR4ISO2  = SkSplitCSV_9(i)
301200170619     C                   ENDIF
301300170619     C*** DDPT
301400170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
301500170619     C                   EVAL      CR4DDPT  = SkSplitCSV_9(i)
301600170619     C                   ENDIF
301700170619     C*** NTWP
301800170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
301900170619     C                   EVAL      CR4NTWP  = SkSplitCSV_9(i)
302000170619     C                   ENDIF
302100170619     C*** PTY
302200170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
302300170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
302400170619     C                   EXSR      CHKNUM
302500170619     C                   IF        PiInt=*on
302600170619     C                   Z-ADD     PiVal         CR4PTY
302700170619     C                   ENDIF
302800170619     C                   ENDIF
302900170619     C*** CSORT
303000170619     C                   IF        %trim(SkSplitFLD(i)) = 'CSORT'
303100170620     C                   EVAL      CR4CSORT = SkSplitCSV_9(i)
303200170619     C                   ENDIF
303300170619     C*
303400170619     C                   ENDIF
303500170619     C                   EVAL      i = i + 1
303600170619     C                   ENDDO
303700170619     C*
303800170619     C*** VER
303900170619     C                   EVAL      CR4VER = NEW_VERSION
304000170620     C*
304100170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
304200170620     C                   EVAL      InStringa = w2048_01
304300170620     C                   EVAL      InSepara = ','
304400170620     C                   SETOFF                                       131419
304500170620     C                   SETON                                        13
304600170620     C                   EXSR      SR_SPLIT
304700170620     C*
304800170620     C* Se nessun elemento => scrivo comunque un record singolo
304900170620     C                   IF        OutIdxMax = *zeros
305000170620     C                   WRITE     DPCR4100
305100170620     C                   ELSE
305200170620     C*
305300170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
305400170620     C                   Z-ADD     1             j                 4 0
305500170620     C                   DOW       j <= %elem(SkSplitCSV_3)
305600170620     C                             AND j <= OutIdxMax
305700170620     C*
305800170620     C* Determino il tipo di Routing Place
305900170620     C                   SELECT
306000170620     C* Injection Code - Range
306100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
306200170620     C                   EVAL      CR4ROUT  = 'I'
306300170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):3:9)
306400170620     C                   EVAL      CR4ROUPE = %subst(SkSplitCSV_3(j):12:9)
306500170620     C* Injection Code - Single
306600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
306700170620     C                   EVAL      CR4ROUT  = 'I'
306800170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:9)
306900170620     C                   EVAL      CR4ROUPE = CR4ROUPB
307000170620     C* Country Code
307100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
307200170620     C                   EVAL      CR4ROUT  = 'C'
307300170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:2)
307400170620     C                   EVAL      CR4ROUPE = CR4ROUPB
307500170620     C* Depot Code - Range
307600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
307700170620     C                   EVAL      CR4ROUT  = 'D'
307800170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):3:7)
307900170620     C                   EVAL      CR4ROUPE = %subst(SkSplitCSV_3(j):10:7)
308000170620     C* Depot Code - Single
308100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
308200170620     C                   EVAL      CR4ROUT  = 'D'
308300170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:7)
308400170620     C                   EVAL      CR4ROUPE = CR4ROUPB
308500170620     C* Depot Group
308600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
308700170620     C                   EVAL      CR4ROUT  = 'G'
308800170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:4)
308900170620     C                   EVAL      CR4ROUPE = CR4ROUPB
309000170620     C* Business Unit
309100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
309200170620     C                   EVAL      CR4ROUT  = 'B'
309300170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:3)
309400170620     C                   EVAL      CR4ROUPE = CR4ROUPB
309500170620     C*
309600170620     C                   ENDSL
309700170620     C*
309800170620     C* Infine inserisco il record corrente
309900170620     C                   WRITE     DPCR4100
310000170620     C*
310100170620     C                   EVAL      j = j + 1
310200170620     C                   ENDDO
310300170620     C                   ENDIF
310400170619     C*
310500170619     C                   ENDSR
310600170619     C***
310700170619
310800170619
310900170619     C*----------------------------------------------------*
311000170619     C*  GESTIONE MAPPATURA "R5DSORT"
311100170619     C*----------------------------------------------------*
311200170619     C     GESCR5        BEGSR
311300170619     C*
311400170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
311500170619     C                   CLEAR                   DPCR5100
311600170619     C*
311700170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
311800170619     C                   Z-ADD     2             i                 4 0
311900170620     C                   DOW       i <= %elem(SkSplitFLD)
312000170620     C                             AND i <= OutIdxMax
312100170619     C*
312200170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
312300170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
312400170619     C                             SkSplitCSV_9(i) <> *blanks
312500170619     C*
312600170621     C*** ROUT / ROUPB / ROUPE
312700170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
312800170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
312900170619     C                   ENDIF
313000170619     C*** DDPT
313100170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
313200170619     C                   EVAL      CR5DDPT  = SkSplitCSV_9(i)
313300170619     C                   ENDIF
313400170619     C*** ISO2
313500170619     C                   IF        %trim(SkSplitFLD(i)) =
313600170619     C                                        'DESTINATIONCOUNTRYCODE'
313700170619     C                   EVAL      CR5ISO2  = SkSplitCSV_9(i)
313800170619     C                   ENDIF
313900170619     C*** PTCB
314000170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
314100170619     C                   EVAL      CR5PTCB  = SkSplitCSV_9(i)
314200170619     C                   ENDIF
314300170619     C*** PTCE
314400170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
314500170619     C                   EVAL      CR5PTCE  = SkSplitCSV_9(i)
314600170619     C                   ENDIF
314700170619     C*** NTWP
314800170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
314900170619     C                   EVAL      CR5NTWP  = SkSplitCSV_9(i)
315000170619     C                   ENDIF
315100170619     C*** PTY
315200170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
315300170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
315400170619     C                   EXSR      CHKNUM
315500170619     C                   IF        PiInt=*on
315600170619     C                   Z-ADD     PiVal         CR5PTY
315700170619     C                   ENDIF
315800170619     C                   ENDIF
315900170619     C*** DSORT
316000170619     C                   IF        %trim(SkSplitFLD(i)) = 'DSORT'
316100170619     C                   EVAL      CR5DSORT = SkSplitCSV_9(i)
316200170619     C                   ENDIF
316300170619     C*
316400170619     C                   ENDIF
316500170619     C                   EVAL      i = i + 1
316600170619     C                   ENDDO
316700170619     C*
316800170619     C*** VER
316900170621     C                   EVAL      CR5VER = NEW_VERSION
317000170620     C*
317100170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
317200170620     C                   EVAL      InStringa = w2048_01
317300170620     C                   EVAL      InSepara = ','
317400170620     C                   SETOFF                                       131419
317500170620     C                   SETON                                        13
317600170620     C                   EXSR      SR_SPLIT
317700170620     C*
317800170620     C* Se nessun elemento => scrivo comunque un record singolo
317900170620     C                   IF        OutIdxMax = *zeros
318000170620     C                   WRITE     DPCR5100
318100170620     C                   ELSE
318200170620     C*
318300170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
318400170620     C                   Z-ADD     1             j                 4 0
318500170620     C                   DOW       j <= %elem(SkSplitCSV_3)
318600170620     C                             AND j <= OutIdxMax
318700170620     C*
318800170620     C* Determino il tipo di Routing Place
318900170620     C                   SELECT
319000170620     C* Injection Code - Range
319100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
319200170620     C                   EVAL      CR5ROUT  = 'I'
319300170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):3:9)
319400170620     C                   EVAL      CR5ROUPE = %subst(SkSplitCSV_3(j):12:9)
319500170620     C* Injection Code - Single
319600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
319700170620     C                   EVAL      CR5ROUT  = 'I'
319800170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:9)
319900170620     C                   EVAL      CR5ROUPE = CR5ROUPB
320000170620     C* Country Code
320100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
320200170620     C                   EVAL      CR5ROUT  = 'C'
320300170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:2)
320400170620     C                   EVAL      CR5ROUPE = CR5ROUPB
320500170620     C* Depot Code - Range
320600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
320700170620     C                   EVAL      CR5ROUT  = 'D'
320800170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):3:7)
320900170620     C                   EVAL      CR5ROUPE = %subst(SkSplitCSV_3(j):10:7)
321000170620     C* Depot Code - Single
321100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
321200170620     C                   EVAL      CR5ROUT  = 'D'
321300170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:7)
321400170620     C                   EVAL      CR5ROUPE = CR5ROUPB
321500170620     C* Depot Group
321600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
321700170620     C                   EVAL      CR5ROUT  = 'G'
321800170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:4)
321900170620     C                   EVAL      CR5ROUPE = CR5ROUPB
322000170620     C* Business Unit
322100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
322200170620     C                   EVAL      CR5ROUT  = 'B'
322300170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:3)
322400170620     C                   EVAL      CR5ROUPE = CR5ROUPB
322500170620     C*
322600170620     C                   ENDSL
322700170620     C*
322800170620     C* Infine inserisco il record corrente
322900170620     C                   WRITE     DPCR5100
323000170620     C*
323100170620     C                   EVAL      j = j + 1
323200170620     C                   ENDDO
323300170620     C                   ENDIF
323400170619     C*
323500170619     C                   ENDSR
323600170620     C***
323700170616
323800170616
323900141028     C*----------------------------------------------------*
324000141028     C*  CONTROLLO NUMERICITA' CAMPI
324100141028     C*----------------------------------------------------*
324200141028     C     CHKNUM        BEGSR
324300141028     C*
324400141028     C                   IF        PiDecChr = *blanks
324500141028     C                   EVAL      PiDecChr = CharNUM
324600141028     C                   ENDIF
324700141028     C*
324800141028     C                   callp     UBISNUM_Check(PiStr
324900141028     C                                          :PiDecChr
325000141028     C                                          :PiVal
325100141028     C                                          :PiNum
325200141028     C                                          :PiInt)
325300141028     C*
325400141028     C                   ENDSR
325500141028     C***
325600141028
325700141028
325800141028     C*----------------------------------------------------*
325900141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
326000141028     C*----------------------------------------------------*
326100141028     C     SR_SPLIT      BEGSR
326200141028     C*
326300141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
326400170620     C   13              EVAL      InTypeOut = '3'
326500170620     C   14              EVAL      InTypeOut = '4'
326600170620     C   19              EVAL      InTypeOut = '9'
326700170616     C                   CALL      'XSPLIT3'
326800141028     C                   PARM                    InStringa
326900141028     C                   PARM                    InSepara
327000170616     C                   PARM                    InTypeOut
327100141028     C                   PARM      ''            wSkParam
327200170620     C                   PARM                    OutIdxMax
327300141028     C                   PARM                    OutErrore
327400141028     C                   PARM                    OutMsg
327500170620     C   13              MOVEA     wSkParam      SkSplitCSV_3
327600170620     C   14              MOVEA     wSkParam      SkSplitCSV_4
327700170620     C   19              MOVEA     wSkParam      SkSplitCSV_9
327800141028     C*
327900141028     C                   ENDSR
328000160801
328100160801
328200160801     C*--------------------------------------------------------
328300160801     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
328400160801     C*--------------------------------------------------------
328500160801     C     CARTAB        BEGSR
328600160801     C*
328700160801     C* TABELLA '15' - NAZIONI
328800160801     C                   clear                   skNAZISO
328900160801     C                   clear                   skNAZBAR
329000160801     C                   eval      tblKUT = 1
329100160801     C                   eval      tblCOD = '15'
329200170616     C     KEYtab_P      setll     tabel00f
329300170616     C     KEYtab_P      reade     tabel00f
329400160801     C                   dow       not %eof(tabel00f)
329500160801     C                   if        tblFLG = *blanks
329600160801     C                   movel(p)  tblUNI        ds15
329700160801     C                   add       1             jNAZ
329800160801     C                   eval      skNAZBAR(jNAZ) = tblKEY
329900170620     C                   eval      skNAZISO(jNAZ) = %trim(§15COD)
330000160801     C                   endif
330100170616     C     KEYtab_P      reade     tabel00f
330200160801     C                   enddo
330300160801     C*
330400160801     C                   ENDSR
330500160801     C***
330600170622
330700170622
330800170622     C*--------------------------------------------------------------------------------------------*
330900170622     C* SNDALERT - Invio email di alert al assistenza IT
331000170622     C*--------------------------------------------------------------------------------------------*
331100170622     C     sndAlert      BEGSR
331200170622     C*
331300170622     C* Inizializzo variabili
331400170622     C                   movel     *blanks       wrkEml          253
331500170622     C                   movel     *blanks       wrkCcEml        253
331600170622     C                   movel     *blanks       wrkEmlMsg      5000
331700170622     C                   movel     *blanks       wrkEmlOgg        44
331800170622     C*
331900170622     C* Valorizzo i campi della e-m@ail
332000170622     C                   eval      wrkEml='cedalert@brt.it'
332100170622     C                   eval      wrkCcEml='fabrizio.gurrieri@brt.it'
332200170622     C                   eval      wrkEmlOgg=
332300170622     C                               'DPD GeoRDB 2017 - Importazione '         +
332400170622     C                               'VERSIONE: ' + %editc(NEW_VERSION:'X')
332500170622     C                   EVAL      wrkEmlMsg = 'Rilevata possibilità '         +
332600170622     C                             'troncamento dati al record: '              +
332700170622     C                                         %editc(currRCD:'4')             +
332800170622     C                                                                    ':/N'+
332900170622     C                                                                    ':/N'
333000170622     C*
333100170622     C                   call(e)   'TIS701C1'
333200170622     C                   parm                    wrkEml
333300170622     C                   parm                    wrkCcEml
333400170622     C                   parm                    wrkEmlOgg
333500170622     C                   parm                    wrkEmlMsg
333600170622     C                   parm                    wrkEsito          1
333700170622     C*
333800170622     C                   ENDSR
333900070411
334000990910
334100000613     C     *inzsr        BEGSR
334200000613     C*
334300170616     C* CALCOLA LA VARIE DATE DI RIFERIMENTO
334400170616     C                   z-add     *zeros        oggi              8 0
334500170616     C                   z-add     *zeros        domani            8 0
334600170616     C                   eval      oggi   = %dec(%date() : *ISO)
334700170616     C                   eval      domani = %dec(%date()+%days(1) : *ISO)
334800160801     C*
334900170616     C* Chiave su TABEL00F - Parziale
335000170616     C     KEYtab_P      KLIST
335100160801     C                   KFLD                    tblKUT
335200160801     C                   KFLD                    tblCOD
335300170616     C*
335400170616     C* Chiave su DPCCC00F - Completa
335500170616     C     KEYccc_11C    KLIST
335600170616     C                   KFLD                    cccATB
335700170616     C                   KFLD                    cccVER
335800170616     C                   KFLD                    cccISO2
335900170616     C*
336000170616     C* Chiave su DPCST00F - Completa
336100170616     C     KEYcst_11C    KLIST
336200170616     C                   KFLD                    cstATB
336300170616     C                   KFLD                    cstVER
336400170616     C                   KFLD                    cstISO2
336500170616     C                   KFLD                    cstREGC
336600170616     C                   KFLD                    cstSTAC
336700170616     C*
336800170616     C* Chiave su DPCEL00F - Completa
336900170616     C     KEYcel_11C    KLIST
337000170616     C                   KFLD                    celATB
337100170616     C                   KFLD                    celVER
337200170616     C                   KFLD                    celELMC
337300170619     C*
337400170619     C* Chiave su DPCSO00F - Completa
337500170619     C     KEYcso_11C    KLIST
337600170619     C                   KFLD                    csoATB
337700170619     C                   KFLD                    csoVER
337800170619     C                   KFLD                    csoSORC
337900000830     C*
338000000613     C                   ENDSR
338100000613     C***
