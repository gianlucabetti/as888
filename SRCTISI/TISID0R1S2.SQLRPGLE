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
003700170621     D wDESxx          S            128    VARYING INZ
003800170621     D wCSTSRTC        S                   LIKE(CSTSRTC)  INZ
003900170621     D w2048_01        S           2048    VARYING INZ
004000170621     D w2048_02        S           2048    VARYING INZ
004100170621     D w2048_03        S           2048    VARYING INZ
004200170621     D w2048_04        S           2048    VARYING INZ
004300170621     D sOutIdxMax_w1   S                   LIKE(OutIdxMax) INZ
004400170621     D sOutIdxMax_w2   S                   LIKE(OutIdxMax) INZ
004500170621     D sOutIdxMax_w3   S                   LIKE(OutIdxMax) INZ
004600170621     D sOutIdxMax_w4   S                   LIKE(OutIdxMax) INZ
004700170621     D wSORC           S              3    INZ
004800170621     D wSkCSV_03_w1    S             64    DIM(1024) INZ
004900170621     D wSkCSV_03_w2    S             64    DIM(1024) INZ
005000170621     D wSkCSV_03_w3    S             64    DIM(1024) INZ
005100170621     D wSkCSV_03_w4    S             64    DIM(1024) INZ
005200170616
005300170616     D*------------
005400170616     D skDEFKEY        S             64    DIM(128)
005500170620     D DS_DEF          DS
005600170620     D   skDEF                       64    DIM(128)
005700170620     D skDEFS          S                   DIM(128) LIKE(DS_DEF)
005800170616     D jDEFS           S              3  0 INZ
005900160606
006000160801     D*------------
006100170620     D jNAZ            s              4  0 INZ(*zeros)
006200170620     D skNAZISO        S              2    DIM(1000)
006300160801     D skNAZBAR        S              3    DIM(1000)
006400160606
006500141028
006600141028     D*------------------
006700141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006800141028     D*------------------
006900170621     D CharCSV         S              1    INZ(';')
007000170621     D CharNUM         S              1    INZ(',')
007100141028
007200141028
007300141028     D*------------------
007400141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
007500141028     D*------------------
007600170616     D InStringa       S         131072A   VARYING                              (stringa input)
007700170616     D InSepara        S             10A                                        (separatore)
007800170616     D InTypeOut       S              2A                                        (tipo output)
007900170616     D wSkParam        S         131072A                                        (output)
008000170620     D OutIdxMax       S              4S 0                                      (max indice valoriz)
008100170616     D OutErrore       S              1A                                        (flag errore)
008200170616     D OutMsg          S             80A                                        (messaggio errore)
008300170620     D SkSplitCSV_3    S             64    DIM(1024)
008400170616     D SkSplitCSV_4    S            128    DIM(512)
008500170616     D SkSplitCSV_9    S           2048    DIM(64)
008600990908
008700141028
008800141028     D*------------------
008900141028     D* LINKING A DEFINIZIONI ESTERNE
009000141028     D*------------------
009100141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009200141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009300141028
009400141028
009500010201
009600000613     C*
009700160801     C                   EXSR      CARTAB                                       CARICA TABELLE
009800170616     C                   EXSR      RWFILE                                       ELABORAZIONE
009900000616     C*
010000010201     C                   seton                                        LR
010100990908
010200000801
010300040526     C     RWFILE        BEGSR
010400170620     C*
010500170621     C                   if        not %open(WFDPDRDBF)
010600170621     C                   open      WFDPDRDBF
010700170620     C                   endif
010800150127     C*
010900170621     C                   READ      WFDPDRDBF
011000170621     C                   DOW       not %eof(WFDPDRDBF)
011100000801     C*
011200170621     C                   if        WDPDRDB <> *blanks
011300141028     C*
011400170616     C* "normalizzo" il dato di input corrente
011500170621     C                   eval      wWDPDRDB = %trim(%subst(WDPDRDB:1:
011600170621     C                                        %len(%trim(WDPDRDB))-1))
011700040910     C*
011800170616     C* Eseguo routine di traduzione
011900170616     C                   exsr      exeTrad
012000040802     C*
012100000905     C                   endif
012200000905     C*
012300170621     C***                DELETE    WFDPDRDB0
012400170621     C                   READ      WFDPDRDBF
012500170621     C                   ENDDO
012600170620     C*
012700170621     C                   if        %open(WFDPDRDBF)
012800170621     C                   close     WFDPDRDBF
012900170620     C                   endif
013000990920     C*
013100910830     C                   ENDSR
013200000613     C***
013300170616
013400140922
013500141028     C*----------------------------------------------------*
013600141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
013700141028     C*----------------------------------------------------*
013800141028     C     INZVAR        BEGSR
013900141028     C*
014000141028     C* Inizializzo variabili di wrk
014100170620     C                   SETOFF                                       131419
014200170620     C                   CLEAR                   SkSplitCSV_3
014300170616     C                   CLEAR                   SkSplitCSV_4
014400170616     C                   CLEAR                   SkSplitCSV_9
014500170621     C                   CLEAR                   wDESxx
014600170620     C                   CLEAR                   wCSTSRTC
014700170620     C                   CLEAR                   w2048_01
014800170621     C                   CLEAR                   w2048_02
014900170621     C                   CLEAR                   w2048_03
015000170621     C                   CLEAR                   w2048_04
015100141028     C*
015200141028     C                   ENDSR
015300170616     C***
015400170616
015500170616
015600000801     C*----------------------------------------------------*
015700170616     C*  TRADUZIONE DATI
015800000801     C*----------------------------------------------------*
015900170616     C     EXETRAD       BEGSR
016000170616     C*
016100170616     C* Inizializzazioni a liverro di singolo record
016200170616     C                   EXSR      INZVAR
016300170616     C*
016400170616     C* Ignoro i record relativi al "file"
016500170616     C                   SELECT
016600170621     C                   WHEN      %subst(wWDPDRDB:1:1)  = '#' AND
016700170621     C                             %subst(wWDPDRDB:1:4) <> '#DEF'
016800170616     C*
016900170616     C* Carico innanzitutto tutti i record di DEFinizione
017000170621     C                   WHEN      %subst(wWDPDRDB:1:16) = '#DEF:GEOROUTING:'
017100170621     C                   EVAL      InStringa = %subst(wWDPDRDB:16+1)
017200170616     C                   EVAL      InSepara = CharCSV
017300170620     C                   SETOFF                                       131419
017400170616     C                   SETON                                        14
017500170616     C                   EXSR      SR_SPLIT
017600170616     C                   EVAL      skDEF = %subarr(SkSplitCSV_4:1)
017700170616     C                   ADD       1             jDEFS
017800170616     C                   EVAL      skDEFKEY(jDEFS) = skDEF(1)
017900170620     C                   EVAL      skDEFS(jDEFS)   = DS_DEF
018000170620     C*
018100170616     C* Elaboro i record di dati
018200170616     C                   OTHER
018300170616     C*
018400170616     C* Splitto il record di dati corrrente
018500170621     C                   EVAL      InStringa = %trim(wWDPDRDB)
018600170616     C                   EVAL      InSepara = CharCSV
018700170620     C                   SETOFF                                       131419
018800170616     C                   SETON                                        19
018900170616     C                   EXSR      SR_SPLIT
019000170616     C*
019100170616     C* Ogni record di dati lo "mappo" tramite la propria struttura dati
019200170616     C                   EXSR      MAPPADATI
019300170616     C*
019400170616     C                   ENDSL
019500170616     C*
019600170616     C                   ENDSR
019700170616     C***
019800170616
019900170616
020000170616     C*----------------------------------------------------*
020100170616     C*  GESTIONE MAPPATURE DATI
020200170616     C*----------------------------------------------------*
020300170616     C     MAPPADATI     BEGSR
020400170616     C*
020500170616     C* Verifico che il tipo record dati corrente risulti già mappato
020600170616     C                   EVAL      currMAP = %trim(SkSplitCSV_9(1))
020700170616     C                   EVAL      jDEFS = %lookup(currMAP:skDEFKEY:1)
020800170616     C*
020900170616     C* Se tipo record dati corrente già mappato => carico relativa mappatura
021000170616     C                   IF        jDEFS > *zeros
021100170620     C                   EVAL      DS_DEF = skDEFS(jDEFS)
021200170616     C                   EVAL      SkSplitFLD = %subarr(skDEF:1)
021300170616     C*
021400170616     C* Rispetto ad ogni tipo record dati => gestisco con relativa mappatura
021500170616     C                   SELECT
021600170616     C                   WHEN      currMAP ='VERSION'
021700170616     C                   EXSR      gesCVE
021800170616     C*
021900170616     C                   WHEN      currMAP ='COUNTRY'
022000170616     C                   EXSR      gesCCC
022100170616     C*
022200170620     C***                WHEN      currMAP ='COUNTRYTRANSLATION'
022300170619     C***                EXSR      gesXXX
022400170616     C*
022500170616     C                   WHEN      currMAP ='COUNTRY.IT'
022600170616     C                   EXSR      gesCCC_IT
022700170616     C*
022800170616     C                   WHEN      currMAP ='COUNTRY.DE'
022900170616     C                   EXSR      gesCCC_DE
023000170616     C*
023100170616     C                   WHEN      currMAP ='COUNTRY.FR'
023200170616     C                   EXSR      gesCCC_FR
023300170616     C*
023400170616     C                   WHEN      currMAP ='COUNTRY.EN'
023500170616     C                   EXSR      gesCCC_EN
023600170616     C*
023700170616     C                   WHEN      currMAP ='BUSINESSUNIT'
023800170616     C                   EXSR      gesCBU
023900170616     C*
024000170616     C                   WHEN      currMAP ='POSTALCODE'
024100170616     C                   EXSR      gesCPC
024200170616     C*
024300170620     C***                WHEN      currMAP ='STATE'
024400170619     C***                EXSR      gesCST
024500170616     C*
024600170620     C***                WHEN      currMAP ='STATE.IT'
024700170619     C***                EXSR      gesCST_IT
024800170616     C*
024900170620     C***                WHEN      currMAP ='STATE.DE'
025000170619     C***                EXSR      gesCST_DE
025100170616     C*
025200170620     C***                WHEN      currMAP ='STATE.FR'
025300170619     C***                EXSR      gesCST_FR
025400170616     C*
025500170620     C***                WHEN      currMAP ='STATE.EN'
025600170619     C***                EXSR      gesCST_EN
025700170616     C*
025800170616     C                   WHEN      currMAP ='DEPOTS'
025900170619     C                   EXSR      gesCDP
026000170616     C*
026100170620     C***                WHEN      currMAP ='ELEMENTS'
026200170619     C***                EXSR      gesCEL
026300170616     C*
026400170620     C***                WHEN      currMAP ='ELEMENTS.IT'
026500170619     C***                EXSR      gesCEL_IT
026600170616     C*
026700170620     C***                WHEN      currMAP ='ELEMENTS.EN'
026800170619     C***                EXSR      gesCEL_EN
026900170616     C*
027000170620     C***                WHEN      currMAP ='ELEMENTS.DE'
027100170619     C***                EXSR      gesCEL_DE
027200170616     C*
027300170620     C***                WHEN      currMAP ='ELEMENTS.FR'
027400170619     C***                EXSR      gesCEL_FR
027500170616     C*
027600170620     C***                WHEN      currMAP ='ELEMENTS.PT'
027700170619     C***                EXSR      gesCEL_PT
027800170619     C*
027900170616     C                   WHEN      currMAP ='SOCODE'
028000170616     C                   EXSR      gesCSO
028100170616     C*
028200170616     C                   WHEN      currMAP ='ASCODE'
028300170619     C                   EXSR      gesCAS
028400170616     C*
028500170616     C                   WHEN      currMAP ='IC'
028600170619     C                   EXSR      gesCIC
028700170616     C*
028800170616     C                   WHEN      currMAP ='NETWORK'
028900170619     C                   EXSR      gesCNT
029000170616     C*
029100170616     C                   WHEN      currMAP ='NETWORKPLAN'
029200170616     C                   EXSR      gesCNP
029300170616     C*
029400170620     C***                WHEN      currMAP ='RCODE'
029500170620     C***                EXSR      gesXXX
029600170616     C*
029700170616     C                   WHEN      currMAP ='ALLOWSO'
029800170619     C                   EXSR      gesCWS
029900170616     C*
030000170616     C                   WHEN      currMAP ='ALLOWAS'
030100170619     C                   EXSR      gesCWA
030200170619     C*
030300170619     C                   WHEN      currMAP ='R1DBU'
030400170619     C                   EXSR      gesCR1
030500170619     C*
030600170619     C                   WHEN      currMAP ='R2ROUTE'
030700170619     C                   EXSR      gesCR2
030800170619     C*
030900170619     C                   WHEN      currMAP ='R3OSORT'
031000170619     C                   EXSR      gesCR3
031100170619     C*
031200170619     C                   WHEN      currMAP ='R4CSORT'
031300170620     C                   EXSR      gesCR4
031400170619     C*
031500170619     C                   WHEN      currMAP ='R5DSORT'
031600170619     C                   EXSR      gesCR5
031700170616     C*
031800170620     C***                WHEN      currMAP ='DEPARTURE'
031900170619     C***                EXSR      gesXXX
032000170616     C*
032100170616     C                   WHEN      currMAP ='RELATION'
032200170619     C                   EXSR      gesCRE
032300170616     C*
032400170616     C                   WHEN      currMAP ='SOSERVICEINFO.IT'
032500170616     C                   EXSR      gesCSO_IT
032600170616     C*
032700170616     C                   WHEN      currMAP ='SOSERVICEINFO.EN'
032800170616     C                   EXSR      gesCSO_EN
032900170616     C*
033000170616     C                   WHEN      currMAP ='SOSERVICEINFO.DE'
033100170616     C                   EXSR      gesCSO_DE
033200170616     C*
033300170616     C                   WHEN      currMAP ='SOSERVICEINFO.FR'
033400170616     C                   EXSR      gesCSO_FR
033500170616     C*
033600170616     C                   WHEN      currMAP ='SOSERVICEINFO.ET'
033700170616     C                   EXSR      gesCSO_ET
033800170616     C*
033900170616     C                   WHEN      currMAP ='SOSERVICEINFO.CS'
034000170616     C                   EXSR      gesCSO_CS
034100170616     C*
034200170616     C                   ENDSL
034300170616     C*
034400170616     C                   ENDIF
034500170616     C*
034600170616     C                   ENDSR
034700170616     C***
034800170616
034900170616
035000170616     C*----------------------------------------------------*
035100170616     C*  GESTIONE MAPPATURA "VERSION"
035200170616     C*----------------------------------------------------*
035300170616     C     GESCVE        BEGSR
035400170616     C*
035500170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
035600170616     C                   CLEAR                   DPCVE100
035700170616     C*
035800170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
035900170616     C                   Z-ADD     2             i                 4 0
036000170620     C                   DOW       i <= %elem(SkSplitFLD)
036100170620     C                             AND i <= OutIdxMax
036200170616     C*
036300170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
036400170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
036500170616     C                             SkSplitCSV_9(i) <> *blanks
036600170616     C*
036700170616     C*** VERD
036800170616     C                   IF        %trim(SkSplitFLD(i)) = 'VERSIONNUMBER'
036900170616     C                   EVAL      CVEVERD = SkSplitCSV_9(i)
037000170616     C                   ENDIF
037100170616     C*** DDE
037200170616     C                   IF        %trim(SkSplitFLD(i)) = 'VALIDFROM'
037300170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
037400170616     C                   EXSR      CHKNUM
037500170616     C                   IF        PiInt=*on
037600170616     C                   Z-ADD     PiVal         CVEDDE
037700170616     C                   ELSE
037800170616     C                   dump(A)
037900170616     C                   return
038000170616     C                   ENDIF
038100170616     C                   ENDIF
038200170616     C*** DSC
038300170616     C                   IF        %trim(SkSplitFLD(i)) = 'VALIDTO'
038400170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
038500170616     C                   EXSR      CHKNUM
038600170616     C                   IF        PiInt=*on
038700170616     C                   Z-ADD     PiVal         CVEDSC
038800170616     C                   ELSE
038900170616     C                   dump(A)
039000170616     C                   return
039100170616     C                   ENDIF
039200170616     C                   ENDIF
039300170616     C*** BCID
039400170616     C                   IF        %trim(SkSplitFLD(i)) = 'BARCODEID'
039500170616     C                   EVAL      CVEVERD = SkSplitCSV_9(i)
039600170616     C                   ENDIF
039700170616     C*
039800170616     C                   ENDIF
039900170616     C                   EVAL      i = i + 1
040000170616     C                   ENDDO
040100170616     C*
040200170616     C*** DIM
040300170616     C                   EVAL      CVEDIM = oggi
040400170616     C*
040500170616     C* Reperisco la versione più alta corrente
040600170616     C*
040700170616     C/EXEC SQL
040800170616     C+ SET :MAX_VERSION = (SELECT MAX(CVEVER) FROM DPCVE10F)
040900170616     C/END-EXEC
041000170616     C*
041100170616     C* Stacco il nuovo numero versione BRT come incremento della versione più alta presente
041200170616     C                   EVAL      NEW_VERSION = MAX_VERSION + 1
041300170616     C*
041400170616     C* Effetttuo considerazioni si "sicurezza" sulle date di validità della versione DPD
041500170616     C                   IF        CVEDDE < domani
041600170616     C                   EVAL      CVEDDE = domani
041700170616     C                   ENDIF
041800170616     C                   IF        CVEDSC < domani
041900170616     C                   EVAL      CVEDSC = domani
042000170616     C                   ENDIF
042100170616     C*
042200170616     C* Annullo tutte le versioni che decorrono
042300170616     C* nel range di validità della versione DPD corrente
042400170616     C*
042500170616     C/EXEC SQL
042600170616     C+ UPDATE DPCVE10F SET CVEATB = 'A'
042700170616     C+ WHERE CVEDDE BETWEEN :CVEDDE AND :CVEDSC
042800170616     C/END-EXEC
042900170616     C*
043000170616     C* Faccio scadere a oggi tutte le versioni aventi data scadenza
043100170616     C* nel range di validità della versione DPD corrente
043200170616     C*
043300170616     C/EXEC SQL
043400170616     C+ UPDATE DPCVE10F SET CVEDSC = :oggi
043500170616     C+ WHERE CVEDSC BETWEEN :CVEDDE AND :CVEDSC
043600170616     C/END-EXEC
043700170616     C*
043800170620     C* Quindi lancio driver di "allineamento validità versioni DPD GeoRDB 2017"
043900170616     C*
044000170620     C                   CALL      'TISID3R1'
044100170620     C*
044200170620     C*** VER
044300170620     C                   EVAL      CVEVER = NEW_VERSION
044400170616     C*
044500170616     C* Infine inserisco il record corrente
044600170616     C                   WRITE     DPCVE100
044700170616     C*
044800170616     C                   ENDSR
044900170616     C***
045000170616
045100170616
045200170616     C*----------------------------------------------------*
045300170616     C*  GESTIONE MAPPATURA "COUNTRY"
045400170616     C*----------------------------------------------------*
045500170616     C     GESCCC        BEGSR
045600170616     C*
045700170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
045800170616     C                   CLEAR                   DPCCC100
045900170616     C*
046000170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
046100170616     C                   Z-ADD     2             i                 4 0
046200170620     C                   DOW       i <= %elem(SkSplitFLD)
046300170620     C                             AND i <= OutIdxMax
046400170616     C*
046500170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
046600170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
046700170616     C                             SkSplitCSV_9(i) <> *blanks
046800170616     C*
046900170616     C*** ISO2
047000170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
047100170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
047200170616     C                   ENDIF
047300170616     C*** ISON
047400170616     C                   IF        %trim(SkSplitFLD(i)) = 'ISONUMCOUNTRYCODE'
047500170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
047600170616     C                   EXSR      CHKNUM
047700170616     C                   IF        PiInt=*on
047800170616     C                   Z-ADD     PiVal         CCCISON
047900170616     C                   ENDIF
048000170616     C                   ENDIF
048100170616     C*** ISO3
048200170616     C                   IF        %trim(SkSplitFLD(i)) = 'ISOALPHA3COUNTRYCODE'
048300170616     C                   EVAL      CCCISO3 = SkSplitCSV_9(i)
048400170616     C                   ENDIF
048500170616     C*** PTC
048600170616     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGPOSTCODENO'
048700170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
048800170616     C                   EXSR      CHKNUM
048900170616     C                   IF        PiInt=*on
049000170616     C                   Z-ADD     PiVal         CCCPTC
049100170616     C                   ENDIF
049200170616     C                   ENDIF
049300170616     C*** PATT
049400170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODEPATTERN'
049500170620     C                   EVAL      CCCPATTL = SkSplitCSV_9(i)
049600170616     C                   ENDIF
049700170616     C*** FNS
049800170616     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGNEEDSTATE'
049900170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
050000170616     C                   EXSR      CHKNUM
050100170616     C                   IF        PiInt=*on
050200170616     C                   Z-ADD     PiVal         CCCFNS
050300170616     C                   ENDIF
050400170616     C                   ENDIF
050500170616     C*** PVAL
050600170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODEVALIDATION'
050700170616     C                   EVAL      CCCPVAL = SkSplitCSV_9(i)
050800170616     C                   ENDIF
050900170616     C*** CVAL
051000170616     C                   IF        %trim(SkSplitFLD(i)) = 'CITYVALIDATION'
051100170616     C                   EVAL      CCCCVAL = SkSplitCSV_9(i)
051200170616     C                   ENDIF
051300170616     C*** FREE
051400170616     C                   IF        %trim(SkSplitFLD(i)) = 'FREETRADEAREA'
051500170616     C                   EVAL      CCCFREE = SkSplitCSV_9(i)
051600170616     C                   ENDIF
051700170616     C*** DUMY
051800170616     C                   IF        %trim(SkSplitFLD(i)) = 'DUMMYCOUNTRYCODE'
051900170616     C                   EVAL      CCCDUMY = SkSplitCSV_9(i)
052000170616     C                   ENDIF
052100170616     C*
052200170616     C                   ENDIF
052300170616     C                   EVAL      i = i + 1
052400170616     C                   ENDDO
052500170616     C*
052600170616     C*** VER
052700170616     C                   EVAL      CCCVER = NEW_VERSION
052800170616     C*** BRT
052900170620     C                   Z-ADD     1             jNAZ
053000170620     C     CCCISO2       LOOKUP    skNAZISO(jNAZ)                         55
053100170616     C                   IF        %found
053200170616     C                   EVAL      CCCBRT = skNAZBAR(jNAZ)
053300170616     C                   ENDIF
053400170616     C*
053500170616     C* Infine inserisco il record corrente
053600170616     C                   WRITE     DPCCC100
053700170616     C*
053800170616     C                   ENDSR
053900170616     C***
054000170616
054100170616
054200170616     C*----------------------------------------------------*
054300170616     C*  GESTIONE MAPPATURA "COUNTRY.IT"
054400170616     C*----------------------------------------------------*
054500170616     C     GESCCC_IT     BEGSR
054600170616     C*
054700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
054800170616     C                   Z-ADD     2             i                 4 0
054900170620     C                   DOW       i <= %elem(SkSplitFLD)
055000170620     C                             AND i <= OutIdxMax
055100170616     C*
055200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
055300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
055400170616     C                             SkSplitCSV_9(i) <> *blanks
055500170616     C*
055600170616     C*** ISO2
055700170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
055800170616     C                   EVAL      CCCISO2 = SkSplitCSV_9(i)
055900170616     C                   ENDIF
056000170616     C*** NAME
056100170616     C                   IF        %trim(SkSplitFLD(i)) = 'NAME'
056200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
056300170616     C                   ENDIF
056400170616     C*
056500170616     C                   ENDIF
056600170616     C                   EVAL      i = i + 1
056700170616     C                   ENDDO
056800170616     C*
056900170616     C*** ATB
057000170616     C                   EVAL      CCCATB = *blanks
057100170616     C*** VER
057200170616     C                   EVAL      CCCVER = NEW_VERSION
057300170616     C*
057400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
057500170621     C                   IF        wDESxx <> *blanks
057600170616     C     KEYccc_11C    CHAIN     dpccc11i
057700170616     C                   IF        %found(dpccc11i)
057800170621     C                   EVAL      CCCDESIT = wDESxx
057900170616     C                   UPDATE    DPCCC100
058000170616     C                   ENDIF
058100170620     C                   ENDIF
058200170616     C*
058300170616     C                   ENDSR
058400170616     C***
058500170616
058600170616
058700170616     C*----------------------------------------------------*
058800170616     C*  GESTIONE MAPPATURA "COUNTRY.DE"
058900170616     C*----------------------------------------------------*
059000170616     C     GESCCC_DE     BEGSR
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
062300170621     C                   EVAL      CCCDESDE = wDESxx
062400170616     C                   UPDATE    DPCCC100
062500170620     C                   ENDIF
062600170616     C                   ENDIF
062700170616     C*
062800170616     C                   ENDSR
062900170616     C***
063000170616
063100170616
063200170616     C*----------------------------------------------------*
063300170616     C*  GESTIONE MAPPATURA "COUNTRY.FR"
063400170616     C*----------------------------------------------------*
063500170616     C     GESCCC_FR     BEGSR
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
066800170621     C                   EVAL      CCCDESFR = wDESxx
066900170616     C                   UPDATE    DPCCC100
067000170616     C                   ENDIF
067100170620     C                   ENDIF
067200170616     C*
067300170616     C                   ENDSR
067400170616     C***
067500170616
067600170616
067700170616     C*----------------------------------------------------*
067800170616     C*  GESTIONE MAPPATURA "COUNTRY.EN"
067900170616     C*----------------------------------------------------*
068000170616     C     GESCCC_EN     BEGSR
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
071300170621     C                   EVAL      CCCDESEN = wDESxx
071400170616     C                   UPDATE    DPCCC100
071500170616     C                   ENDIF
071600170620     C                   ENDIF
071700170616     C*
071800170616     C                   ENDSR
071900170616     C***
072000170616
072100170616
072200170616     C*----------------------------------------------------*
072300170616     C*  GESTIONE MAPPATURA "BUSINESSUNIT"
072400170616     C*----------------------------------------------------*
072500170616     C     GESCBU        BEGSR
072600170616     C*
072700170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
072800170616     C                   CLEAR                   DPCBU100
072900170616     C*
073000170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
073100170616     C                   Z-ADD     2             i                 4 0
073200170620     C                   DOW       i <= %elem(SkSplitFLD)
073300170620     C                             AND i <= OutIdxMax
073400170616     C*
073500170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
073600170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
073700170616     C                             SkSplitCSV_9(i) <> *blanks
073800170616     C*
073900170616     C*** BUCA
074000170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUALPHASTR'
074100170616     C                   EVAL      CBUBUCA = SkSplitCSV_9(i)
074200170616     C                   ENDIF
074300170616     C*** BUCN
074400170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
074500170616     C                   EVAL      PiStr = SkSplitCSV_9(i)
074600170616     C                   EXSR      CHKNUM
074700170616     C                   IF        PiInt=*on
074800170616     C                   Z-ADD     PiVal         CBUBUCN
074900170616     C                   ENDIF
075000170616     C                   ENDIF
075100170616     C*** NAME
075200170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUNAME'
075300170616     C                   EVAL      CBUNAME = SkSplitCSV_9(i)
075400170616     C                   ENDIF
075500170616     C*** ISO2
075600170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUDEFAULTCOUNTRY'
075700170616     C                   EVAL      CBUISO2 = SkSplitCSV_9(i)
075800170616     C                   ENDIF
075900170616     C*** TYPE
076000170616     C                   IF        %trim(SkSplitFLD(i)) = 'TYPEOFBU'
076100170616     C                   EVAL      CBUTYPE = SkSplitCSV_9(i)
076200170616     C                   ENDIF
076300170616     C*** GRPF
076400170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUFINANCIALGROUP'
076500170616     C                   EVAL      CBUGRPF = SkSplitCSV_9(i)
076600170616     C                   ENDIF
076700170616     C*** GRPG
076800170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUGEOGRAPHGROUP'
076900170616     C                   EVAL      CBUGRPG = SkSplitCSV_9(i)
077000170616     C                   ENDIF
077100170616     C*** GRPT
077200170616     C                   IF        %trim(SkSplitFLD(i)) = 'BUTECHNICALGROUP'
077300170616     C                   EVAL      CBUGRPT = SkSplitCSV_9(i)
077400170616     C                   ENDIF
077500170616     C*** DPDC
077600170616     C                   IF        %trim(SkSplitFLD(i)) = 'DPDCONTRACT'
077700170616     C                   EVAL      CBUDPDC = SkSplitCSV_9(i)
077800170616     C                   ENDIF
077900170616     C*
078000170616     C                   ENDIF
078100170616     C                   EVAL      i = i + 1
078200170616     C                   ENDDO
078300170616     C*
078400170616     C*** VER
078500170616     C                   EVAL      CBUVER = NEW_VERSION
078600170616     C*
078700170616     C* Infine inserisco il record corrente
078800170616     C                   WRITE     DPCBU100
078900170616     C*
079000170616     C                   ENDSR
079100170616     C***
079200170616
079300170616
079400170616     C*----------------------------------------------------*
079500170616     C*  GESTIONE MAPPATURA "POSTALCODE"
079600170616     C*----------------------------------------------------*
079700170616     C     GESCPC        BEGSR
079800170616     C*
079900170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
080000170616     C                   CLEAR                   DPCPC100
080100170616     C*
080200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
080300170616     C                   Z-ADD     2             i                 4 0
080400170620     C                   DOW       i <= %elem(SkSplitFLD)
080500170620     C                             AND i <= OutIdxMax
080600170616     C*
080700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
080800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
080900170616     C                             SkSplitCSV_9(i) <> *blanks
081000170616     C*
081100170616     C*** ISO2
081200170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
081300170616     C                   EVAL      CPCISO2 = SkSplitCSV_9(i)
081400170616     C                   ENDIF
081500170616     C*** PTCB
081600170616     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
081700170616     C                   EVAL      CPCPTCB = SkSplitCSV_9(i)
081800170616     C                   ENDIF
081900170616     C*** PTCE
082000170616     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
082100170616     C                   EVAL      CPCPTCE = SkSplitCSV_9(i)
082200170616     C                   ENDIF
082300170616     C*** PTCT
082400170616     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODETYPE'
082500170620     C                   EVAL      CPCPTCT = SkSplitCSV_9(i)
082600170616     C                   ENDIF
082700170616     C*
082800170616     C                   ENDIF
082900170616     C                   EVAL      i = i + 1
083000170616     C                   ENDDO
083100170619     C*
083200170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
083300170619     C                   IF        CPCPTCE = *blanks
083400170619     C                   EVAL      CPCPTCE = CPCPTCB
083500170619     C                   ENDIF
083600170616     C*
083700170616     C*** VER
083800170616     C                   EVAL      CPCVER = NEW_VERSION
083900170616     C*
084000170616     C* Infine inserisco il record corrente
084100170616     C                   WRITE     DPCPC100
084200170616     C*
084300170616     C                   ENDSR
084400170616     C***
084500170616
084600170616
084700170616     C*----------------------------------------------------*
084800170616     C*  GESTIONE MAPPATURA "STATE"
084900170616     C*----------------------------------------------------*
085000170616     C     GESCST        BEGSR
085100170616     C*
085200170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
085300170616     C                   CLEAR                   DPCST100
085400170616     C*
085500170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
085600170616     C                   Z-ADD     2             i                 4 0
085700170620     C                   DOW       i <= %elem(SkSplitFLD)
085800170620     C                             AND i <= OutIdxMax
085900170616     C*
086000170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086100170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
086200170616     C                             SkSplitCSV_9(i) <> *blanks
086300170616     C*
086400170616     C*** ISO2
086500170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
086600170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
086700170616     C                   ENDIF
086800170616     C*** REGC
086900170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODE'
087000170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
087100170616     C                   ENDIF
087200170616     C*** STAC
087300170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
087400170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
087500170616     C                   ENDIF
087600170616     C*
087700170616     C                   ENDIF
087800170616     C                   EVAL      i = i + 1
087900170616     C                   ENDDO
088000170616     C*
088100170616     C*** VER
088200170616     C                   EVAL      CSTVER = NEW_VERSION
088300170616     C*
088400170616     C* Infine inserisco il record corrente
088500170616     C                   WRITE     DPCST100
088600170616     C*
088700170616     C                   ENDSR
088800170616     C***
088900170616
089000170616
089100170616     C*----------------------------------------------------*
089200170616     C*  GESTIONE MAPPATURA "STATE.IT"
089300170616     C*----------------------------------------------------*
089400170616     C     GESCST_IT     BEGSR
089500170616     C*
089600170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089700170616     C                   Z-ADD     2             i                 4 0
089800170620     C                   DOW       i <= %elem(SkSplitFLD)
089900170620     C                             AND i <= OutIdxMax
090000170616     C*
090100170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090200170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
090300170616     C                             SkSplitCSV_9(i) <> *blanks
090400170616     C*
090500170616     C*** ISO2
090600170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
090700170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
090800170616     C                   ENDIF
090900170616     C*** REGC
091000170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
091100170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
091200170616     C                   ENDIF
091300170616     C*** STAC
091400170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
091500170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
091600170616     C                   ENDIF
091700170616     C*** SRTC
091800170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
091900170616     C                   ENDIF
092000170616     C*** DESxx
092100170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
092200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
092300170616     C                   ENDIF
092400170616     C*
092500170616     C                   ENDIF
092600170616     C                   EVAL      i = i + 1
092700170616     C                   ENDDO
092800170616     C*
092900170616     C*** ATB
093000170616     C                   EVAL      CSTATB = *blanks
093100170616     C*** VER
093200170616     C                   EVAL      CSTVER = NEW_VERSION
093300170616     C*
093400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
093500170621     C                   IF        wDESxx <> *blanks
093600170616     C     KEYcst_11C    CHAIN     dpCST11i
093700170616     C                   IF        %found(dpCST11i)
093800170621     C                   EVAL      CSTDESIT = wDESxx
093900170616     C                   UPDATE    DPCST100
094000170616     C                   ENDIF
094100170620     C                   ENDIF
094200170616     C*
094300170616     C                   ENDSR
094400170616     C***
094500170616
094600170616
094700170616     C*----------------------------------------------------*
094800170616     C*  GESTIONE MAPPATURA "STATE.DE"
094900170616     C*----------------------------------------------------*
095000170616     C     GESCST_DE     BEGSR
095100170616     C*
095200170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095300170616     C                   Z-ADD     2             i                 4 0
095400170620     C                   DOW       i <= %elem(SkSplitFLD)
095500170620     C                             AND i <= OutIdxMax
095600170616     C*
095700170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095800170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
095900170616     C                             SkSplitCSV_9(i) <> *blanks
096000170616     C*
096100170616     C*** ISO2
096200170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
096300170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
096400170616     C                   ENDIF
096500170616     C*** REGC
096600170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
096700170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
096800170616     C                   ENDIF
096900170616     C*** STAC
097000170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
097100170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
097200170616     C                   ENDIF
097300170616     C*** SRTC
097400170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
097500170616     C                   ENDIF
097600170616     C*** DESxx
097700170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
097800170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
097900170616     C                   ENDIF
098000170616     C*
098100170616     C                   ENDIF
098200170616     C                   EVAL      i = i + 1
098300170616     C                   ENDDO
098400170616     C*
098500170616     C*** ATB
098600170616     C                   EVAL      CSTATB = *blanks
098700170616     C*** VER
098800170616     C                   EVAL      CSTVER = NEW_VERSION
098900170616     C*
099000170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
099100170621     C                   IF        wDESxx <> *blanks
099200170616     C     KEYcst_11C    CHAIN     dpCST11i
099300170616     C                   IF        %found(dpCST11i)
099400170621     C                   EVAL      CSTDESDE = wDESxx
099500170616     C                   UPDATE    DPCST100
099600170616     C                   ENDIF
099700170620     C                   ENDIF
099800170616     C*
099900170616     C                   ENDSR
100000170616     C***
100100170616
100200170616
100300170616     C*----------------------------------------------------*
100400170616     C*  GESTIONE MAPPATURA "STATE.FR"
100500170616     C*----------------------------------------------------*
100600170616     C     GESCST_FR     BEGSR
100700170616     C*
100800170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100900170616     C                   Z-ADD     2             i                 4 0
101000170620     C                   DOW       i <= %elem(SkSplitFLD)
101100170620     C                             AND i <= OutIdxMax
101200170616     C*
101300170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101400170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
101500170616     C                             SkSplitCSV_9(i) <> *blanks
101600170616     C*
101700170616     C*** ISO2
101800170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
101900170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
102000170616     C                   ENDIF
102100170616     C*** REGC
102200170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
102300170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
102400170616     C                   ENDIF
102500170616     C*** STAC
102600170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
102700170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
102800170616     C                   ENDIF
102900170616     C*** SRTC
103000170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
103100170616     C                   ENDIF
103200170616     C*** DESxx
103300170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
103400170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
103500170616     C                   ENDIF
103600170616     C*
103700170616     C                   ENDIF
103800170616     C                   EVAL      i = i + 1
103900170616     C                   ENDDO
104000170616     C*
104100170616     C*** ATB
104200170616     C                   EVAL      CSTATB = *blanks
104300170616     C*** VER
104400170616     C                   EVAL      CSTVER = NEW_VERSION
104500170616     C*
104600170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
104700170621     C                   IF        wDESxx <> *blanks
104800170616     C     KEYcst_11C    CHAIN     dpCST11i
104900170616     C                   IF        %found(dpCST11i)
105000170621     C                   EVAL      CSTDESFR = wDESxx
105100170616     C                   UPDATE    DPCST100
105200170620     C                   ENDIF
105300170616     C                   ENDIF
105400170616     C*
105500170616     C                   ENDSR
105600170616     C***
105700170616
105800170616
105900170616     C*----------------------------------------------------*
106000170616     C*  GESTIONE MAPPATURA "STATE.EN"
106100170616     C*----------------------------------------------------*
106200170616     C     GESCST_EN     BEGSR
106300170616     C*
106400170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
106500170616     C                   Z-ADD     2             i                 4 0
106600170620     C                   DOW       i <= %elem(SkSplitFLD)
106700170620     C                             AND i <= OutIdxMax
106800170616     C*
106900170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
107000170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
107100170616     C                             SkSplitCSV_9(i) <> *blanks
107200170616     C*
107300170616     C*** ISO2
107400170616     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
107500170616     C                   EVAL      CSTISO2 = SkSplitCSV_9(i)
107600170616     C                   ENDIF
107700170616     C*** REGC
107800170616     C                   IF        %trim(SkSplitFLD(i)) = 'REGIONCODEE'
107900170616     C                   EVAL      CSTREGC = SkSplitCSV_9(i)
108000170616     C                   ENDIF
108100170616     C*** STAC
108200170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATECODE'
108300170616     C                   EVAL      CSTSTAC = SkSplitCSV_9(i)
108400170616     C                   ENDIF
108500170616     C*** SRTC
108600170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATESHORTCUT'
108700170616     C                   ENDIF
108800170616     C*** DESxx
108900170616     C                   IF        %trim(SkSplitFLD(i)) = 'STATENAME'
109000170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
109100170616     C                   ENDIF
109200170616     C*
109300170616     C                   ENDIF
109400170616     C                   EVAL      i = i + 1
109500170616     C                   ENDDO
109600170616     C*
109700170616     C*** ATB
109800170616     C                   EVAL      CSTATB = *blanks
109900170616     C*** VER
110000170616     C                   EVAL      CSTVER = NEW_VERSION
110100170616     C*
110200170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
110300170621     C                   IF        wDESxx <> *blanks
110400170616     C     KEYcst_11C    CHAIN     dpCST11i
110500170616     C                   IF        %found(dpCST11i)
110600170621     C                   EVAL      CSTDESEN = wDESxx
110700170616     C                   UPDATE    DPCST100
110800170620     C                   ENDIF
110900170616     C                   ENDIF
111000170616     C*
111100170616     C                   ENDSR
111200170616     C***
111300170616
111400170616
111500170616     C*----------------------------------------------------*
111600170616     C*  GESTIONE MAPPATURA "ELEMENTS"
111700170616     C*----------------------------------------------------*
111800170616     C     GESCEL        BEGSR
111900170616     C*
112000170616     C* Inizializzo il formato record del file relativo alla mappatura corrente
112100170616     C                   CLEAR                   DPCEL100
112200170616     C*
112300170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
112400170616     C                   Z-ADD     2             i                 4 0
112500170620     C                   DOW       i <= %elem(SkSplitFLD)
112600170620     C                             AND i <= OutIdxMax
112700170616     C*
112800170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
112900170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
113000170616     C                             SkSplitCSV_9(i) <> *blanks
113100170616     C*
113200170620     C*** ELMC
113300170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
113400170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
113500170620     C                   EXSR      CHKNUM
113600170620     C                   IF        PiInt=*on
113700170620     C                   Z-ADD     PiVal         CELELMC
113800170620     C                   ENDIF
113900170620     C                   ENDIF
114000170616     C*** SRTC
114100170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTSHORTCUT'
114200170616     C                   EVAL      CELSRTC = SkSplitCSV_9(i)
114300170616     C                   ENDIF
114400170616     C*** ELMT
114500170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTTYPE'
114600170616     C                   EVAL      CELELMT = SkSplitCSV_9(i)
114700170616     C                   ENDIF
114800170616     C*
114900170616     C                   ENDIF
115000170616     C                   EVAL      i = i + 1
115100170616     C                   ENDDO
115200170616     C*
115300170616     C*** VER
115400170616     C                   EVAL      CELVER = NEW_VERSION
115500170616     C*
115600170616     C* Infine inserisco il record corrente
115700170616     C                   WRITE     DPCEL100
115800170616     C*
115900170616     C                   ENDSR
116000170616     C***
116100170616
116200170616
116300170616     C*----------------------------------------------------*
116400170616     C*  GESTIONE MAPPATURA "ELEMENTS.IT"
116500170616     C*----------------------------------------------------*
116600170616     C     GESCEL_IT     BEGSR
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
118500170616     C*** DESxx
118600170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
118700170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
118800170616     C                   ENDIF
118900170616     C*
119000170616     C                   ENDIF
119100170616     C                   EVAL      i = i + 1
119200170616     C                   ENDDO
119300170616     C*
119400170616     C*** ATB
119500170616     C                   EVAL      CELATB = *blanks
119600170616     C*** VER
119700170616     C                   EVAL      CELVER = NEW_VERSION
119800170616     C*
119900170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
120000170621     C                   IF        wDESxx <> *blanks
120100170616     C     KEYcel_11C    CHAIN     dpCEL11i
120200170616     C                   IF        %found(dpCEL11i)
120300170621     C                   EVAL      CELDESIT = wDESxx
120400170616     C                   UPDATE    DPCEL100
120500170620     C                   ENDIF
120600170616     C                   ENDIF
120700170616     C*
120800170616     C                   ENDSR
120900170616     C***
121000170616
121100170616
121200170616     C*----------------------------------------------------*
121300170616     C*  GESTIONE MAPPATURA "ELEMENTS.DE"
121400170616     C*----------------------------------------------------*
121500170616     C     GESCEL_DE     BEGSR
121600170616     C*
121700170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
121800170616     C                   Z-ADD     2             i                 4 0
121900170620     C                   DOW       i <= %elem(SkSplitFLD)
122000170620     C                             AND i <= OutIdxMax
122100170616     C*
122200170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
122300170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
122400170616     C                             SkSplitCSV_9(i) <> *blanks
122500170616     C*
122600170620     C*** ELMC
122700170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
122800170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
122900170620     C                   EXSR      CHKNUM
123000170620     C                   IF        PiInt=*on
123100170620     C                   Z-ADD     PiVal         CELELMC
123200170620     C                   ENDIF
123300170620     C                   ENDIF
123400170616     C*** DESxx
123500170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
123600170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
123700170616     C                   ENDIF
123800170616     C*
123900170616     C                   ENDIF
124000170616     C                   EVAL      i = i + 1
124100170616     C                   ENDDO
124200170616     C*
124300170616     C*** ATB
124400170616     C                   EVAL      CELATB = *blanks
124500170616     C*** VER
124600170616     C                   EVAL      CELVER = NEW_VERSION
124700170616     C*
124800170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
124900170621     C                   IF        wDESxx <> *blanks
125000170616     C     KEYcel_11C    CHAIN     dpCEL11i
125100170616     C                   IF        %found(dpCEL11i)
125200170621     C                   EVAL      CELDESDE = wDESxx
125300170616     C                   UPDATE    DPCEL100
125400170620     C                   ENDIF
125500170616     C                   ENDIF
125600170616     C*
125700170616     C                   ENDSR
125800170616     C***
125900170616
126000170616
126100170616     C*----------------------------------------------------*
126200170616     C*  GESTIONE MAPPATURA "ELEMENTS.FR"
126300170616     C*----------------------------------------------------*
126400170616     C     GESCEL_FR     BEGSR
126500170616     C*
126600170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
126700170616     C                   Z-ADD     2             i                 4 0
126800170620     C                   DOW       i <= %elem(SkSplitFLD)
126900170620     C                             AND i <= OutIdxMax
127000170616     C*
127100170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
127200170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
127300170616     C                             SkSplitCSV_9(i) <> *blanks
127400170616     C*
127500170620     C*** ELMC
127600170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
127700170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
127800170620     C                   EXSR      CHKNUM
127900170620     C                   IF        PiInt=*on
128000170620     C                   Z-ADD     PiVal         CELELMC
128100170620     C                   ENDIF
128200170620     C                   ENDIF
128300170616     C*** DESxx
128400170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
128500170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
128600170616     C                   ENDIF
128700170616     C*
128800170616     C                   ENDIF
128900170616     C                   EVAL      i = i + 1
129000170616     C                   ENDDO
129100170616     C*
129200170616     C*** ATB
129300170616     C                   EVAL      CELATB = *blanks
129400170616     C*** VER
129500170616     C                   EVAL      CELVER = NEW_VERSION
129600170616     C*
129700170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
129800170621     C                   IF        wDESxx <> *blanks
129900170616     C     KEYcel_11C    CHAIN     dpCEL11i
130000170616     C                   IF        %found(dpCEL11i)
130100170621     C                   EVAL      CELDESFR = wDESxx
130200170616     C                   UPDATE    DPCEL100
130300170620     C                   ENDIF
130400170616     C                   ENDIF
130500170616     C*
130600170616     C                   ENDSR
130700170616     C***
130800170616
130900170616
131000170616     C*----------------------------------------------------*
131100170616     C*  GESTIONE MAPPATURA "ELEMENTS.EN"
131200170616     C*----------------------------------------------------*
131300170616     C     GESCEL_EN     BEGSR
131400170616     C*
131500170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
131600170616     C                   Z-ADD     2             i                 4 0
131700170620     C                   DOW       i <= %elem(SkSplitFLD)
131800170620     C                             AND i <= OutIdxMax
131900170616     C*
132000170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
132100170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
132200170616     C                             SkSplitCSV_9(i) <> *blanks
132300170616     C*
132400170620     C*** ELMC
132500170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
132600170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
132700170620     C                   EXSR      CHKNUM
132800170620     C                   IF        PiInt=*on
132900170620     C                   Z-ADD     PiVal         CELELMC
133000170620     C                   ENDIF
133100170620     C                   ENDIF
133200170616     C*** DESxx
133300170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
133400170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
133500170616     C                   ENDIF
133600170616     C*
133700170616     C                   ENDIF
133800170616     C                   EVAL      i = i + 1
133900170616     C                   ENDDO
134000170616     C*
134100170616     C*** ATB
134200170616     C                   EVAL      CELATB = *blanks
134300170616     C*** VER
134400170616     C                   EVAL      CELVER = NEW_VERSION
134500170616     C*
134600170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
134700170621     C                   IF        wDESxx <> *blanks
134800170616     C     KEYcel_11C    CHAIN     dpCEL11i
134900170616     C                   IF        %found(dpCEL11i)
135000170621     C                   EVAL      CELDESEN = wDESxx
135100170616     C                   UPDATE    DPCEL100
135200170620     C                   ENDIF
135300170616     C                   ENDIF
135400170616     C*
135500170616     C                   ENDSR
135600170616     C***
135700170616
135800170616
135900170616     C*----------------------------------------------------*
136000170616     C*  GESTIONE MAPPATURA "ELEMENTS.PT"
136100170616     C*----------------------------------------------------*
136200170616     C     GESCEL_PT     BEGSR
136300170616     C*
136400170616     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
136500170616     C                   Z-ADD     2             i                 4 0
136600170620     C                   DOW       i <= %elem(SkSplitFLD)
136700170620     C                             AND i <= OutIdxMax
136800170616     C*
136900170616     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
137000170616     C                   IF        SkSplitFLD(i)   <> *blanks AND
137100170616     C                             SkSplitCSV_9(i) <> *blanks
137200170616     C*
137300170620     C*** ELMC
137400170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODE'
137500170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
137600170620     C                   EXSR      CHKNUM
137700170620     C                   IF        PiInt=*on
137800170620     C                   Z-ADD     PiVal         CELELMC
137900170620     C                   ENDIF
138000170620     C                   ENDIF
138100170616     C*** DESxx
138200170616     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTDESCRIPTION'
138300170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
138400170616     C                   ENDIF
138500170616     C*
138600170616     C                   ENDIF
138700170616     C                   EVAL      i = i + 1
138800170616     C                   ENDDO
138900170616     C*
139000170616     C*** ATB
139100170616     C                   EVAL      CELATB = *blanks
139200170616     C*** VER
139300170616     C                   EVAL      CELVER = NEW_VERSION
139400170616     C*
139500170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
139600170621     C                   IF        wDESxx <> *blanks
139700170616     C     KEYcel_11C    CHAIN     dpCEL11i
139800170616     C                   IF        %found(dpCEL11i)
139900170621     C                   EVAL      CELDESPT = wDESxx
140000170616     C                   UPDATE    DPCEL100
140100170620     C                   ENDIF
140200170616     C                   ENDIF
140300170616     C*
140400170616     C                   ENDSR
140500170619     C***
140600170619
140700170619
140800170619     C*----------------------------------------------------*
140900170619     C*  GESTIONE MAPPATURA "SOCODE"
141000170619     C*----------------------------------------------------*
141100170619     C     GESCSO        BEGSR
141200170619     C*
141300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
141400170619     C                   CLEAR                   DPCSO100
141500170619     C*
141600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
141700170619     C                   Z-ADD     2             i                 4 0
141800170620     C                   DOW       i <= %elem(SkSplitFLD)
141900170620     C                             AND i <= OutIdxMax
142000170619     C*
142100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
142200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
142300170619     C                             SkSplitCSV_9(i) <> *blanks
142400170619     C*
142500170619     C*** SORC
142600170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
142700170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
142800170619     C                   EXSR      CHKNUM
142900170619     C                   IF        PiInt=*on
143000170619     C                   Z-ADD     PiVal         CSOSORC
143100170619     C                   ENDIF
143200170619     C                   ENDIF
143300170619     C*** STXT
143400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICETEXT'
143500170620     C                   EVAL      CSOSTXT  = SkSplitCSV_9(i)
143600170619     C                   ENDIF
143700170619     C*** SMRK
143800170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICEMARK'
143900170620     C                   EVAL      CSOSMRK  = SkSplitCSV_9(i)
144000170619     C                   ENDIF
144100170620     C*** ELMC
144200170619     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODES'
144300170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
144400170619     C                   ENDIF
144500170619     C*** ASCO
144600170619     C                   IF        %trim(SkSplitFLD(i)) = 'ASCODENEEDED'
144700170620     C                   EVAL      CSOASCO  = SkSplitCSV_9(i)
144800170619     C                   ENDIF
144900170619     C*
145000170619     C                   ENDIF
145100170619     C                   EVAL      i = i + 1
145200170619     C                   ENDDO
145300170619     C*
145400170619     C*** VER
145500170619     C                   EVAL      CSOVER = NEW_VERSION
145600170620     C*
145700170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
145800170620     C                   EVAL      InStringa = w2048_01
145900170620     C                   EVAL      InSepara = ','
146000170620     C                   SETOFF                                       131419
146100170620     C                   SETON                                        13
146200170620     C                   EXSR      SR_SPLIT
146300170620     C*
146400170620     C* Se nessun elemento => scrivo comunque un record singolo
146500170620     C                   IF        OutIdxMax = *zeros
146600170620     C                   WRITE     DPCSO100
146700170620     C                   ELSE
146800170620     C*
146900170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
147000170620     C                   Z-ADD     1             j                 4 0
147100170620     C                   DOW       j <= %elem(SkSplitCSV_3)
147200170620     C                             AND j <= OutIdxMax
147300170620     C*** ELMC
147400170620     C                   EVAL      PiStr = SkSplitCSV_3(j)
147500170620     C                   EXSR      CHKNUM
147600170620     C                   IF        PiInt=*on
147700170620     C                   Z-ADD     PiVal         CSOELMC
147800170620     C                   ENDIF
147900170620     C*
148000170620     C* Infine inserisco il record corrente
148100170620     C                   WRITE     DPCSO100
148200170620     C*
148300170620     C                   EVAL      j = j + 1
148400170620     C                   ENDDO
148500170620     C                   ENDIF
148600170619     C*
148700170619     C                   ENDSR
148800170619     C***
148900170619
149000170619
149100170619     C*----------------------------------------------------*
149200170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.IT"
149300170619     C*----------------------------------------------------*
149400170620     C     GESCSO_IT     BEGSR
149500170619     C*
149600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
149700170619     C                   Z-ADD     2             i                 4 0
149800170620     C                   DOW       i <= %elem(SkSplitFLD)
149900170620     C                             AND i <= OutIdxMax
150000170620     C*
150100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
150200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
150300170619     C                             SkSplitCSV_9(i) <> *blanks
150400170619     C*
150500170620     C*** SORC
150600170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
150700170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
150800170620     C                   EXSR      CHKNUM
150900170620     C                   IF        PiInt=*on
151000170620     C                   Z-ADD     PiVal         CSOSORC
151100170620     C                   ENDIF
151200170620     C                   ENDIF
151300170619     C*** DESxx
151400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
151500170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
151600170619     C                   ENDIF
151700170619     C*
151800170619     C                   ENDIF
151900170620     C                   EVAL      i = i + 1
152000170619     C                   ENDDO
152100170619     C*
152200170619     C*** ATB
152300170619     C                   EVAL      CSOATB = *blanks
152400170619     C*** VER
152500170619     C                   EVAL      CSOVER = NEW_VERSION
152600170619     C*
152700170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
152800170621     C                   IF        wDESxx <> *blanks
152900170620     C     KEYcso_11C    CHAIN     dpCSO11i
153000170619     C                   IF        %found(dpCSO11i)
153100170621     C                   EVAL      CSODESIT = wDESxx
153200170619     C                   UPDATE    DPCSO100
153300170620     C                   ENDIF
153400170619     C                   ENDIF
153500170619     C*
153600170619     C                   ENDSR
153700170619     C***
153800170619
153900170619
154000170619     C*----------------------------------------------------*
154100170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.EN"
154200170619     C*----------------------------------------------------*
154300170619     C     GESCSO_EN     BEGSR
154400170619     C*
154500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
154600170619     C                   Z-ADD     2             i                 4 0
154700170620     C                   DOW       i <= %elem(SkSplitFLD)
154800170620     C                             AND i <= OutIdxMax
154900170619     C*
155000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
155100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
155200170619     C                             SkSplitCSV_9(i) <> *blanks
155300170619     C*
155400170620     C*** SORC
155500170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
155600170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
155700170620     C                   EXSR      CHKNUM
155800170620     C                   IF        PiInt=*on
155900170620     C                   Z-ADD     PiVal         CSOSORC
156000170620     C                   ENDIF
156100170620     C                   ENDIF
156200170619     C*** DESxx
156300170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
156400170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
156500170619     C                   ENDIF
156600170619     C*
156700170619     C                   ENDIF
156800170620     C                   EVAL      i = i + 1
156900170619     C                   ENDDO
157000170619     C*
157100170619     C*** ATB
157200170619     C                   EVAL      CSOATB = *blanks
157300170619     C*** VER
157400170619     C                   EVAL      CSOVER = NEW_VERSION
157500170619     C*
157600170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
157700170621     C                   IF        wDESxx <> *blanks
157800170619     C     KEYcso_11C    CHAIN     dpCSO11i
157900170619     C                   IF        %found(dpCSO11i)
158000170621     C                   EVAL      CSODESEN = wDESxx
158100170619     C                   UPDATE    DPCSO100
158200170620     C                   ENDIF
158300170619     C                   ENDIF
158400170619     C*
158500170619     C                   ENDSR
158600170619     C***
158700170619
158800170619
158900170619     C*----------------------------------------------------*
159000170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.DE"
159100170619     C*----------------------------------------------------*
159200170619     C     GESCSO_DE     BEGSR
159300170619     C*
159400170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
159500170619     C                   Z-ADD     2             i                 4 0
159600170620     C                   DOW       i <= %elem(SkSplitFLD)
159700170620     C                             AND i <= OutIdxMax
159800170619     C*
159900170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
160000170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
160100170619     C                             SkSplitCSV_9(i) <> *blanks
160200170619     C*
160300170620     C*** SORC
160400170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
160500170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
160600170620     C                   EXSR      CHKNUM
160700170620     C                   IF        PiInt=*on
160800170620     C                   Z-ADD     PiVal         CSOSORC
160900170620     C                   ENDIF
161000170620     C                   ENDIF
161100170619     C*** DESxx
161200170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
161300170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
161400170619     C                   ENDIF
161500170619     C*
161600170619     C                   ENDIF
161700170620     C                   EVAL      i = i + 1
161800170619     C                   ENDDO
161900170619     C*
162000170619     C*** ATB
162100170619     C                   EVAL      CSOATB = *blanks
162200170619     C*** VER
162300170619     C                   EVAL      CSOVER = NEW_VERSION
162400170619     C*
162500170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
162600170621     C                   IF        wDESxx <> *blanks
162700170619     C     KEYcso_11C    CHAIN     dpCSO11i
162800170619     C                   IF        %found(dpCSO11i)
162900170621     C                   EVAL      CSODESDE = wDESxx
163000170619     C                   UPDATE    DPCSO100
163100170620     C                   ENDIF
163200170619     C                   ENDIF
163300170619     C*
163400170619     C                   ENDSR
163500170619     C***
163600170619
163700170619
163800170619     C*----------------------------------------------------*
163900170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.FR"
164000170619     C*----------------------------------------------------*
164100170619     C     GESCSO_FR     BEGSR
164200170619     C*
164300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
164400170619     C                   Z-ADD     2             i                 4 0
164500170620     C                   DOW       i <= %elem(SkSplitFLD)
164600170620     C                             AND i <= OutIdxMax
164700170619     C*
164800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
164900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
165000170619     C                             SkSplitCSV_9(i) <> *blanks
165100170619     C*
165200170620     C*** SORC
165300170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
165400170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
165500170620     C                   EXSR      CHKNUM
165600170620     C                   IF        PiInt=*on
165700170620     C                   Z-ADD     PiVal         CSOSORC
165800170620     C                   ENDIF
165900170620     C                   ENDIF
166000170619     C*** DESxx
166100170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
166200170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
166300170619     C                   ENDIF
166400170619     C*
166500170619     C                   ENDIF
166600170620     C                   EVAL      i = i + 1
166700170619     C                   ENDDO
166800170619     C*
166900170619     C*** ATB
167000170619     C                   EVAL      CSOATB = *blanks
167100170619     C*** VER
167200170619     C                   EVAL      CSOVER = NEW_VERSION
167300170619     C*
167400170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
167500170621     C                   IF        wDESxx <> *blanks
167600170619     C     KEYcso_11C    CHAIN     dpCSO11i
167700170619     C                   IF        %found(dpCSO11i)
167800170621     C                   EVAL      CSODESFR = wDESxx
167900170619     C                   UPDATE    DPCSO100
168000170620     C                   ENDIF
168100170619     C                   ENDIF
168200170619     C*
168300170619     C                   ENDSR
168400170619     C***
168500170619
168600170619
168700170619     C*----------------------------------------------------*
168800170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.ET"
168900170619     C*----------------------------------------------------*
169000170619     C     GESCSO_ET     BEGSR
169100170619     C*
169200170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
169300170619     C                   Z-ADD     2             i                 4 0
169400170620     C                   DOW       i <= %elem(SkSplitFLD)
169500170620     C                             AND i <= OutIdxMax
169600170619     C*
169700170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
169800170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
169900170619     C                             SkSplitCSV_9(i) <> *blanks
170000170619     C*
170100170620     C*** SORC
170200170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
170300170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
170400170620     C                   EXSR      CHKNUM
170500170620     C                   IF        PiInt=*on
170600170620     C                   Z-ADD     PiVal         CSOSORC
170700170620     C                   ENDIF
170800170620     C                   ENDIF
170900170619     C*** DESxx
171000170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
171100170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
171200170619     C                   ENDIF
171300170619     C*
171400170619     C                   ENDIF
171500170620     C                   EVAL      i = i + 1
171600170619     C                   ENDDO
171700170619     C*
171800170619     C*** ATB
171900170619     C                   EVAL      CSOATB = *blanks
172000170619     C*** VER
172100170619     C                   EVAL      CSOVER = NEW_VERSION
172200170619     C*
172300170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
172400170621     C                   IF        wDESxx <> *blanks
172500170619     C     KEYcso_11C    CHAIN     dpCSO11i
172600170619     C                   IF        %found(dpCSO11i)
172700170621     C                   EVAL      CSODESET = wDESxx
172800170619     C                   UPDATE    DPCSO100
172900170620     C                   ENDIF
173000170619     C                   ENDIF
173100170619     C*
173200170619     C                   ENDSR
173300170619     C***
173400170619
173500170619
173600170619     C*----------------------------------------------------*
173700170619     C*  GESTIONE MAPPATURA "SOSERVICEINFO.CS"
173800170619     C*----------------------------------------------------*
173900170619     C     GESCSO_CS     BEGSR
174000170619     C*
174100170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
174200170619     C                   Z-ADD     2             i                 4 0
174300170620     C                   DOW       i <= %elem(SkSplitFLD)
174400170620     C                             AND i <= OutIdxMax
174500170619     C*
174600170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
174700170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
174800170619     C                             SkSplitCSV_9(i) <> *blanks
174900170619     C*
175000170620     C*** SORC
175100170620     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODE'
175200170620     C                   EVAL      PiStr = SkSplitCSV_9(i)
175300170620     C                   EXSR      CHKNUM
175400170620     C                   IF        PiInt=*on
175500170620     C                   Z-ADD     PiVal         CSOSORC
175600170620     C                   ENDIF
175700170620     C                   ENDIF
175800170619     C*** DESxx
175900170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOFIELDINFO'
176000170621     C                   EVAL      wDESxx = SkSplitCSV_9(i)
176100170619     C                   ENDIF
176200170619     C*
176300170619     C                   ENDIF
176400170620     C                   EVAL      i = i + 1
176500170619     C                   ENDDO
176600170619     C*
176700170619     C*** ATB
176800170619     C                   EVAL      CSOATB = *blanks
176900170619     C*** VER
177000170619     C                   EVAL      CSOVER = NEW_VERSION
177100170619     C*
177200170620     C* Aggancio la tabella per aggiornare la descrizione in lingua relativa
177300170621     C                   IF        wDESxx <> *blanks
177400170619     C     KEYcso_11C    CHAIN     dpCSO11i
177500170619     C                   IF        %found(dpCSO11i)
177600170621     C                   EVAL      CSODESCS = wDESxx
177700170619     C                   UPDATE    DPCSO100
177800170620     C                   ENDIF
177900170619     C                   ENDIF
178000170619     C*
178100170619     C                   ENDSR
178200170619     C***
178300170619
178400170619
178500170619     C*----------------------------------------------------*
178600170619     C*  GESTIONE MAPPATURA "ASCODE"
178700170619     C*----------------------------------------------------*
178800170619     C     GESCAS        BEGSR
178900170619     C*
179000170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
179100170619     C                   CLEAR                   DPCAS100
179200170619     C*
179300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
179400170619     C                   Z-ADD     2             i                 4 0
179500170620     C                   DOW       i <= %elem(SkSplitFLD)
179600170620     C                             AND i <= OutIdxMax
179700170619     C*
179800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
179900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
180000170619     C                             SkSplitCSV_9(i) <> *blanks
180100170619     C*
180200170619     C*** ADSC
180300170619     C                   IF        %trim(SkSplitFLD(i)) = 'ASCODE'
180400170620     C                   EVAL      CASADSC = SkSplitCSV_9(i)
180500170619     C                   ENDIF
180600170619     C*** STXT
180700170619     C                   IF        %trim(SkSplitFLD(i)) = 'SERVICETEXT'
180800170620     C                   EVAL      CASSTXT = SkSplitCSV_9(i)
180900170619     C                   ENDIF
181000170620     C*** ELMC
181100170620     C                   IF        %trim(SkSplitFLD(i)) = 'ELEMENTCODES'
181200170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
181300170620     C                   ENDIF
181400170619     C*
181500170619     C                   ENDIF
181600170620     C                   EVAL      i = i + 1
181700170619     C                   ENDDO
181800170619     C*
181900170619     C*** VER
182000170619     C                   EVAL      CASVER = NEW_VERSION
182100170620     C*
182200170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
182300170620     C                   EVAL      InStringa = w2048_01
182400170620     C                   EVAL      InSepara = ','
182500170620     C                   SETOFF                                       131419
182600170620     C                   SETON                                        13
182700170620     C                   EXSR      SR_SPLIT
182800170620     C*
182900170620     C* Se nessun elemento => scrivo comunque un record singolo
183000170620     C                   IF        OutIdxMax = *zeros
183100170620     C                   WRITE     DPCAS100
183200170620     C                   ELSE
183300170620     C*
183400170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
183500170620     C                   Z-ADD     1             j                 4 0
183600170620     C                   DOW       j <= %elem(SkSplitCSV_3)
183700170620     C                             AND j <= OutIdxMax
183800170620     C*** ELMC
183900170620     C                   EVAL      PiStr = SkSplitCSV_3(j)
184000170620     C                   EXSR      CHKNUM
184100170620     C                   IF        PiInt=*on
184200170620     C                   Z-ADD     PiVal         CASELMC
184300170620     C                   ENDIF
184400170620     C*
184500170620     C* Infine inserisco il record corrente
184600170620     C                   WRITE     DPCAS100
184700170620     C*
184800170620     C                   EVAL      j = j + 1
184900170620     C                   ENDDO
185000170620     C                   ENDIF
185100170619     C*
185200170619     C                   ENDSR
185300170619     C***
185400170619
185500170619
185600170619     C*----------------------------------------------------*
185700170619     C*  GESTIONE MAPPATURA "IC"
185800170619     C*----------------------------------------------------*
185900170619     C     GESCIC        BEGSR
186000170619     C*
186100170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
186200170619     C                   CLEAR                   DPCIC100
186300170619     C*
186400170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
186500170619     C                   Z-ADD     2             i                 4 0
186600170620     C                   DOW       i <= %elem(SkSplitFLD)
186700170620     C                             AND i <= OutIdxMax
186800170619     C*
186900170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
187000170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
187100170619     C                             SkSplitCSV_9(i) <> *blanks
187200170619     C*
187300170619     C*** ICC
187400170619     C                   IF        %trim(SkSplitFLD(i)) = 'INJECTIONCODE'
187500170619     C                   EVAL      CICICC  = SkSplitCSV_9(i)
187600170619     C                   ENDIF
187700170619     C*** DPTN
187800170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
187900170619     C                   EVAL      CICDPTN = SkSplitCSV_9(i)
188000170619     C                   ENDIF
188100170619     C*** ICD
188200170619     C                   IF        %trim(SkSplitFLD(i)) = 'DESCRIPTION'
188300170619     C                   EVAL      CICICD  = SkSplitCSV_9(i)
188400170619     C                   ENDIF
188500170619     C*
188600170619     C                   ENDIF
188700170620     C                   EVAL      i = i + 1
188800170619     C                   ENDDO
188900170619     C*
189000170619     C*** VER
189100170619     C                   EVAL      CICVER = NEW_VERSION
189200170619     C*
189300170619     C* Infine inserisco il record corrente
189400170619     C                   WRITE     DPCIC100
189500170619     C*
189600170619     C                   ENDSR
189700170619     C***
189800170619
189900170619
190000170619     C*----------------------------------------------------*
190100170619     C*  GESTIONE MAPPATURA "NETWORK"
190200170619     C*----------------------------------------------------*
190300170619     C     GESCNT        BEGSR
190400170619     C*
190500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
190600170619     C                   CLEAR                   DPCNT100
190700170619     C*
190800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
190900170619     C                   Z-ADD     2             i                 4 0
191000170620     C                   DOW       i <= %elem(SkSplitFLD)
191100170620     C                             AND i <= OutIdxMax
191200170619     C*
191300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
191400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
191500170619     C                             SkSplitCSV_9(i) <> *blanks
191600170619     C*
191700170619     C*** NTWC
191800170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKCODE'
191900170619     C                   EVAL      CNTNTWC = SkSplitCSV_9(i)
192000170619     C                   ENDIF
192100170619     C*** BUCN
192200170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
192300170619     C                   EVAL      CNTBUCN = SkSplitCSV_9(i)
192400170619     C                   ENDIF
192500170619     C*** ISO2
192600170619     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
192700170619     C                   EVAL      CNTISO2 = SkSplitCSV_9(i)
192800170619     C                   ENDIF
192900170619     C*** NTWN
193000170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKNAME'
193100170619     C                   EVAL      CNTNTWN = SkSplitCSV_9(i)
193200170619     C                   ENDIF
193300170619     C*
193400170619     C                   ENDIF
193500170619     C                   EVAL      i = i + 1
193600170619     C                   ENDDO
193700170619     C*
193800170619     C*** VER
193900170619     C                   EVAL      CNTVER = NEW_VERSION
194000170619     C*
194100170619     C* Infine inserisco il record corrente
194200170619     C                   WRITE     DPCNT100
194300170619     C*
194400170619     C                   ENDSR
194500170619     C***
194600170619
194700170619
194800170619     C*----------------------------------------------------*
194900170621     C*  GESTIONE MAPPATURA "NETWORKPLAN"
195000170619     C*----------------------------------------------------*
195100170619     C     GESCNP        BEGSR
195200170619     C*
195300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
195400170619     C                   CLEAR                   DPCNP100
195500170619     C*
195600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
195700170619     C                   Z-ADD     2             i                 4 0
195800170620     C                   DOW       i <= %elem(SkSplitFLD)
195900170620     C                             AND i <= OutIdxMax
196000170619     C*
196100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
196200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
196300170619     C                             SkSplitCSV_9(i) <> *blanks
196400170619     C*
196500170619     C*** NTWP
196600170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
196700170619     C                   EVAL      CNPNTWP  = SkSplitCSV_9(i)
196800170619     C                   ENDIF
196900170621     C*** SORCB / SORCE
197000170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOCODELIST'
197100170621     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
197200170619     C                   ENDIF
197300170619     C*** ASCOL
197400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SOASCODELIST'
197500170619     C                   EVAL      CNPASCOL = SkSplitCSV_9(i)
197600170619     C                   ENDIF
197700170619     C*** FULL
197800170619     C                   IF        %trim(SkSplitFLD(i)) = 'FULLNETWORKPLAN'
197900170619     C                   EVAL      CNPFULL  = SkSplitCSV_9(i)
198000170619     C                   ENDIF
198100170619     C*** PTY
198200170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
198300170619     C                   EVAL      CNPPTY   = SkSplitCSV_9(i)
198400170619     C                   ENDIF
198500170619     C*
198600170619     C                   ENDIF
198700170619     C                   EVAL      i = i + 1
198800170619     C                   ENDDO
198900170619     C*
199000170619     C*** VER
199100170621     C                   EVAL      CNPVER = NEW_VERSION
199200170621     C*
199300170621     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
199400170621     C                   EVAL      InStringa = w2048_01
199500170621     C                   EVAL      InSepara = ','
199600170621     C                   SETOFF                                       131419
199700170621     C                   SETON                                        13
199800170621     C                   EXSR      SR_SPLIT
199900170621     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
200000170621     C*
200100170621     C* Se nessun elemento => scrivo comunque un record singolo
200200170621     C                   IF        sOutIdxMax_w1 = *zeros
200300170621     C                   WRITE     DPCNP100
200400170621     C                   ELSE
200500170621     C*
200600170621     C* Sviluppo tutti gli elementi della lista dei Routing Places
200700170621     C                   Z-ADD     1             j                 4 0
200800170621     C                   DOW       j <= %elem(SkSplitCSV_3)
200900170621     C                             AND j <= sOutIdxMax_w1
201000170621     C*
201100170621     C* Determino il tipo di Routing Place
201200170621     C                   SELECT
201300170621     C* SOCODE - Single
201400170621     C                   WHEN      %len(%trim((SkSplitCSV_3(j)))) = 4
201500170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):2:3)
201600170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
201700170621     C                   EVAL      CNPSORCE = CNPSORCB
201800170621     C                   WRITE     DPCNP100
201900170621     C* SOCODE - Range
202000170621     C                   WHEN      %scan('-':SkSplitCSV_3(j)) > *zeros
202100170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):2:3)
202200170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
202300170621     C                   EVAL      wSORC    = %subst(SkSplitCSV_3(j):6:3)
202400170621     C                   EVAL      CNPSORCE = %dec(wSORC:3:0)
202500170621     C                   WRITE     DPCNP100
202600170621     C* SOCODE - List
202700170621     C                   WHEN      %scan(':':SkSplitCSV_3(j)) > *zeros
202800170621     C                   EVAL      InStringa = %subst(SkSplitCSV_3(j):2)
202900170621     C                   EVAL      InSepara = ':'
203000170621     C                   SETOFF                                       131419
203100170621     C                   SETON                                        14
203200170621     C                   EXSR      SR_SPLIT
203300170621     C*
203400170621     C* Sviluppo tutti gli elementi della lista dei Routing Places
203500170621     C                   Z-ADD     1             z                 4 0
203600170621     C                   DOW       z <= %elem(SkSplitCSV_4)
203700170621     C                             AND z <= OutIdxMax
203800170621     C                   EVAL      wSORC    = SkSplitCSV_4(z)
203900170621     C                   EVAL      CNPSORCB = %dec(wSORC:3:0)
204000170621     C                   EVAL      CNPSORCE = CNPSORCB
204100170621     C                   WRITE     DPCNP100
204200170621     C                   EVAL      z = z + 1
204300170621     C                   ENDDO
204400170621     C*
204500170621     C                   ENDSL
204600170621     C*
204700170621     C                   EVAL      j = j + 1
204800170621     C                   ENDDO
204900170621     C                   ENDIF
205000170619     C*
205100170619     C                   ENDSR
205200170619     C***
205300170619
205400170619
205500170619     C*----------------------------------------------------*
205600170619     C*  GESTIONE MAPPATURA "ALLOWSO"
205700170619     C*----------------------------------------------------*
205800170619     C     GESCWS        BEGSR
205900170619     C*
206000170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
206100170619     C                   CLEAR                   DPCWS100
206200170619     C*
206300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
206400170619     C                   Z-ADD     2             i                 4 0
206500170620     C                   DOW       i <= %elem(SkSplitFLD)
206600170620     C                             AND i <= OutIdxMax
206700170619     C*
206800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
206900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
207000170619     C                             SkSplitCSV_9(i) <> *blanks
207100170619     C*
207200170621     C*** RULFT / RULFB / RULFE
207300170619     C                   IF        %trim(SkSplitFLD(i)) = 'RULEFROM'
207400170621     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
207500170621     C                   ENDIF
207600170621     C*** RULSB / RULSE
207700170621     C                   IF        %trim(SkSplitFLD(i)) = 'RULESERVICE'
207800170621     C                   EVAL      w2048_02 = SkSplitCSV_9(i)
207900170621     C                   ENDIF
208000170621     C*** RULTT / RULTB / RULTE
208100170621     C                   IF        %trim(SkSplitFLD(i)) = 'RULETO'
208200170621     C                   EVAL      w2048_03 = SkSplitCSV_9(i)
208300170619     C                   ENDIF
208400170619     C*** ZONT
208500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ZONETO'
208600170619     C                   EVAL      CWSZONT  = SkSplitCSV_9(i)
208700170619     C                   ENDIF
208800170619     C*** ALWID
208900170619     C                   IF        %trim(SkSplitFLD(i)) = 'UNIQUEALLOWID'
209000170619     C                   EVAL      CWSALWID = SkSplitCSV_9(i)
209100170619     C                   ENDIF
209200170619     C*
209300170619     C                   ENDIF
209400170619     C                   EVAL      i = i + 1
209500170619     C                   ENDDO
209600170619     C*
209700170619     C*** VER
209800170619     C                   EVAL      CWSVER = NEW_VERSION
209900170621     C*
210000170621     C* Splitto il 1° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
210100170621     C                   EVAL      InStringa = w2048_01
210200170621     C                   EVAL      InSepara = ','
210300170621     C                   SETOFF                                       131419
210400170621     C                   SETON                                        13
210500170621     C                   EXSR      SR_SPLIT
210600170621     C                   EVAL      wSkCSV_03_w1  = %subarr(SkSplitCSV_3:1)
210700170621     C                   EVAL      sOutIdxMax_w1 = OutIdxMax
210800170621     C*
210900170621     C* Splitto il 2° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
211000170621     C                   EVAL      InStringa = w2048_02
211100170621     C                   EVAL      InSepara = ','
211200170621     C                   SETOFF                                       131419
211300170621     C                   SETON                                        13
211400170621     C                   EXSR      SR_SPLIT
211500170621     C                   EVAL      wSkCSV_03_w2  = %subarr(SkSplitCSV_3:1)
211600170621     C                   EVAL      sOutIdxMax_w2 = OutIdxMax
211700170621     C*
211800170621     C* Splitto il 3° campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
211900170621     C                   EVAL      InStringa = w2048_03
212000170621     C                   EVAL      InSepara = ','
212100170621     C                   SETOFF                                       131419
212200170621     C                   SETON                                        13
212300170621     C                   EXSR      SR_SPLIT
212400170621     C                   EVAL      wSkCSV_03_w3  = %subarr(SkSplitCSV_3:1)
212500170621     C                   EVAL      sOutIdxMax_w3 = OutIdxMax
212600170621     C*
212700170621     C* Verifico eventuale assenza di elementi che in tal caso forzo a 1
212800170621     C                   IF        sOutIdxMax_w1 = *zeros
212900170621     C                   EVAL      sOutIdxMax_w1 = 1
213000170621     C                   ENDIF
213100170621     C                   IF        sOutIdxMax_w2 = *zeros
213200170621     C                   EVAL      sOutIdxMax_w2 = 1
213300170621     C                   ENDIF
213400170621     C                   IF        sOutIdxMax_w3 = *zeros
213500170621     C                   EVAL      sOutIdxMax_w3 = 1
213600170621     C                   ENDIF
213700170621     C*
213800170621     C* Sviluppo tutti gli elementi "moltiplicando" le liste delle RULE FROM/SOCODE/TO
213900170621     C*
214000170621     C* 1° lista
214100170621     C                   Z-ADD     1             i                 4 0
214200170621     C                   DOW       i <= %elem(wSkCSV_03_w1)
214300170621     C                             AND i <= sOutIdxMax_w1
214400170621     C*
214500170621     C* Determino il tipo di RULE FROM
214600170621     C                   SELECT
214700170621     C* DEPOT - Single
214800170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'D' AND
214900170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 8
215000170621     C                   EVAL      CWSRULFT = 'D'
215100170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:7)
215200170621     C                   EVAL      CWSRULFE = CWSRULFB
215300170621     C* DEPOT - Range
215400170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'D' AND
215500170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 15
215600170621     C                   EVAL      CWSRULFT = 'D'
215700170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:7)
215800170621     C                   EVAL      CWSRULFE = %subst(wSkCSV_03_w1(i):9:7)
215900170621     C* DEPOT GROUP - Single
216000170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'G'
216100170621     C                   EVAL      CWSRULFT = 'G'
216200170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:4)
216300170621     C                   EVAL      CWSRULFE = CWSRULFB
216400170621     C* COUNTRY - Single
216500170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'C'
216600170621     C                   EVAL      CWSRULFT = 'C'
216700170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:2)
216800170621     C                   EVAL      CWSRULFE = CWSRULFB
216900170621     C* NETWORK - Single
217000170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'N'
217100170621     C                   EVAL      CWSRULFT = 'N'
217200170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:3)
217300170621     C                   EVAL      CWSRULFE = CWSRULFB
217400170621     C* IC - Single
217500170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'I'
217600170621     C                   EVAL      CWSRULFT = 'I'
217700170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:9)
217800170621     C                   EVAL      CWSRULFE = CWSRULFB
217900170621     C* BU - Single
218000170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'B' AND
218100170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 4
218200170621     C                   EVAL      CWSRULFT = 'B'
218300170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:3)
218400170621     C                   EVAL      CWSRULFE = CWSRULFB
218500170621     C* BU+COUNTRY - Single
218600170621     C                   WHEN      %subst(wSkCSV_03_w1(i):1:1) = 'B' AND
218700170621     C                             %len(%trim((wSkCSV_03_w1(i)))) = 6
218800170621     C                   EVAL      CWSRULFT = 'B'
218900170621     C                   EVAL      CWSRULFB = %subst(wSkCSV_03_w1(i):2:5)
219000170621     C                   EVAL      CWSRULFE = CWSRULFB
219100170621     C*
219200170621     C                   ENDSL
219300170621     C*
219400170621     C* 2° lista
219500170621     C                   Z-ADD     1             j                 4 0
219600170621     C                   DOW       j <= %elem(wSkCSV_03_w2)
219700170621     C                             AND j <= sOutIdxMax_w2
219800170621     C*
219900170621     C* Determino il tipo di RULE SERVICE
220000170621     C                   SELECT
220100170621     C* SOCODE - Single
220200170621     C                   WHEN      %subst(wSkCSV_03_w2(j):1:2) = 'SO' AND
220300170621     C                             %len(%trim((wSkCSV_03_w2(j)))) = 5
220400170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):3:3)
220500170621     C                   EVAL      CWSRULSB = %dec(wSORC:3:0)
220600170621     C                   EVAL      CWSRULSE = CWSRULSB
220700170621     C* SOCODE - Range
220800170621     C                   WHEN      %subst(wSkCSV_03_w2(j):1:2) = 'SO' AND
220900170621     C                             %len(%trim((wSkCSV_03_w2(j)))) = 8
221000170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):3:3)
221100170621     C                   EVAL      CWSRULSB = %dec(wSORC:3:0)
221200170621     C                   EVAL      wSORC    = %subst(wSkCSV_03_w2(j):6:3)
221300170621     C                   EVAL      CWSRULSE = %dec(wSORC:3:0)
221400170621     C*
221500170621     C                   ENDSL
221600170621     C*
221700170621     C* 3° lista
221800170621     C                   Z-ADD     1             z                 4 0
221900170621     C                   DOW       z <= %elem(wSkCSV_03_w3)
222000170621     C                             AND z <= sOutIdxMax_w3
222100170621     C*
222200170621     C* Determino il tipo di RULE TO
222300170621     C                   SELECT
222400170621     C* DEPOT - Single
222500170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'D' AND
222600170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 8
222700170621     C                   EVAL      CWSRULTT = 'D'
222800170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:7)
222900170621     C                   EVAL      CWSRULTE = CWSRULTB
223000170621     C* DEPOT - Range
223100170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'D' AND
223200170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 15
223300170621     C                   EVAL      CWSRULTT = 'D'
223400170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:7)
223500170621     C                   EVAL      CWSRULTE = %subst(wSkCSV_03_w3(z):9:7)
223600170621     C* DEPOT GROUP - Single
223700170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'G'
223800170621     C                   EVAL      CWSRULTT = 'G'
223900170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:4)
224000170621     C                   EVAL      CWSRULTE = CWSRULTB
224100170621     C* COUNTRY - Single
224200170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'C'
224300170621     C                   EVAL      CWSRULTT = 'C'
224400170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:2)
224500170621     C                   EVAL      CWSRULTE = CWSRULTB
224600170621     C* NETWORK - Single
224700170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'N'
224800170621     C                   EVAL      CWSRULTT = 'N'
224900170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:3)
225000170621     C                   EVAL      CWSRULTE = CWSRULTB
225100170621     C* COUNTRY+ZIP CODE - Single
225200170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'Z' AND
225300170621     C                             %scan('-':wSkCSV_03_w3(z)) = *zeros
225400170621     C                   EVAL      CWSRULTT = 'Z'
225500170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:11)
225600170621     C                   EVAL      CWSRULTE = CWSRULTB
225700170621     C* COUNTRY+ZIP CODE - Range
225800170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'Z' AND
225900170621     C                             %scan('-':wSkCSV_03_w3(z)) > *zeros
226000170621     C                   EVAL      CWSRULTT = 'Z'
226100170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w3(z):2:
226200170621     C                                        %scan('-':wSkCSV_03_w3(z))-4)
226300170621     C                   EVAL      CWSRULTE = %subst(wSkCSV_03_w3(z):
226400170621     C                                        %scan('-':wSkCSV_03_w3(z))+1:9)
226500170621     C* BU+COUNTRY - Single
226600170621     C                   WHEN      %subst(wSkCSV_03_w3(z):1:1) = 'B' AND
226700170621     C                             %len(%trim((wSkCSV_03_w3(z)))) = 6
226800170621     C                   EVAL      CWSRULTT = 'B'
226900170621     C                   EVAL      CWSRULTB = %subst(wSkCSV_03_w1(z):2:5)
227000170621     C                   EVAL      CWSRULTE = CWSRULTB
227100170621     C*
227200170621     C                   ENDSL
227300170621     C*
227400170621     C* Inserisco il record corrente
227500170621     C                   WRITE     DPCWS100
227600170621     C*
227700170621     C                   EVAL      z = z + 1
227800170621     C                   ENDDO
227900170621     C*
228000170621     C                   EVAL      j = j + 1
228100170621     C                   ENDDO
228200170621     C*
228300170621     C                   EVAL      i = i + 1
228400170621     C                   ENDDO
228500170619     C*
228600170619     C                   ENDSR
228700170619     C***
228800170619
228900170619
229000170619     C*----------------------------------------------------*
229100170619     C*  GESTIONE MAPPATURA "ALLOWAS"
229200170619     C*----------------------------------------------------*
229300170619     C     GESCWA        BEGSR
229400170619     C*
229500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
229600170619     C                   CLEAR                   DPCWA100
229700170619     C*
229800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
229900170619     C                   Z-ADD     2             i                 4 0
230000170620     C                   DOW       i <= %elem(SkSplitFLD)
230100170620     C                             AND i <= OutIdxMax
230200170619     C*
230300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
230400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
230500170619     C                             SkSplitCSV_9(i) <> *blanks
230600170619     C*
230700170619     C*** ALWID
230800170619     C                   IF        %trim(SkSplitFLD(i)) = 'UNIQUEALLOWID'
230900170619     C                   EVAL      CWAALWID = SkSplitCSV_9(i)
231000170619     C                   ENDIF
231100170619     C*** RULS
231200170619     C                   IF        %trim(SkSplitFLD(i)) = 'RULESERVICE'
231300170619     C                   EVAL      CWARULS  = SkSplitCSV_9(i)
231400170619     C                   ENDIF
231500170619     C*** ZONT
231600170619     C                   IF        %trim(SkSplitFLD(i)) = 'ZONETO'
231700170619     C                   EVAL      CWAZONT  = SkSplitCSV_9(i)
231800170619     C                   ENDIF
231900170619     C*
232000170619     C                   ENDIF
232100170619     C                   EVAL      i = i + 1
232200170619     C                   ENDDO
232300170619     C*
232400170619     C*** VER
232500170619     C                   EVAL      CWAVER = NEW_VERSION
232600170619     C*
232700170619     C* Infine inserisco il record corrente
232800170619     C                   WRITE     DPCWA100
232900170619     C*
233000170619     C                   ENDSR
233100170619     C***
233200170619
233300170619
233400170619     C*----------------------------------------------------*
233500170619     C*  GESTIONE MAPPATURA "RELATION"
233600170619     C*----------------------------------------------------*
233700170619     C     GESCRE        BEGSR
233800170619     C*
233900170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
234000170619     C                   CLEAR                   DPCRE100
234100170619     C*
234200170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
234300170619     C                   Z-ADD     2             i                 4 0
234400170620     C                   DOW       i <= %elem(SkSplitFLD)
234500170620     C                             AND i <= OutIdxMax
234600170619     C*
234700170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
234800170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
234900170619     C                             SkSplitCSV_9(i) <> *blanks
235000170619     C*
235100170619     C*** DPTN
235200170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
235300170619     C                   EVAL      CREDPTN  = SkSplitCSV_9(i)
235400170619     C                   ENDIF
235500170619     C*** ROUT
235600170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTE'
235700170619     C                   EVAL      CREROUT  = SkSplitCSV_9(i)
235800170619     C                   ENDIF
235900170619     C*** RELA
236000170619     C                   IF        %trim(SkSplitFLD(i)) = 'RELATION'
236100170619     C                   EVAL      CRERELA  = SkSplitCSV_9(i)
236200170619     C                   ENDIF
236300170619     C*
236400170619     C                   ENDIF
236500170619     C                   EVAL      i = i + 1
236600170619     C                   ENDDO
236700170619     C*
236800170619     C*** VER
236900170619     C                   EVAL      CREVER = NEW_VERSION
237000170619     C*
237100170619     C* Infine inserisco il record corrente
237200170619     C                   WRITE     DPCRE100
237300170619     C*
237400170619     C                   ENDSR
237500170619     C***
237600170619
237700170619
237800170619     C*----------------------------------------------------*
237900170619     C*  GESTIONE MAPPATURA "DEPOTS"
238000170619     C*----------------------------------------------------*
238100170619     C     GESCDP        BEGSR
238200170619     C*
238300170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
238400170619     C                   CLEAR                   DPCDP100
238500170619     C*
238600170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
238700170619     C                   Z-ADD     2             i                 4 0
238800170620     C                   DOW       i <= %elem(SkSplitFLD)
238900170620     C                             AND i <= OutIdxMax
239000170619     C*
239100170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
239200170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
239300170619     C                             SkSplitCSV_9(i) <> *blanks
239400170619     C*
239500170619     C*** DPTN
239600170619     C                   IF        %trim(SkSplitFLD(i)) = 'GEOPOSTDEPOTNUMBER'
239700170619     C                   EVAL      CDPDPTN  = SkSplitCSV_9(i)
239800170619     C                   ENDIF
239900170619     C*** DSTR
240000170619     C                   IF        %trim(SkSplitFLD(i)) = 'DEPOTSTR'
240100170619     C                   EVAL      CDPDSTR  = SkSplitCSV_9(i)
240200170619     C                   ENDIF
240300170619     C*** IATA
240400170619     C                   IF        %trim(SkSplitFLD(i)) = 'IATALIKECODE'
240500170619     C                   EVAL      CDPIATA  = SkSplitCSV_9(i)
240600170619     C                   ENDIF
240700170619     C*** GRPID
240800170619     C                   IF        %trim(SkSplitFLD(i)) = 'GROUPID'
240900170619     C                   EVAL      CDPGRPID = SkSplitCSV_9(i)
241000170619     C                   ENDIF
241100170619     C*** NAME1
241200170619     C                   IF        %trim(SkSplitFLD(i)) = 'COMPANYNAME'
241300170619     C                   EVAL      CDPNAME1 = SkSplitCSV_9(i)
241400170619     C                   ENDIF
241500170619     C*** NAME2
241600170619     C                   IF        %trim(SkSplitFLD(i)) = 'COMPANYNAME2'
241700170619     C                   EVAL      CDPNAME2 = SkSplitCSV_9(i)
241800170619     C                   ENDIF
241900170619     C*** STRET
242000170619     C                   IF        %trim(SkSplitFLD(i)) = 'STREET'
242100170619     C                   EVAL      CDPSTRET = SkSplitCSV_9(i)
242200170619     C                   ENDIF
242300170619     C*** STNBR
242400170619     C                   IF        %trim(SkSplitFLD(i)) = 'PROPNUM'
242500170619     C                   EVAL      CDPSTNBR = SkSplitCSV_9(i)
242600170619     C                   ENDIF
242700170619     C*** ADDR1
242800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS2'
242900170619     C                   EVAL      CDPADDR1 = SkSplitCSV_9(i)
243000170619     C                   ENDIF
243100170619     C*** ADDR2
243200170619     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS3'
243300170619     C                   EVAL      CDPADDR2 = SkSplitCSV_9(i)
243400170619     C                   ENDIF
243500170619     C*** FLOOR
243600170619     C                   IF        %trim(SkSplitFLD(i)) = 'FLOOR'
243700170619     C                   EVAL      CDPFLOOR = SkSplitCSV_9(i)
243800170619     C                   ENDIF
243900170619     C*** BUILD
244000170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUILDING'
244100170619     C                   EVAL      CDPBUILD = SkSplitCSV_9(i)
244200170619     C                   ENDIF
244300170619     C*** ISO2
244400170619     C                   IF        %trim(SkSplitFLD(i)) = 'COUNTRYCODE'
244500170619     C                   EVAL      CDPISO2  = SkSplitCSV_9(i)
244600170619     C                   ENDIF
244700170619     C*** STATE
244800170619     C                   IF        %trim(SkSplitFLD(i)) = 'STATE'
244900170619     C                   EVAL      CDPSTATE = SkSplitCSV_9(i)
245000170619     C                   ENDIF
245100170619     C*** PTC
245200170619     C                   IF        %trim(SkSplitFLD(i)) = 'POSTCODE'
245300170619     C                   EVAL      CDPPTC   = SkSplitCSV_9(i)
245400170619     C                   ENDIF
245500170619     C*** CITY
245600170619     C                   IF        %trim(SkSplitFLD(i)) = 'CITYNAME'
245700170619     C                   EVAL      CDPCITY  = SkSplitCSV_9(i)
245800170619     C                   ENDIF
245900170619     C*** TEL
246000170619     C                   IF        %trim(SkSplitFLD(i)) = 'PHONE'
246100170619     C                   EVAL      CDPTEL   = SkSplitCSV_9(i)
246200170619     C                   ENDIF
246300170619     C*** FAX
246400170619     C                   IF        %trim(SkSplitFLD(i)) = 'FAX'
246500170619     C                   EVAL      CDPFAX   = SkSplitCSV_9(i)
246600170619     C                   ENDIF
246700170619     C*** EMAIL
246800170619     C                   IF        %trim(SkSplitFLD(i)) = 'MAIL'
246900170619     C                   EVAL      CDPEMAIL = SkSplitCSV_9(i)
247000170619     C                   ENDIF
247100170619     C*** WEB
247200170619     C                   IF        %trim(SkSplitFLD(i)) = 'WEB'
247300170619     C                   EVAL      CDPWEB   = SkSplitCSV_9(i)
247400170619     C                   ENDIF
247500170619     C*** BUCN
247600170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
247700170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
247800170619     C                   EXSR      CHKNUM
247900170619     C                   IF        PiInt=*on
248000170619     C                   Z-ADD     PiVal         CDPBUCN
248100170619     C                   ENDIF
248200170619     C                   ENDIF
248300170619     C*** GEOX
248400170619     C                   IF        %trim(SkSplitFLD(i)) = 'SGPSLAT'
248500170619     C                   EVAL      CDPGEOX  = SkSplitCSV_9(i)
248600170619     C                   ENDIF
248700170619     C*** GEOY
248800170619     C                   IF        %trim(SkSplitFLD(i)) = 'SGPSLONG'
248900170619     C                   EVAL      CDPGEOY  = SkSplitCSV_9(i)
249000170619     C                   ENDIF
249100170619     C*** DUMY
249200170619     C                   IF        %trim(SkSplitFLD(i)) = 'DUMMYDEPOT'
249300170619     C                   EVAL      CDPDUMY  = SkSplitCSV_9(i)
249400170619     C                   ENDIF
249500170619     C*
249600170619     C                   ENDIF
249700170619     C                   EVAL      i = i + 1
249800170619     C                   ENDDO
249900170619     C*
250000170619     C*** VER
250100170619     C                   EVAL      CDPVER = NEW_VERSION
250200170619     C*
250300170619     C* Infine inserisco il record corrente
250400170619     C                   WRITE     DPCDP100
250500170619     C*
250600170619     C                   ENDSR
250700170619     C***
250800170619
250900170619
251000170619     C*----------------------------------------------------*
251100170619     C*  GESTIONE MAPPATURA "R1DBU"
251200170619     C*----------------------------------------------------*
251300170619     C     GESCR1        BEGSR
251400170619     C*
251500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
251600170619     C                   CLEAR                   DPCR1100
251700170619     C*
251800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
251900170619     C                   Z-ADD     2             i                 4 0
252000170620     C                   DOW       i <= %elem(SkSplitFLD)
252100170620     C                             AND i <= OutIdxMax
252200170619     C*
252300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
252400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
252500170619     C                             SkSplitCSV_9(i) <> *blanks
252600170621     C*
252700170621     C*** ROUT / ROUPB / ROUPE
252800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
252900170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
253000170619     C                   ENDIF
253100170619     C*** TRADS
253200170619     C                   IF        %trim(SkSplitFLD(i)) = 'TRADESTATUS'
253300170619     C                   EVAL      CR1TRADS = SkSplitCSV_9(i)
253400170619     C                   ENDIF
253500170619     C*** ISO2
253600170619     C                   IF        %trim(SkSplitFLD(i)) =
253700170619     C                                        'DESTINATIONCOUNTRYCODE'
253800170619     C                   EVAL      CR1ISO2  = SkSplitCSV_9(i)
253900170619     C                   ENDIF
254000170619     C*** PTCB
254100170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
254200170619     C                   EVAL      CR1PTCB  = SkSplitCSV_9(i)
254300170619     C                   ENDIF
254400170619     C*** PTCE
254500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
254600170619     C                   EVAL      CR1PTCE  = SkSplitCSV_9(i)
254700170619     C                   ENDIF
254800170619     C*** NTWP
254900170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
255000170619     C                   EVAL      CR1NTWP  = SkSplitCSV_9(i)
255100170619     C                   ENDIF
255200170619     C*** PTY
255300170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
255400170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
255500170619     C                   EXSR      CHKNUM
255600170619     C                   IF        PiInt=*on
255700170619     C                   Z-ADD     PiVal         CR1PTY
255800170619     C                   ENDIF
255900170619     C                   ENDIF
256000170619     C*** BUCN
256100170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
256200170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
256300170619     C                   EXSR      CHKNUM
256400170619     C                   IF        PiInt=*on
256500170619     C                   Z-ADD     PiVal         CR1BUCN
256600170619     C                   ENDIF
256700170619     C                   ENDIF
256800170619     C*
256900170619     C                   ENDIF
257000170619     C                   EVAL      i = i + 1
257100170619     C                   ENDDO
257200170619     C*
257300170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
257400170619     C                   IF        CR1PTCE = *blanks
257500170619     C                   EVAL      CR1PTCE = CR1PTCB
257600170619     C                   ENDIF
257700170619     C*
257800170619     C*** VER
257900170619     C                   EVAL      CR1VER = NEW_VERSION
258000170620     C*
258100170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
258200170620     C                   EVAL      InStringa = w2048_01
258300170620     C                   EVAL      InSepara = ','
258400170620     C                   SETOFF                                       131419
258500170620     C                   SETON                                        13
258600170620     C                   EXSR      SR_SPLIT
258700170620     C*
258800170620     C* Se nessun elemento => scrivo comunque un record singolo
258900170620     C                   IF        OutIdxMax = *zeros
259000170620     C                   WRITE     DPCR1100
259100170620     C                   ELSE
259200170620     C*
259300170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
259400170620     C                   Z-ADD     1             j                 4 0
259500170620     C                   DOW       j <= %elem(SkSplitCSV_3)
259600170620     C                             AND j <= OutIdxMax
259700170620     C*
259800170620     C* Determino il tipo di Routing Place
259900170620     C                   SELECT
260000170620     C* Injection Code - Range
260100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
260200170620     C                   EVAL      CR1ROUT  = 'I'
260300170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):3:9)
260400170620     C                   EVAL      CR1ROUPE = %subst(SkSplitCSV_3(j):12:9)
260500170620     C* Injection Code - Single
260600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
260700170620     C                   EVAL      CR1ROUT  = 'I'
260800170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:9)
260900170620     C                   EVAL      CR1ROUPE = CR1ROUPB
261000170620     C* Country Code
261100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
261200170620     C                   EVAL      CR1ROUT  = 'C'
261300170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:2)
261400170620     C                   EVAL      CR1ROUPE = CR1ROUPB
261500170620     C* Depot Code - Range
261600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
261700170620     C                   EVAL      CR1ROUT  = 'D'
261800170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):3:7)
261900170620     C                   EVAL      CR1ROUPE = %subst(SkSplitCSV_3(j):10:7)
262000170620     C* Depot Code - Single
262100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
262200170620     C                   EVAL      CR1ROUT  = 'D'
262300170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:7)
262400170620     C                   EVAL      CR1ROUPE = CR1ROUPB
262500170620     C* Depot Group
262600170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
262700170620     C                   EVAL      CR1ROUT  = 'G'
262800170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:4)
262900170620     C                   EVAL      CR1ROUPE = CR1ROUPB
263000170620     C* Business Unit
263100170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
263200170620     C                   EVAL      CR1ROUT  = 'B'
263300170620     C                   EVAL      CR1ROUPB = %subst(SkSplitCSV_3(j):2:3)
263400170620     C                   EVAL      CR1ROUPE = CR1ROUPB
263500170620     C*
263600170620     C                   ENDSL
263700170619     C*
263800170619     C* Infine inserisco il record corrente
263900170619     C                   WRITE     DPCR1100
264000170620     C*
264100170620     C                   EVAL      j = j + 1
264200170620     C                   ENDDO
264300170620     C                   ENDIF
264400170619     C*
264500170619     C                   ENDSR
264600170619     C***
264700170619
264800170619
264900170619     C*----------------------------------------------------*
265000170619     C*  GESTIONE MAPPATURA "R2ROUTE"
265100170619     C*----------------------------------------------------*
265200170619     C     GESCR2        BEGSR
265300170619     C*
265400170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
265500170619     C                   CLEAR                   DPCR2100
265600170619     C*
265700170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
265800170619     C                   Z-ADD     2             i                 4 0
265900170620     C                   DOW       i <= %elem(SkSplitFLD)
266000170620     C                             AND i <= OutIdxMax
266100170619     C*
266200170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
266300170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
266400170619     C                             SkSplitCSV_9(i) <> *blanks
266500170619     C*
266600170621     C*** ROUT / ROUPB / ROUPE
266700170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
266800170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
266900170619     C                   ENDIF
267000170619     C*** BUCN
267100170619     C                   IF        %trim(SkSplitFLD(i)) = 'BUCODE'
267200170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
267300170619     C                   EXSR      CHKNUM
267400170619     C                   IF        PiInt=*on
267500170619     C                   Z-ADD     PiVal         CR2BUCN
267600170619     C                   ENDIF
267700170619     C                   ENDIF
267800170619     C*** ISO2
267900170619     C                   IF        %trim(SkSplitFLD(i)) =
268000170619     C                                        'DESTINATIONCOUNTRYCODE'
268100170619     C                   EVAL      CR2ISO2  = SkSplitCSV_9(i)
268200170619     C                   ENDIF
268300170619     C*** TRADS
268400170619     C                   IF        %trim(SkSplitFLD(i)) = 'TRADESTATUS'
268500170619     C                   EVAL      CR2TRADS = SkSplitCSV_9(i)
268600170619     C                   ENDIF
268700170619     C*** PTCB
268800170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
268900170619     C                   EVAL      CR2PTCB  = SkSplitCSV_9(i)
269000170619     C                   ENDIF
269100170619     C*** PTCE
269200170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
269300170619     C                   EVAL      CR2PTCE  = SkSplitCSV_9(i)
269400170619     C                   ENDIF
269500170619     C*** NTWP
269600170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
269700170619     C                   EVAL      CR2NTWP  = SkSplitCSV_9(i)
269800170619     C                   ENDIF
269900170619     C*** PTY
270000170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
270100170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
270200170619     C                   EXSR      CHKNUM
270300170619     C                   IF        PiInt=*on
270400170619     C                   Z-ADD     PiVal         CR2PTY
270500170619     C                   ENDIF
270600170619     C                   ENDIF
270700170619     C*** DDPT
270800170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
270900170619     C                   EVAL      CR2DDPT  = SkSplitCSV_9(i)
271000170619     C                   ENDIF
271100170619     C*** SSORT
271200170619     C                   IF        %trim(SkSplitFLD(i)) = 'SSORT'
271300170619     C                   EVAL      CR2SSORT = SkSplitCSV_9(i)
271400170619     C                   ENDIF
271500170619     C*** PTNC
271600170619     C                   IF        %trim(SkSplitFLD(i)) = 'PARTNERCODE'
271700170619     C                   EVAL      CR2PTNC  = SkSplitCSV_9(i)
271800170619     C                   ENDIF
271900170619     C*
272000170619     C                   ENDIF
272100170619     C                   EVAL      i = i + 1
272200170619     C                   ENDDO
272300170619     C*
272400170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
272500170619     C                   IF        CR2PTCE = *blanks
272600170619     C                   EVAL      CR2PTCE = CR2PTCB
272700170619     C                   ENDIF
272800170619     C*
272900170619     C*** VER
273000170619     C                   EVAL      CR2VER = NEW_VERSION
273100170620     C*
273200170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
273300170620     C                   EVAL      InStringa = w2048_01
273400170620     C                   EVAL      InSepara = ','
273500170620     C                   SETOFF                                       131419
273600170620     C                   SETON                                        13
273700170620     C                   EXSR      SR_SPLIT
273800170620     C*
273900170620     C* Se nessun elemento => scrivo comunque un record singolo
274000170620     C                   IF        OutIdxMax = *zeros
274100170620     C                   WRITE     DPCR2100
274200170620     C                   ELSE
274300170620     C*
274400170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
274500170620     C                   Z-ADD     1             j                 4 0
274600170620     C                   DOW       j <= %elem(SkSplitCSV_3)
274700170620     C                             AND j <= OutIdxMax
274800170620     C*
274900170620     C* Determino il tipo di Routing Place
275000170620     C                   SELECT
275100170620     C* Injection Code - Range
275200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
275300170620     C                   EVAL      CR2ROUT  = 'I'
275400170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):3:9)
275500170620     C                   EVAL      CR2ROUPE = %subst(SkSplitCSV_3(j):12:9)
275600170620     C* Injection Code - Single
275700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
275800170620     C                   EVAL      CR2ROUT  = 'I'
275900170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:9)
276000170620     C                   EVAL      CR2ROUPE = CR2ROUPB
276100170620     C* Country Code
276200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
276300170620     C                   EVAL      CR2ROUT  = 'C'
276400170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:2)
276500170620     C                   EVAL      CR2ROUPE = CR2ROUPB
276600170620     C* Depot Code - Range
276700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
276800170620     C                   EVAL      CR2ROUT  = 'D'
276900170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):3:7)
277000170620     C                   EVAL      CR2ROUPE = %subst(SkSplitCSV_3(j):10:7)
277100170620     C* Depot Code - Single
277200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
277300170620     C                   EVAL      CR2ROUT  = 'D'
277400170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:7)
277500170620     C                   EVAL      CR2ROUPE = CR2ROUPB
277600170620     C* Depot Group
277700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
277800170620     C                   EVAL      CR2ROUT  = 'G'
277900170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:4)
278000170620     C                   EVAL      CR2ROUPE = CR2ROUPB
278100170620     C* Business Unit
278200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
278300170620     C                   EVAL      CR2ROUT  = 'B'
278400170620     C                   EVAL      CR2ROUPB = %subst(SkSplitCSV_3(j):2:3)
278500170620     C                   EVAL      CR2ROUPE = CR2ROUPB
278600170620     C*
278700170620     C                   ENDSL
278800170620     C*
278900170620     C* Infine inserisco il record corrente
279000170620     C                   WRITE     DPCR2100
279100170620     C*
279200170620     C                   EVAL      j = j + 1
279300170620     C                   ENDDO
279400170620     C                   ENDIF
279500170619     C*
279600170619     C                   ENDSR
279700170619     C***
279800170619
279900170619
280000170619     C*----------------------------------------------------*
280100170619     C*  GESTIONE MAPPATURA "R3OSORT"
280200170619     C*----------------------------------------------------*
280300170619     C     GESCR3        BEGSR
280400170619     C*
280500170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
280600170619     C                   CLEAR                   DPCR3100
280700170619     C*
280800170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
280900170619     C                   Z-ADD     2             i                 4 0
281000170620     C                   DOW       i <= %elem(SkSplitFLD)
281100170620     C                             AND i <= OutIdxMax
281200170619     C*
281300170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
281400170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
281500170619     C                             SkSplitCSV_9(i) <> *blanks
281600170619     C*
281700170621     C*** ROUT / ROUPB / ROUPE
281800170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
281900170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
282000170619     C                   ENDIF
282100170619     C*** ISO2
282200170619     C                   IF        %trim(SkSplitFLD(i)) =
282300170619     C                                        'DESTINATIONCOUNTRYCODE'
282400170619     C                   EVAL      CR3ISO2  = SkSplitCSV_9(i)
282500170619     C                   ENDIF
282600170619     C*** DDPT
282700170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
282800170619     C                   EVAL      CR3DDPT  = SkSplitCSV_9(i)
282900170619     C                   ENDIF
283000170619     C*** PTCB
283100170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
283200170619     C                   EVAL      CR3PTCB  = SkSplitCSV_9(i)
283300170619     C                   ENDIF
283400170619     C*** PTCE
283500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
283600170619     C                   EVAL      CR3PTCE  = SkSplitCSV_9(i)
283700170619     C                   ENDIF
283800170619     C*** NTWP
283900170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
284000170619     C                   EVAL      CR3NTWP  = SkSplitCSV_9(i)
284100170619     C                   ENDIF
284200170619     C*** PTY
284300170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
284400170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
284500170619     C                   EXSR      CHKNUM
284600170619     C                   IF        PiInt=*on
284700170619     C                   Z-ADD     PiVal         CR3PTY
284800170619     C                   ENDIF
284900170619     C                   ENDIF
285000170619     C*** OSORT
285100170619     C                   IF        %trim(SkSplitFLD(i)) = 'OSORT'
285200170620     C                   EVAL      CR3OSORT = SkSplitCSV_9(i)
285300170619     C                   ENDIF
285400170619     C*
285500170619     C                   ENDIF
285600170619     C                   EVAL      i = i + 1
285700170619     C                   ENDDO
285800170619     C*
285900170619     C* Se POSTAL CODE END = *blanks => si assume POSTAL CODE END = POSTAL CODE FROM
286000170619     C                   IF        CR3PTCE = *blanks
286100170619     C                   EVAL      CR3PTCE = CR3PTCB
286200170619     C                   ENDIF
286300170619     C*
286400170619     C*** VER
286500170619     C                   EVAL      CR3VER = NEW_VERSION
286600170620     C*
286700170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
286800170620     C                   EVAL      InStringa = w2048_01
286900170620     C                   EVAL      InSepara = ','
287000170620     C                   SETOFF                                       131419
287100170620     C                   SETON                                        13
287200170620     C                   EXSR      SR_SPLIT
287300170620     C*
287400170620     C* Se nessun elemento => scrivo comunque un record singolo
287500170620     C                   IF        OutIdxMax = *zeros
287600170620     C                   WRITE     DPCR3100
287700170620     C                   ELSE
287800170620     C*
287900170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
288000170620     C                   Z-ADD     1             j                 4 0
288100170620     C                   DOW       j <= %elem(SkSplitCSV_3)
288200170620     C                             AND j <= OutIdxMax
288300170620     C*
288400170620     C* Determino il tipo di Routing Place
288500170620     C                   SELECT
288600170620     C* Injection Code - Range
288700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
288800170620     C                   EVAL      CR3ROUT  = 'I'
288900170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):3:9)
289000170620     C                   EVAL      CR3ROUPE = %subst(SkSplitCSV_3(j):12:9)
289100170620     C* Injection Code - Single
289200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
289300170620     C                   EVAL      CR3ROUT  = 'I'
289400170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:9)
289500170620     C                   EVAL      CR3ROUPE = CR3ROUPB
289600170620     C* Country Code
289700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
289800170620     C                   EVAL      CR3ROUT  = 'C'
289900170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:2)
290000170620     C                   EVAL      CR3ROUPE = CR3ROUPB
290100170620     C* Depot Code - Range
290200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
290300170620     C                   EVAL      CR3ROUT  = 'D'
290400170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):3:7)
290500170620     C                   EVAL      CR3ROUPE = %subst(SkSplitCSV_3(j):10:7)
290600170620     C* Depot Code - Single
290700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
290800170620     C                   EVAL      CR3ROUT  = 'D'
290900170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:7)
291000170620     C                   EVAL      CR3ROUPE = CR3ROUPB
291100170620     C* Depot Group
291200170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
291300170620     C                   EVAL      CR3ROUT  = 'G'
291400170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:4)
291500170620     C                   EVAL      CR3ROUPE = CR3ROUPB
291600170620     C* Business Unit
291700170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
291800170620     C                   EVAL      CR3ROUT  = 'B'
291900170620     C                   EVAL      CR3ROUPB = %subst(SkSplitCSV_3(j):2:3)
292000170620     C                   EVAL      CR3ROUPE = CR3ROUPB
292100170620     C*
292200170620     C                   ENDSL
292300170620     C*
292400170620     C* Infine inserisco il record corrente
292500170620     C                   WRITE     DPCR3100
292600170620     C*
292700170620     C                   EVAL      j = j + 1
292800170620     C                   ENDDO
292900170620     C                   ENDIF
293000170619     C*
293100170619     C                   ENDSR
293200170619     C***
293300170619
293400170619
293500170619     C*----------------------------------------------------*
293600170619     C*  GESTIONE MAPPATURA "R4CSORT"
293700170619     C*----------------------------------------------------*
293800170619     C     GESCR4        BEGSR
293900170619     C*
294000170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
294100170619     C                   CLEAR                   DPCR4100
294200170619     C*
294300170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
294400170619     C                   Z-ADD     2             i                 4 0
294500170620     C                   DOW       i <= %elem(SkSplitFLD)
294600170620     C                             AND i <= OutIdxMax
294700170619     C*
294800170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
294900170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
295000170619     C                             SkSplitCSV_9(i) <> *blanks
295100170619     C*
295200170621     C*** ROUT / ROUPB / ROUPE
295300170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
295400170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
295500170619     C                   ENDIF
295600170619     C*** ISO2
295700170619     C                   IF        %trim(SkSplitFLD(i)) =
295800170619     C                                        'DESTINATIONCOUNTRYCODE'
295900170619     C                   EVAL      CR4ISO2  = SkSplitCSV_9(i)
296000170619     C                   ENDIF
296100170619     C*** DDPT
296200170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
296300170619     C                   EVAL      CR4DDPT  = SkSplitCSV_9(i)
296400170619     C                   ENDIF
296500170619     C*** NTWP
296600170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
296700170619     C                   EVAL      CR4NTWP  = SkSplitCSV_9(i)
296800170619     C                   ENDIF
296900170619     C*** PTY
297000170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
297100170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
297200170619     C                   EXSR      CHKNUM
297300170619     C                   IF        PiInt=*on
297400170619     C                   Z-ADD     PiVal         CR4PTY
297500170619     C                   ENDIF
297600170619     C                   ENDIF
297700170619     C*** CSORT
297800170619     C                   IF        %trim(SkSplitFLD(i)) = 'CSORT'
297900170620     C                   EVAL      CR4CSORT = SkSplitCSV_9(i)
298000170619     C                   ENDIF
298100170619     C*
298200170619     C                   ENDIF
298300170619     C                   EVAL      i = i + 1
298400170619     C                   ENDDO
298500170619     C*
298600170619     C*** VER
298700170619     C                   EVAL      CR4VER = NEW_VERSION
298800170620     C*
298900170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
299000170620     C                   EVAL      InStringa = w2048_01
299100170620     C                   EVAL      InSepara = ','
299200170620     C                   SETOFF                                       131419
299300170620     C                   SETON                                        13
299400170620     C                   EXSR      SR_SPLIT
299500170620     C*
299600170620     C* Se nessun elemento => scrivo comunque un record singolo
299700170620     C                   IF        OutIdxMax = *zeros
299800170620     C                   WRITE     DPCR4100
299900170620     C                   ELSE
300000170620     C*
300100170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
300200170620     C                   Z-ADD     1             j                 4 0
300300170620     C                   DOW       j <= %elem(SkSplitCSV_3)
300400170620     C                             AND j <= OutIdxMax
300500170620     C*
300600170620     C* Determino il tipo di Routing Place
300700170620     C                   SELECT
300800170620     C* Injection Code - Range
300900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
301000170620     C                   EVAL      CR4ROUT  = 'I'
301100170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):3:9)
301200170620     C                   EVAL      CR4ROUPE = %subst(SkSplitCSV_3(j):12:9)
301300170620     C* Injection Code - Single
301400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
301500170620     C                   EVAL      CR4ROUT  = 'I'
301600170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:9)
301700170620     C                   EVAL      CR4ROUPE = CR4ROUPB
301800170620     C* Country Code
301900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
302000170620     C                   EVAL      CR4ROUT  = 'C'
302100170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:2)
302200170620     C                   EVAL      CR4ROUPE = CR4ROUPB
302300170620     C* Depot Code - Range
302400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
302500170620     C                   EVAL      CR4ROUT  = 'D'
302600170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):3:7)
302700170620     C                   EVAL      CR4ROUPE = %subst(SkSplitCSV_3(j):10:7)
302800170620     C* Depot Code - Single
302900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
303000170620     C                   EVAL      CR4ROUT  = 'D'
303100170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:7)
303200170620     C                   EVAL      CR4ROUPE = CR4ROUPB
303300170620     C* Depot Group
303400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
303500170620     C                   EVAL      CR4ROUT  = 'G'
303600170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:4)
303700170620     C                   EVAL      CR4ROUPE = CR4ROUPB
303800170620     C* Business Unit
303900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
304000170620     C                   EVAL      CR4ROUT  = 'B'
304100170620     C                   EVAL      CR4ROUPB = %subst(SkSplitCSV_3(j):2:3)
304200170620     C                   EVAL      CR4ROUPE = CR4ROUPB
304300170620     C*
304400170620     C                   ENDSL
304500170620     C*
304600170620     C* Infine inserisco il record corrente
304700170620     C                   WRITE     DPCR4100
304800170620     C*
304900170620     C                   EVAL      j = j + 1
305000170620     C                   ENDDO
305100170620     C                   ENDIF
305200170619     C*
305300170619     C                   ENDSR
305400170619     C***
305500170619
305600170619
305700170619     C*----------------------------------------------------*
305800170619     C*  GESTIONE MAPPATURA "R5DSORT"
305900170619     C*----------------------------------------------------*
306000170619     C     GESCR5        BEGSR
306100170619     C*
306200170619     C* Inizializzo il formato record del file relativo alla mappatura corrente
306300170619     C                   CLEAR                   DPCR5100
306400170619     C*
306500170619     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
306600170619     C                   Z-ADD     2             i                 4 0
306700170620     C                   DOW       i <= %elem(SkSplitFLD)
306800170620     C                             AND i <= OutIdxMax
306900170619     C*
307000170619     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
307100170619     C                   IF        SkSplitFLD(i)   <> *blanks AND
307200170619     C                             SkSplitCSV_9(i) <> *blanks
307300170619     C*
307400170621     C*** ROUT / ROUPB / ROUPE
307500170619     C                   IF        %trim(SkSplitFLD(i)) = 'ROUTINGPLACES'
307600170620     C                   EVAL      w2048_01 = SkSplitCSV_9(i)
307700170619     C                   ENDIF
307800170619     C*** DDPT
307900170619     C                   IF        %trim(SkSplitFLD(i)) = 'DDEPOT'
308000170619     C                   EVAL      CR5DDPT  = SkSplitCSV_9(i)
308100170619     C                   ENDIF
308200170619     C*** ISO2
308300170619     C                   IF        %trim(SkSplitFLD(i)) =
308400170619     C                                        'DESTINATIONCOUNTRYCODE'
308500170619     C                   EVAL      CR5ISO2  = SkSplitCSV_9(i)
308600170619     C                   ENDIF
308700170619     C*** PTCB
308800170619     C                   IF        %trim(SkSplitFLD(i)) = 'BEGINPOSTCODE'
308900170619     C                   EVAL      CR5PTCB  = SkSplitCSV_9(i)
309000170619     C                   ENDIF
309100170619     C*** PTCE
309200170619     C                   IF        %trim(SkSplitFLD(i)) = 'ENDPOSTCODE'
309300170619     C                   EVAL      CR5PTCE  = SkSplitCSV_9(i)
309400170619     C                   ENDIF
309500170619     C*** NTWP
309600170619     C                   IF        %trim(SkSplitFLD(i)) = 'NETWORKPLANID'
309700170619     C                   EVAL      CR5NTWP  = SkSplitCSV_9(i)
309800170619     C                   ENDIF
309900170619     C*** PTY
310000170619     C                   IF        %trim(SkSplitFLD(i)) = 'PRIORITY'
310100170619     C                   EVAL      PiStr = SkSplitCSV_9(i)
310200170619     C                   EXSR      CHKNUM
310300170619     C                   IF        PiInt=*on
310400170619     C                   Z-ADD     PiVal         CR5PTY
310500170619     C                   ENDIF
310600170619     C                   ENDIF
310700170619     C*** DSORT
310800170619     C                   IF        %trim(SkSplitFLD(i)) = 'DSORT'
310900170619     C                   EVAL      CR5DSORT = SkSplitCSV_9(i)
311000170619     C                   ENDIF
311100170619     C*
311200170619     C                   ENDIF
311300170619     C                   EVAL      i = i + 1
311400170619     C                   ENDDO
311500170619     C*
311600170619     C*** VER
311700170621     C                   EVAL      CR5VER = NEW_VERSION
311800170620     C*
311900170620     C* Splitto il campo "lista" e lo sviluppo in tutti i suoi elementi (e reltivi tipi)
312000170620     C                   EVAL      InStringa = w2048_01
312100170620     C                   EVAL      InSepara = ','
312200170620     C                   SETOFF                                       131419
312300170620     C                   SETON                                        13
312400170620     C                   EXSR      SR_SPLIT
312500170620     C*
312600170620     C* Se nessun elemento => scrivo comunque un record singolo
312700170620     C                   IF        OutIdxMax = *zeros
312800170620     C                   WRITE     DPCR5100
312900170620     C                   ELSE
313000170620     C*
313100170620     C* Sviluppo tutti gli elementi della lista dei Routing Places
313200170620     C                   Z-ADD     1             j                 4 0
313300170620     C                   DOW       j <= %elem(SkSplitCSV_3)
313400170620     C                             AND j <= OutIdxMax
313500170620     C*
313600170620     C* Determino il tipo di Routing Place
313700170620     C                   SELECT
313800170620     C* Injection Code - Range
313900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'IR'
314000170620     C                   EVAL      CR5ROUT  = 'I'
314100170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):3:9)
314200170620     C                   EVAL      CR5ROUPE = %subst(SkSplitCSV_3(j):12:9)
314300170620     C* Injection Code - Single
314400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'I'
314500170620     C                   EVAL      CR5ROUT  = 'I'
314600170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:9)
314700170620     C                   EVAL      CR5ROUPE = CR5ROUPB
314800170620     C* Country Code
314900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'C'
315000170620     C                   EVAL      CR5ROUT  = 'C'
315100170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:2)
315200170620     C                   EVAL      CR5ROUPE = CR5ROUPB
315300170620     C* Depot Code - Range
315400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:2) = 'DR'
315500170620     C                   EVAL      CR5ROUT  = 'D'
315600170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):3:7)
315700170620     C                   EVAL      CR5ROUPE = %subst(SkSplitCSV_3(j):10:7)
315800170620     C* Depot Code - Single
315900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'D'
316000170620     C                   EVAL      CR5ROUT  = 'D'
316100170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:7)
316200170620     C                   EVAL      CR5ROUPE = CR5ROUPB
316300170620     C* Depot Group
316400170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'G'
316500170620     C                   EVAL      CR5ROUT  = 'G'
316600170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:4)
316700170620     C                   EVAL      CR5ROUPE = CR5ROUPB
316800170620     C* Business Unit
316900170620     C                   WHEN      %subst(SkSplitCSV_3(j):1:1) = 'B'
317000170620     C                   EVAL      CR5ROUT  = 'B'
317100170620     C                   EVAL      CR5ROUPB = %subst(SkSplitCSV_3(j):2:3)
317200170620     C                   EVAL      CR5ROUPE = CR5ROUPB
317300170620     C*
317400170620     C                   ENDSL
317500170620     C*
317600170620     C* Infine inserisco il record corrente
317700170620     C                   WRITE     DPCR5100
317800170620     C*
317900170620     C                   EVAL      j = j + 1
318000170620     C                   ENDDO
318100170620     C                   ENDIF
318200170619     C*
318300170619     C                   ENDSR
318400170620     C***
318500170616
318600170616
318700141028     C*----------------------------------------------------*
318800141028     C*  CONTROLLO NUMERICITA' CAMPI
318900141028     C*----------------------------------------------------*
319000141028     C     CHKNUM        BEGSR
319100141028     C*
319200141028     C                   IF        PiDecChr = *blanks
319300141028     C                   EVAL      PiDecChr = CharNUM
319400141028     C                   ENDIF
319500141028     C*
319600141028     C                   callp     UBISNUM_Check(PiStr
319700141028     C                                          :PiDecChr
319800141028     C                                          :PiVal
319900141028     C                                          :PiNum
320000141028     C                                          :PiInt)
320100141028     C*
320200141028     C                   ENDSR
320300141028     C***
320400141028
320500141028
320600141028     C*----------------------------------------------------*
320700141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
320800141028     C*----------------------------------------------------*
320900141028     C     SR_SPLIT      BEGSR
321000141028     C*
321100141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
321200170620     C   13              EVAL      InTypeOut = '3'
321300170620     C   14              EVAL      InTypeOut = '4'
321400170620     C   19              EVAL      InTypeOut = '9'
321500170616     C                   CALL      'XSPLIT3'
321600141028     C                   PARM                    InStringa
321700141028     C                   PARM                    InSepara
321800170616     C                   PARM                    InTypeOut
321900141028     C                   PARM      ''            wSkParam
322000170620     C                   PARM                    OutIdxMax
322100141028     C                   PARM                    OutErrore
322200141028     C                   PARM                    OutMsg
322300170620     C   13              MOVEA     wSkParam      SkSplitCSV_3
322400170620     C   14              MOVEA     wSkParam      SkSplitCSV_4
322500170620     C   19              MOVEA     wSkParam      SkSplitCSV_9
322600141028     C*
322700141028     C                   ENDSR
322800160801
322900160801
323000160801     C*--------------------------------------------------------
323100160801     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
323200160801     C*--------------------------------------------------------
323300160801     C     CARTAB        BEGSR
323400160801     C*
323500160801     C* TABELLA '15' - NAZIONI
323600160801     C                   clear                   skNAZISO
323700160801     C                   clear                   skNAZBAR
323800160801     C                   eval      tblKUT = 1
323900160801     C                   eval      tblCOD = '15'
324000170616     C     KEYtab_P      setll     tabel00f
324100170616     C     KEYtab_P      reade     tabel00f
324200160801     C                   dow       not %eof(tabel00f)
324300160801     C                   if        tblFLG = *blanks
324400160801     C                   movel(p)  tblUNI        ds15
324500160801     C                   add       1             jNAZ
324600160801     C                   eval      skNAZBAR(jNAZ) = tblKEY
324700170620     C                   eval      skNAZISO(jNAZ) = %trim(§15COD)
324800160801     C                   endif
324900170616     C     KEYtab_P      reade     tabel00f
325000160801     C                   enddo
325100160801     C*
325200160801     C                   ENDSR
325300160801     C***
325400070411
325500990910
325600000613     C     *inzsr        BEGSR
325700000613     C*
325800170616     C* CALCOLA LA VARIE DATE DI RIFERIMENTO
325900170616     C                   z-add     *zeros        oggi              8 0
326000170616     C                   z-add     *zeros        domani            8 0
326100170616     C                   eval      oggi   = %dec(%date() : *ISO)
326200170616     C                   eval      domani = %dec(%date()+%days(1) : *ISO)
326300160801     C*
326400170616     C* Chiave su TABEL00F - Parziale
326500170616     C     KEYtab_P      KLIST
326600160801     C                   KFLD                    tblKUT
326700160801     C                   KFLD                    tblCOD
326800170616     C*
326900170616     C* Chiave su DPCCC00F - Completa
327000170616     C     KEYccc_11C    KLIST
327100170616     C                   KFLD                    cccATB
327200170616     C                   KFLD                    cccVER
327300170616     C                   KFLD                    cccISO2
327400170616     C*
327500170616     C* Chiave su DPCST00F - Completa
327600170616     C     KEYcst_11C    KLIST
327700170616     C                   KFLD                    cstATB
327800170616     C                   KFLD                    cstVER
327900170616     C                   KFLD                    cstISO2
328000170616     C                   KFLD                    cstREGC
328100170616     C                   KFLD                    cstSTAC
328200170616     C*
328300170616     C* Chiave su DPCEL00F - Completa
328400170616     C     KEYcel_11C    KLIST
328500170616     C                   KFLD                    celATB
328600170616     C                   KFLD                    celVER
328700170616     C                   KFLD                    celELMC
328800170619     C*
328900170619     C* Chiave su DPCSO00F - Completa
329000170619     C     KEYcso_11C    KLIST
329100170619     C                   KFLD                    csoATB
329200170619     C                   KFLD                    csoVER
329300170619     C                   KFLD                    csoSORC
329400000830     C*
329500000613     C                   ENDSR
329600000613     C***
