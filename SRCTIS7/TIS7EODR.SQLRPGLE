000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200170726     H DFTACTGRP(*NO) ACTGRP('BARTVAS')
000300150420     H DECEDIT('0,') DATEDIT(*DMY.)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140114     FTIVGD00F  O    E             DISK
000700160906     Ftis7prgf  uF   E             DISK    rename(tis7prgf:tis7prg0)
000800160906     F                                     prefix(f_)
000900010330     D*----------------------------------------------------
001000010330     D* DICHIARAZIOINE VARIABILI DI WRK
001100010330     D*----------------------------------------------------
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500010330     D esito           s              1
001600010330     D prmlit          s             10
001700010330     D prmfir          s             10
001800170512     D pptTIP          s              2
001900170512     D FIVABDS       e ds                  extname(fivabwwr)
002000160906     D wVABCTR_E       s                   like(VABCTR) inz
002100150420     D wrkesito        s                   like(esito)
002200160902     D s_vindta        s                   like(vindta) inz(*blanks)
002300160902
002400160902
002500160902     D*------------------
002600160902     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
002700160902     D*------------------
002800160902     D InStringa       S          65535A   VARYING                              (stringa input)
002900160902     D InSepara        S             10A                                        (separatore)
003000160902     D InTypeOut       S              1A                                        (tipo output)
003100160902     D wSkParam        S          65535A                                        (output)
003200160902     D OutErrore       S              1A                                        (flag errore)
003300160902     D OutMsg          S             80A                                        (messaggio errore)
003400160902     D SkSplitCSV_5    S            256    DIM(256)
003500160902
003600160902
003700030715     D*------------------
003800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003900030715     D*------------------
004000160902     D SkSplitFLD      S                   LIKE(SkSplitCSV_5) DIM(256)
004100160902     D SkSplitCSV      S                   LIKE(SkSplitCSV_5) DIM(256)
004200160902     D CharCSV         S              1
004300160902     D CharTXT         S              1
004400160902     D CharNUM         S              1
004500160902     D CharSOS         S              1
004600160902     D i               s              3  0 INZ(1)
004700030715     D wGiro           s              1  0 INZ(*zeros)
004800170512
004900170512
005000030822     D*------------------
005100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005200030822     D*------------------
005300160902     D posDaDft        S              3  0 INZ(*zeros)
005400160902     D posADft         S              3  0 INZ(*zeros)
005500160902     D wGiroDft        s              1  0 INZ(*zeros)
005600170512
005700170512
005800170512     D*--------------------
005900170512     D* INTERFACCIA AL DRIVER RICERCA SPEDIZIONE PER RMN/RMA
006000170512     D*--------------------
006100170512     D/COPY GAITRASRC/SRCPROTOPI,UBWS775R
006200170515     D*--------------------
006300170515     D* INTERFACCIA AL DRIVER RICERCA CODICE UNIFICANTE
006400170515     D*--------------------
006500170515     D/COPY GAITRASRC/SRCPROTOPI,UBBS10R
006600170512
006700170512
006800170512     D*--------------------
006900170512     D* STRUTTURE PER CHIAMATA AL DROVER GENERAZIONE FNVAC
007000170512     D*--------------------
007100170512     D FNVACDS       E DS                  EXTNAME(FNVAC00T)
007200170512     D*--------------------
007300170512     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
007400170512     D*--------------------
007500170512     D DSINPUT         DS
007600170512     D  DATADA                        8  0
007700170512     D  DATAA                         8  0
007800170512     D  CODCLI                        7  0
007900170512     D  FLGUNI                        1
008000170512     D  FLGCONS                       1
008100170512     D  FLGVAS                        1
008200170512     D  FLGOPZ                        1
008300170512     D  CODCLIVAS                     7
008400170512     D  TIPFILE                       2
008500170512     D  FLGEXE                        1
008600170512     D  TIPCLI                        1
008700170512     D  FLGNOE                        1
008800170512     D  KAAS                          4  0
008900170512     D  KLNP                          3  0
009000170512     D  KNRS                          2  0
009100170512     D  KNSP                          7  0
009200170512     D  DACMDA                        8  0
009300170512     D  DACMA                         8  0
009400170512     D*--------------------
009500160908
009600160908
009700031201     D*------------------
009800031201     D* Costanti
009900031201     D*------------------
010000160902     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010100160908     D MAIU            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010200150420
010300010330
010400010330
010500990921     C                   reset                   esito
010600990921     C                   reset                   wrkesito
010700010601     C*
010800160908     C                   exsr      defcam
010900170512     C                   exsr      calprog
011000150420     C                   exsr      rwfile
011100170512     C                   exsr      strvgd
011200160906     C*
011300010601     C                   seton                                        lr
011400160906
011500160906
011600160906     C*--------------------------------------------------------
011700160906     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
011800160906     C*--------------------------------------------------------
011900160906     C     STRVGD        BEGSR
012000160906     C*
012100160906     C
012200170119     C/EXEC SQL
012300170227     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
012400170119     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
012500170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
012600170119     C+       VGDTSC = :VGDTSC
012700170119     C/END-EXEC
012800160906     C
012900160906     C*
013000160906     C                   ENDSR
013100160906     C***
013200010601
013300010601
013400010330     C*--------------------------------------------------------
013500150420     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIVABWWF              *
013600010330     C*--------------------------------------------------------
013700150420     C     RWFILE        BEGSR
013800010330     C*
013900010330     C                   if        not %open(tivin00r)
014000010330     C                   open      tivin00r
014100010330     C                   endif
014200170515     C*
014300170515     C* Reperisco subito il Codice Padre del KSC VAS corrente
014400170515     C                   movel     *zeros        wCodUnif          7 0
014500170515     C                   eval      iBS10CLI  = %dec(%subst(vlrKSC:2:7):7:0)
014600170515     C                   exsr      rtvUNI
014700170515     C                   eval      wCodUnif = oBS10COP
014800160908     C*
014900030822     C                   do        *HIVAL
015000010330     C*
015100030822     C                   read      tivin00r                               70
015200160902     C*
015300160902     C* Salvo il buffer orginale ricevuto dal cliente
015400160902     C                   eval      s_vindta = vindta
015500160902     C*
015600160902     C                   if        *in70 = *off and
015700160902     C                             vindta <> *blanks and
015800160902     C                             (vinflg = *blanks
015900160902     C                              or vinflg = '0'
016000160902     C                              or vinflg = '2')
016100171019     C*
016200171019     C* Se giro = 1
016300171019     C                   if        wGiro = *zeros
016400171019     C     minu:MAIU     XLATE     vindta        vindta
016500171019     C                   endif
016600160902     C*
016700160902     C* Controlli di "base" sui dati grezzi
016800160902     C                   exsr      normCSV
016900040510     C*
017000160902     C                   if        wFlgVINDTA = 1
017100010330     C*
017200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017300010711     C                   if        vinflg = *blanks or vinflg = '0'
017400010711     C                   clear                   vinmsg
017500010711     C                   endif
017600160902     C*
017700160902     C* Splitto i dati
017800160902     C                   CLEAR                   SkSplitCSV_5
017900160902     C                   EVAL      InStringa = vindta
018000160902     C                   EVAL      InSepara  = CharCSV
018100160902     C                   EXSR      SR_SPLIT
018200010601     C*
018300160902     C* Se OK => procedo con elaborazione
018400010330     C                   exsr      inzvar
018500010605     C                   exsr      impvab
018600010601     C*
018700010604     C* Ebbene...
018800160908     C                   if        wGiro > 1
018900170512     C                   exsr      srcspe
019000150420     C                   endif
019100010604     C*
019200010604     C                   if        *in31 = *off and
019300010604     C                             *in32 = *off
019400010604     C                   eval      vinflg = '1'
019500010604     C                   else
019600010604     C                   eval      vinflg = '2'
019700010604     C                   endif
019800010601     C*
019900160902     C                   else
020000160902     C                   eval      vinflg = '1'
020100010604     C                   endif
020200160902     C*
020300160902     C                   else
020400160902     C                   eval      vinflg = '1'
020500160902     C                   endif
020600010601     C*
020700160902     C                   eval      vindta = s_vindta
020800010601     C  N70              update    tivin000
020900010330     C*
021000030822     C  N70              enddo
021100010601     C
021200010601     C*
021300160902     C* Stabilisco l'esito della traduzione
021400170512     C                   eval      wrkesito = '0'
021500010330     C*
021600010330     C                   if        %open(tivin00r)
021700010330     C                   close     tivin00r
021800010330     C                   endif
021900010330     C*
022000010330     C                   ENDSR
022100010330     C***
022200010601
022300010601
022400010330     C*----------------------------------------------------*
022500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022600010330     C*----------------------------------------------------*
022700010330     C     INZVAR        BEGSR
022800030822     C*
022900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
023000170512     C                   CLEAR                   FIVABDS
023100030822     C                   CLEAR                   SkSplitCSV
023200030822     C                   EVAL      i = 1
023300030822     C*
023400030822     C* Reimposto i valori di default
023500030822     C                   EXSR      DEFCAM
023600030822     C*
023700010330     C                   ENDSR
023800160902
023900160902
024000010330     C*----------------------------------------------------*
024100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
024200010330     C*----------------------------------------------------*
024300010330     C     DEFCAM        BEGSR
024400110706     C*
024500160906     C                   SETOFF                                       7172
024600030822     C*
024700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
024800030715     C* e delimitatore testo.
024900170512     C                   EVAL      pptTIP  = %subst(vlrppt:1:2)
025000170512     C                   EVAL      CharCSV = %subst(vlrppt:3:1)
025100170512     C                   EVAL      CharTXT = %subst(vlrppt:4:1)
025200170512     C                   EVAL      CharNUM = %subst(vlrppt:5:1)
025300090403     C*
025400090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
025500160902     C                   EVAL      CharSOS = CharNUM
025600030822     C*
025700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
025800030822     C                   EVAL      posDaDft = 1
025900030822     C                   EVAL      posADft  = 0
026000030822     C                   EVAL      wGiroDft = 0
026100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
026200030822     C                             posDaDft > 0
026300030822     C*
026400030822     C* Gestisco il 1° giro
026500030822     C                   IF        wGiroDft = 0
026600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
026700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
026800030822     C* Incremento il contatore dei "giri"
026900030822     C                   EVAL      wGiroDft = 1
027000030822     C                   ELSE
027100030822     C                   EVAL      posDaDft = posADft
027200030822     C                   ENDIF
027300030822     C* Eseguo lo scan x trovare la fine del campo corrente
027400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
027500030822     C*
027600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
027700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
027800030822     C                   IF        posDaDft > 0 AND
027900030822     C                             posADft  > 0
028000110706     C* RMN
028100110706     C                   IF        %subst(
028200110706     C                             %subst(vlrppt:posDaDft+1:
028300110706     C                             posADft-posDaDft-1):1:3)
028400110706     C                             = 'RMN'
028500110706     C                   IF        %trim(%subst(
028600110706     C                             %subst(vlrppt:posDaDft+1:
028700110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
028800110706     C                             = '*NSP'
028900160902     C                   SETON                                        71
029000110706     C                   ENDIF
029100110706     C                   ENDIF
029200060331     C* FGS
029300060331     C                   IF        %subst(
029400060331     C                             %subst(vlrppt:posDaDft+1:
029500060331     C                             posADft-posDaDft-1):1:3)
029600060331     C                             = 'FGS'
029700060331     C                   EVAL      PiStr=%trim(%subst(
029800060331     C                             %subst(vlrppt:posDaDft+1:
029900060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030000060331     C                   EXSR      CHKNUM
030100060331     C                   IF        PiInt=*on
030200060331     C                   Z-ADD     PiVal         VABFGS
030300060331     C                   ENDIF
030400060331     C                   ENDIF
030500030822     C* NCL
030600030822     C                   IF        %subst(
030700030822     C                             %subst(vlrppt:posDaDft+1:
030800030822     C                             posADft-posDaDft-1):1:3)
030900030822     C                             = 'NCL'
031000030822     C                   EVAL      PiStr=%trim(%subst(
031100030822     C                             %subst(vlrppt:posDaDft+1:
031200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031300030822     C                   EXSR      CHKNUM
031400030822     C                   IF        PiInt=*on
031500030822     C                   Z-ADD     PiVal         VABNCL
031600030822     C                   ENDIF
031700030822     C                   ENDIF
031800030822     C* CCM
031900030822     C                   IF        %subst(
032000030822     C                             %subst(vlrppt:posDaDft+1:
032100030822     C                             posADft-posDaDft-1):1:3)
032200030822     C                             = 'CCM'
032300030822     C                   EVAL      PiStr=%trim(%subst(
032400030822     C                             %subst(vlrppt:posDaDft+1:
032500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032600030822     C                   EXSR      CHKNUM
032700030822     C                   IF        PiInt=*on
032800030822     C                   Z-ADD     PiVal         VABCCM
032900030822     C                   ENDIF
033000030822     C                   ENDIF
033100030822     C* LNP
033200030822     C                   IF        %subst(
033300030822     C                             %subst(vlrppt:posDaDft+1:
033400030822     C                             posADft-posDaDft-1):1:3)
033500030822     C                             = 'LNP'
033600030822     C                   EVAL      PiStr=%trim(%subst(
033700030822     C                             %subst(vlrppt:posDaDft+1:
033800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033900030822     C                   EXSR      CHKNUM
034000030822     C                   IF        PiInt=*on
034100030822     C                   Z-ADD     PiVal         VABLNP
034200030822     C                   ENDIF
034300030822     C                   ENDIF
034400030822     C* NRS
034500030822     C                   IF        %subst(
034600030822     C                             %subst(vlrppt:posDaDft+1:
034700030822     C                             posADft-posDaDft-1):1:3)
034800030822     C                             = 'NRS'
034900030822     C                   EVAL      PiStr=%trim(%subst(
035000030822     C                             %subst(vlrppt:posDaDft+1:
035100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035200030822     C                   EXSR      CHKNUM
035300030822     C                   IF        PiInt=*on
035400030822     C                   Z-ADD     PiVal         VABNRS
035500030822     C                   ENDIF
035600030822     C                   ENDIF
035700160906     C* CTR
035800030822     C                   IF        %subst(
035900030822     C                             %subst(vlrppt:posDaDft+1:
036000030822     C                             posADft-posDaDft-1):1:3)
036100030822     C                             = 'CTR'
036200030822     C                   EVAL      PiStr=%trim(%subst(
036300030822     C                             %subst(vlrppt:posDaDft+1:
036400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036500030822     C                   EXSR      CHKNUM
036600030822     C                   IF        PiInt=*on
036700160906     C                   Z-ADD     PiVal         VABCTR
036800030822     C                   ENDIF
036900030822     C                   ENDIF
037000160906     C* CTE (ovvero CTR per Estero)
037100160906     C                   IF        %subst(
037200160906     C                             %subst(vlrppt:posDaDft+1:
037300160906     C                             posADft-posDaDft-1):1:3)
037400160906     C                             = 'CTE'
037500160906     C                   EVAL      PiStr=%trim(%subst(
037600160906     C                             %subst(vlrppt:posDaDft+1:
037700160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037800160906     C                   EXSR      CHKNUM
037900160906     C                   IF        PiInt=*on
038000160906     C                   Z-ADD     PiVal         wVABCTR_E
038100160906     C                   SETON                                        72
038200160906     C                   ENDIF
038300160906     C                   ENDIF
038400030822     C* PKB
038500030822     C                   IF        %subst(
038600030822     C                             %subst(vlrppt:posDaDft+1:
038700030822     C                             posADft-posDaDft-1):1:3)
038800030822     C                             = 'PKB'
038900030822     C                   EVAL      PiStr=%trim(%subst(
039000030822     C                             %subst(vlrppt:posDaDft+1:
039100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039200030822     C                   EXSR      CHKNUM
039300030822     C                   IF        PiNum=*on
039400030822     C                   Z-ADD     PiVal         VABPKB
039500030822     C                   ENDIF
039600030822     C                   ENDIF
039700030822     C* VLB
039800030822     C                   IF        %subst(
039900030822     C                             %subst(vlrppt:posDaDft+1:
040000030822     C                             posADft-posDaDft-1):1:3)
040100030822     C                             = 'VLB'
040200030822     C                   EVAL      PiStr=%trim(%subst(
040300030822     C                             %subst(vlrppt:posDaDft+1:
040400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040500030822     C                   EXSR      CHKNUM
040600030822     C                   IF        PiNum=*on
040700030822     C                   Z-ADD     PiVal         VABVLB
040800030822     C                   ENDIF
040900030822     C                   ENDIF
041000030822     C* QFT
041100030822     C                   IF        %subst(
041200030822     C                             %subst(vlrppt:posDaDft+1:
041300030822     C                             posADft-posDaDft-1):1:3)
041400030822     C                             = 'QFT'
041500030822     C                   EVAL      PiStr=%trim(%subst(
041600030822     C                             %subst(vlrppt:posDaDft+1:
041700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041800030822     C                   EXSR      CHKNUM
041900030822     C                   IF        PiNum=*on
042000030822     C                   Z-ADD     PiVal         VABQFT
042100030822     C                   ENDIF
042200030822     C                   ENDIF
042300030822     C* CBO
042400030822     C                   IF        %subst(
042500030822     C                             %subst(vlrppt:posDaDft+1:
042600030822     C                             posADft-posDaDft-1):1:3)
042700030822     C                             = 'CBO'
042800030822     C                   EVAL      VABCBO=%trim(%subst(
042900030822     C                             %subst(vlrppt:posDaDft+1:
043000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043100030822     C                   ENDIF
043200030822     C* TSP
043300030822     C                   IF        %subst(
043400030822     C                             %subst(vlrppt:posDaDft+1:
043500030822     C                             posADft-posDaDft-1):1:3)
043600030822     C                             = 'TSP'
043700030822     C                   EVAL      VABTSP=%trim(%subst(
043800030822     C                             %subst(vlrppt:posDaDft+1:
043900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044000030822     C                   ENDIF
044100030822     C* VAS
044200030822     C                   IF        %subst(
044300030822     C                             %subst(vlrppt:posDaDft+1:
044400030822     C                             posADft-posDaDft-1):1:3)
044500030822     C                             = 'VAS'
044600030822     C                   EVAL      VABVAS=%trim(%subst(
044700030822     C                             %subst(vlrppt:posDaDft+1:
044800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044900030822     C                   ENDIF
045000030822     C* VCA
045100030822     C                   IF        %subst(
045200030822     C                             %subst(vlrppt:posDaDft+1:
045300030822     C                             posADft-posDaDft-1):1:3)
045400030822     C                             = 'VCA'
045500030822     C                   EVAL      VABVCA=%trim(%subst(
045600030822     C                             %subst(vlrppt:posDaDft+1:
045700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045800030822     C                   ENDIF
045900030822     C* GCA
046000030822     C                   IF        %subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):1:3)
046300030822     C                             = 'GCA'
046400030822     C                   EVAL      VABGCA=%trim(%subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046700030822     C                   ENDIF
046800030822     C* CTM
046900030822     C                   IF        %subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):1:3)
047200030822     C                             = 'CTM'
047300030822     C                   EVAL      VABCTM=%trim(%subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047600030822     C                   ENDIF
047700030822     C* FFD
047800030822     C                   IF        %subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):1:3)
048100030822     C                             = 'FFD'
048200030822     C                   EVAL      VABFFD=%trim(%subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048500030822     C                   ENDIF
048600030822     C* VAD
048700030822     C                   IF        %subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):1:3)
049000030822     C                             = 'VAD'
049100030822     C                   EVAL      VABVAD=%trim(%subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049400030822     C                   ENDIF
049500030822     C* GMA
049600030822     C                   IF        %subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):1:3)
049900030822     C                             = 'GMA'
050000030822     C                   EVAL      VABGMA=%trim(%subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050300030822     C                   ENDIF
050400030822     C* GGA
050500030822     C                   IF        %subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):1:3)
050800030822     C                             = 'GGA'
050900030822     C                   EVAL      VABGGA=%trim(%subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200030822     C                   ENDIF
051300030822     C* GVA
051400030822     C                   IF        %subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):1:3)
051700030822     C                             = 'GVA'
051800030822     C                   EVAL      VABGVA=%trim(%subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052100030822     C                   ENDIF
052200030822     C* TC1
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'TC1'
052700030822     C                   EVAL      VABTC1=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   ENDIF
053100030822     C* TC2
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'TC2'
053600030822     C                   EVAL      VABTC2=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   ENDIF
054000030822     C* ...
054100030822     C                   ENDIF
054200030822     C                   ENDDO
054300020204     C*
054400010330     C                   ENDSR
054500170512     C*----------------------------------------------------*
054600170512
054700170512
054800170512
054900010607     C*----------------------------------------------------*
055000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
055100010607     C*----------------------------------------------------*
055200010607     C     IMPVAB        BEGSR
055300010607     C*
055400010607     C                   SETOFF                                       3132
055500030515     C*
055600010607     C                   MOVEL     datcor        VABAAS
055700010607     C                   MOVE      datcor        VABMGS
055800160902     C*
055900160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
056000160902     C* i caratteri di delimitazione testo e gli apici
056100160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
056200160902     C                   EVAL      i = 1
056300160902     C                   DOW       i < %elem(SkSplitCSV)
056400160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
056500160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
056600160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
056700160902     C                   EVAL      i = i + 1
056800160902     C                   ENDDO
056900010607     C*
057000030715     C**********
057100160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
057200030715     C**********
057300030715     C                   IF        wGiro = *zeros
057400160902     C*
057500160902     C                   EVAL      wGiro = 1
057600160902     C                   EVAL      SkSplitFLD = SkSplitCSV
057700160902     C*
057800160902     C                   ELSE
057900160902     C*
058000160902     C**********
058100160902     C* Elaboro le righe di DATI cliente
058200160902     C**********
058300160902     C                   EVAL      wGiro = 2
058400030715     C*
058500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
058600030715     C                   EVAL      i = 1
058700030715     C                   DOW       i < %elem(SkSplitFLD)
058800030715     C*
058900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
059000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
059100030715     C                             SkSplitCSV(i) <> *blanks
059200030715     C*** RSD
059300170512     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
059400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
059500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
059600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
059700030715     C* ==
059800030715     C                   ENDIF
059900170512     C*** IND
060000170512     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
060100170512     C                   EVAL      VABLOD = SkSplitCSV(i)
060200170512     C                   ENDIF
060300030715     C*** LOD
060400170512     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
060500160909     C                   EVAL      VABLOD = SkSplitCSV(i)
060600030715     C                   ENDIF
060700030715     C*** PRD
060800170512     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
060900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
061000030715     C                   ENDIF
061100160902     C*** CAD
061200170512     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
061300160902     C                   EVAL      VABCAD = SkSplitCSV(i)
061400160902     C                   ENDIF
061500030721     C*** NZD
061600170512     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
061700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
061800030721     C                   ENDIF
061900170512     C*** RMA
062000170512     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
062100170512     C                   EVAL      VABRMA = SkSplitCSV(i)
062200150515     C                   ENDIF
062300030715     C*** FFD
062400160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
062500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
062600030715     C                   ENDIF
062700160902     C*** CBO
062800160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
062900160902     C                   EVAL      VABCBO = SkSplitCSV(i)
063000150420     C                   ENDIF
063100030721     C*** GC1
063200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
063300150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
063400150420     C                   MOVEL     *blanks       wGC               1
063500150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
063600150420     C                   IF        SkSplitCSV(i) = 'M' OR
063700150420     C                             SkSplitCSV(i) = 'P'
063800150420     C                   MOVE(P)   wGC           VABGC1
063900150420     C                   ELSE
064000150420     C                   MOVEL(P)  wGC           VABGC1
064100150420     C                   ENDIF
064200150420     C                   ENDIF
064300030721     C                   ENDIF
064400030721     C*** GC2
064500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
064600120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
064700120314     C                   MOVEL     *blanks       wGC               1
064800120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
064900120314     C                   IF        SkSplitCSV(i) = 'M' OR
065000120314     C                             SkSplitCSV(i) = 'P'
065100120314     C                   MOVE(P)   wGC           VABGC2
065200120314     C                   ELSE
065300120314     C                   MOVEL(P)  wGC           VABGC2
065400120314     C                   ENDIF
065500120314     C                   ENDIF
065600030721     C                   ENDIF
065700030721     C*** TSP
065800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
065900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
066000030721     C                   ENDIF
066100030721     C*** NAS
066200160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
066300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
066400030721     C                   ENDIF
066500030721     C*** TIC
066600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
066700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
066800030721     C                   ENDIF
066900030721     C*** CTM
067000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
067100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
067200030721     C                   ENDIF
067300030721     C*** TC1
067400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
067500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
067600030721     C                   ENDIF
067700030721     C*** TC2
067800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
067900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
068000030721     C                   ENDIF
068100030721     C*** RMO
068200160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
068300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
068400030721     C                   ENDIF
068500160902     C*** CMO
068600160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
068700160902     C                   EVAL      VABCMO = SkSplitCSV(i)
068800160902     C                   ENDIF
068900030721     C*** NMO
069000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
069100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
069200030721     C                   ENDIF
069300030721     C*
069400070705     C* Reperisco quindi i campi numerici...
069500030721     C*** RMN
069600170512     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
069700030721     C                   EVAL      PiStr=SkSplitCSV(i)
069800030721     C                   EXSR      CHKNUM
069900030721     C                   IF        PiInt=*on
070000030721     C                   Z-ADD     PiVal         VABRMN
070100030721     C                   ELSE
070200030721     C                   SETON                                        32
070300030721     C                   EVAL      VABRMN = 1
070400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
070500170725     C                             + ' ' + 'VABRMN'
070600030721     C                   ENDIF
070700030721     C                   ENDIF
070800030721     C*** NSP
070900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
071000030721     C                   EVAL      PiStr=SkSplitCSV(i)
071100030721     C                   EXSR      CHKNUM
071200100517     C                   IF        PiInt=*on
071300030721     C                   Z-ADD     PiVal         VABNSP
071400030721     C                   ELSE
071500030721     C                   SETON                                        32
071600030721     C                   EVAL      VABNSP = *zeros
071700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
071800030721     C                             + ' ' + 'VABNSP'
071900030721     C                   ENDIF
072000030721     C                   ENDIF
072100030721     C*** CCM
072200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
072300030721     C                   EVAL      PiStr=SkSplitCSV(i)
072400030721     C                   EXSR      CHKNUM
072500030721     C                   IF        PiInt=*on      AND
072600030721     C                             PiVal<=9999999 AND
072700030721     C                             PiVal>*zeros
072800030721     C                   Z-ADD     PiVal         VABCCM
072900030721     C                   ELSE
073000030721     C                   SETON                                        32
073100030721     C                   EVAL      VABCCM = *zeros
073200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
073300030721     C                             + ' ' + 'VABCCM'
073400030721     C                   ENDIF
073500030721     C                   ENDIF
073600170725     C*** AAS
073700170725     C                   IF        %trim(SkSplitFLD(i)) = 'VABAAS'
073800170725     C                   EVAL      PiStr=SkSplitCSV(i)
073900170725     C                   EXSR      CHKNUM
074000170725     C                   IF        PiInt=*on
074100170725     C                   Z-ADD     PiVal         VABAAS
074200170725     C                   ENDIF
074300170725     C                   ENDIF
074400030721     C*** LNP
074500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
074600030721     C                   EVAL      PiStr=SkSplitCSV(i)
074700030721     C                   EXSR      CHKNUM
074800030721     C                   IF        PiInt=*on    AND
074900030721     C                             PiVal<=999   AND
075000030721     C                             PiVal>*zeros
075100030721     C                   Z-ADD     PiVal         VABLNP
075200030721     C                   ELSE
075300030721     C                   SETON                                        32
075400030721     C                   EVAL      VABLNP = *zeros
075500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
075600030721     C                             + ' ' + 'VABLNP'
075700030721     C                   ENDIF
075800030721     C                   ENDIF
075900030721     C*** NRS
076000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
076100030721     C                   EVAL      PiStr=SkSplitCSV(i)
076200030721     C                   EXSR      CHKNUM
076300030721     C                   IF        PiInt=*on AND
076400030721     C                             PiVal<=99
076500030721     C                   Z-ADD     PiVal         VABNRS
076600030721     C                   ELSE
076700030721     C                   SETON                                        32
076800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
076900030721     C                             + ' ' + 'VABNRS'
077000030721     C                   ENDIF
077100030721     C                   ENDIF
077200030721     C*** LNA
077300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
077400120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
077500120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
077600120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
077700120314     C                             %trim(SkSplitCSV(i)) <> *blanks
077800030721     C                   EVAL      PiStr=SkSplitCSV(i)
077900030721     C                   EXSR      CHKNUM
078000030721     C                   IF        PiInt=*on    AND
078100030721     C                             PiVal<=999   AND
078200030721     C                             PiVal>*zeros
078300030721     C                   Z-ADD     PiVal         VABLNA
078400030721     C                   ELSE
078500030721     C                   SETON                                        32
078600030721     C                   EVAL      VABLNA = *zeros
078700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
078800030721     C                             + ' ' + 'VABLNA'
078900120314     C                   ENDIF
079000030721     C                   ENDIF
079100030721     C                   ENDIF
079200030721     C*** CTR
079300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
079400030721     C                   EVAL      PiStr=SkSplitCSV(i)
079500030721     C                   EXSR      CHKNUM
079600030721     C                   IF        PiInt=*on  AND
079700030721     C                             PiVal<=999
079800030721     C                   Z-ADD     PiVal         VABCTR
079900030721     C                   ELSE
080000030721     C                   SETON                                        32
080100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
080200030721     C                             + ' ' + 'VABCTR'
080300030721     C                   ENDIF
080400030721     C                   ENDIF
080500030721     C*** DCR
080600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
080700030721     C                   EVAL      PiStr=SkSplitCSV(i)
080800030721     C                   EXSR      CHKNUM
080900030721     C                   IF        PiInt=*on       AND
081000030721     C                             PiVal<=99999999
081100030721     C                   Z-ADD     PiVal         VABDCR
081200030721     C                   ELSE
081300030721     C                   SETON                                        32
081400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
081500030721     C                             + ' ' + 'VABDCR'
081600030721     C                   ENDIF
081700030721     C                   ENDIF
081800030715     C*
081900030715     C                   ENDIF
082000030715     C                   EVAL      i = i + 1
082100030715     C                   ENDDO
082200070705     C*
082300160902     C* Considerazioni particolari "fuori ciclo"
082400110706     C*
082500160908     C* Verifica campi "particolari" VAB
082600160908     C                   EXSR      CHKVAB
082700160908     C*
082800030715     C                   ENDIF
082900020204     C*
083000010607     C                   ENDSR
083100010607     C*----------------------------------------------------*
083200160908
083300160908
083400160908     C*----------------------------------------------------*
083500160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
083600160908     C*----------------------------------------------------*
083700160908     C     CHKVAB        BEGSR
083800160908     C*
083900160908     C*** NZD
084000160908     C                   IF        VABNZD = 'IT'  OR
084100160908     C                             VABNZD = 'ITA' OR
084200160908     C                             VABNZD = 'Ita' OR
084300160908     C                             VABNZD = 'ita'
084400160908     C                   EVAL      VABNZD = *blanks
084500160908     C                   ENDIF
084600160908     C*** CTR
084700160908     C                   IF        *in72
084800160908     C                   IF        VABNZD <> *blanks
084900160908     C                   EVAL      VABCTR =  wVABCTR_E
085000160908     C                   ENDIF
085100160908     C                   ENDIF
085200160908     C*** CBO
085300160908     C                   IF        VABCBO = *blanks
085400160908     C                   EVAL      VABCBO = '1'
085500160908     C                   ENDIF
085600160908     C*** CCM
085700160908     C                   IF        VABCCM = *zeros
085800160908     C                   EVAL      PiStr=VLRKSC
085900160908     C                   EXSR      CHKNUM
086000160908     C                   IF        PiInt=*on
086100160908     C                   Z-ADD     PiVal         VABCCM
086200170725     C                   EVAL      iWS775TLE = 'WW'
086300160908     C                   ELSE
086400160908     C                   SETON                                        31
086500160908     C                   ENDIF
086600170725     C                   ELSE
086700170725     C                   EVAL      iWS775TLE = *blanks
086800160908     C                   ENDIF
086900160908     C*** LNP
087000160908     C                   IF        VABLNP = *zeros
087100160908     C                   IF        VLRPOI = 046 OR
087200160908     C                             VLRPOI = 789 OR
087300160908     C                             VLRPOI = 999
087400160908     C                   MOVEL     VABCCM        VABLNP
087500160908     C                   ELSE
087600160908     C                   EVAL      VABLNP = VLRPOI
087700160908     C                   ENDIF
087800160908     C                   ENDIF
087900160908     C*
088000160908     C                   ENDSR
088100170512     C*----------------------------------------------------*
088200010330
088300170512
088400010330
088500010330     C*----------------------------------------------------*
088600010330     C*  CONTROLLO NUMERICITA' CAMPI
088700010330     C*----------------------------------------------------*
088800010330     C     CHKNUM        BEGSR
088900010330     C*
089000010606     C                   IF        PiDecChr = *blanks
089100030715     C                   EVAL      PiDecChr = CharNUM
089200010606     C                   ENDIF
089300010606     C*
089400010606     C                   CALL(e)   'ISNUMERIC'
089500010330     C                   PARM                    PiStr            30
089600010606     C                   PARM                    PiDecChr          1
089700010330     C                   PARM      *ZEROS        PiVal            30 9
089800010330     C                   PARM      '0'           PiInt             1
089900010330     C                   PARM      '0'           PiNum             1
090000010330     C                   IF        %error
090100010606     C                   EVAL      PiNum=*off
090200010330     C                   ENDIF
090300010330     C*
090400010330     C                   ENDSR
090500170512     C*----------------------------------------------------*
090600160906
090700170512
090800160906
090900170512     C*----------------------------------------------------*
091000160906      /TITLE Valorizzazione Progressivo Applicazione
091100160906     C     calprog       begsr
091200160906     C*
091300160906     C                   movel     *blanks       dwlisv            2
091400160906     C                   movel     *all'0'       dwlprg           10
091500160906     C                   z-add     *zeros        wrkprg            8 0
091600160906     C*
091700160906     C                   eval      dwlisv = vlrisv
091800160906     C*
091900170324     C     *start        setll     tis7prgf
092000160906     C                   read(e)   tis7prgf
092100160906     C                   if        not %error
092200160906     C                   eval      dwlprg = f_tis7prgf
092300160906     C*
092400160906     C                   move(p)   dwlprg        wrkprg
092500160906     C                   add       1             wrkprg
092600160906     C                   move(p)   wrkprg        dwlprg
092700160906     C                   movel     dwlisv        dwlprg
092800160906     C*
092900160906     C                   eval      f_tis7prgf = dwlprg
093000160906     C                   update    tis7prg0
093100170324     C                   else
093200170324     C                   eval      f_tis7prgf = 'XX00000000'
093300160906     C                   endif
093400160906     C*
093500160906     C                   endsr
093600170512     C*----------------------------------------------------*
093700170512
093800100419
093900100419
094000170512     C*----------------------------------------------------*
094100160902     C     normCSV       BEGSR
094200160902     C*
094300160902     C                   z-add     *zeros        wLenVINDTA        4 0
094400160902     C                   z-add     *zeros        wFlgVINDTA        1 0
094500160902     C                   setoff                                       80
094600160902     C*
094700160902     C                   dow       wLenVINDTA < %len(vindta)
094800160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
094900160902     C                   movel     *blanks       wAlfa1            1
095000160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
095100160902     C*
095200160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
095300160902     C                   if        wAlfa1 < X'40'
095400160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
095500160902     C                   endif
095600160902     C*
095700160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
095800160902     C                   if        wAlfa1 <> CharCSV and
095900160902     C                             wAlfa1 <> CharTXT and
096000160902     C                             wAlfa1 <> CharNUM
096100160902     C                   z-add     1             wFlgVINDTA
096200160902     C                   endif
096300160902     C*
096400160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
096500160902     C* separatori campi
096600160902     C                   if        CharTXT <> *blanks
096700160902     C                   if        wAlfa1 = CharTXT
096800160902     C                   if        not *in80
096900160902     C                   seton                                        80
097000160902     C                   else
097100160902     C                   setoff                                       80
097200160902     C                   endif
097300160902     C                   endif
097400160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
097500160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
097600160902     C                   endif
097700160902     C                   endif
097800160902     C*
097900160902     C                   enddo
098000100419     C*
098100100419     C                   ENDSR
098200170512     C*----------------------------------------------------*
098300170512
098400170512
098500170512
098600170512     C*----------------------------------------------------*
098700170512      /TITLE Ricerca spedizione tramite criteri indicati nel file di input
098800170512     C     srcSpe        BEGSR
098900170512     C*
099000170512     C                   z-add     *zeros        wAAS              4 0
099100170512     C                   z-add     *zeros        wLNP              3 0
099200170512     C                   z-add     *zeros        wNRS              2 0
099300170512     C                   z-add     *zeros        wNSP              7 0
099400170512     C                   z-add     *zeros        wDSP              8 0
099500170512     C                   z-add     *zeros        wCLI              7 0
099600170512     C*
099700170512     C* Effettuo ricerca specifica in base agli elementi a disposizione
099800170512     C                   select
099900170512     C                   when      vabAAS > *zeros AND                          * Spedizione
100000170512     C                             vabLNP > *zeros AND
100100170512     C                             vabNSP > *zeros
100200170512     C                   eval      wAAS = vabAAS
100300170512     C                   eval      wLNP = vabLNP
100400170512     C                   eval      wNRS = vabNRS
100500170512     C                   eval      wNSP = vabNSP
100600170512     C                   eval      wDSP = vabAAS*10000+vabMGS
100700170512     C                   eval      wCLI = vabCCM
100800170515     C                   exsr      exeOut
100900170512     C*
101000170512     C                   when      vabRMA <> *blanks                            * RMA
101100170512     C                   eval      iWS775TIP  = 'A'
101200170512     C                   exsr      srcRMNRMA
101300170512     C                   eval      wAAS = oWS775AAS
101400170512     C                   eval      wLNP = oWS775LNP
101500170512     C                   eval      wNRS = oWS775NRS
101600170512     C                   eval      wNSP = oWS775NSP
101700170512     C                   eval      wDSP = oWS775DSP
101800170512     C                   eval      wCLI = oWS775CLI
101900170515     C                   exsr      exeOut
102000170512     C*
102100170512     C                   when      vabRMN  > *zeros                             * RMN
102200170512     C                   eval      iWS775TIP  = 'N'
102300170512     C                   exsr      srcRMNRMA
102400170512     C                   eval      wAAS = oWS775AAS
102500170512     C                   eval      wLNP = oWS775LNP
102600170512     C                   eval      wNRS = oWS775NRS
102700170512     C                   eval      wNSP = oWS775NSP
102800170512     C                   eval      wDSP = oWS775DSP
102900170512     C                   eval      wCLI = oWS775CLI
103000170515     C                   exsr      exeOut
103100170512     C*
103200170512     C                   other                                                  * Ricerche SQL
103300170515     C***                exsr      exeTAS
103400170515     C***                exsr      exeOut
103500170512     C*
103600170512     C                   endsl
103700170515     C*
103800170515     C                   ENDSR
103900170515     C*----------------------------------------------------*
104000170515
104100170515
104200170515
104300170515     C*----------------------------------------------------*
104400170515      /TITLE Generazioe dati output
104500170515     C     exeOut        BEGSR
104600170515     C*
104700170515     C* Verifica se il CCM indicato e il KSC VAS sono della stessa famiglia
104800170515     C                   eval      iBS10CLI  = wCLI
104900170515     C                   exsr      rtvUNI
105000170515     C                   if        oBS10Esito = *zeros AND
105100170515     C                             wCodUnif = oBS10COP
105200170515     C                   else
105300170515     C                   z-add     *zeros        wAAS
105400170515     C                   z-add     *zeros        wLNP
105500170515     C                   z-add     *zeros        wNRS
105600170515     C                   z-add     *zeros        wNSP
105700170515     C                   z-add     *zeros        wDSP
105800170515     C                   z-add     *zeros        wCLI
105900170515     C                   endif
106000170515     C*
106100170515     C* Se reperita spedizione lancio la generazione del file VAC
106200170515     C                   if        wAAS > *zeros AND
106300170515     C                             wLNP > *zeros AND
106400170515     C                             wNSP > *zeros
106500170515     C                   exsr      exeVAC
106600170515     C                   else
106700170515     C                   clear                   FNVACDS
106800170515     C                   eval      vacAAS = vabAAS
106900170515     C                   eval      vacLNP = vabLNP
107000170515     C                   eval      vacNRS = vabNRS
107100170515     C                   eval      vacNSP = vabNSP
107200170515     C                   eval      vacMGS = vabMGS
107300170515     C                   eval      vacCCM = vabCCM
107400170515     C                   eval      vacRMA = vabRMA
107500170515     C                   eval      vacRMN = vabRMN
107600170515     C                   eval      vacRSD = vabRSD
107700170515     C                   eval      vacRMO = 'Not found / Not allow'
107800170515     C                   endif
107900170515     C*
108000170515     C* Scarico il buffer di output per la spedizione corrente
108100170515     C                   exsr      wriVGD
108200170512     C*
108300170512     C                   ENDSR
108400170512     C*----------------------------------------------------*
108500170512
108600170512
108700170512
108800170512     C*----------------------------------------------------*
108900170512      /TITLE Ricerca spedizione per RMN/RMA
109000170512     C     srcRMNRMA     BEGSR
109100170512     C*
109200170512     C                   select
109300170512     C                   when      iWS775TIP  = 'A'
109400170512     C                   eval      iWS775AAS  = vabAAS
109500170512     C                   eval      iWS775RMA  = vabRMA
109600170512     C                   eval      iWS775RMN  = *zeros
109700170512     C                   eval      iWS775CLI  = vabCCM
109800170512     C                   eval      iWS775LANG = *blanks
109900170512     C*
110000170512     C                   when      iWS775TIP  = 'N'
110100170512     C                   eval      iWS775AAS  = vabAAS
110200170512     C                   eval      iWS775RMA  = *blanks
110300170512     C                   eval      iWS775RMN  = vabRMN
110400170512     C                   eval      iWS775CLI  = vabCCM
110500170512     C                   eval      iWS775LANG = *blanks
110600170512     C*
110700170512     C                   endsl
110800170512     C*
110900170512     C                   Call      'UBWS775R'
111000170512     C                   Parm                    iWS775TIP
111100170512     C                   Parm                    iWS775AAS
111200170512     C                   Parm                    iWS775RMA
111300170512     C                   Parm                    iWS775RMN
111400170512     C                   Parm                    iWS775CLI
111500170725     C                   Parm                    iWS775TLE
111600170512     C                   Parm                    iWS775LANG
111700170512     C                   Parm                    oWS775Esito
111800170512     C                   Parm                    oWS775AAS
111900170512     C                   Parm                    oWS775LNP
112000170512     C                   Parm                    oWS775NRS
112100170512     C                   Parm                    oWS775NSP
112200170512     C                   Parm                    oWS775DSP
112300170512     C                   Parm                    oWS775CLI
112400170725     C                   Parm                    oWS775COP
112500170512     C                   Parm                    oWS775Msg
112600170512     C*
112700170512     C                   ENDSR
112800170512     C*----------------------------------------------------*
112900170512
113000170512
113100170512
113200170512     C*----------------------------------------------------*
113300170512     C     exeVAC        BEGSR
113400170512     C*
113500170512     C* Valorizzo la DS d passaggio parametri
113600170512     C                   clear                   DSINPUT
113700170512     C*
113800170512     C                   eval      DATADA  = wDSP
113900170512     C                   eval      DATAA   = wDSP
114000170512     C                   eval      CODCLI  = wCLI
114100170512     C                   eval      FLGUNI  = 'S'
114200170512     C                   eval      FLGCONS = 'N'
114300170512     C                   eval      TIPCLI  = 'E'
114400170512     C                   eval      FLGNOE  = 'S'
114500170512     C                   eval      KAAS    = wAAS
114600170512     C                   eval      KLNP    = wLNP
114700170512     C                   eval      KNRS    = wNRS
114800170512     C                   eval      KNSP    = wNSP
114900170512     C*
115000170512     C* Quindi elabora
115100170512     C                   CALL      'TIS7VCR'
115200170512     C                   PARM                    DSINPUT
115300170512     C                   PARM                    FNVACDS
115400170512     C*
115500170512     C                   ENDSR
115600170512     C*----------------------------------------------------*
115700170515
115800170515
115900170515
116000170515     C*----------------------------------------------------*
116100170515      /TITLE Reperimento Codice Padre
116200170515     C     rtvUNI        BEGSR
116300170515     C*
116400170515     C                   eval      iBS10DRF  = *zeros
116500170515     C                   eval      iBS10TLE  = 'WW'
116600170515     C*
116700170515     C                   Call      'UBBS10R'
116800170515     C                   Parm                    iBS10DRF
116900170515     C                   Parm                    iBS10TLE
117000170515     C                   Parm                    iBS10CLI
117100170515     C                   Parm                    oBS10Esito
117200170515     C                   Parm                    oBS10COP
117300170515     C                   Parm                    oBS10TIP
117400170515     C*
117500170515     C                   ENDSR
117600170515     C*----------------------------------------------------*
117700150420
117800150420
117900150420
118000170512     C*----------------------------------------------------*
118100170512      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
118200150420     C     wriVGD        BEGSR
118300150420     C*
118400150420     C                   clear                   tivgd000
118500170512     C                   eval      vgdDTA = FNVACDS
118600170512     C                   eval      vgdTIP = pptTIP
118700150420     C                   eval      vgdKSU = vlrKSC
118800150420     C                   eval      vgdTSC = 'WW'
118900150420     C                   eval      vgdDAT = datcor
119000160906     C                   eval      vgdPRG = f_tis7prgf
119100160920     C                   eval      vgdPGM = procname
119200160906     C                   eval      vgdSTO = '?'
119300150420     C                   write     tivgd000
119400150420     C*
119500150420     C                   ENDSR
119600170512     C*----------------------------------------------------*
119700170512
119800160902
119900160902
120000160902     C*----------------------------------------------------*
120100160902     C     SR_SPLIT      BEGSR
120200160902     C*
120300160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
120400160902     C                   CALL      'XSPLIT2'
120500160902     C                   PARM                    InStringa
120600160902     C                   PARM                    InSepara
120700160902     C                   PARM      '5'           InTypeOut
120800160902     C                   PARM      ''            wSkParam
120900160902     C                   PARM                    OutErrore
121000160902     C                   PARM                    OutMsg
121100160902     C                   MOVEA     wSkParam      SkSplitCSV_5
121200160902     C*
121300160902     C                   ENDSR
121400170512
121500150420
121600150420
121700170512     C*----------------------------------------------------*
121800000613     C     *inzsr        BEGSR
121900990910     C*
122000990910     C     *entry        plist
122100990920     C                   parm                    tivlrds
122200990921     C                   parm      wrkesito      esito
122300000724     C                   parm                    prmlit
122400000710     C                   parm                    prmfir
122500010330     C*
122600010330     C* CALCOLA LA DATA CORRENTE
122700150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
122800150420     C                   eval      datcor = %dec(%date() : *ISO)
122900000613     C*
123000000613     C                   ENDSR
123100000613     C***
