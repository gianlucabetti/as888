000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700040113     D*------------
000800040113     D* SCHIEDE A PROGRAMMA
000900040113     D*------------
001000040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800010330     D esito           s              1
001900010330     D prmlit          s             10
002000010330     D prmfir          s             10
002100010330     D wrkesito        s                   like(esito)
002200010330     D rrnum           s              6  0 INZ(*zeros)
002300030715     D*------------------
002400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002500030715     D*------------------
002600030715     D SkSplitFLD      S             10    DIM(100)
002700030715     D SkSplitCSV      S            256    DIM(100)
002800030715     D CharCSV         S              1
002900030715     D CharTXT         S              1
003000030715     D CharNUM         S              1
003100030715     D posDa           S              3  0 INZ(*zeros)
003200030715     D posA            S              3  0 INZ(*zeros)
003300030715     D i               s              3  0 INZ(1)
003400030715     D wGiro           s              1  0 INZ(*zeros)
003500030822     D*------------------
003600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003700030822     D*------------------
003800030822     D posDaDft        S              3  0 INZ(*zeros)
003900030822     D posADft         S              3  0 INZ(*zeros)
004000030822     D j               s              3  0 INZ(1)
004100030822     D wGiroDft        s              1  0 INZ(*zeros)
004200010330     D*------------------
004300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004400010330     D*------------------
004500010330     D WLBDA8          DS                  INZ
004600010330     D  G08DAT                 1      8  0
004700010330     D  G08INV                 9     16  0
004800010330     D  G08ERR                17     17
004900010330     D  G08TGI                18     22  0
005000031201     D*------------------
005100031201     D* Costanti
005200031201     D*------------------
005300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005500010330
005600010330
005700010330
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100010601     C*
006200010601     C                   exsr      opeini
006300010605     C                   exsr      rwvab
006400010601     C*
006500010601     C                   seton                                        lr
006600010601
006700010601
006800010601
006900010601
007000010601     C*--------------------------------------------------------
007100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007200010601     C*--------------------------------------------------------
007300010601     C     PREELA        BEGSR
007400010601     C*
007500010601     C* SE OCCORRE SPEDIRE IN FILIALE
007600010601     C                   if        invfil <> *zeros and
007700010601     C                             flgGiro = '0'
007800010601     C*
007900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008000010601     C                   eval      flgGiro = '1'
008100010601     C*
008200010601     C                   endif
008300010601     C*
008400010601     C                   ENDSR
008500010601     C***
008600010601
008700010601
008800010601
008900010601
009000010601
009100010601
009200010601
009300010601     C*--------------------------------------------------------
009400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009500010601     C*--------------------------------------------------------
009600010601     C     ENDELA        BEGSR
009700000616     C*
009800010601     C                   ENDSR
009900010601     C***
010000000613
010100010330
010200010601
010300010601
010400010601
010500010330     C*--------------------------------------------------------
010600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
010700010330     C*--------------------------------------------------------
010800010605     C     RWVAB         BEGSR
010900010330     C*
011000010330     C                   if        not %open(tivin00r)
011100010330     C                   open      tivin00r
011200010330     C                   endif
011300021025     C                   if        not %open(fivabwwr)
011400021025     C                   open      fivabwwr
011500010330     C                   endif
011600010330     C*
011700010604     C                   clear                   §CTROK
011800010604     C                   clear                   §CTRMO
011900010604     C                   clear                   §CTRNO
012000010330     C*
012100030822     C                   do        *HIVAL
012200010330     C*
012300030822     C                   read      tivin00r                               70
012400010618     C*
012500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012600010618     C                   if        vinflg = '1'
012700010618     C                   eval      flgOk = '1'
012800010618     C                   endif
012900010618     C*
013000010330     C                   if        vindta > *blanks
013100010330     C                   add       1             rrnum
013200010330     C*
013300010601     C                   if        *in70 = *off and
013400010330     C                             (vinflg = *blanks
013500010330     C                              or vinflg = '0'
013600010330     C                              or vinflg = '2')
013700010330     C*
013800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013900010711     C                   if        vinflg = *blanks or vinflg = '0'
014000010711     C                   clear                   vinmsg
014100010711     C                   endif
014200010601     C*
014300010330     C                   exsr      inzvar
014400010605     C                   exsr      impvab
014500010601     C*
014600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014700010605     C                   eval      depfil = VABLNP
014800010601     C                   exsr      repfil
014900010601     C                   if        depfil = invfil
015000021025     C                   if        vlrpoi = 999
015100030822     C                   move(P)   invfil        VABFGS
015200021025     C                   else
015300030822     C                   move(P)   vlrpoi        VABFGS
015400021025     C                   endif
015500010601     C*
015600010601     C                   exsr      PREELA
015700010601     C*
015800010604     C* Ebbene...
015900010604     C*
016000030822     C  N31              add       1             §CTROK            7 0
016100030822     C   32              add       1             §CTRMO            7 0
016200030822     C   31              add       1             §CTRNO            7 0
016300030822     C                   if        wGiro = 2
016400030822     C  N31              write     FIVAB000
016500030822     C                   endif
016600010604     C*
016700010604     C                   if        *in31 = *off and
016800010604     C                             *in32 = *off
016900010604     C                   eval      vinflg = '1'
017000010604     C                   else
017100010604     C                   eval      vinflg = '2'
017200010604     C                   endif
017300010604     C                   endif
017400010601     C*
017500010604     C                   endif
017600010604     C*
017700010330     C                   else
017800010330     C                   eval      vinflg = '1'
017900010330     C                   endif
018000010601     C*
018100010601     C  N70              update    tivin000
018200010330     C*
018300030822     C  N70              enddo
018400010601     C
018500010601     C*
018600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018700010601     C                   if        cntNonEl = *zeros or
018800010601     C                             flgMulti = '0'
018900010330     C* Se non ci sono record con errori ...
019000010601     C                   if        §ctrno = 0 and
019100010604     C                             §ctrmo = 0 and
019200010601     C                             flgStato <> '2'
019300010330     C* ... restituisco esito OK.
019400010330     C                   eval      wrkesito = '0'
019500010330     C                   else
019600010330     C                   if        §ctrok > 0
019700010330     C                   eval      wrkesito = '1'
019800010330     C                   else
019900010615     C                   if        flgOk = '0'
020000010615     C                   eval      wrkesito = '2'
020100010615     C                   else
020200010615     C                   eval      wrkesito = '6'
020300010615     C                   endif
020400010330     C                   endif
020500010330     C                   endif
020600010601     C                   else
020700010601     C                   eval      wrkesito = '9'
020800010601     C                   endif
020900010330     C*
021000010330     C                   if        %open(tivin00r)
021100010330     C                   close     tivin00r
021200010330     C                   endif
021300021025     C                   if        %open(fivabwwr)
021400021025     C                   close     fivabwwr
021500010330     C                   endif
021600010601     C*
021700010601     C                   if        vlrpoi <> 999
021800010601     C                   eval      invfil = vlrpoi
021900010601     C                   endif
022000010330     C*
022100010330     C                   if        §ctrok > 0
022200010601     C                             and invfil > *zeros
022300010330     C                   exsr      invio
022400010330     C                   endif
022500010601     C*
022600010618     C                   if        flgGiro = '1'
022700010601     C                   exsr      endela
022800010618     C                   endif
022900010330     C*
023000010330     C                   ENDSR
023100010330     C***
023200010601
023300010601
023400010601
023500010330     C*----------------------------------------------------*
023600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023700010330     C*----------------------------------------------------*
023800010330     C     INZVAR        BEGSR
023900010330     C*
024000030822     C* Inizializzo variabili di wrk
024100020204     C                   Z-ADD     *zeros        Num5_0            5 0
024200030822     C*
024300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
024400030822     C                   CLEAR                   FIVAB000
024500030822     C                   CLEAR                   SkSplitCSV
024600030822     C                   EVAL      i = 1
024700030822     C                   EVAL      posDa = *zeros
024800030822     C                   EVAL      posA  = *zeros
024900030822     C*
025000030822     C* Reimposto i valori di default
025100030822     C                   EXSR      DEFCAM
025200030822     C*
025300010330     C                   ENDSR
025400010330     C*----------------------------------------------------*
025500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
025600010330     C*----------------------------------------------------*
025700010330     C     DEFCAM        BEGSR
025800030822     C*
025900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
026000030715     C* e delimitatore testo.
026100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
026200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
026300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
026400030822     C*
026500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
026600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
026700030715     C                   SETON                                        50
026800030715     C                   ELSE
026900030715     C                   SETOFF                                       50
027000030715     C                   ENDIF
027100030822     C*
027200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
027300030822     C                   EVAL      posDaDft = 1
027400030822     C                   EVAL      posADft  = 0
027500030822     C                   EVAL      wGiroDft = 0
027600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
027700030822     C                             posDaDft > 0
027800030822     C*
027900030822     C* Gestisco il 1° giro
028000030822     C                   IF        wGiroDft = 0
028100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
028200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
028300030822     C* Incremento il contatore dei "giri"
028400030822     C                   EVAL      wGiroDft = 1
028500030822     C                   ELSE
028600030822     C                   EVAL      posDaDft = posADft
028700030822     C                   ENDIF
028800030822     C* Eseguo lo scan x trovare la fine del campo corrente
028900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
029000030822     C*
029100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
029200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
029300030822     C                   IF        posDaDft > 0 AND
029400030822     C                             posADft  > 0
029500030822     C* NCL
029600030822     C                   IF        %subst(
029700030822     C                             %subst(vlrppt:posDaDft+1:
029800030822     C                             posADft-posDaDft-1):1:3)
029900030822     C                             = 'NCL'
030000030822     C                   EVAL      PiStr=%trim(%subst(
030100030822     C                             %subst(vlrppt:posDaDft+1:
030200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030300030822     C                   EXSR      CHKNUM
030400030822     C                   IF        PiInt=*on
030500030822     C                   Z-ADD     PiVal         VABNCL
030600030822     C                   ENDIF
030700030822     C                   ENDIF
030800030822     C* CCM
030900030822     C                   IF        %subst(
031000030822     C                             %subst(vlrppt:posDaDft+1:
031100030822     C                             posADft-posDaDft-1):1:3)
031200030822     C                             = 'CCM'
031300030822     C                   EVAL      PiStr=%trim(%subst(
031400030822     C                             %subst(vlrppt:posDaDft+1:
031500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031600030822     C                   EXSR      CHKNUM
031700030822     C                   IF        PiInt=*on
031800030822     C                   Z-ADD     PiVal         VABCCM
031900030822     C                   ENDIF
032000030822     C                   ENDIF
032100030822     C* LNP
032200030822     C                   IF        %subst(
032300030822     C                             %subst(vlrppt:posDaDft+1:
032400030822     C                             posADft-posDaDft-1):1:3)
032500030822     C                             = 'LNP'
032600030822     C                   EVAL      PiStr=%trim(%subst(
032700030822     C                             %subst(vlrppt:posDaDft+1:
032800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032900030822     C                   EXSR      CHKNUM
033000030822     C                   IF        PiInt=*on
033100030822     C                   Z-ADD     PiVal         VABLNP
033200030822     C                   ENDIF
033300030822     C                   ENDIF
033400030822     C* NRS
033500030822     C                   IF        %subst(
033600030822     C                             %subst(vlrppt:posDaDft+1:
033700030822     C                             posADft-posDaDft-1):1:3)
033800030822     C                             = 'NRS'
033900030822     C                   EVAL      PiStr=%trim(%subst(
034000030822     C                             %subst(vlrppt:posDaDft+1:
034100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034200030822     C                   EXSR      CHKNUM
034300030822     C                   IF        PiInt=*on
034400030822     C                   Z-ADD     PiVal         VABNRS
034500030822     C                   ENDIF
034600030822     C                   ENDIF
034700030822     C* CTR
034800030822     C                   IF        %subst(
034900030822     C                             %subst(vlrppt:posDaDft+1:
035000030822     C                             posADft-posDaDft-1):1:3)
035100030822     C                             = 'CTR'
035200030822     C                   EVAL      PiStr=%trim(%subst(
035300030822     C                             %subst(vlrppt:posDaDft+1:
035400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035500030822     C                   EXSR      CHKNUM
035600030822     C                   IF        PiInt=*on
035700030822     C                   Z-ADD     PiVal         VABCTR
035800030822     C                   ENDIF
035900030822     C                   ENDIF
036000030822     C* PKB
036100030822     C                   IF        %subst(
036200030822     C                             %subst(vlrppt:posDaDft+1:
036300030822     C                             posADft-posDaDft-1):1:3)
036400030822     C                             = 'PKB'
036500030822     C                   EVAL      PiStr=%trim(%subst(
036600030822     C                             %subst(vlrppt:posDaDft+1:
036700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036800030822     C                   EXSR      CHKNUM
036900030822     C                   IF        PiNum=*on
037000030822     C                   Z-ADD     PiVal         VABPKB
037100030822     C                   ENDIF
037200030822     C                   ENDIF
037300030822     C* VLB
037400030822     C                   IF        %subst(
037500030822     C                             %subst(vlrppt:posDaDft+1:
037600030822     C                             posADft-posDaDft-1):1:3)
037700030822     C                             = 'VLB'
037800030822     C                   EVAL      PiStr=%trim(%subst(
037900030822     C                             %subst(vlrppt:posDaDft+1:
038000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038100030822     C                   EXSR      CHKNUM
038200030822     C                   IF        PiNum=*on
038300030822     C                   Z-ADD     PiVal         VABVLB
038400030822     C                   ENDIF
038500030822     C                   ENDIF
038600030822     C* QFT
038700030822     C                   IF        %subst(
038800030822     C                             %subst(vlrppt:posDaDft+1:
038900030822     C                             posADft-posDaDft-1):1:3)
039000030822     C                             = 'QFT'
039100030822     C                   EVAL      PiStr=%trim(%subst(
039200030822     C                             %subst(vlrppt:posDaDft+1:
039300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039400030822     C                   EXSR      CHKNUM
039500030822     C                   IF        PiNum=*on
039600030822     C                   Z-ADD     PiVal         VABQFT
039700030822     C                   ENDIF
039800030822     C                   ENDIF
039900030822     C* CBO
040000030822     C                   IF        %subst(
040100030822     C                             %subst(vlrppt:posDaDft+1:
040200030822     C                             posADft-posDaDft-1):1:3)
040300030822     C                             = 'CBO'
040400030822     C                   EVAL      VABCBO=%trim(%subst(
040500030822     C                             %subst(vlrppt:posDaDft+1:
040600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040700030822     C                   ENDIF
040800030822     C* TSP
040900030822     C                   IF        %subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):1:3)
041200030822     C                             = 'TSP'
041300030822     C                   EVAL      VABTSP=%trim(%subst(
041400030822     C                             %subst(vlrppt:posDaDft+1:
041500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041600030822     C                   ENDIF
041700030822     C* VAS
041800030822     C                   IF        %subst(
041900030822     C                             %subst(vlrppt:posDaDft+1:
042000030822     C                             posADft-posDaDft-1):1:3)
042100030822     C                             = 'VAS'
042200030822     C                   EVAL      VABVAS=%trim(%subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042500030822     C                   ENDIF
042600030822     C* VCA
042700030822     C                   IF        %subst(
042800030822     C                             %subst(vlrppt:posDaDft+1:
042900030822     C                             posADft-posDaDft-1):1:3)
043000030822     C                             = 'VCA'
043100030822     C                   EVAL      VABVCA=%trim(%subst(
043200030822     C                             %subst(vlrppt:posDaDft+1:
043300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043400030822     C                   ENDIF
043500030822     C* GCA
043600030822     C                   IF        %subst(
043700030822     C                             %subst(vlrppt:posDaDft+1:
043800030822     C                             posADft-posDaDft-1):1:3)
043900030822     C                             = 'GCA'
044000030822     C                   EVAL      VABGCA=%trim(%subst(
044100030822     C                             %subst(vlrppt:posDaDft+1:
044200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044300030822     C                   ENDIF
044400030822     C* CTM
044500030822     C                   IF        %subst(
044600030822     C                             %subst(vlrppt:posDaDft+1:
044700030822     C                             posADft-posDaDft-1):1:3)
044800030822     C                             = 'CTM'
044900030822     C                   EVAL      VABCTM=%trim(%subst(
045000030822     C                             %subst(vlrppt:posDaDft+1:
045100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045200030822     C                   ENDIF
045300030822     C* FFD
045400030822     C                   IF        %subst(
045500030822     C                             %subst(vlrppt:posDaDft+1:
045600030822     C                             posADft-posDaDft-1):1:3)
045700030822     C                             = 'FFD'
045800030822     C                   EVAL      VABFFD=%trim(%subst(
045900030822     C                             %subst(vlrppt:posDaDft+1:
046000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046100030822     C                   ENDIF
046200030822     C* VAD
046300030822     C                   IF        %subst(
046400030822     C                             %subst(vlrppt:posDaDft+1:
046500030822     C                             posADft-posDaDft-1):1:3)
046600030822     C                             = 'VAD'
046700030822     C                   EVAL      VABVAD=%trim(%subst(
046800030822     C                             %subst(vlrppt:posDaDft+1:
046900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047000030822     C                   ENDIF
047100030822     C* GMA
047200030822     C                   IF        %subst(
047300030822     C                             %subst(vlrppt:posDaDft+1:
047400030822     C                             posADft-posDaDft-1):1:3)
047500030822     C                             = 'GMA'
047600030822     C                   EVAL      VABGMA=%trim(%subst(
047700030822     C                             %subst(vlrppt:posDaDft+1:
047800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047900030822     C                   ENDIF
048000030822     C* GGA
048100030822     C                   IF        %subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):1:3)
048400030822     C                             = 'GGA'
048500030822     C                   EVAL      VABGGA=%trim(%subst(
048600030822     C                             %subst(vlrppt:posDaDft+1:
048700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800030822     C                   ENDIF
048900030822     C* GVA
049000030822     C                   IF        %subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):1:3)
049300030822     C                             = 'GVA'
049400030822     C                   EVAL      VABGVA=%trim(%subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700030822     C                   ENDIF
049800030822     C* TC1
049900030822     C                   IF        %subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):1:3)
050200030822     C                             = 'TC1'
050300030822     C                   EVAL      VABTC1=%trim(%subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600030822     C                   ENDIF
050700030822     C* TC2
050800030822     C                   IF        %subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):1:3)
051100030822     C                             = 'TC2'
051200030822     C                   EVAL      VABTC2=%trim(%subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500030822     C                   ENDIF
051600030822     C* ...
051700030822     C                   ENDIF
051800030822     C                   ENDDO
051900020204     C*
052000010330     C                   ENDSR
052100010607     C*----------------------------------------------------*
052200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
052300010607     C*----------------------------------------------------*
052400010607     C     IMPVAB        BEGSR
052500010607     C*
052600010607     C                   SETOFF                                       3132
052700030515     C*
052800010607     C                   MOVEL     datcor        VABAAS
052900010607     C                   MOVE      datcor        VABMGS
053000010607     C*
053100030715     C**********
053200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
053300030715     C**********
053400030715     C                   IF        wGiro = *zeros
053500040113     C*
053600040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
053700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
053800040113     C                                      %trim(intcol(3))
053900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
054000031201     C     minu:maiu     XLATE     vindta        vindta
054100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
054200030715     C                   DOW       posDa <= %len(%trim(vindta))
054300030715     C*
054400030715     C* Gestisco il 1° campo
054500030715     C                   IF        i = 1
054600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
054700030715     C                   EVAL      posDa = 1
054800030715     C* Eseguo lo scan x trovare la fine del primo campo
054900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
055000030715     C* A questo punto "estraggo" il campo corrente
055100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
055200030715     C                                                    (posA-posDa))
055300030715     C* X i campi successivi al 1°
055400030715     C                   ELSE
055500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
055600030715     C                   EVAL      posDa = posA + 1
055700030715     C* Eseguo lo scan x trovare la fine del campo corrente
055800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
055900030715     C* Gestisco l'ultimo campo
056000030715     C                   IF        posA = *zeros
056100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
056200030715     C* A questo punto "estraggo" il campo corrente
056300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
056400030715     C                                                    (posA-posDa))
056500030715     C                   LEAVE
056600030715     C                   ELSE
056700030715     C* A questo punto "estraggo" il campo corrente
056800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
056900030715     C                                                    (posA-posDa))
057000030715     C                   ENDIF
057100030715     C                   ENDIF
057200030715     C* Incremento il contatore d campo
057300030715     C                   EVAL      i = i +1
057400030715     C                   ENDDO
057500030715     C*
057600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
057700030715     C* delimitazione testo
057800030715     C                   EVAL      i = 1
057900030715     C                   DOW       i < %elem(SkSplitFLD)
058000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
058100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
058200030715     C                   EVAL      i = i + 1
058300030715     C                   ENDDO
058400030715     C                   EVAL      wGiro = 1
058500030715     C                   ELSE
058600030715     C                   EVAL      wGiro = 2
058700031204     C**********
058800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
058900031204     C* d separatore campo
059000031204     C**********
059100031204     C                   EVAL      vindta = %trim(vindta)
059200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
059300031204     C                   EVAL      vindta = %subst(vindta:2)
059400031204     C                   ENDDO
059500031204     C*
059600031204     C                   Z-ADD     *zeros        lunghInput        4 0
059700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
059800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
059900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
060000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
060100031204     C                   ENDDO
060200030715     C**********
060300030715     C* Effettuo lo split del campo dati d input
060400030715     C**********
060500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
060600030715     C                   DOW       posDa <= %len(%trim(vindta))
060700030715     C*
060800030715     C* Gestisco il 1° campo
060900030715     C                   IF        i = 1
061000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
061100030715     C                   EVAL      posDa = 1
061200030715     C* Eseguo lo scan x trovare la fine del primo campo
061300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
061400030715     C* A questo punto "estraggo" il campo corrente
061500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
061600030715     C                                                    (posA-posDa))
061700030715     C* X i campi successivi al 1°
061800030715     C                   ELSE
061900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
062000030715     C                   EVAL      posDa = posA + 1
062100031204     C* Verifico che nn vi sia il campo nn valorizzato
062200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
062300031204     C* Se campo nn valorizzato skippo al prossimo
062400031204     C                   EVAL      SkSplitCSV(i) = *blanks
062500031204     C                   EVAL      posA  = posA + 1
062600031204     C                   ELSE
062700030715     C* Eseguo lo scan x trovare la fine del campo corrente
062800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
062900030715     C* Gestisco l'ultimo campo
063000030715     C                   IF        posA = *zeros
063100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
063200030715     C* A questo punto "estraggo" il campo corrente
063300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
063400030715     C                                                    (posA-posDa))
063500030715     C                   LEAVE
063600030715     C                   ELSE
063700030715     C* A questo punto "estraggo" il campo corrente
063800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
063900030715     C                                                    (posA-posDa))
064000030715     C                   ENDIF
064100031204     C                   ENDIF
064200030715     C                   ENDIF
064300030715     C* Incremento il contatore d campo
064400030715     C                   EVAL      i = i +1
064500030715     C                   ENDDO
064600030715     C*
064700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
064800030715     C* delimitazione testo
064900030715     C                   EVAL      i = 1
065000030715     C                   DOW       i < %elem(SkSplitCSV)
065100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
065200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
065300030715     C                   EVAL      i = i + 1
065400030715     C                   ENDDO
065500030715     C*
065600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
065700030715     C                   EVAL      i = 1
065800030715     C                   DOW       i < %elem(SkSplitFLD)
065900030715     C*
066000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
066100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
066200030715     C                             SkSplitCSV(i) <> *blanks
066300030721     C*** ATB
066400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
066500030721     C                   EVAL      VABATB = SkSplitCSV(i)
066600030721     C                   ENDIF
066700030715     C*** RSD
066800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
066900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
067000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
067100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
067200030715     C* ==
067300030715     C                   ENDIF
067400030721     C*** RD2
067500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
067600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
067700030721     C                   ENDIF
067800030715     C*** IND
067900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
068000030715     C                   EVAL      VABIND = SkSplitCSV(i)
068100030715     C                   ENDIF
068200030715     C*** LOD
068300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
068400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
068500030715     C                   ENDIF
068600030715     C*** PRD
068700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
068800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
068900030715     C                   ENDIF
069000030721     C*** NZD
069100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
069200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
069300030721     C                   ENDIF
069400030715     C*** RMA
069500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
069600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
069700030715     C                   ENDIF
069800030715     C*** NOT
069900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
070000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
070100030715     C                   ENDIF
070200030715     C*** NT2
070300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
070400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
070500030715     C                   ENDIF
070600030715     C*** VCA
070700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
070800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
070900030715     C                   ENDIF
071000030715     C*** VAS
071100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
071200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
071300030715     C                   ENDIF
071400030715     C*** FFD
071500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
071600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
071700030715     C                   ENDIF
071800030715     C*** CBO
071900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
072000030715     C                   EVAL      VABCBO = SkSplitCSV(i)
072100030715     C                   ENDIF
072200030715     C
072300030721     C*** GC1
072400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
072500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
072600030721     C                   ENDIF
072700030721     C*** GC2
072800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
072900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
073000030721     C                   ENDIF
073100030721     C*** TSP
073200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
073300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
073400030721     C                   ENDIF
073500030721     C*** NAS
073600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
073700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
073800030721     C                   ENDIF
073900030721     C*** TIC
074000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
074100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
074200030721     C                   ENDIF
074300030721     C*** GCA
074400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
074500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
074600030721     C                   ENDIF
074700030721     C*** XCO
074800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
074900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
075000030721     C                   ENDIF
075100030721     C*** CTM
075200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
075300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
075400030721     C                   ENDIF
075500030721     C*** TCR
075600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
075700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
075800030721     C                   ENDIF
075900030721     C*** CTS
076000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
076100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
076200030721     C                   ENDIF
076300030721     C*** FTM
076400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
076500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
076600030721     C                   ENDIF
076700030721     C*** VAD
076800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
076900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
077000030721     C                   ENDIF
077100030721     C*** GMA
077200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
077300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
077400030721     C                   ENDIF
077500030721     C*** GGA
077600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
077700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
077800030721     C                   ENDIF
077900030721     C*** GVA
078000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
078100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
078200030721     C                   ENDIF
078300030721     C*** TC1
078400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
078500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
078600030721     C                   ENDIF
078700030721     C*** TC2
078800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
078900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
079000030721     C                   ENDIF
079100030721     C*** SCL
079200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
079300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
079400030721     C                   ENDIF
079500030721     C*** RMO
079600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
079700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
079800030721     C                   ENDIF
079900030721     C*** NMO
080000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
080100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
080200030721     C                   ENDIF
080300030721     C*
080400030721     C* Reperisco quindi i campi numerici...
080500030721     C*** CAD
080600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
080700030721     C                   EVAL      PiStr=SkSplitCSV(i)
080800030721     C                   EXSR      CHKNUM
080900030721     C                   IF        PiInt=*on
081000030721     C                   Z-ADD     PiVal         Num5_0
081100030721     C                   MOVEL(p)  Num5_0        VABCAD
081200030721     C                   ELSE
081300030721     C                   SETON                                        32
081400030721     C                   EVAL      VABCAD = *zeros
081500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
081600030721     C                             + ' ' + 'VABCAD'
081700030721     C                   ENDIF
081800030721     C                   ENDIF
081900030721     C*** RMN
082000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
082100030721     C                   EVAL      PiStr=SkSplitCSV(i)
082200030721     C                   EXSR      CHKNUM
082300030721     C                   IF        PiInt=*on
082400030721     C                   Z-ADD     PiVal         VABRMN
082500030721     C                   ELSE
082600030721     C                   SETON                                        32
082700030721     C                   EVAL      VABRMN = 1
082800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
082900090618     C                             + ' ' + 'VABRMN'
083000030721     C                   ENDIF
083100030721     C                   ENDIF
083200030721     C*** NSP
083300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
083400030721     C                   EVAL      PiStr=SkSplitCSV(i)
083500030721     C                   EXSR      CHKNUM
083600030721     C                   IF        PiInt=*on AND
083700030721     C                             PiVal<=9999999
083800030721     C                   Z-ADD     PiVal         VABNSP
083900030721     C                   ELSE
084000030721     C                   SETON                                        32
084100030721     C                   EVAL      VABNSP = *zeros
084200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
084300030721     C                             + ' ' + 'VABNSP'
084400030721     C                   ENDIF
084500030721     C                   ENDIF
084600030721     C*** NCL
084700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
084800030721     C                   EVAL      PiStr=SkSplitCSV(i)
084900030721     C                   EXSR      CHKNUM
085000030721     C                   IF        PiInt=*on
085100030721     C                   Z-ADD     PiVal         VABNCL
085200030721     C                   ELSE
085300030721     C                   SETON                                        32
085400030721     C                   EVAL      VABNCL = *zeros
085500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
085600030721     C                             + ' ' + 'VABNCL'
085700030721     C                   ENDIF
085800030721     C                   ENDIF
085900030721     C*** PKB
086000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
086100030721     C                   EVAL      PiStr=SkSplitCSV(i)
086200030721     C                   EXSR      CHKNUM
086300030721     C                   IF        PiNum=*on
086400030721     C                   Z-ADD     PiVal         VABPKB
086500030721     C                   ELSE
086600030721     C                   SETON                                        32
086700030721     C                   EVAL      VABPKB = *zeros
086800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
086900030721     C                             + ' ' + 'VABPKB'
087000030721     C                   ENDIF
087100030721     C                   ENDIF
087200030721     C*** CAS
087300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
087400030721     C                   EVAL      PiStr=SkSplitCSV(i)
087500030721     C                   EXSR      CHKNUM
087600030721     C                   IF        PiNum=*on
087700030721     C                   Z-ADD     PiVal         VABCAS
087800030721     C                   ELSE
087900030721     C                   SETON                                        32
088000030721     C                   EVAL      VABCAS = *zeros
088100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
088200030721     C                             + ' ' + 'VABCAS'
088300030721     C                   ENDIF
088400030721     C                   ENDIF
088500030721     C*** CCM
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
088700030721     C                   EVAL      PiStr=SkSplitCSV(i)
088800030721     C                   EXSR      CHKNUM
088900030721     C                   IF        PiInt=*on      AND
089000030721     C                             PiVal<=9999999 AND
089100030721     C                             PiVal>*zeros
089200030721     C                   Z-ADD     PiVal         VABCCM
089300030721     C                   ELSE
089400030721     C                   SETON                                        32
089500030721     C                   EVAL      VABCCM = *zeros
089600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
089700030721     C                             + ' ' + 'VABCCM'
089800030721     C                   ENDIF
089900030721     C                   ENDIF
090000030721     C*** LNP
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
090200030721     C                   EVAL      PiStr=SkSplitCSV(i)
090300030721     C                   EXSR      CHKNUM
090400030721     C                   IF        PiInt=*on    AND
090500030721     C                             PiVal<=999   AND
090600030721     C                             PiVal>*zeros
090700030721     C                   Z-ADD     PiVal         VABLNP
090800030721     C                   ELSE
090900030721     C                   SETON                                        32
091000030721     C                   EVAL      VABLNP = *zeros
091100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
091200030721     C                             + ' ' + 'VABLNP'
091300030721     C                   ENDIF
091400030721     C                   ENDIF
091500030721     C*** NRS
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
091700030721     C                   EVAL      PiStr=SkSplitCSV(i)
091800030721     C                   EXSR      CHKNUM
091900030721     C                   IF        PiInt=*on AND
092000030721     C                             PiVal<=99
092100030721     C                   Z-ADD     PiVal         VABNRS
092200030721     C                   ELSE
092300030721     C                   SETON                                        32
092400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092500030721     C                             + ' ' + 'VABNRS'
092600030721     C                   ENDIF
092700030721     C                   ENDIF
092800030721     C*** LNA
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
093000030721     C                   EVAL      PiStr=SkSplitCSV(i)
093100030721     C                   EXSR      CHKNUM
093200030721     C                   IF        PiInt=*on    AND
093300030721     C                             PiVal<=999   AND
093400030721     C                             PiVal>*zeros
093500030721     C                   Z-ADD     PiVal         VABLNA
093600030721     C                   ELSE
093700030721     C                   SETON                                        32
093800030721     C                   EVAL      VABLNA = *zeros
093900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094000030721     C                             + ' ' + 'VABLNA'
094100030721     C                   ENDIF
094200030721     C                   ENDIF
094300030721     C*** CTR
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
094500030721     C                   EVAL      PiStr=SkSplitCSV(i)
094600030721     C                   EXSR      CHKNUM
094700030721     C                   IF        PiInt=*on  AND
094800030721     C                             PiVal<=999
094900030721     C                   Z-ADD     PiVal         VABCTR
095000030721     C                   ELSE
095100030721     C                   SETON                                        32
095200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095300030721     C                             + ' ' + 'VABCTR'
095400030721     C                   ENDIF
095500030721     C                   ENDIF
095600030721     C*** IAS
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
095800030721     C                   EVAL      PiStr=SkSplitCSV(i)
095900030721     C                   EXSR      CHKNUM
096000030721     C                   IF        PiNum=*on
096100030721     C                   Z-ADD     PiVal         VABIAS
096200030721     C                   ELSE
096300030721     C                   SETON                                        32
096400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096500030721     C                             + ' ' + 'VABIAS'
096600030721     C                   ENDIF
096700030721     C                   ENDIF
096800030721     C*** VLB
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
097000030721     C                   EVAL      PiStr=SkSplitCSV(i)
097100030721     C                   EXSR      CHKNUM
097200030721     C                   IF        PiNum=*on
097300030721     C                   Z-ADD     PiVal         VABVLB
097400030721     C                   ELSE
097500030721     C                   SETON                                        32
097600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097700030721     C                             + ' ' + 'VABVLB'
097800030721     C                   ENDIF
097900030721     C                   ENDIF
098000030721     C*** QFT
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
098200030721     C                   EVAL      PiStr=SkSplitCSV(i)
098300030721     C                   EXSR      CHKNUM
098400030721     C                   IF        PiNum=*on
098500030721     C                   Z-ADD     PiVal         VABQFT
098600030721     C                   ELSE
098700030721     C                   SETON                                        32
098800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098900030721     C                             + ' ' + 'VABQFT'
099000030721     C                   ENDIF
099100030721     C                   ENDIF
099200030721     C*** NCD
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
099400030721     C                   EVAL      PiStr=SkSplitCSV(i)
099500030721     C                   EXSR      CHKNUM
099600030721     C                   IF        PiInt=*on      AND
099700030721     C                             PiVal<=9999999
099800030721     C                   Z-ADD     PiVal         VABNCD
099900030721     C                   ELSE
100000030721     C                   SETON                                        32
100100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100200030721     C                             + ' ' + 'VABNCD'
100300030721     C                   ENDIF
100400030721     C                   ENDIF
100500030721     C*** NCA
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
100700030721     C                   EVAL      PiStr=SkSplitCSV(i)
100800030721     C                   EXSR      CHKNUM
100900030721     C                   IF        PiInt=*on      AND
101000030721     C                             PiVal<=9999999
101100030721     C                   Z-ADD     PiVal         VABNCA
101200030721     C                   ELSE
101300030721     C                   SETON                                        32
101400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101500030721     C                             + ' ' + 'VABNCA'
101600030721     C                   ENDIF
101700030721     C                   ENDIF
101800030721     C*** ZNC
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
102000030721     C                   EVAL      PiStr=SkSplitCSV(i)
102100030721     C                   EXSR      CHKNUM
102200030721     C                   IF        PiInt=*on AND
102300030721     C                             PiVal<=99
102400030721     C                   Z-ADD     PiVal         VABZNC
102500030721     C                   ELSE
102600030721     C                   SETON                                        32
102700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102800030721     C                             + ' ' + 'VABZNC'
102900030721     C                   ENDIF
103000030721     C                   ENDIF
103100030721     C*** DCR
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
103300030721     C                   EVAL      PiStr=SkSplitCSV(i)
103400030721     C                   EXSR      CHKNUM
103500030721     C                   IF        PiInt=*on       AND
103600030721     C                             PiVal<=99999999
103700030721     C                   Z-ADD     PiVal         VABDCR
103800030721     C                   ELSE
103900030721     C                   SETON                                        32
104000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104100030721     C                             + ' ' + 'VABDCR'
104200030721     C                   ENDIF
104300030721     C                   ENDIF
104400030721     C*** HCR
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
104600030721     C                   EVAL      PiStr=SkSplitCSV(i)
104700030721     C                   EXSR      CHKNUM
104800030721     C                   IF        PiInt=*on   AND
104900030721     C                             PiVal<=9999
105000030721     C                   Z-ADD     PiVal         VABHCR
105100030721     C                   ELSE
105200030721     C                   SETON                                        32
105300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105400030721     C                             + ' ' + 'VABHCR'
105500030721     C                   ENDIF
105600030721     C                   ENDIF
105700030721     C*** VMD
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
105900030721     C                   EVAL      PiStr=SkSplitCSV(i)
106000030721     C                   EXSR      CHKNUM
106100030721     C                   IF        PiNum=*on
106200030721     C                   Z-ADD     PiVal         VABVMD
106300030721     C                   ELSE
106400030721     C                   SETON                                        32
106500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106600030721     C                             + ' ' + 'VABVMD'
106700030721     C                   ENDIF
106800030721     C                   ENDIF
106900030721     C*** ANT
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
107100030721     C                   EVAL      PiStr=SkSplitCSV(i)
107200030721     C                   EXSR      CHKNUM
107300030721     C                   IF        PiInt=*on        AND
107400030721     C                             PiVal<=999999999
107500030721     C                   Z-ADD     PiVal         VABANT
107600030721     C                   ELSE
107700030721     C                   SETON                                        32
107800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107900030721     C                             + ' ' + 'VABANT'
108000030721     C                   ENDIF
108100030721     C                   ENDIF
108200030721     C*** CMO
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
108400030721     C                   EVAL      PiStr=SkSplitCSV(i)
108500030721     C                   EXSR      CHKNUM
108600030721     C                   IF        PiInt=*on
108700030721     C                   Z-ADD     PiVal         Num5_0
108800030721     C                   MOVEL(p)  Num5_0        VABCMO
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      VABCMO = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABCMO'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600080331     C*
109700080331     C* =======> CONSIDERAZIONI PARTICOLATI DENTRO CICLO
109800080331     C*
109900080331     C*** W_NOTE
110000080331     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOTE'
110100080331     C                   EVAL      VABNOT = %subst(SkSplitCSV(i):1:35)
110200080331     C                   EVAL      VABNT2 = %subst(SkSplitCSV(i):36:35)
110300080331     C                   ENDIF
110400080331     C* <======= *END*
110500030715     C*
110600030715     C                   ENDIF
110700030715     C                   EVAL      i = i + 1
110800030715     C                   ENDDO
110900040113     C*
111000040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
111100040113     C                   IF        VABCAS > *zeros
111200040113     C                   EVAL      VABCBO = '4'
111300040113     C                   EVAL      VABVCA = 'EUR'
111400080505     C                   EVAL      VABTIC = 'BM'
111500040113     C                   ELSE
111600040113     C                   EVAL      VABCBO = '1'
111700040113     C                   ENDIF
111800020204     C*
111900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
112000020204     C                   EXSR      CHKIMPDIV
112100030715     C                   ENDIF
112200020204     C*
112300010607     C                   ENDSR
112400010607     C*----------------------------------------------------*
112500010601
112600020204
112700020204     C*----------------------------------------------------*
112800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
112900020204     C*----------------------------------------------------*
113000020204     C     CHKIMPDIV     BEGSR
113100020204     C*
113200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
113300020204     C                   Z-ADD     *zeros        wrkDec            9 9
113400020204     C*
113500020204     C* Come prima cosa effettuo considerazioni sulla divisa
113600020204     C                   IF        vabIAS > *zeros
113700020204     C                   IF        vabVAS <> 'EUR'
113800020204     C                   EVAL      vabVAS =  'ITL'
113900020204     C                   ENDIF
114000020204     C                   ENDIF
114100020204     C*
114200020204     C                   IF        vabCAS > *zeros
114300020204     C                   IF        vabVCA <> 'EUR'
114400020204     C                   EVAL      vabVCA =  'ITL'
114500020204     C                   ENDIF
114600020204     C                   ENDIF
114700020204     C*
114800020204     C                   IF        vabVMD > *zeros
114900020204     C                   IF        vabVAD <> 'EUR'
115000020204     C                   EVAL      vabVAD =  'ITL'
115100020204     C                   ENDIF
115200020204     C                   ENDIF
115300020204     C*
115400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
115500020204     C                   Z-ADD     vabIAS        wrkDec
115600020204     C                   IF        wrkDec > *zeros
115700020204     C                   IF        vabVAS = 'ITL'
115800020204     C                   EVAL      vabIAS = *zeros
115900020204     C                   ENDIF
116000020204     C                   ENDIF
116100020204     C*
116200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
116300020204     C                   Z-ADD     vabCAS        wrkDec
116400020204     C                   IF        wrkDec > *zeros
116500020204     C                   IF        vabVCA = 'ITL'
116600020204     C                   EVAL      vabCAS = *zeros
116700020204     C                   ENDIF
116800020204     C                   ENDIF
116900020204     C*
117000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
117100020204     C                   Z-ADD     vabVMD        wrkDec
117200020204     C                   IF        wrkDec > *zeros
117300020204     C                   IF        vabVAD = 'ITL'
117400020204     C                   EVAL      vabVMD = *zeros
117500020204     C                   ENDIF
117600020204     C                   ENDIF
117700020204     C*
117800020204     C                   ENDSR
117900020204     C***
118000020204
118100010330
118200010330
118300010330     C*----------------------------------------------------*
118400010330     C*  CONTROLLO NUMERICITA' CAMPI
118500010330     C*----------------------------------------------------*
118600010330     C     CHKNUM        BEGSR
118700010330     C*
118800010606     C                   IF        PiDecChr = *blanks
118900030715     C                   EVAL      PiDecChr = CharNUM
119000010606     C                   ENDIF
119100010606     C*
119200010606     C                   CALL(e)   'ISNUMERIC'
119300010330     C                   PARM                    PiStr            30
119400010606     C                   PARM                    PiDecChr          1
119500010330     C                   PARM      *ZEROS        PiVal            30 9
119600010330     C                   PARM      '0'           PiInt             1
119700010330     C                   PARM      '0'           PiNum             1
119800010330     C                   IF        %error
119900010606     C                   EVAL      PiNum=*off
120000010330     C                   ENDIF
120100010330     C*
120200010330     C                   ENDSR
120300010330     C***
120400010330
120500010601
120600010601
120700010601
120800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
120900010601     C     repfil        BEGSR
121000010601     C*
121100010601     C                   if        invfil = *zeros and
121200010601     C                             depfil > *zeros and
121300010629     C                             (vinflg = *blanks or
121400010629     C                              vinflg = *zeros)
121500010601     C
121600010601     C                   eval      invfil = depfil
121700010601     C                   endif
121800010601     C*
121900010601     C                   if        depfil <> invfil and
122000010601     C                             invfil > *zeros
122100010601     C                   eval      flgMulti = '1'
122200010601     C                   if        vinflg = *blanks
122300010601     C                   add       1             cntNonEl
122400010601     C                   endif
122500010601     C                   endif
122600010601     C*
122700010601     C                   if        vinflg = '2'
122800010601     C                   eval      flgStato = '2'
122900010601     C                   endif
123000010601     C*
123100010601     C                   ENDSR
123200010601     C***
123300010601
123400010601
123500010601
123600010330
123700010330
123800010330
123900990920      /TITLE Invio dei dati al punto operativo.
124000000613     C     invio         BEGSR
124100990920     C*
124200990920     C                   reset                   dscmz
124300010601     C                   move      invfil        cmzdst
124400990920     C                   eval      cmzfld = vlrfou
124500990920     C                   eval      cmzmbd = vlrhdl
124600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
124700000710     C                   if        prmfir = *blanks
124800021025     C                   eval      cmzfla = 'FIVAB00F'
124900021025     C                   eval      cmzmba = 'FIVAB00F'
125000000710     C                   else
125100000710     C                   eval      cmzfla = prmfir
125200000710     C                   eval      cmzmba = prmfir
125300000710     C                   endif
125400990920     C                   eval      cmznrr = *zeros
125500990920     C                   move      §ctrok        cmznrr
125600021018     C                   eval      cmzlba = vlrfl1
125700990920     C                   call(e)   'TIS711C'
125800990920     C                   parm                    dscmz
125900990921     C                   parm      *blanks       esito
126000990920     C                   if        %error
126100990920     C                             or cmzerr = '1'
126200990921     C                             or esito  = '1'
126300000710     C                   eval      wrkesito = '3'
126400990920     C                   endif
126500990920     C*
126600000613     C                   ENDSR
126700990910
126800010601
126900010601
127000010601
127100010601
127200010601      /TITLE Invio dei dati al punto operativo.
127300010601     C     opeini        BEGSR
127400010601     C*
127500010601     C* Inizializzo flag e contatori operativi
127600010601     C                   movel     '0'           flgGiro           1
127700010601     C                   movel     '0'           flgMulti          1
127800010601     C                   movel     '1'           flgStato          1
127900010615     C                   movel     '0'           flgOk             1
128000010601     C                   z-add     *zeros        cntNonEl         10 0
128100010601     C                   z-add     *zeros        depfil            3 0
128200010601     C                   z-add     *zeros        invfil            3 0
128300010601     C*
128400010601     C                   ENDSR
128500010601     C***
128600010601
128700010601
128800050421
128900050421     C     *pssr         BEGSR
129000050421     C*
129100050421     C                   eval      wrkesito = '2'
129200050421     C*
129300050421     C                   ENDSR     '*CANCL'
129400050421     C***
129500050421
129600010601
129700010330
129800010330
129900000613     C     *inzsr        BEGSR
130000990910     C*
130100990910     C     *entry        plist
130200990920     C                   parm                    tivlrds
130300990921     C                   parm      wrkesito      esito
130400000724     C                   parm                    prmlit
130500000710     C                   parm                    prmfir
130600010330     C*
130700010330     C* CALCOLA LA DATA CORRENTE
130800010330     C                   time                    wn14             14 0
130900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
131000010330     C                   z-add     wn8           g08dat
131100010330     C                   z-add     *zeros        g08inv
131200010330     C                   movel     '0'           g08err
131300010330     C                   call      'XSRDA8'
131400010330     C                   parm                    wlbda8
131500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
131600000613     C*
131700000613     C                   ENDSR
131800000613     C***
131900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
132000090618VABRMA;*;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABPKB;VABNCL;VABCAS;W_NOTE;*;VABRMN
132100040113
132200040113
