000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040223     Ftitv97p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040223     Ftitv97ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600040113     D*------------
001700040113     D* SCHIEDE A PROGRAMMA
001800040113     D*------------
001900040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700010330     D esito           s              1
002800010330     D prmlit          s             10
002900010330     D prmfir          s             10
003000010330     D wrkesito        s                   like(esito)
003100010330     D wrkdata         s               d
003200010330     D wrkora          s               t
003300010330     D rrnum           s              6  0 INZ(*zeros)
003400010330     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600030715     D*------------------
003700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003800030715     D*------------------
003900030715     D SkSplitFLD      S             10    DIM(100)
004000030715     D SkSplitCSV      S            256    DIM(100)
004100030715     D CharCSV         S              1
004200030715     D CharTXT         S              1
004300030715     D CharNUM         S              1
004400030715     D posDa           S              3  0 INZ(*zeros)
004500030715     D posA            S              3  0 INZ(*zeros)
004600030715     D i               s              3  0 INZ(1)
004700030715     D wGiro           s              1  0 INZ(*zeros)
004800030822     D*------------------
004900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005000030822     D*------------------
005100030822     D posDaDft        S              3  0 INZ(*zeros)
005200030822     D posADft         S              3  0 INZ(*zeros)
005300030822     D j               s              3  0 INZ(1)
005400030822     D wGiroDft        s              1  0 INZ(*zeros)
005500010330     D*------------------
005600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700010330     D*------------------
005800010330     D WLBDA8          DS                  INZ
005900010330     D  G08DAT                 1      8  0
006000010330     D  G08INV                 9     16  0
006100010330     D  G08ERR                17     17
006200010330     D  G08TGI                18     22  0
006300031201     D*------------------
006400031201     D* Costanti
006500031201     D*------------------
006600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006800010330
006900010330
007000010330
007100990915     C                   time                    wrkdata
007200990915     C                   time                    wrkora
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500990921     C                   reset                   wrkesito
007600010601     C*
007700010601     C                   exsr      opeini
007800010605     C                   exsr      rwvab
007900010601     C*
008000010601     C                   seton                                        lr
008100010601
008200010601
008300010601
008400010601
008500010601     C*--------------------------------------------------------
008600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008700010601     C*--------------------------------------------------------
008800010601     C     PREELA        BEGSR
008900010601     C*
009000010601     C* SE OCCORRE SPEDIRE IN FILIALE
009100010601     C                   if        invfil <> *zeros and
009200010601     C                             flgGiro = '0'
009300010601     C*
009400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009500010601     C                   eval      flgGiro = '1'
009600000724     C*
009700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009900010601     C     invfil        chain     azorg01l
010000000724     C                   if        %found
010100000724     C                   movel(p)  CMD(1)        depcmd
010200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010300000724     C*
010400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010500011113     C                   Z-ADD     150           LENGH            15 5
010600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010700000724     C                   PARM                    depcmd
010800000724     C                   PARM                    LENGH
010900000724     C*
011000000724     C                   endif
011100000616     C*
011200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011300000616     C                   movel(p)  CMD(2)        depcmd
011400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011500000616     C*
011600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011700011113     C                   Z-ADD     150           LENGH            15 5
011800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000616     C                   PARM                    depcmd
012000000616     C                   PARM                    LENGH
012100000613     C*
012200040223     C                   if        not %open(titv97ps)
012300040223     C                   open      titv97ps
012400030715     C                   except    testdett
012500000613     C                   endif
012600010601     C*
012700010601     C                   endif
012800010601     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500010601
013600010601
013700010601
013800010601     C*--------------------------------------------------------
013900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014000010601     C*--------------------------------------------------------
014100010601     C     ENDELA        BEGSR
014200990915     C*
014300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014400000613     C*
014500040223     C                   if        %open(titv97ps)
014600000616     C                   except    findett
014700040223     C                   close     titv97ps
014800000613     C                   endif
014900000616     C*
015000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015100011113     C                   Z-ADD     150           LENGH            15 5
015200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015300000616     C                   PARM                    CMD(3)
015400000616     C                   PARM                    LENGH
015500000616     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000010601
016100010601
016200000613
016300000613     C*--------------------------------------------------------
016400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016500000613     C*--------------------------------------------------------
016600000621     C     STPR          BEGSR
016700000613     C*
016800040223     C                   if        not %open(titv97p)
016900040223     C   50              open      titv97p
017000990915     C                   endif
017100990915     C*
017200030715     C   50              except    riepilogo
017300990915     C*
017400040223     C                   if        %open(titv97p)
017500040223     C   50              close     titv97p
017600990914     C                   endif
017700000613     C*
017800000613     C                   ENDSR
017900000613     C***
018000000613
018100010330
018200010601
018300010601
018400010601
018500010330     C*--------------------------------------------------------
018600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018700010330     C*--------------------------------------------------------
018800010605     C     RWVAB         BEGSR
018900010330     C*
019000010330     C                   if        not %open(tivin00r)
019100010330     C                   open      tivin00r
019200010330     C                   endif
019300021025     C                   if        not %open(fivabwwr)
019400021025     C                   open      fivabwwr
019500010330     C                   endif
019600010330     C*
019700010604     C                   clear                   §CTROK
019800010604     C                   clear                   §CTRMO
019900010604     C                   clear                   §CTRNO
020000010330     C*
020100030822     C                   do        *HIVAL
020200010330     C*
020300030822     C                   read      tivin00r                               70
020400010618     C*
020500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020600010618     C                   if        vinflg = '1'
020700010618     C                   eval      flgOk = '1'
020800010618     C                   endif
020900010618     C*
021000010330     C                   if        vindta > *blanks
021100010330     C                   add       1             rrnum
021200010330     C*
021300010601     C                   if        *in70 = *off and
021400010330     C                             (vinflg = *blanks
021500010330     C                              or vinflg = '0'
021600010330     C                              or vinflg = '2')
021700010330     C*
021800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021900010711     C                   if        vinflg = *blanks or vinflg = '0'
022000010711     C                   clear                   vinmsg
022100010711     C                   endif
022200010601     C*
022300010330     C                   exsr      inzvar
022400010605     C                   exsr      impvab
022500010601     C*
022600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022700010605     C                   eval      depfil = VABLNP
022800010601     C                   exsr      repfil
022900010601     C                   if        depfil = invfil
023000021025     C                   if        vlrpoi = 999
023100030822     C                   move(P)   invfil        VABFGS
023200021025     C                   else
023300030822     C                   move(P)   vlrpoi        VABFGS
023400021025     C                   endif
023500010601     C*
023600010601     C                   exsr      PREELA
023700010601     C*
023800010604     C* Ebbene...
023900010604     C*
024000030822     C  N31              add       1             §CTROK            7 0
024100030822     C   32              add       1             §CTRMO            7 0
024200030822     C   31              add       1             §CTRNO            7 0
024300030822     C                   if        wGiro = 2
024400030822     C  N31              write     FIVAB000
024500030822     C                   endif
024600010604     C*
024700010604     C                   if        *in31 = *off and
024800010604     C                             *in32 = *off
024900010604     C                   eval      vinflg = '1'
025000010604     C                   else
025100010604     C                   eval      recko = vindta
025200010604     C                   except    rigadett
025300010604     C                   eval      vinflg = '2'
025400010604     C                   endif
025500010604     C                   endif
025600010601     C*
025700010604     C                   endif
025800010604     C*
025900010330     C                   else
026000010330     C                   eval      vinflg = '1'
026100010330     C                   endif
026200010601     C*
026300010601     C  N70              update    tivin000
026400010330     C*
026500030822     C  N70              enddo
026600010601     C
026700010601     C*
026800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026900010601     C                   if        cntNonEl = *zeros or
027000010601     C                             flgMulti = '0'
027100010330     C* Se non ci sono record con errori ...
027200010601     C                   if        §ctrno = 0 and
027300010604     C                             §ctrmo = 0 and
027400010601     C                             flgStato <> '2'
027500010330     C* ... restituisco esito OK.
027600010330     C                   eval      wrkesito = '0'
027700010330     C                   else
027800010330     C                   if        §ctrok > 0
027900010330     C                   eval      wrkesito = '1'
028000010330     C                   else
028100010615     C                   if        flgOk = '0'
028200010615     C                   eval      wrkesito = '2'
028300010615     C                   else
028400010615     C                   eval      wrkesito = '6'
028500010615     C                   endif
028600010330     C                   endif
028700010330     C                   endif
028800010601     C                   else
028900010601     C                   eval      wrkesito = '9'
029000010601     C                   endif
029100010330     C*
029200010330     C                   if        %open(tivin00r)
029300010330     C                   close     tivin00r
029400010330     C                   endif
029500021025     C                   if        %open(fivabwwr)
029600021025     C                   close     fivabwwr
029700010330     C                   endif
029800010601     C*
029900010601     C                   if        vlrpoi <> 999
030000010601     C                   eval      invfil = vlrpoi
030100010601     C                   endif
030200010330     C*
030300010330     C                   if        §ctrok > 0
030400010601     C                             and invfil > *zeros
030500010330     C                   exsr      invio
030600010330     C                   endif
030700010601     C*
030800010618     C                   if        flgGiro = '1'
030900010601     C                   exsr      endela
031000010618     C                   endif
031100010330     C*
031200010330     C                   ENDSR
031300010330     C***
031400010601
031500010601
031600010601
031700010330     C*----------------------------------------------------*
031800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031900010330     C*----------------------------------------------------*
032000010330     C     INZVAR        BEGSR
032100010330     C*
032200030822     C* Inizializzo variabili di wrk
032300020204     C                   Z-ADD     *zeros        Num5_0            5 0
032400030822     C*
032500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
032600030822     C                   CLEAR                   FIVAB000
032700030822     C                   CLEAR                   SkSplitCSV
032800030822     C                   EVAL      i = 1
032900030822     C                   EVAL      posDa = *zeros
033000030822     C                   EVAL      posA  = *zeros
033100030822     C*
033200030822     C* Reimposto i valori di default
033300030822     C                   EXSR      DEFCAM
033400030822     C*
033500010330     C                   ENDSR
033600010330     C*----------------------------------------------------*
033700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
033800010330     C*----------------------------------------------------*
033900010330     C     DEFCAM        BEGSR
034000030822     C*
034100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
034200030715     C* e delimitatore testo.
034300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
034400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
034500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
034600030822     C*
034700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
034800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
034900030715     C                   SETON                                        50
035000030715     C                   ELSE
035100030715     C                   SETOFF                                       50
035200030715     C                   ENDIF
035300030822     C*
035400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
035500030822     C                   EVAL      posDaDft = 1
035600030822     C                   EVAL      posADft  = 0
035700030822     C                   EVAL      wGiroDft = 0
035800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
035900030822     C                             posDaDft > 0
036000030822     C*
036100030822     C* Gestisco il 1° giro
036200030822     C                   IF        wGiroDft = 0
036300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
036400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
036500030822     C* Incremento il contatore dei "giri"
036600030822     C                   EVAL      wGiroDft = 1
036700030822     C                   ELSE
036800030822     C                   EVAL      posDaDft = posADft
036900030822     C                   ENDIF
037000030822     C* Eseguo lo scan x trovare la fine del campo corrente
037100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
037200030822     C*
037300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
037400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
037500030822     C                   IF        posDaDft > 0 AND
037600030822     C                             posADft  > 0
037700030822     C* NCL
037800030822     C                   IF        %subst(
037900030822     C                             %subst(vlrppt:posDaDft+1:
038000030822     C                             posADft-posDaDft-1):1:3)
038100030822     C                             = 'NCL'
038200030822     C                   EVAL      PiStr=%trim(%subst(
038300030822     C                             %subst(vlrppt:posDaDft+1:
038400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038500030822     C                   EXSR      CHKNUM
038600030822     C                   IF        PiInt=*on
038700030822     C                   Z-ADD     PiVal         VABNCL
038800030822     C                   ENDIF
038900030822     C                   ENDIF
039000030822     C* CCM
039100030822     C                   IF        %subst(
039200030822     C                             %subst(vlrppt:posDaDft+1:
039300030822     C                             posADft-posDaDft-1):1:3)
039400030822     C                             = 'CCM'
039500030822     C                   EVAL      PiStr=%trim(%subst(
039600030822     C                             %subst(vlrppt:posDaDft+1:
039700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039800030822     C                   EXSR      CHKNUM
039900030822     C                   IF        PiInt=*on
040000030822     C                   Z-ADD     PiVal         VABCCM
040100030822     C                   ENDIF
040200030822     C                   ENDIF
040300030822     C* LNP
040400030822     C                   IF        %subst(
040500030822     C                             %subst(vlrppt:posDaDft+1:
040600030822     C                             posADft-posDaDft-1):1:3)
040700030822     C                             = 'LNP'
040800030822     C                   EVAL      PiStr=%trim(%subst(
040900030822     C                             %subst(vlrppt:posDaDft+1:
041000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041100030822     C                   EXSR      CHKNUM
041200030822     C                   IF        PiInt=*on
041300030822     C                   Z-ADD     PiVal         VABLNP
041400030822     C                   ENDIF
041500030822     C                   ENDIF
041600030822     C* NRS
041700030822     C                   IF        %subst(
041800030822     C                             %subst(vlrppt:posDaDft+1:
041900030822     C                             posADft-posDaDft-1):1:3)
042000030822     C                             = 'NRS'
042100030822     C                   EVAL      PiStr=%trim(%subst(
042200030822     C                             %subst(vlrppt:posDaDft+1:
042300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042400030822     C                   EXSR      CHKNUM
042500030822     C                   IF        PiInt=*on
042600030822     C                   Z-ADD     PiVal         VABNRS
042700030822     C                   ENDIF
042800030822     C                   ENDIF
042900030822     C* CTR
043000030822     C                   IF        %subst(
043100030822     C                             %subst(vlrppt:posDaDft+1:
043200030822     C                             posADft-posDaDft-1):1:3)
043300030822     C                             = 'CTR'
043400030822     C                   EVAL      PiStr=%trim(%subst(
043500030822     C                             %subst(vlrppt:posDaDft+1:
043600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043700030822     C                   EXSR      CHKNUM
043800030822     C                   IF        PiInt=*on
043900030822     C                   Z-ADD     PiVal         VABCTR
044000030822     C                   ENDIF
044100030822     C                   ENDIF
044200030822     C* PKB
044300030822     C                   IF        %subst(
044400030822     C                             %subst(vlrppt:posDaDft+1:
044500030822     C                             posADft-posDaDft-1):1:3)
044600030822     C                             = 'PKB'
044700030822     C                   EVAL      PiStr=%trim(%subst(
044800030822     C                             %subst(vlrppt:posDaDft+1:
044900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045000030822     C                   EXSR      CHKNUM
045100030822     C                   IF        PiNum=*on
045200030822     C                   Z-ADD     PiVal         VABPKB
045300030822     C                   ENDIF
045400030822     C                   ENDIF
045500030822     C* VLB
045600030822     C                   IF        %subst(
045700030822     C                             %subst(vlrppt:posDaDft+1:
045800030822     C                             posADft-posDaDft-1):1:3)
045900030822     C                             = 'VLB'
046000030822     C                   EVAL      PiStr=%trim(%subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300030822     C                   EXSR      CHKNUM
046400030822     C                   IF        PiNum=*on
046500030822     C                   Z-ADD     PiVal         VABVLB
046600030822     C                   ENDIF
046700030822     C                   ENDIF
046800030822     C* QFT
046900030822     C                   IF        %subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):1:3)
047200030822     C                             = 'QFT'
047300030822     C                   EVAL      PiStr=%trim(%subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047600030822     C                   EXSR      CHKNUM
047700030822     C                   IF        PiNum=*on
047800030822     C                   Z-ADD     PiVal         VABQFT
047900030822     C                   ENDIF
048000030822     C                   ENDIF
048100030822     C* CBO
048200030822     C                   IF        %subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):1:3)
048500030822     C                             = 'CBO'
048600030822     C                   EVAL      VABCBO=%trim(%subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048900030822     C                   ENDIF
049000030822     C* TSP
049100030822     C                   IF        %subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):1:3)
049400030822     C                             = 'TSP'
049500030822     C                   EVAL      VABTSP=%trim(%subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049800030822     C                   ENDIF
049900030822     C* VAS
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'VAS'
050400030822     C                   EVAL      VABVAS=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   ENDIF
050800030822     C* VCA
050900030822     C                   IF        %subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):1:3)
051200030822     C                             = 'VCA'
051300030822     C                   EVAL      VABVCA=%trim(%subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600030822     C                   ENDIF
051700030822     C* GCA
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'GCA'
052200030822     C                   EVAL      VABGCA=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   ENDIF
052600030822     C* CTM
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'CTM'
053100030822     C                   EVAL      VABCTM=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   ENDIF
053500030822     C* FFD
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'FFD'
054000030822     C                   EVAL      VABFFD=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   ENDIF
054400030822     C* VAD
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'VAD'
054900030822     C                   EVAL      VABVAD=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   ENDIF
055300030822     C* GMA
055400030822     C                   IF        %subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):1:3)
055700030822     C                             = 'GMA'
055800030822     C                   EVAL      VABGMA=%trim(%subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100030822     C                   ENDIF
056200030822     C* GGA
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'GGA'
056700030822     C                   EVAL      VABGGA=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   ENDIF
057100030822     C* GVA
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'GVA'
057600030822     C                   EVAL      VABGVA=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   ENDIF
058000030822     C* TC1
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'TC1'
058500030822     C                   EVAL      VABTC1=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   ENDIF
058900030822     C* TC2
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'TC2'
059400030822     C                   EVAL      VABTC2=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   ENDIF
059800030822     C* ...
059900030822     C                   ENDIF
060000030822     C                   ENDDO
060100020204     C*
060200010330     C                   ENDSR
060300010607     C*----------------------------------------------------*
060400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
060500010607     C*----------------------------------------------------*
060600010607     C     IMPVAB        BEGSR
060700010607     C*
060800010607     C                   SETOFF                                       3132
060900030515     C*
061000010607     C                   MOVEL     datcor        VABAAS
061100010607     C                   MOVE      datcor        VABMGS
061200010607     C*
061300030715     C**********
061400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
061500030715     C**********
061600030715     C                   IF        wGiro = *zeros
061700040113     C*
061800040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
061900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
062000040113     C                                      %trim(intcol(3))
062100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
062200031201     C     minu:maiu     XLATE     vindta        vindta
062300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
062400030715     C                   DOW       posDa <= %len(%trim(vindta))
062500030715     C*
062600030715     C* Gestisco il 1° campo
062700030715     C                   IF        i = 1
062800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
062900030715     C                   EVAL      posDa = 1
063000030715     C* Eseguo lo scan x trovare la fine del primo campo
063100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
063200030715     C* A questo punto "estraggo" il campo corrente
063300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
063400030715     C                                                    (posA-posDa))
063500030715     C* X i campi successivi al 1°
063600030715     C                   ELSE
063700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
063800030715     C                   EVAL      posDa = posA + 1
063900030715     C* Eseguo lo scan x trovare la fine del campo corrente
064000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
064100030715     C* Gestisco l'ultimo campo
064200030715     C                   IF        posA = *zeros
064300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
064400030715     C* A questo punto "estraggo" il campo corrente
064500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
064600030715     C                                                    (posA-posDa))
064700030715     C                   LEAVE
064800030715     C                   ELSE
064900030715     C* A questo punto "estraggo" il campo corrente
065000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065100030715     C                                                    (posA-posDa))
065200030715     C                   ENDIF
065300030715     C                   ENDIF
065400030715     C* Incremento il contatore d campo
065500030715     C                   EVAL      i = i +1
065600030715     C                   ENDDO
065700030715     C*
065800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
065900030715     C* delimitazione testo
066000030715     C                   EVAL      i = 1
066100030715     C                   DOW       i < %elem(SkSplitFLD)
066200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
066300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
066400030715     C                   EVAL      i = i + 1
066500030715     C                   ENDDO
066600030715     C                   EVAL      wGiro = 1
066700030715     C                   ELSE
066800030715     C                   EVAL      wGiro = 2
066900031204     C**********
067000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
067100031204     C* d separatore campo
067200031204     C**********
067300031204     C                   EVAL      vindta = %trim(vindta)
067400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
067500031204     C                   EVAL      vindta = %subst(vindta:2)
067600031204     C                   ENDDO
067700031204     C*
067800031204     C                   Z-ADD     *zeros        lunghInput        4 0
067900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
068000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
068100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
068200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
068300031204     C                   ENDDO
068400030715     C**********
068500030715     C* Effettuo lo split del campo dati d input
068600030715     C**********
068700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068800030715     C                   DOW       posDa <= %len(%trim(vindta))
068900030715     C*
069000030715     C* Gestisco il 1° campo
069100030715     C                   IF        i = 1
069200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
069300030715     C                   EVAL      posDa = 1
069400030715     C* Eseguo lo scan x trovare la fine del primo campo
069500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069600030715     C* A questo punto "estraggo" il campo corrente
069700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
069800030715     C                                                    (posA-posDa))
069900030715     C* X i campi successivi al 1°
070000030715     C                   ELSE
070100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
070200030715     C                   EVAL      posDa = posA + 1
070300031204     C* Verifico che nn vi sia il campo nn valorizzato
070400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
070500031204     C* Se campo nn valorizzato skippo al prossimo
070600031204     C                   EVAL      SkSplitCSV(i) = *blanks
070700031204     C                   EVAL      posA  = posA + 1
070800031204     C                   ELSE
070900030715     C* Eseguo lo scan x trovare la fine del campo corrente
071000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071100030715     C* Gestisco l'ultimo campo
071200030715     C                   IF        posA = *zeros
071300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
071400030715     C* A questo punto "estraggo" il campo corrente
071500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
071600030715     C                                                    (posA-posDa))
071700030715     C                   LEAVE
071800030715     C                   ELSE
071900030715     C* A questo punto "estraggo" il campo corrente
072000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
072100030715     C                                                    (posA-posDa))
072200030715     C                   ENDIF
072300031204     C                   ENDIF
072400030715     C                   ENDIF
072500030715     C* Incremento il contatore d campo
072600030715     C                   EVAL      i = i +1
072700030715     C                   ENDDO
072800030715     C*
072900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
073000030715     C* delimitazione testo
073100030715     C                   EVAL      i = 1
073200030715     C                   DOW       i < %elem(SkSplitCSV)
073300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
073400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
073500030715     C                   EVAL      i = i + 1
073600030715     C                   ENDDO
073700030715     C*
073800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
073900030715     C                   EVAL      i = 1
074000030715     C                   DOW       i < %elem(SkSplitFLD)
074100030715     C*
074200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
074300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
074400030715     C                             SkSplitCSV(i) <> *blanks
074500030721     C*** ATB
074600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
074700030721     C                   EVAL      VABATB = SkSplitCSV(i)
074800030721     C                   ENDIF
074900030715     C*** RSD
075000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
075100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
075200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
075300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
075400030715     C* ==
075500030715     C                   ENDIF
075600030721     C*** RD2
075700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
075800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
075900030721     C                   ENDIF
076000030715     C*** IND
076100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
076200030715     C                   EVAL      VABIND = SkSplitCSV(i)
076300030715     C                   ENDIF
076400030715     C*** LOD
076500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
076600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
076700030715     C                   ENDIF
076800030715     C*** PRD
076900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
077000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
077100030715     C                   ENDIF
077200030721     C*** NZD
077300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
077400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
077500030721     C                   ENDIF
077600030715     C*** RMA
077700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
077800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
077900030715     C                   ENDIF
078000030715     C*** NOT
078100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
078200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
078300030715     C                   ENDIF
078400030715     C*** NT2
078500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
078600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
078700030715     C                   ENDIF
078800030715     C*** VCA
078900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
079000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
079100030715     C                   ENDIF
079200030715     C*** VAS
079300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
079400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
079500030715     C                   ENDIF
079600030715     C*** FFD
079700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
079800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
079900030715     C                   ENDIF
080000030715     C*** CBO
080100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
080200030715     C                   EVAL      VABCBO = SkSplitCSV(i)
080300030715     C                   ENDIF
080400030715     C
080500030721     C*** GC1
080600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
080700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
080800030721     C                   ENDIF
080900030721     C*** GC2
081000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
081100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
081200030721     C                   ENDIF
081300030721     C*** TSP
081400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
081500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
081600030721     C                   ENDIF
081700030721     C*** NAS
081800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
081900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
082000030721     C                   ENDIF
082100030721     C*** TIC
082200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
082300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
082400030721     C                   ENDIF
082500030721     C*** GCA
082600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
082700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
082800030721     C                   ENDIF
082900030721     C*** XCO
083000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
083100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
083200030721     C                   ENDIF
083300030721     C*** CTM
083400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
083500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
083600030721     C                   ENDIF
083700030721     C*** TCR
083800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
083900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
084000030721     C                   ENDIF
084100030721     C*** CTS
084200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
084300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
084400030721     C                   ENDIF
084500030721     C*** FTM
084600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
084700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
084800030721     C                   ENDIF
084900030721     C*** VAD
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
085100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** GMA
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
085500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*** GGA
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
085900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** GVA
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
086300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** TC1
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
086700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** TC2
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
087100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** SCL
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
087500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** RMO
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
087900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** NMO
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
088300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*
088600030721     C* Reperisco quindi i campi numerici...
088700030721     C*** CAD
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
088900030721     C                   EVAL      PiStr=SkSplitCSV(i)
089000030721     C                   EXSR      CHKNUM
089100030721     C                   IF        PiInt=*on
089200030721     C                   Z-ADD     PiVal         Num5_0
089300030721     C                   MOVEL(p)  Num5_0        VABCAD
089400030721     C                   ELSE
089500030721     C                   SETON                                        32
089600030721     C                   EVAL      VABCAD = *zeros
089700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
089800030721     C                             + ' ' + 'VABCAD'
089900030721     C                   ENDIF
090000030721     C                   ENDIF
090100030721     C*** RMN
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
090300030721     C                   EVAL      PiStr=SkSplitCSV(i)
090400030721     C                   EXSR      CHKNUM
090500030721     C                   IF        PiInt=*on
090600030721     C                   Z-ADD     PiVal         VABRMN
090700040113     C                   EVAL      VABRMA=SkSplitCSV(i)
090800030721     C                   ELSE
090900030721     C                   SETON                                        32
091000030721     C                   EVAL      VABRMN = 1
091100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
091200040113     C                             + ' ' + 'VABRMN VABRMA'
091300030721     C                   ENDIF
091400030721     C                   ENDIF
091500030721     C*** NSP
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
091700030721     C                   EVAL      PiStr=SkSplitCSV(i)
091800030721     C                   EXSR      CHKNUM
091900030721     C                   IF        PiInt=*on AND
092000030721     C                             PiVal<=9999999
092100030721     C                   Z-ADD     PiVal         VABNSP
092200030721     C                   ELSE
092300030721     C                   SETON                                        32
092400030721     C                   EVAL      VABNSP = *zeros
092500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092600030721     C                             + ' ' + 'VABNSP'
092700030721     C                   ENDIF
092800030721     C                   ENDIF
092900030721     C*** NCL
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
093100030721     C                   EVAL      PiStr=SkSplitCSV(i)
093200030721     C                   EXSR      CHKNUM
093300030721     C                   IF        PiInt=*on
093400030721     C                   Z-ADD     PiVal         VABNCL
093500030721     C                   ELSE
093600030721     C                   SETON                                        32
093700030721     C                   EVAL      VABNCL = *zeros
093800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093900030721     C                             + ' ' + 'VABNCL'
094000030721     C                   ENDIF
094100030721     C                   ENDIF
094200030721     C*** PKB
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
094400030721     C                   EVAL      PiStr=SkSplitCSV(i)
094500030721     C                   EXSR      CHKNUM
094600030721     C                   IF        PiNum=*on
094700030721     C                   Z-ADD     PiVal         VABPKB
094800030721     C                   ELSE
094900030721     C                   SETON                                        32
095000030721     C                   EVAL      VABPKB = *zeros
095100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095200030721     C                             + ' ' + 'VABPKB'
095300030721     C                   ENDIF
095400030721     C                   ENDIF
095500030721     C*** CAS
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
095700030721     C                   EVAL      PiStr=SkSplitCSV(i)
095800030721     C                   EXSR      CHKNUM
095900030721     C                   IF        PiNum=*on
096000030721     C                   Z-ADD     PiVal         VABCAS
096100030721     C                   ELSE
096200030721     C                   SETON                                        32
096300030721     C                   EVAL      VABCAS = *zeros
096400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096500030721     C                             + ' ' + 'VABCAS'
096600030721     C                   ENDIF
096700030721     C                   ENDIF
096800030721     C*** CCM
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
097000030721     C                   EVAL      PiStr=SkSplitCSV(i)
097100030721     C                   EXSR      CHKNUM
097200030721     C                   IF        PiInt=*on      AND
097300030721     C                             PiVal<=9999999 AND
097400030721     C                             PiVal>*zeros
097500030721     C                   Z-ADD     PiVal         VABCCM
097600030721     C                   ELSE
097700030721     C                   SETON                                        32
097800030721     C                   EVAL      VABCCM = *zeros
097900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098000030721     C                             + ' ' + 'VABCCM'
098100030721     C                   ENDIF
098200030721     C                   ENDIF
098300030721     C*** LNP
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
098500030721     C                   EVAL      PiStr=SkSplitCSV(i)
098600030721     C                   EXSR      CHKNUM
098700030721     C                   IF        PiInt=*on    AND
098800030721     C                             PiVal<=999   AND
098900030721     C                             PiVal>*zeros
099000030721     C                   Z-ADD     PiVal         VABLNP
099100030721     C                   ELSE
099200030721     C                   SETON                                        32
099300030721     C                   EVAL      VABLNP = *zeros
099400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099500030721     C                             + ' ' + 'VABLNP'
099600030721     C                   ENDIF
099700030721     C                   ENDIF
099800030721     C*** NRS
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
100000030721     C                   EVAL      PiStr=SkSplitCSV(i)
100100030721     C                   EXSR      CHKNUM
100200030721     C                   IF        PiInt=*on AND
100300030721     C                             PiVal<=99
100400030721     C                   Z-ADD     PiVal         VABNRS
100500030721     C                   ELSE
100600030721     C                   SETON                                        32
100700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100800030721     C                             + ' ' + 'VABNRS'
100900030721     C                   ENDIF
101000030721     C                   ENDIF
101100030721     C*** LNA
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
101300030721     C                   EVAL      PiStr=SkSplitCSV(i)
101400030721     C                   EXSR      CHKNUM
101500030721     C                   IF        PiInt=*on    AND
101600030721     C                             PiVal<=999   AND
101700030721     C                             PiVal>*zeros
101800030721     C                   Z-ADD     PiVal         VABLNA
101900030721     C                   ELSE
102000030721     C                   SETON                                        32
102100030721     C                   EVAL      VABLNA = *zeros
102200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102300030721     C                             + ' ' + 'VABLNA'
102400030721     C                   ENDIF
102500030721     C                   ENDIF
102600030721     C*** CTR
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
102800030721     C                   EVAL      PiStr=SkSplitCSV(i)
102900030721     C                   EXSR      CHKNUM
103000030721     C                   IF        PiInt=*on  AND
103100030721     C                             PiVal<=999
103200030721     C                   Z-ADD     PiVal         VABCTR
103300030721     C                   ELSE
103400030721     C                   SETON                                        32
103500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103600030721     C                             + ' ' + 'VABCTR'
103700030721     C                   ENDIF
103800030721     C                   ENDIF
103900030721     C*** IAS
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
104100030721     C                   EVAL      PiStr=SkSplitCSV(i)
104200030721     C                   EXSR      CHKNUM
104300030721     C                   IF        PiNum=*on
104400030721     C                   Z-ADD     PiVal         VABIAS
104500030721     C                   ELSE
104600030721     C                   SETON                                        32
104700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104800030721     C                             + ' ' + 'VABIAS'
104900030721     C                   ENDIF
105000030721     C                   ENDIF
105100030721     C*** VLB
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
105300030721     C                   EVAL      PiStr=SkSplitCSV(i)
105400030721     C                   EXSR      CHKNUM
105500030721     C                   IF        PiNum=*on
105600030721     C                   Z-ADD     PiVal         VABVLB
105700030721     C                   ELSE
105800030721     C                   SETON                                        32
105900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106000030721     C                             + ' ' + 'VABVLB'
106100030721     C                   ENDIF
106200030721     C                   ENDIF
106300030721     C*** QFT
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
106500030721     C                   EVAL      PiStr=SkSplitCSV(i)
106600030721     C                   EXSR      CHKNUM
106700030721     C                   IF        PiNum=*on
106800030721     C                   Z-ADD     PiVal         VABQFT
106900030721     C                   ELSE
107000030721     C                   SETON                                        32
107100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107200030721     C                             + ' ' + 'VABQFT'
107300030721     C                   ENDIF
107400030721     C                   ENDIF
107500030721     C*** NCD
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
107700030721     C                   EVAL      PiStr=SkSplitCSV(i)
107800030721     C                   EXSR      CHKNUM
107900030721     C                   IF        PiInt=*on      AND
108000030721     C                             PiVal<=9999999
108100030721     C                   Z-ADD     PiVal         VABNCD
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108500030721     C                             + ' ' + 'VABNCD'
108600030721     C                   ENDIF
108700030721     C                   ENDIF
108800030721     C*** NCA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
109000030721     C                   EVAL      PiStr=SkSplitCSV(i)
109100030721     C                   EXSR      CHKNUM
109200030721     C                   IF        PiInt=*on      AND
109300030721     C                             PiVal<=9999999
109400030721     C                   Z-ADD     PiVal         VABNCA
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109800030721     C                             + ' ' + 'VABNCA'
109900030721     C                   ENDIF
110000030721     C                   ENDIF
110100030721     C*** ZNC
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
110300030721     C                   EVAL      PiStr=SkSplitCSV(i)
110400030721     C                   EXSR      CHKNUM
110500030721     C                   IF        PiInt=*on AND
110600030721     C                             PiVal<=99
110700030721     C                   Z-ADD     PiVal         VABZNC
110800030721     C                   ELSE
110900030721     C                   SETON                                        32
111000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111100030721     C                             + ' ' + 'VABZNC'
111200030721     C                   ENDIF
111300030721     C                   ENDIF
111400030721     C*** DCR
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiInt=*on       AND
111900030721     C                             PiVal<=99999999
112000030721     C                   Z-ADD     PiVal         VABDCR
112100030721     C                   ELSE
112200030721     C                   SETON                                        32
112300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112400030721     C                             + ' ' + 'VABDCR'
112500030721     C                   ENDIF
112600030721     C                   ENDIF
112700030721     C*** HCR
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
112900030721     C                   EVAL      PiStr=SkSplitCSV(i)
113000030721     C                   EXSR      CHKNUM
113100030721     C                   IF        PiInt=*on   AND
113200030721     C                             PiVal<=9999
113300030721     C                   Z-ADD     PiVal         VABHCR
113400030721     C                   ELSE
113500030721     C                   SETON                                        32
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700030721     C                             + ' ' + 'VABHCR'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** VMD
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiNum=*on
114500030721     C                   Z-ADD     PiVal         VABVMD
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABVMD'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** ANT
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
115400030721     C                   EVAL      PiStr=SkSplitCSV(i)
115500030721     C                   EXSR      CHKNUM
115600030721     C                   IF        PiInt=*on        AND
115700030721     C                             PiVal<=999999999
115800030721     C                   Z-ADD     PiVal         VABANT
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116200030721     C                             + ' ' + 'VABANT'
116300030721     C                   ENDIF
116400030721     C                   ENDIF
116500030721     C*** CMO
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiInt=*on
117000030721     C                   Z-ADD     PiVal         Num5_0
117100030721     C                   MOVEL(p)  Num5_0        VABCMO
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABCMO = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABCMO'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900040223     C*
118000040223     C* Eseguo considerazioni sul campo "Nota" contenente informazioni sulla consegna richiesta
118100040223 xxx C*                  IF        %trim(SkSplitFLD(i)) = 'CONSRICH'
118200040223     C*    '/'           SCAN      SkSplitCSV(i) wPosDCR           3 0
118300040223     C*                  IF        %found
118400040223     C*                  MOVEL     *all'0'       wMmGgDCR          4
118500040223     C*                  EVAL      wMmGgDCR = %subst(SkSplitCSV(i):wPosDCR+1:2)+
118600040223     C*                                       %subst(SkSplitCSV(i):wPosDCR-2:2)
118700040223     C*                  MOVEL     VABAAS        VABDCR
118800040223     C*                  MOVE      wMmGgDCR      VABDCR
118900040223     C*                  ENDIF
119000040223     C*                  ENDIF
119100040223     C*
119200040223     C* "Tipo Pagamento"
119300040223     C                   IF        %trim(SkSplitFLD(i)) = 'TIPOPAG'
119400040223     C                   MOVEL     *blanks       wTipoPag        256
119500040223     C                   EVAL      wTipoPag = SkSplitCSV(i)
119600040223     C                   ENDIF
119700030715     C*
119800030715     C                   ENDIF
119900030715     C                   EVAL      i = i + 1
120000030715     C                   ENDDO
120100040223     C*
120200040223     C* Eseguo considerazioni sul campo "Tipo Pagamento" in funzione del contrassegno
120300040223     C                   IF        wTipoPag = 'Contrassegno'
120400040223     C                   EVAL      VABTIC = 'BM'
120500040223     C                   ELSE
120600040223     C                   IF        wTipoPag = 'Contrassegno *'
120700040223     C                   EVAL      VABTIC = 'TM'
120800040223     C                   ELSE
120900040223     C                   EVAL      VABCAS = *zeros
121000040223     C                   ENDIF
121100040223     C                   ENDIF
121200040113     C*
121300040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
121400040223     C                   IF        VABCBO = *blanks
121500040223     C                   EVAL      VABCBO = '1'
121600040223     C                   ENDIF
121700040113     C                   IF        VABCAS > *zeros
121800040223     C                   EVAL      VABVCA = 'EUR'
121900040223     C                   IF        VABCBO = '1'
122000040113     C                   EVAL      VABCBO = '4'
122100040223     C                   ENDIF
122200040223     C                   IF        VABCBO = '2'
122300040223     C                   EVAL      VABCBO = '6'
122400040223     C                   ENDIF
122500040113     C                   ENDIF
122600020204     C*
122700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
122800020204     C                   EXSR      CHKIMPDIV
122900030715     C                   ENDIF
123000020204     C*
123100010607     C                   ENDSR
123200010607     C*----------------------------------------------------*
123300010601
123400020204
123500020204     C*----------------------------------------------------*
123600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
123700020204     C*----------------------------------------------------*
123800020204     C     CHKIMPDIV     BEGSR
123900020204     C*
124000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
124100020204     C                   Z-ADD     *zeros        wrkDec            9 9
124200020204     C*
124300020204     C* Come prima cosa effettuo considerazioni sulla divisa
124400020204     C                   IF        vabIAS > *zeros
124500020204     C                   IF        vabVAS <> 'EUR'
124600020204     C                   EVAL      vabVAS =  'ITL'
124700020204     C                   ENDIF
124800020204     C                   ENDIF
124900020204     C*
125000020204     C                   IF        vabCAS > *zeros
125100020204     C                   IF        vabVCA <> 'EUR'
125200020204     C                   EVAL      vabVCA =  'ITL'
125300020204     C                   ENDIF
125400020204     C                   ENDIF
125500020204     C*
125600020204     C                   IF        vabVMD > *zeros
125700020204     C                   IF        vabVAD <> 'EUR'
125800020204     C                   EVAL      vabVAD =  'ITL'
125900020204     C                   ENDIF
126000020204     C                   ENDIF
126100020204     C*
126200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
126300020204     C                   Z-ADD     vabIAS        wrkDec
126400020204     C                   IF        wrkDec > *zeros
126500020204     C                   IF        vabVAS = 'ITL'
126600020204     C                   EVAL      vabIAS = *zeros
126700020204     C                   ENDIF
126800020204     C                   ENDIF
126900020204     C*
127000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
127100020204     C                   Z-ADD     vabCAS        wrkDec
127200020204     C                   IF        wrkDec > *zeros
127300020204     C                   IF        vabVCA = 'ITL'
127400020204     C                   EVAL      vabCAS = *zeros
127500020204     C                   ENDIF
127600020204     C                   ENDIF
127700020204     C*
127800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
127900020204     C                   Z-ADD     vabVMD        wrkDec
128000020204     C                   IF        wrkDec > *zeros
128100020204     C                   IF        vabVAD = 'ITL'
128200020204     C                   EVAL      vabVMD = *zeros
128300020204     C                   ENDIF
128400020204     C                   ENDIF
128500020204     C*
128600020204     C                   ENDSR
128700020204     C***
128800020204
128900010330
129000010330
129100010330     C*----------------------------------------------------*
129200010330     C*  CONTROLLO NUMERICITA' CAMPI
129300010330     C*----------------------------------------------------*
129400010330     C     CHKNUM        BEGSR
129500010330     C*
129600010606     C                   IF        PiDecChr = *blanks
129700030715     C                   EVAL      PiDecChr = CharNUM
129800010606     C                   ENDIF
129900010606     C*
130000010606     C                   CALL(e)   'ISNUMERIC'
130100010330     C                   PARM                    PiStr            30
130200010606     C                   PARM                    PiDecChr          1
130300010330     C                   PARM      *ZEROS        PiVal            30 9
130400010330     C                   PARM      '0'           PiInt             1
130500010330     C                   PARM      '0'           PiNum             1
130600010330     C                   IF        %error
130700010606     C                   EVAL      PiNum=*off
130800010330     C                   ENDIF
130900010330     C*
131000010330     C                   ENDSR
131100010330     C***
131200010330
131300010601
131400010601
131500010601
131600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
131700010601     C     repfil        BEGSR
131800010601     C*
131900010601     C                   if        invfil = *zeros and
132000010601     C                             depfil > *zeros and
132100010629     C                             (vinflg = *blanks or
132200010629     C                              vinflg = *zeros)
132300010601     C
132400010601     C                   eval      invfil = depfil
132500010601     C                   endif
132600010601     C*
132700010601     C                   if        depfil <> invfil and
132800010601     C                             invfil > *zeros
132900010601     C                   eval      flgMulti = '1'
133000010601     C                   if        vinflg = *blanks
133100010601     C                   add       1             cntNonEl
133200010601     C                   endif
133300010601     C                   endif
133400010601     C*
133500010601     C                   if        vinflg = '2'
133600010601     C                   eval      flgStato = '2'
133700010601     C                   endif
133800010601     C*
133900010601     C                   ENDSR
134000010601     C***
134100010601
134200010601
134300010601
134400010330
134500010330
134600010330
134700990920      /TITLE Invio dei dati al punto operativo.
134800000613     C     invio         BEGSR
134900990920     C*
135000990920     C                   reset                   dscmz
135100010601     C                   move      invfil        cmzdst
135200990920     C                   eval      cmzfld = vlrfou
135300990920     C                   eval      cmzmbd = vlrhdl
135400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
135500000710     C                   if        prmfir = *blanks
135600021025     C                   eval      cmzfla = 'FIVAB00F'
135700021025     C                   eval      cmzmba = 'FIVAB00F'
135800000710     C                   else
135900000710     C                   eval      cmzfla = prmfir
136000000710     C                   eval      cmzmba = prmfir
136100000710     C                   endif
136200990920     C                   eval      cmznrr = *zeros
136300990920     C                   move      §ctrok        cmznrr
136400021018     C                   eval      cmzlba = vlrfl1
136500990920     C                   call(e)   'TIS711C'
136600990920     C                   parm                    dscmz
136700990921     C                   parm      *blanks       esito
136800990920     C                   if        %error
136900990920     C                             or cmzerr = '1'
137000990921     C                             or esito  = '1'
137100000710     C                   eval      wrkesito = '3'
137200990920     C                   endif
137300990920     C*
137400000613     C                   ENDSR
137500990910
137600010601
137700010601
137800010601
137900010601
138000010601      /TITLE Invio dei dati al punto operativo.
138100010601     C     opeini        BEGSR
138200010601     C*
138300010601     C* Inizializzo flag e contatori operativi
138400010601     C                   movel     '0'           flgGiro           1
138500010601     C                   movel     '0'           flgMulti          1
138600010601     C                   movel     '1'           flgStato          1
138700010615     C                   movel     '0'           flgOk             1
138800010601     C                   z-add     *zeros        cntNonEl         10 0
138900010601     C                   z-add     *zeros        depfil            3 0
139000010601     C                   z-add     *zeros        invfil            3 0
139100010601     C*
139200010601     C                   ENDSR
139300010601     C***
139400010601
139500010601
139600050421
139700050421     C     *pssr         BEGSR
139800050421     C*
139900050421     C                   eval      wrkesito = '2'
140000050421     C*
140100050421     C                   ENDSR     '*CANCL'
140200050421     C***
140300050421
140400010601
140500010330
140600010330
140700000613     C     *inzsr        BEGSR
140800990910     C*
140900990910     C     *entry        plist
141000990920     C                   parm                    tivlrds
141100990921     C                   parm      wrkesito      esito
141200000724     C                   parm                    prmlit
141300000710     C                   parm                    prmfir
141400010330     C*
141500010330     C* CALCOLA LA DATA CORRENTE
141600010330     C                   time                    wn14             14 0
141700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
141800010330     C                   z-add     wn8           g08dat
141900010330     C                   z-add     *zeros        g08inv
142000010330     C                   movel     '0'           g08err
142100010330     C                   call      'XSRDA8'
142200010330     C                   parm                    wlbda8
142300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
142400000613     C*
142500000613     C                   ENDSR
142600000613     C***
142700990908
142800040223     Otitv97p   E            riepilogo         2
142900990915     O                                              'Upload via Internet'
143000990915     O                                           +1 'Traduzione TIVIN00R -'
143100021025     O                                           +1 'FIVABWWR'
143200990915     O                                           +1 'Testate bolle'
143300990915     O          E            riepilogo   2
143400990915     O                       wrkdata
143500990915     O                       wrkora              +1
143600990915     O                       procname            +1
143700990915     O          E            riepilogo   2
143800990915     O                                              'Cliente..................:'
143900010605     O                       VABCCM        z     +1
144000990915     O          E            riepilogo   2
144100990920     O                                              'Riferimento Strategi.....:'
144200990920     O                       vlrhdl              +1
144300990915     O          E            riepilogo   2
144400990915     O                                              'Giusti...................:'
144500971022     O                       §CTROK        2   +  1
144600990915     O          E            riepilogo   2
144700990915     O                                              'Sbagliati e corretti.....:'
144800971022     O                       §CTRMO        2   +  1
144900990915     O          E            riepilogo   2
145000990915     O                                              'Sbagliati e scartati.....:'
145100971022     O                       §CTRNO        2   +  1
145200000613
145300040223     Otitv97ps  E            testdett          2
145400000613     O                                              'Upload via Internet'
145500000613     O                                           +1 'Traduzione TIVIN00R -'
145600021025     O                                           +1 'FIVABWWR'
145700000613     O                                           +1 'Testate bolle'
145800000616     O          E            testdett    3
145900000613     O                                           +2 'N° rec'
146000000613     O                                           +3 'Anteprima contenuto'
146100000616     O          E            rigadett    2
146200000613     O                       rrnum               +2
146300000621     O                       recko               +3
146400000616     O          E            findett     2
146500000613     O                       wrkdata
146600000613     O                       wrkora              +1
146700000613     O                       procname            +1
146800000616     O          E            findett     2
146900000613     O                                              'Cliente..................:'
147000010605     O                       VABCCM        z     +1
147100000616     O          E            findett     2
147200000613     O                                              'Riferimento Strategi.....:'
147300000613     O                       vlrhdl              +1
147400000616     O          E            findett     2
147500000613     O                                              'Giusti...................:'
147600000613     O                       §CTROK        2   +  1
147700000616     O          E            findett     2
147800000613     O                                              'Sbagliati e corretti.....:'
147900000613     O                       §CTRMO        2   +  1
148000000616     O          E            findett     2
148100000613     O                                              'Sbagliati e scartati.....:'
148200000613     O                       §CTRNO        2   +  1
148300000613** CMD - COMANDI CL
148400040223OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV97P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
148500040223OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV97PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
148600040223DLTOVR FILE(TITV97P TITV97PS) LVL(*)
148700010606
148800000613
148900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
149000040223Data;VABRMN;VABCBO;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;Telefono;VABNCL;
149100040223VABPKB;VABNT2;TIPOPAG;VABNOT;VABCAS
149200040113
