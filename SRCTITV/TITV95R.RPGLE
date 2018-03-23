000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040216     Ftitv95p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040216     Ftitv95ps  O    f  198        PRINTER usropn
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
012200040216     C                   if        not %open(titv95ps)
012300040216     C                   open      titv95ps
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
014500040216     C                   if        %open(titv95ps)
014600000616     C                   except    findett
014700040216     C                   close     titv95ps
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
016800040216     C                   if        not %open(titv95p)
016900040216     C   50              open      titv95p
017000990915     C                   endif
017100990915     C*
017200030715     C   50              except    riepilogo
017300990915     C*
017400040216     C                   if        %open(titv95p)
017500040216     C   50              close     titv95p
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
021000040216     C                   if        vindta > *blanks AND
021100040216     C                             %trim(vindta) <> *all';'
021200010330     C                   add       1             rrnum
021300010330     C*
021400010601     C                   if        *in70 = *off and
021500010330     C                             (vinflg = *blanks
021600010330     C                              or vinflg = '0'
021700010330     C                              or vinflg = '2')
021800010330     C*
021900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022000010711     C                   if        vinflg = *blanks or vinflg = '0'
022100010711     C                   clear                   vinmsg
022200010711     C                   endif
022300010601     C*
022400010330     C                   exsr      inzvar
022500010605     C                   exsr      impvab
022600010601     C*
022700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022800010605     C                   eval      depfil = VABLNP
022900010601     C                   exsr      repfil
023000010601     C                   if        depfil = invfil
023100021025     C                   if        vlrpoi = 999
023200030822     C                   move(P)   invfil        VABFGS
023300021025     C                   else
023400030822     C                   move(P)   vlrpoi        VABFGS
023500021025     C                   endif
023600010601     C*
023700010601     C                   exsr      PREELA
023800010601     C*
023900010604     C* Ebbene...
024000010604     C*
024100030822     C  N31              add       1             §CTROK            7 0
024200030822     C   32              add       1             §CTRMO            7 0
024300030822     C   31              add       1             §CTRNO            7 0
024400030822     C                   if        wGiro = 2
024500030822     C  N31              write     FIVAB000
024600030822     C                   endif
024700010604     C*
024800010604     C                   if        *in31 = *off and
024900010604     C                             *in32 = *off
025000010604     C                   eval      vinflg = '1'
025100010604     C                   else
025200010604     C                   eval      recko = vindta
025300010604     C                   except    rigadett
025400010604     C                   eval      vinflg = '2'
025500010604     C                   endif
025600010604     C                   endif
025700010601     C*
025800010604     C                   endif
025900010604     C*
026000010330     C                   else
026100010330     C                   eval      vinflg = '1'
026200010330     C                   endif
026300010601     C*
026400010601     C  N70              update    tivin000
026500010330     C*
026600030822     C  N70              enddo
026700010601     C
026800010601     C*
026900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027000010601     C                   if        cntNonEl = *zeros or
027100010601     C                             flgMulti = '0'
027200010330     C* Se non ci sono record con errori ...
027300010601     C                   if        §ctrno = 0 and
027400010604     C                             §ctrmo = 0 and
027500010601     C                             flgStato <> '2'
027600010330     C* ... restituisco esito OK.
027700010330     C                   eval      wrkesito = '0'
027800010330     C                   else
027900010330     C                   if        §ctrok > 0
028000010330     C                   eval      wrkesito = '1'
028100010330     C                   else
028200010615     C                   if        flgOk = '0'
028300010615     C                   eval      wrkesito = '2'
028400010615     C                   else
028500010615     C                   eval      wrkesito = '6'
028600010615     C                   endif
028700010330     C                   endif
028800010330     C                   endif
028900010601     C                   else
029000010601     C                   eval      wrkesito = '9'
029100010601     C                   endif
029200010330     C*
029300010330     C                   if        %open(tivin00r)
029400010330     C                   close     tivin00r
029500010330     C                   endif
029600021025     C                   if        %open(fivabwwr)
029700021025     C                   close     fivabwwr
029800010330     C                   endif
029900010601     C*
030000010601     C                   if        vlrpoi <> 999
030100010601     C                   eval      invfil = vlrpoi
030200010601     C                   endif
030300010330     C*
030400010330     C                   if        §ctrok > 0
030500010601     C                             and invfil > *zeros
030600010330     C                   exsr      invio
030700010330     C                   endif
030800010601     C*
030900010618     C                   if        flgGiro = '1'
031000010601     C                   exsr      endela
031100010618     C                   endif
031200010330     C*
031300010330     C                   ENDSR
031400010330     C***
031500010601
031600010601
031700010601
031800010330     C*----------------------------------------------------*
031900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032000010330     C*----------------------------------------------------*
032100010330     C     INZVAR        BEGSR
032200010330     C*
032300030822     C* Inizializzo variabili di wrk
032400020204     C                   Z-ADD     *zeros        Num5_0            5 0
032500030822     C*
032600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
032700030822     C                   CLEAR                   FIVAB000
032800030822     C                   CLEAR                   SkSplitCSV
032900030822     C                   EVAL      i = 1
033000030822     C                   EVAL      posDa = *zeros
033100030822     C                   EVAL      posA  = *zeros
033200030822     C*
033300030822     C* Reimposto i valori di default
033400030822     C                   EXSR      DEFCAM
033500030822     C*
033600010330     C                   ENDSR
033700010330     C*----------------------------------------------------*
033800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
033900010330     C*----------------------------------------------------*
034000010330     C     DEFCAM        BEGSR
034100030822     C*
034200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
034300030715     C* e delimitatore testo.
034400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
034500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
034600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
034700030822     C*
034800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
034900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
035000030715     C                   SETON                                        50
035100030715     C                   ELSE
035200030715     C                   SETOFF                                       50
035300030715     C                   ENDIF
035400030822     C*
035500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
035600030822     C                   EVAL      posDaDft = 1
035700030822     C                   EVAL      posADft  = 0
035800030822     C                   EVAL      wGiroDft = 0
035900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
036000030822     C                             posDaDft > 0
036100030822     C*
036200030822     C* Gestisco il 1° giro
036300030822     C                   IF        wGiroDft = 0
036400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
036500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
036600030822     C* Incremento il contatore dei "giri"
036700030822     C                   EVAL      wGiroDft = 1
036800030822     C                   ELSE
036900030822     C                   EVAL      posDaDft = posADft
037000030822     C                   ENDIF
037100030822     C* Eseguo lo scan x trovare la fine del campo corrente
037200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
037300030822     C*
037400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
037500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
037600030822     C                   IF        posDaDft > 0 AND
037700030822     C                             posADft  > 0
037800030822     C* NCL
037900030822     C                   IF        %subst(
038000030822     C                             %subst(vlrppt:posDaDft+1:
038100030822     C                             posADft-posDaDft-1):1:3)
038200030822     C                             = 'NCL'
038300030822     C                   EVAL      PiStr=%trim(%subst(
038400030822     C                             %subst(vlrppt:posDaDft+1:
038500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038600030822     C                   EXSR      CHKNUM
038700030822     C                   IF        PiInt=*on
038800030822     C                   Z-ADD     PiVal         VABNCL
038900030822     C                   ENDIF
039000030822     C                   ENDIF
039100030822     C* CCM
039200030822     C                   IF        %subst(
039300030822     C                             %subst(vlrppt:posDaDft+1:
039400030822     C                             posADft-posDaDft-1):1:3)
039500030822     C                             = 'CCM'
039600030822     C                   EVAL      PiStr=%trim(%subst(
039700030822     C                             %subst(vlrppt:posDaDft+1:
039800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039900030822     C                   EXSR      CHKNUM
040000030822     C                   IF        PiInt=*on
040100030822     C                   Z-ADD     PiVal         VABCCM
040200030822     C                   ENDIF
040300030822     C                   ENDIF
040400030822     C* LNP
040500030822     C                   IF        %subst(
040600030822     C                             %subst(vlrppt:posDaDft+1:
040700030822     C                             posADft-posDaDft-1):1:3)
040800030822     C                             = 'LNP'
040900030822     C                   EVAL      PiStr=%trim(%subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041200030822     C                   EXSR      CHKNUM
041300030822     C                   IF        PiInt=*on
041400030822     C                   Z-ADD     PiVal         VABLNP
041500030822     C                   ENDIF
041600030822     C                   ENDIF
041700030822     C* NRS
041800030822     C                   IF        %subst(
041900030822     C                             %subst(vlrppt:posDaDft+1:
042000030822     C                             posADft-posDaDft-1):1:3)
042100030822     C                             = 'NRS'
042200030822     C                   EVAL      PiStr=%trim(%subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042500030822     C                   EXSR      CHKNUM
042600030822     C                   IF        PiInt=*on
042700030822     C                   Z-ADD     PiVal         VABNRS
042800030822     C                   ENDIF
042900030822     C                   ENDIF
043000030822     C* CTR
043100030822     C                   IF        %subst(
043200030822     C                             %subst(vlrppt:posDaDft+1:
043300030822     C                             posADft-posDaDft-1):1:3)
043400030822     C                             = 'CTR'
043500030822     C                   EVAL      PiStr=%trim(%subst(
043600030822     C                             %subst(vlrppt:posDaDft+1:
043700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043800030822     C                   EXSR      CHKNUM
043900030822     C                   IF        PiInt=*on
044000030822     C                   Z-ADD     PiVal         VABCTR
044100030822     C                   ENDIF
044200030822     C                   ENDIF
044300030822     C* PKB
044400030822     C                   IF        %subst(
044500030822     C                             %subst(vlrppt:posDaDft+1:
044600030822     C                             posADft-posDaDft-1):1:3)
044700030822     C                             = 'PKB'
044800030822     C                   EVAL      PiStr=%trim(%subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045100030822     C                   EXSR      CHKNUM
045200030822     C                   IF        PiNum=*on
045300030822     C                   Z-ADD     PiVal         VABPKB
045400030822     C                   ENDIF
045500030822     C                   ENDIF
045600030822     C* VLB
045700030822     C                   IF        %subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):1:3)
046000030822     C                             = 'VLB'
046100030822     C                   EVAL      PiStr=%trim(%subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400030822     C                   EXSR      CHKNUM
046500030822     C                   IF        PiNum=*on
046600030822     C                   Z-ADD     PiVal         VABVLB
046700030822     C                   ENDIF
046800030822     C                   ENDIF
046900030822     C* QFT
047000030822     C                   IF        %subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):1:3)
047300030822     C                             = 'QFT'
047400030822     C                   EVAL      PiStr=%trim(%subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047700030822     C                   EXSR      CHKNUM
047800030822     C                   IF        PiNum=*on
047900030822     C                   Z-ADD     PiVal         VABQFT
048000030822     C                   ENDIF
048100030822     C                   ENDIF
048200030822     C* CBO
048300030822     C                   IF        %subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):1:3)
048600030822     C                             = 'CBO'
048700030822     C                   EVAL      VABCBO=%trim(%subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000030822     C                   ENDIF
049100030822     C* TSP
049200030822     C                   IF        %subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):1:3)
049500030822     C                             = 'TSP'
049600030822     C                   EVAL      VABTSP=%trim(%subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900030822     C                   ENDIF
050000030822     C* VAS
050100030822     C                   IF        %subst(
050200030822     C                             %subst(vlrppt:posDaDft+1:
050300030822     C                             posADft-posDaDft-1):1:3)
050400030822     C                             = 'VAS'
050500030822     C                   EVAL      VABVAS=%trim(%subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050800030822     C                   ENDIF
050900030822     C* VCA
051000030822     C                   IF        %subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):1:3)
051300030822     C                             = 'VCA'
051400030822     C                   EVAL      VABVCA=%trim(%subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700030822     C                   ENDIF
051800030822     C* GCA
051900030822     C                   IF        %subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):1:3)
052200030822     C                             = 'GCA'
052300030822     C                   EVAL      VABGCA=%trim(%subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600030822     C                   ENDIF
052700030822     C* CTM
052800030822     C                   IF        %subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):1:3)
053100030822     C                             = 'CTM'
053200030822     C                   EVAL      VABCTM=%trim(%subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500030822     C                   ENDIF
053600030822     C* FFD
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'FFD'
054100030822     C                   EVAL      VABFFD=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   ENDIF
054500030822     C* VAD
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'VAD'
055000030822     C                   EVAL      VABVAD=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   ENDIF
055400030822     C* GMA
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'GMA'
055900030822     C                   EVAL      VABGMA=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   ENDIF
056300030822     C* GGA
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'GGA'
056800030822     C                   EVAL      VABGGA=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   ENDIF
057200030822     C* GVA
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'GVA'
057700030822     C                   EVAL      VABGVA=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   ENDIF
058100030822     C* TC1
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'TC1'
058600030822     C                   EVAL      VABTC1=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   ENDIF
059000030822     C* TC2
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'TC2'
059500030822     C                   EVAL      VABTC2=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   ENDIF
059900030822     C* ...
060000030822     C                   ENDIF
060100030822     C                   ENDDO
060200020204     C*
060300010330     C                   ENDSR
060400010607     C*----------------------------------------------------*
060500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
060600010607     C*----------------------------------------------------*
060700010607     C     IMPVAB        BEGSR
060800010607     C*
060900010607     C                   SETOFF                                       3132
061000030515     C*
061100010607     C                   MOVEL     datcor        VABAAS
061200010607     C                   MOVE      datcor        VABMGS
061300010607     C*
061400030715     C**********
061500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
061600030715     C**********
061700030715     C                   IF        wGiro = *zeros
061800040113     C*
061900040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
062000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
062100040113     C                                      %trim(intcol(3))
062200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
062300031201     C     minu:maiu     XLATE     vindta        vindta
062400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
062500030715     C                   DOW       posDa <= %len(%trim(vindta))
062600030715     C*
062700030715     C* Gestisco il 1° campo
062800030715     C                   IF        i = 1
062900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
063000030715     C                   EVAL      posDa = 1
063100030715     C* Eseguo lo scan x trovare la fine del primo campo
063200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
063300030715     C* A questo punto "estraggo" il campo corrente
063400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
063500030715     C                                                    (posA-posDa))
063600030715     C* X i campi successivi al 1°
063700030715     C                   ELSE
063800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
063900030715     C                   EVAL      posDa = posA + 1
064000030715     C* Eseguo lo scan x trovare la fine del campo corrente
064100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
064200030715     C* Gestisco l'ultimo campo
064300030715     C                   IF        posA = *zeros
064400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
064500030715     C* A questo punto "estraggo" il campo corrente
064600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
064700030715     C                                                    (posA-posDa))
064800030715     C                   LEAVE
064900030715     C                   ELSE
065000030715     C* A questo punto "estraggo" il campo corrente
065100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065200030715     C                                                    (posA-posDa))
065300030715     C                   ENDIF
065400030715     C                   ENDIF
065500030715     C* Incremento il contatore d campo
065600030715     C                   EVAL      i = i +1
065700030715     C                   ENDDO
065800030715     C*
065900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
066000030715     C* delimitazione testo
066100030715     C                   EVAL      i = 1
066200030715     C                   DOW       i < %elem(SkSplitFLD)
066300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
066400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
066500030715     C                   EVAL      i = i + 1
066600030715     C                   ENDDO
066700030715     C                   EVAL      wGiro = 1
066800030715     C                   ELSE
066900030715     C                   EVAL      wGiro = 2
067000031204     C**********
067100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
067200031204     C* d separatore campo
067300031204     C**********
067400031204     C                   EVAL      vindta = %trim(vindta)
067500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
067600031204     C                   EVAL      vindta = %subst(vindta:2)
067700031204     C                   ENDDO
067800031204     C*
067900031204     C                   Z-ADD     *zeros        lunghInput        4 0
068000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
068100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
068200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
068300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
068400031204     C                   ENDDO
068500030715     C**********
068600030715     C* Effettuo lo split del campo dati d input
068700030715     C**********
068800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068900030715     C                   DOW       posDa <= %len(%trim(vindta))
069000030715     C*
069100030715     C* Gestisco il 1° campo
069200030715     C                   IF        i = 1
069300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
069400030715     C                   EVAL      posDa = 1
069500030715     C* Eseguo lo scan x trovare la fine del primo campo
069600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069700030715     C* A questo punto "estraggo" il campo corrente
069800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
069900030715     C                                                    (posA-posDa))
070000030715     C* X i campi successivi al 1°
070100030715     C                   ELSE
070200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
070300030715     C                   EVAL      posDa = posA + 1
070400031204     C* Verifico che nn vi sia il campo nn valorizzato
070500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
070600031204     C* Se campo nn valorizzato skippo al prossimo
070700031204     C                   EVAL      SkSplitCSV(i) = *blanks
070800031204     C                   EVAL      posA  = posA + 1
070900031204     C                   ELSE
071000030715     C* Eseguo lo scan x trovare la fine del campo corrente
071100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071200030715     C* Gestisco l'ultimo campo
071300030715     C                   IF        posA = *zeros
071400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
071500030715     C* A questo punto "estraggo" il campo corrente
071600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
071700030715     C                                                    (posA-posDa))
071800030715     C                   LEAVE
071900030715     C                   ELSE
072000030715     C* A questo punto "estraggo" il campo corrente
072100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
072200030715     C                                                    (posA-posDa))
072300030715     C                   ENDIF
072400031204     C                   ENDIF
072500030715     C                   ENDIF
072600030715     C* Incremento il contatore d campo
072700030715     C                   EVAL      i = i +1
072800030715     C                   ENDDO
072900030715     C*
073000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
073100030715     C* delimitazione testo
073200030715     C                   EVAL      i = 1
073300030715     C                   DOW       i < %elem(SkSplitCSV)
073400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
073500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
073600030715     C                   EVAL      i = i + 1
073700030715     C                   ENDDO
073800030715     C*
073900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
074000030715     C                   EVAL      i = 1
074100030715     C                   DOW       i < %elem(SkSplitFLD)
074200030715     C*
074300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
074400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
074500030715     C                             SkSplitCSV(i) <> *blanks
074600030721     C*** ATB
074700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
074800030721     C                   EVAL      VABATB = SkSplitCSV(i)
074900030721     C                   ENDIF
075000030715     C*** RSD
075100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
075200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
075300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
075400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
075500030715     C* ==
075600040216     C                   IF        %len(%trimr(SkSplitCSV(i))) > 35
075700040216     C                   EVAL      VABRD2 = %subst(SkSplitCSV(i):36)
075800040216     C                   ENDIF
075900030715     C                   ENDIF
076000030721     C*** RD2
076100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
076200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
076300030721     C                   ENDIF
076400030715     C*** IND
076500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
076600030715     C                   EVAL      VABIND = SkSplitCSV(i)
076700030715     C                   ENDIF
076800030715     C*** LOD
076900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
077000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
077100030715     C                   ENDIF
077200030715     C*** PRD
077300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
077400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
077500030715     C                   ENDIF
077600030721     C*** NZD
077700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
077800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
077900030721     C                   ENDIF
078000030715     C*** RMA
078100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
078200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
078300030715     C                   ENDIF
078400030715     C*** NOT
078500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
078600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
078700030715     C                   ENDIF
078800030715     C*** NT2
078900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
079000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
079100030715     C                   ENDIF
079200030715     C*** VCA
079300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
079400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
079500030715     C                   ENDIF
079600030715     C*** VAS
079700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
079800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
079900030715     C                   ENDIF
080000030715     C*** FFD
080100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
080200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
080300030715     C                   ENDIF
080400030715     C*** CBO
080500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
080600030715     C                   EVAL      VABCBO = SkSplitCSV(i)
080700030715     C                   ENDIF
080800030715     C
080900030721     C*** GC1
081000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
081100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
081200030721     C                   ENDIF
081300030721     C*** GC2
081400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
081500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
081600030721     C                   ENDIF
081700030721     C*** TSP
081800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
081900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
082000030721     C                   ENDIF
082100030721     C*** NAS
082200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
082300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
082400030721     C                   ENDIF
082500030721     C*** TIC
082600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
082700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
082800030721     C                   ENDIF
082900030721     C*** GCA
083000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
083100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
083200030721     C                   ENDIF
083300030721     C*** XCO
083400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
083500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
083600030721     C                   ENDIF
083700030721     C*** CTM
083800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
083900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
084000030721     C                   ENDIF
084100030721     C*** TCR
084200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
084300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
084400030721     C                   ENDIF
084500030721     C*** CTS
084600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
084700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
084800030721     C                   ENDIF
084900030721     C*** FTM
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
085100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** VAD
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
085500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*** GMA
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
085900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** GGA
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
086300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** GVA
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
086700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** TC1
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
087100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** TC2
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
087500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** SCL
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
087900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** RMO
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
088300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** NMO
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
088700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*
089000030721     C* Reperisco quindi i campi numerici...
089100030721     C*** CAD
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
089300030721     C                   EVAL      PiStr=SkSplitCSV(i)
089400030721     C                   EXSR      CHKNUM
089500030721     C                   IF        PiInt=*on
089600030721     C                   Z-ADD     PiVal         Num5_0
089700030721     C                   MOVEL(p)  Num5_0        VABCAD
089800030721     C                   ELSE
089900030721     C                   SETON                                        32
090000030721     C                   EVAL      VABCAD = *zeros
090100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
090200030721     C                             + ' ' + 'VABCAD'
090300030721     C                   ENDIF
090400030721     C                   ENDIF
090500030721     C*** RMN
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
090700030721     C                   EVAL      PiStr=SkSplitCSV(i)
090800030721     C                   EXSR      CHKNUM
090900030721     C                   IF        PiInt=*on
091000030721     C                   Z-ADD     PiVal         VABRMN
091100040113     C                   EVAL      VABRMA=SkSplitCSV(i)
091200030721     C                   ELSE
091300030721     C                   SETON                                        32
091400030721     C                   EVAL      VABRMN = 1
091500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
091600040113     C                             + ' ' + 'VABRMN VABRMA'
091700030721     C                   ENDIF
091800030721     C                   ENDIF
091900030721     C*** NSP
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
092100030721     C                   EVAL      PiStr=SkSplitCSV(i)
092200030721     C                   EXSR      CHKNUM
092300030721     C                   IF        PiInt=*on AND
092400030721     C                             PiVal<=9999999
092500030721     C                   Z-ADD     PiVal         VABNSP
092600030721     C                   ELSE
092700030721     C                   SETON                                        32
092800030721     C                   EVAL      VABNSP = *zeros
092900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093000030721     C                             + ' ' + 'VABNSP'
093100030721     C                   ENDIF
093200030721     C                   ENDIF
093300030721     C*** NCL
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
093500030721     C                   EVAL      PiStr=SkSplitCSV(i)
093600030721     C                   EXSR      CHKNUM
093700030721     C                   IF        PiInt=*on
093800030721     C                   Z-ADD     PiVal         VABNCL
093900030721     C                   ELSE
094000030721     C                   SETON                                        32
094100030721     C                   EVAL      VABNCL = *zeros
094200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094300030721     C                             + ' ' + 'VABNCL'
094400030721     C                   ENDIF
094500030721     C                   ENDIF
094600030721     C*** PKB
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
094800030721     C                   EVAL      PiStr=SkSplitCSV(i)
094900030721     C                   EXSR      CHKNUM
095000030721     C                   IF        PiNum=*on
095100030721     C                   Z-ADD     PiVal         VABPKB
095200030721     C                   ELSE
095300030721     C                   SETON                                        32
095400030721     C                   EVAL      VABPKB = *zeros
095500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095600030721     C                             + ' ' + 'VABPKB'
095700030721     C                   ENDIF
095800030721     C                   ENDIF
095900030721     C*** CAS
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
096100040216     C                   IF        %subst(SkSplitCSV(i):1:4) = 'EURO'
096200040216     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5)
096300040216     C                   ELSE
096400030721     C                   EVAL      PiStr=SkSplitCSV(i)
096500040216     C                   ENDIF
096600030721     C                   EXSR      CHKNUM
096700030721     C                   IF        PiNum=*on
096800030721     C                   Z-ADD     PiVal         VABCAS
096900030721     C                   ELSE
097000030721     C                   SETON                                        32
097100030721     C                   EVAL      VABCAS = *zeros
097200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097300030721     C                             + ' ' + 'VABCAS'
097400030721     C                   ENDIF
097500030721     C                   ENDIF
097600030721     C*** CCM
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
097800030721     C                   EVAL      PiStr=SkSplitCSV(i)
097900030721     C                   EXSR      CHKNUM
098000030721     C                   IF        PiInt=*on      AND
098100030721     C                             PiVal<=9999999 AND
098200030721     C                             PiVal>*zeros
098300030721     C                   Z-ADD     PiVal         VABCCM
098400030721     C                   ELSE
098500030721     C                   SETON                                        32
098600030721     C                   EVAL      VABCCM = *zeros
098700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098800030721     C                             + ' ' + 'VABCCM'
098900030721     C                   ENDIF
099000030721     C                   ENDIF
099100030721     C*** LNP
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
099300030721     C                   EVAL      PiStr=SkSplitCSV(i)
099400030721     C                   EXSR      CHKNUM
099500030721     C                   IF        PiInt=*on    AND
099600030721     C                             PiVal<=999   AND
099700030721     C                             PiVal>*zeros
099800030721     C                   Z-ADD     PiVal         VABLNP
099900030721     C                   ELSE
100000030721     C                   SETON                                        32
100100030721     C                   EVAL      VABLNP = *zeros
100200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100300030721     C                             + ' ' + 'VABLNP'
100400030721     C                   ENDIF
100500030721     C                   ENDIF
100600030721     C*** NRS
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
100800030721     C                   EVAL      PiStr=SkSplitCSV(i)
100900030721     C                   EXSR      CHKNUM
101000030721     C                   IF        PiInt=*on AND
101100030721     C                             PiVal<=99
101200030721     C                   Z-ADD     PiVal         VABNRS
101300030721     C                   ELSE
101400030721     C                   SETON                                        32
101500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101600030721     C                             + ' ' + 'VABNRS'
101700030721     C                   ENDIF
101800030721     C                   ENDIF
101900030721     C*** LNA
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
102100030721     C                   EVAL      PiStr=SkSplitCSV(i)
102200030721     C                   EXSR      CHKNUM
102300030721     C                   IF        PiInt=*on    AND
102400030721     C                             PiVal<=999   AND
102500030721     C                             PiVal>*zeros
102600030721     C                   Z-ADD     PiVal         VABLNA
102700030721     C                   ELSE
102800030721     C                   SETON                                        32
102900030721     C                   EVAL      VABLNA = *zeros
103000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103100030721     C                             + ' ' + 'VABLNA'
103200030721     C                   ENDIF
103300030721     C                   ENDIF
103400030721     C*** CTR
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
103600030721     C                   EVAL      PiStr=SkSplitCSV(i)
103700030721     C                   EXSR      CHKNUM
103800030721     C                   IF        PiInt=*on  AND
103900030721     C                             PiVal<=999
104000030721     C                   Z-ADD     PiVal         VABCTR
104100030721     C                   ELSE
104200030721     C                   SETON                                        32
104300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104400030721     C                             + ' ' + 'VABCTR'
104500030721     C                   ENDIF
104600030721     C                   ENDIF
104700030721     C*** IAS
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
104900030721     C                   EVAL      PiStr=SkSplitCSV(i)
105000030721     C                   EXSR      CHKNUM
105100030721     C                   IF        PiNum=*on
105200030721     C                   Z-ADD     PiVal         VABIAS
105300030721     C                   ELSE
105400030721     C                   SETON                                        32
105500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105600030721     C                             + ' ' + 'VABIAS'
105700030721     C                   ENDIF
105800030721     C                   ENDIF
105900030721     C*** VLB
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
106100030721     C                   EVAL      PiStr=SkSplitCSV(i)
106200030721     C                   EXSR      CHKNUM
106300030721     C                   IF        PiNum=*on
106400030721     C                   Z-ADD     PiVal         VABVLB
106500030721     C                   ELSE
106600030721     C                   SETON                                        32
106700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106800030721     C                             + ' ' + 'VABVLB'
106900030721     C                   ENDIF
107000030721     C                   ENDIF
107100030721     C*** QFT
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
107300030721     C                   EVAL      PiStr=SkSplitCSV(i)
107400030721     C                   EXSR      CHKNUM
107500030721     C                   IF        PiNum=*on
107600030721     C                   Z-ADD     PiVal         VABQFT
107700030721     C                   ELSE
107800030721     C                   SETON                                        32
107900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108000030721     C                             + ' ' + 'VABQFT'
108100030721     C                   ENDIF
108200030721     C                   ENDIF
108300030721     C*** NCD
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
108500030721     C                   EVAL      PiStr=SkSplitCSV(i)
108600030721     C                   EXSR      CHKNUM
108700030721     C                   IF        PiInt=*on      AND
108800030721     C                             PiVal<=9999999
108900030721     C                   Z-ADD     PiVal         VABNCD
109000030721     C                   ELSE
109100030721     C                   SETON                                        32
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABNCD'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** NCA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiInt=*on      AND
110100030721     C                             PiVal<=9999999
110200030721     C                   Z-ADD     PiVal         VABNCA
110300030721     C                   ELSE
110400030721     C                   SETON                                        32
110500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110600030721     C                             + ' ' + 'VABNCA'
110700030721     C                   ENDIF
110800030721     C                   ENDIF
110900030721     C*** ZNC
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
111100030721     C                   EVAL      PiStr=SkSplitCSV(i)
111200030721     C                   EXSR      CHKNUM
111300030721     C                   IF        PiInt=*on AND
111400030721     C                             PiVal<=99
111500030721     C                   Z-ADD     PiVal         VABZNC
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABZNC'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** DCR
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on       AND
112700030721     C                             PiVal<=99999999
112800030721     C                   Z-ADD     PiVal         VABDCR
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABDCR'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** HCR
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on   AND
114000030721     C                             PiVal<=9999
114100030721     C                   Z-ADD     PiVal         VABHCR
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABHCR'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** VMD
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiNum=*on
115300030721     C                   Z-ADD     PiVal         VABVMD
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABVMD'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** ANT
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on        AND
116500030721     C                             PiVal<=999999999
116600030721     C                   Z-ADD     PiVal         VABANT
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABANT'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** CMO
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on
117800030721     C                   Z-ADD     PiVal         Num5_0
117900030721     C                   MOVEL(p)  Num5_0        VABCMO
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      VABCMO = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABCMO'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030715     C*
118800030715     C                   ENDIF
118900030715     C                   EVAL      i = i + 1
119000030715     C                   ENDDO
119100040216     C*
119200040216     C* Eseguo operazioni personalizzate
119300040216     C                   MOVEL     VABRMN        VABRMA
119400040216     C                   MOVE      VABRMN        VABNSP
119500040113     C*
119600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
119700040113     C                   IF        VABCAS > *zeros
119800040113     C                   EVAL      VABCBO = '4'
119900040113     C                   EVAL      VABVCA = 'EUR'
120000040113     C                   ELSE
120100040113     C                   EVAL      VABCBO = '1'
120200040113     C                   ENDIF
120300020204     C*
120400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
120500020204     C                   EXSR      CHKIMPDIV
120600030715     C                   ENDIF
120700020204     C*
120800010607     C                   ENDSR
120900010607     C*----------------------------------------------------*
121000010601
121100020204
121200020204     C*----------------------------------------------------*
121300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
121400020204     C*----------------------------------------------------*
121500020204     C     CHKIMPDIV     BEGSR
121600020204     C*
121700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
121800020204     C                   Z-ADD     *zeros        wrkDec            9 9
121900020204     C*
122000020204     C* Come prima cosa effettuo considerazioni sulla divisa
122100020204     C                   IF        vabIAS > *zeros
122200020204     C                   IF        vabVAS <> 'EUR'
122300020204     C                   EVAL      vabVAS =  'ITL'
122400020204     C                   ENDIF
122500020204     C                   ENDIF
122600020204     C*
122700020204     C                   IF        vabCAS > *zeros
122800020204     C                   IF        vabVCA <> 'EUR'
122900020204     C                   EVAL      vabVCA =  'ITL'
123000020204     C                   ENDIF
123100020204     C                   ENDIF
123200020204     C*
123300020204     C                   IF        vabVMD > *zeros
123400020204     C                   IF        vabVAD <> 'EUR'
123500020204     C                   EVAL      vabVAD =  'ITL'
123600020204     C                   ENDIF
123700020204     C                   ENDIF
123800020204     C*
123900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
124000020204     C                   Z-ADD     vabIAS        wrkDec
124100020204     C                   IF        wrkDec > *zeros
124200020204     C                   IF        vabVAS = 'ITL'
124300020204     C                   EVAL      vabIAS = *zeros
124400020204     C                   ENDIF
124500020204     C                   ENDIF
124600020204     C*
124700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
124800020204     C                   Z-ADD     vabCAS        wrkDec
124900020204     C                   IF        wrkDec > *zeros
125000020204     C                   IF        vabVCA = 'ITL'
125100020204     C                   EVAL      vabCAS = *zeros
125200020204     C                   ENDIF
125300020204     C                   ENDIF
125400020204     C*
125500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
125600020204     C                   Z-ADD     vabVMD        wrkDec
125700020204     C                   IF        wrkDec > *zeros
125800020204     C                   IF        vabVAD = 'ITL'
125900020204     C                   EVAL      vabVMD = *zeros
126000020204     C                   ENDIF
126100020204     C                   ENDIF
126200020204     C*
126300020204     C                   ENDSR
126400020204     C***
126500020204
126600010330
126700010330
126800010330     C*----------------------------------------------------*
126900010330     C*  CONTROLLO NUMERICITA' CAMPI
127000010330     C*----------------------------------------------------*
127100010330     C     CHKNUM        BEGSR
127200010330     C*
127300010606     C                   IF        PiDecChr = *blanks
127400030715     C                   EVAL      PiDecChr = CharNUM
127500010606     C                   ENDIF
127600010606     C*
127700010606     C                   CALL(e)   'ISNUMERIC'
127800010330     C                   PARM                    PiStr            30
127900010606     C                   PARM                    PiDecChr          1
128000010330     C                   PARM      *ZEROS        PiVal            30 9
128100010330     C                   PARM      '0'           PiInt             1
128200010330     C                   PARM      '0'           PiNum             1
128300010330     C                   IF        %error
128400010606     C                   EVAL      PiNum=*off
128500010330     C                   ENDIF
128600010330     C*
128700010330     C                   ENDSR
128800010330     C***
128900010330
129000010601
129100010601
129200010601
129300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
129400010601     C     repfil        BEGSR
129500010601     C*
129600010601     C                   if        invfil = *zeros and
129700010601     C                             depfil > *zeros and
129800010629     C                             (vinflg = *blanks or
129900010629     C                              vinflg = *zeros)
130000010601     C
130100010601     C                   eval      invfil = depfil
130200010601     C                   endif
130300010601     C*
130400010601     C                   if        depfil <> invfil and
130500010601     C                             invfil > *zeros
130600010601     C                   eval      flgMulti = '1'
130700010601     C                   if        vinflg = *blanks
130800010601     C                   add       1             cntNonEl
130900010601     C                   endif
131000010601     C                   endif
131100010601     C*
131200010601     C                   if        vinflg = '2'
131300010601     C                   eval      flgStato = '2'
131400010601     C                   endif
131500010601     C*
131600010601     C                   ENDSR
131700010601     C***
131800010601
131900010601
132000010601
132100010330
132200010330
132300010330
132400990920      /TITLE Invio dei dati al punto operativo.
132500000613     C     invio         BEGSR
132600990920     C*
132700990920     C                   reset                   dscmz
132800010601     C                   move      invfil        cmzdst
132900990920     C                   eval      cmzfld = vlrfou
133000990920     C                   eval      cmzmbd = vlrhdl
133100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
133200000710     C                   if        prmfir = *blanks
133300021025     C                   eval      cmzfla = 'FIVAB00F'
133400021025     C                   eval      cmzmba = 'FIVAB00F'
133500000710     C                   else
133600000710     C                   eval      cmzfla = prmfir
133700000710     C                   eval      cmzmba = prmfir
133800000710     C                   endif
133900990920     C                   eval      cmznrr = *zeros
134000990920     C                   move      §ctrok        cmznrr
134100021018     C                   eval      cmzlba = vlrfl1
134200990920     C                   call(e)   'TIS711C'
134300990920     C                   parm                    dscmz
134400990921     C                   parm      *blanks       esito
134500990920     C                   if        %error
134600990920     C                             or cmzerr = '1'
134700990921     C                             or esito  = '1'
134800000710     C                   eval      wrkesito = '3'
134900990920     C                   endif
135000990920     C*
135100000613     C                   ENDSR
135200990910
135300010601
135400010601
135500010601
135600010601
135700010601      /TITLE Invio dei dati al punto operativo.
135800010601     C     opeini        BEGSR
135900010601     C*
136000010601     C* Inizializzo flag e contatori operativi
136100010601     C                   movel     '0'           flgGiro           1
136200010601     C                   movel     '0'           flgMulti          1
136300010601     C                   movel     '1'           flgStato          1
136400010615     C                   movel     '0'           flgOk             1
136500010601     C                   z-add     *zeros        cntNonEl         10 0
136600010601     C                   z-add     *zeros        depfil            3 0
136700010601     C                   z-add     *zeros        invfil            3 0
136800010601     C*
136900010601     C                   ENDSR
137000010601     C***
137100010601
137200010601
137300050421
137400050421     C     *pssr         BEGSR
137500050421     C*
137600050421     C                   eval      wrkesito = '2'
137700050421     C*
137800050421     C                   ENDSR     '*CANCL'
137900050421     C***
138000050421
138100010601
138200010330
138300010330
138400000613     C     *inzsr        BEGSR
138500990910     C*
138600990910     C     *entry        plist
138700990920     C                   parm                    tivlrds
138800990921     C                   parm      wrkesito      esito
138900000724     C                   parm                    prmlit
139000000710     C                   parm                    prmfir
139100010330     C*
139200010330     C* CALCOLA LA DATA CORRENTE
139300010330     C                   time                    wn14             14 0
139400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
139500010330     C                   z-add     wn8           g08dat
139600010330     C                   z-add     *zeros        g08inv
139700010330     C                   movel     '0'           g08err
139800010330     C                   call      'XSRDA8'
139900010330     C                   parm                    wlbda8
140000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
140100000613     C*
140200000613     C                   ENDSR
140300000613     C***
140400990908
140500040216     Otitv95p   E            riepilogo         2
140600990915     O                                              'Upload via Internet'
140700990915     O                                           +1 'Traduzione TIVIN00R -'
140800021025     O                                           +1 'FIVABWWR'
140900990915     O                                           +1 'Testate bolle'
141000990915     O          E            riepilogo   2
141100990915     O                       wrkdata
141200990915     O                       wrkora              +1
141300990915     O                       procname            +1
141400990915     O          E            riepilogo   2
141500990915     O                                              'Cliente..................:'
141600010605     O                       VABCCM        z     +1
141700990915     O          E            riepilogo   2
141800990920     O                                              'Riferimento Strategi.....:'
141900990920     O                       vlrhdl              +1
142000990915     O          E            riepilogo   2
142100990915     O                                              'Giusti...................:'
142200971022     O                       §CTROK        2   +  1
142300990915     O          E            riepilogo   2
142400990915     O                                              'Sbagliati e corretti.....:'
142500971022     O                       §CTRMO        2   +  1
142600990915     O          E            riepilogo   2
142700990915     O                                              'Sbagliati e scartati.....:'
142800971022     O                       §CTRNO        2   +  1
142900000613
143000040216     Otitv95ps  E            testdett          2
143100000613     O                                              'Upload via Internet'
143200000613     O                                           +1 'Traduzione TIVIN00R -'
143300021025     O                                           +1 'FIVABWWR'
143400000613     O                                           +1 'Testate bolle'
143500000616     O          E            testdett    3
143600000613     O                                           +2 'N° rec'
143700000613     O                                           +3 'Anteprima contenuto'
143800000616     O          E            rigadett    2
143900000613     O                       rrnum               +2
144000000621     O                       recko               +3
144100000616     O          E            findett     2
144200000613     O                       wrkdata
144300000613     O                       wrkora              +1
144400000613     O                       procname            +1
144500000616     O          E            findett     2
144600000613     O                                              'Cliente..................:'
144700010605     O                       VABCCM        z     +1
144800000616     O          E            findett     2
144900000613     O                                              'Riferimento Strategi.....:'
145000000613     O                       vlrhdl              +1
145100000616     O          E            findett     2
145200000613     O                                              'Giusti...................:'
145300000613     O                       §CTROK        2   +  1
145400000616     O          E            findett     2
145500000613     O                                              'Sbagliati e corretti.....:'
145600000613     O                       §CTRMO        2   +  1
145700000616     O          E            findett     2
145800000613     O                                              'Sbagliati e scartati.....:'
145900000613     O                       §CTRNO        2   +  1
146000000613** CMD - COMANDI CL
146100040216OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV95P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
146200040216OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV95PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
146300040216DLTOVR FILE(TITV95P TITV95PS) LVL(*)
146400010606
146500000613
146600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
146700050311X;VABRMN;VABRMA;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;VABCAS
146800040216
146900040113
