000100041025      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente LUNA - Cod. 0960763)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700030715     Ftitvxlp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030715     Ftitvxlps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600040726     D*------------
001700040726     D* SCHIEDE A PROGRAMMA
001800040726     D*------------
001900040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002000041025     D skWvab          S            501    DIM(20000) INZ(*blanks)
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D wrkdata         s               d
003300010330     D wrkora          s               t
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500010330     D recko           s            150    INZ(*blanks)
003600011113     D depcmd          s            150    INZ(*blanks)
003700040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003800041025     D DSVAB         e ds                  EXTNAME(FIVAB00F) INZ
003900041025     D skJ             S              5  0 INZ(*zeros)
004000030715     D*------------------
004100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004200030715     D*------------------
004300030715     D SkSplitFLD      S             10    DIM(100)
004400030715     D SkSplitCSV      S            256    DIM(100)
004500030715     D CharCSV         S              1
004600030715     D CharTXT         S              1
004700030715     D CharNUM         S              1
004800030715     D posDa           S              3  0 INZ(*zeros)
004900030715     D posA            S              3  0 INZ(*zeros)
005000030715     D i               s              3  0 INZ(1)
005100030715     D wGiro           s              1  0 INZ(*zeros)
005200030822     D*------------------
005300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005400030822     D*------------------
005500030822     D posDaDft        S              3  0 INZ(*zeros)
005600030822     D posADft         S              3  0 INZ(*zeros)
005700030822     D j               s              3  0 INZ(1)
005800030822     D wGiroDft        s              1  0 INZ(*zeros)
005900010330     D*------------------
006000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006100010330     D*------------------
006200010330     D WLBDA8          DS                  INZ
006300010330     D  G08DAT                 1      8  0
006400010330     D  G08INV                 9     16  0
006500010330     D  G08ERR                17     17
006600010330     D  G08TGI                18     22  0
006700031201     D*------------------
006800031201     D* Costanti
006900031201     D*------------------
007000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007200010330
007300010330
007400010330
007500990915     C                   time                    wrkdata
007600990915     C                   time                    wrkora
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900990921     C                   reset                   wrkesito
008000010601     C*
008100010601     C                   exsr      opeini
008200010605     C                   exsr      rwvab
008300010601     C*
008400010601     C                   seton                                        lr
008500010601
008600010601
008700010601
008800010601
008900010601     C*--------------------------------------------------------
009000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009100010601     C*--------------------------------------------------------
009200010601     C     PREELA        BEGSR
009300010601     C*
009400010601     C* SE OCCORRE SPEDIRE IN FILIALE
009500010601     C                   if        invfil <> *zeros and
009600010601     C                             flgGiro = '0'
009700010601     C*
009800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009900010601     C                   eval      flgGiro = '1'
010000000724     C*
010100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010300010601     C     invfil        chain     azorg01l
010400000724     C                   if        %found
010500000724     C                   movel(p)  CMD(1)        depcmd
010600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010700000724     C*
010800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010900011113     C                   Z-ADD     150           LENGH            15 5
011000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011100000724     C                   PARM                    depcmd
011200000724     C                   PARM                    LENGH
011300000724     C*
011400000724     C                   endif
011500000616     C*
011600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011700000616     C                   movel(p)  CMD(2)        depcmd
011800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011900000616     C*
012000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    depcmd
012400000616     C                   PARM                    LENGH
012500000613     C*
012600030715     C                   if        not %open(titvxlps)
012700030715     C                   open      titvxlps
012800030715     C                   except    testdett
012900000613     C                   endif
013000010601     C*
013100010601     C                   endif
013200010601     C*
013300010601     C                   ENDSR
013400010601     C***
013500010601
013600010601
013700010601
013800010601
013900010601     C*--------------------------------------------------------
014000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014100010601     C*--------------------------------------------------------
014200010601     C     ENDELA        BEGSR
014300990915     C*
014400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014500000613     C*
014600030715     C                   if        %open(titvxlps)
014700000616     C                   except    findett
014800030715     C                   close     titvxlps
014900000613     C                   endif
015000000616     C*
015100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015200011113     C                   Z-ADD     150           LENGH            15 5
015300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015400000616     C                   PARM                    CMD(3)
015500000616     C                   PARM                    LENGH
015600000616     C*
015700010601     C                   ENDSR
015800010601     C***
015900010601
016000010601
016100010601
016200010601
016300000613
016400000613     C*--------------------------------------------------------
016500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016600000613     C*--------------------------------------------------------
016700000621     C     STPR          BEGSR
016800000613     C*
016900030715     C                   if        not %open(titvxlp)
017000030715     C   50              open      titvxlp
017100990915     C                   endif
017200990915     C*
017300030715     C   50              except    riepilogo
017400990915     C*
017500030715     C                   if        %open(titvxlp)
017600030715     C   50              close     titvxlp
017700990914     C                   endif
017800000613     C*
017900000613     C                   ENDSR
018000000613     C***
018100000613
018200010330
018300010601
018400010601
018500010601
018600010330     C*--------------------------------------------------------
018700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018800010330     C*--------------------------------------------------------
018900010605     C     RWVAB         BEGSR
019000010330     C*
019100010330     C                   if        not %open(tivin00r)
019200010330     C                   open      tivin00r
019300010330     C                   endif
019400021025     C                   if        not %open(fivabwwr)
019500021025     C                   open      fivabwwr
019600010330     C                   endif
019700010330     C*
019800010604     C                   clear                   §CTROK
019900010604     C                   clear                   §CTRMO
020000010604     C                   clear                   §CTRNO
020100041025     C*
020200041025     C                   CLEAR                   FIVAB000
020300041025     C                   setoff                                       11
020400010330     C*
020500030822     C                   do        *HIVAL
020600010330     C*
020700030822     C                   read      tivin00r                               70
020800010618     C*
020900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021000010618     C                   if        vinflg = '1'
021100010618     C                   eval      flgOk = '1'
021200010618     C                   endif
021300010618     C*
021400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021500040510     C                   z-add     *zeros        wLenVINDTA        4 0
021600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
022000040510     C                   z-add     1             wFlgVINDTA
022100040510     C                   leave
022200040510     C                   endif
022300040510     C                   enddo
022400040510     C*
022500040218     C                   if        vindta > *blanks AND
022600040510     C                             wFlgVINDTA = 1
022700010330     C                   add       1             rrnum
022800010330     C*
022900010601     C                   if        *in70 = *off and
023000010330     C                             (vinflg = *blanks
023100010330     C                              or vinflg = '0'
023200010330     C                              or vinflg = '2')
023300010330     C*
023400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023500010711     C                   if        vinflg = *blanks or vinflg = '0'
023600010711     C                   clear                   vinmsg
023700010711     C                   endif
023800010601     C*
023900010330     C                   exsr      inzvar
024000010605     C                   exsr      impvab
024100010601     C*
024200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024300010605     C                   eval      depfil = VABLNP
024400010601     C                   exsr      repfil
024500010601     C                   if        depfil = invfil
024600021025     C                   if        vlrpoi = 999
024700030822     C                   move(P)   invfil        VABFGS
024800021025     C                   else
024900030822     C                   move(P)   vlrpoi        VABFGS
025000021025     C                   endif
025100010601     C*
025200010601     C                   exsr      PREELA
025300010601     C*
025400010604     C* Ebbene...
025500010604     C*
025600030822     C  N31              add       1             §CTROK            7 0
025700030822     C   32              add       1             §CTRMO            7 0
025800030822     C   31              add       1             §CTRNO            7 0
025900030822     C                   if        wGiro = 2
026000041025     C                   if        *IN31 = *off
026100041025     C                   add       1             skJ
026200041025     C                   movel     VABRMN        skWvab(skJ)
026300041025     C                   move      DSVAB         skWvab(skJ)
026400041025     C                   endif
026500030822     C                   endif
026600010604     C*
026700010604     C                   if        *in31 = *off and
026800010604     C                             *in32 = *off
026900010604     C                   eval      vinflg = '1'
027000010604     C                   else
027100010604     C                   eval      recko = vindta
027200010604     C                   except    rigadett
027300010604     C                   eval      vinflg = '2'
027400010604     C                   endif
027500010604     C                   endif
027600010601     C*
027700010604     C                   endif
027800010604     C*
027900010330     C                   else
028000010330     C                   eval      vinflg = '1'
028100010330     C                   endif
028200010601     C*
028300010601     C  N70              update    tivin000
028400010330     C*
028500030822     C  N70              enddo
028600041025     C*
028700041025     C* Scarico la schiera delle bolle in precedenza tradotte accorpando x medesimo RMN
028800041025     C* e sommando pesi e colli
028900041025     C                   exsr      ACCORPBOL
029000010601     C*
029100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029200010601     C                   if        cntNonEl = *zeros or
029300010601     C                             flgMulti = '0'
029400010330     C* Se non ci sono record con errori ...
029500010601     C                   if        §ctrno = 0 and
029600010604     C                             §ctrmo = 0 and
029700010601     C                             flgStato <> '2'
029800010330     C* ... restituisco esito OK.
029900010330     C                   eval      wrkesito = '0'
030000010330     C                   else
030100010330     C                   if        §ctrok > 0
030200010330     C                   eval      wrkesito = '1'
030300010330     C                   else
030400010615     C                   if        flgOk = '0'
030500010615     C                   eval      wrkesito = '2'
030600010615     C                   else
030700010615     C                   eval      wrkesito = '6'
030800010615     C                   endif
030900010330     C                   endif
031000010330     C                   endif
031100010601     C                   else
031200010601     C                   eval      wrkesito = '9'
031300010601     C                   endif
031400010330     C*
031500010330     C                   if        %open(tivin00r)
031600010330     C                   close     tivin00r
031700010330     C                   endif
031800021025     C                   if        %open(fivabwwr)
031900021025     C                   close     fivabwwr
032000010330     C                   endif
032100010601     C*
032200010601     C                   if        vlrpoi <> 999
032300010601     C                   eval      invfil = vlrpoi
032400010601     C                   endif
032500010330     C*
032600010330     C                   if        §ctrok > 0
032700010601     C                             and invfil > *zeros
032800010330     C                   exsr      invio
032900010330     C                   endif
033000010601     C*
033100010618     C                   if        flgGiro = '1'
033200010601     C                   exsr      endela
033300010618     C                   endif
033400010330     C*
033500010330     C                   ENDSR
033600010330     C***
033700041025
033800041025
033900041025
034000041025
034100041025     C*-----------------------------------------------------------*
034200041025     C* ACCORPBOL - OPERAZIONI DI ACCORPAMENTO E SCRITTURA BOLLE  *
034300041025     C*-----------------------------------------------------------*
034400041025     C     ACCORPBOL     BEGSR
034500041025     C*
034600041025     C* Innanzitutto ordino la schiera d wrk
034700041025     C                   sorta     skWvab
034800041025     C*
034900041025     C* Inizializzo campi d wrk e d deposito
035000041025     C                   z-add     *zeros        wNCL             10 0
035100041025     C                   z-add     *zeros        wPKB             30 9
035200041025     C*
035300041025     C                   z-add     1             skJ
035400041025     C                   dow       skJ <= %elem(skWvab)
035500041025     C                   if        skWvab(skJ) <> *blanks
035600041025     C                   eval      DSVAB = %subst(skWvab(skJ):16)
035700041025     C                   eval      wNCL = VABNCL
035800041025     C                   eval      wPKB = VABPKB
035900041025     C* Se NN è l'ultima bolla
036000041025     C                   if        skJ < %elem(skWvab)
036100041025     C* X stesso RMN => accorpo
036200041025     C                   if        %subst(skWvab(skJ):1:15)  =
036300041025     C                             %subst(skWvab(skJ+1):1:15)
036400041025     C                   eval      DSVAB = %subst(skWvab(skJ):16)
036500041025     C                   eval      wNCL = VABNCL
036600041025     C                   eval      wPKB = VABPKB
036700041025     C                   eval      DSVAB = %subst(skWvab(skJ+1):16)
036800041025     C                   eval      VABNCL = wNCL + VABNCL
036900041025     C                   eval      VABPKB = wPKB + VABPKB
037000041025     C                   eval      %subst(skWvab(skJ+1):16) = DSVAB
037100041025     C                   else
037200041025     C                   eval      DSVAB = %subst(skWvab(skJ):16)
037300041025     C                   eval      VABNCL = wNCL
037400041025     C                   eval      VABPKB = wPKB
037500041025     C                   write     FIVAB000
037600041025     C                   z-add     *zeros        wNCL
037700041025     C                   z-add     *zeros        wPKB
037800041025     C                   endif
037900041025     C                   else
038000041025     C* Se è l'ultima bolla => scarico direttamente il buffer
038100041025     C                   write     FIVAB000
038200041025     C                   endif
038300041025     C                   endif
038400041025     C                   add       1             skJ
038500041025     C                   enddo
038600041025     C*
038700041025     C                   ENDSR
038800041025     C***
038900010601
039000010601
039100010601
039200010330     C*----------------------------------------------------*
039300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039400010330     C*----------------------------------------------------*
039500010330     C     INZVAR        BEGSR
039600010330     C*
039700030822     C* Inizializzo variabili di wrk
039800020204     C                   Z-ADD     *zeros        Num5_0            5 0
039900030822     C*
040000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040100030822     C                   CLEAR                   SkSplitCSV
040200030822     C                   EVAL      i = 1
040300030822     C                   EVAL      posDa = *zeros
040400030822     C                   EVAL      posA  = *zeros
040500030822     C*
040600030822     C* Reimposto i valori di default
040700030822     C                   EXSR      DEFCAM
040800030822     C*
040900010330     C                   ENDSR
041000010330     C*----------------------------------------------------*
041100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
041200010330     C*----------------------------------------------------*
041300010330     C     DEFCAM        BEGSR
041400030822     C*
041500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
041600030715     C* e delimitatore testo.
041700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
041800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
041900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
042000030822     C*
042100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
042200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
042300030715     C                   SETON                                        50
042400030715     C                   ELSE
042500030715     C                   SETOFF                                       50
042600030715     C                   ENDIF
042700030822     C*
042800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
042900030822     C                   EVAL      posDaDft = 1
043000030822     C                   EVAL      posADft  = 0
043100030822     C                   EVAL      wGiroDft = 0
043200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
043300030822     C                             posDaDft > 0
043400030822     C*
043500030822     C* Gestisco il 1° giro
043600030822     C                   IF        wGiroDft = 0
043700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
043800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
043900030822     C* Incremento il contatore dei "giri"
044000030822     C                   EVAL      wGiroDft = 1
044100030822     C                   ELSE
044200030822     C                   EVAL      posDaDft = posADft
044300030822     C                   ENDIF
044400030822     C* Eseguo lo scan x trovare la fine del campo corrente
044500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
044600030822     C*
044700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
044800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
044900030822     C                   IF        posDaDft > 0 AND
045000030822     C                             posADft  > 0
045100030822     C* NCL
045200030822     C                   IF        %subst(
045300030822     C                             %subst(vlrppt:posDaDft+1:
045400030822     C                             posADft-posDaDft-1):1:3)
045500030822     C                             = 'NCL'
045600030822     C                   EVAL      PiStr=%trim(%subst(
045700030822     C                             %subst(vlrppt:posDaDft+1:
045800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045900030822     C                   EXSR      CHKNUM
046000030822     C                   IF        PiInt=*on
046100041025     C                   Z-ADD     PiVal         VABNCL
046200030822     C                   ENDIF
046300030822     C                   ENDIF
046400030822     C* CCM
046500030822     C                   IF        %subst(
046600030822     C                             %subst(vlrppt:posDaDft+1:
046700030822     C                             posADft-posDaDft-1):1:3)
046800030822     C                             = 'CCM'
046900030822     C                   EVAL      PiStr=%trim(%subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047200030822     C                   EXSR      CHKNUM
047300030822     C                   IF        PiInt=*on
047400030822     C                   Z-ADD     PiVal         VABCCM
047500030822     C                   ENDIF
047600030822     C                   ENDIF
047700030822     C* LNP
047800030822     C                   IF        %subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):1:3)
048100030822     C                             = 'LNP'
048200030822     C                   EVAL      PiStr=%trim(%subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048500030822     C                   EXSR      CHKNUM
048600030822     C                   IF        PiInt=*on
048700030822     C                   Z-ADD     PiVal         VABLNP
048800030822     C                   ENDIF
048900030822     C                   ENDIF
049000030822     C* NRS
049100030822     C                   IF        %subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):1:3)
049400030822     C                             = 'NRS'
049500030822     C                   EVAL      PiStr=%trim(%subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049800030822     C                   EXSR      CHKNUM
049900030822     C                   IF        PiInt=*on
050000030822     C                   Z-ADD     PiVal         VABNRS
050100030822     C                   ENDIF
050200030822     C                   ENDIF
050300030822     C* CTR
050400030822     C                   IF        %subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):1:3)
050700030822     C                             = 'CTR'
050800030822     C                   EVAL      PiStr=%trim(%subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100030822     C                   EXSR      CHKNUM
051200030822     C                   IF        PiInt=*on
051300030822     C                   Z-ADD     PiVal         VABCTR
051400030822     C                   ENDIF
051500030822     C                   ENDIF
051600030822     C* PKB
051700030822     C                   IF        %subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):1:3)
052000030822     C                             = 'PKB'
052100030822     C                   EVAL      PiStr=%trim(%subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400030822     C                   EXSR      CHKNUM
052500030822     C                   IF        PiNum=*on
052600030822     C                   Z-ADD     PiVal         VABPKB
052700030822     C                   ENDIF
052800030822     C                   ENDIF
052900030822     C* VLB
053000030822     C                   IF        %subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):1:3)
053300030822     C                             = 'VLB'
053400030822     C                   EVAL      PiStr=%trim(%subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700030822     C                   EXSR      CHKNUM
053800030822     C                   IF        PiNum=*on
053900030822     C                   Z-ADD     PiVal         VABVLB
054000030822     C                   ENDIF
054100030822     C                   ENDIF
054200030822     C* QFT
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'QFT'
054700030822     C                   EVAL      PiStr=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   EXSR      CHKNUM
055100030822     C                   IF        PiNum=*on
055200030822     C                   Z-ADD     PiVal         VABQFT
055300030822     C                   ENDIF
055400030822     C                   ENDIF
055500030822     C* CBO
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'CBO'
056000030822     C                   EVAL      VABCBO=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   ENDIF
056400030822     C* TSP
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'TSP'
056900030822     C                   EVAL      VABTSP=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   ENDIF
057300030822     C* VAS
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'VAS'
057800030822     C                   EVAL      VABVAS=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   ENDIF
058200030822     C* VCA
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'VCA'
058700030822     C                   EVAL      VABVCA=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   ENDIF
059100030822     C* GCA
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'GCA'
059600030822     C                   EVAL      VABGCA=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   ENDIF
060000030822     C* CTM
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'CTM'
060500030822     C                   EVAL      VABCTM=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   ENDIF
060900030822     C* FFD
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'FFD'
061400030822     C                   EVAL      VABFFD=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   ENDIF
061800030822     C* VAD
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'VAD'
062300030822     C                   EVAL      VABVAD=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   ENDIF
062700030822     C* GMA
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'GMA'
063200030822     C                   EVAL      VABGMA=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   ENDIF
063600030822     C* GGA
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'GGA'
064100030822     C                   EVAL      VABGGA=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   ENDIF
064500030822     C* GVA
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'GVA'
065000030822     C                   EVAL      VABGVA=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   ENDIF
065400030822     C* TC1
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'TC1'
065900030822     C                   EVAL      VABTC1=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   ENDIF
066300030822     C* TC2
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'TC2'
066800030822     C                   EVAL      VABTC2=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   ENDIF
067200030822     C* ...
067300030822     C                   ENDIF
067400030822     C                   ENDDO
067500020204     C*
067600010330     C                   ENDSR
067700010607     C*----------------------------------------------------*
067800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
067900010607     C*----------------------------------------------------*
068000010607     C     IMPVAB        BEGSR
068100010607     C*
068200010607     C                   SETOFF                                       3132
068300030515     C*
068400010607     C                   MOVEL     datcor        VABAAS
068500010607     C                   MOVE      datcor        VABMGS
068600010607     C*
068700030715     C**********
068800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
068900030715     C**********
069000030715     C                   IF        wGiro = *zeros
069100040726     C*
069200040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
069300040726     C                   EVAL      depvinDTA = vinDTA
069400040726     C*
069500041025     C* Se richiesta inserisco qui la forzatura della intesatazione colonne personalizzata
069600041025     C
069700041025     C                   IF        intcol(1)+intcol(2)+intcol(3) <> *blanks
069800040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
069900040726     C                                      %trim(intcol(3))
070000041025     C                   ENDIF
070100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
070200031201     C     minu:maiu     XLATE     vindta        vindta
070300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
070400040324     C                   DOW       posDa <= %len(%trimr(vindta))
070500030715     C*
070600030715     C* Gestisco il 1° campo
070700030715     C                   IF        i = 1
070800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
070900030715     C                   EVAL      posDa = 1
071000030715     C* Eseguo lo scan x trovare la fine del primo campo
071100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071200030715     C* A questo punto "estraggo" il campo corrente
071300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071400030715     C                                                    (posA-posDa))
071500030715     C* X i campi successivi al 1°
071600030715     C                   ELSE
071700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
071800030715     C                   EVAL      posDa = posA + 1
071900030715     C* Eseguo lo scan x trovare la fine del campo corrente
072000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072100030715     C* Gestisco l'ultimo campo
072200030715     C                   IF        posA = *zeros
072300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
072400030715     C* A questo punto "estraggo" il campo corrente
072500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072600030715     C                                                    (posA-posDa))
072700030715     C                   LEAVE
072800030715     C                   ELSE
072900030715     C* A questo punto "estraggo" il campo corrente
073000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073100030715     C                                                    (posA-posDa))
073200030715     C                   ENDIF
073300030715     C                   ENDIF
073400030715     C* Incremento il contatore d campo
073500030715     C                   EVAL      i = i +1
073600030715     C                   ENDDO
073700030715     C*
073800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
073900030715     C* delimitazione testo
074000030715     C                   EVAL      i = 1
074100030715     C                   DOW       i < %elem(SkSplitFLD)
074200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
074300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
074400030715     C                   EVAL      i = i + 1
074500030715     C                   ENDDO
074600030715     C                   EVAL      wGiro = 1
074700040726     C* Ripristino la 1° riga originale
074800040726     C                   EVAL      vinDTA = depvinDTA
074900030715     C                   ELSE
075000030715     C                   EVAL      wGiro = 2
075100031204     C**********
075200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
075300031204     C* d separatore campo
075400031204     C**********
075500040726     C                   EVAL      vindta = %trim(vindta)
075600040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
075700040726     C                   EVAL      vindta = %subst(vindta:2)
075800040726     C                   ENDDO
075900031204     C*
076000031204     C                   Z-ADD     *zeros        lunghInput        4 0
076100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
076200040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
076300040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
076400040726     C                   EVAL      lunghInput = %len(%trim(vindta))
076500040726     C                   ENDDO
076600030715     C**********
076700030715     C* Effettuo lo split del campo dati d input
076800030715     C**********
076900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077000040324     C                   DOW       posDa <= %len(%trimr(vindta))
077100030715     C*
077200030715     C* Gestisco il 1° campo
077300030715     C                   IF        i = 1
077400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077500030715     C                   EVAL      posDa = 1
077600030715     C* Eseguo lo scan x trovare la fine del primo campo
077700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077800030715     C* A questo punto "estraggo" il campo corrente
077900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078000030715     C                                                    (posA-posDa))
078100030715     C* X i campi successivi al 1°
078200030715     C                   ELSE
078300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078400030715     C                   EVAL      posDa = posA + 1
078500031204     C* Verifico che nn vi sia il campo nn valorizzato
078600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
078700031204     C* Se campo nn valorizzato skippo al prossimo
078800031204     C                   EVAL      SkSplitCSV(i) = *blanks
078900031204     C                   EVAL      posA  = posA + 1
079000031204     C                   ELSE
079100030715     C* Eseguo lo scan x trovare la fine del campo corrente
079200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079300030715     C* Gestisco l'ultimo campo
079400030715     C                   IF        posA = *zeros
079500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
079600030715     C* A questo punto "estraggo" il campo corrente
079700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079800030715     C                                                    (posA-posDa))
079900030715     C                   LEAVE
080000030715     C                   ELSE
080100030715     C* A questo punto "estraggo" il campo corrente
080200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080300030715     C                                                    (posA-posDa))
080400030715     C                   ENDIF
080500031204     C                   ENDIF
080600030715     C                   ENDIF
080700030715     C* Incremento il contatore d campo
080800030715     C                   EVAL      i = i +1
080900030715     C                   ENDDO
081000030715     C*
081100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081200030715     C* delimitazione testo
081300030715     C                   EVAL      i = 1
081400030715     C                   DOW       i < %elem(SkSplitCSV)
081500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
081600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
081700030715     C                   EVAL      i = i + 1
081800030715     C                   ENDDO
081900030715     C*
082000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
082100030715     C                   EVAL      i = 1
082200030715     C                   DOW       i < %elem(SkSplitFLD)
082300030715     C*
082400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
082500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
082600030715     C                             SkSplitCSV(i) <> *blanks
082700030721     C*** ATB
082800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
082900030721     C                   EVAL      VABATB = SkSplitCSV(i)
083000030721     C                   ENDIF
083100030715     C*** RSD
083200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
083300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
083400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
083500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
083600030715     C* ==
083700030715     C                   ENDIF
083800030721     C*** RD2
083900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
084000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
084100030721     C                   ENDIF
084200030715     C*** IND
084300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
084400030715     C                   EVAL      VABIND = SkSplitCSV(i)
084500030715     C                   ENDIF
084600030715     C*** LOD
084700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
084800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
084900030715     C                   ENDIF
085000030715     C*** PRD
085100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
085200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
085300030715     C                   ENDIF
085400030721     C*** NZD
085500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
085600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
085700030721     C                   ENDIF
085800030715     C*** RMA
085900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
086000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
086100030715     C                   ENDIF
086200030715     C*** NOT
086300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
086400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
086500030715     C                   ENDIF
086600030715     C*** NT2
086700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
086800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
086900030715     C                   ENDIF
087000030715     C*** VCA
087100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
087200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
087300030715     C                   ENDIF
087400030715     C*** VAS
087500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
087600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
087700030715     C                   ENDIF
087800030715     C*** FFD
087900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
088000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
088100030715     C                   ENDIF
088200030715     C*** CBO
088300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
088400030715     C                   EVAL      VABCBO = SkSplitCSV(i)
088500030715     C                   ENDIF
088600030715     C
088700030721     C*** GC1
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
088900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** GC2
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
089300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** TSP
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
089700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** NAS
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** TIC
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
090500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** GCA
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
090900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** XCO
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
091300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** CTM
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
091700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** TCR
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
092100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** CTS
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
092500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** FTM
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
092900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** VAD
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
093300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** GMA
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
093700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** GGA
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
094100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** GVA
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
094500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** TC1
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
094900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** TC2
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
095300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** SCL
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
095700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** RMO
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
096100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** NMO
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
096500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*
096800030721     C* Reperisco quindi i campi numerici...
096900030721     C*** CAD
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
097100030721     C                   EVAL      PiStr=SkSplitCSV(i)
097200030721     C                   EXSR      CHKNUM
097300030721     C                   IF        PiInt=*on
097400030721     C                   Z-ADD     PiVal         Num5_0
097500030721     C                   MOVEL(p)  Num5_0        VABCAD
097600030721     C                   ELSE
097700030721     C                   SETON                                        32
097800030721     C                   EVAL      VABCAD = *zeros
097900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098000030721     C                             + ' ' + 'VABCAD'
098100030721     C                   ENDIF
098200030721     C                   ENDIF
098300030721     C*** RMN
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
098500030721     C                   EVAL      PiStr=SkSplitCSV(i)
098600030721     C                   EXSR      CHKNUM
098700030721     C                   IF        PiInt=*on
098800030721     C                   Z-ADD     PiVal         VABRMN
098900030721     C                   ELSE
099000041025     C                   SETON                                        31
099100041025     C                   EVAL      VABRMN = *zeros
099200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099300030721     C                             + ' ' + 'VABRMN'
099400030721     C                   ENDIF
099500030721     C                   ENDIF
099600030721     C*** NSP
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
099800030721     C                   EVAL      PiStr=SkSplitCSV(i)
099900030721     C                   EXSR      CHKNUM
100000040726     C                   IF        PiInt=*on
100100040726     C                   Z-ADD     PiVal         VABNSP
100200040802     C*                  IF        PiVal>9999999
100300040802     C*                  SETON                                        32
100400040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
100500040802     C*                            + ' ' + 'VABNSP'
100600040802     C*                  ENDIF
100700030721     C                   ELSE
100800030721     C                   SETON                                        32
100900030721     C                   EVAL      VABNSP = *zeros
101000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101100030721     C                             + ' ' + 'VABNSP'
101200030721     C                   ENDIF
101300030721     C                   ENDIF
101400030721     C*** NCL
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
101600030721     C                   EVAL      PiStr=SkSplitCSV(i)
101700030721     C                   EXSR      CHKNUM
101800030721     C                   IF        PiInt=*on
101900041025     C  N11              Z-ADD     PiVal         VABNCL
102000041025     C   11              ADD       PiVal         VABNCL
102100030721     C                   ELSE
102200030721     C                   SETON                                        32
102300030721     C                   EVAL      VABNCL = *zeros
102400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102500030721     C                             + ' ' + 'VABNCL'
102600030721     C                   ENDIF
102700030721     C                   ENDIF
102800030721     C*** PKB
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
103000030721     C                   EVAL      PiStr=SkSplitCSV(i)
103100030721     C                   EXSR      CHKNUM
103200030721     C                   IF        PiNum=*on
103300041025     C  N11              Z-ADD     PiVal         VABPKB
103400041025     C   11              ADD       PiVal         VABPKB
103500030721     C                   ELSE
103600030721     C                   SETON                                        32
103700030721     C                   EVAL      VABPKB = *zeros
103800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103900030721     C                             + ' ' + 'VABPKB'
104000030721     C                   ENDIF
104100030721     C                   ENDIF
104200030721     C*** CAS
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
104400030721     C                   EVAL      PiStr=SkSplitCSV(i)
104500030721     C                   EXSR      CHKNUM
104600030721     C                   IF        PiNum=*on
104700030721     C                   Z-ADD     PiVal         VABCAS
104800030721     C                   ELSE
104900030721     C                   SETON                                        32
105000030721     C                   EVAL      VABCAS = *zeros
105100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105200030721     C                             + ' ' + 'VABCAS'
105300030721     C                   ENDIF
105400030721     C                   ENDIF
105500030721     C*** CCM
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
105700030721     C                   EVAL      PiStr=SkSplitCSV(i)
105800030721     C                   EXSR      CHKNUM
105900030721     C                   IF        PiInt=*on      AND
106000030721     C                             PiVal<=9999999 AND
106100030721     C                             PiVal>*zeros
106200030721     C                   Z-ADD     PiVal         VABCCM
106300030721     C                   ELSE
106400030721     C                   SETON                                        32
106500030721     C                   EVAL      VABCCM = *zeros
106600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106700030721     C                             + ' ' + 'VABCCM'
106800030721     C                   ENDIF
106900030721     C                   ENDIF
107000030721     C*** LNP
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
107200030721     C                   EVAL      PiStr=SkSplitCSV(i)
107300030721     C                   EXSR      CHKNUM
107400030721     C                   IF        PiInt=*on    AND
107500030721     C                             PiVal<=999   AND
107600030721     C                             PiVal>*zeros
107700030721     C                   Z-ADD     PiVal         VABLNP
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      VABLNP = *zeros
108100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108200030721     C                             + ' ' + 'VABLNP'
108300030721     C                   ENDIF
108400030721     C                   ENDIF
108500030721     C*** NRS
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
108700030721     C                   EVAL      PiStr=SkSplitCSV(i)
108800030721     C                   EXSR      CHKNUM
108900030721     C                   IF        PiInt=*on AND
109000030721     C                             PiVal<=99
109100030721     C                   Z-ADD     PiVal         VABNRS
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABNRS'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** LNA
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiInt=*on    AND
110300030721     C                             PiVal<=999   AND
110400030721     C                             PiVal>*zeros
110500030721     C                   Z-ADD     PiVal         VABLNA
110600030721     C                   ELSE
110700030721     C                   SETON                                        32
110800030721     C                   EVAL      VABLNA = *zeros
110900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111000030721     C                             + ' ' + 'VABLNA'
111100030721     C                   ENDIF
111200030721     C                   ENDIF
111300030721     C*** CTR
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
111500030721     C                   EVAL      PiStr=SkSplitCSV(i)
111600030721     C                   EXSR      CHKNUM
111700030721     C                   IF        PiInt=*on  AND
111800030721     C                             PiVal<=999
111900030721     C                   Z-ADD     PiVal         VABCTR
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300030721     C                             + ' ' + 'VABCTR'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600030721     C*** IAS
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiNum=*on
113100030721     C                   Z-ADD     PiVal         VABIAS
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500030721     C                             + ' ' + 'VABIAS'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** VLB
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiNum=*on
114300030721     C                   Z-ADD     PiVal         VABVLB
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABVLB'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** QFT
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiNum=*on
115500030721     C                   Z-ADD     PiVal         VABQFT
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABQFT'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** NCD
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiInt=*on      AND
116700030721     C                             PiVal<=9999999
116800030721     C                   Z-ADD     PiVal         VABNCD
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABNCD'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** NCA
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiInt=*on      AND
118000030721     C                             PiVal<=9999999
118100030721     C                   Z-ADD     PiVal         VABNCA
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500030721     C                             + ' ' + 'VABNCA'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800030721     C*** ZNC
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiInt=*on AND
119300030721     C                             PiVal<=99
119400030721     C                   Z-ADD     PiVal         VABZNC
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABZNC'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** DCR
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on       AND
120600030721     C                             PiVal<=99999999
120700030721     C                   Z-ADD     PiVal         VABDCR
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABDCR'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** HCR
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on   AND
121900030721     C                             PiVal<=9999
122000030721     C                   Z-ADD     PiVal         VABHCR
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABHCR'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** VMD
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiNum=*on
123200030721     C                   Z-ADD     PiVal         VABVMD
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABVMD'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** ANT
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on        AND
124400030721     C                             PiVal<=999999999
124500030721     C                   Z-ADD     PiVal         VABANT
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABANT'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** CMO
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on
125700030721     C                   Z-ADD     PiVal         Num5_0
125800030721     C                   MOVEL(p)  Num5_0        VABCMO
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      VABCMO = *zeros
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABCMO'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030715     C*
126700030715     C                   ENDIF
126800030715     C                   EVAL      i = i + 1
126900030715     C                   ENDDO
127000040726     C*
127100040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
127200040726     C                   IF        VABLNP = *zeros
127300040726     C                   SETON                                        31
127400040726     C                   ENDIF
127500040726     C*
127600040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
127700040726     C* d valorizzarli sempre entrambi
127800040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
127900040726     C                   Z-ADD     VABNSP        VABRMN
128000040726     C                   ENDIF
128100040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
128200040726     C                   MOVEL     VABRMN        VABRMA
128300040726     C                   ENDIF
128400040726     C*
128500040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
128600040726     C                   IF        VABCAS > *zeros
128700040726     C                   IF        VABVCA = *blanks
128800040726     C                   EVAL      VABVCA = 'EUR'
128900040726     C                   ENDIF
129000040726     C                   IF        VABCBO = '1'
129100040726     C                   EVAL      VABCBO = '4'
129200040726     C                   ENDIF
129300040726     C                   IF        VABCBO = '2'
129400040726     C                   EVAL      VABCBO = '6'
129500040726     C                   ENDIF
129600040726     C                   ENDIF
129700020204     C*
129800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
129900020204     C                   EXSR      CHKIMPDIV
130000030715     C                   ENDIF
130100020204     C*
130200010607     C                   ENDSR
130300010607     C*----------------------------------------------------*
130400010601
130500020204
130600020204     C*----------------------------------------------------*
130700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
130800020204     C*----------------------------------------------------*
130900020204     C     CHKIMPDIV     BEGSR
131000020204     C*
131100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
131200020204     C                   Z-ADD     *zeros        wrkDec            9 9
131300020204     C*
131400020204     C* Come prima cosa effettuo considerazioni sulla divisa
131500020204     C                   IF        vabIAS > *zeros
131600020204     C                   IF        vabVAS <> 'EUR'
131700020204     C                   EVAL      vabVAS =  'ITL'
131800020204     C                   ENDIF
131900020204     C                   ENDIF
132000020204     C*
132100020204     C                   IF        vabCAS > *zeros
132200020204     C                   IF        vabVCA <> 'EUR'
132300020204     C                   EVAL      vabVCA =  'ITL'
132400020204     C                   ENDIF
132500020204     C                   ENDIF
132600020204     C*
132700020204     C                   IF        vabVMD > *zeros
132800020204     C                   IF        vabVAD <> 'EUR'
132900020204     C                   EVAL      vabVAD =  'ITL'
133000020204     C                   ENDIF
133100020204     C                   ENDIF
133200020204     C*
133300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
133400020204     C                   Z-ADD     vabIAS        wrkDec
133500020204     C                   IF        wrkDec > *zeros
133600020204     C                   IF        vabVAS = 'ITL'
133700020204     C                   EVAL      vabIAS = *zeros
133800020204     C                   ENDIF
133900020204     C                   ENDIF
134000020204     C*
134100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
134200020204     C                   Z-ADD     vabCAS        wrkDec
134300020204     C                   IF        wrkDec > *zeros
134400020204     C                   IF        vabVCA = 'ITL'
134500020204     C                   EVAL      vabCAS = *zeros
134600020204     C                   ENDIF
134700020204     C                   ENDIF
134800020204     C*
134900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
135000020204     C                   Z-ADD     vabVMD        wrkDec
135100020204     C                   IF        wrkDec > *zeros
135200020204     C                   IF        vabVAD = 'ITL'
135300020204     C                   EVAL      vabVMD = *zeros
135400020204     C                   ENDIF
135500020204     C                   ENDIF
135600020204     C*
135700020204     C                   ENDSR
135800020204     C***
135900020204
136000010330
136100010330
136200010330     C*----------------------------------------------------*
136300010330     C*  CONTROLLO NUMERICITA' CAMPI
136400010330     C*----------------------------------------------------*
136500010330     C     CHKNUM        BEGSR
136600010330     C*
136700010606     C                   IF        PiDecChr = *blanks
136800030715     C                   EVAL      PiDecChr = CharNUM
136900010606     C                   ENDIF
137000010606     C*
137100010606     C                   CALL(e)   'ISNUMERIC'
137200010330     C                   PARM                    PiStr            30
137300010606     C                   PARM                    PiDecChr          1
137400010330     C                   PARM      *ZEROS        PiVal            30 9
137500010330     C                   PARM      '0'           PiInt             1
137600010330     C                   PARM      '0'           PiNum             1
137700010330     C                   IF        %error
137800010606     C                   EVAL      PiNum=*off
137900010330     C                   ENDIF
138000010330     C*
138100010330     C                   ENDSR
138200010330     C***
138300010330
138400010601
138500010601
138600010601
138700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
138800010601     C     repfil        BEGSR
138900010601     C*
139000010601     C                   if        invfil = *zeros and
139100010601     C                             depfil > *zeros and
139200010629     C                             (vinflg = *blanks or
139300010629     C                              vinflg = *zeros)
139400010601     C
139500010601     C                   eval      invfil = depfil
139600010601     C                   endif
139700010601     C*
139800010601     C                   if        depfil <> invfil and
139900010601     C                             invfil > *zeros
140000010601     C                   eval      flgMulti = '1'
140100010601     C                   if        vinflg = *blanks
140200010601     C                   add       1             cntNonEl
140300010601     C                   endif
140400010601     C                   endif
140500010601     C*
140600010601     C                   if        vinflg = '2'
140700010601     C                   eval      flgStato = '2'
140800010601     C                   endif
140900010601     C*
141000010601     C                   ENDSR
141100010601     C***
141200010601
141300010601
141400010601
141500010330
141600010330
141700010330
141800990920      /TITLE Invio dei dati al punto operativo.
141900000613     C     invio         BEGSR
142000990920     C*
142100990920     C                   reset                   dscmz
142200010601     C                   move      invfil        cmzdst
142300990920     C                   eval      cmzfld = vlrfou
142400990920     C                   eval      cmzmbd = vlrhdl
142500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
142600000710     C                   if        prmfir = *blanks
142700021025     C                   eval      cmzfla = 'FIVAB00F'
142800021025     C                   eval      cmzmba = 'FIVAB00F'
142900000710     C                   else
143000000710     C                   eval      cmzfla = prmfir
143100000710     C                   eval      cmzmba = prmfir
143200000710     C                   endif
143300990920     C                   eval      cmznrr = *zeros
143400990920     C                   move      §ctrok        cmznrr
143500021018     C                   eval      cmzlba = vlrfl1
143600990920     C                   call(e)   'TIS711C'
143700990920     C                   parm                    dscmz
143800990921     C                   parm      *blanks       esito
143900990920     C                   if        %error
144000990920     C                             or cmzerr = '1'
144100990921     C                             or esito  = '1'
144200000710     C                   eval      wrkesito = '3'
144300990920     C                   endif
144400990920     C*
144500000613     C                   ENDSR
144600990910
144700010601
144800010601
144900010601
145000010601
145100010601      /TITLE Invio dei dati al punto operativo.
145200010601     C     opeini        BEGSR
145300010601     C*
145400010601     C* Inizializzo flag e contatori operativi
145500010601     C                   movel     '0'           flgGiro           1
145600010601     C                   movel     '0'           flgMulti          1
145700010601     C                   movel     '1'           flgStato          1
145800010615     C                   movel     '0'           flgOk             1
145900010601     C                   z-add     *zeros        cntNonEl         10 0
146000010601     C                   z-add     *zeros        depfil            3 0
146100010601     C                   z-add     *zeros        invfil            3 0
146200010601     C*
146300010601     C                   ENDSR
146400010601     C***
146500010601
146600010601
146700050421
146800050421
146900050421     C     *pssr         BEGSR
147000050421     C*
147100050421     C                   eval      wrkesito = '2'
147200050421     C*
147300050421     C                   ENDSR     '*CANCL'
147400050421     C***
147500050421
147600010601
147700010330
147800010330
147900000613     C     *inzsr        BEGSR
148000990910     C*
148100990910     C     *entry        plist
148200990920     C                   parm                    tivlrds
148300990921     C                   parm      wrkesito      esito
148400000724     C                   parm                    prmlit
148500000710     C                   parm                    prmfir
148600010330     C*
148700010330     C* CALCOLA LA DATA CORRENTE
148800010330     C                   time                    wn14             14 0
148900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
149000010330     C                   z-add     wn8           g08dat
149100010330     C                   z-add     *zeros        g08inv
149200010330     C                   movel     '0'           g08err
149300010330     C                   call      'XSRDA8'
149400010330     C                   parm                    wlbda8
149500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
149600000613     C*
149700000613     C                   ENDSR
149800000613     C***
149900990908
150000030715     Otitvxlp   E            riepilogo         2
150100990915     O                                              'Upload via Internet'
150200990915     O                                           +1 'Traduzione TIVIN00R -'
150300021025     O                                           +1 'FIVABWWR'
150400990915     O                                           +1 'Testate bolle'
150500990915     O          E            riepilogo   2
150600990915     O                       wrkdata
150700990915     O                       wrkora              +1
150800990915     O                       procname            +1
150900990915     O          E            riepilogo   2
151000990915     O                                              'Cliente..................:'
151100010605     O                       VABCCM        z     +1
151200990915     O          E            riepilogo   2
151300990920     O                                              'Riferimento Strategi.....:'
151400990920     O                       vlrhdl              +1
151500990915     O          E            riepilogo   2
151600990915     O                                              'Giusti...................:'
151700971022     O                       §CTROK        2   +  1
151800990915     O          E            riepilogo   2
151900990915     O                                              'Sbagliati e corretti.....:'
152000971022     O                       §CTRMO        2   +  1
152100990915     O          E            riepilogo   2
152200990915     O                                              'Sbagliati e scartati.....:'
152300971022     O                       §CTRNO        2   +  1
152400000613
152500030715     Otitvxlps  E            testdett          2
152600000613     O                                              'Upload via Internet'
152700000613     O                                           +1 'Traduzione TIVIN00R -'
152800021025     O                                           +1 'FIVABWWR'
152900000613     O                                           +1 'Testate bolle'
153000000616     O          E            testdett    3
153100000613     O                                           +2 'N° rec'
153200000613     O                                           +3 'Anteprima contenuto'
153300000616     O          E            rigadett    2
153400000613     O                       rrnum               +2
153500000621     O                       recko               +3
153600000616     O          E            findett     2
153700000613     O                       wrkdata
153800000613     O                       wrkora              +1
153900000613     O                       procname            +1
154000000616     O          E            findett     2
154100000613     O                                              'Cliente..................:'
154200010605     O                       VABCCM        z     +1
154300000616     O          E            findett     2
154400000613     O                                              'Riferimento Strategi.....:'
154500000613     O                       vlrhdl              +1
154600000616     O          E            findett     2
154700000613     O                                              'Giusti...................:'
154800000613     O                       §CTROK        2   +  1
154900000616     O          E            findett     2
155000000613     O                                              'Sbagliati e corretti.....:'
155100000613     O                       §CTRMO        2   +  1
155200000616     O          E            findett     2
155300000613     O                                              'Sbagliati e scartati.....:'
155400000613     O                       §CTRNO        2   +  1
155500000613** CMD - COMANDI CL
155600030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
155700030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
155800030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
155900010606
156000000613
156100040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
156200041025
156300041025
156400040726
