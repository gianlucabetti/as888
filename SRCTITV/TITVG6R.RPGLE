000100050105      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente SMC - Cod. 0052904)
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
002000050103     D skWvab          S            516    DIM(20000) INZ(*blanks)
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
004000050103     D wVABRMNalfa     s             15    INZ(*blanks)
004100030715     D*------------------
004200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004300030715     D*------------------
004400030715     D SkSplitFLD      S             10    DIM(100)
004500030715     D SkSplitCSV      S            256    DIM(100)
004600030715     D CharCSV         S              1
004700030715     D CharTXT         S              1
004800030715     D CharNUM         S              1
004900030715     D posDa           S              3  0 INZ(*zeros)
005000030715     D posA            S              3  0 INZ(*zeros)
005100030715     D i               s              3  0 INZ(1)
005200030715     D wGiro           s              1  0 INZ(*zeros)
005300030822     D*------------------
005400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005500030822     D*------------------
005600030822     D posDaDft        S              3  0 INZ(*zeros)
005700030822     D posADft         S              3  0 INZ(*zeros)
005800030822     D j               s              3  0 INZ(1)
005900030822     D wGiroDft        s              1  0 INZ(*zeros)
006000010330     D*------------------
006100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200010330     D*------------------
006300010330     D WLBDA8          DS                  INZ
006400010330     D  G08DAT                 1      8  0
006500010330     D  G08INV                 9     16  0
006600010330     D  G08ERR                17     17
006700010330     D  G08TGI                18     22  0
006800031201     D*------------------
006900031201     D* Costanti
007000031201     D*------------------
007100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007300010330
007400010330
007500010330
007600990915     C                   time                    wrkdata
007700990915     C                   time                    wrkora
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100010601     C*
008200010601     C                   exsr      opeini
008300010605     C                   exsr      rwvab
008400010601     C*
008500010601     C                   seton                                        lr
008600010601
008700010601
008800010601
008900010601
009000010601     C*--------------------------------------------------------
009100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009200010601     C*--------------------------------------------------------
009300010601     C     PREELA        BEGSR
009400010601     C*
009500010601     C* SE OCCORRE SPEDIRE IN FILIALE
009600010601     C                   if        invfil <> *zeros and
009700010601     C                             flgGiro = '0'
009800010601     C*
009900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010000010601     C                   eval      flgGiro = '1'
010100000724     C*
010200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010400010601     C     invfil        chain     azorg01l
010500000724     C                   if        %found
010600000724     C                   movel(p)  CMD(1)        depcmd
010700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010800000724     C*
010900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011000011113     C                   Z-ADD     150           LENGH            15 5
011100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011200000724     C                   PARM                    depcmd
011300000724     C                   PARM                    LENGH
011400000724     C*
011500000724     C                   endif
011600000616     C*
011700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011800000616     C                   movel(p)  CMD(2)        depcmd
011900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012000000616     C*
012100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    depcmd
012500000616     C                   PARM                    LENGH
012600000613     C*
012700030715     C                   if        not %open(titvxlps)
012800030715     C                   open      titvxlps
012900030715     C                   except    testdett
013000000613     C                   endif
013100010601     C*
013200010601     C                   endif
013300010601     C*
013400010601     C                   ENDSR
013500010601     C***
013600010601
013700010601
013800010601
013900010601
014000010601     C*--------------------------------------------------------
014100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014200010601     C*--------------------------------------------------------
014300010601     C     ENDELA        BEGSR
014400990915     C*
014500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014600000613     C*
014700030715     C                   if        %open(titvxlps)
014800000616     C                   except    findett
014900030715     C                   close     titvxlps
015000000613     C                   endif
015100000616     C*
015200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015300011113     C                   Z-ADD     150           LENGH            15 5
015400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015500000616     C                   PARM                    CMD(3)
015600000616     C                   PARM                    LENGH
015700000616     C*
015800010601     C                   ENDSR
015900010601     C***
016000010601
016100010601
016200010601
016300010601
016400000613
016500000613     C*--------------------------------------------------------
016600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016700000613     C*--------------------------------------------------------
016800000621     C     STPR          BEGSR
016900000613     C*
017000030715     C                   if        not %open(titvxlp)
017100030715     C   50              open      titvxlp
017200990915     C                   endif
017300990915     C*
017400030715     C   50              except    riepilogo
017500990915     C*
017600030715     C                   if        %open(titvxlp)
017700030715     C   50              close     titvxlp
017800990914     C                   endif
017900000613     C*
018000000613     C                   ENDSR
018100000613     C***
018200000613
018300010330
018400010601
018500010601
018600010601
018700010330     C*--------------------------------------------------------
018800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018900010330     C*--------------------------------------------------------
019000010605     C     RWVAB         BEGSR
019100010330     C*
019200010330     C                   if        not %open(tivin00r)
019300010330     C                   open      tivin00r
019400010330     C                   endif
019500021025     C                   if        not %open(fivabwwr)
019600021025     C                   open      fivabwwr
019700010330     C                   endif
019800010330     C*
019900010604     C                   clear                   §CTROK
020000010604     C                   clear                   §CTRMO
020100010604     C                   clear                   §CTRNO
020200010330     C*
020300030822     C                   do        *HIVAL
020400050510     C*
020500050510     C* Spengo l'indicatore che indica se in fase d traduzione record i valori d peso e volume
020600050510     C* devono essere sommati ai precedenti valori
020700050510     C                   CLEAR                   FIVAB000
020800050105     C*
020900050105     C* Solo al primo "giro" se nn presente nel file simulo la presenza della 1° riga d intestaz.
021000050105     C                   if        wGiro = *zeros
021100050105     C   12              clear                   tivin000
021200050105     C   12              exsr      inzvar
021300050105     C   12              exsr      impvab
021400050105     C                   endif
021500010330     C*
021600030822     C                   read      tivin00r                               70
021700010618     C*
021800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021900010618     C                   if        vinflg = '1'
022000010618     C                   eval      flgOk = '1'
022100010618     C                   endif
022200010618     C*
022300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
022400050103     C                   z-add     *zeros        wLenVINDTA        4 0
022500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
022600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
022700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
022800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
022900040510     C                   z-add     1             wFlgVINDTA
023000040510     C                   leave
023100040510     C                   endif
023200040510     C                   enddo
023300040510     C*
023400040218     C                   if        vindta > *blanks AND
023500040510     C                             wFlgVINDTA = 1
023600010330     C                   add       1             rrnum
023700010330     C*
023800010601     C                   if        *in70 = *off and
023900010330     C                             (vinflg = *blanks
024000010330     C                              or vinflg = '0'
024100010330     C                              or vinflg = '2')
024200010330     C*
024300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024400010711     C                   if        vinflg = *blanks or vinflg = '0'
024500010711     C                   clear                   vinmsg
024600010711     C                   endif
024700010601     C*
024800010330     C                   exsr      inzvar
024900010605     C                   exsr      impvab
025000010601     C*
025100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025200010605     C                   eval      depfil = VABLNP
025300010601     C                   exsr      repfil
025400010601     C                   if        depfil = invfil
025500021025     C                   if        vlrpoi = 999
025600030822     C                   move(P)   invfil        VABFGS
025700021025     C                   else
025800030822     C                   move(P)   vlrpoi        VABFGS
025900021025     C                   endif
026000010601     C*
026100010601     C                   exsr      PREELA
026200010601     C*
026300010604     C* Ebbene...
026400010604     C*
026500030822     C  N31              add       1             §CTROK            7 0
026600030822     C   32              add       1             §CTRMO            7 0
026700030822     C   31              add       1             §CTRNO            7 0
026800030822     C                   if        wGiro = 2
026900041025     C                   if        *IN31 = *off
027000041025     C                   add       1             skJ
027100050103     C                   movel     VABRMN        wVABRMNalfa
027200050105     C                   eval      skWvab(skJ) = wVABRMNalfa + DSVAB
027300041025     C                   endif
027400030822     C                   endif
027500010604     C*
027600010604     C                   if        *in31 = *off and
027700010604     C                             *in32 = *off
027800010604     C                   eval      vinflg = '1'
027900010604     C                   else
028000010604     C                   eval      recko = vindta
028100010604     C                   except    rigadett
028200010604     C                   eval      vinflg = '2'
028300010604     C                   endif
028400010604     C                   endif
028500010601     C*
028600010604     C                   endif
028700010604     C*
028800010330     C                   else
028900010330     C                   eval      vinflg = '1'
029000010330     C                   endif
029100010601     C*
029200010601     C  N70              update    tivin000
029300010330     C*
029400030822     C  N70              enddo
029500041025     C*
029600041025     C* Scarico la schiera delle bolle in precedenza tradotte accorpando x medesimo RMN
029700041025     C* e sommando pesi e colli
029800041025     C                   exsr      ACCORPBOL
029900010601     C*
030000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030100010601     C                   if        cntNonEl = *zeros or
030200010601     C                             flgMulti = '0'
030300050103     C* Se non ci sono record con errori...
030400010601     C                   if        §ctrno = 0 and
030500010604     C                             §ctrmo = 0 and
030600010601     C                             flgStato <> '2'
030700010330     C* ... restituisco esito OK.
030800010330     C                   eval      wrkesito = '0'
030900010330     C                   else
031000010330     C                   if        §ctrok > 0
031100010330     C                   eval      wrkesito = '1'
031200010330     C                   else
031300010615     C                   if        flgOk = '0'
031400010615     C                   eval      wrkesito = '2'
031500010615     C                   else
031600010615     C                   eval      wrkesito = '6'
031700010615     C                   endif
031800010330     C                   endif
031900010330     C                   endif
032000010601     C                   else
032100010601     C                   eval      wrkesito = '9'
032200010601     C                   endif
032300010330     C*
032400010330     C                   if        %open(tivin00r)
032500010330     C                   close     tivin00r
032600010330     C                   endif
032700021025     C                   if        %open(fivabwwr)
032800021025     C                   close     fivabwwr
032900010330     C                   endif
033000010601     C*
033100010601     C                   if        vlrpoi <> 999
033200010601     C                   eval      invfil = vlrpoi
033300010601     C                   endif
033400010330     C*
033500010330     C                   if        §ctrok > 0
033600010601     C                             and invfil > *zeros
033700010330     C                   exsr      invio
033800010330     C                   endif
033900010601     C*
034000010618     C                   if        flgGiro = '1'
034100010601     C                   exsr      endela
034200010618     C                   endif
034300010330     C*
034400010330     C                   ENDSR
034500010330     C***
034600041025
034700041025
034800041025
034900041025
035000041025     C*-----------------------------------------------------------*
035100041025     C* ACCORPBOL - OPERAZIONI DI ACCORPAMENTO E SCRITTURA BOLLE  *
035200041025     C*-----------------------------------------------------------*
035300050105     C     ACCORPBOL     BEGSR
035400041025     C*
035500041025     C* Innanzitutto ordino la schiera d wrk
035600041025     C                   sorta     skWvab
035700041025     C*
035800041025     C* Inizializzo campi d wrk e d deposito
035900041025     C                   z-add     *zeros        wNCL             10 0
036000050105     C                   z-add     *zeros        wPKB             13 1
036100050105     C                   z-add     *zeros        wNumDDT          10 0
036200041025     C*
036300041025     C                   z-add     1             skJ
036400041025     C                   dow       skJ <= %elem(skWvab)
036500041025     C                   if        skWvab(skJ) <> *blanks
036600050105     C* Porto i dati dall'elemento d schiera al buffer del FIVAB
036700050105     C                   eval      DSVAB = %subst(skWvab(skJ):16:486)
036800050105     C* Incremento il contatore dei DDT accorpati
036900050105     C                   add       1             wNumDDT
037000041025     C* Se NN è l'ultima bolla
037100050103     C                   if        skJ < %elem(skWvab)
037200041025     C* X stesso RMN => accorpo
037300041025     C                   if        %subst(skWvab(skJ):1:15)  =
037400041025     C                             %subst(skWvab(skJ+1):1:15)
037500050103     C                   eval      wNCL = wNCL + VABNCL
037600050105     C                   eval      wPKB = wPKB + VABPKB
037700041025     C                   else
037800050103     C                   eval      VABNCL = wNCL + VABNCL
037900050105     C                   eval      VABPKB = wPKB + VABPKB
038000050111     C                   eval      VABRMO = 'N° DDT: ' +
038100050105     C                                      %trim(%editc(wNumDDT:'4'))
038200041025     C                   write     FIVAB000
038300041025     C                   z-add     *zeros        wNCL
038400050105     C                   z-add     *zeros        wPKB
038500050105     C                   z-add     *zeros        wNumDDT
038600041025     C                   endif
038700041025     C                   else
038800041025     C* Se è l'ultima bolla => scarico direttamente il buffer
038900050103     C                   eval      VABNCL = wNCL + VABNCL
039000050105     C                   eval      VABPKB = wPKB + VABPKB
039100050105     C                   eval      VABNOT = 'N° DDT: ' +
039200050105     C                                      %trim(%editc(wNumDDT:'4'))
039300041025     C                   write     FIVAB000
039400041025     C                   endif
039500041025     C                   endif
039600041025     C                   add       1             skJ
039700041025     C                   enddo
039800041025     C*
039900041025     C                   ENDSR
040000041025     C***
040100010601
040200010601
040300010601
040400010330     C*----------------------------------------------------*
040500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040600010330     C*----------------------------------------------------*
040700010330     C     INZVAR        BEGSR
040800010330     C*
040900030822     C* Inizializzo variabili di wrk
041000020204     C                   Z-ADD     *zeros        Num5_0            5 0
041100030822     C*
041200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
041300030822     C                   CLEAR                   SkSplitCSV
041400030822     C                   EVAL      i = 1
041500030822     C                   EVAL      posDa = *zeros
041600030822     C                   EVAL      posA  = *zeros
041700030822     C*
041800030822     C* Reimposto i valori di default
041900030822     C                   EXSR      DEFCAM
042000030822     C*
042100010330     C                   ENDSR
042200010330     C*----------------------------------------------------*
042300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
042400010330     C*----------------------------------------------------*
042500010330     C     DEFCAM        BEGSR
042600030822     C*
042700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
042800030715     C* e delimitatore testo.
042900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
043000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
043100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
043200030822     C*
043300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
043400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
043500030715     C                   SETON                                        50
043600030715     C                   ELSE
043700030715     C                   SETOFF                                       50
043800030715     C                   ENDIF
043900030822     C*
044000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
044100030822     C                   EVAL      posDaDft = 1
044200030822     C                   EVAL      posADft  = 0
044300030822     C                   EVAL      wGiroDft = 0
044400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
044500030822     C                             posDaDft > 0
044600030822     C*
044700030822     C* Gestisco il 1° giro
044800030822     C                   IF        wGiroDft = 0
044900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
045000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
045100030822     C* Incremento il contatore dei "giri"
045200030822     C                   EVAL      wGiroDft = 1
045300030822     C                   ELSE
045400030822     C                   EVAL      posDaDft = posADft
045500030822     C                   ENDIF
045600030822     C* Eseguo lo scan x trovare la fine del campo corrente
045700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
045800030822     C*
045900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
046000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
046100030822     C                   IF        posDaDft > 0 AND
046200030822     C                             posADft  > 0
046300030822     C* NCL
046400030822     C                   IF        %subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):1:3)
046700030822     C                             = 'NCL'
046800030822     C                   EVAL      PiStr=%trim(%subst(
046900030822     C                             %subst(vlrppt:posDaDft+1:
047000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047100030822     C                   EXSR      CHKNUM
047200030822     C                   IF        PiInt=*on
047300041025     C                   Z-ADD     PiVal         VABNCL
047400030822     C                   ENDIF
047500030822     C                   ENDIF
047600030822     C* CCM
047700030822     C                   IF        %subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):1:3)
048000030822     C                             = 'CCM'
048100030822     C                   EVAL      PiStr=%trim(%subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048400030822     C                   EXSR      CHKNUM
048500030822     C                   IF        PiInt=*on
048600030822     C                   Z-ADD     PiVal         VABCCM
048700030822     C                   ENDIF
048800030822     C                   ENDIF
048900030822     C* LNP
049000030822     C                   IF        %subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):1:3)
049300030822     C                             = 'LNP'
049400030822     C                   EVAL      PiStr=%trim(%subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700030822     C                   EXSR      CHKNUM
049800030822     C                   IF        PiInt=*on
049900030822     C                   Z-ADD     PiVal         VABLNP
050000030822     C                   ENDIF
050100030822     C                   ENDIF
050200030822     C* NRS
050300030822     C                   IF        %subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):1:3)
050600030822     C                             = 'NRS'
050700030822     C                   EVAL      PiStr=%trim(%subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051000030822     C                   EXSR      CHKNUM
051100030822     C                   IF        PiInt=*on
051200030822     C                   Z-ADD     PiVal         VABNRS
051300030822     C                   ENDIF
051400030822     C                   ENDIF
051500030822     C* CTR
051600030822     C                   IF        %subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):1:3)
051900030822     C                             = 'CTR'
052000030822     C                   EVAL      PiStr=%trim(%subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052300030822     C                   EXSR      CHKNUM
052400030822     C                   IF        PiInt=*on
052500030822     C                   Z-ADD     PiVal         VABCTR
052600030822     C                   ENDIF
052700030822     C                   ENDIF
052800030822     C* PKB
052900030822     C                   IF        %subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):1:3)
053200030822     C                             = 'PKB'
053300030822     C                   EVAL      PiStr=%trim(%subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053600030822     C                   EXSR      CHKNUM
053700030822     C                   IF        PiNum=*on
053800030822     C                   Z-ADD     PiVal         VABPKB
053900030822     C                   ENDIF
054000030822     C                   ENDIF
054100030822     C* VLB
054200030822     C                   IF        %subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):1:3)
054500030822     C                             = 'VLB'
054600030822     C                   EVAL      PiStr=%trim(%subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054900030822     C                   EXSR      CHKNUM
055000030822     C                   IF        PiNum=*on
055100030822     C                   Z-ADD     PiVal         VABVLB
055200030822     C                   ENDIF
055300030822     C                   ENDIF
055400030822     C* QFT
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'QFT'
055900030822     C                   EVAL      PiStr=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   EXSR      CHKNUM
056300030822     C                   IF        PiNum=*on
056400030822     C                   Z-ADD     PiVal         VABQFT
056500030822     C                   ENDIF
056600030822     C                   ENDIF
056700030822     C* CBO
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'CBO'
057200030822     C                   EVAL      VABCBO=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   ENDIF
057600030822     C* TSP
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'TSP'
058100030822     C                   EVAL      VABTSP=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   ENDIF
058500030822     C* VAS
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'VAS'
059000030822     C                   EVAL      VABVAS=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   ENDIF
059400030822     C* VCA
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'VCA'
059900030822     C                   EVAL      VABVCA=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   ENDIF
060300030822     C* GCA
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'GCA'
060800030822     C                   EVAL      VABGCA=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* CTM
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'CTM'
061700030822     C                   EVAL      VABCTM=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* FFD
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'FFD'
062600030822     C                   EVAL      VABFFD=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* VAD
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'VAD'
063500030822     C                   EVAL      VABVAD=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* GMA
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'GMA'
064400030822     C                   EVAL      VABGMA=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* GGA
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'GGA'
065300030822     C                   EVAL      VABGGA=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* GVA
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'GVA'
066200030822     C                   EVAL      VABGVA=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* TC1
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'TC1'
067100030822     C                   EVAL      VABTC1=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* TC2
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'TC2'
068000030822     C                   EVAL      VABTC2=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400050104     C* RMO
068500050104     C                   IF        %subst(
068600050104     C                             %subst(vlrppt:posDaDft+1:
068700050104     C                             posADft-posDaDft-1):1:3)
068800050104     C                             = 'RMO'
068900050104     C                   EVAL      VABRMO=%trim(%subst(
069000050104     C                             %subst(vlrppt:posDaDft+1:
069100050104     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200050104     C                   ENDIF
069300030822     C* ...
069400030822     C                   ENDIF
069500030822     C                   ENDDO
069600020204     C*
069700010330     C                   ENDSR
069800010607     C*----------------------------------------------------*
069900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070000010607     C*----------------------------------------------------*
070100010607     C     IMPVAB        BEGSR
070200010607     C*
070300010607     C                   SETOFF                                       3132
070400030515     C*
070500010607     C                   MOVEL     datcor        VABAAS
070600010607     C                   MOVE      datcor        VABMGS
070700010607     C*
070800030715     C**********
070900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071000030715     C**********
071100030715     C                   IF        wGiro = *zeros
071200040726     C*
071300040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
071400050105     C  N12              EVAL      depvinDTA = vinDTA
071500040726     C*
071600041025     C* Se richiesta inserisco qui la forzatura della intesatazione colonne personalizzata
071700050103     C                   IF        intcol(1)+intcol(2)+intcol(3) <> *blanks
071800040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
071900040726     C                                      %trim(intcol(3))
072000041025     C                   ENDIF
072100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
072200031201     C     minu:maiu     XLATE     vindta        vindta
072300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
072400040324     C                   DOW       posDa <= %len(%trimr(vindta))
072500030715     C*
072600030715     C* Gestisco il 1° campo
072700030715     C                   IF        i = 1
072800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
072900030715     C                   EVAL      posDa = 1
073000030715     C* Eseguo lo scan x trovare la fine del primo campo
073100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073200030715     C* A questo punto "estraggo" il campo corrente
073300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073400030715     C                                                    (posA-posDa))
073500030715     C* X i campi successivi al 1°
073600030715     C                   ELSE
073700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
073800030715     C                   EVAL      posDa = posA + 1
073900030715     C* Eseguo lo scan x trovare la fine del campo corrente
074000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074100030715     C* Gestisco l'ultimo campo
074200030715     C                   IF        posA = *zeros
074300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
074400030715     C* A questo punto "estraggo" il campo corrente
074500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074600030715     C                                                    (posA-posDa))
074700030715     C                   LEAVE
074800030715     C                   ELSE
074900030715     C* A questo punto "estraggo" il campo corrente
075000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075100030715     C                                                    (posA-posDa))
075200030715     C                   ENDIF
075300030715     C                   ENDIF
075400030715     C* Incremento il contatore d campo
075500030715     C                   EVAL      i = i +1
075600030715     C                   ENDDO
075700030715     C*
075800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
075900030715     C* delimitazione testo
076000030715     C                   EVAL      i = 1
076100030715     C                   DOW       i < %elem(SkSplitFLD)
076200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
076300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
076400030715     C                   EVAL      i = i + 1
076500030715     C                   ENDDO
076600030715     C                   EVAL      wGiro = 1
076700040726     C* Ripristino la 1° riga originale
076800050105     C  N12              EVAL      vinDTA = depvinDTA
076900030715     C                   ELSE
077000030715     C                   EVAL      wGiro = 2
077100031204     C**********
077200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
077300031204     C* d separatore campo
077400031204     C**********
077500040726     C                   EVAL      vindta = %trim(vindta)
077600040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
077700040726     C                   EVAL      vindta = %subst(vindta:2)
077800040726     C                   ENDDO
077900031204     C*
078000031204     C                   Z-ADD     *zeros        lunghInput        4 0
078100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
078200040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
078300040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
078400040726     C                   EVAL      lunghInput = %len(%trim(vindta))
078500040726     C                   ENDDO
078600030715     C**********
078700030715     C* Effettuo lo split del campo dati d input
078800030715     C**********
078900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079000040324     C                   DOW       posDa <= %len(%trimr(vindta))
079100030715     C*
079200030715     C* Gestisco il 1° campo
079300030715     C                   IF        i = 1
079400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079500030715     C                   EVAL      posDa = 1
079600030715     C* Eseguo lo scan x trovare la fine del primo campo
079700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079800030715     C* A questo punto "estraggo" il campo corrente
079900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080000030715     C                                                    (posA-posDa))
080100030715     C* X i campi successivi al 1°
080200030715     C                   ELSE
080300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080400030715     C                   EVAL      posDa = posA + 1
080500031204     C* Verifico che nn vi sia il campo nn valorizzato
080600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
080700031204     C* Se campo nn valorizzato skippo al prossimo
080800031204     C                   EVAL      SkSplitCSV(i) = *blanks
080900031204     C                   EVAL      posA  = posA + 1
081000031204     C                   ELSE
081100030715     C* Eseguo lo scan x trovare la fine del campo corrente
081200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081300030715     C* Gestisco l'ultimo campo
081400030715     C                   IF        posA = *zeros
081500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
081600030715     C* A questo punto "estraggo" il campo corrente
081700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081800030715     C                                                    (posA-posDa))
081900030715     C                   LEAVE
082000030715     C                   ELSE
082100030715     C* A questo punto "estraggo" il campo corrente
082200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082300030715     C                                                    (posA-posDa))
082400030715     C                   ENDIF
082500031204     C                   ENDIF
082600030715     C                   ENDIF
082700030715     C* Incremento il contatore d campo
082800030715     C                   EVAL      i = i +1
082900030715     C                   ENDDO
083000030715     C*
083100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083200030715     C* delimitazione testo
083300030715     C                   EVAL      i = 1
083400030715     C                   DOW       i < %elem(SkSplitCSV)
083500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
083600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
083700030715     C                   EVAL      i = i + 1
083800030715     C                   ENDDO
083900030715     C*
084000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
084100030715     C                   EVAL      i = 1
084200030715     C                   DOW       i < %elem(SkSplitFLD)
084300030715     C*
084400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
084500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
084600030715     C                             SkSplitCSV(i) <> *blanks
084700030721     C*** ATB
084800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
084900030721     C                   EVAL      VABATB = SkSplitCSV(i)
085000030721     C                   ENDIF
085100030715     C*** RSD
085200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
085300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
085400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
085500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
085600030715     C* ==
085700030715     C                   ENDIF
085800030721     C*** RD2
085900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
086000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
086100030721     C                   ENDIF
086200030715     C*** IND
086300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
086400030715     C                   EVAL      VABIND = SkSplitCSV(i)
086500030715     C                   ENDIF
086600030715     C*** LOD
086700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
086800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
086900030715     C                   ENDIF
087000030715     C*** PRD
087100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
087200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
087300030715     C                   ENDIF
087400030721     C*** NZD
087500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
087600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
087700030721     C                   ENDIF
087800030715     C*** RMA
087900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
088000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
088100030715     C                   ENDIF
088200030715     C*** NOT
088300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
088400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
088500030715     C                   ENDIF
088600030715     C*** NT2
088700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
088800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
088900030715     C                   ENDIF
089000030715     C*** VCA
089100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
089200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
089300030715     C                   ENDIF
089400030715     C*** VAS
089500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
089600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
089700030715     C                   ENDIF
089800030715     C*** FFD
089900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
090000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
090100030715     C                   ENDIF
090200030715     C*** CBO
090300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
090400030715     C                   EVAL      VABCBO = SkSplitCSV(i)
090500030715     C                   ENDIF
090600030715     C
090700030721     C*** GC1
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
090900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** GC2
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
091300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** TSP
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
091700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** NAS
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
092100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** TIC
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
092500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** GCA
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
092900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** XCO
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
093300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** CTM
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
093700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** TCR
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
094100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** CTS
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
094500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** FTM
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
094900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** VAD
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
095300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** GMA
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
095700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** GGA
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
096100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** GVA
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
096500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** TC1
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
096900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030721     C*** TC2
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
097300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** SCL
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
097700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** RMO
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
098100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** NMO
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
098500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*
098800030721     C* Reperisco quindi i campi numerici...
098900030721     C*** CAD
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
099100030721     C                   EVAL      PiStr=SkSplitCSV(i)
099200030721     C                   EXSR      CHKNUM
099300030721     C                   IF        PiInt=*on
099400030721     C                   Z-ADD     PiVal         Num5_0
099500030721     C                   MOVEL(p)  Num5_0        VABCAD
099600030721     C                   ELSE
099700030721     C                   SETON                                        32
099800030721     C                   EVAL      VABCAD = *zeros
099900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100000030721     C                             + ' ' + 'VABCAD'
100100030721     C                   ENDIF
100200030721     C                   ENDIF
100300050105     C*** RMN
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
100500050105     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):4)
100600030721     C                   EXSR      CHKNUM
100700030721     C                   IF        PiInt=*on
100800030721     C                   Z-ADD     PiVal         VABRMN
100900050105     C                   Z-ADD     PiVal         VABNSP
101000030721     C                   ELSE
101100041025     C                   SETON                                        31
101200041025     C                   EVAL      VABRMN = *zeros
101300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101400050105     C                             + ' ' + 'VABRMN VABNSP'
101500030721     C                   ENDIF
101600030721     C                   ENDIF
101700050105     C*** NSP
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101900030721     C                   EVAL      PiStr=SkSplitCSV(i)
102000030721     C                   EXSR      CHKNUM
102100040726     C                   IF        PiInt=*on
102200040726     C                   Z-ADD     PiVal         VABNSP
102300030721     C                   ELSE
102400030721     C                   SETON                                        32
102500030721     C                   EVAL      VABNSP = *zeros
102600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102700050105     C                             + ' ' + 'VABNSP'
102800030721     C                   ENDIF
102900030721     C                   ENDIF
103000030721     C*** NCL
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
103200030721     C                   EVAL      PiStr=SkSplitCSV(i)
103300030721     C                   EXSR      CHKNUM
103400030721     C                   IF        PiInt=*on
103500041025     C  N11              Z-ADD     PiVal         VABNCL
103600041025     C   11              ADD       PiVal         VABNCL
103700030721     C                   ELSE
103800030721     C                   SETON                                        32
103900030721     C                   EVAL      VABNCL = *zeros
104000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104100050105     C                             + ' ' + 'VABNCL'
104200030721     C                   ENDIF
104300030721     C                   ENDIF
104400050105     C*** PKB
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
104600030721     C                   EVAL      PiStr=SkSplitCSV(i)
104700030721     C                   EXSR      CHKNUM
104800030721     C                   IF        PiNum=*on
104900041025     C  N11              Z-ADD     PiVal         VABPKB
105000041025     C   11              ADD       PiVal         VABPKB
105100030721     C                   ELSE
105200030721     C                   SETON                                        32
105300030721     C                   EVAL      VABPKB = *zeros
105400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105500030721     C                             + ' ' + 'VABPKB'
105600030721     C                   ENDIF
105700030721     C                   ENDIF
105800030721     C*** CAS
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
106000030721     C                   EVAL      PiStr=SkSplitCSV(i)
106100030721     C                   EXSR      CHKNUM
106200030721     C                   IF        PiNum=*on
106300030721     C                   Z-ADD     PiVal         VABCAS
106400030721     C                   ELSE
106500030721     C                   SETON                                        32
106600030721     C                   EVAL      VABCAS = *zeros
106700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106800030721     C                             + ' ' + 'VABCAS'
106900030721     C                   ENDIF
107000030721     C                   ENDIF
107100030721     C*** CCM
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
107300030721     C                   EVAL      PiStr=SkSplitCSV(i)
107400030721     C                   EXSR      CHKNUM
107500030721     C                   IF        PiInt=*on      AND
107600030721     C                             PiVal<=9999999 AND
107700030721     C                             PiVal>*zeros
107800030721     C                   Z-ADD     PiVal         VABCCM
107900030721     C                   ELSE
108000030721     C                   SETON                                        32
108100030721     C                   EVAL      VABCCM = *zeros
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300050105     C                             + ' ' + 'VABCCM'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** LNP
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiInt=*on    AND
109100030721     C                             PiVal<=999   AND
109200030721     C                             PiVal>*zeros
109300030721     C                   Z-ADD     PiVal         VABLNP
109400030721     C                   ELSE
109500030721     C                   SETON                                        32
109600030721     C                   EVAL      VABLNP = *zeros
109700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109800050105     C                             + ' ' + 'VABLNP'
109900030721     C                   ENDIF
110000030721     C                   ENDIF
110100030721     C*** NRS
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
110300030721     C                   EVAL      PiStr=SkSplitCSV(i)
110400030721     C                   EXSR      CHKNUM
110500030721     C                   IF        PiInt=*on AND
110600030721     C                             PiVal<=99
110700030721     C                   Z-ADD     PiVal         VABNRS
110800030721     C                   ELSE
110900030721     C                   SETON                                        32
111000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111100050105     C                             + ' ' + 'VABNRS'
111200030721     C                   ENDIF
111300030721     C                   ENDIF
111400030721     C*** LNA
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiInt=*on    AND
111900030721     C                             PiVal<=999   AND
112000030721     C                             PiVal>*zeros
112100030721     C                   Z-ADD     PiVal         VABLNA
112200030721     C                   ELSE
112300030721     C                   SETON                                        32
112400030721     C                   EVAL      VABLNA = *zeros
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600030721     C                             + ' ' + 'VABLNA'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** CTR
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiInt=*on  AND
113400030721     C                             PiVal<=999
113500030721     C                   Z-ADD     PiVal         VABCTR
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABCTR'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** IAS
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiNum=*on
114700030721     C                   Z-ADD     PiVal         VABIAS
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABIAS'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** VLB
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiNum=*on
115900030721     C                   Z-ADD     PiVal         VABVLB
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABVLB'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** QFT
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiNum=*on
117100030721     C                   Z-ADD     PiVal         VABQFT
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500030721     C                             + ' ' + 'VABQFT'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** NCD
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiInt=*on      AND
118300030721     C                             PiVal<=9999999
118400030721     C                   Z-ADD     PiVal         VABNCD
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABNCD'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** NCA
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on      AND
119600030721     C                             PiVal<=9999999
119700030721     C                   Z-ADD     PiVal         VABNCA
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABNCA'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** ZNC
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on AND
120900030721     C                             PiVal<=99
121000030721     C                   Z-ADD     PiVal         VABZNC
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABZNC'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** DCR
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
121900050103     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):7:4)+
122000050103     C                                   %subst(%trim(SkSplitCSV(i)):4:2)+
122100050103     C                                   %subst(%trim(SkSplitCSV(i)):1:2)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on       AND
122400030721     C                             PiVal<=99999999
122500030721     C                   Z-ADD     PiVal         VABDCR
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABDCR'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** HCR
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on   AND
123700030721     C                             PiVal<=9999
123800030721     C                   Z-ADD     PiVal         VABHCR
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABHCR'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** VMD
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiNum=*on
125000030721     C                   Z-ADD     PiVal         VABVMD
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABVMD'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** ANT
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on        AND
126200030721     C                             PiVal<=999999999
126300030721     C                   Z-ADD     PiVal         VABANT
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABANT'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** CMO
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on
127500030721     C                   Z-ADD     PiVal         Num5_0
127600030721     C                   MOVEL(p)  Num5_0        VABCMO
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      VABCMO = *zeros
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABCMO'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030715     C*
128500030715     C                   ENDIF
128600030715     C                   EVAL      i = i + 1
128700030715     C                   ENDDO
128800040726     C*
128900040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
129000040726     C                   IF        VABLNP = *zeros
129100040726     C                   SETON                                        31
129200040726     C                   ENDIF
129300040726     C*
129400040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
129500040726     C* d valorizzarli sempre entrambi
129600040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
129700040726     C                   Z-ADD     VABNSP        VABRMN
129800040726     C                   ENDIF
129900040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
130000040726     C                   MOVEL     VABRMN        VABRMA
130100040726     C                   ENDIF
130200040726     C*
130300040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
130400040726     C                   IF        VABCAS > *zeros
130500040726     C                   IF        VABVCA = *blanks
130600040726     C                   EVAL      VABVCA = 'EUR'
130700040726     C                   ENDIF
130800040726     C                   IF        VABCBO = '1'
130900040726     C                   EVAL      VABCBO = '4'
131000040726     C                   ENDIF
131100040726     C                   IF        VABCBO = '2'
131200040726     C                   EVAL      VABCBO = '6'
131300040726     C                   ENDIF
131400040726     C                   ENDIF
131500020204     C*
131600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
131700020204     C                   EXSR      CHKIMPDIV
131800030715     C                   ENDIF
131900020204     C*
132000010607     C                   ENDSR
132100010607     C*----------------------------------------------------*
132200010601
132300020204
132400020204     C*----------------------------------------------------*
132500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
132600020204     C*----------------------------------------------------*
132700020204     C     CHKIMPDIV     BEGSR
132800020204     C*
132900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
133000020204     C                   Z-ADD     *zeros        wrkDec            9 9
133100020204     C*
133200020204     C* Come prima cosa effettuo considerazioni sulla divisa
133300020204     C                   IF        vabIAS > *zeros
133400020204     C                   IF        vabVAS <> 'EUR'
133500020204     C                   EVAL      vabVAS =  'ITL'
133600020204     C                   ENDIF
133700020204     C                   ENDIF
133800020204     C*
133900020204     C                   IF        vabCAS > *zeros
134000020204     C                   IF        vabVCA <> 'EUR'
134100020204     C                   EVAL      vabVCA =  'ITL'
134200020204     C                   ENDIF
134300020204     C                   ENDIF
134400020204     C*
134500020204     C                   IF        vabVMD > *zeros
134600020204     C                   IF        vabVAD <> 'EUR'
134700020204     C                   EVAL      vabVAD =  'ITL'
134800020204     C                   ENDIF
134900020204     C                   ENDIF
135000020204     C*
135100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
135200020204     C                   Z-ADD     vabIAS        wrkDec
135300020204     C                   IF        wrkDec > *zeros
135400020204     C                   IF        vabVAS = 'ITL'
135500020204     C                   EVAL      vabIAS = *zeros
135600020204     C                   ENDIF
135700020204     C                   ENDIF
135800020204     C*
135900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
136000020204     C                   Z-ADD     vabCAS        wrkDec
136100020204     C                   IF        wrkDec > *zeros
136200020204     C                   IF        vabVCA = 'ITL'
136300020204     C                   EVAL      vabCAS = *zeros
136400020204     C                   ENDIF
136500020204     C                   ENDIF
136600020204     C*
136700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
136800020204     C                   Z-ADD     vabVMD        wrkDec
136900020204     C                   IF        wrkDec > *zeros
137000020204     C                   IF        vabVAD = 'ITL'
137100020204     C                   EVAL      vabVMD = *zeros
137200020204     C                   ENDIF
137300020204     C                   ENDIF
137400020204     C*
137500020204     C                   ENDSR
137600020204     C***
137700020204
137800010330
137900010330
138000010330     C*----------------------------------------------------*
138100010330     C*  CONTROLLO NUMERICITA' CAMPI
138200010330     C*----------------------------------------------------*
138300010330     C     CHKNUM        BEGSR
138400010330     C*
138500010606     C                   IF        PiDecChr = *blanks
138600030715     C                   EVAL      PiDecChr = CharNUM
138700010606     C                   ENDIF
138800010606     C*
138900010606     C                   CALL(e)   'ISNUMERIC'
139000010330     C                   PARM                    PiStr            30
139100010606     C                   PARM                    PiDecChr          1
139200010330     C                   PARM      *ZEROS        PiVal            30 9
139300010330     C                   PARM      '0'           PiInt             1
139400010330     C                   PARM      '0'           PiNum             1
139500010330     C                   IF        %error
139600010606     C                   EVAL      PiNum=*off
139700010330     C                   ENDIF
139800010330     C*
139900010330     C                   ENDSR
140000010330     C***
140100010330
140200010601
140300010601
140400010601
140500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
140600010601     C     repfil        BEGSR
140700010601     C*
140800010601     C                   if        invfil = *zeros and
140900010601     C                             depfil > *zeros and
141000010629     C                             (vinflg = *blanks or
141100010629     C                              vinflg = *zeros)
141200010601     C
141300010601     C                   eval      invfil = depfil
141400010601     C                   endif
141500010601     C*
141600010601     C                   if        depfil <> invfil and
141700010601     C                             invfil > *zeros
141800010601     C                   eval      flgMulti = '1'
141900010601     C                   if        vinflg = *blanks
142000010601     C                   add       1             cntNonEl
142100010601     C                   endif
142200010601     C                   endif
142300010601     C*
142400010601     C                   if        vinflg = '2'
142500010601     C                   eval      flgStato = '2'
142600010601     C                   endif
142700010601     C*
142800010601     C                   ENDSR
142900010601     C***
143000010601
143100010601
143200050103
143300050103      /TITLE Invio dei dati al punto operativo.
143400050103     C     invio         BEGSR
143500050103     C*
143600050105     C* 1° invio FIVAB
143700050103     C                   reset                   dscmz
143800050103     C                   move      vlrpoi        cmzdst
143900050103     C                   eval      cmzfld = vlrfou
144000050103     C                   eval      cmzmbd = vlrhdl
144100050103     C                   eval      %subst(cmzmbd:1:1) = 'M'
144200050103     C***                if        prmfir = *blanks
144300050103     C                   eval      cmzfla = 'FIVAB00F'
144400050103     C                   eval      cmzmba = 'FIVAB00F'
144500050103     C***                else
144600050103     C***                eval      cmzfla = prmfir
144700050103     C***                eval      cmzmba = prmfir
144800050103     C***                endif
144900050103     C                   eval      cmznrr = *zeros
145000050103     C                   move      §ctrok        cmznrr
145100050103     C                   eval      cmzlba = vlrfl1
145200050103     C                   call(e)   'TIS711C'
145300050103     C                   parm                    dscmz
145400050103     C                   parm      *blanks       esito
145500050103     C                   if        %error
145600050103     C                             or cmzerr = '1'
145700050103     C                             or esito  = '1'
145800050103     C                   eval      wrkesito = '3'
145900050103     C                   endif
146000050103     C*
146100050103     C                   ENDSR
146200050103     C***
146300010601
146400010601
146500010601
146600010601      /TITLE Invio dei dati al punto operativo.
146700010601     C     opeini        BEGSR
146800010601     C*
146900010601     C* Inizializzo flag e contatori operativi
147000010601     C                   movel     '0'           flgGiro           1
147100010601     C                   movel     '0'           flgMulti          1
147200010601     C                   movel     '1'           flgStato          1
147300010615     C                   movel     '0'           flgOk             1
147400010601     C                   z-add     *zeros        cntNonEl         10 0
147500010601     C                   z-add     *zeros        depfil            3 0
147600010601     C                   z-add     *zeros        invfil            3 0
147700010601     C*
147800010601     C                   ENDSR
147900010601     C***
148000010601
148100050421
148200050421
148300050421     C     *pssr         BEGSR
148400050421     C*
148500050421     C                   eval      wrkesito = '2'
148600050421     C*
148700050421     C                   ENDSR     '*CANCL'
148800050421     C***
148900050421
149000010330
149100010330
149200000613     C     *inzsr        BEGSR
149300990910     C*
149400990910     C     *entry        plist
149500990920     C                   parm                    tivlrds
149600990921     C                   parm      wrkesito      esito
149700000724     C                   parm                    prmlit
149800000710     C                   parm                    prmfir
149900010330     C*
150000010330     C* CALCOLA LA DATA CORRENTE
150100010330     C                   time                    wn14             14 0
150200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
150300010330     C                   z-add     wn8           g08dat
150400010330     C                   z-add     *zeros        g08inv
150500010330     C                   movel     '0'           g08err
150600010330     C                   call      'XSRDA8'
150700010330     C                   parm                    wlbda8
150800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
150900000613     C*
151000050105     C* Gestisco alcuni indicatori che influenzano il traduttore a seconda delle particolarità
151100050105     C* del file attuale da tradurre
151200050105     C* - se occorre sommmare direttamente in traduzione i campi quali colli e peso => *IN11 = *ON
151300050105     C                   setoff                                       11
151400050105     C* - se nel file è assente la prima riga con le instestazioni => *IN12 = *ON
151500050105     C                   seton                                        12
151600050105     C*
151700000613     C                   ENDSR
151800000613     C***
151900990908
152000030715     Otitvxlp   E            riepilogo         2
152100990915     O                                              'Upload via Internet'
152200990915     O                                           +1 'Traduzione TIVIN00R -'
152300021025     O                                           +1 'FIVABWWR'
152400990915     O                                           +1 'Testate bolle'
152500990915     O          E            riepilogo   2
152600990915     O                       wrkdata
152700990915     O                       wrkora              +1
152800990915     O                       procname            +1
152900990915     O          E            riepilogo   2
153000990915     O                                              'Cliente..................:'
153100010605     O                       VABCCM        z     +1
153200990915     O          E            riepilogo   2
153300990920     O                                              'Riferimento Strategi.....:'
153400990920     O                       vlrhdl              +1
153500990915     O          E            riepilogo   2
153600990915     O                                              'Giusti...................:'
153700971022     O                       §CTROK        2   +  1
153800990915     O          E            riepilogo   2
153900990915     O                                              'Sbagliati e corretti.....:'
154000971022     O                       §CTRMO        2   +  1
154100990915     O          E            riepilogo   2
154200990915     O                                              'Sbagliati e scartati.....:'
154300971022     O                       §CTRNO        2   +  1
154400000613
154500030715     Otitvxlps  E            testdett          2
154600000613     O                                              'Upload via Internet'
154700000613     O                                           +1 'Traduzione TIVIN00R -'
154800021025     O                                           +1 'FIVABWWR'
154900000613     O                                           +1 'Testate bolle'
155000000616     O          E            testdett    3
155100000613     O                                           +2 'N° rec'
155200000613     O                                           +3 'Anteprima contenuto'
155300000616     O          E            rigadett    2
155400000613     O                       rrnum               +2
155500000621     O                       recko               +3
155600000616     O          E            findett     2
155700000613     O                       wrkdata
155800000613     O                       wrkora              +1
155900000613     O                       procname            +1
156000000616     O          E            findett     2
156100000613     O                                              'Cliente..................:'
156200010605     O                       VABCCM        z     +1
156300000616     O          E            findett     2
156400000613     O                                              'Riferimento Strategi.....:'
156500000613     O                       vlrhdl              +1
156600000616     O          E            findett     2
156700000613     O                                              'Giusti...................:'
156800000613     O                       §CTROK        2   +  1
156900000616     O          E            findett     2
157000000613     O                                              'Sbagliati e corretti.....:'
157100000613     O                       §CTRMO        2   +  1
157200000616     O          E            findett     2
157300000613     O                                              'Sbagliati e scartati.....:'
157400000613     O                       §CTRNO        2   +  1
157500000613** CMD - COMANDI CL
157600030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
157700030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
157800030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
157900010606
158000000613
158100040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
158200050105VABRSD;VABIND;VABLOD;VABCAD;VABPRD;VABRMA;FILLER1;VABNCL;FILLER2;VABPKB;FILLER3;
158300050105FILLER4;FILLER5;FILLER6;VABRMN
158400040726
