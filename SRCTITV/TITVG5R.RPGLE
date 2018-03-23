000100050105      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente MASTER SPA (CMS) - Cod. 2100711)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700050103     FFIVADwwr  O    E             DISK    usropn
000800030715     Ftitvxlp   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000030715     Ftitvxlps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700040726     D*------------
001800040726     D* SCHIEDE A PROGRAMMA
001900040726     D*------------
002000040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002100050103     D skWvab          S            516    DIM(20000) INZ(*blanks)
002200010330     D*----------------------------------------------------
002300010330     D* DICHIARAZIOINE VARIABILI DI WRK
002400010330     D*----------------------------------------------------
002500010330     D dscmz         e ds                  inz
002600010330     D psds           sds
002700010330     D  procname         *PROC
002800010330     D tivlrds       e ds                  extname(tivlr00f)
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D wrkdata         s               d
003400010330     D wrkora          s               t
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600010330     D recko           s            150    INZ(*blanks)
003700011113     D depcmd          s            150    INZ(*blanks)
003800040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003900041025     D DSVAB         e ds                  EXTNAME(FIVAB00F) INZ
004000041025     D skJ             S              5  0 INZ(*zeros)
004100050103     D parccm          s              8    INZ(*blanks)
004200050103     D parmbr          s             10    INZ(*blanks)
004300050103     D paropz          s              1    INZ(*blanks)
004400050103     D chkcall         s              1    INZ(*blanks)
004500050103     D wVABRMNalfa     s             15    INZ(*blanks)
004600030715     D*------------------
004700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004800030715     D*------------------
004900030715     D SkSplitFLD      S             10    DIM(100)
005000030715     D SkSplitCSV      S            256    DIM(100)
005100030715     D CharCSV         S              1
005200030715     D CharTXT         S              1
005300030715     D CharNUM         S              1
005400030715     D posDa           S              3  0 INZ(*zeros)
005500030715     D posA            S              3  0 INZ(*zeros)
005600030715     D i               s              3  0 INZ(1)
005700030715     D wGiro           s              1  0 INZ(*zeros)
005800030822     D*------------------
005900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006000030822     D*------------------
006100030822     D posDaDft        S              3  0 INZ(*zeros)
006200030822     D posADft         S              3  0 INZ(*zeros)
006300030822     D j               s              3  0 INZ(1)
006400030822     D wGiroDft        s              1  0 INZ(*zeros)
006500010330     D*------------------
006600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006700010330     D*------------------
006800010330     D WLBDA8          DS                  INZ
006900010330     D  G08DAT                 1      8  0
007000010330     D  G08INV                 9     16  0
007100010330     D  G08ERR                17     17
007200010330     D  G08TGI                18     22  0
007300031201     D*------------------
007400031201     D* Costanti
007500031201     D*------------------
007600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007800010330
007900010330
008000010330
008100990915     C                   time                    wrkdata
008200990915     C                   time                    wrkora
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600010601     C*
008700010601     C                   exsr      opeini
008800010605     C                   exsr      rwvab
008900081006     C*
009000081006     C                   exsr      endela
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601
009700010601     C*--------------------------------------------------------
009800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009900010601     C*--------------------------------------------------------
010000010601     C     PREELA        BEGSR
010100010601     C*
010200010601     C* SE OCCORRE SPEDIRE IN FILIALE
010300010601     C                   if        invfil <> *zeros and
010400010601     C                             flgGiro = '0'
010500010601     C*
010600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010601     C                   eval      flgGiro = '1'
010800000724     C*
010900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100010601     C     invfil        chain     azorg01l
011200000724     C                   if        %found
011300000724     C                   movel(p)  CMD(1)        depcmd
011400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011500000724     C*
011600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011700011113     C                   Z-ADD     150           LENGH            15 5
011800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000724     C                   PARM                    depcmd
012000000724     C                   PARM                    LENGH
012100000724     C*
012200000724     C                   endif
012300000616     C*
012400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500000616     C                   movel(p)  CMD(2)        depcmd
012600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012700000616     C*
012800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012900011113     C                   Z-ADD     150           LENGH            15 5
013000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013100000616     C                   PARM                    depcmd
013200000616     C                   PARM                    LENGH
013300000613     C*
013400030715     C                   if        not %open(titvxlps)
013500030715     C                   open      titvxlps
013600030715     C                   except    testdett
013700000613     C                   endif
013800010601     C*
013900010601     C                   endif
014000010601     C*
014100010601     C                   ENDSR
014200010601     C***
014300010601
014400010601
014500010601
014600010601
014700010601     C*--------------------------------------------------------
014800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014900010601     C*--------------------------------------------------------
015000010601     C     ENDELA        BEGSR
015100990915     C*
015200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015300000613     C*
015400030715     C                   if        %open(titvxlps)
015500000616     C                   except    findett
015600030715     C                   close     titvxlps
015700000613     C                   endif
015800081006     C*
015900081006     C                   if        %open(tivin00r)
016000081006     C                   close     tivin00r
016100081006     C                   endif
016200081006     C                   if        %open(fivabwwr)
016300081006     C                   close     fivabwwr
016400081006     C                   endif
016500081006     C                   if        %open(fivadwwr)
016600081006     C                   close     fivadwwr
016700081006     C                   endif
016800000616     C*
016900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017000011113     C                   Z-ADD     150           LENGH            15 5
017100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017200000616     C                   PARM                    CMD(3)
017300000616     C                   PARM                    LENGH
017400081006     C*
017500081006     C* Effettuo la chiamata al CLLE preposto
017600081006     C                   call(e)   'TITVVDC'
017700081006     C                   parm                    parccm
017800081006     C                   parm                    parmbr
017900081006     C                   parm      '2'           paropz
018000000616     C*
018100010601     C                   ENDSR
018200010601     C***
018300010601
018400010601
018500010601
018600010601
018700000613
018800000613     C*--------------------------------------------------------
018900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019000000613     C*--------------------------------------------------------
019100000621     C     STPR          BEGSR
019200000613     C*
019300030715     C                   if        not %open(titvxlp)
019400030715     C   50              open      titvxlp
019500990915     C                   endif
019600990915     C*
019700030715     C   50              except    riepilogo
019800990915     C*
019900030715     C                   if        %open(titvxlp)
020000030715     C   50              close     titvxlp
020100990914     C                   endif
020200000613     C*
020300000613     C                   ENDSR
020400000613     C***
020500000613
020600010330
020700010601
020800010601
020900010601
021000010330     C*--------------------------------------------------------
021100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021200010330     C*--------------------------------------------------------
021300010605     C     RWVAB         BEGSR
021400010330     C*
021500010330     C                   if        not %open(tivin00r)
021600010330     C                   open      tivin00r
021700010330     C                   endif
021800021025     C                   if        not %open(fivabwwr)
021900021025     C                   open      fivabwwr
022000010330     C                   endif
022100050103     C*
022200050103     C* Eseguo operazioni di aggiunta nuovo membro in FIVADWWR
022300050103     C                   exsr      prevad
022400050103     C*
022500050103     C                   if        chkcall = '0'
022600050103     C*
022700050103     C                   if        not %open(fivadwwr)
022800050103     C                   open      fivadwwr
022900050103     C                   endif
023000010330     C*
023100010604     C                   clear                   §CTROK
023200050103     C                   clear                   §CTROKVD
023300010604     C                   clear                   §CTRMO
023400010604     C                   clear                   §CTRNO
023500041025     C*
023600050103     C* Spengo l'indicatore che indica se in fase d traduzione record i valori d peso e volume
023700050103     C* devono essere sommati ai precedenti valori
023800041025     C                   CLEAR                   FIVAB000
023900041025     C                   setoff                                       11
024000010330     C*
024100030822     C                   do        *HIVAL
024200010330     C*
024300030822     C                   read      tivin00r                               70
024400010618     C*
024500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024600010618     C                   if        vinflg = '1'
024700010618     C                   eval      flgOk = '1'
024800010618     C                   endif
024900010618     C*
025000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025100050103     C                   z-add     *zeros        wLenVINDTA        4 0
025200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
025300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
025400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
025500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
025600040510     C                   z-add     1             wFlgVINDTA
025700040510     C                   leave
025800040510     C                   endif
025900040510     C                   enddo
026000040510     C*
026100040218     C                   if        vindta > *blanks AND
026200040510     C                             wFlgVINDTA = 1
026300010330     C                   add       1             rrnum
026400010330     C*
026500010601     C                   if        *in70 = *off and
026600010330     C                             (vinflg = *blanks
026700010330     C                              or vinflg = '0'
026800010330     C                              or vinflg = '2')
026900010330     C*
027000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027100010711     C                   if        vinflg = *blanks or vinflg = '0'
027200010711     C                   clear                   vinmsg
027300010711     C                   endif
027400010601     C*
027500010330     C                   exsr      inzvar
027600010605     C                   exsr      impvab
027700010601     C*
027800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027900010605     C                   eval      depfil = VABLNP
028000010601     C                   exsr      repfil
028100010601     C                   if        depfil = invfil
028200021025     C                   if        vlrpoi = 999
028300030822     C                   move(P)   invfil        VABFGS
028400021025     C                   else
028500030822     C                   move(P)   vlrpoi        VABFGS
028600021025     C                   endif
028700010601     C*
028800010601     C                   exsr      PREELA
028900010601     C*
029000010604     C* Ebbene...
029100010604     C*
029200030822     C  N31              add       1             §CTROK            7 0
029300050103     C  N31              add       1             §CTROKVD          7 0
029400030822     C   32              add       1             §CTRMO            7 0
029500030822     C   31              add       1             §CTRNO            7 0
029600030822     C                   if        wGiro = 2
029700041025     C                   if        *IN31 = *off
029800041025     C                   add       1             skJ
029900050103     C                   movel     VABRMN        wVABRMNalfa
030000050103     C                   eval      skWvab(skJ) = wVABRMNalfa + DSVAB + VADCDP
030100041025     C                   endif
030200030822     C                   endif
030300010604     C*
030400010604     C                   if        *in31 = *off and
030500010604     C                             *in32 = *off
030600010604     C                   eval      vinflg = '1'
030700010604     C                   else
030800010604     C                   eval      recko = vindta
030900010604     C                   except    rigadett
031000010604     C                   eval      vinflg = '2'
031100010604     C                   endif
031200010604     C                   endif
031300010601     C*
031400010604     C                   endif
031500010604     C*
031600010330     C                   else
031700010330     C                   eval      vinflg = '1'
031800010330     C                   endif
031900010601     C*
032000010601     C  N70              update    tivin000
032100010330     C*
032200030822     C  N70              enddo
032300041025     C*
032400041025     C* Scarico la schiera delle bolle in precedenza tradotte accorpando x medesimo RMN
032500041025     C* e sommando pesi e colli
032600041025     C                   exsr      ACCORPBOL
032700050103     C*
032800050103     C                   endif
032900010601     C*
033000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033100010601     C                   if        cntNonEl = *zeros or
033200010601     C                             flgMulti = '0'
033300050103     C* Se non ci sono record con errori...
033400010601     C                   if        §ctrno = 0 and
033500010604     C                             §ctrmo = 0 and
033600010601     C                             flgStato <> '2'
033700010330     C* ... restituisco esito OK.
033800010330     C                   eval      wrkesito = '0'
033900010330     C                   else
034000010330     C                   if        §ctrok > 0
034100010330     C                   eval      wrkesito = '1'
034200010330     C                   else
034300010615     C                   if        flgOk = '0'
034400010615     C                   eval      wrkesito = '2'
034500010615     C                   else
034600010615     C                   eval      wrkesito = '6'
034700010615     C                   endif
034800010330     C                   endif
034900010330     C                   endif
035000010601     C                   else
035100010601     C                   eval      wrkesito = '9'
035200010601     C                   endif
035300010601     C*
035400010601     C                   if        vlrpoi <> 999
035500010601     C                   eval      invfil = vlrpoi
035600010601     C                   endif
035700010330     C*
035800010330     C                   if        §ctrok > 0
035900010601     C                             and invfil > *zeros
036000010330     C                   exsr      invio
036100010330     C                   endif
036200010601     C*
036300010618     C                   if        flgGiro = '1'
036400010601     C                   exsr      endela
036500010618     C                   endif
036600010330     C*
036700010330     C                   ENDSR
036800010330     C***
036900041025
037000041025
037100041025
037200041025
037300041025     C*-----------------------------------------------------------*
037400041025     C* ACCORPBOL - OPERAZIONI DI ACCORPAMENTO E SCRITTURA BOLLE  *
037500041025     C*-----------------------------------------------------------*
037600041025     C     ACCORPBOL     BEGSR
037700041025     C*
037800041025     C* Innanzitutto ordino la schiera d wrk
037900041025     C                   sorta     skWvab
038000041025     C*
038100041025     C* Inizializzo campi d wrk e d deposito
038200041025     C                   z-add     *zeros        wNCL             10 0
038300041025     C*
038400041025     C                   z-add     1             skJ
038500041025     C                   dow       skJ <= %elem(skWvab)
038600041025     C                   if        skWvab(skJ) <> *blanks
038700050103     C
038800050103     C                   eval      DSVAB = %subst(skWvab(skJ):16:486)
038900050103     C* Ad ogni elemento d schiera corrisponde 1 record d dettaglio colli
039000050103     C                   eval      VADFGS = VABFGS
039100050103     C                   eval      VADCCM = VABCCM
039200050103     C                   eval      VADAAS = VABAAS
039300050103     C                   eval      VADLNP = VABLNP
039400050103     C                   eval      VADNRS = VABNRS
039500050103     C                   eval      VADNSP = VABNSP
039600050103     C                   eval      VADNCL = VABNCL
039700050103     C                   eval      VADCDP = %subst(skWvab(skJ):502:15)
039800050103     C                   write     FIVAD000
039900041025     C* Se NN è l'ultima bolla
040000050103     C                   if        skJ < %elem(skWvab)
040100041025     C* X stesso RMN => accorpo
040200041025     C                   if        %subst(skWvab(skJ):1:15)  =
040300041025     C                             %subst(skWvab(skJ+1):1:15)
040400050103     C                   eval      wNCL = wNCL + VABNCL
040500041025     C                   else
040600050103     C                   eval      VABNCL = wNCL + VABNCL
040700050103     C                   eval      VABPKB = VABPKB * VABNCL
040800041025     C                   write     FIVAB000
040900041025     C                   z-add     *zeros        wNCL
041000041025     C                   endif
041100041025     C                   else
041200041025     C* Se è l'ultima bolla => scarico direttamente il buffer
041300050103     C                   eval      VABNCL = wNCL + VABNCL
041400050103     C                   eval      VABPKB = VABPKB * VABNCL
041500041025     C                   write     FIVAB000
041600041025     C                   endif
041700041025     C                   endif
041800041025     C                   add       1             skJ
041900041025     C                   enddo
042000041025     C*
042100041025     C                   ENDSR
042200041025     C***
042300010601
042400010601
042500010601
042600010330     C*----------------------------------------------------*
042700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042800010330     C*----------------------------------------------------*
042900010330     C     INZVAR        BEGSR
043000010330     C*
043100030822     C* Inizializzo variabili di wrk
043200020204     C                   Z-ADD     *zeros        Num5_0            5 0
043300030822     C*
043400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
043500030822     C                   CLEAR                   SkSplitCSV
043600030822     C                   EVAL      i = 1
043700030822     C                   EVAL      posDa = *zeros
043800030822     C                   EVAL      posA  = *zeros
043900030822     C*
044000030822     C* Reimposto i valori di default
044100030822     C                   EXSR      DEFCAM
044200030822     C*
044300010330     C                   ENDSR
044400010330     C*----------------------------------------------------*
044500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044600010330     C*----------------------------------------------------*
044700010330     C     DEFCAM        BEGSR
044800030822     C*
044900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
045000030715     C* e delimitatore testo.
045100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
045200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
045300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
045400030822     C*
045500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
045600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
045700030715     C                   SETON                                        50
045800030715     C                   ELSE
045900030715     C                   SETOFF                                       50
046000030715     C                   ENDIF
046100030822     C*
046200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
046300030822     C                   EVAL      posDaDft = 1
046400030822     C                   EVAL      posADft  = 0
046500030822     C                   EVAL      wGiroDft = 0
046600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
046700030822     C                             posDaDft > 0
046800050118     C*
046900050118     C* Imposto i valori d default iniziali
047000050118     C                   EVAL      VABCCM = 2100711
047100050118     C                   EVAL      VABLNP = 210
047200050118     C                   EVAL      VABCBO = '1'
047300030822     C*
047400030822     C* Gestisco il 1° giro
047500030822     C                   IF        wGiroDft = 0
047600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
047700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
047800030822     C* Incremento il contatore dei "giri"
047900030822     C                   EVAL      wGiroDft = 1
048000030822     C                   ELSE
048100030822     C                   EVAL      posDaDft = posADft
048200030822     C                   ENDIF
048300030822     C* Eseguo lo scan x trovare la fine del campo corrente
048400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
048500030822     C*
048600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
048700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
048800030822     C                   IF        posDaDft > 0 AND
048900030822     C                             posADft  > 0
049000030822     C* NCL
049100030822     C                   IF        %subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):1:3)
049400030822     C                             = 'NCL'
049500030822     C                   EVAL      PiStr=%trim(%subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049800030822     C                   EXSR      CHKNUM
049900030822     C                   IF        PiInt=*on
050000041025     C                   Z-ADD     PiVal         VABNCL
050100030822     C                   ENDIF
050200030822     C                   ENDIF
050300030822     C* CCM
050400030822     C                   IF        %subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):1:3)
050700030822     C                             = 'CCM'
050800030822     C                   EVAL      PiStr=%trim(%subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100030822     C                   EXSR      CHKNUM
051200030822     C                   IF        PiInt=*on
051300030822     C                   Z-ADD     PiVal         VABCCM
051400030822     C                   ENDIF
051500030822     C                   ENDIF
051600030822     C* LNP
051700030822     C                   IF        %subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):1:3)
052000030822     C                             = 'LNP'
052100030822     C                   EVAL      PiStr=%trim(%subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400030822     C                   EXSR      CHKNUM
052500030822     C                   IF        PiInt=*on
052600030822     C                   Z-ADD     PiVal         VABLNP
052700030822     C                   ENDIF
052800030822     C                   ENDIF
052900030822     C* NRS
053000030822     C                   IF        %subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):1:3)
053300030822     C                             = 'NRS'
053400030822     C                   EVAL      PiStr=%trim(%subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700030822     C                   EXSR      CHKNUM
053800030822     C                   IF        PiInt=*on
053900030822     C                   Z-ADD     PiVal         VABNRS
054000030822     C                   ENDIF
054100030822     C                   ENDIF
054200030822     C* CTR
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'CTR'
054700030822     C                   EVAL      PiStr=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   EXSR      CHKNUM
055100030822     C                   IF        PiInt=*on
055200030822     C                   Z-ADD     PiVal         VABCTR
055300030822     C                   ENDIF
055400030822     C                   ENDIF
055500030822     C* PKB
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'PKB'
056000030822     C                   EVAL      PiStr=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   EXSR      CHKNUM
056400030822     C                   IF        PiNum=*on
056500030822     C                   Z-ADD     PiVal         VABPKB
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* VLB
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'VLB'
057300030822     C                   EVAL      PiStr=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   EXSR      CHKNUM
057700030822     C                   IF        PiNum=*on
057800030822     C                   Z-ADD     PiVal         VABVLB
057900030822     C                   ENDIF
058000030822     C                   ENDIF
058100030822     C* QFT
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'QFT'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiNum=*on
059100030822     C                   Z-ADD     PiVal         VABQFT
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* CBO
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'CBO'
059900030822     C                   EVAL      VABCBO=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   ENDIF
060300030822     C* TSP
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'TSP'
060800030822     C                   EVAL      VABTSP=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* VAS
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'VAS'
061700030822     C                   EVAL      VABVAS=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* VCA
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'VCA'
062600030822     C                   EVAL      VABVCA=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* GCA
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'GCA'
063500030822     C                   EVAL      VABGCA=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* CTM
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'CTM'
064400030822     C                   EVAL      VABCTM=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* FFD
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'FFD'
065300030822     C                   EVAL      VABFFD=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* VAD
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'VAD'
066200030822     C                   EVAL      VABVAD=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* GMA
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'GMA'
067100030822     C                   EVAL      VABGMA=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* GGA
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'GGA'
068000030822     C                   EVAL      VABGGA=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* GVA
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'GVA'
068900030822     C                   EVAL      VABGVA=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* TC1
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'TC1'
069800030822     C                   EVAL      VABTC1=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* TC2
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'TC2'
070700030822     C                   EVAL      VABTC2=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100050104     C* RMO
071200050104     C                   IF        %subst(
071300050104     C                             %subst(vlrppt:posDaDft+1:
071400050104     C                             posADft-posDaDft-1):1:3)
071500050104     C                             = 'RMO'
071600050104     C                   EVAL      VABRMO=%trim(%subst(
071700050104     C                             %subst(vlrppt:posDaDft+1:
071800050104     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900050104     C                   ENDIF
072000030822     C* ...
072100030822     C                   ENDIF
072200030822     C                   ENDDO
072300020204     C*
072400010330     C                   ENDSR
072500010607     C*----------------------------------------------------*
072600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072700010607     C*----------------------------------------------------*
072800010607     C     IMPVAB        BEGSR
072900010607     C*
073000010607     C                   SETOFF                                       3132
073100030515     C*
073200010607     C                   MOVEL     datcor        VABAAS
073300010607     C                   MOVE      datcor        VABMGS
073400010607     C*
073500030715     C**********
073600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073700030715     C**********
073800030715     C                   IF        wGiro = *zeros
073900040726     C*
074000040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
074100040726     C                   EVAL      depvinDTA = vinDTA
074200040726     C*
074300041025     C* Se richiesta inserisco qui la forzatura della intesatazione colonne personalizzata
074400050103     C                   IF        intcol(1)+intcol(2)+intcol(3) <> *blanks
074500040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074600040726     C                                      %trim(intcol(3))
074700041025     C                   ENDIF
074800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074900031201     C     minu:maiu     XLATE     vindta        vindta
075000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075100040324     C                   DOW       posDa <= %len(%trimr(vindta))
075200030715     C*
075300030715     C* Gestisco il 1° campo
075400030715     C                   IF        i = 1
075500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075600030715     C                   EVAL      posDa = 1
075700030715     C* Eseguo lo scan x trovare la fine del primo campo
075800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075900030715     C* A questo punto "estraggo" il campo corrente
076000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076100030715     C                                                    (posA-posDa))
076200030715     C* X i campi successivi al 1°
076300030715     C                   ELSE
076400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076500030715     C                   EVAL      posDa = posA + 1
076600030715     C* Eseguo lo scan x trovare la fine del campo corrente
076700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076800030715     C* Gestisco l'ultimo campo
076900030715     C                   IF        posA = *zeros
077000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
077100030715     C* A questo punto "estraggo" il campo corrente
077200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077300030715     C                                                    (posA-posDa))
077400030715     C                   LEAVE
077500030715     C                   ELSE
077600030715     C* A questo punto "estraggo" il campo corrente
077700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077800030715     C                                                    (posA-posDa))
077900030715     C                   ENDIF
078000030715     C                   ENDIF
078100030715     C* Incremento il contatore d campo
078200030715     C                   EVAL      i = i +1
078300030715     C                   ENDDO
078400030715     C*
078500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078600030715     C* delimitazione testo
078700030715     C                   EVAL      i = 1
078800030715     C                   DOW       i < %elem(SkSplitFLD)
078900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
079000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
079100030715     C                   EVAL      i = i + 1
079200030715     C                   ENDDO
079300030715     C                   EVAL      wGiro = 1
079400040726     C* Ripristino la 1° riga originale
079500040726     C                   EVAL      vinDTA = depvinDTA
079600030715     C                   ELSE
079700030715     C                   EVAL      wGiro = 2
079800031204     C**********
079900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
080000031204     C* d separatore campo
080100031204     C**********
080200040726     C                   EVAL      vindta = %trim(vindta)
080300040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
080400040726     C                   EVAL      vindta = %subst(vindta:2)
080500040726     C                   ENDDO
080600031204     C*
080700031204     C                   Z-ADD     *zeros        lunghInput        4 0
080800040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
080900040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
081000040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
081100040726     C                   EVAL      lunghInput = %len(%trim(vindta))
081200040726     C                   ENDDO
081300030715     C**********
081400030715     C* Effettuo lo split del campo dati d input
081500030715     C**********
081600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081700040324     C                   DOW       posDa <= %len(%trimr(vindta))
081800030715     C*
081900030715     C* Gestisco il 1° campo
082000030715     C                   IF        i = 1
082100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082200030715     C                   EVAL      posDa = 1
082300030715     C* Eseguo lo scan x trovare la fine del primo campo
082400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082500030715     C* A questo punto "estraggo" il campo corrente
082600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082700030715     C                                                    (posA-posDa))
082800030715     C* X i campi successivi al 1°
082900030715     C                   ELSE
083000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083100030715     C                   EVAL      posDa = posA + 1
083200031204     C* Verifico che nn vi sia il campo nn valorizzato
083300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
083400031204     C* Se campo nn valorizzato skippo al prossimo
083500031204     C                   EVAL      SkSplitCSV(i) = *blanks
083600031204     C                   EVAL      posA  = posA + 1
083700031204     C                   ELSE
083800030715     C* Eseguo lo scan x trovare la fine del campo corrente
083900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084000030715     C* Gestisco l'ultimo campo
084100030715     C                   IF        posA = *zeros
084200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C                   LEAVE
084700030715     C                   ELSE
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C                   ENDIF
085200031204     C                   ENDIF
085300030715     C                   ENDIF
085400030715     C* Incremento il contatore d campo
085500030715     C                   EVAL      i = i +1
085600030715     C                   ENDDO
085700030715     C*
085800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085900030715     C* delimitazione testo
086000030715     C                   EVAL      i = 1
086100030715     C                   DOW       i < %elem(SkSplitCSV)
086200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
086300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
086400030715     C                   EVAL      i = i + 1
086500030715     C                   ENDDO
086600030715     C*
086700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086800030715     C                   EVAL      i = 1
086900030715     C                   DOW       i < %elem(SkSplitFLD)
087000030715     C*
087100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
087200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
087300030715     C                             SkSplitCSV(i) <> *blanks
087400030721     C*** ATB
087500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087600030721     C                   EVAL      VABATB = SkSplitCSV(i)
087700030721     C                   ENDIF
087800030715     C*** RSD
087900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
088000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
088100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
088200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
088300030715     C* ==
088400030715     C                   ENDIF
088500030721     C*** RD2
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030715     C*** IND
089000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
089100030715     C                   EVAL      VABIND = SkSplitCSV(i)
089200030715     C                   ENDIF
089300030715     C*** LOD
089400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
089500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089600030715     C                   ENDIF
089700030715     C*** PRD
089800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
090000030715     C                   ENDIF
090100030721     C*** NZD
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
090300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030715     C*** RMA
090600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090800030715     C                   ENDIF
090900030715     C*** NOT
091000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
091100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
091200030715     C                   ENDIF
091300030715     C*** NT2
091400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
091500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091600030715     C                   ENDIF
091700030715     C*** VCA
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030715     C*** VAS
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
092300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030715     C*** FFD
092600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092800030715     C                   ENDIF
092900030715     C*** CBO
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
093100030715     C                   EVAL      VABCBO = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030715     C
093400030721     C*** GC1
093500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
093600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
093700030721     C                   ENDIF
093800030721     C*** GC2
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
094000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030721     C*** TSP
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
094400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030721     C*** NAS
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
094800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030721     C*** TIC
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
095200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030721     C*** GCA
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030721     C*** XCO
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
096000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030721     C*** CTM
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030721     C*** TCR
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
096800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*** CTS
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
097200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** FTM
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** VAD
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
098000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** GMA
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** GGA
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
098800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** GVA
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
099200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** TC1
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** TC2
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
100000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** SCL
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** RMO
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
100800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** NMO
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
101200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101300030721     C                   ENDIF
101400050103     C*** VADCDP
101500050103     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP'
101600050103     C                   EVAL      VADCDP = SkSplitCSV(i)
101700050103     C                   ENDIF
101800030721     C*
101900030721     C* Reperisco quindi i campi numerici...
102000030721     C*** CAD
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
102200030721     C                   EVAL      PiStr=SkSplitCSV(i)
102300030721     C                   EXSR      CHKNUM
102400030721     C                   IF        PiInt=*on
102500030721     C                   Z-ADD     PiVal         Num5_0
102600030721     C                   MOVEL(p)  Num5_0        VABCAD
102700030721     C                   ELSE
102800030721     C                   SETON                                        32
102900030721     C                   EVAL      VABCAD = *zeros
103000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103100030721     C                             + ' ' + 'VABCAD'
103200030721     C                   ENDIF
103300030721     C                   ENDIF
103400030721     C*** RMN
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
103600030721     C                   EVAL      PiStr=SkSplitCSV(i)
103700030721     C                   EXSR      CHKNUM
103800030721     C                   IF        PiInt=*on
103900030721     C                   Z-ADD     PiVal         VABRMN
104000030721     C                   ELSE
104100041025     C                   SETON                                        31
104200041025     C                   EVAL      VABRMN = *zeros
104300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104400030721     C                             + ' ' + 'VABRMN'
104500030721     C                   ENDIF
104600030721     C                   ENDIF
104700030721     C*** NSP
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104900030721     C                   EVAL      PiStr=SkSplitCSV(i)
105000030721     C                   EXSR      CHKNUM
105100040726     C                   IF        PiInt=*on
105200040726     C                   Z-ADD     PiVal         VABNSP
105300050103     C                   Z-ADD     PiVal         VABRMN
105400030721     C                   ELSE
105500030721     C                   SETON                                        32
105600030721     C                   EVAL      VABNSP = *zeros
105700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105800050103     C                             + ' ' + 'VABNSP VABRMN VADNSP'
105900030721     C                   ENDIF
106000030721     C                   ENDIF
106100030721     C*** NCL
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
106300030721     C                   EVAL      PiStr=SkSplitCSV(i)
106400030721     C                   EXSR      CHKNUM
106500030721     C                   IF        PiInt=*on
106600041025     C  N11              Z-ADD     PiVal         VABNCL
106700041025     C   11              ADD       PiVal         VABNCL
106800030721     C                   ELSE
106900030721     C                   SETON                                        32
107000030721     C                   EVAL      VABNCL = *zeros
107100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107200050103     C                             + ' ' + 'VABNCL VADNCL'
107300030721     C                   ENDIF
107400030721     C                   ENDIF
107500030721     C*** PKB
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
107700030721     C                   EVAL      PiStr=SkSplitCSV(i)
107800030721     C                   EXSR      CHKNUM
107900030721     C                   IF        PiNum=*on
108000041025     C  N11              Z-ADD     PiVal         VABPKB
108100041025     C   11              ADD       PiVal         VABPKB
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      VABPKB = *zeros
108500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108600030721     C                             + ' ' + 'VABPKB'
108700030721     C                   ENDIF
108800030721     C                   ENDIF
108900030721     C*** CAS
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiNum=*on
109400030721     C                   Z-ADD     PiVal         VABCAS
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      VABCAS = *zeros
109800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109900030721     C                             + ' ' + 'VABCAS'
110000030721     C                   ENDIF
110100030721     C                   ENDIF
110200030721     C*** CCM
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiInt=*on      AND
110700030721     C                             PiVal<=9999999 AND
110800030721     C                             PiVal>*zeros
110900030721     C                   Z-ADD     PiVal         VABCCM
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      VABCCM = *zeros
111300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111400050103     C                             + ' ' + 'VABCCM VADCCM'
111500030721     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** LNP
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on    AND
112200030721     C                             PiVal<=999   AND
112300030721     C                             PiVal>*zeros
112400030721     C                   Z-ADD     PiVal         VABLNP
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700030721     C                   EVAL      VABLNP = *zeros
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900050103     C                             + ' ' + 'VABLNP VADLNP'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** NRS
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on AND
113700030721     C                             PiVal<=99
113800030721     C                   Z-ADD     PiVal         VABNRS
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200050103     C                             + ' ' + 'VABNRS VADNRS'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** LNA
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiInt=*on    AND
115000030721     C                             PiVal<=999   AND
115100030721     C                             PiVal>*zeros
115200030721     C                   Z-ADD     PiVal         VABLNA
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABLNA = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABLNA'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** CTR
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on  AND
116500030721     C                             PiVal<=999
116600030721     C                   Z-ADD     PiVal         VABCTR
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABCTR'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** IAS
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiNum=*on
117800030721     C                   Z-ADD     PiVal         VABIAS
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200030721     C                             + ' ' + 'VABIAS'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** VLB
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiNum=*on
119000030721     C                   Z-ADD     PiVal         VABVLB
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABVLB'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** QFT
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiNum=*on
120200030721     C                   Z-ADD     PiVal         VABQFT
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABQFT'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** NCD
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on      AND
121400030721     C                             PiVal<=9999999
121500030721     C                   Z-ADD     PiVal         VABNCD
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABNCD'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** NCA
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on      AND
122700030721     C                             PiVal<=9999999
122800030721     C                   Z-ADD     PiVal         VABNCA
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABNCA'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** ZNC
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on AND
124000030721     C                             PiVal<=99
124100030721     C                   Z-ADD     PiVal         VABZNC
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABZNC'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** DCR
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
125000050103     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):7:4)+
125100050103     C                                   %subst(%trim(SkSplitCSV(i)):4:2)+
125200050103     C                                   %subst(%trim(SkSplitCSV(i)):1:2)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on       AND
125500030721     C                             PiVal<=99999999
125600030721     C                   Z-ADD     PiVal         VABDCR
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABDCR'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** HCR
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on   AND
126800030721     C                             PiVal<=9999
126900030721     C                   Z-ADD     PiVal         VABHCR
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABHCR'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** VMD
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiNum=*on
128100030721     C                   Z-ADD     PiVal         VABVMD
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABVMD'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** ANT
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on        AND
129300030721     C                             PiVal<=999999999
129400030721     C                   Z-ADD     PiVal         VABANT
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABANT'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** CMO
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on
130600030721     C                   Z-ADD     PiVal         Num5_0
130700030721     C                   MOVEL(p)  Num5_0        VABCMO
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      VABCMO = *zeros
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABCMO'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030715     C*
131600030715     C                   ENDIF
131700030715     C                   EVAL      i = i + 1
131800030715     C                   ENDDO
131900040726     C*
132000040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
132100040726     C                   IF        VABLNP = *zeros
132200040726     C                   SETON                                        31
132300040726     C                   ENDIF
132400040726     C*
132500040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
132600040726     C* d valorizzarli sempre entrambi
132700040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
132800040726     C                   Z-ADD     VABNSP        VABRMN
132900040726     C                   ENDIF
133000040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
133100040726     C                   MOVEL     VABRMN        VABRMA
133200040726     C                   ENDIF
133300040726     C*
133400040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
133500040726     C                   IF        VABCAS > *zeros
133600040726     C                   IF        VABVCA = *blanks
133700040726     C                   EVAL      VABVCA = 'EUR'
133800040726     C                   ENDIF
133900040726     C                   IF        VABCBO = '1'
134000040726     C                   EVAL      VABCBO = '4'
134100040726     C                   ENDIF
134200040726     C                   IF        VABCBO = '2'
134300040726     C                   EVAL      VABCBO = '6'
134400040726     C                   ENDIF
134500040726     C                   ENDIF
134600020204     C*
134700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
134800020204     C                   EXSR      CHKIMPDIV
134900030715     C                   ENDIF
135000020204     C*
135100010607     C                   ENDSR
135200010607     C*----------------------------------------------------*
135300010601
135400020204
135500020204     C*----------------------------------------------------*
135600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
135700020204     C*----------------------------------------------------*
135800020204     C     CHKIMPDIV     BEGSR
135900020204     C*
136000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
136100020204     C                   Z-ADD     *zeros        wrkDec            9 9
136200020204     C*
136300020204     C* Come prima cosa effettuo considerazioni sulla divisa
136400020204     C                   IF        vabIAS > *zeros
136500020204     C                   IF        vabVAS <> 'EUR'
136600020204     C                   EVAL      vabVAS =  'ITL'
136700020204     C                   ENDIF
136800020204     C                   ENDIF
136900020204     C*
137000020204     C                   IF        vabCAS > *zeros
137100020204     C                   IF        vabVCA <> 'EUR'
137200020204     C                   EVAL      vabVCA =  'ITL'
137300020204     C                   ENDIF
137400020204     C                   ENDIF
137500020204     C*
137600020204     C                   IF        vabVMD > *zeros
137700020204     C                   IF        vabVAD <> 'EUR'
137800020204     C                   EVAL      vabVAD =  'ITL'
137900020204     C                   ENDIF
138000020204     C                   ENDIF
138100020204     C*
138200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
138300020204     C                   Z-ADD     vabIAS        wrkDec
138400020204     C                   IF        wrkDec > *zeros
138500020204     C                   IF        vabVAS = 'ITL'
138600020204     C                   EVAL      vabIAS = *zeros
138700020204     C                   ENDIF
138800020204     C                   ENDIF
138900020204     C*
139000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
139100020204     C                   Z-ADD     vabCAS        wrkDec
139200020204     C                   IF        wrkDec > *zeros
139300020204     C                   IF        vabVCA = 'ITL'
139400020204     C                   EVAL      vabCAS = *zeros
139500020204     C                   ENDIF
139600020204     C                   ENDIF
139700020204     C*
139800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
139900020204     C                   Z-ADD     vabVMD        wrkDec
140000020204     C                   IF        wrkDec > *zeros
140100020204     C                   IF        vabVAD = 'ITL'
140200020204     C                   EVAL      vabVMD = *zeros
140300020204     C                   ENDIF
140400020204     C                   ENDIF
140500020204     C*
140600020204     C                   ENDSR
140700020204     C***
140800020204
140900010330
141000010330
141100010330     C*----------------------------------------------------*
141200010330     C*  CONTROLLO NUMERICITA' CAMPI
141300010330     C*----------------------------------------------------*
141400010330     C     CHKNUM        BEGSR
141500010330     C*
141600010606     C                   IF        PiDecChr = *blanks
141700030715     C                   EVAL      PiDecChr = CharNUM
141800010606     C                   ENDIF
141900010606     C*
142000010606     C                   CALL(e)   'ISNUMERIC'
142100010330     C                   PARM                    PiStr            30
142200010606     C                   PARM                    PiDecChr          1
142300010330     C                   PARM      *ZEROS        PiVal            30 9
142400010330     C                   PARM      '0'           PiInt             1
142500010330     C                   PARM      '0'           PiNum             1
142600010330     C                   IF        %error
142700010606     C                   EVAL      PiNum=*off
142800010330     C                   ENDIF
142900010330     C*
143000010330     C                   ENDSR
143100010330     C***
143200010330
143300010601
143400010601
143500010601
143600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
143700010601     C     repfil        BEGSR
143800010601     C*
143900010601     C                   if        invfil = *zeros and
144000010601     C                             depfil > *zeros and
144100010629     C                             (vinflg = *blanks or
144200010629     C                              vinflg = *zeros)
144300010601     C
144400010601     C                   eval      invfil = depfil
144500010601     C                   endif
144600010601     C*
144700010601     C                   if        depfil <> invfil and
144800010601     C                             invfil > *zeros
144900010601     C                   eval      flgMulti = '1'
145000010601     C                   if        vinflg = *blanks
145100010601     C                   add       1             cntNonEl
145200010601     C                   endif
145300010601     C                   endif
145400010601     C*
145500010601     C                   if        vinflg = '2'
145600010601     C                   eval      flgStato = '2'
145700010601     C                   endif
145800010601     C*
145900010601     C                   ENDSR
146000010601     C***
146100010601
146200010601
146300050103
146400050103      /TITLE Invio dei dati al punto operativo.
146500050103     C     invio         BEGSR
146600050103     C*
146700050103     C* 1° invio FIVAD
146800050103     C                   reset                   dscmz
146900050103     C                   move      vlrpoi        cmzdst
147000050103     C                   eval      cmzfld = 'FIVADWWR'
147100050103     C                   eval      cmzmbd = vlrhdl
147200050103     C                   eval      %subst(cmzmbd:1:1) = 'M'
147300050103     C***                if        prmfir = *blanks
147400050103     C                   eval      cmzfla = 'FIVAD00F'
147500050103     C                   eval      cmzmba = 'FIVAD00F'
147600050103     C***                else
147700050103     C***                eval      cmzfla = prmfir
147800050103     C***                eval      cmzmba = prmfir
147900050103     C***                endif
148000050103     C                   eval      cmznrr = *zeros
148100050103     C                   move      §ctrokvd      cmznrr
148200050103     C                   eval      cmzlba = vlrfl1
148300050103     C                   call(e)   'TIS711C'
148400050103     C                   parm                    dscmz
148500050103     C                   parm      *blanks       esito
148600050103     C                   if        %error
148700050103     C                             or cmzerr = '1'
148800050103     C                             or esito  = '1'
148900050103     C                   eval      wrkesito = '3'
149000050103     C                   else
149100050103     C*
149200050103     C* 2° invio FIVAB
149300050103     C                   reset                   dscmz
149400050103     C                   move      vlrpoi        cmzdst
149500050103     C                   eval      cmzfld = vlrfou
149600050103     C                   eval      cmzmbd = vlrhdl
149700050103     C                   eval      %subst(cmzmbd:1:1) = 'M'
149800050103     C***                if        prmfir = *blanks
149900050103     C                   eval      cmzfla = 'FIVAB00F'
150000050103     C                   eval      cmzmba = 'FIVAB00F'
150100050103     C***                else
150200050103     C***                eval      cmzfla = prmfir
150300050103     C***                eval      cmzmba = prmfir
150400050103     C***                endif
150500050103     C                   eval      cmznrr = *zeros
150600050103     C                   move      §ctrok        cmznrr
150700050103     C                   eval      cmzlba = vlrfl1
150800050103     C                   call(e)   'TIS711C'
150900050103     C                   parm                    dscmz
151000050103     C                   parm      *blanks       esito
151100050103     C                   if        %error
151200050103     C                             or cmzerr = '1'
151300050103     C                             or esito  = '1'
151400050103     C                   eval      wrkesito = '3'
151500050103     C                   endif
151600050103     C                   endif
151700050103     C*
151800050103     C                   ENDSR
151900050103     C***
152000010601
152100010601
152200010601
152300010601      /TITLE Invio dei dati al punto operativo.
152400010601     C     opeini        BEGSR
152500010601     C*
152600010601     C* Inizializzo flag e contatori operativi
152700010601     C                   movel     '0'           flgGiro           1
152800010601     C                   movel     '0'           flgMulti          1
152900010601     C                   movel     '1'           flgStato          1
153000010615     C                   movel     '0'           flgOk             1
153100010601     C                   z-add     *zeros        cntNonEl         10 0
153200010601     C                   z-add     *zeros        depfil            3 0
153300010601     C                   z-add     *zeros        invfil            3 0
153400010601     C*
153500010601     C                   ENDSR
153600010601     C***
153700010601
153800050103
153900050103     C*----------------------------------------------------*
154000050103     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVADWWR
154100050103     C*----------------------------------------------------*
154200050103     C     PREVAD        BEGSR
154300050103     C*
154400050103     C* Compongo il nome del membro da dare al FIVADWWR
154500050103     C                   eval      parmbr = vlrhdl
154600050103     C                   movel     'M'           parmbr
154700050103     C                   eval      parccm = vlrKSC
154800050103     C                   eval      paropz = '1'
154900050103     C* Effettuo la chiamata al CLLE preposto
155000050103     C                   call(e)   'TITVVDC'
155100050103     C                   parm                    parccm
155200050103     C                   parm                    parmbr
155300050103     C                   parm                    paropz
155400050103     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
155500050103     C                   if        %error
155600050103     C                   movel     '1'           chkcall
155700050103     C                   else
155800050103     C                   movel     '0'           chkcall
155900050103     C                   endif
156000050103     C*
156100050103     C                   ENDSR
156200050103     C*----------------------------------------------------*
156300010601
156400010330
156500050421
156600050421
156700081006     C     *pssr         BEGSR
156800081006     C*
156900081006     C                   if        %open(tivin00r)
157000081006     C                   close     tivin00r
157100081006     C                   endif
157200081006     C                   if        %open(fivabwwr)
157300081006     C                   close     fivabwwr
157400081006     C                   endif
157500081006     C                   if        %open(fivadwwr)
157600081006     C                   close     fivadwwr
157700081006     C                   endif
157800081006     C*
157900081006     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
158000081006     C                   Z-ADD     150           LENGH            15 5
158100081006     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
158200081006     C                   PARM                    CMD(3)
158300081006     C                   PARM                    LENGH
158400081006     C*
158500081006     C* Effettuo la chiamata al CLLE preposto
158600081006     C                   call(e)   'TITVVDC'
158700081006     C                   parm                    parccm
158800081006     C                   parm                    parmbr
158900081006     C                   parm      '2'           paropz
159000050421     C*
159100050421     C                   eval      wrkesito = '2'
159200050421     C*
159300050421     C                   ENDSR     '*CANCL'
159400050421     C***
159500050421
159600050421
159700010330
159800000613     C     *inzsr        BEGSR
159900990910     C*
160000990910     C     *entry        plist
160100990920     C                   parm                    tivlrds
160200990921     C                   parm      wrkesito      esito
160300000724     C                   parm                    prmlit
160400000710     C                   parm                    prmfir
160500010330     C*
160600010330     C* CALCOLA LA DATA CORRENTE
160700010330     C                   time                    wn14             14 0
160800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
160900010330     C                   z-add     wn8           g08dat
161000010330     C                   z-add     *zeros        g08inv
161100010330     C                   movel     '0'           g08err
161200010330     C                   call      'XSRDA8'
161300010330     C                   parm                    wlbda8
161400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
161500000613     C*
161600000613     C                   ENDSR
161700000613     C***
161800990908
161900030715     Otitvxlp   E            riepilogo         2
162000990915     O                                              'Upload via Internet'
162100990915     O                                           +1 'Traduzione TIVIN00R -'
162200021025     O                                           +1 'FIVABWWR'
162300990915     O                                           +1 'Testate bolle'
162400990915     O          E            riepilogo   2
162500990915     O                       wrkdata
162600990915     O                       wrkora              +1
162700990915     O                       procname            +1
162800990915     O          E            riepilogo   2
162900990915     O                                              'Cliente..................:'
163000010605     O                       VABCCM        z     +1
163100990915     O          E            riepilogo   2
163200990920     O                                              'Riferimento Strategi.....:'
163300990920     O                       vlrhdl              +1
163400990915     O          E            riepilogo   2
163500990915     O                                              'Giusti...................:'
163600971022     O                       §CTROK        2   +  1
163700990915     O          E            riepilogo   2
163800990915     O                                              'Sbagliati e corretti.....:'
163900971022     O                       §CTRMO        2   +  1
164000990915     O          E            riepilogo   2
164100990915     O                                              'Sbagliati e scartati.....:'
164200971022     O                       §CTRNO        2   +  1
164300000613
164400030715     Otitvxlps  E            testdett          2
164500000613     O                                              'Upload via Internet'
164600000613     O                                           +1 'Traduzione TIVIN00R -'
164700021025     O                                           +1 'FIVABWWR'
164800000613     O                                           +1 'Testate bolle'
164900000616     O          E            testdett    3
165000000613     O                                           +2 'N° rec'
165100000613     O                                           +3 'Anteprima contenuto'
165200000616     O          E            rigadett    2
165300000613     O                       rrnum               +2
165400000621     O                       recko               +3
165500000616     O          E            findett     2
165600000613     O                       wrkdata
165700000613     O                       wrkora              +1
165800000613     O                       procname            +1
165900000616     O          E            findett     2
166000000613     O                                              'Cliente..................:'
166100010605     O                       VABCCM        z     +1
166200000616     O          E            findett     2
166300000613     O                                              'Riferimento Strategi.....:'
166400000613     O                       vlrhdl              +1
166500000616     O          E            findett     2
166600000613     O                                              'Giusti...................:'
166700000613     O                       §CTROK        2   +  1
166800000616     O          E            findett     2
166900000613     O                                              'Sbagliati e corretti.....:'
167000000613     O                       §CTRMO        2   +  1
167100000616     O          E            findett     2
167200000613     O                                              'Sbagliati e scartati.....:'
167300000613     O                       §CTRNO        2   +  1
167400000613** CMD - COMANDI CL
167500030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
167600030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
167700030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
167800010606
167900000613
168000040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
168100050103VABNSP;VABRMA;FILLER1;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VADCDP;FILLER2;VABNCL;
168200050103FILLER3;VABDCR;FILLER4
168300040726
