000100041108      /TITLE Upload via Internet: traduzione in FIVABWWR/FIBATWWR. (Cliente ARTE' - Cod. 0012232)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700041108     FFIVATwwr  O    E             DISK    usropn
000800050504     Ftitvf8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050504     Ftitvf8ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040726     D*------------
002000040726     D* SCHIEDE A PROGRAMMA
002100040726     D*------------
002200040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000050504     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004100041108     d wbarcode        s                   like(vatnot) inz(*blanks)
004200041108     d wtel            s                   like(vatnot) inz(*blanks)
004300041108     d wref            s                   like(vatnot) inz(*blanks)
004400050504     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004500041108     D parccm          s              8    INZ(*blanks)
004600041108     D parmbr          s             10    INZ(*blanks)
004700041108     D paropz          s              1    INZ(*blanks)
004800041108     D chkcall         s              1    INZ(*blanks)
004900030715     D*------------------
005000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005100030715     D*------------------
005200030715     D SkSplitFLD      S             10    DIM(100)
005300030715     D SkSplitCSV      S            256    DIM(100)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700030715     D posDa           S              3  0 INZ(*zeros)
005800030715     D posA            S              3  0 INZ(*zeros)
005900030715     D i               s              3  0 INZ(1)
006000041122     d savrmn          s                   like(vabrmn)
006100050511     d savnsp          s                   like(vabnsp)
006200030822     D*------------------
006300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006400030822     D*------------------
006500030822     D posDaDft        S              3  0 INZ(*zeros)
006600030822     D posADft         S              3  0 INZ(*zeros)
006700030822     D j               s              3  0 INZ(1)
006800030822     D wGiroDft        s              1  0 INZ(*zeros)
006900010330     D*------------------
007000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007100010330     D*------------------
007200010330     D WLBDA8          DS                  INZ
007300010330     D  G08DAT                 1      8  0
007400010330     D  G08INV                 9     16  0
007500010330     D  G08ERR                17     17
007600010330     D  G08TGI                18     22  0
007700031201     D*------------------
007800031201     D* Costanti
007900031201     D*------------------
008000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008200010330
008300041122     D*------------------
008400041122     D* DS REPERIMENTO NUMERATORE
008500041122     D*------------------
008600041122     D trul33ds      e ds                  inz
008700041122     D*------------------
008800041122     D* DS ARCHITETTURA
008900041122     D*------------------
009000041122     D kpjba         e ds                  inz
009100041122     D*------------------
009200010330
009300010330
009400990915     C                   time                    wrkdata
009500990915     C                   time                    wrkora
009600000913     C                   reset                   rrnum
009700990921     C                   reset                   esito
009800990921     C                   reset                   wrkesito
009900010601     C*
010000010601     C                   exsr      opeini
010100010605     C                   exsr      rwvab
010200010601     C*
010300010601     C                   seton                                        lr
010400010601
010500010601
010600010601
010700010601
010800010601     C*--------------------------------------------------------
010900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011000010601     C*--------------------------------------------------------
011100010601     C     PREELA        BEGSR
011200010601     C*
011300010601     C* SE OCCORRE SPEDIRE IN FILIALE
011400010601     C                   if        invfil <> *zeros and
011500010601     C                             flgGiro = '0'
011600010601     C*
011700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011800010601     C                   eval      flgGiro = '1'
011900000724     C*
012000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012200010601     C     invfil        chain     azorg01l
012300000724     C                   if        %found
012400000724     C                   movel(p)  CMD(1)        depcmd
012500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012600000724     C*
012700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012800011113     C                   Z-ADD     150           LENGH            15 5
012900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013000000724     C                   PARM                    depcmd
013100000724     C                   PARM                    LENGH
013200000724     C*
013300000724     C                   endif
013400000616     C*
013500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013600000616     C                   movel(p)  CMD(2)        depcmd
013700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013800000616     C*
013900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014000011113     C                   Z-ADD     150           LENGH            15 5
014100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014200000616     C                   PARM                    depcmd
014300000616     C                   PARM                    LENGH
014400000613     C*
014500050504     C                   if        not %open(titvf8ps)
014600050504     C                   open      titvf8ps
014700030715     C                   except    testdett
014800000613     C                   endif
014900010601     C*
015000010601     C                   endif
015100010601     C*
015200010601     C                   ENDSR
015300010601     C***
015400010601
015500010601
015600010601
015700010601
015800010601     C*--------------------------------------------------------
015900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016000010601     C*--------------------------------------------------------
016100010601     C     ENDELA        BEGSR
016200990915     C*
016300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016400041108     C*
016500041108     C* Effettuo la chiamata al CLLE preposto
016600041108     C                   call(e)   'TITVVTC'
016700041108     C                   parm                    parccm
016800041108     C                   parm                    parmbr
016900041108     C                   parm      '2'           paropz
017000050504     C*
017100050504     C* Effettuo lancio TISI95 solo x chiusura
017200050504     C                   CLEAR                   TISI95DS
017300050504     C                   EVAL      I95TLA = 'C'
017400050504     C                   CALL      'TISI95R'
017500050504     C                   PARM                    TISI95DS
017600000613     C*
017700050504     C                   if        %open(titvf8ps)
017800000616     C                   except    findett
017900050504     C                   close     titvf8ps
018000000613     C                   endif
018100000616     C*
018200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018300011113     C                   Z-ADD     150           LENGH            15 5
018400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018500000616     C                   PARM                    CMD(3)
018600000616     C                   PARM                    LENGH
018700000616     C*
018800010601     C                   ENDSR
018900010601     C***
019000010601
019100010601
019200010601
019300010601
019400000613
019500000613     C*--------------------------------------------------------
019600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019700000613     C*--------------------------------------------------------
019800000621     C     STPR          BEGSR
019900000613     C*
020000050504     C                   if        not %open(titvf8p)
020100050504     C   50              open      titvf8p
020200990915     C                   endif
020300990915     C*
020400030715     C   50              except    riepilogo
020500990915     C*
020600050504     C                   if        %open(titvf8p)
020700050504     C   50              close     titvf8p
020800990914     C                   endif
020900000613     C*
021000000613     C                   ENDSR
021100000613     C***
021200000613
021300010330
021400010601
021500010601
021600010601
021700010330     C*--------------------------------------------------------
021800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021900010330     C*--------------------------------------------------------
022000010605     C     RWVAB         BEGSR
022100010330     C*
022200010330     C                   if        not %open(tivin00r)
022300010330     C                   open      tivin00r
022400010330     C                   endif
022500021025     C                   if        not %open(fivabwwr)
022600021025     C                   open      fivabwwr
022700010330     C                   endif
022800010330     C*
022900041108     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023000041108     C                   exsr      prevat
023100041108     C*
023200041108     C                   if        chkcall = '0'
023300041108     C*
023400041108     C                   if        not %open(fivatwwr)
023500041108     C                   open      fivatwwr
023600041108     C                   endif
023700041108     C*
023800041108     C                   clear                   §CTROKVB
023900041109     C                   clear                   §CTROKVT          7 0
024000010604     C                   clear                   §CTRMO
024100010604     C                   clear                   §CTRNO
024200050504
024300050504      * per prima cosa mi imposto la schiera con le intestazioni dei campi
024400050504     c                   EXSR      INTEST
024500010330     C*
024600030822     C                   do        *HIVAL
024700010330     C*
024800030822     C                   read      tivin00r                               70
024900010618     C*
025000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025100010618     C                   if        vinflg = '1'
025200010618     C                   eval      flgOk = '1'
025300010618     C                   endif
025400010618     C*
025500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025600040510     C                   z-add     *zeros        wLenVINDTA        4 0
025700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
025800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
025900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026100040510     C                   z-add     1             wFlgVINDTA
026200040510     C                   leave
026300040510     C                   endif
026400040510     C                   enddo
026500040510     C*
026600040218     C                   if        vindta > *blanks AND
026700040510     C                             wFlgVINDTA = 1
026800010330     C                   add       1             rrnum
026900010330     C*
027000010601     C                   if        *in70 = *off and
027100010330     C                             (vinflg = *blanks
027200010330     C                              or vinflg = '0'
027300010330     C                              or vinflg = '2')
027400010330     C*
027500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027600010711     C                   if        vinflg = *blanks or vinflg = '0'
027700010711     C                   clear                   vinmsg
027800010711     C                   endif
027900010601     C*
028000010330     C                   exsr      inzvar
028100010605     C                   exsr      impvab
028200010601     C*
028300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028400010605     C                   eval      depfil = VABLNP
028500010601     C                   exsr      repfil
028600010601     C                   if        depfil = invfil
028700021025     C                   if        vlrpoi = 999
028800030822     C                   move(P)   invfil        VABFGS
028900041108     C                   move(P)   invfil        VATFGS
029000021025     C                   else
029100030822     C                   move(P)   vlrpoi        VABFGS
029200041108     C                   move(P)   vlrpoi        VATFGS
029300021025     C                   endif
029400010601     C*
029500010601     C                   exsr      PREELA
029600010601     C*
029700010604     C* Ebbene...
029800010604     C*
029900041108     C  N31              add       1             §CTROKVB          7 0
030000030822     C   32              add       1             §CTRMO            7 0
030100030822     C   31              add       1             §CTRNO            7 0
030200050504     C                   if        depspe =  *zeros OR
030300050511     C                             depspe <> savNSP
030400050504     C  N31              exsr      wrivab
030500050511     C  N31              eval      depspe = savNSP
030600050504     C                   endif
030700010604     C*
030800010604     C                   if        *in31 = *off and
030900010604     C                             *in32 = *off
031000010604     C                   eval      vinflg = '1'
031100010604     C                   else
031200010604     C                   eval      recko = vindta
031300010604     C                   except    rigadett
031400010604     C                   eval      vinflg = '2'
031500010604     C                   endif
031600010604     C                   endif
031700010601     C*
031800010604     C                   endif
031900010604     C*
032000010330     C                   else
032100010330     C                   eval      vinflg = '1'
032200010330     C                   endif
032300010601     C*
032400010601     C  N70              update    tivin000
032500010330     C*
032600030822     C  N70              enddo
032700041025     C*
032800041109     C                   endif
032900010601     C*
033000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033100010601     C                   if        cntNonEl = *zeros or
033200010601     C                             flgMulti = '0'
033300010330     C* Se non ci sono record con errori ...
033400010601     C                   if        §ctrno = 0 and
033500010604     C                             §ctrmo = 0 and
033600010601     C                             flgStato <> '2'
033700010330     C* ... restituisco esito OK.
033800010330     C                   eval      wrkesito = '0'
033900010330     C                   else
034000041108     C                   if        §ctrokvb > 0
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
035300010330     C*
035400010330     C                   if        %open(tivin00r)
035500010330     C                   close     tivin00r
035600010330     C                   endif
035700021025     C                   if        %open(fivabwwr)
035800021025     C                   close     fivabwwr
035900010330     C                   endif
036000041108     C                   if        %open(fivatwwr)
036100041108     C                   close     fivatwwr
036200041108     C                   endif
036300010601     C*
036400010601     C                   if        vlrpoi <> 999
036500010601     C                   eval      invfil = vlrpoi
036600010601     C                   endif
036700010330     C*
036800041108     C                   if        §ctrokvb > 0
036900010601     C                             and invfil > *zeros
037000010330     C                   exsr      invio
037100010330     C                   endif
037200010601     C*
037300010618     C                   if        flgGiro = '1'
037400010601     C                   exsr      endela
037500010618     C                   endif
037600010330     C*
037700010330     C                   ENDSR
037800010330     C***
037900041109
038000041109
038100041109
038200050504     C*-----------------------------------------------------------*
038300050504     C* INTEST - PREPARO LA SCHIERA CON LE INTESTAZIONI DEI CAMPI *
038400050504     C*-----------------------------------------------------------*
038500050504     C     INTEST        BEGSR
038600050504
038700050504     C                   CLEAR                   SkSplitFLD
038800050504     C                   EVAL      i = 1
038900050504     C                   EVAL      posDa = *zeros
039000050504     C                   EVAL      posA  = *zeros
039100050504     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
039200050504     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
039300050504     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
039400050504
039500050504     C**********
039600050504     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
039700050504     C**********
039800050504     C                   IF        intcol(1)+intcol(2)+intcol(3) <> *blanks
039900050504     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
040000050504     C                                      %trim(intcol(3))
040100050504     C                   ENDIF
040200050504     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
040300050504     C     minu:maiu     XLATE     vindta        vindta
040400050504     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
040500050504     C                   DOW       posDa <= %len(%trimr(vindta))
040600050504     C*
040700050504     C* Gestisco il 1° campo
040800050504     C                   IF        i = 1
040900050504     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
041000050504     C                   EVAL      posDa = 1
041100050504     C* Eseguo lo scan x trovare la fine del primo campo
041200050504     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
041300050504     C* A questo punto "estraggo" il campo corrente
041400050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041500050504     C                                                    (posA-posDa))
041600050504     C* X i campi successivi al 1°
041700050504     C                   ELSE
041800050504     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
041900050504     C                   EVAL      posDa = posA + 1
042000050504     C* Eseguo lo scan x trovare la fine del campo corrente
042100050504     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
042200050504     C* Gestisco l'ultimo campo
042300050504     C                   IF        posA = *zeros
042400050504     C                   EVAL      posA = %len(%trimr(vindta)) + 1
042500050504     C* A questo punto "estraggo" il campo corrente
042600050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
042700050504     C                                                    (posA-posDa))
042800050504     C                   LEAVE
042900050504     C                   ELSE
043000050504     C* A questo punto "estraggo" il campo corrente
043100050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
043200050504     C                                                    (posA-posDa))
043300050504     C                   ENDIF
043400050504     C                   ENDIF
043500050504     C* Incremento il contatore d campo
043600050504     C                   EVAL      i = i +1
043700050504     C                   ENDDO
043800050504     C*
043900050504     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
044000050504     C* delimitazione testo
044100050504     C                   EVAL      i = 1
044200050504     C                   DOW       i < %elem(SkSplitFLD)
044300050504     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
044400050504     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
044500050504     C                   EVAL      i = i + 1
044600050504     C                   ENDDO
044700050504
044800050504     C                   ENDSR
044900050504
045000041109
045100041109
045200050504     C*----------------------------------------------------*
045300050504     C*  SCARICAMENTO BUFFER RECORDS VAB
045400050504     C*----------------------------------------------------*
045500050504     C     WRIVAB        BEGSR
045600050504     C*
045700050504     C                   write     fivab000                                     => scarico il VAB
045800050504     C*
045900050504     C                   ENDSR
046000050504
046100050504
046200050504
046300041108     C*----------------------------------------------------*
046400041108     C*  SCARICAMENTO BUFFER RECORDS VAT
046500041108     C*----------------------------------------------------*
046600041108     C     WRIVAT        BEGSR
046700041108     C*
046800041108     C* Attribuisco lo stesso numero numero spedizione del file testate
046900050511     C**!!!              EVAL      VATNSP = VABNSP
047000050511     C                   EVAL      VATNSP = savNSP
047100041109      * Barcode
047200041108     C                   EVAL      VATTRC = 'E'
047300041108     C                   EVAL      VATNOT = wbarcode
047400041108     C                   WRITE     FIVAT000
047500041109     C                   ADD       1             §CTROKVT
047600041109      * Telefono
047700041109     c                   If        wtel <> *Blanks
047800041109     C                   EVAL      VATTRC = 'B'
047900041109     C                   EVAL      VATNOT = wtel
048000041109     C                   WRITE     FIVAT000
048100041109     C                   ADD       1             §CTROKVT
048200041109     c                   EndIf
048300041109      * Referente
048400041109     c                   If        wref <> *Blanks
048500041109     C                   EVAL      VATTRC = 'A'
048600041109     C                   EVAL      VATNOT = wref
048700041109     C                   WRITE     FIVAT000
048800041109     C                   ADD       1             §CTROKVT
048900041109     c                   EndIf
049000041108     C*
049100041108     C                   ENDSR
049200010601
049300010601
049400010601
049500041108     C*----------------------------------------------------*
049600041108     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049700041108     C*----------------------------------------------------*
049800041108     C     PREVAT        BEGSR
049900041108     C*
050000041108     C* Compongo il nome del membro da dare al FIVATWWR
050100041108     C                   eval      parmbr = vlrhdl
050200041108     C                   movel     'M'           parmbr
050300041108     C                   eval      parccm = vlrksc
050400041108     C                   eval      paropz = '1'
050500041108     C* Effettuo la chiamata al CLLE preposto
050600041108     C                   call(e)   'TITVVTC'
050700041108     C                   parm                    parccm
050800041108     C                   parm                    parmbr
050900041108     C                   parm                    paropz
051000041108     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051100041108     C                   if        %error
051200041108     C                   movel     '1'           chkcall
051300041108     C                   else
051400041108     C                   movel     '0'           chkcall
051500041108     C                   endif
051600041108     C*
051700041108     C                   ENDSR
051800041108
051900041108
052000041108
052100010330     C*----------------------------------------------------*
052200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052300010330     C*----------------------------------------------------*
052400010330     C     INZVAR        BEGSR
052500010330     C*
052600030822     C* Inizializzo variabili di wrk
052700020204     C                   Z-ADD     *zeros        Num5_0            5 0
052800041108     C                   clear                   wbarcode
052900041108     C                   clear                   wtel
053000041108     C                   clear                   wref
053100030822     C*
053200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053300030822     C                   CLEAR                   SkSplitCSV
053400041108     c                   CLEAR                   FIVAT000
053500050503     C                   CLEAR                   FIVAB000
053600030822     C                   EVAL      i = 1
053700030822     C                   EVAL      posDa = *zeros
053800030822     C                   EVAL      posA  = *zeros
053900030822     C*
054000030822     C* Reimposto i valori di default
054100030822     C                   EXSR      DEFCAM
054200030822     C*
054300010330     C                   ENDSR
054400010330     C*----------------------------------------------------*
054500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054600010330     C*----------------------------------------------------*
054700010330     C     DEFCAM        BEGSR
054800030822     C*
054900050504     C* Imposto i valori d default a programma dei campi fondamentali
055000050504     C                   EVAL      VABCCM = 0012232
055100050504     C                   EVAL      VATCCM = 0012232
055200050504     C                   EVAL      VABLNP = 001
055300050504     C                   EVAL      VATLNP = 001
055400050504     C                   EVAL      VABCTR = 001
055500050504     C                   EVAL      VABCTM = '7Q'
055600050504     C*
055700050504     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055800050504     C* e delimitatore testo.
055900050504     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056000050504     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056100050504     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056200030822     C*
056300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
056500030715     C                   SETON                                        50
056600030715     C                   ELSE
056700030715     C                   SETOFF                                       50
056800030715     C                   ENDIF
056900030822     C*
057000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057100030822     C                   EVAL      posDaDft = 1
057200030822     C                   EVAL      posADft  = 0
057300030822     C                   EVAL      wGiroDft = 0
057400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057500030822     C                             posDaDft > 0
057600030822     C*
057700030822     C* Gestisco il 1° giro
057800030822     C                   IF        wGiroDft = 0
057900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058100030822     C* Incremento il contatore dei "giri"
058200030822     C                   EVAL      wGiroDft = 1
058300030822     C                   ELSE
058400030822     C                   EVAL      posDaDft = posADft
058500030822     C                   ENDIF
058600030822     C* Eseguo lo scan x trovare la fine del campo corrente
058700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058800030822     C*
058900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059100030822     C                   IF        posDaDft > 0 AND
059200030822     C                             posADft  > 0
059300030822     C* NCL
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'NCL'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300041025     C                   Z-ADD     PiVal         VABNCL
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* CCM
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'CCM'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABCCM
061700041108     C                   Z-ADD     PiVal         VATCCM
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* LNP
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'LNP'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiInt=*on
063000030822     C                   Z-ADD     PiVal         VABLNP
063100041108     C                   Z-ADD     PiVal         VATLNP
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* NRS
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'NRS'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiInt=*on
064400030822     C                   Z-ADD     PiVal         VABNRS
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* CTR
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'CTR'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiInt=*on
065700030822     C                   Z-ADD     PiVal         VABCTR
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* PKB
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'PKB'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiNum=*on
067000030822     C                   Z-ADD     PiVal         VABPKB
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* VLB
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'VLB'
067800030822     C                   EVAL      PiStr=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   EXSR      CHKNUM
068200030822     C                   IF        PiNum=*on
068300030822     C                   Z-ADD     PiVal         VABVLB
068400030822     C                   ENDIF
068500030822     C                   ENDIF
068600030822     C* QFT
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'QFT'
069100030822     C                   EVAL      PiStr=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   EXSR      CHKNUM
069500030822     C                   IF        PiNum=*on
069600030822     C                   Z-ADD     PiVal         VABQFT
069700030822     C                   ENDIF
069800030822     C                   ENDIF
069900030822     C* CBO
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'CBO'
070400030822     C                   EVAL      VABCBO=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   ENDIF
070800030822     C* TSP
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'TSP'
071300030822     C                   EVAL      VABTSP=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   ENDIF
071700030822     C* VAS
071800030822     C                   IF        %subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):1:3)
072100030822     C                             = 'VAS'
072200030822     C                   EVAL      VABVAS=%trim(%subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500030822     C                   ENDIF
072600030822     C* VCA
072700030822     C                   IF        %subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):1:3)
073000030822     C                             = 'VCA'
073100030822     C                   EVAL      VABVCA=%trim(%subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073400030822     C                   ENDIF
073500030822     C* GCA
073600030822     C                   IF        %subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):1:3)
073900030822     C                             = 'GCA'
074000030822     C                   EVAL      VABGCA=%trim(%subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300030822     C                   ENDIF
074400030822     C* CTM
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'CTM'
074900030822     C                   EVAL      VABCTM=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300030822     C* FFD
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'FFD'
075800030822     C                   EVAL      VABFFD=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200030822     C* VAD
076300030822     C                   IF        %subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):1:3)
076600030822     C                             = 'VAD'
076700030822     C                   EVAL      VABVAD=%trim(%subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000030822     C                   ENDIF
077100030822     C* GMA
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'GMA'
077600030822     C                   EVAL      VABGMA=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000030822     C* GGA
078100030822     C                   IF        %subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):1:3)
078400030822     C                             = 'GGA'
078500030822     C                   EVAL      VABGGA=%trim(%subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800030822     C                   ENDIF
078900030822     C* GVA
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'GVA'
079400030822     C                   EVAL      VABGVA=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* TC1
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'TC1'
080300030822     C                   EVAL      VABTC1=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700030822     C* TC2
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100030822     C                             = 'TC2'
081200030822     C                   EVAL      VABTC2=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600030822     C* ...
081700030822     C                   ENDIF
081800030822     C                   ENDDO
081900020204     C*
082000010330     C                   ENDSR
082100010607     C*----------------------------------------------------*
082200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082300010607     C*----------------------------------------------------*
082400010607     C     IMPVAB        BEGSR
082500010607     C*
082600010607     C                   SETOFF                                       3132
082700030515     C*
082800010607     C                   MOVEL     datcor        VABAAS
082900041108     C                   MOVEL     datcor        VATAAS
083000010607     C                   MOVE      datcor        VABMGS
083100010607     C*
083200031204     C**********
083300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
083400031204     C* d separatore campo
083500031204     C**********
083600040726     C                   EVAL      vindta = %trim(vindta)
083700040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083800040726     C                   EVAL      vindta = %subst(vindta:2)
083900040726     C                   ENDDO
084000031204     C*
084100031204     C                   Z-ADD     *zeros        lunghInput        4 0
084200040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
084300040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
084400040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084500040726     C                   EVAL      lunghInput = %len(%trim(vindta))
084600040726     C                   ENDDO
084700030715     C**********
084800030715     C* Effettuo lo split del campo dati d input
084900030715     C**********
085000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085100040324     C                   DOW       posDa <= %len(%trimr(vindta))
085200030715     C*
085300030715     C* Gestisco il 1° campo
085400030715     C                   IF        i = 1
085500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085600030715     C                   EVAL      posDa = 1
085700030715     C* Eseguo lo scan x trovare la fine del primo campo
085800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085900030715     C* A questo punto "estraggo" il campo corrente
086000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086100030715     C                                                    (posA-posDa))
086200030715     C* X i campi successivi al 1°
086300030715     C                   ELSE
086400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086500030715     C                   EVAL      posDa = posA + 1
086600031204     C* Verifico che nn vi sia il campo nn valorizzato
086700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086800031204     C* Se campo nn valorizzato skippo al prossimo
086900031204     C                   EVAL      SkSplitCSV(i) = *blanks
087000031204     C                   EVAL      posA  = posA + 1
087100031204     C                   ELSE
087200030715     C* Eseguo lo scan x trovare la fine del campo corrente
087300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087400030715     C* Gestisco l'ultimo campo
087500030715     C                   IF        posA = *zeros
087600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C                   LEAVE
088100030715     C                   ELSE
088200030715     C* A questo punto "estraggo" il campo corrente
088300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088400030715     C                                                    (posA-posDa))
088500030715     C                   ENDIF
088600031204     C                   ENDIF
088700030715     C                   ENDIF
088800030715     C* Incremento il contatore d campo
088900030715     C                   EVAL      i = i +1
089000030715     C                   ENDDO
089100030715     C*
089200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089300030715     C* delimitazione testo
089400030715     C                   EVAL      i = 1
089500030715     C                   DOW       i < %elem(SkSplitCSV)
089600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089800030715     C                   EVAL      i = i + 1
089900030715     C                   ENDDO
090000030715     C*
090100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090200030715     C                   EVAL      i = 1
090300030715     C                   DOW       i < %elem(SkSplitFLD)
090400030715     C*
090500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090700030715     C                             SkSplitCSV(i) <> *blanks
090800030721     C*** ATB
090900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
091000030721     C                   EVAL      VABATB = SkSplitCSV(i)
091100030721     C                   ENDIF
091200030715     C*** RSD
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
091400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
091500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091700030715     C* ==
091800030715     C                   ENDIF
091900030721     C*** RD2
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
092100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030715     C*** IND
092400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
092500030715     C                   EVAL      VABIND = SkSplitCSV(i)
092600030715     C                   ENDIF
092700030715     C*** LOD
092800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
093000030715     C                   ENDIF
093100030715     C*** PRD
093200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
093300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
093400030715     C                   ENDIF
093500030721     C*** NZD
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030715     C*** RMA
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
094100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094200030715     C                   ENDIF
094300030715     C*** NOT
094400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
094500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094600030715     C                   ENDIF
094700030715     C*** NT2
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095000030715     C                   ENDIF
095100030715     C*** VCA
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
095400030715     C                   ENDIF
095500030715     C*** VAS
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** FFD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** CBO
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
096500030715     C                   EVAL      VABCBO = SkSplitCSV(i)
096600041108     c                   if        vabcbo = 'F'
096700041108     c                   eval      vabcbo = '1'
096800041108     c                   endif
096900041108     c                   if        vabcbo = 'A'
097000041108     c                   eval      vabcbo = '2'
097100041108     c                   endif
097200030715     C                   ENDIF
097300030715     C
097400030721     C*** GC1
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** GC2
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** TSP
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
098400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** NAS
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** TIC
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
099200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** GCA
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** XCO
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
100000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** CTM
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
100400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** TCR
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** CTS
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
101200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** FTM
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** VAD
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
102000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GMA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
102400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** GGA
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** GVA
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
103200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TC1
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103600050503     c                   select
103700050503     c                   when      sksplitcsv(i) <> *Blanks and vabtc1 = *Blanks
103800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103900050503     c                   when      sksplitcsv(i) <> *Blanks and vabtc2 = *Blanks
104000050503     C                   EVAL      VABTC2 = SkSplitCSV(i)
104100050503     c                   Endsl
104200030721     C                   ENDIF
104300030721     C*** TC2
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
104500050503     c                   select
104600050503     c                   when      sksplitcsv(i) <> *Blanks and vabtc1 = *Blanks
104700050503     C                   EVAL      VABTC1 = SkSplitCSV(i)
104800050503     c                   when      sksplitcsv(i) <> *Blanks and vabtc2 = *Blanks
104900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105000050503     c                   Endsl
105100030721     C                   ENDIF
105200030721     C*** SCL
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** RMO
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** NMO
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*
106500030721     C* Reperisco quindi i campi numerici...
106600030721     C*** CAD
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106800030721     C                   EVAL      PiStr=SkSplitCSV(i)
106900030721     C                   EXSR      CHKNUM
107000030721     C                   IF        PiInt=*on
107100030721     C                   Z-ADD     PiVal         Num5_0
107200030721     C                   MOVEL(p)  Num5_0        VABCAD
107300030721     C                   ELSE
107400030721     C                   SETON                                        32
107500030721     C                   EVAL      VABCAD = *zeros
107600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107700030721     C                             + ' ' + 'VABCAD'
107800030721     C                   ENDIF
107900030721     C                   ENDIF
108000041122     C*** RMN
108100041122     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108200041122     C                   EVAL      PiStr=SkSplitCSV(i)
108300041122     C                   EXSR      CHKNUM
108400041122     C                   IF        PiInt=*on
108500041122     C                   Z-ADD     PiVal         VABRMN
108600041122     C                   ELSE
108700041122     C                   SETON                                        32
108800041122     C                   EVAL      VABRMN = *zeros
108900041122     C                   EVAL      vinmsg = %trimr(vinmsg)
109000041122     C                             + ' ' + 'VABRMN'
109100041122     C                   ENDIF
109200041122     C* NSP => Stacco un numeratore da AZNUM
109300041122      * a cambio di RMN
109400041122     c                   If        VabRmn <> savrmn
109500041122     C                   clear                   TRUL33DS
109600041122     C                   eval      I33OPE = *zeros
109700041122     C                   eval      I33CNU = 302
109800041122     C                   eval      I33NUM = 1
109900041122     C                   movel     TRUL33DS      KPJBU
110000041122     C                   call      'TRUL33R'
110100041122     C                   parm                    KPJBA
110200041122     C                   movel     KPJBU         TRUL33DS
110300041122     C                   if        O33ERR = *zeros
110400041122     C                   z-add     O33NRF        VABNSP
110500041122     C                   else
110600041122     C                   seton                                        32
110700041122     C                   EVAL      vinmsg = %trimr(vinmsg)
110800041122     C                             + ' ' + 'VABNSP'
110900041122     C                   endif
111000050511     c                   Eval      savnsp = vabnsp
111100041122     c                   Eval      savrmn = vabrmn
111200041122     C                   endif
111300050511     C                   ENDIF
111400030721     C*** NCL
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiInt=*on
111900041108     C                   Z-ADD     PiVal         VABNCL
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      VABNCL = *zeros
112300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112400030721     C                             + ' ' + 'VABNCL'
112500030721     C                   ENDIF
112600030721     C                   ENDIF
112700030721     C*** PKB
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112900030721     C                   EVAL      PiStr=SkSplitCSV(i)
113000030721     C                   EXSR      CHKNUM
113100030721     C                   IF        PiNum=*on
113200041108     C                   Z-ADD     PiVal         VABPKB
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      VABPKB = *zeros
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700030721     C                             + ' ' + 'VABPKB'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** CAS
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiNum=*on
114500030721     C                   Z-ADD     PiVal         VABCAS
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABCAS = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABCAS'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** CCM
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on      AND
115800030721     C                             PiVal<=9999999 AND
115900030721     C                             PiVal>*zeros
116000030721     C                   Z-ADD     PiVal         VABCCM
116100041108     C                   Z-ADD     PiVal         VATCCM
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABCCM = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600041108     C                             + ' ' + 'VABCCM VATCCM'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** LNP
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on    AND
117400030721     C                             PiVal<=999   AND
117500030721     C                             PiVal>*zeros
117600030721     C                   Z-ADD     PiVal         VABLNP
117700041108     C                   Z-ADD     PiVal         VATLNP
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      VABLNP = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200041108     C                             + ' ' + 'VABLNP VATLNP'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** NRS
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiInt=*on AND
119000030721     C                             PiVal<=99
119100030721     C                   Z-ADD     PiVal         VABNRS
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABNRS'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** LNA
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on    AND
120300030721     C                             PiVal<=999   AND
120400030721     C                             PiVal>*zeros
120500030721     C                   Z-ADD     PiVal         VABLNA
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      VABLNA = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABLNA'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** CTR
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on  AND
121800030721     C                             PiVal<=999
121900030721     C                   Z-ADD     PiVal         VABCTR
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCTR'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** IAS
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiNum=*on
123100030721     C                   Z-ADD     PiVal         VABIAS
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABIAS'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** VLB
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABVLB
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABVLB'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** QFT
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiNum=*on
125500030721     C                   Z-ADD     PiVal         VABQFT
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABQFT'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** NCD
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on      AND
126700030721     C                             PiVal<=9999999
126800030721     C                   Z-ADD     PiVal         VABNCD
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABNCD'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** NCA
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on      AND
128000030721     C                             PiVal<=9999999
128100030721     C                   Z-ADD     PiVal         VABNCA
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABNCA'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** ZNC
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on AND
129300030721     C                             PiVal<=99
129400030721     C                   Z-ADD     PiVal         VABZNC
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABZNC'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** DCR
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on       AND
130600030721     C                             PiVal<=99999999
130700030721     C                   Z-ADD     PiVal         VABDCR
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABDCR'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** HCR
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on   AND
131900030721     C                             PiVal<=9999
132000030721     C                   Z-ADD     PiVal         VABHCR
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABHCR'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** VMD
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiNum=*on
133200030721     C                   Z-ADD     PiVal         VABVMD
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABVMD'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900030721     C*** ANT
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiInt=*on        AND
134400030721     C                             PiVal<=999999999
134500030721     C                   Z-ADD     PiVal         VABANT
134600030721     C                   ELSE
134700030721     C                   SETON                                        32
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABANT'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200030721     C*** CMO
135300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiInt=*on
135700030721     C                   Z-ADD     PiVal         Num5_0
135800030721     C                   MOVEL(p)  Num5_0        VABCMO
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      VABCMO = *zeros
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABCMO'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600041108
136700041108     c* Gestico i campi "personalizzati"
136800041108     c* BARCODE
136900041108     C                   IF        %trim(SkSplitFLD(i)) = 'BARCODE'
137000041108     C                   EVAL      wbarcode = SkSplitCSV(i)
137100041108     C                   ENDIF
137200041108     c* TEL
137300041108     C                   IF        %trim(SkSplitFLD(i)) = 'TEL'
137400041108     C                   EVAL      wtel = SkSplitCSV(i)
137500041108     C                   ENDIF
137600041108     c* REF
137700041108     C                   IF        %trim(SkSplitFLD(i)) = 'REF'
137800041108     C                   EVAL      wref = SkSplitCSV(i)
137900041108     C                   ENDIF
138000030715     C*
138100030715     C                   ENDIF
138200030715     C                   EVAL      i = i + 1
138300030715     C                   ENDDO
138400050504     C*
138500050504     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
138600050504     C* Reperisco la provincia dal CAP e dalla località
138700050504     C                   IF        VABCAD <> *blanks
138800050504     C                   CLEAR                   TISI95DS
138900050504     C                   EVAL      I95TCN = '3'
139000050504     C                   Z-ADD     datcor        I95DAT
139100050504     C                   EVAL      I95CAP = VABCAD
139200050504     C                   EVAL      I95LOC = VABLOD
139300050504     C                   CALL      'TISI95R'
139400050504     C                   PARM                    TISI95DS
139500050504     C                   EVAL      VABPRD = O95PRV
139600050504     C                   ENDIF
139700040726     C*
139800040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
139900040726     C                   IF        VABLNP = *zeros
140000040726     C                   SETON                                        31
140100050504     C                   EVAL      vinmsg = %trimr(vinmsg)
140200050504     C                             + ' ' + 'VABLNP'
140300040726     C                   ENDIF
140400050504     C*
140500050504     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
140600050504     C  n31              EXSR      wrivat
140700040726     C*
140800040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
140900040726     C* d valorizzarli sempre entrambi
141000040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
141100040726     C                   Z-ADD     VABNSP        VABRMN
141200040726     C                   ENDIF
141300040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
141400040726     C                   MOVEL     VABRMN        VABRMA
141500040726     C                   ENDIF
141600040726     C*
141700040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
141800040726     C                   IF        VABCAS > *zeros
141900040726     C                   IF        VABVCA = *blanks
142000040726     C                   EVAL      VABVCA = 'EUR'
142100040726     C                   ENDIF
142200040726     C                   IF        VABCBO = '1'
142300040726     C                   EVAL      VABCBO = '4'
142400040726     C                   ENDIF
142500040726     C                   IF        VABCBO = '2'
142600040726     C                   EVAL      VABCBO = '6'
142700040726     C                   ENDIF
142800040726     C                   ENDIF
142900020204     C*
143000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
143100020204     C                   EXSR      CHKIMPDIV
143200020204     C*
143300010607     C                   ENDSR
143400010607     C*----------------------------------------------------*
143500010601
143600041109
143700041109
143800020204
143900020204     C*----------------------------------------------------*
144000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
144100020204     C*----------------------------------------------------*
144200020204     C     CHKIMPDIV     BEGSR
144300020204     C*
144400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
144500020204     C                   Z-ADD     *zeros        wrkDec            9 9
144600020204     C*
144700020204     C* Come prima cosa effettuo considerazioni sulla divisa
144800020204     C                   IF        vabIAS > *zeros
144900020204     C                   IF        vabVAS <> 'EUR'
145000020204     C                   EVAL      vabVAS =  'ITL'
145100020204     C                   ENDIF
145200020204     C                   ENDIF
145300020204     C*
145400020204     C                   IF        vabCAS > *zeros
145500020204     C                   IF        vabVCA <> 'EUR'
145600020204     C                   EVAL      vabVCA =  'ITL'
145700020204     C                   ENDIF
145800020204     C                   ENDIF
145900020204     C*
146000020204     C                   IF        vabVMD > *zeros
146100020204     C                   IF        vabVAD <> 'EUR'
146200020204     C                   EVAL      vabVAD =  'ITL'
146300020204     C                   ENDIF
146400020204     C                   ENDIF
146500020204     C*
146600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
146700020204     C                   Z-ADD     vabIAS        wrkDec
146800020204     C                   IF        wrkDec > *zeros
146900020204     C                   IF        vabVAS = 'ITL'
147000020204     C                   EVAL      vabIAS = *zeros
147100020204     C                   ENDIF
147200020204     C                   ENDIF
147300020204     C*
147400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
147500020204     C                   Z-ADD     vabCAS        wrkDec
147600020204     C                   IF        wrkDec > *zeros
147700020204     C                   IF        vabVCA = 'ITL'
147800020204     C                   EVAL      vabCAS = *zeros
147900020204     C                   ENDIF
148000020204     C                   ENDIF
148100020204     C*
148200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
148300020204     C                   Z-ADD     vabVMD        wrkDec
148400020204     C                   IF        wrkDec > *zeros
148500020204     C                   IF        vabVAD = 'ITL'
148600020204     C                   EVAL      vabVMD = *zeros
148700020204     C                   ENDIF
148800020204     C                   ENDIF
148900020204     C*
149000020204     C                   ENDSR
149100020204     C***
149200020204
149300010330
149400010330
149500010330     C*----------------------------------------------------*
149600010330     C*  CONTROLLO NUMERICITA' CAMPI
149700010330     C*----------------------------------------------------*
149800010330     C     CHKNUM        BEGSR
149900010330     C*
150000010606     C                   IF        PiDecChr = *blanks
150100030715     C                   EVAL      PiDecChr = CharNUM
150200010606     C                   ENDIF
150300010606     C*
150400010606     C                   CALL(e)   'ISNUMERIC'
150500010330     C                   PARM                    PiStr            30
150600010606     C                   PARM                    PiDecChr          1
150700010330     C                   PARM      *ZEROS        PiVal            30 9
150800010330     C                   PARM      '0'           PiInt             1
150900010330     C                   PARM      '0'           PiNum             1
151000010330     C                   IF        %error
151100010606     C                   EVAL      PiNum=*off
151200010330     C                   ENDIF
151300010330     C*
151400010330     C                   ENDSR
151500010330     C***
151600010330
151700010601
151800010601
151900010601
152000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
152100010601     C     repfil        BEGSR
152200010601     C*
152300010601     C                   if        invfil = *zeros and
152400010601     C                             depfil > *zeros and
152500010629     C                             (vinflg = *blanks or
152600010629     C                              vinflg = *zeros)
152700010601     C
152800010601     C                   eval      invfil = depfil
152900010601     C                   endif
153000010601     C*
153100010601     C                   if        depfil <> invfil and
153200010601     C                             invfil > *zeros
153300010601     C                   eval      flgMulti = '1'
153400010601     C                   if        vinflg = *blanks
153500010601     C                   add       1             cntNonEl
153600010601     C                   endif
153700010601     C                   endif
153800010601     C*
153900010601     C                   if        vinflg = '2'
154000010601     C                   eval      flgStato = '2'
154100010601     C                   endif
154200010601     C*
154300010601     C                   ENDSR
154400010601     C***
154500010601
154600010601
154700010601
154800010330
154900010330
155000010330
155100990920      /TITLE Invio dei dati al punto operativo.
155200000613     C     invio         BEGSR
155300041108
155400041108     C* 1° invio FIVAT
155500041108     C                   reset                   dscmz
155600041108     C                   move      invfil        cmzdst
155700041108     C                   eval      cmzfld = 'FIVATWWR'
155800041108     C                   eval      cmzmbd = vlrhdl
155900041108     C                   eval      %subst(cmzmbd:1:1) = 'M'
156000041108     C                   eval      cmzfla = 'FIVAT00F'
156100041108     C                   eval      cmzmba = 'FIVAT00F'
156200041108     C                   eval      cmznrr = *zeros
156300041108     C                   move      §ctrokvt      cmznrr
156400041108     C                   eval      cmzlba = vlrfl1
156500041108     C                   call(e)   'TIS711C'
156600041108     C                   parm                    dscmz
156700041108     C                   parm      *blanks       esito
156800041108     C                   if        %error
156900041108     C                             or cmzerr = '1'
157000041108     C                             or esito  = '1'
157100041108     C                   eval      wrkesito = '3'
157200041108     C                   else
157300990920     C*
157400041108     C* 2° invio FIVAB
157500990920     C                   reset                   dscmz
157600010601     C                   move      invfil        cmzdst
157700990920     C                   eval      cmzfld = vlrfou
157800990920     C                   eval      cmzmbd = vlrhdl
157900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
158000021025     C                   eval      cmzfla = 'FIVAB00F'
158100021025     C                   eval      cmzmba = 'FIVAB00F'
158200990920     C                   eval      cmznrr = *zeros
158300041108     C                   move      §ctrokvb      cmznrr
158400021018     C                   eval      cmzlba = vlrfl1
158500990920     C                   call(e)   'TIS711C'
158600990920     C                   parm                    dscmz
158700990921     C                   parm      *blanks       esito
158800990920     C                   if        %error
158900990920     C                             or cmzerr = '1'
159000990921     C                             or esito  = '1'
159100000710     C                   eval      wrkesito = '3'
159200990920     C                   endif
159300041108     C                   endif
159400990920     C*
159500000613     C                   ENDSR
159600990910
159700010601
159800010601
159900010601
160000010601
160100010601      /TITLE Invio dei dati al punto operativo.
160200010601     C     opeini        BEGSR
160300010601     C*
160400010601     C* Inizializzo flag e contatori operativi
160500010601     C                   movel     '0'           flgGiro           1
160600010601     C                   movel     '0'           flgMulti          1
160700010601     C                   movel     '1'           flgStato          1
160800010615     C                   movel     '0'           flgOk             1
160900010601     C                   z-add     *zeros        cntNonEl         10 0
161000010601     C                   z-add     *zeros        depfil            3 0
161100010601     C                   z-add     *zeros        invfil            3 0
161200010601     C*
161300010601     C                   ENDSR
161400010601     C***
161500010601
161600010601
161700050421
161800050421
161900070411
162000070411     C     *pssr         BEGSR
162100070411     C*
162200070411     C                   if        %open(tivin00r)
162300070411     C                   close     tivin00r
162400070411     C                   endif
162500070411     C                   if        %open(fivabwwr)
162600070411     C                   close     fivabwwr
162700070411     C                   endif
162800070411     C                   if        %open(fivatwwr)
162900070411     C                   close     fivatwwr
163000070411     C                   endif
163100070411     C*
163200070411     C* Effettuo la chiamata al CLLE preposto
163300070411     C                   call(e)   'TITVVTC'
163400070411     C                   parm                    parccm
163500070411     C                   parm                    parmbr
163600070411     C                   parm      '2'           paropz
163700070411     C*
163800070411     C                   eval      wrkesito = '2'
163900070411     C*
164000070411     C                   seton                                        LR
164100070411     C*
164200070411     C                   ENDSR     '*CANCL'
164300070411     C***
164400070411
164500050421
164600010601
164700010330
164800010330
164900000613     C     *inzsr        BEGSR
165000990910     C*
165100990910     C     *entry        plist
165200990920     C                   parm                    tivlrds
165300990921     C                   parm      wrkesito      esito
165400000724     C                   parm                    prmlit
165500000710     C                   parm                    prmfir
165600010330     C*
165700010330     C* CALCOLA LA DATA CORRENTE
165800010330     C                   time                    wn14             14 0
165900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
166000010330     C                   z-add     wn8           g08dat
166100010330     C                   z-add     *zeros        g08inv
166200010330     C                   movel     '0'           g08err
166300010330     C                   call      'XSRDA8'
166400010330     C                   parm                    wlbda8
166500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
166600000613     C*
166700000613     C                   ENDSR
166800000613     C***
166900990908
167000050504     Otitvf8p   E            riepilogo         2
167100990915     O                                              'Upload via Internet'
167200990915     O                                           +1 'Traduzione TIVIN00R -'
167300021025     O                                           +1 'FIVABWWR'
167400990915     O                                           +1 'Testate bolle'
167500990915     O          E            riepilogo   2
167600990915     O                       wrkdata
167700990915     O                       wrkora              +1
167800990915     O                       procname            +1
167900990915     O          E            riepilogo   2
168000990915     O                                              'Cliente..................:'
168100010605     O                       VABCCM        z     +1
168200990915     O          E            riepilogo   2
168300990920     O                                              'Riferimento Strategi.....:'
168400990920     O                       vlrhdl              +1
168500990915     O          E            riepilogo   2
168600990915     O                                              'Giusti...................:'
168700041108     O                       §CTROKVB      2   +  1
168800990915     O          E            riepilogo   2
168900990915     O                                              'Sbagliati e corretti.....:'
169000971022     O                       §CTRMO        2   +  1
169100990915     O          E            riepilogo   2
169200990915     O                                              'Sbagliati e scartati.....:'
169300971022     O                       §CTRNO        2   +  1
169400000613
169500050504     Otitvf8ps  E            testdett          2
169600000613     O                                              'Upload via Internet'
169700000613     O                                           +1 'Traduzione TIVIN00R -'
169800021025     O                                           +1 'FIVABWWR'
169900000613     O                                           +1 'Testate bolle'
170000000616     O          E            testdett    3
170100000613     O                                           +2 'N° rec'
170200000613     O                                           +3 'Anteprima contenuto'
170300000616     O          E            rigadett    2
170400000613     O                       rrnum               +2
170500000621     O                       recko               +3
170600000616     O          E            findett     2
170700000613     O                       wrkdata
170800000613     O                       wrkora              +1
170900000613     O                       procname            +1
171000000616     O          E            findett     2
171100000613     O                                              'Cliente..................:'
171200010605     O                       VABCCM        z     +1
171300000616     O          E            findett     2
171400000613     O                                              'Riferimento Strategi.....:'
171500000613     O                       vlrhdl              +1
171600000616     O          E            findett     2
171700000613     O                                              'Giusti...................:'
171800041108     O                       §CTROKVB      2   +  1
171900000616     O          E            findett     2
172000000613     O                                              'Sbagliati e corretti.....:'
172100000613     O                       §CTRMO        2   +  1
172200000616     O          E            findett     2
172300000613     O                                              'Sbagliati e scartati.....:'
172400000613     O                       §CTRNO        2   +  1
172500000613** CMD - COMANDI CL
172600050504OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
172700050504OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
172800050504DLTOVR FILE(TITVF8P TITVF8PS) LVL(*)
172900010606
173000000613
173100040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
173200041109Filler0|Filler1|Filler2|VABCBO|VABRSD|VABRD2|VABIND|VABLOD|VABCAD|VABPRD|
173300050503VABPKB|VABCAS|VABVCA|VABRMA|BARCODE|VABNCL|TEL|REF|VABRMN|VABTSP|VABTC1|VABTC2
173400040726
