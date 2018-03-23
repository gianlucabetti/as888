000100050712      /TITLE Upload via Internet: traduzione in FIVABWWR/FIBATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700041108     FFIVATwwr  O    E             DISK    usropn
000800050712     Ftitvn3p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050712     Ftitvn3ps  O    f  198        PRINTER usropn
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
004100050504     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004200041108     D parccm          s              8    INZ(*blanks)
004300041108     D parmbr          s             10    INZ(*blanks)
004400041108     D paropz          s              1    INZ(*blanks)
004500041108     D chkcall         s              1    INZ(*blanks)
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
005700030822     D*------------------
005800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005900030822     D*------------------
006000030822     D posDaDft        S              3  0 INZ(*zeros)
006100030822     D posADft         S              3  0 INZ(*zeros)
006200030822     D j               s              3  0 INZ(1)
006300030822     D wGiroDft        s              1  0 INZ(*zeros)
006400010330     D*------------------
006500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600010330     D*------------------
006700010330     D WLBDA8          DS                  INZ
006800010330     D  G08DAT                 1      8  0
006900010330     D  G08INV                 9     16  0
007000010330     D  G08ERR                17     17
007100010330     D  G08TGI                18     22  0
007200031201     D*------------------
007300031201     D* Costanti
007400031201     D*------------------
007500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007700010330
007800010330
007900010330
008000990915     C                   time                    wrkdata
008100990915     C                   time                    wrkora
008200000913     C                   reset                   rrnum
008300990921     C                   reset                   esito
008400990921     C                   reset                   wrkesito
008500010601     C*
008600010601     C                   exsr      opeini
008700010605     C                   exsr      rwvab
008800010601     C*
008900010601     C                   seton                                        lr
009000010601
009100010601
009200010601
009300010601
009400010601     C*--------------------------------------------------------
009500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009600010601     C*--------------------------------------------------------
009700010601     C     PREELA        BEGSR
009800010601     C*
009900010601     C* SE OCCORRE SPEDIRE IN FILIALE
010000010601     C                   if        invfil <> *zeros and
010100010601     C                             flgGiro = '0'
010200010601     C*
010300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010400010601     C                   eval      flgGiro = '1'
010500000724     C*
010600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010800010601     C     invfil        chain     azorg01l
010900000724     C                   if        %found
011000000724     C                   movel(p)  CMD(1)        depcmd
011100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011200000724     C*
011300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000724     C                   PARM                    depcmd
011700000724     C                   PARM                    LENGH
011800000724     C*
011900000724     C                   endif
012000000616     C*
012100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012200000616     C                   movel(p)  CMD(2)        depcmd
012300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012400000616     C*
012500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    depcmd
012900000616     C                   PARM                    LENGH
013000000613     C*
013100050712     C                   if        not %open(titvn3ps)
013200050712     C                   open      titvn3ps
013300030715     C                   except    testdett
013400000613     C                   endif
013500010601     C*
013600010601     C                   endif
013700010601     C*
013800010601     C                   ENDSR
013900010601     C***
014000010601
014100010601
014200010601
014300010601
014400010601     C*--------------------------------------------------------
014500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014600010601     C*--------------------------------------------------------
014700010601     C     ENDELA        BEGSR
014800990915     C*
014900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015000041108     C*
015100041108     C* Effettuo la chiamata al CLLE preposto
015200041108     C                   call(e)   'TITVVTC'
015300041108     C                   parm                    parccm
015400041108     C                   parm                    parmbr
015500041108     C                   parm      '2'           paropz
015600050504     C*
015700050504     C* Effettuo lancio TISI95 solo x chiusura
015800050504     C                   CLEAR                   TISI95DS
015900050504     C                   EVAL      I95TLA = 'C'
016000050504     C                   CALL      'TISI95R'
016100050504     C                   PARM                    TISI95DS
016200000613     C*
016300050712     C                   if        %open(titvn3ps)
016400000616     C                   except    findett
016500050712     C                   close     titvn3ps
016600000613     C                   endif
016700000616     C*
016800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016900011113     C                   Z-ADD     150           LENGH            15 5
017000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017100000616     C                   PARM                    CMD(3)
017200000616     C                   PARM                    LENGH
017300000616     C*
017400010601     C                   ENDSR
017500010601     C***
017600010601
017700010601
017800010601
017900010601
018000000613
018100000613     C*--------------------------------------------------------
018200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018300000613     C*--------------------------------------------------------
018400000621     C     STPR          BEGSR
018500000613     C*
018600050712     C                   if        not %open(titvn3p)
018700050712     C   50              open      titvn3p
018800990915     C                   endif
018900990915     C*
019000030715     C   50              except    riepilogo
019100990915     C*
019200050712     C                   if        %open(titvn3p)
019300050712     C   50              close     titvn3p
019400990914     C                   endif
019500000613     C*
019600000613     C                   ENDSR
019700000613     C***
019800000613
019900010330
020000010601
020100010601
020200010601
020300010330     C*--------------------------------------------------------
020400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020500010330     C*--------------------------------------------------------
020600010605     C     RWVAB         BEGSR
020700010330     C*
020800010330     C                   if        not %open(tivin00r)
020900010330     C                   open      tivin00r
021000010330     C                   endif
021100021025     C                   if        not %open(fivabwwr)
021200021025     C                   open      fivabwwr
021300010330     C                   endif
021400010330     C*
021500041108     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021600041108     C                   exsr      prevat
021700041108     C*
021800041108     C                   if        chkcall = '0'
021900041108     C*
022000041108     C                   if        not %open(fivatwwr)
022100041108     C                   open      fivatwwr
022200041108     C                   endif
022300041108     C*
022400041108     C                   clear                   §CTROKVB
022500041109     C                   clear                   §CTROKVT          7 0
022600010604     C                   clear                   §CTRMO
022700010604     C                   clear                   §CTRNO
022800050504
022900050504      * per prima cosa mi imposto la schiera con le intestazioni dei campi
023000050504     c                   EXSR      INTEST
023100010330     C*
023200030822     C                   do        *HIVAL
023300010330     C*
023400030822     C                   read      tivin00r                               70
023500010618     C*
023600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023700010618     C                   if        vinflg = '1'
023800010618     C                   eval      flgOk = '1'
023900010618     C                   endif
024000010618     C*
024100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024200040510     C                   z-add     *zeros        wLenVINDTA        4 0
024300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024700040510     C                   z-add     1             wFlgVINDTA
024800040510     C                   leave
024900040510     C                   endif
025000040510     C                   enddo
025100040510     C*
025200040218     C                   if        vindta > *blanks AND
025300040510     C                             wFlgVINDTA = 1
025400010330     C                   add       1             rrnum
025500010330     C*
025600010601     C                   if        *in70 = *off and
025700010330     C                             (vinflg = *blanks
025800010330     C                              or vinflg = '0'
025900010330     C                              or vinflg = '2')
026000010330     C*
026100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026200010711     C                   if        vinflg = *blanks or vinflg = '0'
026300010711     C                   clear                   vinmsg
026400010711     C                   endif
026500010601     C*
026600010330     C                   exsr      inzvar
026700010601     C*
026800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026900010605     C                   eval      depfil = VABLNP
027000010601     C                   exsr      repfil
027100010601     C                   if        depfil = invfil
027200021025     C                   if        vlrpoi = 999
027300030822     C                   move(P)   invfil        VABFGS
027400041108     C                   move(P)   invfil        VATFGS
027500021025     C                   else
027600030822     C                   move(P)   vlrpoi        VABFGS
027700041108     C                   move(P)   vlrpoi        VATFGS
027800021025     C                   endif
027900050713     C*
028000050713     C                   exsr      impvab
028100010601     C*
028200010601     C                   exsr      PREELA
028300010601     C*
028400010604     C* Ebbene...
028500010604     C*
028600041108     C  N31              add       1             §CTROKVB          7 0
028700030822     C   32              add       1             §CTRMO            7 0
028800030822     C   31              add       1             §CTRNO            7 0
028900050504     C                   if        depspe =  *zeros OR
029000050712     C                             depspe <> vabNSP
029100050504     C  N31              exsr      wrivab
029200050712     C  N31              eval      depspe = vabNSP
029300050504     C                   endif
029400010604     C*
029500010604     C                   if        *in31 = *off and
029600010604     C                             *in32 = *off
029700010604     C                   eval      vinflg = '1'
029800010604     C                   else
029900010604     C                   eval      recko = vindta
030000010604     C                   except    rigadett
030100010604     C                   eval      vinflg = '2'
030200010604     C                   endif
030300010604     C                   endif
030400010601     C*
030500010604     C                   endif
030600010604     C*
030700010330     C                   else
030800010330     C                   eval      vinflg = '1'
030900010330     C                   endif
031000010601     C*
031100010601     C  N70              update    tivin000
031200010330     C*
031300030822     C  N70              enddo
031400041025     C*
031500041109     C                   endif
031600010601     C*
031700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031800010601     C                   if        cntNonEl = *zeros or
031900010601     C                             flgMulti = '0'
032000010330     C* Se non ci sono record con errori ...
032100010601     C                   if        §ctrno = 0 and
032200010604     C                             §ctrmo = 0 and
032300010601     C                             flgStato <> '2'
032400010330     C* ... restituisco esito OK.
032500010330     C                   eval      wrkesito = '0'
032600010330     C                   else
032700041108     C                   if        §ctrokvb > 0
032800010330     C                   eval      wrkesito = '1'
032900010330     C                   else
033000010615     C                   if        flgOk = '0'
033100010615     C                   eval      wrkesito = '2'
033200010615     C                   else
033300010615     C                   eval      wrkesito = '6'
033400010615     C                   endif
033500010330     C                   endif
033600010330     C                   endif
033700010601     C                   else
033800010601     C                   eval      wrkesito = '9'
033900010601     C                   endif
034000010330     C*
034100010330     C                   if        %open(tivin00r)
034200010330     C                   close     tivin00r
034300010330     C                   endif
034400021025     C                   if        %open(fivabwwr)
034500021025     C                   close     fivabwwr
034600010330     C                   endif
034700041108     C                   if        %open(fivatwwr)
034800041108     C                   close     fivatwwr
034900041108     C                   endif
035000010601     C*
035100010601     C                   if        vlrpoi <> 999
035200010601     C                   eval      invfil = vlrpoi
035300010601     C                   endif
035400010330     C*
035500041108     C                   if        §ctrokvb > 0
035600010601     C                             and invfil > *zeros
035700010330     C                   exsr      invio
035800010330     C                   endif
035900010601     C*
036000010618     C                   if        flgGiro = '1'
036100010601     C                   exsr      endela
036200010618     C                   endif
036300010330     C*
036400010330     C                   ENDSR
036500010330     C***
036600041109
036700041109
036800041109
036900050504     C*-----------------------------------------------------------*
037000050504     C* INTEST - PREPARO LA SCHIERA CON LE INTESTAZIONI DEI CAMPI *
037100050504     C*-----------------------------------------------------------*
037200050504     C     INTEST        BEGSR
037300050504
037400050504     C                   CLEAR                   SkSplitFLD
037500050504     C                   EVAL      i = 1
037600050504     C                   EVAL      posDa = *zeros
037700050504     C                   EVAL      posA  = *zeros
037800050504     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
037900050504     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
038000050504     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
038100050504
038200050504     C**********
038300050504     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
038400050504     C**********
038500050504     C                   IF        intcol(1)+intcol(2)+intcol(3) <> *blanks
038600050504     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
038700050504     C                                      %trim(intcol(3))
038800050504     C                   ENDIF
038900050504     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
039000050504     C     minu:maiu     XLATE     vindta        vindta
039100050504     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
039200050504     C                   DOW       posDa <= %len(%trimr(vindta))
039300050504     C*
039400050504     C* Gestisco il 1° campo
039500050504     C                   IF        i = 1
039600050504     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
039700050504     C                   EVAL      posDa = 1
039800050504     C* Eseguo lo scan x trovare la fine del primo campo
039900050504     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040000050504     C* A questo punto "estraggo" il campo corrente
040100050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
040200050504     C                                                    (posA-posDa))
040300050504     C* X i campi successivi al 1°
040400050504     C                   ELSE
040500050504     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
040600050504     C                   EVAL      posDa = posA + 1
040700050504     C* Eseguo lo scan x trovare la fine del campo corrente
040800050504     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040900050504     C* Gestisco l'ultimo campo
041000050504     C                   IF        posA = *zeros
041100050504     C                   EVAL      posA = %len(%trimr(vindta)) + 1
041200050504     C* A questo punto "estraggo" il campo corrente
041300050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041400050504     C                                                    (posA-posDa))
041500050504     C                   LEAVE
041600050504     C                   ELSE
041700050504     C* A questo punto "estraggo" il campo corrente
041800050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041900050504     C                                                    (posA-posDa))
042000050504     C                   ENDIF
042100050504     C                   ENDIF
042200050504     C* Incremento il contatore d campo
042300050504     C                   EVAL      i = i +1
042400050504     C                   ENDDO
042500050504     C*
042600050504     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
042700050504     C* delimitazione testo
042800050504     C                   EVAL      i = 1
042900050504     C                   DOW       i < %elem(SkSplitFLD)
043000050504     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
043100050504     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
043200050504     C                   EVAL      i = i + 1
043300050504     C                   ENDDO
043400050504
043500050504     C                   ENDSR
043600050504
043700041109
043800041109
043900050504     C*----------------------------------------------------*
044000050504     C*  SCARICAMENTO BUFFER RECORDS VAB
044100050504     C*----------------------------------------------------*
044200050504     C     WRIVAB        BEGSR
044300050504     C*
044400050504     C                   write     fivab000                                     => scarico il VAB
044500050504     C*
044600050504     C                   ENDSR
044700050504
044800050504
044900050504
045000041108     C*----------------------------------------------------*
045100041108     C*  SCARICAMENTO BUFFER RECORDS VAT
045200041108     C*----------------------------------------------------*
045300041108     C     WRIVAT        BEGSR
045400041108     C*
045500041108     C* Attribuisco lo stesso numero numero spedizione del file testate
045600050712     C                   EVAL      VATNSP = VABNSP
045700041108     C                   EVAL      VATTRC = 'E'
045800041108     C                   WRITE     FIVAT000
045900041109     C                   ADD       1             §CTROKVT
046000041108     C*
046100041108     C                   ENDSR
046200010601
046300010601
046400010601
046500041108     C*----------------------------------------------------*
046600041108     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046700041108     C*----------------------------------------------------*
046800041108     C     PREVAT        BEGSR
046900041108     C*
047000041108     C* Compongo il nome del membro da dare al FIVATWWR
047100041108     C                   eval      parmbr = vlrhdl
047200041108     C                   movel     'M'           parmbr
047300041108     C                   eval      parccm = vlrksc
047400041108     C                   eval      paropz = '1'
047500041108     C* Effettuo la chiamata al CLLE preposto
047600041108     C                   call(e)   'TITVVTC'
047700041108     C                   parm                    parccm
047800041108     C                   parm                    parmbr
047900041108     C                   parm                    paropz
048000041108     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048100041108     C                   if        %error
048200041108     C                   movel     '1'           chkcall
048300041108     C                   else
048400041108     C                   movel     '0'           chkcall
048500041108     C                   endif
048600041108     C*
048700041108     C                   ENDSR
048800041108
048900041108
049000041108
049100010330     C*----------------------------------------------------*
049200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049300010330     C*----------------------------------------------------*
049400010330     C     INZVAR        BEGSR
049500010330     C*
049600030822     C* Inizializzo variabili di wrk
049700020204     C                   Z-ADD     *zeros        Num5_0            5 0
049800030822     C*
049900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050000030822     C                   CLEAR                   SkSplitCSV
050100041108     c                   CLEAR                   FIVAT000
050200050503     C                   CLEAR                   FIVAB000
050300030822     C                   EVAL      i = 1
050400030822     C                   EVAL      posDa = *zeros
050500030822     C                   EVAL      posA  = *zeros
050600030822     C*
050700030822     C* Reimposto i valori di default
050800030822     C                   EXSR      DEFCAM
050900030822     C*
051000010330     C                   ENDSR
051100010330     C*----------------------------------------------------*
051200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051300010330     C*----------------------------------------------------*
051400010330     C     DEFCAM        BEGSR
051500030822     C*
051600050504     C* Imposto i valori d default a programma dei campi fondamentali
051700050712     C                   EVAL      VABCCM = 1000349
051800050712     C                   EVAL      VATCCM = 1000349
051900050712     C                   EVAL      VABLNP = 100
052000050712     C                   EVAL      VATLNP = 100
052100050712     C                   EVAL      VABCTR = 300
052200050504     C                   EVAL      VABCTM = '7Q'
052300050504     C*
052400050504     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052500050504     C* e delimitatore testo.
052600050504     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052700050504     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052800050504     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052900030822     C*
053000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
053200030715     C                   SETON                                        50
053300030715     C                   ELSE
053400030715     C                   SETOFF                                       50
053500030715     C                   ENDIF
053600030822     C*
053700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053800030822     C                   EVAL      posDaDft = 1
053900030822     C                   EVAL      posADft  = 0
054000030822     C                   EVAL      wGiroDft = 0
054100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054200030822     C                             posDaDft > 0
054300030822     C*
054400030822     C* Gestisco il 1° giro
054500030822     C                   IF        wGiroDft = 0
054600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054800030822     C* Incremento il contatore dei "giri"
054900030822     C                   EVAL      wGiroDft = 1
055000030822     C                   ELSE
055100030822     C                   EVAL      posDaDft = posADft
055200030822     C                   ENDIF
055300030822     C* Eseguo lo scan x trovare la fine del campo corrente
055400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055500030822     C*
055600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055800030822     C                   IF        posDaDft > 0 AND
055900030822     C                             posADft  > 0
056000030822     C* NCL
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'NCL'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000041025     C                   Z-ADD     PiVal         VABNCL
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* CCM
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'CCM'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABCCM
058400041108     C                   Z-ADD     PiVal         VATCCM
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* LNP
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'LNP'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABLNP
059800041108     C                   Z-ADD     PiVal         VATLNP
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* NRS
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'NRS'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiInt=*on
061100030822     C                   Z-ADD     PiVal         VABNRS
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* CTR
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'CTR'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiInt=*on
062400030822     C                   Z-ADD     PiVal         VABCTR
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* PKB
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'PKB'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiNum=*on
063700030822     C                   Z-ADD     PiVal         VABPKB
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* VLB
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'VLB'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiNum=*on
065000030822     C                   Z-ADD     PiVal         VABVLB
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* QFT
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'QFT'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiNum=*on
066300030822     C                   Z-ADD     PiVal         VABQFT
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* CBO
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'CBO'
067100030822     C                   EVAL      VABCBO=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* TSP
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'TSP'
068000030822     C                   EVAL      VABTSP=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* VAS
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'VAS'
068900030822     C                   EVAL      VABVAS=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* VCA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'VCA'
069800030822     C                   EVAL      VABVCA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* GCA
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'GCA'
070700030822     C                   EVAL      VABGCA=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* CTM
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'CTM'
071600030822     C                   EVAL      VABCTM=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* FFD
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'FFD'
072500030822     C                   EVAL      VABFFD=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* VAD
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'VAD'
073400030822     C                   EVAL      VABVAD=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* GMA
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'GMA'
074300030822     C                   EVAL      VABGMA=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* GGA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'GGA'
075200030822     C                   EVAL      VABGGA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* GVA
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'GVA'
076100030822     C                   EVAL      VABGVA=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* TC1
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'TC1'
077000030822     C                   EVAL      VABTC1=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* TC2
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'TC2'
077900030822     C                   EVAL      VABTC2=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* ...
078400030822     C                   ENDIF
078500030822     C                   ENDDO
078600020204     C*
078700010330     C                   ENDSR
078800010607     C*----------------------------------------------------*
078900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079000010607     C*----------------------------------------------------*
079100010607     C     IMPVAB        BEGSR
079200010607     C*
079300010607     C                   SETOFF                                       3132
079400030515     C*
079500010607     C                   MOVEL     datcor        VABAAS
079600041108     C                   MOVEL     datcor        VATAAS
079700010607     C                   MOVE      datcor        VABMGS
079800010607     C*
079900031204     C**********
080000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
080100031204     C* d separatore campo
080200031204     C**********
080300040726     C                   EVAL      vindta = %trim(vindta)
080400040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
080500040726     C                   EVAL      vindta = %subst(vindta:2)
080600040726     C                   ENDDO
080700031204     C*
080800031204     C                   Z-ADD     *zeros        lunghInput        4 0
080900040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
081000040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
081100040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
081200040726     C                   EVAL      lunghInput = %len(%trim(vindta))
081300040726     C                   ENDDO
081400030715     C**********
081500030715     C* Effettuo lo split del campo dati d input
081600030715     C**********
081700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081800040324     C                   DOW       posDa <= %len(%trimr(vindta))
081900030715     C*
082000030715     C* Gestisco il 1° campo
082100030715     C                   IF        i = 1
082200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082300030715     C                   EVAL      posDa = 1
082400030715     C* Eseguo lo scan x trovare la fine del primo campo
082500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082600030715     C* A questo punto "estraggo" il campo corrente
082700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082800030715     C                                                    (posA-posDa))
082900030715     C* X i campi successivi al 1°
083000030715     C                   ELSE
083100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083200030715     C                   EVAL      posDa = posA + 1
083300031204     C* Verifico che nn vi sia il campo nn valorizzato
083400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
083500031204     C* Se campo nn valorizzato skippo al prossimo
083600031204     C                   EVAL      SkSplitCSV(i) = *blanks
083700031204     C                   EVAL      posA  = posA + 1
083800031204     C                   ELSE
083900030715     C* Eseguo lo scan x trovare la fine del campo corrente
084000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084100030715     C* Gestisco l'ultimo campo
084200030715     C                   IF        posA = *zeros
084300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   LEAVE
084800030715     C                   ELSE
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C                   ENDIF
085300031204     C                   ENDIF
085400030715     C                   ENDIF
085500030715     C* Incremento il contatore d campo
085600030715     C                   EVAL      i = i +1
085700030715     C                   ENDDO
085800030715     C*
085900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086000030715     C* delimitazione testo
086100030715     C                   EVAL      i = 1
086200030715     C                   DOW       i < %elem(SkSplitCSV)
086300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
086400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
086500030715     C                   EVAL      i = i + 1
086600030715     C                   ENDDO
086700030715     C*
086800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086900030715     C                   EVAL      i = 1
087000030715     C                   DOW       i < %elem(SkSplitFLD)
087100030715     C*
087200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
087300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
087400030715     C                             SkSplitCSV(i) <> *blanks
087500030721     C*** ATB
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087700030721     C                   EVAL      VABATB = SkSplitCSV(i)
087800030721     C                   ENDIF
087900050712     C*** CBO
088000050712     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
088100050712     c                   Select
088200050712     c                   when      %trim(SkSplitCSV(i)) = '01' or
088300050712     c                             %trim(SkSplitCSV(i)) = '05'
088400050712     c                   eval      VABCBO = '1'
088500050712     c                   when      %trim(SkSplitCSV(i)) = '02'
088600050712     c                   eval      VABCBO = '2'
088700050712     c                   endsl
088800050712     C                   ENDIF
088900030715     C*** RSD
089000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
089100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
089200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
089300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
089400030715     C* ==
089500030715     C                   ENDIF
089600030721     C*** RD2
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
089800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030715     C*** IND
090100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
090200030715     C                   EVAL      VABIND = SkSplitCSV(i)
090300030715     C                   ENDIF
090400030715     C*** LOD
090500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
090600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
090700030715     C                   ENDIF
090800030715     C*** PRD
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
091000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
091100030715     C                   ENDIF
091200030721     C*** NZD
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
091400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030715     C*** RMA
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
091800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** NOT
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
092200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030715     C*** NT2
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
092600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030715     C*** VAS
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
093000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
093100030715     C                   ENDIF
093200030715     C*** FFD
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
093400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030721     C*** GC1
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
093800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** GC2
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
094200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*** TSP
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
094600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** NAS
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
095000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** GCA
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** XCO
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
095800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** CTM
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** TCR
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
096600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** CTS
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
097000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** FTM
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** VAD
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
097800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** GMA
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** GGA
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
098600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** GVA
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
099000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** TC1
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** TC2
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
099800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** SCL
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** RMO
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
100600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** NMO
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
101000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101100030721     C                   ENDIF
101200050712     c*** VATNOT
101300050712     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
101400050712     C                   EVAL      VATNOT = SkSplitCSV(i)
101500050712     C                   ENDIF
101600030721     C*
101700030721     C* Reperisco quindi i campi numerici...
101800030721     C*** CAD
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
102000030721     C                   EVAL      PiStr=SkSplitCSV(i)
102100030721     C                   EXSR      CHKNUM
102200030721     C                   IF        PiInt=*on
102300030721     C                   Z-ADD     PiVal         Num5_0
102400030721     C                   MOVEL(p)  Num5_0        VABCAD
102500030721     C                   ELSE
102600030721     C                   SETON                                        32
102700030721     C                   EVAL      VABCAD = *zeros
102800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102900030721     C                             + ' ' + 'VABCAD'
103000030721     C                   ENDIF
103100030721     C                   ENDIF
103200050712     C*** RMN/NSP
103300041122     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
103400041122     C                   EVAL      PiStr=SkSplitCSV(i)
103500041122     C                   EXSR      CHKNUM
103600041122     C                   IF        PiInt=*on
103700041122     C                   Z-ADD     PiVal         VABRMN
103800050712     C                   Z-ADD     PiVal         VABNSP
103900041122     C                   ELSE
104000050712     C                   SETON                                        31
104100041122     C                   EVAL      VABRMN = *zeros
104200041122     C                   EVAL      vinmsg = %trimr(vinmsg)
104300050712     C                             + ' ' + 'VABRMN VABNSP'
104400041122     C                   ENDIF
104500050712     C                   ENDIF
104600030721     C*** NCL
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
104800030721     C                   EVAL      PiStr=SkSplitCSV(i)
104900030721     C                   EXSR      CHKNUM
105000030721     C                   IF        PiInt=*on
105100041108     C                   Z-ADD     PiVal         VABNCL
105200030721     C                   ELSE
105300030721     C                   SETON                                        32
105400030721     C                   EVAL      VABNCL = *zeros
105500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105600030721     C                             + ' ' + 'VABNCL'
105700030721     C                   ENDIF
105800030721     C                   ENDIF
105900030721     C*** PKB
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106100030721     C                   EVAL      PiStr=SkSplitCSV(i)
106200030721     C                   EXSR      CHKNUM
106300030721     C                   IF        PiNum=*on
106400041108     C                   Z-ADD     PiVal         VABPKB
106500030721     C                   ELSE
106600030721     C                   SETON                                        32
106700030721     C                   EVAL      VABPKB = *zeros
106800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106900030721     C                             + ' ' + 'VABPKB'
107000030721     C                   ENDIF
107100030721     C                   ENDIF
107200030721     C*** CAS
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
107400030721     C                   EVAL      PiStr=SkSplitCSV(i)
107500030721     C                   EXSR      CHKNUM
107600030721     C                   IF        PiNum=*on
107700030721     C                   Z-ADD     PiVal         VABCAS
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      VABCAS = *zeros
108100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108200030721     C                             + ' ' + 'VABCAS'
108300030721     C                   ENDIF
108400030721     C                   ENDIF
108500050712      *** VCA
108600050712     c                   IF        %trim(SkSplitFLD(i)) = 'VABVCA' and
108700050712     c                             VABCAS > *Zeros
108800050712     c                   Select
108900050712     c                   when      %trim(SkSplitCSV(i)) = '0'
109000050712     c                   eval      VABVCA = 'EUR'                               EURO
109100050712     c                   when      %trim(SkSplitCSV(i)) = '38'
109200050712     c                   eval      VABVCA = 'GPB'                               STERLINA INGLESE
109300050712     c                   when      %trim(SkSplitCSV(i)) = '31'
109400050712     c                   eval      VABVCA = 'USD'                               DOLLARO USA
109500050712     c                   endsl
109600050712     C                   ENDIF
109700050712      *** TIC
109800050712     c                   IF        %trim(SkSplitFLD(i)) = 'VABTIC' and
109900050712     c                             VABCAS > *Zeros
110000050712     c                   eval      VABTIC = 'BM'
110100050712     C                   ENDIF
110200030721     C*** CCM
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiInt=*on      AND
110700030721     C                             PiVal<=9999999 AND
110800030721     C                             PiVal>*zeros
110900030721     C                   Z-ADD     PiVal         VABCCM
111000041108     C                   Z-ADD     PiVal         VATCCM
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      VABCCM = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500041108     C                             + ' ' + 'VABCCM VATCCM'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** LNP
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on    AND
112300030721     C                             PiVal<=999   AND
112400030721     C                             PiVal>*zeros
112500030721     C                   Z-ADD     PiVal         VABLNP
112600041108     C                   Z-ADD     PiVal         VATLNP
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABLNP = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100041108     C                             + ' ' + 'VABLNP VATLNP'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** NRS
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on AND
113900030721     C                             PiVal<=99
114000030721     C                   Z-ADD     PiVal         VABNRS
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABNRS'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** LNA
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on    AND
115200030721     C                             PiVal<=999   AND
115300030721     C                             PiVal>*zeros
115400030721     C                   Z-ADD     PiVal         VABLNA
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      VABLNA = *zeros
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABLNA'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** CTR
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiInt=*on  AND
116700030721     C                             PiVal<=999
116800030721     C                   Z-ADD     PiVal         VABCTR
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABCTR'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** IAS
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiNum=*on
118000030721     C                   Z-ADD     PiVal         VABIAS
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABIAS'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** VLB
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABVLB
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABVLB'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** QFT
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiNum=*on
120400030721     C                   Z-ADD     PiVal         VABQFT
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABQFT'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** NCD
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on      AND
121600030721     C                             PiVal<=9999999
121700030721     C                   Z-ADD     PiVal         VABNCD
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABNCD'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** NCA
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on      AND
122900030721     C                             PiVal<=9999999
123000030721     C                   Z-ADD     PiVal         VABNCA
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABNCA'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** ZNC
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on AND
124200030721     C                             PiVal<=99
124300030721     C                   Z-ADD     PiVal         VABZNC
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABZNC'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** DCR
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
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
131900050504     C*
132000050504     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
132100050504     C* Reperisco la provincia dal CAP e dalla località
132200050504     C                   IF        VABCAD <> *blanks
132300050504     C                   CLEAR                   TISI95DS
132400050504     C                   EVAL      I95TCN = '3'
132500050504     C                   Z-ADD     datcor        I95DAT
132600050504     C                   EVAL      I95CAP = VABCAD
132700050504     C                   EVAL      I95LOC = VABLOD
132800050504     C                   CALL      'TISI95R'
132900050504     C                   PARM                    TISI95DS
133000050504     C                   EVAL      VABPRD = O95PRV
133100050504     C                   ENDIF
133200040726     C*
133300040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
133400040726     C                   IF        VABLNP = *zeros
133500040726     C                   SETON                                        31
133600050504     C                   EVAL      vinmsg = %trimr(vinmsg)
133700050504     C                             + ' ' + 'VABLNP'
133800040726     C                   ENDIF
133900050504     C*
134000050504     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
134100050504     C  n31              EXSR      wrivat
134200040726     C*
134300040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
134400040726     C* d valorizzarli sempre entrambi
134500040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
134600040726     C                   Z-ADD     VABNSP        VABRMN
134700040726     C                   ENDIF
134800040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
134900040726     C                   MOVEL     VABRMN        VABRMA
135000040726     C                   ENDIF
135100040726     C*
135200040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
135300040726     C                   IF        VABCAS > *zeros
135400040726     C                   IF        VABVCA = *blanks
135500040726     C                   EVAL      VABVCA = 'EUR'
135600040726     C                   ENDIF
135700040726     C                   IF        VABCBO = '1'
135800040726     C                   EVAL      VABCBO = '4'
135900040726     C                   ENDIF
136000040726     C                   IF        VABCBO = '2'
136100040726     C                   EVAL      VABCBO = '6'
136200040726     C                   ENDIF
136300040726     C                   ENDIF
136400020204     C*
136500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
136600020204     C                   EXSR      CHKIMPDIV
136700020204     C*
136800010607     C                   ENDSR
136900010607     C*----------------------------------------------------*
137000010601
137100041109
137200041109
137300020204
137400020204     C*----------------------------------------------------*
137500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
137600020204     C*----------------------------------------------------*
137700020204     C     CHKIMPDIV     BEGSR
137800020204     C*
137900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
138000020204     C                   Z-ADD     *zeros        wrkDec            9 9
138100020204     C*
138200020204     C* Come prima cosa effettuo considerazioni sulla divisa
138300020204     C                   IF        vabIAS > *zeros
138400020204     C                   IF        vabVAS <> 'EUR'
138500020204     C                   EVAL      vabVAS =  'ITL'
138600020204     C                   ENDIF
138700020204     C                   ENDIF
138800020204     C*
138900020204     C                   IF        vabCAS > *zeros
139000020204     C                   IF        vabVCA <> 'EUR'
139100020204     C                   EVAL      vabVCA =  'ITL'
139200020204     C                   ENDIF
139300020204     C                   ENDIF
139400020204     C*
139500020204     C                   IF        vabVMD > *zeros
139600020204     C                   IF        vabVAD <> 'EUR'
139700020204     C                   EVAL      vabVAD =  'ITL'
139800020204     C                   ENDIF
139900020204     C                   ENDIF
140000020204     C*
140100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
140200020204     C                   Z-ADD     vabIAS        wrkDec
140300020204     C                   IF        wrkDec > *zeros
140400020204     C                   IF        vabVAS = 'ITL'
140500020204     C                   EVAL      vabIAS = *zeros
140600020204     C                   ENDIF
140700020204     C                   ENDIF
140800020204     C*
140900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
141000020204     C                   Z-ADD     vabCAS        wrkDec
141100020204     C                   IF        wrkDec > *zeros
141200020204     C                   IF        vabVCA = 'ITL'
141300020204     C                   EVAL      vabCAS = *zeros
141400020204     C                   ENDIF
141500020204     C                   ENDIF
141600020204     C*
141700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
141800020204     C                   Z-ADD     vabVMD        wrkDec
141900020204     C                   IF        wrkDec > *zeros
142000020204     C                   IF        vabVAD = 'ITL'
142100020204     C                   EVAL      vabVMD = *zeros
142200020204     C                   ENDIF
142300020204     C                   ENDIF
142400020204     C*
142500020204     C                   ENDSR
142600020204     C***
142700020204
142800010330
142900010330
143000010330     C*----------------------------------------------------*
143100010330     C*  CONTROLLO NUMERICITA' CAMPI
143200010330     C*----------------------------------------------------*
143300010330     C     CHKNUM        BEGSR
143400010330     C*
143500010606     C                   IF        PiDecChr = *blanks
143600030715     C                   EVAL      PiDecChr = CharNUM
143700010606     C                   ENDIF
143800010606     C*
143900010606     C                   CALL(e)   'ISNUMERIC'
144000010330     C                   PARM                    PiStr            30
144100010606     C                   PARM                    PiDecChr          1
144200010330     C                   PARM      *ZEROS        PiVal            30 9
144300010330     C                   PARM      '0'           PiInt             1
144400010330     C                   PARM      '0'           PiNum             1
144500010330     C                   IF        %error
144600010606     C                   EVAL      PiNum=*off
144700010330     C                   ENDIF
144800010330     C*
144900010330     C                   ENDSR
145000010330     C***
145100010330
145200010601
145300010601
145400010601
145500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
145600010601     C     repfil        BEGSR
145700010601     C*
145800010601     C                   if        invfil = *zeros and
145900010601     C                             depfil > *zeros and
146000010629     C                             (vinflg = *blanks or
146100010629     C                              vinflg = *zeros)
146200010601     C
146300010601     C                   eval      invfil = depfil
146400010601     C                   endif
146500010601     C*
146600010601     C                   if        depfil <> invfil and
146700010601     C                             invfil > *zeros
146800010601     C                   eval      flgMulti = '1'
146900010601     C                   if        vinflg = *blanks
147000010601     C                   add       1             cntNonEl
147100010601     C                   endif
147200010601     C                   endif
147300010601     C*
147400010601     C                   if        vinflg = '2'
147500010601     C                   eval      flgStato = '2'
147600010601     C                   endif
147700010601     C*
147800010601     C                   ENDSR
147900010601     C***
148000010601
148100010601
148200010601
148300010330
148400010330
148500010330
148600990920      /TITLE Invio dei dati al punto operativo.
148700000613     C     invio         BEGSR
148800041108
148900041108     C* 1° invio FIVAT
149000041108     C                   reset                   dscmz
149100041108     C                   move      invfil        cmzdst
149200041108     C                   eval      cmzfld = 'FIVATWWR'
149300041108     C                   eval      cmzmbd = vlrhdl
149400041108     C                   eval      %subst(cmzmbd:1:1) = 'M'
149500041108     C                   eval      cmzfla = 'FIVAT00F'
149600041108     C                   eval      cmzmba = 'FIVAT00F'
149700041108     C                   eval      cmznrr = *zeros
149800041108     C                   move      §ctrokvt      cmznrr
149900041108     C                   eval      cmzlba = vlrfl1
150000041108     C                   call(e)   'TIS711C'
150100041108     C                   parm                    dscmz
150200041108     C                   parm      *blanks       esito
150300041108     C                   if        %error
150400041108     C                             or cmzerr = '1'
150500041108     C                             or esito  = '1'
150600041108     C                   eval      wrkesito = '3'
150700041108     C                   else
150800990920     C*
150900041108     C* 2° invio FIVAB
151000990920     C                   reset                   dscmz
151100010601     C                   move      invfil        cmzdst
151200990920     C                   eval      cmzfld = vlrfou
151300990920     C                   eval      cmzmbd = vlrhdl
151400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
151500021025     C                   eval      cmzfla = 'FIVAB00F'
151600021025     C                   eval      cmzmba = 'FIVAB00F'
151700990920     C                   eval      cmznrr = *zeros
151800041108     C                   move      §ctrokvb      cmznrr
151900021018     C                   eval      cmzlba = vlrfl1
152000990920     C                   call(e)   'TIS711C'
152100990920     C                   parm                    dscmz
152200990921     C                   parm      *blanks       esito
152300990920     C                   if        %error
152400990920     C                             or cmzerr = '1'
152500990921     C                             or esito  = '1'
152600000710     C                   eval      wrkesito = '3'
152700990920     C                   endif
152800041108     C                   endif
152900990920     C*
153000000613     C                   ENDSR
153100990910
153200010601
153300010601
153400010601
153500010601
153600010601      /TITLE Invio dei dati al punto operativo.
153700010601     C     opeini        BEGSR
153800010601     C*
153900010601     C* Inizializzo flag e contatori operativi
154000010601     C                   movel     '0'           flgGiro           1
154100010601     C                   movel     '0'           flgMulti          1
154200010601     C                   movel     '1'           flgStato          1
154300010615     C                   movel     '0'           flgOk             1
154400010601     C                   z-add     *zeros        cntNonEl         10 0
154500010601     C                   z-add     *zeros        depfil            3 0
154600010601     C                   z-add     *zeros        invfil            3 0
154700010601     C*
154800010601     C                   ENDSR
154900010601     C***
155000010601
155100010601
155200050421
155300050421
155400070411
155500070411     C     *pssr         BEGSR
155600070411     C*
155700070411     C                   if        %open(tivin00r)
155800070411     C                   close     tivin00r
155900070411     C                   endif
156000070411     C                   if        %open(fivabwwr)
156100070411     C                   close     fivabwwr
156200070411     C                   endif
156300070411     C                   if        %open(fivatwwr)
156400070411     C                   close     fivatwwr
156500070411     C                   endif
156600070411     C*
156700070411     C* Effettuo la chiamata al CLLE preposto
156800070411     C                   call(e)   'TITVVTC'
156900070411     C                   parm                    parccm
157000070411     C                   parm                    parmbr
157100070411     C                   parm      '2'           paropz
157200070411     C*
157300070411     C                   eval      wrkesito = '2'
157400070411     C*
157500070411     C                   seton                                        LR
157600070411     C*
157700070411     C                   ENDSR     '*CANCL'
157800070411     C***
157900070411
158000050421
158100010601
158200010330
158300010330
158400000613     C     *inzsr        BEGSR
158500990910     C*
158600990910     C     *entry        plist
158700990920     C                   parm                    tivlrds
158800990921     C                   parm      wrkesito      esito
158900000724     C                   parm                    prmlit
159000000710     C                   parm                    prmfir
159100010330     C*
159200010330     C* CALCOLA LA DATA CORRENTE
159300010330     C                   time                    wn14             14 0
159400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
159500010330     C                   z-add     wn8           g08dat
159600010330     C                   z-add     *zeros        g08inv
159700010330     C                   movel     '0'           g08err
159800010330     C                   call      'XSRDA8'
159900010330     C                   parm                    wlbda8
160000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
160100000613     C*
160200000613     C                   ENDSR
160300000613     C***
160400990908
160500050712     Otitvn3p   E            riepilogo         2
160600990915     O                                              'Upload via Internet'
160700990915     O                                           +1 'Traduzione TIVIN00R -'
160800021025     O                                           +1 'FIVABWWR'
160900990915     O                                           +1 'Testate bolle'
161000990915     O          E            riepilogo   2
161100990915     O                       wrkdata
161200990915     O                       wrkora              +1
161300990915     O                       procname            +1
161400990915     O          E            riepilogo   2
161500990915     O                                              'Cliente..................:'
161600010605     O                       VABCCM        z     +1
161700990915     O          E            riepilogo   2
161800990920     O                                              'Riferimento Strategi.....:'
161900990920     O                       vlrhdl              +1
162000990915     O          E            riepilogo   2
162100990915     O                                              'Giusti...................:'
162200041108     O                       §CTROKVB      2   +  1
162300990915     O          E            riepilogo   2
162400990915     O                                              'Sbagliati e corretti.....:'
162500971022     O                       §CTRMO        2   +  1
162600990915     O          E            riepilogo   2
162700990915     O                                              'Sbagliati e scartati.....:'
162800971022     O                       §CTRNO        2   +  1
162900000613
163000050712     Otitvn3ps  E            testdett          2
163100000613     O                                              'Upload via Internet'
163200000613     O                                           +1 'Traduzione TIVIN00R -'
163300021025     O                                           +1 'FIVABWWR'
163400000613     O                                           +1 'Testate bolle'
163500000616     O          E            testdett    3
163600000613     O                                           +2 'N° rec'
163700000613     O                                           +3 'Anteprima contenuto'
163800000616     O          E            rigadett    2
163900000613     O                       rrnum               +2
164000000621     O                       recko               +3
164100000616     O          E            findett     2
164200000613     O                       wrkdata
164300000613     O                       wrkora              +1
164400000613     O                       procname            +1
164500000616     O          E            findett     2
164600000613     O                                              'Cliente..................:'
164700010605     O                       VABCCM        z     +1
164800000616     O          E            findett     2
164900000613     O                                              'Riferimento Strategi.....:'
165000000613     O                       vlrhdl              +1
165100000616     O          E            findett     2
165200000613     O                                              'Giusti...................:'
165300041108     O                       §CTROKVB      2   +  1
165400000616     O          E            findett     2
165500000613     O                                              'Sbagliati e corretti.....:'
165600000613     O                       §CTRMO        2   +  1
165700000616     O          E            findett     2
165800000613     O                                              'Sbagliati e scartati.....:'
165900000613     O                       §CTRNO        2   +  1
166000000613** CMD - COMANDI CL
166100050712OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVN3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
166200050712OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVN3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
166300050712DLTOVR FILE(TITVN3P TITVN3PS) LVL(*)
166400010606
166500000613
166600040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
166700050712X;VABCBO;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;VABCAS;VABVCA;VABTIC;
166800050712VABRMN;VATNOT;VABRMA
166900040726
