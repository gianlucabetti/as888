000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800050927     Ftitvo6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050927     Ftitvo6ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040706     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400040706     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004500050927     D wVATNOT_A       s                   LIKE(vatnot) INZ(*blanks)
004600050927     D wVATNOT_B       s                   LIKE(vatnot) INZ(*blanks)
004700050927     D wVATNOT_E       s                   LIKE(vatnot) INZ(*blanks)
004800050927     D wVABNSPalfa     s              7    INZ(*blanks)
004900030715     D*------------------
005000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005100030715     D*------------------
005200030715     D SkSplitFLD      S             10    DIM(100)
005300030715     D SkSplitCSV      S            256    DIM(100)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700050927     D CharINT         S              1
005800030715     D posDa           S              3  0 INZ(*zeros)
005900030715     D posA            S              3  0 INZ(*zeros)
006000030715     D i               s              3  0 INZ(1)
006100030715     D wGiro           s              1  0 INZ(*zeros)
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
008300010330
008400010330
008500990915     C                   time                    wrkdata
008600990915     C                   time                    wrkora
008700000913     C                   reset                   rrnum
008800990921     C                   reset                   esito
008900990921     C                   reset                   wrkesito
009000010601     C*
009100010601     C                   exsr      opeini
009200010605     C                   exsr      rwvab
009300010601     C*
009400010601     C                   seton                                        lr
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010000010601     C*--------------------------------------------------------
010100010601     C     PREELA        BEGSR
010200010601     C*
010300010601     C* SE OCCORRE SPEDIRE IN FILIALE
010400010601     C                   if        invfil <> *zeros and
010500010601     C                             flgGiro = '0'
010600010601     C*
010700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010800010601     C                   eval      flgGiro = '1'
010900000724     C*
011000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011200010601     C     invfil        chain     azorg01l
011300000724     C                   if        %found
011400000724     C                   movel(p)  CMD(1)        depcmd
011500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011600000724     C*
011700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012000000724     C                   PARM                    depcmd
012100000724     C                   PARM                    LENGH
012200000724     C*
012300000724     C                   endif
012400000616     C*
012500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012600000616     C                   movel(p)  CMD(2)        depcmd
012700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012800000616     C*
012900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013000011113     C                   Z-ADD     150           LENGH            15 5
013100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200000616     C                   PARM                    depcmd
013300000616     C                   PARM                    LENGH
013400000613     C*
013500050927     C                   if        not %open(titvo6ps)
013600050927     C                   open      titvo6ps
013700030715     C                   except    testdett
013800000613     C                   endif
013900010601     C*
014000010601     C                   endif
014100010601     C*
014200010601     C                   ENDSR
014300010601     C***
014400010601
014500010601
014600010601
014700010601
014800010601     C*--------------------------------------------------------
014900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015000010601     C*--------------------------------------------------------
015100010601     C     ENDELA        BEGSR
015200990915     C*
015300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015400040119     C*
015500040119     C* Effettuo la chiamata al CLLE preposto
015600040119     C                   call(e)   'TITVVTC'
015700040119     C                   parm                    parccm
015800040119     C                   parm                    parmbr
015900040119     C                   parm      '2'           paropz
016000050201     C*
016100050201     C* Effettuo lancio TISI95 solo x chiusura
016200050201     C                   CLEAR                   TISI95DS
016300050201     C                   EVAL      I95TLA = 'C'
016400050201     C                   CALL      'TISI95R'
016500050201     C                   PARM                    TISI95DS
016600000613     C*
016700050927     C                   if        %open(titvo6ps)
016800000616     C                   except    findett
016900050927     C                   close     titvo6ps
017000000613     C                   endif
017100000616     C*
017200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017300011113     C                   Z-ADD     150           LENGH            15 5
017400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017500000616     C                   PARM                    CMD(3)
017600000616     C                   PARM                    LENGH
017700000616     C*
017800010601     C                   ENDSR
017900010601     C***
018000010601
018100010601
018200010601
018300000613
018400000613     C*--------------------------------------------------------
018500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018600000613     C*--------------------------------------------------------
018700000621     C     STPR          BEGSR
018800000613     C*
018900050927     C                   if        not %open(titvo6p)
019000050927     C   50              open      titvo6p
019100990915     C                   endif
019200990915     C*
019300030715     C   50              except    riepilogo
019400990915     C*
019500050927     C                   if        %open(titvo6p)
019600050927     C   50              close     titvo6p
019700990914     C                   endif
019800000613     C*
019900000613     C                   ENDSR
020000000613     C***
020100000613
020200010601
020300010601
020400010330     C*--------------------------------------------------------
020500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020600010330     C*--------------------------------------------------------
020700010605     C     RWVAB         BEGSR
020800010330     C*
020900010330     C                   if        not %open(tivin00r)
021000010330     C                   open      tivin00r
021100010330     C                   endif
021200021025     C                   if        not %open(fivabwwr)
021300021025     C                   open      fivabwwr
021400010330     C                   endif
021500040119     C*
021600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021700040119     C                   exsr      prevat
021800040119     C*
021900040119     C                   if        chkcall = '0'
022000040119     C*
022100040119     C                   if        not %open(fivatwwr)
022200040119     C                   open      fivatwwr
022300040119     C                   endif
022400010330     C*
022500040119     C                   clear                   §CTROKVB          7 0
022600040119     C                   clear                   §CTROKVT          7 0
022700040119     C                   clear                   §CTRMO            7 0
022800040119     C                   clear                   §CTRNO            7 0
022900050927     C*
023000050330     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
023100050330     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
023200050927     C                   if        CharINT = 'N'
023300050330     C                   exsr      inzvar
023400050330     C                   exsr      impvab
023500050927     C                   endif
023600050330     C*
023700050330     C                   do        *HIVAL
023800010330     C*
023900040706     C                   read      tivin00r                               70
024000010618     C*
024100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024200010618     C                   if        vinflg = '1'
024300010618     C                   eval      flgOk = '1'
024400010618     C                   endif
024500050330     C*
024600050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024700050330     C                   z-add     *zeros        wLenVINDTA        4 0
024800050330     C                   z-add     *zeros        wFlgVINDTA        1 0
024900050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
025000050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
025100050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
025200050330     C                   z-add     1             wFlgVINDTA
025300050330     C                   leave
025400050330     C                   endif
025500050330     C                   enddo
025600010618     C*
025700050330     C                   if        vindta > *blanks AND
025800050330     C                             wFlgVINDTA = 1
025900010330     C                   add       1             rrnum
026000010330     C*
026100010601     C                   if        *in70 = *off and
026200010330     C                             (vinflg = *blanks
026300010330     C                              or vinflg = '0'
026400010330     C                              or vinflg = '2')
026500010330     C*
026600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026700010711     C                   if        vinflg = *blanks or vinflg = '0'
026800010711     C                   clear                   vinmsg
026900010711     C                   endif
027000010601     C*
027100010330     C                   exsr      inzvar
027200010605     C                   exsr      impvab
027300010601     C*
027400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027500010605     C                   eval      depfil = VABLNP
027600010601     C                   exsr      repfil
027700010601     C                   if        depfil = invfil
027800021025     C                   if        vlrpoi = 999
027900030822     C                   move(P)   invfil        VABFGS
028000040119     C                   move(P)   invfil        VATFGS
028100021025     C                   else
028200030822     C                   move(P)   vlrpoi        VABFGS
028300040119     C                   move(P)   vlrpoi        VATFGS
028400021025     C                   endif
028500010601     C*
028600010601     C                   exsr      PREELA
028700010601     C*
028800010604     C* Ebbene...
028900010604     C*
029000040119     C  N31              add       1             §CTROKVB
029100040119     C   32              add       1             §CTRMO
029200040119     C   31              add       1             §CTRNO
029300030822     C                   if        wGiro = 2
029400040119     C  N31              exsr      wrivab
029500040119     C                   endif
029600010604     C*
029700010604     C                   if        *in31 = *off and
029800010604     C                             *in32 = *off
029900010604     C                   eval      vinflg = '1'
030000010604     C                   else
030100010604     C                   eval      recko = vindta
030200010604     C                   except    rigadett
030300010604     C                   eval      vinflg = '2'
030400010604     C                   endif
030500010604     C                   endif
030600010601     C*
030700010604     C                   endif
030800010604     C*
030900010330     C                   else
031000010330     C                   eval      vinflg = '1'
031100010330     C                   endif
031200010601     C*
031300010601     C  N70              update    tivin000
031400010330     C*
031500030822     C  N70              enddo
031600040119     C*
031700040119     C                   endif                                                  (endif - chkcall)
031800010601     C*
031900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032000010601     C                   if        cntNonEl = *zeros or
032100010601     C                             flgMulti = '0'
032200010330     C* Se non ci sono record con errori ...
032300010601     C                   if        §ctrno = 0 and
032400010604     C                             §ctrmo = 0 and
032500010601     C                             flgStato <> '2'
032600010330     C* ... restituisco esito OK.
032700010330     C                   eval      wrkesito = '0'
032800010330     C                   else
032900040119     C                   if        §ctrokvb > 0
033000010330     C                   eval      wrkesito = '1'
033100010330     C                   else
033200010615     C                   if        flgOk = '0'
033300010615     C                   eval      wrkesito = '2'
033400010615     C                   else
033500010615     C                   eval      wrkesito = '6'
033600010615     C                   endif
033700010330     C                   endif
033800010330     C                   endif
033900010601     C                   else
034000010601     C                   eval      wrkesito = '9'
034100010601     C                   endif
034200010330     C*
034300010330     C                   if        %open(tivin00r)
034400010330     C                   close     tivin00r
034500010330     C                   endif
034600021025     C                   if        %open(fivabwwr)
034700021025     C                   close     fivabwwr
034800010330     C                   endif
034900040512     C                   if        %open(fivatwwr)
035000040512     C                   close     fivatwwr
035100040512     C                   endif
035200010601     C*
035300010601     C                   if        vlrpoi <> 999
035400010601     C                   eval      invfil = vlrpoi
035500010601     C                   endif
035600010330     C*
035700040119     C                   if        §ctrokvb > 0
035800010601     C                             and invfil > *zeros
035900010330     C                   exsr      invio
036000010330     C                   endif
036100010601     C*
036200010618     C                   if        flgGiro = '1'
036300010601     C                   exsr      endela
036400010618     C                   endif
036500010330     C*
036600010330     C                   ENDSR
036700010330     C***
036800040119
036900040119
037000040119
037100040119     C*----------------------------------------------------*
037200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037300040119     C*----------------------------------------------------*
037400040119     C     WRIVAB        BEGSR
037500040119     C*
037600040119     C                   write     fivab000                                     => scarico il VAB
037700040119     C*
037800040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
037900050330     C                   exsr      wrivat
038000040119     C*
038100040119     C                   ENDSR
038200040119
038300040119
038400040119
038500040119     C*----------------------------------------------------*
038600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038700040119     C*----------------------------------------------------*
038800040706     C     WRIVAT        BEGSR
038900040706     C*
039000040706     C* Attribuisco lo stesso numero numero spedizione del file testate
039100050927     C                   EVAL      VATNSP = VABNSP
039200040119     C*
039300050927     C                   EVAL      VATNOT = wVATNOT_A
039400050927     C                   EVAL      VATTRC = 'A'
039500050330     C                   write     FIVAT000
039600050927     C*
039700050927     C                   EVAL      VATNOT = wVATNOT_B
039800050927     C                   EVAL      VATTRC = 'B'
039900050927     C                   write     FIVAT000
040000050927     C*
040100050927     C                   EVAL      VATNOT = wVATNOT_E
040200050927     C                   EVAL      VATTRC = 'E'
040300050927     C                   write     FIVAT000
040400040119     C*
040500040119     C                   ENDSR
040600040119
040700040119
040800040119
040900040119     C*----------------------------------------------------*
041000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041100040119     C*----------------------------------------------------*
041200040119     C     PREVAT        BEGSR
041300040119     C*
041400040119     C* Compongo il nome del membro da dare al FIVATWWR
041500040119     C                   eval      parmbr = vlrhdl
041600040119     C                   movel     'M'           parmbr
041700050330     C                   eval      parccm = %subst(vlrKSC:2:7)
041800040119     C                   eval      paropz = '1'
041900040119     C* Effettuo la chiamata al CLLE preposto
042000040119     C                   call(e)   'TITVVTC'
042100040119     C                   parm                    parccm
042200040119     C                   parm                    parmbr
042300040119     C                   parm                    paropz
042400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042500040119     C                   if        %error
042600040119     C                   movel     '1'           chkcall
042700040119     C                   else
042800040119     C                   movel     '0'           chkcall
042900040119     C                   endif
043000040119     C*
043100040119     C                   ENDSR
043200010601
043300010601
043400010601
043500010330     C*----------------------------------------------------*
043600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
043700010330     C*----------------------------------------------------*
043800040119     C     INZVAR        BEGSR
043900010330     C*
044000030822     C* Inizializzo variabili di wrk
044100020204     C                   Z-ADD     *zeros        Num5_0            5 0
044200030822     C*
044300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
044400030822     C                   CLEAR                   FIVAB000
044500040119     C                   CLEAR                   FIVAT000
044600030822     C                   CLEAR                   SkSplitCSV
044700030822     C                   EVAL      i = 1
044800030822     C                   EVAL      posDa = *zeros
044900030822     C                   EVAL      posA  = *zeros
045000030822     C*
045100030822     C* Reimposto i valori di default
045200030822     C                   EXSR      DEFCAM
045300030822     C*
045400010330     C                   ENDSR
045500010330     C*----------------------------------------------------*
045600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
045700010330     C*----------------------------------------------------*
045800010330     C     DEFCAM        BEGSR
045900040119     C*
046000040119     C* Imposto i valori d default a programma dei campi fondamentali
046100050927     C                   EVAL      VABCCM = 0053385
046200050927     C                   EVAL      VATCCM = 0053385
046300050927     C                   EVAL      VABLNP = 005
046400050927     C                   EVAL      VATLNP = 005
046500050927     C                   EVAL      VABCTR = 300
046600040119     C                   EVAL      VABCTM = '7Q'
046700040119     C                   EVAL      VABCBO = '1'
046800030822     C*
046900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047000030715     C* e delimitatore testo.
047100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
047200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
047300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
047400050927     C                   EVAL      CharINT = %subst(vlrppt:5:1)
047500050330     C*
047600050330     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
047700050330     C                   IF        %subst(vlrppt:1:1) = 'S'
047800050330     C                   SETON                                        50
047900050330     C                   ELSE
048000050330     C                   SETOFF                                       50
048100050330     C                   ENDIF
048200030822     C*
048300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
048400030822     C                   EVAL      posDaDft = 1
048500030822     C                   EVAL      posADft  = 0
048600030822     C                   EVAL      wGiroDft = 0
048700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
048800030822     C                             posDaDft > 0
048900030822     C*
049000030822     C* Gestisco il 1° giro
049100030822     C                   IF        wGiroDft = 0
049200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
049300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
049400030822     C* Incremento il contatore dei "giri"
049500030822     C                   EVAL      wGiroDft = 1
049600030822     C                   ELSE
049700030822     C                   EVAL      posDaDft = posADft
049800030822     C                   ENDIF
049900030822     C* Eseguo lo scan x trovare la fine del campo corrente
050000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
050100030822     C*
050200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
050300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
050400030822     C                   IF        posDaDft > 0 AND
050500030822     C                             posADft  > 0
050600030822     C* NCL
050700030822     C                   IF        %subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):1:3)
051000030822     C                             = 'NCL'
051100030822     C                   EVAL      PiStr=%trim(%subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051400030822     C                   EXSR      CHKNUM
051500030822     C                   IF        PiInt=*on
051600030822     C                   Z-ADD     PiVal         VABNCL
051700030822     C                   ENDIF
051800030822     C                   ENDIF
051900030822     C* CCM
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'CCM'
052400030822     C                   EVAL      PiStr=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   EXSR      CHKNUM
052800030822     C                   IF        PiInt=*on
052900030822     C                   Z-ADD     PiVal         VABCCM
053000040119     C                   Z-ADD     PiVal         VATCCM
053100030822     C                   ENDIF
053200030822     C                   ENDIF
053300030822     C* LNP
053400030822     C                   IF        %subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):1:3)
053700030822     C                             = 'LNP'
053800030822     C                   EVAL      PiStr=%trim(%subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100030822     C                   EXSR      CHKNUM
054200030822     C                   IF        PiInt=*on
054300030822     C                   Z-ADD     PiVal         VABLNP
054400040119     C                   Z-ADD     PiVal         VATLNP
054500030822     C                   ENDIF
054600030822     C                   ENDIF
054700030822     C* NRS
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'NRS'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABNRS
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* CTR
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'CTR'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABCTR
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* PKB
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'PKB'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiNum=*on
058300050330     C                   Z-ADD     PiVal         VABPKB
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* VLB
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'VLB'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiNum=*on
059600030822     C                   Z-ADD     PiVal         VABVLB
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* QFT
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'QFT'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiNum=*on
060900030822     C                   Z-ADD     PiVal         VABQFT
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* CBO
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'CBO'
061700030822     C                   EVAL      VABCBO=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* TSP
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'TSP'
062600030822     C                   EVAL      VABTSP=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* VAS
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'VAS'
063500030822     C                   EVAL      VABVAS=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* VCA
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'VCA'
064400030822     C                   EVAL      VABVCA=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* GCA
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'GCA'
065300030822     C                   EVAL      VABGCA=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* CTM
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'CTM'
066200030822     C                   EVAL      VABCTM=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* FFD
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'FFD'
067100030822     C                   EVAL      VABFFD=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* VAD
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'VAD'
068000030822     C                   EVAL      VABVAD=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* GMA
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'GMA'
068900030822     C                   EVAL      VABGMA=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* GGA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'GGA'
069800030822     C                   EVAL      VABGGA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* GVA
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'GVA'
070700030822     C                   EVAL      VABGVA=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* TC1
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'TC1'
071600030822     C                   EVAL      VABTC1=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* TC2
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'TC2'
072500030822     C                   EVAL      VABTC2=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* ...
073000030822     C                   ENDIF
073100030822     C                   ENDDO
073200020204     C*
073300010330     C                   ENDSR
073400010607     C*----------------------------------------------------*
073500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
073600010607     C*----------------------------------------------------*
073700010607     C     IMPVAB        BEGSR
073800010607     C*
073900010607     C                   SETOFF                                       3132
074000030515     C*
074100010607     C                   MOVEL     datcor        VABAAS
074200040119     C                   MOVEL     datcor        VATAAS
074300010607     C                   MOVE      datcor        VABMGS
074400010607     C*
074500030715     C**********
074600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
074700030715     C**********
074800030715     C                   IF        wGiro = *zeros
074900040706     C*
075000040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
075100040706     C                   EVAL      depvinDTA = vinDTA
075200040113     C*
075300040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
075400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
075500040113     C                                      %trim(intcol(3))
075600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
075700031201     C     minu:maiu     XLATE     vindta        vindta
075800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075900030715     C                   DOW       posDa <= %len(%trim(vindta))
076000030715     C*
076100030715     C* Gestisco il 1° campo
076200030715     C                   IF        i = 1
076300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
076400030715     C                   EVAL      posDa = 1
076500030715     C* Eseguo lo scan x trovare la fine del primo campo
076600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076700030715     C* A questo punto "estraggo" il campo corrente
076800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076900030715     C                                                    (posA-posDa))
077000030715     C* X i campi successivi al 1°
077100030715     C                   ELSE
077200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
077300030715     C                   EVAL      posDa = posA + 1
077400030715     C* Eseguo lo scan x trovare la fine del campo corrente
077500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077600030715     C* Gestisco l'ultimo campo
077700030715     C                   IF        posA = *zeros
077800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
077900030715     C* A questo punto "estraggo" il campo corrente
078000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078100030715     C                                                    (posA-posDa))
078200030715     C                   LEAVE
078300030715     C                   ELSE
078400030715     C* A questo punto "estraggo" il campo corrente
078500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078600030715     C                                                    (posA-posDa))
078700030715     C                   ENDIF
078800030715     C                   ENDIF
078900030715     C* Incremento il contatore d campo
079000030715     C                   EVAL      i = i +1
079100030715     C                   ENDDO
079200030715     C*
079300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079400030715     C* delimitazione testo
079500030715     C                   EVAL      i = 1
079600030715     C                   DOW       i < %elem(SkSplitFLD)
079700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
079800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
079900030715     C                   EVAL      i = i + 1
080000030715     C                   ENDDO
080100030715     C                   EVAL      wGiro = 1
080200040706     C* Ripristino la 1° riga originale
080300040706     C                   EVAL      vinDTA = depvinDTA
080400030715     C                   ELSE
080500030715     C                   EVAL      wGiro = 2
080600031204     C**********
080700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
080800031204     C* d separatore campo
080900031204     C**********
081000031204     C                   EVAL      vindta = %trim(vindta)
081100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081200031204     C                   EVAL      vindta = %subst(vindta:2)
081300031204     C                   ENDDO
081400031204     C*
081500031204     C                   Z-ADD     *zeros        lunghInput        4 0
081600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
081800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
081900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
082000031204     C                   ENDDO
082100030715     C**********
082200030715     C* Effettuo lo split del campo dati d input
082300030715     C**********
082400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082500030715     C                   DOW       posDa <= %len(%trim(vindta))
082600030715     C*
082700030715     C* Gestisco il 1° campo
082800030715     C                   IF        i = 1
082900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083000030715     C                   EVAL      posDa = 1
083100030715     C* Eseguo lo scan x trovare la fine del primo campo
083200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083300030715     C* A questo punto "estraggo" il campo corrente
083400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083500030715     C                                                    (posA-posDa))
083600030715     C* X i campi successivi al 1°
083700030715     C                   ELSE
083800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083900030715     C                   EVAL      posDa = posA + 1
084000031204     C* Verifico che nn vi sia il campo nn valorizzato
084100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084200031204     C* Se campo nn valorizzato skippo al prossimo
084300031204     C                   EVAL      SkSplitCSV(i) = *blanks
084400031204     C                   EVAL      posA  = posA + 1
084500031204     C                   ELSE
084600030715     C* Eseguo lo scan x trovare la fine del campo corrente
084700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084800030715     C* Gestisco l'ultimo campo
084900030715     C                   IF        posA = *zeros
085000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085100030715     C* A questo punto "estraggo" il campo corrente
085200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085300030715     C                                                    (posA-posDa))
085400030715     C                   LEAVE
085500030715     C                   ELSE
085600030715     C* A questo punto "estraggo" il campo corrente
085700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085800030715     C                                                    (posA-posDa))
085900030715     C                   ENDIF
086000031204     C                   ENDIF
086100030715     C                   ENDIF
086200030715     C* Incremento il contatore d campo
086300030715     C                   EVAL      i = i +1
086400030715     C                   ENDDO
086500030715     C*
086600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086700030715     C* delimitazione testo
086800030715     C                   EVAL      i = 1
086900030715     C                   DOW       i < %elem(SkSplitCSV)
087000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
087100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087200030715     C                   EVAL      i = i + 1
087300030715     C                   ENDDO
087400030715     C*
087500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
087600030715     C                   EVAL      i = 1
087700030715     C                   DOW       i < %elem(SkSplitFLD)
087800030715     C*
087900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
088000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
088100030715     C                             SkSplitCSV(i) <> *blanks
088200030721     C*** ATB
088300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
088400030721     C                   EVAL      VABATB = SkSplitCSV(i)
088500030721     C                   ENDIF
088600030715     C*** RSD
088700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
088800050927     C                   EVAL      VABRSD = SkSplitCSV(i)
088900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
089000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
089100030715     C* ==
089200030715     C                   ENDIF
089300030721     C*** RD2
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
089500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030715     C*** IND
089800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
089900030715     C                   EVAL      VABIND = SkSplitCSV(i)
090000030715     C                   ENDIF
090100030715     C*** LOD
090200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
090300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
090400030715     C                   ENDIF
090500030715     C*** PRD
090600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
090700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
090800030715     C                   ENDIF
090900030721     C*** NZD
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
091100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030715     C*** RMA
091400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
091500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
091600030715     C                   ENDIF
091700030715     C*** NOT
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
091900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030715     C*** NT2
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
092300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030715     C*** VCA
092600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
092700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
092800030715     C                   ENDIF
092900030715     C*** VAS
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
093100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030715     C*** FFD
093400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
093500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
093600030715     C                   ENDIF
093700030721     C*** GC1
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
093900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** GC2
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
094300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** TSP
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
094700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*** NAS
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
095100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030721     C*** TIC
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
095500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** GCA
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** XCO
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
096300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** CTM
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** TCR
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
097100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** CTS
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
097500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** FTM
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** VAD
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
098300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** GMA
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** GGA
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
099100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** GVA
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
099500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** TC1
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** TC2
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
100300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** SCL
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** RMO
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
101100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** NMO
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
101500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101600030721     C                   ENDIF
101700040119     C*** CBO
101800040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101900040706     C                   EVAL      VABCBO = SkSplitCSV(i)
102000040119     C                   ENDIF
102100030721     C*
102200040706     C* Reperisco quindi i campi numerici...
102300050927     C*** RMN
102400040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
102500040706     C                   EVAL      PiStr=SkSplitCSV(i)
102600040706     C                   EXSR      CHKNUM
102700040706     C                   IF        PiInt=*on
102800040706     C                   Z-ADD     PiVal         VABRMN
102900040706     C                   ELSE
103000040706     C                   SETON                                        32
103100040706     C                   EVAL      VABRMN = 1
103200040706     C                   EVAL      vinmsg = %trimr(vinmsg)
103300040706     C                             + ' ' + 'VABRMN'
103400040706     C                   ENDIF
103500040706     C                   ENDIF
103600030721     C*** CAD
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
103800030721     C                   EVAL      PiStr=SkSplitCSV(i)
103900030721     C                   EXSR      CHKNUM
104000030721     C                   IF        PiInt=*on
104100030721     C                   Z-ADD     PiVal         Num5_0
104200030721     C                   MOVEL(p)  Num5_0        VABCAD
104300030721     C                   ELSE
104400030721     C                   SETON                                        32
104500030721     C                   EVAL      VABCAD = *zeros
104600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104700030721     C                             + ' ' + 'VABCAD'
104800030721     C                   ENDIF
104900030721     C                   ENDIF
105000030721     C*** NCL
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105200030721     C                   EVAL      PiStr=SkSplitCSV(i)
105300030721     C                   EXSR      CHKNUM
105400030721     C                   IF        PiInt=*on
105500030721     C                   Z-ADD     PiVal         VABNCL
105600030721     C                   ELSE
105700030721     C                   SETON                                        32
105800030721     C                   EVAL      VABNCL = *zeros
105900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106000040706     C                             + ' ' + 'VABNCL'
106100030721     C                   ENDIF
106200030721     C                   ENDIF
106300030721     C*** PKB
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106500030721     C                   EVAL      PiStr=SkSplitCSV(i)
106600030721     C                   EXSR      CHKNUM
106700030721     C                   IF        PiNum=*on
106800050330     C                   Z-ADD(h)  PiVal         VABPKB
106900030721     C                   ELSE
107000030721     C                   SETON                                        32
107100030721     C                   EVAL      VABPKB = *zeros
107200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107300030721     C                             + ' ' + 'VABPKB'
107400030721     C                   ENDIF
107500030721     C                   ENDIF
107600030721     C*** CAS
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
107800030721     C                   EVAL      PiStr=SkSplitCSV(i)
107900030721     C                   EXSR      CHKNUM
108000030721     C                   IF        PiNum=*on
108100030721     C                   Z-ADD     PiVal         VABCAS
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      VABCAS = *zeros
108500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108600030721     C                             + ' ' + 'VABCAS'
108700030721     C                   ENDIF
108800030721     C                   ENDIF
108900030721     C*** CCM
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiInt=*on      AND
109400030721     C                             PiVal<=9999999 AND
109500030721     C                             PiVal>*zeros
109600030721     C                   Z-ADD     PiVal         VABCCM
109700040119     C                   Z-ADD     PiVal         VATCCM
109800030721     C                   ELSE
109900030721     C                   SETON                                        32
110000030721     C                   EVAL      VABCCM = *zeros
110100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110200040119     C                             + ' ' + 'VABCCM VATCCM'
110300030721     C                   ENDIF
110400030721     C                   ENDIF
110500030721     C*** LNP
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiInt=*on    AND
111000030721     C                             PiVal<=999   AND
111100030721     C                             PiVal>*zeros
111200030721     C                   Z-ADD     PiVal         VABLNP
111300040119     C                   Z-ADD     PiVal         VATLNP
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABLNP = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800040119     C                             + ' ' + 'VABLNP VATLNP'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** NRS
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on AND
112600030721     C                             PiVal<=99
112700030721     C                   Z-ADD     PiVal         VABNRS
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100030721     C                             + ' ' + 'VABNRS'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** LNA
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on    AND
113900030721     C                             PiVal<=999   AND
114000030721     C                             PiVal>*zeros
114100030721     C                   Z-ADD     PiVal         VABLNA
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      VABLNA = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABLNA'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** CTR
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiInt=*on  AND
115400030721     C                             PiVal<=999
115500030721     C                   Z-ADD     PiVal         VABCTR
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABCTR'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** IAS
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiNum=*on
116700030721     C                   Z-ADD     PiVal         VABIAS
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABIAS'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** VLB
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiNum=*on
117900030721     C                   Z-ADD     PiVal         VABVLB
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABVLB'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** QFT
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100030721     C                   Z-ADD     PiVal         VABQFT
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABQFT'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** NCD
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on      AND
120300030721     C                             PiVal<=9999999
120400030721     C                   Z-ADD     PiVal         VABNCD
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABNCD'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** NCA
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on      AND
121600030721     C                             PiVal<=9999999
121700030721     C                   Z-ADD     PiVal         VABNCA
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABNCA'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** ZNC
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on AND
122900030721     C                             PiVal<=99
123000030721     C                   Z-ADD     PiVal         VABZNC
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABZNC'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** DCR
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on       AND
124200030721     C                             PiVal<=99999999
124300030721     C                   Z-ADD     PiVal         VABDCR
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABDCR'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** HCR
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on   AND
125500030721     C                             PiVal<=9999
125600030721     C                   Z-ADD     PiVal         VABHCR
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABHCR'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** VMD
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiNum=*on
126800030721     C                   Z-ADD     PiVal         VABVMD
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABVMD'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** ANT
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on        AND
128000030721     C                             PiVal<=999999999
128100030721     C                   Z-ADD     PiVal         VABANT
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABANT'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** CMO
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on
129300030721     C                   Z-ADD     PiVal         Num5_0
129400030721     C                   MOVEL(p)  Num5_0        VABCMO
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      VABCMO = *zeros
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABCMO'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200040706     C*
130300050927     C* Gestisco i campi "personalizzati"
130400050927     C*
130500050927     C*** NSP
130600050927     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
130700050927     C                   EVAL      wVABNSPalfa = SkSplitCSV(i)
130800050927     C                   ENDIF
130900050927     C*
131000050927     C*** VATNOT_A => PERSONA DI RIFERIMENTO DESTINATARIO
131100050927     C                   IF        %trim(SkSplitFLD(i))='VATNOT_A'
131200050927     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
131300050927     C                   ENDIF
131400050927     C*
131500050927     C*** VATNOT_B => TELEFONO DESTINATARIO
131600050927     C                   IF        %trim(SkSplitFLD(i))='VATNOT_B'
131700050927     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
131800050927     C                   ENDIF
131900050927     C*
132000050927     C*** VATNOT_E => PARTE FINALE DEL "CHI SONO"
132100050927     C                   IF        %trim(SkSplitFLD(i))='VATNOT_E'
132200050927     C                   EVAL      wVATNOT_E = SkSplitCSV(i)
132300050927     C                   ENDIF
132400030715     C*
132500030715     C                   ENDIF
132600030715     C                   EVAL      i = i + 1
132700030715     C                   ENDDO
132800050927     C*
132900050927     C* A fine reperimento dati gestisco considerazioni particolari
133000050927     C                   EVAL      wVATNOT_E = %trim(wVABNSPalfa) +
133100050927     C                                         %trim(wVATNOT_E)
133200050927     C*** NSP/RMN
133300050927     C                   EVAL      PiStr=%trim(wVATNOT_E)
133400050927     C                   EXSR      CHKNUM
133500050927     C                   IF        PiInt=*on
133600050927     C                   Z-ADD     PiVal         VABNSP
133700050927     C                   Z-ADD     PiVal         VABRMN
133800050927     C                   ELSE
133900050927     C                   SETON                                        31
134000050927     C                   EVAL      VABNSP = *zeros
134100050927     C                   EVAL      VABRMN = 1
134200050927     C                   EVAL      vinmsg = %trimr(vinmsg)
134300050927     C                             + ' ' + 'VABNSP VABRMN'
134400050927     C                   ENDIF
134500040706     C*
134600040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
134700040706     C* Reperisco la provincia dal CAP e dalla località
134800040706     C                   IF        VABCAD <> *blanks
134900040706     C                   CLEAR                   TISI95DS
135000040706     C                   EVAL      I95TCN = '3'
135100040706     C                   Z-ADD     datcor        I95DAT
135200040706     C                   EVAL      I95CAP = VABCAD
135300040706     C                   EVAL      I95LOC = VABLOD
135400040706     C                   CALL      'TISI95R'
135500040706     C                   PARM                    TISI95DS
135600040706     C                   EVAL      VABPRD = O95PRV
135700040706     C                   ENDIF
135800040706     C*
135900040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
136000040706     C* d valorizzarli sempre entrambi
136100040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
136200040706     C                   Z-ADD     VABNSP        VABRMN
136300040706     C                   ENDIF
136400040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
136500040706     C                   MOVEL     VABRMN        VABRMA
136600040706     C                   ENDIF
136700040113     C*
136800040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
136900040113     C                   IF        VABCAS > *zeros
137000040119     C                   EVAL      VABVCA = 'EUR'
137100040119     C                   IF        VABCBO = '1'
137200040113     C                   EVAL      VABCBO = '4'
137300040119     C                   ENDIF
137400040119     C                   IF        VABCBO = '2'
137500040119     C                   EVAL      VABCBO = '6'
137600040119     C                   ENDIF
137700040113     C                   ENDIF
137800020204     C*
137900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
138000020204     C                   EXSR      CHKIMPDIV
138100030715     C                   ENDIF
138200020204     C*
138300010607     C                   ENDSR
138400010607     C*----------------------------------------------------*
138500010601
138600020204
138700020204     C*----------------------------------------------------*
138800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
138900020204     C*----------------------------------------------------*
139000020204     C     CHKIMPDIV     BEGSR
139100020204     C*
139200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
139300020204     C                   Z-ADD     *zeros        wrkDec            9 9
139400020204     C*
139500020204     C* Come prima cosa effettuo considerazioni sulla divisa
139600020204     C                   IF        vabIAS > *zeros
139700020204     C                   IF        vabVAS <> 'EUR'
139800020204     C                   EVAL      vabVAS =  'ITL'
139900020204     C                   ENDIF
140000020204     C                   ENDIF
140100020204     C*
140200020204     C                   IF        vabCAS > *zeros
140300020204     C                   IF        vabVCA <> 'EUR'
140400020204     C                   EVAL      vabVCA =  'ITL'
140500020204     C                   ENDIF
140600020204     C                   ENDIF
140700020204     C*
140800020204     C                   IF        vabVMD > *zeros
140900020204     C                   IF        vabVAD <> 'EUR'
141000020204     C                   EVAL      vabVAD =  'ITL'
141100020204     C                   ENDIF
141200020204     C                   ENDIF
141300020204     C*
141400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
141500020204     C                   Z-ADD     vabIAS        wrkDec
141600020204     C                   IF        wrkDec > *zeros
141700020204     C                   IF        vabVAS = 'ITL'
141800020204     C                   EVAL      vabIAS = *zeros
141900020204     C                   ENDIF
142000020204     C                   ENDIF
142100020204     C*
142200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
142300020204     C                   Z-ADD     vabCAS        wrkDec
142400020204     C                   IF        wrkDec > *zeros
142500020204     C                   IF        vabVCA = 'ITL'
142600020204     C                   EVAL      vabCAS = *zeros
142700020204     C                   ENDIF
142800020204     C                   ENDIF
142900020204     C*
143000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
143100020204     C                   Z-ADD     vabVMD        wrkDec
143200020204     C                   IF        wrkDec > *zeros
143300020204     C                   IF        vabVAD = 'ITL'
143400020204     C                   EVAL      vabVMD = *zeros
143500020204     C                   ENDIF
143600020204     C                   ENDIF
143700020204     C*
143800020204     C                   ENDSR
143900020204     C***
144000020204
144100010330
144200010330
144300010330     C*----------------------------------------------------*
144400010330     C*  CONTROLLO NUMERICITA' CAMPI
144500010330     C*----------------------------------------------------*
144600010330     C     CHKNUM        BEGSR
144700010330     C*
144800010606     C                   IF        PiDecChr = *blanks
144900030715     C                   EVAL      PiDecChr = CharNUM
145000010606     C                   ENDIF
145100010606     C*
145200010606     C                   CALL(e)   'ISNUMERIC'
145300010330     C                   PARM                    PiStr            30
145400010606     C                   PARM                    PiDecChr          1
145500010330     C                   PARM      *ZEROS        PiVal            30 9
145600010330     C                   PARM      '0'           PiInt             1
145700010330     C                   PARM      '0'           PiNum             1
145800010330     C                   IF        %error
145900010606     C                   EVAL      PiNum=*off
146000010330     C                   ENDIF
146100010330     C*
146200010330     C                   ENDSR
146300010330     C***
146400010330
146500010601
146600010601
146700010601
146800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
146900010601     C     repfil        BEGSR
147000010601     C*
147100010601     C                   if        invfil = *zeros and
147200010601     C                             depfil > *zeros and
147300010629     C                             (vinflg = *blanks or
147400010629     C                              vinflg = *zeros)
147500010601     C
147600010601     C                   eval      invfil = depfil
147700010601     C                   endif
147800010601     C*
147900010601     C                   if        depfil <> invfil and
148000010601     C                             invfil > *zeros
148100010601     C                   eval      flgMulti = '1'
148200010601     C                   if        vinflg = *blanks
148300010601     C                   add       1             cntNonEl
148400010601     C                   endif
148500010601     C                   endif
148600010601     C*
148700010601     C                   if        vinflg = '2'
148800010601     C                   eval      flgStato = '2'
148900010601     C                   endif
149000010601     C*
149100010601     C                   ENDSR
149200010601     C***
149300010601
149400010601
149500010601
149600010330
149700040119      /TITLE Invio dei dati al punto operativo.
149800040119     C     invio         BEGSR
149900040119     C*
150000040119     C* 1° invio FIVAT
150100040119     C                   reset                   dscmz
150200040119     C                   move      vlrpoi        cmzdst
150300040119     C                   eval      cmzfld = 'FIVATWWR'
150400040119     C                   eval      cmzmbd = vlrhdl
150500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
150600040119     C***                if        prmfir = *blanks
150700040119     C                   eval      cmzfla = 'FIVAT00F'
150800040119     C                   eval      cmzmba = 'FIVAT00F'
150900040119     C***                else
151000040119     C***                eval      cmzfla = prmfir
151100040119     C***                eval      cmzmba = prmfir
151200040119     C***                endif
151300040119     C                   eval      cmznrr = *zeros
151400040119     C                   move      §ctrokvt      cmznrr
151500040119     C                   eval      cmzlba = vlrfl1
151600040119     C                   call(e)   'TIS711C'
151700040119     C                   parm                    dscmz
151800040119     C                   parm      *blanks       esito
151900040119     C                   if        %error
152000040119     C                             or cmzerr = '1'
152100040119     C                             or esito  = '1'
152200040119     C                   eval      wrkesito = '3'
152300040119     C                   else
152400040119     C*
152500040119     C* 2° invio FIVAB
152600040119     C                   reset                   dscmz
152700040119     C                   move      vlrpoi        cmzdst
152800040119     C                   eval      cmzfld = vlrfou
152900040119     C                   eval      cmzmbd = vlrhdl
153000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153100040119     C***                if        prmfir = *blanks
153200040119     C                   eval      cmzfla = 'FIVAB00F'
153300040119     C                   eval      cmzmba = 'FIVAB00F'
153400040119     C***                else
153500040119     C***                eval      cmzfla = prmfir
153600040119     C***                eval      cmzmba = prmfir
153700040119     C***                endif
153800040119     C                   eval      cmznrr = *zeros
153900040119     C                   move      §ctrokvb      cmznrr
154000040119     C                   eval      cmzlba = vlrfl1
154100040119     C                   call(e)   'TIS711C'
154200040119     C                   parm                    dscmz
154300040119     C                   parm      *blanks       esito
154400040119     C                   if        %error
154500040119     C                             or cmzerr = '1'
154600040119     C                             or esito  = '1'
154700040119     C                   eval      wrkesito = '3'
154800040119     C                   endif
154900040119     C                   endif
155000040119     C*
155100040119     C                   ENDSR
155200040119     C***
155300010601
155400010601
155500010601
155600010601
155700010601      /TITLE Invio dei dati al punto operativo.
155800010601     C     opeini        BEGSR
155900010601     C*
156000010601     C* Inizializzo flag e contatori operativi
156100010601     C                   movel     '0'           flgGiro           1
156200010601     C                   movel     '0'           flgMulti          1
156300010601     C                   movel     '1'           flgStato          1
156400010615     C                   movel     '0'           flgOk             1
156500010601     C                   z-add     *zeros        cntNonEl         10 0
156600010601     C                   z-add     *zeros        depfil            3 0
156700010601     C                   z-add     *zeros        invfil            3 0
156800010601     C*
156900010601     C                   ENDSR
157000010601     C***
157100010601
157200010601
157300050421
157400050421
157500070411
157600070411     C     *pssr         BEGSR
157700070411     C*
157800070411     C                   if        %open(tivin00r)
157900070411     C                   close     tivin00r
158000070411     C                   endif
158100070411     C                   if        %open(fivabwwr)
158200070411     C                   close     fivabwwr
158300070411     C                   endif
158400070411     C                   if        %open(fivatwwr)
158500070411     C                   close     fivatwwr
158600070411     C                   endif
158700070411     C*
158800070411     C* Effettuo la chiamata al CLLE preposto
158900070411     C                   call(e)   'TITVVTC'
159000070411     C                   parm                    parccm
159100070411     C                   parm                    parmbr
159200070411     C                   parm      '2'           paropz
159300070411     C*
159400070411     C                   eval      wrkesito = '2'
159500070411     C*
159600070411     C                   seton                                        LR
159700070411     C*
159800070411     C                   ENDSR     '*CANCL'
159900070411     C***
160000070411
160100050421
160200010601
160300010330
160400010330
160500000613     C     *inzsr        BEGSR
160600990910     C*
160700990910     C     *entry        plist
160800990920     C                   parm                    tivlrds
160900990921     C                   parm      wrkesito      esito
161000000724     C                   parm                    prmlit
161100000710     C                   parm                    prmfir
161200010330     C*
161300010330     C* CALCOLA LA DATA CORRENTE
161400010330     C                   time                    wn14             14 0
161500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
161600010330     C                   z-add     wn8           g08dat
161700010330     C                   z-add     *zeros        g08inv
161800010330     C                   movel     '0'           g08err
161900010330     C                   call      'XSRDA8'
162000010330     C                   parm                    wlbda8
162100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
162200000613     C*
162300000613     C                   ENDSR
162400000613     C***
162500990908
162600050927     Otitvo6p   E            riepilogo         2
162700990915     O                                              'Upload via Internet'
162800990915     O                                           +1 'Traduzione TIVIN00R -'
162900021025     O                                           +1 'FIVABWWR'
163000990915     O                                           +1 'Testate bolle'
163100990915     O          E            riepilogo   2
163200990915     O                       wrkdata
163300990915     O                       wrkora              +1
163400990915     O                       procname            +1
163500990915     O          E            riepilogo   2
163600990915     O                                              'Cliente..................:'
163700010605     O                       VABCCM        z     +1
163800990915     O          E            riepilogo   2
163900990920     O                                              'Riferimento Strategi.....:'
164000990920     O                       vlrhdl              +1
164100990915     O          E            riepilogo   2
164200990915     O                                              'Giusti...................:'
164300040119     O                       §CTROKVB      2   +  1
164400990915     O          E            riepilogo   2
164500990915     O                                              'Sbagliati e corretti.....:'
164600971022     O                       §CTRMO        2   +  1
164700990915     O          E            riepilogo   2
164800990915     O                                              'Sbagliati e scartati.....:'
164900971022     O                       §CTRNO        2   +  1
165000000613
165100050927     Otitvo6ps  E            testdett          2
165200000613     O                                              'Upload via Internet'
165300000613     O                                           +1 'Traduzione TIVIN00R -'
165400021025     O                                           +1 'FIVABWWR'
165500000613     O                                           +1 'Testate bolle'
165600000616     O          E            testdett    3
165700000613     O                                           +2 'N° rec'
165800000613     O                                           +3 'Anteprima contenuto'
165900000616     O          E            rigadett    2
166000000613     O                       rrnum               +2
166100000621     O                       recko               +3
166200000616     O          E            findett     2
166300000613     O                       wrkdata
166400000613     O                       wrkora              +1
166500000613     O                       procname            +1
166600000616     O          E            findett     2
166700000613     O                                              'Cliente..................:'
166800010605     O                       VABCCM        z     +1
166900000616     O          E            findett     2
167000000613     O                                              'Riferimento Strategi.....:'
167100000613     O                       vlrhdl              +1
167200000616     O          E            findett     2
167300000613     O                                              'Giusti...................:'
167400040119     O                       §CTROKVB      2   +  1
167500000616     O          E            findett     2
167600000613     O                                              'Sbagliati e corretti.....:'
167700000613     O                       §CTRMO        2   +  1
167800000616     O          E            findett     2
167900000613     O                                              'Sbagliati e scartati.....:'
168000000613     O                       §CTRNO        2   +  1
168100000613** CMD - COMANDI CL
168200050927OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVO6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
168300050927OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVO6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
168400050927DLTOVR FILE(TITVO6P TITVO6PS) LVL(*)
168500010606
168600000613
168700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
168800050927VABRSD;VATNOT_A;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;VATNOT_B;
168900050927VABNCL;VABPKB;VABNSP;VATNOT_E;VABDCR;VABNOT
169000040113
