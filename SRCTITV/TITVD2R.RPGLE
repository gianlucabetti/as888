000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800040706     Ftitvd2p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040706     Ftitvd2ps  O    f  198        PRINTER usropn
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
004000040706     D wBarcodeDa      s             11  0 INZ(*zeros)
004100040706     D wBarcodeA       s             11  0 INZ(*zeros)
004200040709     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004300040119     D parccm          s              8    INZ(*blanks)
004400040119     D parmbr          s             10    INZ(*blanks)
004500040119     D paropz          s              1    INZ(*blanks)
004600040119     D chkcall         s              1    INZ(*blanks)
004700040706     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004800030715     D*------------------
004900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005000030715     D*------------------
005100030715     D SkSplitFLD      S             10    DIM(100)
005200030715     D SkSplitCSV      S            256    DIM(100)
005300030715     D CharCSV         S              1
005400030715     D CharTXT         S              1
005500030715     D CharNUM         S              1
005600030715     D posDa           S              3  0 INZ(*zeros)
005700030715     D posA            S              3  0 INZ(*zeros)
005800030715     D i               s              3  0 INZ(1)
005900030715     D wGiro           s              1  0 INZ(*zeros)
006000030822     D*------------------
006100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006200030822     D*------------------
006300030822     D posDaDft        S              3  0 INZ(*zeros)
006400030822     D posADft         S              3  0 INZ(*zeros)
006500030822     D j               s              3  0 INZ(1)
006600030822     D wGiroDft        s              1  0 INZ(*zeros)
006700010330     D*------------------
006800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006900010330     D*------------------
007000010330     D WLBDA8          DS                  INZ
007100010330     D  G08DAT                 1      8  0
007200010330     D  G08INV                 9     16  0
007300010330     D  G08ERR                17     17
007400010330     D  G08TGI                18     22  0
007500040709     D*------------------
007600040709     D* DS CALCOLO CHECK-DIGITS MOD103 (TRUL28R4)
007700040709     D*------------------
007800090916     D TRUL28DSX     E DS
007900031201     D*------------------
008000031201     D* Costanti
008100031201     D*------------------
008200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400010330
008500010330
008600010330
008700990915     C                   time                    wrkdata
008800990915     C                   time                    wrkora
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200010601     C*
009300010601     C                   exsr      opeini
009400010605     C                   exsr      rwvab
009500010601     C*
009600010601     C                   seton                                        lr
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010200010601     C*--------------------------------------------------------
010300010601     C     PREELA        BEGSR
010400010601     C*
010500010601     C* SE OCCORRE SPEDIRE IN FILIALE
010600010601     C                   if        invfil <> *zeros and
010700010601     C                             flgGiro = '0'
010800010601     C*
010900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011000010601     C                   eval      flgGiro = '1'
011100000724     C*
011200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011400010601     C     invfil        chain     azorg01l
011500000724     C                   if        %found
011600000724     C                   movel(p)  CMD(1)        depcmd
011700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011800000724     C*
011900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000724     C                   PARM                    depcmd
012300000724     C                   PARM                    LENGH
012400000724     C*
012500000724     C                   endif
012600000616     C*
012700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012800000616     C                   movel(p)  CMD(2)        depcmd
012900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013000000616     C*
013100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013400000616     C                   PARM                    depcmd
013500000616     C                   PARM                    LENGH
013600000613     C*
013700040706     C                   if        not %open(titvd2ps)
013800040706     C                   open      titvd2ps
013900030715     C                   except    testdett
014000000613     C                   endif
014100010601     C*
014200010601     C                   endif
014300010601     C*
014400010601     C                   ENDSR
014500010601     C***
014600010601
014700010601
014800010601
014900010601
015000010601     C*--------------------------------------------------------
015100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015200010601     C*--------------------------------------------------------
015300010601     C     ENDELA        BEGSR
015400990915     C*
015500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015600040119     C*
015700040119     C* Effettuo la chiamata al CLLE preposto
015800040119     C                   call(e)   'TITVVTC'
015900040119     C                   parm                    parccm
016000040119     C                   parm                    parmbr
016100040119     C                   parm      '2'           paropz
016200050201     C*
016300050201     C* Effettuo lancio TISI95 solo x chiusura
016400050201     C                   CLEAR                   TISI95DS
016500050201     C                   EVAL      I95TLA = 'C'
016600050201     C                   CALL      'TISI95R'
016700050201     C                   PARM                    TISI95DS
016800000613     C*
016900040706     C                   if        %open(titvd2ps)
017000000616     C                   except    findett
017100040706     C                   close     titvd2ps
017200000613     C                   endif
017300000616     C*
017400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017500011113     C                   Z-ADD     150           LENGH            15 5
017600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017700000616     C                   PARM                    CMD(3)
017800000616     C                   PARM                    LENGH
017900000616     C*
018000010601     C                   ENDSR
018100010601     C***
018200010601
018300010601
018400010601
018500000613
018600000613     C*--------------------------------------------------------
018700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018800000613     C*--------------------------------------------------------
018900000621     C     STPR          BEGSR
019000000613     C*
019100040706     C                   if        not %open(titvd2p)
019200040706     C   50              open      titvd2p
019300990915     C                   endif
019400990915     C*
019500030715     C   50              except    riepilogo
019600990915     C*
019700040706     C                   if        %open(titvd2p)
019800040706     C   50              close     titvd2p
019900990914     C                   endif
020000000613     C*
020100000613     C                   ENDSR
020200000613     C***
020300000613
020400010601
020500010601
020600010330     C*--------------------------------------------------------
020700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020800010330     C*--------------------------------------------------------
020900010605     C     RWVAB         BEGSR
021000010330     C*
021100010330     C                   if        not %open(tivin00r)
021200010330     C                   open      tivin00r
021300010330     C                   endif
021400021025     C                   if        not %open(fivabwwr)
021500021025     C                   open      fivabwwr
021600010330     C                   endif
021700040119     C*
021800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021900040119     C                   exsr      prevat
022000040119     C*
022100040119     C                   if        chkcall = '0'
022200040119     C*
022300040119     C                   if        not %open(fivatwwr)
022400040119     C                   open      fivatwwr
022500040119     C                   endif
022600010330     C*
022700040119     C                   clear                   §CTROKVB          7 0
022800040119     C                   clear                   §CTROKVT          7 0
022900040119     C                   clear                   §CTRMO            7 0
023000040119     C                   clear                   §CTRNO            7 0
023100040706     C*
023200040706     C* Eseguo appositamente una 1° lettura fuori ciclo x ignorare la 1° riga
023300040706     C                   setoff                                       70
023400040706     C                   read      tivin00r                               70
023500040706     C                   eval      vinflg = '1'
023600040706     C  N70              update    tivin000
023700040706     C*
023800040706     C  N70              do        *HIVAL
023900010330     C*
024000040706     C                   read      tivin00r                               70
024100010618     C*
024200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024300010618     C                   if        vinflg = '1'
024400010618     C                   eval      flgOk = '1'
024500010618     C                   endif
024600010618     C*
024700040316     C                   if        (vindta > *blanks AND
024800040316     C                              %trim(vindta) <> *all';') OR
024900040316     C                             wGiro = *zeros
025000010330     C                   add       1             rrnum
025100010330     C*
025200010601     C                   if        *in70 = *off and
025300010330     C                             (vinflg = *blanks
025400010330     C                              or vinflg = '0'
025500010330     C                              or vinflg = '2')
025600010330     C*
025700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025800010711     C                   if        vinflg = *blanks or vinflg = '0'
025900010711     C                   clear                   vinmsg
026000010711     C                   endif
026100010601     C*
026200010330     C                   exsr      inzvar
026300010605     C                   exsr      impvab
026400010601     C*
026500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026600010605     C                   eval      depfil = VABLNP
026700010601     C                   exsr      repfil
026800010601     C                   if        depfil = invfil
026900021025     C                   if        vlrpoi = 999
027000030822     C                   move(P)   invfil        VABFGS
027100040119     C                   move(P)   invfil        VATFGS
027200021025     C                   else
027300030822     C                   move(P)   vlrpoi        VABFGS
027400040119     C                   move(P)   vlrpoi        VATFGS
027500021025     C                   endif
027600010601     C*
027700010601     C                   exsr      PREELA
027800010601     C*
027900010604     C* Ebbene...
028000010604     C*
028100040119     C  N31              add       1             §CTROKVB
028200040119     C   32              add       1             §CTRMO
028300040119     C   31              add       1             §CTRNO
028400030822     C                   if        wGiro = 2
028500040119     C                   if        depspe =  *zeros OR
028600040119     C                             depspe <> vabNSP
028700040119     C  N31              exsr      wrivab
028800040119     C  N31              eval      depspe = vabNSP
028900040119     C                   endif
029000040119     C                   endif
029100010604     C*
029200010604     C                   if        *in31 = *off and
029300010604     C                             *in32 = *off
029400010604     C                   eval      vinflg = '1'
029500010604     C                   else
029600010604     C                   eval      recko = vindta
029700010604     C                   except    rigadett
029800010604     C                   eval      vinflg = '2'
029900010604     C                   endif
030000010604     C                   endif
030100010601     C*
030200010604     C                   endif
030300010604     C*
030400010330     C                   else
030500010330     C                   eval      vinflg = '1'
030600010330     C                   endif
030700010601     C*
030800010601     C  N70              update    tivin000
030900010330     C*
031000030822     C  N70              enddo
031100040119     C*
031200040119     C                   endif                                                  (endif - chkcall)
031300010601     C*
031400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031500010601     C                   if        cntNonEl = *zeros or
031600010601     C                             flgMulti = '0'
031700010330     C* Se non ci sono record con errori ...
031800010601     C                   if        §ctrno = 0 and
031900010604     C                             §ctrmo = 0 and
032000010601     C                             flgStato <> '2'
032100010330     C* ... restituisco esito OK.
032200010330     C                   eval      wrkesito = '0'
032300010330     C                   else
032400040119     C                   if        §ctrokvb > 0
032500010330     C                   eval      wrkesito = '1'
032600010330     C                   else
032700010615     C                   if        flgOk = '0'
032800010615     C                   eval      wrkesito = '2'
032900010615     C                   else
033000010615     C                   eval      wrkesito = '6'
033100010615     C                   endif
033200010330     C                   endif
033300010330     C                   endif
033400010601     C                   else
033500010601     C                   eval      wrkesito = '9'
033600010601     C                   endif
033700010330     C*
033800010330     C                   if        %open(tivin00r)
033900010330     C                   close     tivin00r
034000010330     C                   endif
034100021025     C                   if        %open(fivabwwr)
034200021025     C                   close     fivabwwr
034300010330     C                   endif
034400040512     C                   if        %open(fivatwwr)
034500040512     C                   close     fivatwwr
034600040512     C                   endif
034700010601     C*
034800010601     C                   if        vlrpoi <> 999
034900010601     C                   eval      invfil = vlrpoi
035000010601     C                   endif
035100010330     C*
035200040119     C                   if        §ctrokvb > 0
035300010601     C                             and invfil > *zeros
035400010330     C                   exsr      invio
035500010330     C                   endif
035600010601     C*
035700010618     C                   if        flgGiro = '1'
035800010601     C                   exsr      endela
035900010618     C                   endif
036000010330     C*
036100010330     C                   ENDSR
036200010330     C***
036300040119
036400040119
036500040119
036600040119     C*----------------------------------------------------*
036700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036800040119     C*----------------------------------------------------*
036900040119     C     WRIVAB        BEGSR
037000040119     C*
037100040119     C                   write     fivab000                                     => scarico il VAB
037200040119     C*
037300040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
037400041201     C  N51              exsr      wrivat
037500040119     C*
037600040119     C                   ENDSR
037700040119
037800040119
037900040119
038000040119     C*----------------------------------------------------*
038100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038200040119     C*----------------------------------------------------*
038300040706     C     WRIVAT        BEGSR
038400040706     C*
038500040706     C* Attribuisco lo stesso numero numero spedizione del file testate
038600040706     C                   EVAL      VATNSP = VABNSP
038700040119     C*
038800040119     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
038900040706     C     wBarcodeDA    DO        wBarcodeA     wBarcodeNum      11 0
039000040706     C*
039100040706     C* Imposto la parte costante del barcode
039200041025     C                   EVAL      VATNOT = '00340380'
039300040706     C*
039400040706     C* Compongo la parte variabile
039500040706     C                   MOVE(P)   wBarcodeNum   wBarcodeAlfa     11
039600040706     C                   EVAL      VATNOT = %trim(VATNOT) + wBarcodeAlfa
039700040706     C* Eseguo calcolo check-digit
039800090916     C                   CLEAR                   TRUL28DSX
039900090916     C                   EVAL      I284STD = 'E10'
040000040709     C                   EVAL      I284COD = VATNOT
040100040709     C                   CALL      'TRUL28R4'
040200090916     C                   PARM                    TRUL28DSX
040300040706     C* Se tutto ok scarico il buffer del FNVAT
040400040709     C                   IF        O284ERR <> '1'
040500040709     C                   EVAL      VATNOT = O284COD
040600040706     C                   WRITE     FIVAT000
040700040706     C                   ADD       1             §CTROKVT
040800040706     C                   ELSE
040900040706     C                   SETON                                        32
041000040706     C                   EVAL      vinmsg = %trimr(vinmsg)
041100040706     C                             + ' ' + 'VATNOT'
041200040706     C                   ENDIF
041300040119     C                   ENDDO
041400040119     C*
041500040119     C                   ENDSR
041600040119
041700040119
041800040119
041900040119     C*----------------------------------------------------*
042000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042100040119     C*----------------------------------------------------*
042200040119     C     PREVAT        BEGSR
042300040119     C*
042400040119     C* Compongo il nome del membro da dare al FIVATWWR
042500040119     C                   eval      parmbr = vlrhdl
042600040119     C                   movel     'M'           parmbr
042700040706     C                   eval      parccm = '00052805'
042800040119     C                   eval      paropz = '1'
042900040119     C* Effettuo la chiamata al CLLE preposto
043000040119     C                   call(e)   'TITVVTC'
043100040119     C                   parm                    parccm
043200040119     C                   parm                    parmbr
043300040119     C                   parm                    paropz
043400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043500040119     C                   if        %error
043600040119     C                   movel     '1'           chkcall
043700040119     C                   else
043800040119     C                   movel     '0'           chkcall
043900040119     C                   endif
044000040119     C*
044100040119     C                   ENDSR
044200010601
044300010601
044400010601
044500010330     C*----------------------------------------------------*
044600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044700010330     C*----------------------------------------------------*
044800040119     C     INZVAR        BEGSR
044900010330     C*
045000030822     C* Inizializzo variabili di wrk
045100020204     C                   Z-ADD     *zeros        Num5_0            5 0
045200040706     C                   Z-ADD     *zeros        wBarcodeDa
045300040706     C                   Z-ADD     *zeros        wBarcodeA
045400030822     C*
045500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
045600030822     C                   CLEAR                   FIVAB000
045700040119     C                   CLEAR                   FIVAT000
045800030822     C                   CLEAR                   SkSplitCSV
045900030822     C                   EVAL      i = 1
046000030822     C                   EVAL      posDa = *zeros
046100030822     C                   EVAL      posA  = *zeros
046200030822     C*
046300030822     C* Reimposto i valori di default
046400030822     C                   EXSR      DEFCAM
046500030822     C*
046600010330     C                   ENDSR
046700010330     C*----------------------------------------------------*
046800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
046900010330     C*----------------------------------------------------*
047000010330     C     DEFCAM        BEGSR
047100040119     C*
047200040119     C* Imposto i valori d default a programma dei campi fondamentali
047300040706     C                   EVAL      VABCCM = 0052805
047400040706     C                   EVAL      VATCCM = 0052805
047500040706     C                   EVAL      VABLNP = 005
047600040706     C                   EVAL      VATLNP = 005
047700040706     C                   EVAL      VABCTR = 000
047800040119     C                   EVAL      VABCTM = '7Q'
047900040119     C                   EVAL      VABCBO = '1'
048000040119     C                   EVAL      VATTRC = 'E'
048100030822     C*
048200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
048300030715     C* e delimitatore testo.
048400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048700041201     C*
048800041201     C* Valorizzo a *off (ovvero no stampa) il flag che indica se mandare la stampa in filiale
048900041201     C                   SETOFF                                       50
049000030822     C*
049100041201     C* Reperisco il flag che mi indica il tipo soluzoine: A = DiskA altrimenti DiskC
049200041201     C                   IF        %subst(vlrppt:1:1) = 'A'
049300041201     C                   SETON                                        51
049400041201     C                   EVAL      VABCTM = *blanks
049500030715     C                   ELSE
049600041201     C                   SETOFF                                       51
049700030715     C                   ENDIF
049800030822     C*
049900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050000030822     C                   EVAL      posDaDft = 1
050100030822     C                   EVAL      posADft  = 0
050200030822     C                   EVAL      wGiroDft = 0
050300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050400030822     C                             posDaDft > 0
050500030822     C*
050600030822     C* Gestisco il 1° giro
050700030822     C                   IF        wGiroDft = 0
050800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
050900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051000030822     C* Incremento il contatore dei "giri"
051100030822     C                   EVAL      wGiroDft = 1
051200030822     C                   ELSE
051300030822     C                   EVAL      posDaDft = posADft
051400030822     C                   ENDIF
051500030822     C* Eseguo lo scan x trovare la fine del campo corrente
051600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
051700030822     C*
051800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
051900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052000030822     C                   IF        posDaDft > 0 AND
052100030822     C                             posADft  > 0
052200030822     C* NCL
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'NCL'
052700030822     C                   EVAL      PiStr=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   EXSR      CHKNUM
053100030822     C                   IF        PiInt=*on
053200030822     C                   Z-ADD     PiVal         VABNCL
053300030822     C                   ENDIF
053400030822     C                   ENDIF
053500030822     C* CCM
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'CCM'
054000030822     C                   EVAL      PiStr=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   EXSR      CHKNUM
054400030822     C                   IF        PiInt=*on
054500030822     C                   Z-ADD     PiVal         VABCCM
054600040119     C                   Z-ADD     PiVal         VATCCM
054700030822     C                   ENDIF
054800030822     C                   ENDIF
054900030822     C* LNP
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'LNP'
055400030822     C                   EVAL      PiStr=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   EXSR      CHKNUM
055800030822     C                   IF        PiInt=*on
055900030822     C                   Z-ADD     PiVal         VABLNP
056000040119     C                   Z-ADD     PiVal         VATLNP
056100030822     C                   ENDIF
056200030822     C                   ENDIF
056300030822     C* NRS
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'NRS'
056800030822     C                   EVAL      PiStr=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   EXSR      CHKNUM
057200030822     C                   IF        PiInt=*on
057300030822     C                   Z-ADD     PiVal         VABNRS
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* CTR
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'CTR'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABCTR
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* PKB
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'PKB'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiNum=*on
059900030822     C                   Z-ADD     PiVal         VABPKB
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* VLB
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'VLB'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiNum=*on
061200030822     C                   Z-ADD     PiVal         VABVLB
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* QFT
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'QFT'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiNum=*on
062500030822     C                   Z-ADD     PiVal         VABQFT
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* CBO
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'CBO'
063300030822     C                   EVAL      VABCBO=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* TSP
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'TSP'
064200030822     C                   EVAL      VABTSP=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* VAS
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'VAS'
065100030822     C                   EVAL      VABVAS=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* VCA
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'VCA'
066000030822     C                   EVAL      VABVCA=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* GCA
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'GCA'
066900030822     C                   EVAL      VABGCA=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* CTM
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'CTM'
067800030822     C                   EVAL      VABCTM=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* FFD
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'FFD'
068700030822     C                   EVAL      VABFFD=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* VAD
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'VAD'
069600030822     C                   EVAL      VABVAD=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* GMA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'GMA'
070500030822     C                   EVAL      VABGMA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* GGA
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'GGA'
071400030822     C                   EVAL      VABGGA=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* GVA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'GVA'
072300030822     C                   EVAL      VABGVA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* TC1
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'TC1'
073200030822     C                   EVAL      VABTC1=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* TC2
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'TC2'
074100030822     C                   EVAL      VABTC2=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* ...
074600030822     C                   ENDIF
074700030822     C                   ENDDO
074800020204     C*
074900010330     C                   ENDSR
075000010607     C*----------------------------------------------------*
075100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075200010607     C*----------------------------------------------------*
075300010607     C     IMPVAB        BEGSR
075400010607     C*
075500010607     C                   SETOFF                                       3132
075600030515     C*
075700010607     C                   MOVEL     datcor        VABAAS
075800040119     C                   MOVEL     datcor        VATAAS
075900010607     C                   MOVE      datcor        VABMGS
076000010607     C*
076100030715     C**********
076200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076300030715     C**********
076400030715     C                   IF        wGiro = *zeros
076500040706     C*
076600040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
076700040706     C                   EVAL      depvinDTA = vinDTA
076800040113     C*
076900040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
077000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077100040113     C                                      %trim(intcol(3))
077200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077300031201     C     minu:maiu     XLATE     vindta        vindta
077400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077500030715     C                   DOW       posDa <= %len(%trim(vindta))
077600030715     C*
077700030715     C* Gestisco il 1° campo
077800030715     C                   IF        i = 1
077900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078000030715     C                   EVAL      posDa = 1
078100030715     C* Eseguo lo scan x trovare la fine del primo campo
078200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078300030715     C* A questo punto "estraggo" il campo corrente
078400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078500030715     C                                                    (posA-posDa))
078600030715     C* X i campi successivi al 1°
078700030715     C                   ELSE
078800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078900030715     C                   EVAL      posDa = posA + 1
079000030715     C* Eseguo lo scan x trovare la fine del campo corrente
079100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079200030715     C* Gestisco l'ultimo campo
079300030715     C                   IF        posA = *zeros
079400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
079500030715     C* A questo punto "estraggo" il campo corrente
079600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079700030715     C                                                    (posA-posDa))
079800030715     C                   LEAVE
079900030715     C                   ELSE
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C                   ENDIF
080400030715     C                   ENDIF
080500030715     C* Incremento il contatore d campo
080600030715     C                   EVAL      i = i +1
080700030715     C                   ENDDO
080800030715     C*
080900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081000030715     C* delimitazione testo
081100030715     C                   EVAL      i = 1
081200030715     C                   DOW       i < %elem(SkSplitFLD)
081300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081500030715     C                   EVAL      i = i + 1
081600030715     C                   ENDDO
081700030715     C                   EVAL      wGiro = 1
081800040706     C* Ripristino la 1° riga originale
081900040706     C                   EVAL      vinDTA = depvinDTA
082000030715     C                   ELSE
082100030715     C                   EVAL      wGiro = 2
082200031204     C**********
082300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082400031204     C* d separatore campo
082500031204     C**********
082600031204     C                   EVAL      vindta = %trim(vindta)
082700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
082800031204     C                   EVAL      vindta = %subst(vindta:2)
082900031204     C                   ENDDO
083000031204     C*
083100031204     C                   Z-ADD     *zeros        lunghInput        4 0
083200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083600031204     C                   ENDDO
083700030715     C**********
083800030715     C* Effettuo lo split del campo dati d input
083900030715     C**********
084000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084100030715     C                   DOW       posDa <= %len(%trim(vindta))
084200030715     C*
084300030715     C* Gestisco il 1° campo
084400030715     C                   IF        i = 1
084500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084600030715     C                   EVAL      posDa = 1
084700030715     C* Eseguo lo scan x trovare la fine del primo campo
084800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C* X i campi successivi al 1°
085300030715     C                   ELSE
085400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085500030715     C                   EVAL      posDa = posA + 1
085600031204     C* Verifico che nn vi sia il campo nn valorizzato
085700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
085800031204     C* Se campo nn valorizzato skippo al prossimo
085900031204     C                   EVAL      SkSplitCSV(i) = *blanks
086000031204     C                   EVAL      posA  = posA + 1
086100031204     C                   ELSE
086200030715     C* Eseguo lo scan x trovare la fine del campo corrente
086300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086400030715     C* Gestisco l'ultimo campo
086500030715     C                   IF        posA = *zeros
086600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086700030715     C* A questo punto "estraggo" il campo corrente
086800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086900030715     C                                                    (posA-posDa))
087000030715     C                   LEAVE
087100030715     C                   ELSE
087200030715     C* A questo punto "estraggo" il campo corrente
087300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087400030715     C                                                    (posA-posDa))
087500030715     C                   ENDIF
087600031204     C                   ENDIF
087700030715     C                   ENDIF
087800030715     C* Incremento il contatore d campo
087900030715     C                   EVAL      i = i +1
088000030715     C                   ENDDO
088100030715     C*
088200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088300030715     C* delimitazione testo
088400030715     C                   EVAL      i = 1
088500030715     C                   DOW       i < %elem(SkSplitCSV)
088600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
088700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
088800030715     C                   EVAL      i = i + 1
088900030715     C                   ENDDO
089000030715     C*
089100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089200030715     C                   EVAL      i = 1
089300030715     C                   DOW       i < %elem(SkSplitFLD)
089400030715     C*
089500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
089700030715     C                             SkSplitCSV(i) <> *blanks
089800030721     C*** ATB
089900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090000030721     C                   EVAL      VABATB = SkSplitCSV(i)
090100030721     C                   ENDIF
090200030715     C*** RSD
090300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
090700030715     C* ==
090800030715     C                   ENDIF
090900030721     C*** RD2
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030715     C*** IND
091400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091500030715     C                   EVAL      VABIND = SkSplitCSV(i)
091600030715     C                   ENDIF
091700030715     C*** LOD
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030715     C*** PRD
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030721     C*** NZD
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030715     C*** RMA
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030715     C*** NOT
093400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093600030715     C                   ENDIF
093700030715     C*** NT2
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030715     C*** VCA
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** VAS
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** FFD
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030721     C*** GC1
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** GC2
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
095900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** TSP
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** NAS
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
096700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** TIC
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** GCA
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
097500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** XCO
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
097900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** CTM
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
098300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** TCR
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
098700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** CTS
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** FTM
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
099500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** VAD
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
099900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** GMA
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
100300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** GGA
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
100700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** GVA
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** TC1
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
101500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** TC2
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
101900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** SCL
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
102300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** RMO
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
102700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** NMO
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103200030721     C                   ENDIF
103300040119     C*** CBO
103400040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103500040706     C                   EVAL      VABCBO = SkSplitCSV(i)
103600040119     C                   ENDIF
103700030721     C*
103800040706     C* Reperisco quindi i campi numerici...
103900040706     C*** NSP
104000040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104100040706     C                   EVAL      PiStr=SkSplitCSV(i)
104200040706     C                   EXSR      CHKNUM
104300040706     C                   IF        PiInt=*on
104400040706     C                   Z-ADD     PiVal         VABNSP
104500040706     C                   ELSE
104600040706     C                   SETON                                        31
104700040706     C                   EVAL      VABNSP = *zeros
104800040706     C                   EVAL      vinmsg = %trimr(vinmsg)
104900040706     C                             + ' ' + 'VABNSP'
105000040706     C                   ENDIF
105100040706     C                   ENDIF
105200040706     C*** RMN
105300040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105400040706     C                   EVAL      PiStr=SkSplitCSV(i)
105500040706     C                   EXSR      CHKNUM
105600040706     C                   IF        PiInt=*on
105700040706     C                   Z-ADD     PiVal         VABRMN
105800040706     C                   ELSE
105900040706     C                   SETON                                        32
106000040706     C                   EVAL      VABRMN = 1
106100040706     C                   EVAL      vinmsg = %trimr(vinmsg)
106200040706     C                             + ' ' + 'VABRMN'
106300040706     C                   ENDIF
106400040706     C                   ENDIF
106500030721     C*** CAD
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiInt=*on
107000030721     C                   Z-ADD     PiVal         Num5_0
107100030721     C                   MOVEL(p)  Num5_0        VABCAD
107200030721     C                   ELSE
107300030721     C                   SETON                                        32
107400030721     C                   EVAL      VABCAD = *zeros
107500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107600030721     C                             + ' ' + 'VABCAD'
107700030721     C                   ENDIF
107800030721     C                   ENDIF
107900030721     C*** NCL
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108100030721     C                   EVAL      PiStr=SkSplitCSV(i)
108200030721     C                   EXSR      CHKNUM
108300030721     C                   IF        PiInt=*on
108400030721     C                   Z-ADD     PiVal         VABNCL
108500030721     C                   ELSE
108600030721     C                   SETON                                        32
108700030721     C                   EVAL      VABNCL = *zeros
108800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108900040706     C                             + ' ' + 'VABNCL'
109000030721     C                   ENDIF
109100030721     C                   ENDIF
109200030721     C*** PKB
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109400030721     C                   EVAL      PiStr=SkSplitCSV(i)
109500030721     C                   EXSR      CHKNUM
109600030721     C                   IF        PiNum=*on
109700030721     C                   Z-ADD     PiVal         VABPKB
109800030721     C                   ELSE
109900030721     C                   SETON                                        32
110000030721     C                   EVAL      VABPKB = *zeros
110100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110200030721     C                             + ' ' + 'VABPKB'
110300030721     C                   ENDIF
110400030721     C                   ENDIF
110500030721     C*** CAS
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiNum=*on
111000030721     C                   Z-ADD     PiVal         VABCAS
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      VABCAS = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500030721     C                             + ' ' + 'VABCAS'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** CCM
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on      AND
112300030721     C                             PiVal<=9999999 AND
112400030721     C                             PiVal>*zeros
112500030721     C                   Z-ADD     PiVal         VABCCM
112600040119     C                   Z-ADD     PiVal         VATCCM
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABCCM = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040119     C                             + ' ' + 'VABCCM VATCCM'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** LNP
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on    AND
113900030721     C                             PiVal<=999   AND
114000030721     C                             PiVal>*zeros
114100030721     C                   Z-ADD     PiVal         VABLNP
114200040119     C                   Z-ADD     PiVal         VATLNP
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABLNP = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700040119     C                             + ' ' + 'VABLNP VATLNP'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** NRS
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on AND
115500030721     C                             PiVal<=99
115600030721     C                   Z-ADD     PiVal         VABNRS
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABNRS'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** LNA
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiInt=*on    AND
116800030721     C                             PiVal<=999   AND
116900030721     C                             PiVal>*zeros
117000030721     C                   Z-ADD     PiVal         VABLNA
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      VABLNA = *zeros
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500030721     C                             + ' ' + 'VABLNA'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** CTR
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiInt=*on  AND
118300030721     C                             PiVal<=999
118400030721     C                   Z-ADD     PiVal         VABCTR
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABCTR'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** IAS
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiNum=*on
119600030721     C                   Z-ADD     PiVal         VABIAS
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABIAS'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** VLB
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiNum=*on
120800030721     C                   Z-ADD     PiVal         VABVLB
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABVLB'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** QFT
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiNum=*on
122000030721     C                   Z-ADD     PiVal         VABQFT
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABQFT'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** NCD
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on      AND
123200030721     C                             PiVal<=9999999
123300030721     C                   Z-ADD     PiVal         VABNCD
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABNCD'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** NCA
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiInt=*on      AND
124500030721     C                             PiVal<=9999999
124600030721     C                   Z-ADD     PiVal         VABNCA
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABNCA'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** ZNC
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on AND
125800030721     C                             PiVal<=99
125900030721     C                   Z-ADD     PiVal         VABZNC
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABZNC'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** DCR
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on       AND
127100030721     C                             PiVal<=99999999
127200030721     C                   Z-ADD     PiVal         VABDCR
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABDCR'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** HCR
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on   AND
128400030721     C                             PiVal<=9999
128500030721     C                   Z-ADD     PiVal         VABHCR
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABHCR'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** VMD
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiNum=*on
129700030721     C                   Z-ADD     PiVal         VABVMD
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABVMD'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** ANT
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on        AND
130900030721     C                             PiVal<=999999999
131000030721     C                   Z-ADD     PiVal         VABANT
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABANT'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** CMO
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on
132200030721     C                   Z-ADD     PiVal         Num5_0
132300030721     C                   MOVEL(p)  Num5_0        VABCMO
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      VABCMO = *zeros
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800030721     C                             + ' ' + 'VABCMO'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100040706     C*
133200040706     C* Gestisco i campi "personalizzati"
133300040706     C*** COLLODA
133400041201     C                   IF        %trim(SkSplitFLD(i))='COLLODA' AND *IN51='0'
133500040706     C                   EVAL      PiStr=SkSplitCSV(i)
133600040706     C                   EXSR      CHKNUM
133700040706     C                   IF        PiInt=*on
133800040706     C                   Z-ADD     PiVal         wBarcodeDa
133900040706     C                   ELSE
134000040706     C                   SETON                                        32
134100040706     C                   EVAL      wBarcodeDa = *zeros
134200040706     C                   EVAL      vinmsg = %trimr(vinmsg)
134300040706     C                             + ' ' + 'VATNOT'
134400040706     C                   ENDIF
134500040706     C                   ENDIF
134600040706     C*** COLLOA
134700041201     C                   IF        %trim(SkSplitFLD(i))='COLLOA' AND *IN51='0'
134800040706     C                   EVAL      PiStr=SkSplitCSV(i)
134900040706     C                   EXSR      CHKNUM
135000040706     C                   IF        PiInt=*on
135100040706     C                   Z-ADD     PiVal         wBarcodeA
135200040706     C                   ELSE
135300040706     C                   SETON                                        32
135400040706     C                   EVAL      wBarcodeA = *zeros
135500040706     C                   EVAL      vinmsg = %trimr(vinmsg)
135600040706     C                             + ' ' + 'VATNOT'
135700040706     C                   ENDIF
135800040706     C                   ENDIF
135900030715     C*
136000030715     C                   ENDIF
136100030715     C                   EVAL      i = i + 1
136200030715     C                   ENDDO
136300040706     C*
136400040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
136500040706     C* Reperisco la provincia dal CAP e dalla località
136600040706     C                   IF        VABCAD <> *blanks
136700040706     C                   CLEAR                   TISI95DS
136800040706     C                   EVAL      I95TCN = '3'
136900040706     C                   Z-ADD     datcor        I95DAT
137000040706     C                   EVAL      I95CAP = VABCAD
137100040706     C                   EVAL      I95LOC = VABLOD
137200040706     C                   CALL      'TISI95R'
137300040706     C                   PARM                    TISI95DS
137400040706     C                   EVAL      VABPRD = O95PRV
137500040706     C                   ENDIF
137600040706     C*
137700040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
137800040706     C* d valorizzarli sempre entrambi
137900040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
138000040706     C                   Z-ADD     VABNSP        VABRMN
138100040706     C                   ENDIF
138200040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
138300040706     C                   MOVEL     VABRMN        VABRMA
138400040706     C                   ENDIF
138500040706     C*
138600040706     C* Eseguo considerazioni sul range dei barcode
138700041201     C                   IF        *IN51 = *off
138800040706     C                   IF        wBarcodeDa = *zeros OR
138900040706     C                             wBarcodeA  = *zeros OR
139000040706     C                             wBarcodeA  < wBarcodeDa
139100040706     C                   SETON                                        31
139200040706     C                   EVAL      vinmsg = %trimr(vinmsg)
139300040706     C                             + ' ' + 'VATNOT'
139400040706     C                   ENDIF
139500041201     C                   ENDIF
139600040113     C*
139700040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
139800040113     C                   IF        VABCAS > *zeros
139900040119     C                   EVAL      VABVCA = 'EUR'
140000040119     C                   IF        VABCBO = '1'
140100040113     C                   EVAL      VABCBO = '4'
140200040119     C                   ENDIF
140300040119     C                   IF        VABCBO = '2'
140400040119     C                   EVAL      VABCBO = '6'
140500040119     C                   ENDIF
140600040113     C                   ENDIF
140700020204     C*
140800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
140900020204     C                   EXSR      CHKIMPDIV
141000030715     C                   ENDIF
141100020204     C*
141200010607     C                   ENDSR
141300010607     C*----------------------------------------------------*
141400010601
141500020204
141600020204     C*----------------------------------------------------*
141700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
141800020204     C*----------------------------------------------------*
141900020204     C     CHKIMPDIV     BEGSR
142000020204     C*
142100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
142200020204     C                   Z-ADD     *zeros        wrkDec            9 9
142300020204     C*
142400020204     C* Come prima cosa effettuo considerazioni sulla divisa
142500020204     C                   IF        vabIAS > *zeros
142600020204     C                   IF        vabVAS <> 'EUR'
142700020204     C                   EVAL      vabVAS =  'ITL'
142800020204     C                   ENDIF
142900020204     C                   ENDIF
143000020204     C*
143100020204     C                   IF        vabCAS > *zeros
143200020204     C                   IF        vabVCA <> 'EUR'
143300020204     C                   EVAL      vabVCA =  'ITL'
143400020204     C                   ENDIF
143500020204     C                   ENDIF
143600020204     C*
143700020204     C                   IF        vabVMD > *zeros
143800020204     C                   IF        vabVAD <> 'EUR'
143900020204     C                   EVAL      vabVAD =  'ITL'
144000020204     C                   ENDIF
144100020204     C                   ENDIF
144200020204     C*
144300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
144400020204     C                   Z-ADD     vabIAS        wrkDec
144500020204     C                   IF        wrkDec > *zeros
144600020204     C                   IF        vabVAS = 'ITL'
144700020204     C                   EVAL      vabIAS = *zeros
144800020204     C                   ENDIF
144900020204     C                   ENDIF
145000020204     C*
145100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
145200020204     C                   Z-ADD     vabCAS        wrkDec
145300020204     C                   IF        wrkDec > *zeros
145400020204     C                   IF        vabVCA = 'ITL'
145500020204     C                   EVAL      vabCAS = *zeros
145600020204     C                   ENDIF
145700020204     C                   ENDIF
145800020204     C*
145900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
146000020204     C                   Z-ADD     vabVMD        wrkDec
146100020204     C                   IF        wrkDec > *zeros
146200020204     C                   IF        vabVAD = 'ITL'
146300020204     C                   EVAL      vabVMD = *zeros
146400020204     C                   ENDIF
146500020204     C                   ENDIF
146600020204     C*
146700020204     C                   ENDSR
146800020204     C***
146900020204
147000010330
147100010330
147200010330     C*----------------------------------------------------*
147300010330     C*  CONTROLLO NUMERICITA' CAMPI
147400010330     C*----------------------------------------------------*
147500010330     C     CHKNUM        BEGSR
147600010330     C*
147700010606     C                   IF        PiDecChr = *blanks
147800030715     C                   EVAL      PiDecChr = CharNUM
147900010606     C                   ENDIF
148000010606     C*
148100010606     C                   CALL(e)   'ISNUMERIC'
148200010330     C                   PARM                    PiStr            30
148300010606     C                   PARM                    PiDecChr          1
148400010330     C                   PARM      *ZEROS        PiVal            30 9
148500010330     C                   PARM      '0'           PiInt             1
148600010330     C                   PARM      '0'           PiNum             1
148700010330     C                   IF        %error
148800010606     C                   EVAL      PiNum=*off
148900010330     C                   ENDIF
149000010330     C*
149100010330     C                   ENDSR
149200010330     C***
149300010330
149400010601
149500010601
149600010601
149700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149800010601     C     repfil        BEGSR
149900010601     C*
150000010601     C                   if        invfil = *zeros and
150100010601     C                             depfil > *zeros and
150200010629     C                             (vinflg = *blanks or
150300010629     C                              vinflg = *zeros)
150400010601     C
150500010601     C                   eval      invfil = depfil
150600010601     C                   endif
150700010601     C*
150800010601     C                   if        depfil <> invfil and
150900010601     C                             invfil > *zeros
151000010601     C                   eval      flgMulti = '1'
151100010601     C                   if        vinflg = *blanks
151200010601     C                   add       1             cntNonEl
151300010601     C                   endif
151400010601     C                   endif
151500010601     C*
151600010601     C                   if        vinflg = '2'
151700010601     C                   eval      flgStato = '2'
151800010601     C                   endif
151900010601     C*
152000010601     C                   ENDSR
152100010601     C***
152200010601
152300010601
152400010601
152500010330
152600040119      /TITLE Invio dei dati al punto operativo.
152700040119     C     invio         BEGSR
152800040119     C*
152900040119     C* 1° invio FIVAT
153000040119     C                   reset                   dscmz
153100040119     C                   move      vlrpoi        cmzdst
153200040119     C                   eval      cmzfld = 'FIVATWWR'
153300040119     C                   eval      cmzmbd = vlrhdl
153400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153500040119     C***                if        prmfir = *blanks
153600040119     C                   eval      cmzfla = 'FIVAT00F'
153700040119     C                   eval      cmzmba = 'FIVAT00F'
153800040119     C***                else
153900040119     C***                eval      cmzfla = prmfir
154000040119     C***                eval      cmzmba = prmfir
154100040119     C***                endif
154200040119     C                   eval      cmznrr = *zeros
154300040119     C                   move      §ctrokvt      cmznrr
154400040119     C                   eval      cmzlba = vlrfl1
154500040119     C                   call(e)   'TIS711C'
154600040119     C                   parm                    dscmz
154700040119     C                   parm      *blanks       esito
154800040119     C                   if        %error
154900040119     C                             or cmzerr = '1'
155000040119     C                             or esito  = '1'
155100040119     C                   eval      wrkesito = '3'
155200040119     C                   else
155300040119     C*
155400040119     C* 2° invio FIVAB
155500040119     C                   reset                   dscmz
155600040119     C                   move      vlrpoi        cmzdst
155700040119     C                   eval      cmzfld = vlrfou
155800040119     C                   eval      cmzmbd = vlrhdl
155900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
156000040119     C***                if        prmfir = *blanks
156100040119     C                   eval      cmzfla = 'FIVAB00F'
156200040119     C                   eval      cmzmba = 'FIVAB00F'
156300040119     C***                else
156400040119     C***                eval      cmzfla = prmfir
156500040119     C***                eval      cmzmba = prmfir
156600040119     C***                endif
156700040119     C                   eval      cmznrr = *zeros
156800040119     C                   move      §ctrokvb      cmznrr
156900040119     C                   eval      cmzlba = vlrfl1
157000040119     C                   call(e)   'TIS711C'
157100040119     C                   parm                    dscmz
157200040119     C                   parm      *blanks       esito
157300040119     C                   if        %error
157400040119     C                             or cmzerr = '1'
157500040119     C                             or esito  = '1'
157600040119     C                   eval      wrkesito = '3'
157700040119     C                   endif
157800040119     C                   endif
157900040119     C*
158000040119     C                   ENDSR
158100040119     C***
158200010601
158300010601
158400010601
158500010601
158600010601      /TITLE Invio dei dati al punto operativo.
158700010601     C     opeini        BEGSR
158800010601     C*
158900010601     C* Inizializzo flag e contatori operativi
159000010601     C                   movel     '0'           flgGiro           1
159100010601     C                   movel     '0'           flgMulti          1
159200010601     C                   movel     '1'           flgStato          1
159300010615     C                   movel     '0'           flgOk             1
159400010601     C                   z-add     *zeros        cntNonEl         10 0
159500010601     C                   z-add     *zeros        depfil            3 0
159600010601     C                   z-add     *zeros        invfil            3 0
159700010601     C*
159800010601     C                   ENDSR
159900010601     C***
160000010601
160100010601
160200050421
160300050421
160400070411
160500070411     C     *pssr         BEGSR
160600070411     C*
160700070411     C                   if        %open(tivin00r)
160800070411     C                   close     tivin00r
160900070411     C                   endif
161000070411     C                   if        %open(fivabwwr)
161100070411     C                   close     fivabwwr
161200070411     C                   endif
161300070411     C                   if        %open(fivatwwr)
161400070411     C                   close     fivatwwr
161500070411     C                   endif
161600070411     C*
161700070411     C* Effettuo la chiamata al CLLE preposto
161800070411     C                   call(e)   'TITVVTC'
161900070411     C                   parm                    parccm
162000070411     C                   parm                    parmbr
162100070411     C                   parm      '2'           paropz
162200070411     C*
162300070411     C                   eval      wrkesito = '2'
162400070411     C*
162500070411     C                   seton                                        LR
162600070411     C*
162700070411     C                   ENDSR     '*CANCL'
162800070411     C***
162900050421
163000010601
163100010330
163200010330
163300000613     C     *inzsr        BEGSR
163400990910     C*
163500990910     C     *entry        plist
163600990920     C                   parm                    tivlrds
163700990921     C                   parm      wrkesito      esito
163800000724     C                   parm                    prmlit
163900000710     C                   parm                    prmfir
164000010330     C*
164100010330     C* CALCOLA LA DATA CORRENTE
164200010330     C                   time                    wn14             14 0
164300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
164400010330     C                   z-add     wn8           g08dat
164500010330     C                   z-add     *zeros        g08inv
164600010330     C                   movel     '0'           g08err
164700010330     C                   call      'XSRDA8'
164800010330     C                   parm                    wlbda8
164900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
165000000613     C*
165100000613     C                   ENDSR
165200000613     C***
165300990908
165400040706     Otitvd2p   E            riepilogo         2
165500990915     O                                              'Upload via Internet'
165600990915     O                                           +1 'Traduzione TIVIN00R -'
165700021025     O                                           +1 'FIVABWWR'
165800990915     O                                           +1 'Testate bolle'
165900990915     O          E            riepilogo   2
166000990915     O                       wrkdata
166100990915     O                       wrkora              +1
166200990915     O                       procname            +1
166300990915     O          E            riepilogo   2
166400990915     O                                              'Cliente..................:'
166500010605     O                       VABCCM        z     +1
166600990915     O          E            riepilogo   2
166700990920     O                                              'Riferimento Strategi.....:'
166800990920     O                       vlrhdl              +1
166900990915     O          E            riepilogo   2
167000990915     O                                              'Giusti...................:'
167100040119     O                       §CTROKVB      2   +  1
167200990915     O          E            riepilogo   2
167300990915     O                                              'Sbagliati e corretti.....:'
167400971022     O                       §CTRMO        2   +  1
167500990915     O          E            riepilogo   2
167600990915     O                                              'Sbagliati e scartati.....:'
167700971022     O                       §CTRNO        2   +  1
167800000613
167900040706     Otitvd2ps  E            testdett          2
168000000613     O                                              'Upload via Internet'
168100000613     O                                           +1 'Traduzione TIVIN00R -'
168200021025     O                                           +1 'FIVABWWR'
168300000613     O                                           +1 'Testate bolle'
168400000616     O          E            testdett    3
168500000613     O                                           +2 'N° rec'
168600000613     O                                           +3 'Anteprima contenuto'
168700000616     O          E            rigadett    2
168800000613     O                       rrnum               +2
168900000621     O                       recko               +3
169000000616     O          E            findett     2
169100000613     O                       wrkdata
169200000613     O                       wrkora              +1
169300000613     O                       procname            +1
169400000616     O          E            findett     2
169500000613     O                                              'Cliente..................:'
169600010605     O                       VABCCM        z     +1
169700000616     O          E            findett     2
169800000613     O                                              'Riferimento Strategi.....:'
169900000613     O                       vlrhdl              +1
170000000616     O          E            findett     2
170100000613     O                                              'Giusti...................:'
170200040119     O                       §CTROKVB      2   +  1
170300000616     O          E            findett     2
170400000613     O                                              'Sbagliati e corretti.....:'
170500000613     O                       §CTRMO        2   +  1
170600000616     O          E            findett     2
170700000613     O                                              'Sbagliati e scartati.....:'
170800000613     O                       §CTRNO        2   +  1
170900000613** CMD - COMANDI CL
171000040706OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVD2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
171100040706OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVD2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
171200040706DLTOVR FILE(TITVD2P TITVD2PS) LVL(*)
171300010606
171400000613
171500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
171600040713VABNSP;NUMCLI;VABRSD;VABIND;VABCAD;VABLOD;VABNCL;
171700040706VABPKB;COLLODA;COLLOA
171800040113
