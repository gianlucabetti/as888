000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800061106     Ftitvv7p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100061106     Ftitvv7ps  O    f  198        PRINTER usropn
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
004500061106     D wVABRAGSOC      s            100    INZ(*blanks)
004600061106     D wVATNOT_A       s                   LIKE(vatnot) INZ(*blanks)
004700061106     D wVATNOT_B       s                   LIKE(vatnot) INZ(*blanks)
004800061106     D wVABNOTE        s             70    INZ(*blanks)
004900061106     D wVABRMA_1       s                   LIKE(vabrma) INZ(*blanks)
005000061106     D wVABRMA_2       s                   LIKE(vabrma) INZ(*blanks)
005100030715     D*------------------
005200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005300030715     D*------------------
005400030715     D SkSplitFLD      S             10    DIM(100)
005500030715     D SkSplitCSV      S            256    DIM(100)
005600030715     D CharCSV         S              1
005700030715     D CharTXT         S              1
005800030715     D CharNUM         S              1
005900030715     D posDa           S              3  0 INZ(*zeros)
006000030715     D posA            S              3  0 INZ(*zeros)
006100030715     D i               s              3  0 INZ(1)
006200030715     D wGiro           s              1  0 INZ(*zeros)
006300030822     D*------------------
006400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006500030822     D*------------------
006600030822     D posDaDft        S              3  0 INZ(*zeros)
006700030822     D posADft         S              3  0 INZ(*zeros)
006800030822     D j               s              3  0 INZ(1)
006900030822     D wGiroDft        s              1  0 INZ(*zeros)
007000010330     D*------------------
007100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007200010330     D*------------------
007300010330     D WLBDA8          DS                  INZ
007400010330     D  G08DAT                 1      8  0
007500010330     D  G08INV                 9     16  0
007600010330     D  G08ERR                17     17
007700010330     D  G08TGI                18     22  0
007800031201     D*------------------
007900031201     D* Costanti
008000031201     D*------------------
008100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008300010330
008400010330
008500010330
008600990915     C                   time                    wrkdata
008700990915     C                   time                    wrkora
008800000913     C                   reset                   rrnum
008900990921     C                   reset                   esito
009000990921     C                   reset                   wrkesito
009100010601     C*
009200010601     C                   exsr      opeini
009300010605     C                   exsr      rwvab
009400010601     C*
009500010601     C                   seton                                        lr
009600010601
009700010601
009800010601
009900010601     C*--------------------------------------------------------
010000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010100010601     C*--------------------------------------------------------
010200010601     C     PREELA        BEGSR
010300010601     C*
010400010601     C* SE OCCORRE SPEDIRE IN FILIALE
010500010601     C                   if        invfil <> *zeros and
010600010601     C                             flgGiro = '0'
010700010601     C*
010800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010900010601     C                   eval      flgGiro = '1'
011000000724     C*
011100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011300010601     C     invfil        chain     azorg01l
011400000724     C                   if        %found
011500000724     C                   movel(p)  CMD(1)        depcmd
011600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011700000724     C*
011800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000724     C                   PARM                    depcmd
012200000724     C                   PARM                    LENGH
012300000724     C*
012400000724     C                   endif
012500000616     C*
012600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012700000616     C                   movel(p)  CMD(2)        depcmd
012800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012900000616     C*
013000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    depcmd
013400000616     C                   PARM                    LENGH
013500000613     C*
013600061106     C                   if        not %open(titvv7ps)
013700061106     C                   open      titvv7ps
013800030715     C                   except    testdett
013900000613     C                   endif
014000010601     C*
014100010601     C                   endif
014200010601     C*
014300010601     C                   ENDSR
014400010601     C***
014500010601
014600010601
014700010601
014800010601
014900010601     C*--------------------------------------------------------
015000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015100010601     C*--------------------------------------------------------
015200010601     C     ENDELA        BEGSR
015300990915     C*
015400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015500040119     C*
015600040119     C* Effettuo la chiamata al CLLE preposto
015700040119     C                   call(e)   'TITVVTC'
015800040119     C                   parm                    parccm
015900040119     C                   parm                    parmbr
016000040119     C                   parm      '2'           paropz
016100050201     C*
016200050201     C* Effettuo lancio TISI95 solo x chiusura
016300050201     C                   CLEAR                   TISI95DS
016400050201     C                   EVAL      I95TLA = 'C'
016500050201     C                   CALL      'TISI95R'
016600050201     C                   PARM                    TISI95DS
016700000613     C*
016800061106     C                   if        %open(titvv7ps)
016900000616     C                   except    findett
017000061106     C                   close     titvv7ps
017100000613     C                   endif
017200000616     C*
017300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017400011113     C                   Z-ADD     150           LENGH            15 5
017500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017600000616     C                   PARM                    CMD(3)
017700000616     C                   PARM                    LENGH
017800000616     C*
017900010601     C                   ENDSR
018000010601     C***
018100010601
018200010601
018300010601
018400000613
018500000613     C*--------------------------------------------------------
018600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018700000613     C*--------------------------------------------------------
018800000621     C     STPR          BEGSR
018900000613     C*
019000061106     C                   if        not %open(titvv7p)
019100061106     C   50              open      titvv7p
019200990915     C                   endif
019300990915     C*
019400030715     C   50              except    riepilogo
019500990915     C*
019600061106     C                   if        %open(titvv7p)
019700061106     C   50              close     titvv7p
019800990914     C                   endif
019900000613     C*
020000000613     C                   ENDSR
020100000613     C***
020200000613
020300010601
020400010601
020500010330     C*--------------------------------------------------------
020600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020700010330     C*--------------------------------------------------------
020800010605     C     RWVAB         BEGSR
020900010330     C*
021000010330     C                   if        not %open(tivin00r)
021100010330     C                   open      tivin00r
021200010330     C                   endif
021300021025     C                   if        not %open(fivabwwr)
021400021025     C                   open      fivabwwr
021500010330     C                   endif
021600040119     C*
021700040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021800040119     C                   exsr      prevat
021900040119     C*
022000040119     C                   if        chkcall = '0'
022100040119     C*
022200040119     C                   if        not %open(fivatwwr)
022300040119     C                   open      fivatwwr
022400040119     C                   endif
022500010330     C*
022600040119     C                   clear                   §CTROKVB          7 0
022700040119     C                   clear                   §CTROKVT          7 0
022800040119     C                   clear                   §CTRMO            7 0
022900040119     C                   clear                   §CTRNO            7 0
023000040706     C*
023100040706     C  N70              do        *HIVAL
023200010330     C*
023300040706     C                   read      tivin00r                               70
023400010618     C*
023500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023600010618     C                   if        vinflg = '1'
023700010618     C                   eval      flgOk = '1'
023800010618     C                   endif
023900010618     C*
024000040316     C                   if        (vindta > *blanks AND
024100040316     C                              %trim(vindta) <> *all';') OR
024200040316     C                             wGiro = *zeros
024300010330     C                   add       1             rrnum
024400010330     C*
024500010601     C                   if        *in70 = *off and
024600010330     C                             (vinflg = *blanks
024700010330     C                              or vinflg = '0'
024800010330     C                              or vinflg = '2')
024900010330     C*
025000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025100010711     C                   if        vinflg = *blanks or vinflg = '0'
025200010711     C                   clear                   vinmsg
025300010711     C                   endif
025400010601     C*
025500010330     C                   exsr      inzvar
025600010605     C                   exsr      impvab
025700010601     C*
025800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025900010605     C                   eval      depfil = VABLNP
026000010601     C                   exsr      repfil
026100010601     C                   if        depfil = invfil
026200021025     C                   if        vlrpoi = 999
026300030822     C                   move(P)   invfil        VABFGS
026400040119     C                   move(P)   invfil        VATFGS
026500021025     C                   else
026600030822     C                   move(P)   vlrpoi        VABFGS
026700040119     C                   move(P)   vlrpoi        VATFGS
026800021025     C                   endif
026900010601     C*
027000010601     C                   exsr      PREELA
027100010601     C*
027200010604     C* Ebbene...
027300010604     C*
027400040119     C  N31              add       1             §CTROKVB
027500040119     C   32              add       1             §CTRMO
027600040119     C   31              add       1             §CTRNO
027700030822     C                   if        wGiro = 2
027800040119     C  N31              exsr      wrivab
027900040119     C                   endif
028000010604     C*
028100010604     C                   if        *in31 = *off and
028200010604     C                             *in32 = *off
028300010604     C                   eval      vinflg = '1'
028400010604     C                   else
028500010604     C                   eval      recko = vindta
028600010604     C                   except    rigadett
028700010604     C                   eval      vinflg = '2'
028800010604     C                   endif
028900010604     C                   endif
029000010601     C*
029100010604     C                   endif
029200010604     C*
029300010330     C                   else
029400010330     C                   eval      vinflg = '1'
029500010330     C                   endif
029600010601     C*
029700010601     C  N70              update    tivin000
029800010330     C*
029900030822     C  N70              enddo
030000040119     C*
030100040119     C                   endif                                                  (endif - chkcall)
030200010601     C*
030300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030400010601     C                   if        cntNonEl = *zeros or
030500010601     C                             flgMulti = '0'
030600010330     C* Se non ci sono record con errori ...
030700010601     C                   if        §ctrno = 0 and
030800010604     C                             §ctrmo = 0 and
030900010601     C                             flgStato <> '2'
031000010330     C* ... restituisco esito OK.
031100010330     C                   eval      wrkesito = '0'
031200010330     C                   else
031300040119     C                   if        §ctrokvb > 0
031400010330     C                   eval      wrkesito = '1'
031500010330     C                   else
031600010615     C                   if        flgOk = '0'
031700010615     C                   eval      wrkesito = '2'
031800010615     C                   else
031900010615     C                   eval      wrkesito = '6'
032000010615     C                   endif
032100010330     C                   endif
032200010330     C                   endif
032300010601     C                   else
032400010601     C                   eval      wrkesito = '9'
032500010601     C                   endif
032600010330     C*
032700010330     C                   if        %open(tivin00r)
032800010330     C                   close     tivin00r
032900010330     C                   endif
033000021025     C                   if        %open(fivabwwr)
033100021025     C                   close     fivabwwr
033200010330     C                   endif
033300040512     C                   if        %open(fivatwwr)
033400040512     C                   close     fivatwwr
033500040512     C                   endif
033600010601     C*
033700010601     C                   if        vlrpoi <> 999
033800010601     C                   eval      invfil = vlrpoi
033900010601     C                   endif
034000010330     C*
034100040119     C                   if        §ctrokvb > 0
034200010601     C                             and invfil > *zeros
034300010330     C                   exsr      invio
034400010330     C                   endif
034500010601     C*
034600010618     C                   if        flgGiro = '1'
034700010601     C                   exsr      endela
034800010618     C                   endif
034900010330     C*
035000010330     C                   ENDSR
035100010330     C***
035200040119
035300040119
035400040119
035500040119     C*----------------------------------------------------*
035600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035700040119     C*----------------------------------------------------*
035800040119     C     WRIVAB        BEGSR
035900040119     C*
036000040119     C                   write     fivab000                                     => scarico il VAB
036100040119     C*
036200061106     C* Eseguo la scrittura dei record file estensione VAT
036300061106     C                   exsr      wrivat
036400040119     C*
036500040119     C                   ENDSR
036600040119
036700040119
036800040119
036900040119     C*----------------------------------------------------*
037000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037100040119     C*----------------------------------------------------*
037200040706     C     WRIVAT        BEGSR
037300040706     C*
037400040706     C* Attribuisco lo stesso numero numero spedizione del file testate
037500061106     C                   EVAL      VATAAS = VABAAS
037600061106     C                   EVAL      VATLNP = VABLNP
037700061106     C                   EVAL      VATNRS = VABNRS
037800061106     C                   EVAL      VATNSP = VABNSP
037900061106     C*
038000061106     C* Se valorizzato telefono destinatario => scrivo
038100061106     C                   IF        wVATNOT_B <> *blanks
038200061106     C                   EVAL      VATTRC = 'B'
038300061106     C                   EVAL      VATNOT = %trim(wVATNOT_B)
038400040706     C                   WRITE     FIVAT000
038500040706     C                   ADD       1             §CTROKVT
038600040706     C                   ENDIF
038700061106     C*
038800061106     C* Se valorizzato referente consegna => scrivo
038900061106     C                   IF        wVATNOT_A <> *blanks
039000061106     C                   EVAL      VATTRC = 'A'
039100061106     C                   EVAL      VATNOT = %trim(wVATNOT_A)
039200061106     C                   WRITE     FIVAT000
039300061106     C                   ADD       1             §CTROKVT
039400061106     C                   ENDIF
039500040119     C*
039600040119     C                   ENDSR
039700040119
039800040119
039900040119
040000040119     C*----------------------------------------------------*
040100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040200040119     C*----------------------------------------------------*
040300040119     C     PREVAT        BEGSR
040400040119     C*
040500040119     C* Compongo il nome del membro da dare al FIVATWWR
040600040119     C                   eval      parmbr = vlrhdl
040700040119     C                   movel     'M'           parmbr
040800061106     C                   eval      parccm = vlrKSC
040900040119     C                   eval      paropz = '1'
041000040119     C* Effettuo la chiamata al CLLE preposto
041100040119     C                   call(e)   'TITVVTC'
041200040119     C                   parm                    parccm
041300040119     C                   parm                    parmbr
041400040119     C                   parm                    paropz
041500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041600040119     C                   if        %error
041700040119     C                   movel     '1'           chkcall
041800040119     C                   else
041900040119     C                   movel     '0'           chkcall
042000040119     C                   endif
042100040119     C*
042200040119     C                   ENDSR
042300010601
042400010601
042500010601
042600010330     C*----------------------------------------------------*
042700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042800010330     C*----------------------------------------------------*
042900040119     C     INZVAR        BEGSR
043000010330     C*
043100030822     C* Inizializzo variabili di wrk
043200061106     C                   Z-ADD     *zeros        Num5_0            5 0
043300030822     C*
043400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
043500030822     C                   CLEAR                   FIVAB000
043600040119     C                   CLEAR                   FIVAT000
043700030822     C                   CLEAR                   SkSplitCSV
043800030822     C                   EVAL      i = 1
043900030822     C                   EVAL      posDa = *zeros
044000030822     C                   EVAL      posA  = *zeros
044100030822     C*
044200030822     C* Reimposto i valori di default
044300030822     C                   EXSR      DEFCAM
044400030822     C*
044500010330     C                   ENDSR
044600010330     C*----------------------------------------------------*
044700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044800010330     C*----------------------------------------------------*
044900010330     C     DEFCAM        BEGSR
045000040119     C*
045100040119     C* Imposto i valori d default a programma dei campi fondamentali
045200061106     C                   EVAL      VABCCM = 0053991
045300061106     C                   EVAL      VATCCM = 0053991
045400040706     C                   EVAL      VABLNP = 005
045500040706     C                   EVAL      VATLNP = 005
045600040706     C                   EVAL      VABCTR = 000
045700040119     C                   EVAL      VABCBO = '1'
045800030822     C*
045900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046000030715     C* e delimitatore testo.
046100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046400041201     C*
046500041201     C* Valorizzo a *off (ovvero no stampa) il flag che indica se mandare la stampa in filiale
046600041201     C                   SETOFF                                       50
046700030822     C*
046800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
046900030822     C                   EVAL      posDaDft = 1
047000030822     C                   EVAL      posADft  = 0
047100030822     C                   EVAL      wGiroDft = 0
047200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047300030822     C                             posDaDft > 0
047400030822     C*
047500030822     C* Gestisco il 1° giro
047600030822     C                   IF        wGiroDft = 0
047700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
047800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
047900030822     C* Incremento il contatore dei "giri"
048000030822     C                   EVAL      wGiroDft = 1
048100030822     C                   ELSE
048200030822     C                   EVAL      posDaDft = posADft
048300030822     C                   ENDIF
048400030822     C* Eseguo lo scan x trovare la fine del campo corrente
048500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
048600030822     C*
048700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
048800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
048900030822     C                   IF        posDaDft > 0 AND
049000030822     C                             posADft  > 0
049100030822     C* NCL
049200030822     C                   IF        %subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):1:3)
049500030822     C                             = 'NCL'
049600030822     C                   EVAL      PiStr=%trim(%subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900030822     C                   EXSR      CHKNUM
050000030822     C                   IF        PiInt=*on
050100030822     C                   Z-ADD     PiVal         VABNCL
050200030822     C                   ENDIF
050300030822     C                   ENDIF
050400030822     C* CCM
050500030822     C                   IF        %subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):1:3)
050800030822     C                             = 'CCM'
050900030822     C                   EVAL      PiStr=%trim(%subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200030822     C                   EXSR      CHKNUM
051300030822     C                   IF        PiInt=*on
051400030822     C                   Z-ADD     PiVal         VABCCM
051500040119     C                   Z-ADD     PiVal         VATCCM
051600030822     C                   ENDIF
051700030822     C                   ENDIF
051800030822     C* LNP
051900030822     C                   IF        %subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):1:3)
052200030822     C                             = 'LNP'
052300030822     C                   EVAL      PiStr=%trim(%subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600030822     C                   EXSR      CHKNUM
052700030822     C                   IF        PiInt=*on
052800030822     C                   Z-ADD     PiVal         VABLNP
052900040119     C                   Z-ADD     PiVal         VATLNP
053000030822     C                   ENDIF
053100030822     C                   ENDIF
053200030822     C* NRS
053300030822     C                   IF        %subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):1:3)
053600030822     C                             = 'NRS'
053700030822     C                   EVAL      PiStr=%trim(%subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054000030822     C                   EXSR      CHKNUM
054100030822     C                   IF        PiInt=*on
054200030822     C                   Z-ADD     PiVal         VABNRS
054300030822     C                   ENDIF
054400030822     C                   ENDIF
054500030822     C* CTR
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'CTR'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABCTR
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* PKB
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'PKB'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiNum=*on
056800030822     C                   Z-ADD     PiVal         VABPKB
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* VLB
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'VLB'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiNum=*on
058100030822     C                   Z-ADD     PiVal         VABVLB
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* QFT
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'QFT'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiNum=*on
059400030822     C                   Z-ADD     PiVal         VABQFT
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* CBO
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'CBO'
060200030822     C                   EVAL      VABCBO=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   ENDIF
060600030822     C* TSP
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'TSP'
061100030822     C                   EVAL      VABTSP=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   ENDIF
061500030822     C* VAS
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'VAS'
062000030822     C                   EVAL      VABVAS=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   ENDIF
062400030822     C* VCA
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'VCA'
062900030822     C                   EVAL      VABVCA=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   ENDIF
063300030822     C* GCA
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'GCA'
063800030822     C                   EVAL      VABGCA=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200030822     C* CTM
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'CTM'
064700030822     C                   EVAL      VABCTM=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* FFD
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'FFD'
065600030822     C                   EVAL      VABFFD=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* VAD
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'VAD'
066500030822     C                   EVAL      VABVAD=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* GMA
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'GMA'
067400030822     C                   EVAL      VABGMA=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* GGA
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'GGA'
068300030822     C                   EVAL      VABGGA=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* GVA
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'GVA'
069200030822     C                   EVAL      VABGVA=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* TC1
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'TC1'
070100030822     C                   EVAL      VABTC1=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* TC2
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'TC2'
071000030822     C                   EVAL      VABTC2=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* ...
071500030822     C                   ENDIF
071600030822     C                   ENDDO
071700020204     C*
071800010330     C                   ENDSR
071900010607     C*----------------------------------------------------*
072000061106     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072100010607     C*----------------------------------------------------*
072200010607     C     IMPVAB        BEGSR
072300010607     C*
072400010607     C                   SETOFF                                       3132
072500030515     C*
072600010607     C                   MOVEL     datcor        VABAAS
072700040119     C                   MOVEL     datcor        VATAAS
072800010607     C                   MOVE      datcor        VABMGS
072900010607     C*
073000030715     C**********
073100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073200030715     C**********
073300030715     C                   IF        wGiro = *zeros
073400040706     C*
073500040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
073600040706     C                   EVAL      depvinDTA = vinDTA
073700040113     C*
073800040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
073900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074000040113     C                                      %trim(intcol(3))
074100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074200031201     C     minu:maiu     XLATE     vindta        vindta
074300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074400030715     C                   DOW       posDa <= %len(%trim(vindta))
074500030715     C*
074600030715     C* Gestisco il 1° campo
074700030715     C                   IF        i = 1
074800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
074900030715     C                   EVAL      posDa = 1
075000030715     C* Eseguo lo scan x trovare la fine del primo campo
075100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075200030715     C* A questo punto "estraggo" il campo corrente
075300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075400030715     C                                                    (posA-posDa))
075500030715     C* X i campi successivi al 1°
075600030715     C                   ELSE
075700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
075800030715     C                   EVAL      posDa = posA + 1
075900030715     C* Eseguo lo scan x trovare la fine del campo corrente
076000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076100030715     C* Gestisco l'ultimo campo
076200030715     C                   IF        posA = *zeros
076300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076400030715     C* A questo punto "estraggo" il campo corrente
076500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076600030715     C                                                    (posA-posDa))
076700030715     C                   LEAVE
076800030715     C                   ELSE
076900030715     C* A questo punto "estraggo" il campo corrente
077000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077100030715     C                                                    (posA-posDa))
077200030715     C                   ENDIF
077300030715     C                   ENDIF
077400030715     C* Incremento il contatore d campo
077500030715     C                   EVAL      i = i +1
077600030715     C                   ENDDO
077700030715     C*
077800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
077900030715     C* delimitazione testo
078000030715     C                   EVAL      i = 1
078100030715     C                   DOW       i < %elem(SkSplitFLD)
078200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078400030715     C                   EVAL      i = i + 1
078500030715     C                   ENDDO
078600030715     C                   EVAL      wGiro = 1
078700040706     C* Ripristino la 1° riga originale
078800040706     C                   EVAL      vinDTA = depvinDTA
078900030715     C                   ELSE
079000030715     C                   EVAL      wGiro = 2
079100031204     C**********
079200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079300031204     C* d separatore campo
079400031204     C**********
079500031204     C                   EVAL      vindta = %trim(vindta)
079600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
079700031204     C                   EVAL      vindta = %subst(vindta:2)
079800031204     C                   ENDDO
079900031204     C*
080000031204     C                   Z-ADD     *zeros        lunghInput        4 0
080100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080500031204     C                   ENDDO
080600030715     C**********
080700030715     C* Effettuo lo split del campo dati d input
080800030715     C**********
080900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081000030715     C                   DOW       posDa <= %len(%trim(vindta))
081100030715     C*
081200030715     C* Gestisco il 1° campo
081300030715     C                   IF        i = 1
081400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081500030715     C                   EVAL      posDa = 1
081600030715     C* Eseguo lo scan x trovare la fine del primo campo
081700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081800030715     C* A questo punto "estraggo" il campo corrente
081900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082000030715     C                                                    (posA-posDa))
082100030715     C* X i campi successivi al 1°
082200030715     C                   ELSE
082300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082400030715     C                   EVAL      posDa = posA + 1
082500031204     C* Verifico che nn vi sia il campo nn valorizzato
082600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
082700031204     C* Se campo nn valorizzato skippo al prossimo
082800031204     C                   EVAL      SkSplitCSV(i) = *blanks
082900031204     C                   EVAL      posA  = posA + 1
083000031204     C                   ELSE
083100030715     C* Eseguo lo scan x trovare la fine del campo corrente
083200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083300030715     C* Gestisco l'ultimo campo
083400030715     C                   IF        posA = *zeros
083500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C                   LEAVE
084000030715     C                   ELSE
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   ENDIF
084500031204     C                   ENDIF
084600030715     C                   ENDIF
084700030715     C* Incremento il contatore d campo
084800030715     C                   EVAL      i = i +1
084900030715     C                   ENDDO
085000030715     C*
085100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085200030715     C* delimitazione testo
085300030715     C                   EVAL      i = 1
085400030715     C                   DOW       i < %elem(SkSplitCSV)
085500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
085600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
085700030715     C                   EVAL      i = i + 1
085800030715     C                   ENDDO
085900030715     C*
086000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086100030715     C                   EVAL      i = 1
086200030715     C                   DOW       i < %elem(SkSplitFLD)
086300030715     C*
086400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
086600030715     C                             SkSplitCSV(i) <> *blanks
086700030721     C*** ATB
086800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
086900030721     C                   EVAL      VABATB = SkSplitCSV(i)
087000030721     C                   ENDIF
087100030715     C*** RSD
087200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087300061106     C                   EVAL      wVABRAGSOC = %trim(SkSplitCSV(i))
087400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
087500061106     C     '@':'A'       XLATE     wVABRAGSOC    VABRSD
087600061106     C                   EVAL      VABRSD = %subst(wVABRAGSOC:1:35)
087700061106     C                   EVAL      VABRD2 = %subst(wVABRAGSOC:36:35)
087800030715     C* ==
087900030715     C                   ENDIF
088000030721     C*** RD2
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030715     C*** IND
088500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088600030715     C                   EVAL      VABIND = SkSplitCSV(i)
088700030715     C                   ENDIF
088800030715     C*** LOD
088900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
089000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089100030715     C                   ENDIF
089200030715     C*** PRD
089300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089500030715     C                   ENDIF
089600030721     C*** NZD
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
089800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030715     C*** RMA
090100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090300030715     C                   ENDIF
090400030715     C*** NOT
090500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
090700030715     C                   ENDIF
090800030715     C*** NT2
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
091000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091100030715     C                   ENDIF
091200030715     C*** VCA
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030715     C*** VAS
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
091800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** FFD
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030721     C*** GC1
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030721     C*** GC2
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
093000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030721     C*** TSP
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
093400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** NAS
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
093800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** TIC
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
094200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*** GCA
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
094600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** XCO
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
095000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** CTM
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
095400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** TCR
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
095800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** CTS
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
096200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** FTM
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
096600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** VAD
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
097000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** GMA
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
097400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** GGA
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
097800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** GVA
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
098200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** TC1
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
098600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** TC2
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
099000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** SCL
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
099400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** RMO
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
099800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** NMO
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
100200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
100300030721     C                   ENDIF
100400040119     C*** CBO
100500040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100600040706     C                   EVAL      VABCBO = SkSplitCSV(i)
100700040119     C                   ENDIF
100800030721     C*
100900040706     C* Reperisco quindi i campi numerici...
101000040706     C*** NSP
101100040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101200040706     C                   EVAL      PiStr=SkSplitCSV(i)
101300040706     C                   EXSR      CHKNUM
101400040706     C                   IF        PiInt=*on
101500040706     C                   Z-ADD     PiVal         VABNSP
101600040706     C                   ELSE
101700040706     C                   SETON                                        31
101800040706     C                   EVAL      VABNSP = *zeros
101900040706     C                   EVAL      vinmsg = %trimr(vinmsg)
102000040706     C                             + ' ' + 'VABNSP'
102100040706     C                   ENDIF
102200040706     C                   ENDIF
102300040706     C*** RMN
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
106800030721     C                   Z-ADD     PiVal         VABPKB
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
130300061106     C* Gestisco i campi "personalizzati"
130400061106     C*** VATNOTB
130500061106     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOTB'
130600061106     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
130700061106     C                   ENDIF
130800061106     C*** VATNOTA
130900061106     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOTA'
131000061106     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
131100061106     C                   ENDIF
131200061106     C*** VABNOT
131300061106     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
131400061106     C                   EVAL      wVABNOTE = %trim(SkSplitCSV(i))
131500061106     C                   EVAL      VABNOT = %subst(wVABNOTE:1:35)
131600061106     C                   EVAL      VABNT2 = %subst(wVABNOTE:36:35)
131700061106     C                   ENDIF
131800061106     C*** VABRMA_1
131900061106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA_1'
132000061106     C                   EVAL      wVABRMA_1 = %trim(SkSplitCSV(i))
132100061106     C                   ENDIF
132200061106     C*** VABRMA_2
132300061106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA_2'
132400061106     C                   EVAL      wVABRMA_2 = %trim(SkSplitCSV(i))
132500061106     C                   ENDIF
132600030715     C*
132700030715     C                   ENDIF
132800030715     C                   EVAL      i = i + 1
132900030715     C                   ENDDO
133000061106     C*
133100061106     C* Compongo il riferimento mittente alfanumerico
133200061106     C                   EVAL      VABRMA = %trim(wVABRMA_1) + %trim(wVABRMA_2)
133300040706     C*
133400040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
133500040706     C* Reperisco la provincia dal CAP e dalla località
133600040706     C                   IF        VABCAD <> *blanks
133700040706     C                   CLEAR                   TISI95DS
133800040706     C                   EVAL      I95TCN = '3'
133900040706     C                   Z-ADD     datcor        I95DAT
134000040706     C                   EVAL      I95CAP = VABCAD
134100040706     C                   EVAL      I95LOC = VABLOD
134200040706     C                   CALL      'TISI95R'
134300040706     C                   PARM                    TISI95DS
134400040706     C                   EVAL      VABPRD = O95PRV
134500040706     C                   ENDIF
134600040706     C*
134700040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
134800040706     C* d valorizzarli sempre entrambi
134900040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
135000040706     C                   Z-ADD     VABNSP        VABRMN
135100040706     C                   ENDIF
135200040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
135300040706     C                   MOVEL     VABRMN        VABRMA
135400040706     C                   ENDIF
135500040113     C*
135600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
135700040113     C                   IF        VABCAS > *zeros
135800040119     C                   EVAL      VABVCA = 'EUR'
135900040119     C                   IF        VABCBO = '1'
136000040113     C                   EVAL      VABCBO = '4'
136100040119     C                   ENDIF
136200040119     C                   IF        VABCBO = '2'
136300040119     C                   EVAL      VABCBO = '6'
136400040119     C                   ENDIF
136500040113     C                   ENDIF
136600020204     C*
136700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
136800020204     C                   EXSR      CHKIMPDIV
136900030715     C                   ENDIF
137000020204     C*
137100010607     C                   ENDSR
137200010607     C*----------------------------------------------------*
137300010601
137400020204
137500020204     C*----------------------------------------------------*
137600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
137700020204     C*----------------------------------------------------*
137800020204     C     CHKIMPDIV     BEGSR
137900020204     C*
138000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
138100020204     C                   Z-ADD     *zeros        wrkDec            9 9
138200020204     C*
138300020204     C* Come prima cosa effettuo considerazioni sulla divisa
138400020204     C                   IF        vabIAS > *zeros
138500020204     C                   IF        vabVAS <> 'EUR'
138600020204     C                   EVAL      vabVAS =  'ITL'
138700020204     C                   ENDIF
138800020204     C                   ENDIF
138900020204     C*
139000020204     C                   IF        vabCAS > *zeros
139100020204     C                   IF        vabVCA <> 'EUR'
139200020204     C                   EVAL      vabVCA =  'ITL'
139300020204     C                   ENDIF
139400020204     C                   ENDIF
139500020204     C*
139600020204     C                   IF        vabVMD > *zeros
139700020204     C                   IF        vabVAD <> 'EUR'
139800020204     C                   EVAL      vabVAD =  'ITL'
139900020204     C                   ENDIF
140000020204     C                   ENDIF
140100020204     C*
140200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
140300020204     C                   Z-ADD     vabIAS        wrkDec
140400020204     C                   IF        wrkDec > *zeros
140500020204     C                   IF        vabVAS = 'ITL'
140600020204     C                   EVAL      vabIAS = *zeros
140700020204     C                   ENDIF
140800020204     C                   ENDIF
140900020204     C*
141000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
141100020204     C                   Z-ADD     vabCAS        wrkDec
141200020204     C                   IF        wrkDec > *zeros
141300020204     C                   IF        vabVCA = 'ITL'
141400020204     C                   EVAL      vabCAS = *zeros
141500020204     C                   ENDIF
141600020204     C                   ENDIF
141700020204     C*
141800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
141900020204     C                   Z-ADD     vabVMD        wrkDec
142000020204     C                   IF        wrkDec > *zeros
142100020204     C                   IF        vabVAD = 'ITL'
142200020204     C                   EVAL      vabVMD = *zeros
142300020204     C                   ENDIF
142400020204     C                   ENDIF
142500020204     C*
142600020204     C                   ENDSR
142700020204     C***
142800020204
142900010330
143000010330
143100010330     C*----------------------------------------------------*
143200010330     C*  CONTROLLO NUMERICITA' CAMPI
143300010330     C*----------------------------------------------------*
143400010330     C     CHKNUM        BEGSR
143500010330     C*
143600010606     C                   IF        PiDecChr = *blanks
143700030715     C                   EVAL      PiDecChr = CharNUM
143800010606     C                   ENDIF
143900010606     C*
144000010606     C                   CALL(e)   'ISNUMERIC'
144100010330     C                   PARM                    PiStr            30
144200010606     C                   PARM                    PiDecChr          1
144300010330     C                   PARM      *ZEROS        PiVal            30 9
144400010330     C                   PARM      '0'           PiInt             1
144500010330     C                   PARM      '0'           PiNum             1
144600010330     C                   IF        %error
144700010606     C                   EVAL      PiNum=*off
144800010330     C                   ENDIF
144900010330     C*
145000010330     C                   ENDSR
145100010330     C***
145200010330
145300010601
145400010601
145500010601
145600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
145700010601     C     repfil        BEGSR
145800010601     C*
145900010601     C                   if        invfil = *zeros and
146000010601     C                             depfil > *zeros and
146100010629     C                             (vinflg = *blanks or
146200010629     C                              vinflg = *zeros)
146300010601     C
146400010601     C                   eval      invfil = depfil
146500010601     C                   endif
146600010601     C*
146700010601     C                   if        depfil <> invfil and
146800010601     C                             invfil > *zeros
146900010601     C                   eval      flgMulti = '1'
147000010601     C                   if        vinflg = *blanks
147100010601     C                   add       1             cntNonEl
147200010601     C                   endif
147300010601     C                   endif
147400010601     C*
147500010601     C                   if        vinflg = '2'
147600010601     C                   eval      flgStato = '2'
147700010601     C                   endif
147800010601     C*
147900010601     C                   ENDSR
148000010601     C***
148100010601
148200010601
148300010601
148400010330
148500040119      /TITLE Invio dei dati al punto operativo.
148600040119     C     invio         BEGSR
148700040119     C*
148800040119     C* 1° invio FIVAT
148900040119     C                   reset                   dscmz
149000040119     C                   move      vlrpoi        cmzdst
149100040119     C                   eval      cmzfld = 'FIVATWWR'
149200040119     C                   eval      cmzmbd = vlrhdl
149300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
149400040119     C***                if        prmfir = *blanks
149500040119     C                   eval      cmzfla = 'FIVAT00F'
149600040119     C                   eval      cmzmba = 'FIVAT00F'
149700040119     C***                else
149800040119     C***                eval      cmzfla = prmfir
149900040119     C***                eval      cmzmba = prmfir
150000040119     C***                endif
150100040119     C                   eval      cmznrr = *zeros
150200040119     C                   move      §ctrokvt      cmznrr
150300040119     C                   eval      cmzlba = vlrfl1
150400040119     C                   call(e)   'TIS711C'
150500040119     C                   parm                    dscmz
150600040119     C                   parm      *blanks       esito
150700040119     C                   if        %error
150800040119     C                             or cmzerr = '1'
150900040119     C                             or esito  = '1'
151000040119     C                   eval      wrkesito = '3'
151100040119     C                   else
151200040119     C*
151300040119     C* 2° invio FIVAB
151400040119     C                   reset                   dscmz
151500040119     C                   move      vlrpoi        cmzdst
151600040119     C                   eval      cmzfld = vlrfou
151700040119     C                   eval      cmzmbd = vlrhdl
151800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
151900040119     C***                if        prmfir = *blanks
152000040119     C                   eval      cmzfla = 'FIVAB00F'
152100040119     C                   eval      cmzmba = 'FIVAB00F'
152200040119     C***                else
152300040119     C***                eval      cmzfla = prmfir
152400040119     C***                eval      cmzmba = prmfir
152500040119     C***                endif
152600040119     C                   eval      cmznrr = *zeros
152700040119     C                   move      §ctrokvb      cmznrr
152800040119     C                   eval      cmzlba = vlrfl1
152900040119     C                   call(e)   'TIS711C'
153000040119     C                   parm                    dscmz
153100040119     C                   parm      *blanks       esito
153200040119     C                   if        %error
153300040119     C                             or cmzerr = '1'
153400040119     C                             or esito  = '1'
153500040119     C                   eval      wrkesito = '3'
153600040119     C                   endif
153700040119     C                   endif
153800040119     C*
153900040119     C                   ENDSR
154000040119     C***
154100010601
154200010601
154300010601
154400010601
154500010601      /TITLE Invio dei dati al punto operativo.
154600010601     C     opeini        BEGSR
154700010601     C*
154800010601     C* Inizializzo flag e contatori operativi
154900010601     C                   movel     '0'           flgGiro           1
155000010601     C                   movel     '0'           flgMulti          1
155100010601     C                   movel     '1'           flgStato          1
155200010615     C                   movel     '0'           flgOk             1
155300010601     C                   z-add     *zeros        cntNonEl         10 0
155400010601     C                   z-add     *zeros        depfil            3 0
155500010601     C                   z-add     *zeros        invfil            3 0
155600010601     C*
155700010601     C                   ENDSR
155800010601     C***
155900010601
156000010601
156100050421
156200050421
156300070411
156400070411     C     *pssr         BEGSR
156500070411     C*
156600070411     C                   if        %open(tivin00r)
156700070411     C                   close     tivin00r
156800070411     C                   endif
156900070411     C                   if        %open(fivabwwr)
157000070411     C                   close     fivabwwr
157100070411     C                   endif
157200070411     C                   if        %open(fivatwwr)
157300070411     C                   close     fivatwwr
157400070411     C                   endif
157500070411     C*
157600070411     C* Effettuo la chiamata al CLLE preposto
157700070411     C                   call(e)   'TITVVTC'
157800070411     C                   parm                    parccm
157900070411     C                   parm                    parmbr
158000070411     C                   parm      '2'           paropz
158100070411     C*
158200070411     C                   eval      wrkesito = '2'
158300070411     C*
158400070411     C                   seton                                        LR
158500070411     C*
158600070411     C                   ENDSR     '*CANCL'
158700070411     C***
158800070411
158900050421
159000010601
159100010330
159200010330
159300000613     C     *inzsr        BEGSR
159400990910     C*
159500990910     C     *entry        plist
159600990920     C                   parm                    tivlrds
159700990921     C                   parm      wrkesito      esito
159800000724     C                   parm                    prmlit
159900000710     C                   parm                    prmfir
160000010330     C*
160100010330     C* CALCOLA LA DATA CORRENTE
160200010330     C                   time                    wn14             14 0
160300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
160400010330     C                   z-add     wn8           g08dat
160500010330     C                   z-add     *zeros        g08inv
160600010330     C                   movel     '0'           g08err
160700010330     C                   call      'XSRDA8'
160800010330     C                   parm                    wlbda8
160900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
161000000613     C*
161100000613     C                   ENDSR
161200000613     C***
161300990908
161400061106     Otitvv7p   E            riepilogo         2
161500990915     O                                              'Upload via Internet'
161600990915     O                                           +1 'Traduzione TIVIN00R -'
161700021025     O                                           +1 'FIVABWWR'
161800990915     O                                           +1 'Testate bolle'
161900990915     O          E            riepilogo   2
162000990915     O                       wrkdata
162100990915     O                       wrkora              +1
162200990915     O                       procname            +1
162300990915     O          E            riepilogo   2
162400990915     O                                              'Cliente..................:'
162500010605     O                       VABCCM        z     +1
162600990915     O          E            riepilogo   2
162700990920     O                                              'Riferimento Strategi.....:'
162800990920     O                       vlrhdl              +1
162900990915     O          E            riepilogo   2
163000990915     O                                              'Giusti...................:'
163100040119     O                       §CTROKVB      2   +  1
163200990915     O          E            riepilogo   2
163300990915     O                                              'Sbagliati e corretti.....:'
163400971022     O                       §CTRMO        2   +  1
163500990915     O          E            riepilogo   2
163600990915     O                                              'Sbagliati e scartati.....:'
163700971022     O                       §CTRNO        2   +  1
163800000613
163900061106     Otitvv7ps  E            testdett          2
164000000613     O                                              'Upload via Internet'
164100000613     O                                           +1 'Traduzione TIVIN00R -'
164200021025     O                                           +1 'FIVABWWR'
164300000613     O                                           +1 'Testate bolle'
164400000616     O          E            testdett    3
164500000613     O                                           +2 'N° rec'
164600000613     O                                           +3 'Anteprima contenuto'
164700000616     O          E            rigadett    2
164800000613     O                       rrnum               +2
164900000621     O                       recko               +3
165000000616     O          E            findett     2
165100000613     O                       wrkdata
165200000613     O                       wrkora              +1
165300000613     O                       procname            +1
165400000616     O          E            findett     2
165500000613     O                                              'Cliente..................:'
165600010605     O                       VABCCM        z     +1
165700000616     O          E            findett     2
165800000613     O                                              'Riferimento Strategi.....:'
165900000613     O                       vlrhdl              +1
166000000616     O          E            findett     2
166100000613     O                                              'Giusti...................:'
166200040119     O                       §CTROKVB      2   +  1
166300000616     O          E            findett     2
166400000613     O                                              'Sbagliati e corretti.....:'
166500000613     O                       §CTRMO        2   +  1
166600000616     O          E            findett     2
166700000613     O                                              'Sbagliati e scartati.....:'
166800000613     O                       §CTRNO        2   +  1
166900000613** CMD - COMANDI CL
167000061106OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVV7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
167100061106OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVV7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
167200061106DLTOVR FILE(TITVV7P TITVV7PS) LVL(*)
167300010606
167400000613
167500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
167600061106VABNSP;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABNCL;VABRMN;Fill1;VABTSP;
167700061106VABPKB;VABTC1;VATNOTB;VATNOTA;VABNOT;VABRMA_1;VABRMA_2;VABNAS;Fill2
167800040113
