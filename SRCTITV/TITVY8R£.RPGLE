000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800070405     Ftitvy8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
000901070329     F                                     infsr(*pssr)
001000070405     Ftitvy8ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001101070329     F                                     infsr(*pssr)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700040113     D*------------
001800040113     D* SCHIEDE A PROGRAMMA
001900040113     D*------------
002000040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800040128     D tisi95ds      e ds
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D wrkdata         s               d
003400010330     D wrkora          s               t
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600010330     D recko           s            150    INZ(*blanks)
003700011113     D depcmd          s            150    INZ(*blanks)
003800040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003900040510     D parccm          s              8    INZ(*blanks)
004000040119     D parmbr          s             10    INZ(*blanks)
004100040119     D paropz          s              1    INZ(*blanks)
004200040119     D chkcall         s              1    INZ(*blanks)
004300030715     D*------------------
004400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004500030715     D*------------------
004600030715     D SkSplitFLD      S             10    DIM(100)
004700030715     D SkSplitCSV      S            256    DIM(100)
004800030715     D CharCSV         S              1
004900030715     D CharTXT         S              1
005000030715     D CharNUM         S              1
005100030715     D posDa           S              3  0 INZ(*zeros)
005200030715     D posA            S              3  0 INZ(*zeros)
005300030715     D i               s              3  0 INZ(1)
005400030715     D wGiro           s              1  0 INZ(*zeros)
005500060331     D wVATNOT_A       s                   LIKE(VATNOT)
005600060331     D wVATNOT_B       s                   LIKE(VATNOT)
005700060331     D wVATNOT_E       s                   LIKE(VATNOT)
005800060331     D wVABNSP         s                   LIKE(VABNSP) INZ
005900030822     D*------------------
006000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006100030822     D*------------------
006200030822     D posDaDft        S              3  0 INZ(*zeros)
006300030822     D posADft         S              3  0 INZ(*zeros)
006400030822     D j               s              3  0 INZ(1)
006500030822     D wGiroDft        s              1  0 INZ(*zeros)
006600010330     D*------------------
006700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006800010330     D*------------------
006900010330     D WLBDA8          DS                  INZ
007000010330     D  G08DAT                 1      8  0
007100010330     D  G08INV                 9     16  0
007200010330     D  G08ERR                17     17
007300010330     D  G08TGI                18     22  0
007400031201     D*------------------
007500031201     D* Costanti
007600031201     D*------------------
007700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007900010330
008000010330
008100010330
008200990915     C                   time                    wrkdata
008300990915     C                   time                    wrkora
008400000913     C                   reset                   rrnum
008500990921     C                   reset                   esito
008600990921     C                   reset                   wrkesito
008700010601     C*
008800010601     C                   exsr      opeini
008900010605     C                   exsr      rwvab
009000010601     C*
009100010601     C                   seton                                        lr
009200010601
009300010601
009400010601
009500010601     C*--------------------------------------------------------
009600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009700010601     C*--------------------------------------------------------
009800010601     C     PREELA        BEGSR
009900010601     C*
010000010601     C* SE OCCORRE SPEDIRE IN FILIALE
010100010601     C                   if        invfil <> *zeros and
010200010601     C                             flgGiro = '0'
010300010601     C*
010400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010500010601     C                   eval      flgGiro = '1'
010600000724     C*
010700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010900010601     C     invfil        chain     azorg01l
011000000724     C                   if        %found
011100000724     C                   movel(p)  CMD(1)        depcmd
011200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011300000724     C*
011400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011500011113     C                   Z-ADD     150           LENGH            15 5
011600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011700000724     C                   PARM                    depcmd
011800000724     C                   PARM                    LENGH
011900000724     C*
012000000724     C                   endif
012100000616     C*
012200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012300000616     C                   movel(p)  CMD(2)        depcmd
012400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012500000616     C*
012600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012700011113     C                   Z-ADD     150           LENGH            15 5
012800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012900000616     C                   PARM                    depcmd
013000000616     C                   PARM                    LENGH
013100000613     C*
013200070405     C                   if        not %open(titvy8ps)
013300070405     C                   open      titvy8ps
013400070329     C                   except    testdett
013500000613     C                   endif
013600010601     C*
013700010601     C                   endif
013800010601     C*
013900010601     C                   ENDSR
014000010601     C***
014100010601
014200010601
014300010601
014400010601
014500010601     C*--------------------------------------------------------
014600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014700010601     C*--------------------------------------------------------
014800010601     C     ENDELA        BEGSR
014900990915     C*
015000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015100040119     C*
015200040119     C* Effettuo la chiamata al CLLE preposto
015300040119     C                   call(e)   'TITVVTC'
015400040510     C                   parm                    parccm
015500040119     C                   parm                    parmbr
015600040119     C                   parm      '2'           paropz
015700050201     C*
015800050201     C* Effettuo lancio TISI95 solo x chiusura
015900050201     C                   CLEAR                   TISI95DS
016000050201     C                   EVAL      I95TLA = 'C'
016100050201     C                   CALL      'TISI95R'
016200050201     C                   PARM                    TISI95DS
016300000613     C*
016400070405     C                   if        %open(titvy8ps)
016500000616     C                   except    findett
016600070405     C                   close     titvy8ps
016700000613     C                   endif
016800000616     C*
016900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017000011113     C                   Z-ADD     150           LENGH            15 5
017100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017200000616     C                   PARM                    CMD(3)
017300000616     C                   PARM                    LENGH
017400000616     C*
017500010601     C                   ENDSR
017600010601     C***
017700010601
017800010601
017900010601
018000000613
018100000613     C*--------------------------------------------------------
018200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018300000613     C*--------------------------------------------------------
018400000621     C     STPR          BEGSR
018500000613     C*
018600070405     C                   if        not %open(titvy8p)
018700070405     C   50              open      titvy8p
018800990915     C                   endif
018900990915     C*
019000030715     C   50              except    riepilogo
019100990915     C*
019200070405     C                   if        %open(titvy8p)
019300070405     C   50              close     titvy8p
019400990914     C                   endif
019500000613     C*
019600000613     C                   ENDSR
019700000613     C***
019800000613
019900010601
020000010601
020100010330     C*--------------------------------------------------------
020200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020300010330     C*--------------------------------------------------------
020400010605     C     RWVAB         BEGSR
020500010330     C*
020600010330     C                   if        not %open(tivin00r)
020700010330     C                   open      tivin00r
020800010330     C                   endif
020900021025     C                   if        not %open(fivabwwr)
021000021025     C                   open      fivabwwr
021100010330     C                   endif
021200040119     C*
021300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021400040119     C                   exsr      prevat
021500040119     C*
021600040119     C                   if        chkcall = '0'
021700040119     C*
021800040119     C                   if        not %open(fivatwwr)
021900040119     C                   open      fivatwwr
022000040119     C                   endif
022100010330     C*
022200040119     C                   clear                   §CTROKVB          7 0
022300040119     C                   clear                   §CTROKVT          7 0
022400040119     C                   clear                   §CTRMO            7 0
022500040119     C                   clear                   §CTRNO            7 0
022600010330     C*
022700030822     C                   do        *HIVAL
022800010330     C*
022900030822     C                   read      tivin00r                               70
023000010618     C*
023100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023200010618     C                   if        vinflg = '1'
023300010618     C                   eval      flgOk = '1'
023400010618     C                   endif
023500040510     C*
023600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
023700040510     C                   z-add     *zeros        wLenVINDTA        4 0
023800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
023900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024200040510     C                   z-add     1             wFlgVINDTA
024300040510     C                   leave
024400040510     C                   endif
024500040510     C                   enddo
024600010618     C*
024700040510     C                   if        vindta > *blanks AND
024800040510     C                             wFlgVINDTA = 1
024900010330     C                   add       1             rrnum
025000010330     C*
025100010601     C                   if        *in70 = *off and
025200010330     C                             (vinflg = *blanks
025300010330     C                              or vinflg = '0'
025400010330     C                              or vinflg = '2')
025500010330     C*
025600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025700010711     C                   if        vinflg = *blanks or vinflg = '0'
025800010711     C                   clear                   vinmsg
025900010711     C                   endif
026000010601     C*
026100010330     C                   exsr      inzvar
026200010605     C                   exsr      impvab
026300010601     C*
026400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026500010605     C                   eval      depfil = VABLNP
026600010601     C                   exsr      repfil
026700010601     C                   if        depfil = invfil
026800021025     C                   if        vlrpoi = 999
026900030822     C                   move(P)   invfil        VABFGS
027000021025     C                   else
027100030822     C                   move(P)   vlrpoi        VABFGS
027200021025     C                   endif
027300010601     C*
027400010601     C                   exsr      PREELA
027500010601     C*
027600010604     C* Ebbene...
027700010604     C*
027800040119     C  N31              add       1             §CTROKVB
027900040119     C   32              add       1             §CTRMO
028000040119     C   31              add       1             §CTRNO
028100030822     C                   if        wGiro = 2
028200040510     C* Scrivo sempre il VAT
028300040510     C  N31              exsr      wrivat
028400040510     C* Scrivo il VAB solo a rottura d spedizione
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
036900040510     C     WRIVAB        BEGSR
037000040119     C*
037100060331     C* Gestisco l'eventuale rottura x numero spedizione
037200060331     C                   if        VABNSP <> wVABNSP
037201070301     C                   if        wVATNOT_E <> *blanks
037202070301     C                   eval      VABCTM = '7Q'
037203070301     C                   endif
037300040119     C                   write     fivab000                                     => scarico il VAB
037400060331     C                   eval      wVABNSP = VABNSP
037500060331     C                   endif
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038300040119     C*----------------------------------------------------*
038400040510     C     WRIVAT        BEGSR
038500060512     C*
038600060512     C* Inizializzo i campi chiave
038700060512     C                   eval      VATFGS = VABFGS
038800060512     C                   eval      VATAAS = VABAAS
038900060512     C                   eval      VATCCM = VABCCM
039000060512     C                   eval      VATNSP = VABNSP
039100060512     C                   eval      VATLNP = VABLNP
039200040714     C*
039300040714     C* Solo se VATNOT valorizzato scarico il buffer
039400040714     C                   if        VATNOT <> *blanks
039500040119     C*
039600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039700040510     C                   if        VATTRC = *blanks
039800040510     C                   eval      VATTRC = 'E'
039900040510     C                   endif
040000040510     C                   write     FIVAT000
040100040119     C                   add       1             §CTROKVT
040200040714     C*
040300040714     C                   endif
040400060331     C*
040500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040600060331     C                   if        wVATNOT_A <> *blanks
040700060331     C                   eval      VATNOT = wVATNOT_A
040800060331     C                   eval      VATTRC = 'A'
040900060331     C                   write     FIVAT000
041000060331     C                   add       1             §CTROKVT
041100060331     C                   endif
041200060331     C*
041300060331     C                   if        wVATNOT_B <> *blanks
041400060331     C                   eval      VATNOT = wVATNOT_B
041500060331     C                   eval      VATTRC = 'B'
041600060331     C                   write     FIVAT000
041700060331     C                   add       1             §CTROKVT
041800060331     C                   endif
041900060331     C*
042000060331     C                   if        wVATNOT_E <> *blanks
042100060331     C                   eval      VATNOT = wVATNOT_E
042200060331     C                   eval      VATTRC = 'E'
042300060331     C                   write     FIVAT000
042400060331     C                   add       1             §CTROKVT
042500060331     C                   endif
042600040119     C*
042700040119     C                   ENDSR
042800040119
042900040119
043000040119
043100040119     C*----------------------------------------------------*
043200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043300040119     C*----------------------------------------------------*
043400040119     C     PREVAT        BEGSR
043500040119     C*
043600040119     C* Compongo il nome del membro da dare al FIVATWWR
043700040119     C                   eval      parmbr = vlrhdl
043800040119     C                   movel     'M'           parmbr
043900040510     C                   eval      parccm = vlrksc
044000040119     C                   eval      paropz = '1'
044100040119     C* Effettuo la chiamata al CLLE preposto
044200040119     C                   call(e)   'TITVVTC'
044300040119     C                   parm                    parccm
044400040119     C                   parm                    parmbr
044500040119     C                   parm                    paropz
044600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044700040119     C                   if        %error
044800040119     C                   movel     '1'           chkcall
044900040119     C                   else
045000040119     C                   movel     '0'           chkcall
045100040119     C                   endif
045200040119     C*
045300040119     C                   ENDSR
045400010601
045500010601
045600010601
045700010330     C*----------------------------------------------------*
045800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045900010330     C*----------------------------------------------------*
046000040119     C     INZVAR        BEGSR
046100010330     C*
046200030822     C* Inizializzo variabili di wrk
046300020204     C                   Z-ADD     *zeros        Num5_0            5 0
046400060331     C                   MOVEL     *blanks       wVATNOT_A
046500060331     C                   MOVEL     *blanks       wVATNOT_B
046600060331     C                   MOVEL     *blanks       wVATNOT_E
046700030822     C*
046800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046900030822     C                   CLEAR                   FIVAB000
047000040119     C                   CLEAR                   FIVAT000
047100030822     C                   CLEAR                   SkSplitCSV
047200030822     C                   EVAL      i = 1
047300030822     C                   EVAL      posDa = *zeros
047400030822     C                   EVAL      posA  = *zeros
047500030822     C*
047600030822     C* Reimposto i valori di default
047700030822     C                   EXSR      DEFCAM
047800030822     C*
047900010330     C                   ENDSR
048000010330     C*----------------------------------------------------*
048100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048200010330     C*----------------------------------------------------*
048300010330     C     DEFCAM        BEGSR
048400030822     C*
048500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
048600030715     C* e delimitatore testo.
048700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049000030822     C*
049100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
049300030715     C                   SETON                                        50
049400030715     C                   ELSE
049500030715     C                   SETOFF                                       50
049600030715     C                   ENDIF
049700030822     C*
049800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
049900030822     C                   EVAL      posDaDft = 1
050000030822     C                   EVAL      posADft  = 0
050100030822     C                   EVAL      wGiroDft = 0
050200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050300030822     C                             posDaDft > 0
050400030822     C*
050500030822     C* Gestisco il 1° giro
050600030822     C                   IF        wGiroDft = 0
050700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
050800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
050900030822     C* Incremento il contatore dei "giri"
051000030822     C                   EVAL      wGiroDft = 1
051100030822     C                   ELSE
051200030822     C                   EVAL      posDaDft = posADft
051300030822     C                   ENDIF
051400030822     C* Eseguo lo scan x trovare la fine del campo corrente
051500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
051600030822     C*
051700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
051800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
051900030822     C                   IF        posDaDft > 0 AND
052000030822     C                             posADft  > 0
052100030822     C* NCL
052200030822     C                   IF        %subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):1:3)
052500030822     C                             = 'NCL'
052600030822     C                   EVAL      PiStr=%trim(%subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900030822     C                   EXSR      CHKNUM
053000030822     C                   IF        PiInt=*on
053100030822     C                   Z-ADD     PiVal         VABNCL
053200030822     C                   ENDIF
053300030822     C                   ENDIF
053400030822     C* CCM
053500030822     C                   IF        %subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):1:3)
053800030822     C                             = 'CCM'
053900030822     C                   EVAL      PiStr=%trim(%subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200030822     C                   EXSR      CHKNUM
054300030822     C                   IF        PiInt=*on
054400030822     C                   Z-ADD     PiVal         VABCCM
054500030822     C                   ENDIF
054600030822     C                   ENDIF
054700030822     C* LNP
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'LNP'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABLNP
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* NRS
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'NRS'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABNRS
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* CTR
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'CTR'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABCTR
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* PKB
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'PKB'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiNum=*on
059600030822     C                   Z-ADD     PiVal         VABPKB
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* VLB
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'VLB'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiNum=*on
060900030822     C                   Z-ADD     PiVal         VABVLB
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* QFT
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'QFT'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiNum=*on
062200030822     C                   Z-ADD     PiVal         VABQFT
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* CBO
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'CBO'
063000030822     C                   EVAL      VABCBO=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   ENDIF
063400030822     C* TSP
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'TSP'
063900030822     C                   EVAL      VABTSP=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   ENDIF
064300030822     C* VAS
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'VAS'
064800030822     C                   EVAL      VABVAS=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200030822     C* VCA
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'VCA'
065700030822     C                   EVAL      VABVCA=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* GCA
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'GCA'
066600030822     C                   EVAL      VABGCA=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* CTM
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'CTM'
067500030822     C                   EVAL      VABCTM=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* FFD
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'FFD'
068400030822     C                   EVAL      VABFFD=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* VAD
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'VAD'
069300030822     C                   EVAL      VABVAD=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* GMA
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'GMA'
070200030822     C                   EVAL      VABGMA=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* GGA
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'GGA'
071100030822     C                   EVAL      VABGGA=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* GVA
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'GVA'
072000030822     C                   EVAL      VABGVA=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* TC1
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'TC1'
072900030822     C                   EVAL      VABTC1=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* TC2
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'TC2'
073800030822     C                   EVAL      VABTC2=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200040714     C* VATTRC
074300040714     C                   IF        %subst(
074400040714     C                             %subst(vlrppt:posDaDft+1:
074500040714     C                             posADft-posDaDft-1):1:3)
074600040714     C                             = 'TRC'
074700040714     C                   EVAL      VATTRC=%trim(%subst(
074800040714     C                             %subst(vlrppt:posDaDft+1:
074900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000040714     C                   ENDIF
075100030822     C* ...
075200030822     C                   ENDIF
075300030822     C                   ENDDO
075400020204     C*
075500010330     C                   ENDSR
075600010607     C*----------------------------------------------------*
075700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075800010607     C*----------------------------------------------------*
075900010607     C     IMPVAB        BEGSR
076000010607     C*
076100010607     C                   SETOFF                                       3132
076200030515     C*
076300010607     C                   MOVEL     datcor        VABAAS
076400010607     C                   MOVE      datcor        VABMGS
076500010607     C*
076600030715     C**********
076700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076800030715     C**********
076900030715     C                   IF        wGiro = *zeros
077000040113     C*
077100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
077200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
077300040510     C                             %trim(intcol(3))<>*blanks
077400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077500040113     C                                      %trim(intcol(3))
077600040510     C                   ENDIF
077700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077800031201     C     minu:maiu     XLATE     vindta        vindta
077900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078000030715     C                   DOW       posDa <= %len(%trim(vindta))
078100030715     C*
078200030715     C* Gestisco il 1° campo
078300030715     C                   IF        i = 1
078400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078500030715     C                   EVAL      posDa = 1
078600030715     C* Eseguo lo scan x trovare la fine del primo campo
078700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078800030715     C* A questo punto "estraggo" il campo corrente
078900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079000030715     C                                                    (posA-posDa))
079100030715     C* X i campi successivi al 1°
079200030715     C                   ELSE
079300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079400030715     C                   EVAL      posDa = posA + 1
079500030715     C* Eseguo lo scan x trovare la fine del campo corrente
079600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079700030715     C* Gestisco l'ultimo campo
079800030715     C                   IF        posA = *zeros
079900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C                   LEAVE
080400030715     C                   ELSE
080500030715     C* A questo punto "estraggo" il campo corrente
080600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080700030715     C                                                    (posA-posDa))
080800030715     C                   ENDIF
080900030715     C                   ENDIF
081000030715     C* Incremento il contatore d campo
081100030715     C                   EVAL      i = i +1
081200030715     C                   ENDDO
081300030715     C*
081400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081500030715     C* delimitazione testo
081600030715     C                   EVAL      i = 1
081700030715     C                   DOW       i < %elem(SkSplitFLD)
081800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082000030715     C                   EVAL      i = i + 1
082100030715     C                   ENDDO
082200030715     C                   EVAL      wGiro = 1
082300030715     C                   ELSE
082400030715     C                   EVAL      wGiro = 2
082500031204     C**********
082600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082700031204     C* d separatore campo
082800031204     C**********
082900031204     C                   EVAL      vindta = %trim(vindta)
083000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083100031204     C                   EVAL      vindta = %subst(vindta:2)
083200031204     C                   ENDDO
083300031204     C*
083400031204     C                   Z-ADD     *zeros        lunghInput        4 0
083500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083900031204     C                   ENDDO
084000030715     C**********
084100030715     C* Effettuo lo split del campo dati d input
084200030715     C**********
084300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084400030715     C                   DOW       posDa <= %len(%trim(vindta))
084500030715     C*
084600030715     C* Gestisco il 1° campo
084700030715     C                   IF        i = 1
084800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084900030715     C                   EVAL      posDa = 1
085000030715     C* Eseguo lo scan x trovare la fine del primo campo
085100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085200030715     C* A questo punto "estraggo" il campo corrente
085300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085400030715     C                                                    (posA-posDa))
085500030715     C* X i campi successivi al 1°
085600030715     C                   ELSE
085700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085800030715     C                   EVAL      posDa = posA + 1
085900031204     C* Verifico che nn vi sia il campo nn valorizzato
086000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086100031204     C* Se campo nn valorizzato skippo al prossimo
086200031204     C                   EVAL      SkSplitCSV(i) = *blanks
086300031204     C                   EVAL      posA  = posA + 1
086400031204     C                   ELSE
086500030715     C* Eseguo lo scan x trovare la fine del campo corrente
086600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086700030715     C* Gestisco l'ultimo campo
086800030715     C                   IF        posA = *zeros
086900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C                   LEAVE
087400030715     C                   ELSE
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C                   ENDIF
087900031204     C                   ENDIF
088000030715     C                   ENDIF
088100030715     C* Incremento il contatore d campo
088200030715     C                   EVAL      i = i +1
088300030715     C                   ENDDO
088400030715     C*
088500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088600030715     C* delimitazione testo
088700030715     C                   EVAL      i = 1
088800030715     C                   DOW       i < %elem(SkSplitCSV)
088900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089100030715     C                   EVAL      i = i + 1
089200030715     C                   ENDDO
089300030715     C*
089400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089500030715     C                   EVAL      i = 1
089600030715     C                   DOW       i < %elem(SkSplitFLD)
089700030715     C*
089800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090000030715     C                             SkSplitCSV(i) <> *blanks
090100030721     C*** ATB
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090300030721     C                   EVAL      VABATB = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030715     C*** RSD
090600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091000030715     C* ==
091100030715     C                   ENDIF
091200030721     C*** RD2
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030715     C*** IND
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091800030715     C                   EVAL      VABIND = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** LOD
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030715     C*** PRD
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030721     C*** NZD
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030715     C*** RMA
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030715     C*** NOT
093700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093900030715     C                   ENDIF
094000030715     C*** NT2
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094300030715     C                   ENDIF
094400030715     C*** VCA
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094700030715     C                   ENDIF
094800030715     C*** VAS
094900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095100030715     C                   ENDIF
095200030715     C*** FFD
095300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095500030715     C                   ENDIF
095600030721     C*** GC1
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** GC2
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** TSP
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096700030721     C                   ENDIF
096800040510     C*** CBO
096900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
097000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
097100040510     C                   ENDIF
097200030721     C*** NAS
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** TIC
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** GCA
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** XCO
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** CTM
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** TCR
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** CTS
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** FTM
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** VAD
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** GMA
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** GGA
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** GVA
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TC1
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** TC2
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** SCL
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** RMO
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** NMO
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*
104100030721     C* Reperisco quindi i campi numerici...
104200040510     C*** NSP
104300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104400040510     C                   EVAL      PiStr=SkSplitCSV(i)
104500040510     C                   EXSR      CHKNUM
104600040510     C                   IF        PiInt=*on
104700040510     C                   Z-ADD     PiVal         VABNSP
104800040510     C                   ELSE
104900040510     C                   SETON                                        31
105000040510     C                   EVAL      VABNSP = *zeros
105100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
105200040510     C                             + ' ' + 'VABNSP'
105300040510     C                   ENDIF
105400040510     C                   ENDIF
105500030721     C*** CAD
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
105700030721     C                   EVAL      PiStr=SkSplitCSV(i)
105800030721     C                   EXSR      CHKNUM
105900030721     C                   IF        PiInt=*on
106000030721     C                   Z-ADD     PiVal         Num5_0
106100030721     C                   MOVEL(p)  Num5_0        VABCAD
106200030721     C                   ELSE
106300030721     C                   SETON                                        32
106400030721     C                   EVAL      VABCAD = *zeros
106500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106600030721     C                             + ' ' + 'VABCAD'
106700030721     C                   ENDIF
106800030721     C                   ENDIF
106900030721     C*** NCL
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107100030721     C                   EVAL      PiStr=SkSplitCSV(i)
107200030721     C                   EXSR      CHKNUM
107300030721     C                   IF        PiInt=*on
107400030721     C                   Z-ADD     PiVal         VABNCL
107500030721     C                   ELSE
107600030721     C                   SETON                                        32
107700030721     C                   EVAL      VABNCL = *zeros
107800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107900040122     C                             + ' ' + 'VABNCL'
108000030721     C                   ENDIF
108100030721     C                   ENDIF
108200030721     C*** PKB
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
108400030721     C                   EVAL      PiStr=SkSplitCSV(i)
108500030721     C                   EXSR      CHKNUM
108600030721     C                   IF        PiNum=*on
108700030721     C                   Z-ADD     PiVal         VABPKB
108800030721     C                   ELSE
108900030721     C                   SETON                                        32
109000030721     C                   EVAL      VABPKB = *zeros
109100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109200030721     C                             + ' ' + 'VABPKB'
109300030721     C                   ENDIF
109400030721     C                   ENDIF
109500030721     C*** CAS
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
109700030721     C                   EVAL      PiStr=SkSplitCSV(i)
109800030721     C                   EXSR      CHKNUM
109900030721     C                   IF        PiNum=*on
110000030721     C                   Z-ADD     PiVal         VABCAS
110100030721     C                   ELSE
110200030721     C                   SETON                                        32
110300030721     C                   EVAL      VABCAS = *zeros
110400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110500030721     C                             + ' ' + 'VABCAS'
110600030721     C                   ENDIF
110700030721     C                   ENDIF
110800030721     C*** CCM
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiInt=*on      AND
111300030721     C                             PiVal<=9999999 AND
111400030721     C                             PiVal>*zeros
111500030721     C                   Z-ADD     PiVal         VABCCM
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      VABCCM = *zeros
111900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112000040510     C                             + ' ' + 'VABCCM'
112100030721     C                   ENDIF
112200030721     C                   ENDIF
112300030721     C*** LNP
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
112500030721     C                   EVAL      PiStr=SkSplitCSV(i)
112600030721     C                   EXSR      CHKNUM
112700030721     C                   IF        PiInt=*on    AND
112800030721     C                             PiVal<=999   AND
112900030721     C                             PiVal>*zeros
113000030721     C                   Z-ADD     PiVal         VABLNP
113100030721     C                   ELSE
113200040715     C                   SETON                                        32
113300030721     C                   EVAL      VABLNP = *zeros
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500040510     C                             + ' ' + 'VABLNP'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** NRS
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiInt=*on AND
114300030721     C                             PiVal<=99
114400030721     C                   Z-ADD     PiVal         VABNRS
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800030721     C                             + ' ' + 'VABNRS'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** LNA
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on    AND
115600030721     C                             PiVal<=999   AND
115700030721     C                             PiVal>*zeros
115800030721     C                   Z-ADD     PiVal         VABLNA
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      VABLNA = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABLNA'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** CTR
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on  AND
117100030721     C                             PiVal<=999
117200030721     C                   Z-ADD     PiVal         VABCTR
117300030721     C                   ELSE
117400030721     C                   SETON                                        32
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABCTR'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** IAS
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiNum=*on
118400030721     C                   Z-ADD     PiVal         VABIAS
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABIAS'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** VLB
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiNum=*on
119600030721     C                   Z-ADD     PiVal         VABVLB
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABVLB'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** QFT
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiNum=*on
120800030721     C                   Z-ADD     PiVal         VABQFT
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABQFT'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** NCD
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on      AND
122000030721     C                             PiVal<=9999999
122100030721     C                   Z-ADD     PiVal         VABNCD
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABNCD'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** NCA
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiInt=*on      AND
123300030721     C                             PiVal<=9999999
123400030721     C                   Z-ADD     PiVal         VABNCA
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABNCA'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** ZNC
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on AND
124600030721     C                             PiVal<=99
124700030721     C                   Z-ADD     PiVal         VABZNC
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABZNC'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** DCR
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiInt=*on       AND
125900030721     C                             PiVal<=99999999
126000030721     C                   Z-ADD     PiVal         VABDCR
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABDCR'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** HCR
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on   AND
127200030721     C                             PiVal<=9999
127300030721     C                   Z-ADD     PiVal         VABHCR
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABHCR'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** VMD
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiNum=*on
128500030721     C                   Z-ADD     PiVal         VABVMD
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABVMD'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** ANT
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on        AND
129700030721     C                             PiVal<=999999999
129800030721     C                   Z-ADD     PiVal         VABANT
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABANT'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** CMO
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on
131000030721     C                   Z-ADD     PiVal         Num5_0
131100030721     C                   MOVEL(p)  Num5_0        VABCMO
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      VABCMO = *zeros
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABCMO'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900040510     C*
132000040510     C* Reperisco i campi del FIVAT
132100040510     C*** VATNOT
132200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
132300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
132400040510     C                   ENDIF
132500040510     C*** VATTRC
132600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
132700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
132800040510     C                   ENDIF
132900060331     C*** VATNOT_A
133000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
133100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
133200060331     C                   ENDIF
133300060331     C*** VATNOT_B
133400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
133500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
133600060331     C                   ENDIF
133700060331     C*** VATNOT_E
133800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
133900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
134000060331     C                   ENDIF
134100040510     C*
134200030715     C                   ENDIF
134300030715     C                   EVAL      i = i + 1
134400030715     C                   ENDDO
134500040510     C*
134600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
134700040510     C                   IF        VABNSP = *zeros
134800040510     C                   SETON                                        31
134900040510     C                   ENDIF
135000040715     C*
135100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
135200040715     C                   IF        VABLNP = *zeros
135300040715     C                   SETON                                        31
135400040715     C                   ENDIF
135500040510     C*
135600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
135700040510     C                   IF        VABRMN = *zeros
135800040510     C                   Z-ADD     VABNSP        VABRMN
135900040510     C                   ENDIF
136000040510     C*
136100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
136200040510     C                   IF        VABRMA = *blanks
136300040714     C                   MOVEL     VABRMN        VABRMA
136400040510     C                   ENDIF
136500040128     C*
136600040510     C* Se provincia nn valorizzata la reperisco
136700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
136800040510     C                   IF        VABPRD = *blanks
136900040128     C                   CLEAR                   TISI95DS
137000040128     C                   EVAL      I95TCN = '3'
137100040128     C                   Z-ADD     datcor        I95DAT
137200040128     C                   EVAL      I95NAR = VABNZD
137300040128     C                   EVAL      I95CAP = VABCAD
137400040128     C                   EVAL      I95LOC = VABLOD
137500040128     C                   CALL      'TISI95R'
137600040128     C                   PARM                    TISI95DS
137700040128     C                   EVAL      VABPRD = O95PRV
137800040510     C                   ENDIF
137900040113     C*
138000040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
138100040113     C                   IF        VABCAS > *zeros
138200040119     C                   EVAL      VABVCA = 'EUR'
138300040119     C                   IF        VABCBO = '1'
138400040113     C                   EVAL      VABCBO = '4'
138500040119     C                   ENDIF
138600040119     C                   IF        VABCBO = '2'
138700040119     C                   EVAL      VABCBO = '6'
138800040119     C                   ENDIF
138900040113     C                   ENDIF
139000020204     C*
139100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
139200020204     C                   EXSR      CHKIMPDIV
139300030715     C                   ENDIF
139400020204     C*
139500010607     C                   ENDSR
139600010607     C*----------------------------------------------------*
139700010601
139800020204
139900020204     C*----------------------------------------------------*
140000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
140100020204     C*----------------------------------------------------*
140200020204     C     CHKIMPDIV     BEGSR
140300020204     C*
140400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
140500020204     C                   Z-ADD     *zeros        wrkDec            9 9
140600020204     C*
140700020204     C* Come prima cosa effettuo considerazioni sulla divisa
140800020204     C                   IF        vabIAS > *zeros
140900020204     C                   IF        vabVAS <> 'EUR'
141000020204     C                   EVAL      vabVAS =  'ITL'
141100020204     C                   ENDIF
141200020204     C                   ENDIF
141300020204     C*
141400020204     C                   IF        vabCAS > *zeros
141500020204     C                   IF        vabVCA <> 'EUR'
141600020204     C                   EVAL      vabVCA =  'ITL'
141700020204     C                   ENDIF
141800020204     C                   ENDIF
141900020204     C*
142000020204     C                   IF        vabVMD > *zeros
142100020204     C                   IF        vabVAD <> 'EUR'
142200020204     C                   EVAL      vabVAD =  'ITL'
142300020204     C                   ENDIF
142400020204     C                   ENDIF
142500020204     C*
142600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
142700020204     C                   Z-ADD     vabIAS        wrkDec
142800020204     C                   IF        wrkDec > *zeros
142900020204     C                   IF        vabVAS = 'ITL'
143000020204     C                   EVAL      vabIAS = *zeros
143100020204     C                   ENDIF
143200020204     C                   ENDIF
143300020204     C*
143400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
143500020204     C                   Z-ADD     vabCAS        wrkDec
143600020204     C                   IF        wrkDec > *zeros
143700020204     C                   IF        vabVCA = 'ITL'
143800020204     C                   EVAL      vabCAS = *zeros
143900020204     C                   ENDIF
144000020204     C                   ENDIF
144100020204     C*
144200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
144300020204     C                   Z-ADD     vabVMD        wrkDec
144400020204     C                   IF        wrkDec > *zeros
144500020204     C                   IF        vabVAD = 'ITL'
144600020204     C                   EVAL      vabVMD = *zeros
144700020204     C                   ENDIF
144800020204     C                   ENDIF
144900020204     C*
145000020204     C                   ENDSR
145100020204     C***
145200020204
145300010330
145400010330
145500010330     C*----------------------------------------------------*
145600010330     C*  CONTROLLO NUMERICITA' CAMPI
145700010330     C*----------------------------------------------------*
145800010330     C     CHKNUM        BEGSR
145900010330     C*
146000010606     C                   IF        PiDecChr = *blanks
146100030715     C                   EVAL      PiDecChr = CharNUM
146200010606     C                   ENDIF
146300010606     C*
146400010606     C                   CALL(e)   'ISNUMERIC'
146500010330     C                   PARM                    PiStr            30
146600010606     C                   PARM                    PiDecChr          1
146700010330     C                   PARM      *ZEROS        PiVal            30 9
146800010330     C                   PARM      '0'           PiInt             1
146900010330     C                   PARM      '0'           PiNum             1
147000010330     C                   IF        %error
147100010606     C                   EVAL      PiNum=*off
147200010330     C                   ENDIF
147300010330     C*
147400010330     C                   ENDSR
147500010330     C***
147600010330
147700010601
147800010601
147900010601
148000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
148100010601     C     repfil        BEGSR
148200010601     C*
148300010601     C                   if        invfil = *zeros and
148400010601     C                             depfil > *zeros and
148500010629     C                             (vinflg = *blanks or
148600010629     C                              vinflg = *zeros)
148700010601     C
148800010601     C                   eval      invfil = depfil
148900010601     C                   endif
149000010601     C*
149100010601     C                   if        depfil <> invfil and
149200010601     C                             invfil > *zeros
149300010601     C                   eval      flgMulti = '1'
149400010601     C                   if        vinflg = *blanks
149500010601     C                   add       1             cntNonEl
149600010601     C                   endif
149700010601     C                   endif
149800010601     C*
149900010601     C                   if        vinflg = '2'
150000010601     C                   eval      flgStato = '2'
150100010601     C                   endif
150200010601     C*
150300010601     C                   ENDSR
150400010601     C***
150500010601
150600010601
150700010601
150800010330
150900040119      /TITLE Invio dei dati al punto operativo.
151000040119     C     invio         BEGSR
151100040119     C*
151200040119     C* 1° invio FIVAT
151300040119     C                   reset                   dscmz
151400040119     C                   move      vlrpoi        cmzdst
151500040119     C                   eval      cmzfld = 'FIVATWWR'
151600040119     C                   eval      cmzmbd = vlrhdl
151700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
151800040119     C***                if        prmfir = *blanks
151900040119     C                   eval      cmzfla = 'FIVAT00F'
152000040119     C                   eval      cmzmba = 'FIVAT00F'
152100040119     C***                else
152200040119     C***                eval      cmzfla = prmfir
152300040119     C***                eval      cmzmba = prmfir
152400040119     C***                endif
152500040119     C                   eval      cmznrr = *zeros
152600040119     C                   move      §ctrokvt      cmznrr
152700040119     C                   eval      cmzlba = vlrfl1
152800040119     C                   call(e)   'TIS711C'
152900040119     C                   parm                    dscmz
153000040119     C                   parm      *blanks       esito
153100040119     C                   if        %error
153200040119     C                             or cmzerr = '1'
153300040119     C                             or esito  = '1'
153400040119     C                   eval      wrkesito = '3'
153500040119     C                   else
153600040119     C*
153700040119     C* 2° invio FIVAB
153800040119     C                   reset                   dscmz
153900040119     C                   move      vlrpoi        cmzdst
154000040119     C                   eval      cmzfld = vlrfou
154100040119     C                   eval      cmzmbd = vlrhdl
154200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
154300040119     C***                if        prmfir = *blanks
154400040119     C                   eval      cmzfla = 'FIVAB00F'
154500040119     C                   eval      cmzmba = 'FIVAB00F'
154600040119     C***                else
154700040119     C***                eval      cmzfla = prmfir
154800040119     C***                eval      cmzmba = prmfir
154900040119     C***                endif
155000040119     C                   eval      cmznrr = *zeros
155100040119     C                   move      §ctrokvb      cmznrr
155200040119     C                   eval      cmzlba = vlrfl1
155300040119     C                   call(e)   'TIS711C'
155400040119     C                   parm                    dscmz
155500040119     C                   parm      *blanks       esito
155600040119     C                   if        %error
155700040119     C                             or cmzerr = '1'
155800040119     C                             or esito  = '1'
155900040119     C                   eval      wrkesito = '3'
156000040119     C                   endif
156100040119     C                   endif
156200040119     C*
156300040119     C                   ENDSR
156400040119     C***
156500010601
156600010601
156700010601
156800010601
156900010601      /TITLE Invio dei dati al punto operativo.
157000010601     C     opeini        BEGSR
157100010601     C*
157200010601     C* Inizializzo flag e contatori operativi
157300010601     C                   movel     '0'           flgGiro           1
157400010601     C                   movel     '0'           flgMulti          1
157500010601     C                   movel     '1'           flgStato          1
157600010615     C                   movel     '0'           flgOk             1
157700010601     C                   z-add     *zeros        cntNonEl         10 0
157800010601     C                   z-add     *zeros        depfil            3 0
157900010601     C                   z-add     *zeros        invfil            3 0
158000010601     C*
158100010601     C                   ENDSR
158200010601     C***
158201070326
158202070326
159101070326
159102070326
159103070326     C     *pssr         BEGSR
159104070329     C*
159106070329     C                   if        %open(tivin00r)
159107070329     C                   close     tivin00r
159108070329     C                   endif
159109070329     C                   if        %open(fivabwwr)
159110070329     C                   close     fivabwwr
159111070329     C                   endif
159112070329     C                   if        %open(fivatwwr)
159113070329     C                   close     fivatwwr
159114070329     C                   endif
159116070326     C*
159117070326     C* Effettuo la chiamata al CLLE preposto
159118070326     C                   call(e)   'TITVVTC'
159119070326     C                   parm                    parccm
159120070326     C                   parm                    parmbr
159121070326     C                   parm      '2'           paropz
159122070326     C*
159123070326     C                   eval      wrkesito = '2'
159124070404     C*
159125070404     C                   seton                                        LR
159126070326     C*
159127070326     C                   ENDSR     '*CANCL'
159128070326     C***
159129070326
159130070326
159400010330
159500010330
159600000613     C     *inzsr        BEGSR
159700990910     C*
159800990910     C     *entry        plist
159900990920     C                   parm                    tivlrds
160000990921     C                   parm      wrkesito      esito
160100000724     C                   parm                    prmlit
160200000710     C                   parm                    prmfir
160300010330     C*
160400010330     C* CALCOLA LA DATA CORRENTE
160500010330     C                   time                    wn14             14 0
160600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
160700010330     C                   z-add     wn8           g08dat
160800010330     C                   z-add     *zeros        g08inv
160900010330     C                   movel     '0'           g08err
161000010330     C                   call      'XSRDA8'
161100010330     C                   parm                    wlbda8
161200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
161300000613     C*
161400000613     C                   ENDSR
161500000613     C***
161600990908
161700070405     Otitvy8p   E            riepilogo         2
161800990915     O                                              'Upload via Internet'
161900990915     O                                           +1 'Traduzione TIVIN00R -'
162000021025     O                                           +1 'FIVABWWR'
162100990915     O                                           +1 'Testate bolle'
162200990915     O          E            riepilogo   2
162300990915     O                       wrkdata
162400990915     O                       wrkora              +1
162500990915     O                       procname            +1
162600990915     O          E            riepilogo   2
162700990915     O                                              'Cliente..................:'
162800010605     O                       VABCCM        z     +1
162900990915     O          E            riepilogo   2
163000990920     O                                              'Riferimento Strategi.....:'
163100990920     O                       vlrhdl              +1
163200990915     O          E            riepilogo   2
163300990915     O                                              'Giusti...................:'
163400040119     O                       §CTROKVB      2   +  1
163500990915     O          E            riepilogo   2
163600990915     O                                              'Sbagliati e corretti.....:'
163700971022     O                       §CTRMO        2   +  1
163800990915     O          E            riepilogo   2
163900990915     O                                              'Sbagliati e scartati.....:'
164000971022     O                       §CTRNO        2   +  1
164100000613
164200070405     Otitvy8ps  E            testdett          2
164300000613     O                                              'Upload via Internet'
164400000613     O                                           +1 'Traduzione TIVIN00R -'
164500021025     O                                           +1 'FIVABWWR'
164600000613     O                                           +1 'Testate bolle'
164700000616     O          E            testdett    3
164800000613     O                                           +2 'N° rec'
164900000613     O                                           +3 'Anteprima contenuto'
165000000616     O          E            rigadett    2
165100000613     O                       rrnum               +2
165200000621     O                       recko               +3
165300000616     O          E            findett     2
165400000613     O                       wrkdata
165500000613     O                       wrkora              +1
165600000613     O                       procname            +1
165700000616     O          E            findett     2
165800000613     O                                              'Cliente..................:'
165900010605     O                       VABCCM        z     +1
166000000616     O          E            findett     2
166100000613     O                                              'Riferimento Strategi.....:'
166200000613     O                       vlrhdl              +1
166300000616     O          E            findett     2
166400000613     O                                              'Giusti...................:'
166500040119     O                       §CTROKVB      2   +  1
166600000616     O          E            findett     2
166700000613     O                                              'Sbagliati e corretti.....:'
166800000613     O                       §CTRMO        2   +  1
166900000616     O          E            findett     2
167000000613     O                                              'Sbagliati e scartati.....:'
167100000613     O                       §CTRNO        2   +  1
167200000613** CMD - COMANDI CL
167300070405OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
167400070405OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
167500070405DLTOVR FILE(TITVY8P TITVY8PS) LVL(*)
167600010606
167700000613
167800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
167900070405VABCCM;VABNSP;VABCBO;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_B;VABNCL;
168000070405VABPKB;VABCAS;VABIAS;VABVAS;VATNOT_E;VABTIC;VABTC1;VABFFD;VABNOT
168100040113
