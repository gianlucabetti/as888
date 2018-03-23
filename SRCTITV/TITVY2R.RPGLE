000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800070301     Ftitvy2p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070301     Ftitvy2ps  O    f  198        PRINTER usropn
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
003000040128     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040510     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000030715     D CharCSV         S              1
005100030715     D CharTXT         S              1
005200030715     D CharNUM         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700060331     D wVATNOT_A       s                   LIKE(VATNOT)
005800060331     D wVATNOT_B       s                   LIKE(VATNOT)
005900060331     D wVATNOT_E       s                   LIKE(VATNOT)
006000060331     D wVABNSP         s                   LIKE(VABNSP) INZ
006100030822     D*------------------
006200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006300030822     D*------------------
006400030822     D posDaDft        S              3  0 INZ(*zeros)
006500030822     D posADft         S              3  0 INZ(*zeros)
006600030822     D j               s              3  0 INZ(1)
006700030822     D wGiroDft        s              1  0 INZ(*zeros)
006800010330     D*------------------
006900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007000010330     D*------------------
007100010330     D WLBDA8          DS                  INZ
007200010330     D  G08DAT                 1      8  0
007300010330     D  G08INV                 9     16  0
007400010330     D  G08ERR                17     17
007500010330     D  G08TGI                18     22  0
007600031201     D*------------------
007700031201     D* Costanti
007800031201     D*------------------
007900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008100010330
008200010330
008300010330
008400990915     C                   time                    wrkdata
008500990915     C                   time                    wrkora
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800990921     C                   reset                   wrkesito
008900010601     C*
009000010601     C                   exsr      opeini
009100010605     C                   exsr      rwvab
009200010601     C*
009300010601     C                   seton                                        lr
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
013400070301     C                   if        not %open(titvy2ps)
013500070301     C                   open      titvy2ps
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
015300040119     C*
015400040119     C* Effettuo la chiamata al CLLE preposto
015500040119     C                   call(e)   'TITVVTC'
015600040510     C                   parm                    parccm
015700040119     C                   parm                    parmbr
015800040119     C                   parm      '2'           paropz
015900050201     C*
016000050201     C* Effettuo lancio TISI95 solo x chiusura
016100050201     C                   CLEAR                   TISI95DS
016200050201     C                   EVAL      I95TLA = 'C'
016300050201     C                   CALL      'TISI95R'
016400050201     C                   PARM                    TISI95DS
016500000613     C*
016600070301     C                   if        %open(titvy2ps)
016700000616     C                   except    findett
016800070301     C                   close     titvy2ps
016900000613     C                   endif
017000000616     C*
017100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017200011113     C                   Z-ADD     150           LENGH            15 5
017300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017400000616     C                   PARM                    CMD(3)
017500000616     C                   PARM                    LENGH
017600000616     C*
017700010601     C                   ENDSR
017800010601     C***
017900010601
018000010601
018100010601
018200000613
018300000613     C*--------------------------------------------------------
018400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018500000613     C*--------------------------------------------------------
018600000621     C     STPR          BEGSR
018700000613     C*
018800070301     C                   if        not %open(titvy2p)
018900070301     C   50              open      titvy2p
019000990915     C                   endif
019100990915     C*
019200030715     C   50              except    riepilogo
019300990915     C*
019400070301     C                   if        %open(titvy2p)
019500070301     C   50              close     titvy2p
019600990914     C                   endif
019700000613     C*
019800000613     C                   ENDSR
019900000613     C***
020000000613
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
021400040119     C*
021500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021600040119     C                   exsr      prevat
021700040119     C*
021800040119     C                   if        chkcall = '0'
021900040119     C*
022000040119     C                   if        not %open(fivatwwr)
022100040119     C                   open      fivatwwr
022200040119     C                   endif
022300010330     C*
022400040119     C                   clear                   §CTROKVB          7 0
022500040119     C                   clear                   §CTROKVT          7 0
022600040119     C                   clear                   §CTRMO            7 0
022700040119     C                   clear                   §CTRNO            7 0
022800010330     C*
022900030822     C                   do        *HIVAL
023000010330     C*
023100030822     C                   read      tivin00r                               70
023200010618     C*
023300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023400010618     C                   if        vinflg = '1'
023500010618     C                   eval      flgOk = '1'
023600010618     C                   endif
023700040510     C*
023800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
023900040510     C                   z-add     *zeros        wLenVINDTA        4 0
024000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024400040510     C                   z-add     1             wFlgVINDTA
024500040510     C                   leave
024600040510     C                   endif
024700040510     C                   enddo
024800010618     C*
024900040510     C                   if        vindta > *blanks AND
025000040510     C                             wFlgVINDTA = 1
025100010330     C                   add       1             rrnum
025200010330     C*
025300010601     C                   if        *in70 = *off and
025400010330     C                             (vinflg = *blanks
025500010330     C                              or vinflg = '0'
025600010330     C                              or vinflg = '2')
025700010330     C*
025800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025900010711     C                   if        vinflg = *blanks or vinflg = '0'
026000010711     C                   clear                   vinmsg
026100010711     C                   endif
026200010601     C*
026300010330     C                   exsr      inzvar
026400010605     C                   exsr      impvab
026500010601     C*
026600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026700010605     C                   eval      depfil = VABLNP
026800010601     C                   exsr      repfil
026900010601     C                   if        depfil = invfil
027000021025     C                   if        vlrpoi = 999
027100030822     C                   move(P)   invfil        VABFGS
027200021025     C                   else
027300030822     C                   move(P)   vlrpoi        VABFGS
027400021025     C                   endif
027500010601     C*
027600010601     C                   exsr      PREELA
027700010601     C*
027800010604     C* Ebbene...
027900010604     C*
028000040119     C  N31              add       1             §CTROKVB
028100040119     C   32              add       1             §CTRMO
028200040119     C   31              add       1             §CTRNO
028300030822     C                   if        wGiro = 2
028400040510     C* Scrivo sempre il VAT
028500040510     C  N31              exsr      wrivat
028600040510     C* Scrivo il VAB solo a rottura d spedizione
028700040119     C                   if        depspe =  *zeros OR
028800040119     C                             depspe <> vabNSP
028900040119     C  N31              exsr      wrivab
029000040119     C  N31              eval      depspe = vabNSP
029100040119     C                   endif
029200040119     C                   endif
029300010604     C*
029400010604     C                   if        *in31 = *off and
029500010604     C                             *in32 = *off
029600010604     C                   eval      vinflg = '1'
029700010604     C                   else
029800010604     C                   eval      recko = vindta
029900010604     C                   except    rigadett
030000010604     C                   eval      vinflg = '2'
030100010604     C                   endif
030200010604     C                   endif
030300010601     C*
030400010604     C                   endif
030500010604     C*
030600010330     C                   else
030700010330     C                   eval      vinflg = '1'
030800010330     C                   endif
030900010601     C*
031000010601     C  N70              update    tivin000
031100010330     C*
031200030822     C  N70              enddo
031300040119     C*
031400040119     C                   endif                                                  (endif - chkcall)
031500010601     C*
031600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031700010601     C                   if        cntNonEl = *zeros or
031800010601     C                             flgMulti = '0'
031900010330     C* Se non ci sono record con errori ...
032000010601     C                   if        §ctrno = 0 and
032100010604     C                             §ctrmo = 0 and
032200010601     C                             flgStato <> '2'
032300010330     C* ... restituisco esito OK.
032400010330     C                   eval      wrkesito = '0'
032500010330     C                   else
032600040119     C                   if        §ctrokvb > 0
032700010330     C                   eval      wrkesito = '1'
032800010330     C                   else
032900010615     C                   if        flgOk = '0'
033000010615     C                   eval      wrkesito = '2'
033100010615     C                   else
033200010615     C                   eval      wrkesito = '6'
033300010615     C                   endif
033400010330     C                   endif
033500010330     C                   endif
033600010601     C                   else
033700010601     C                   eval      wrkesito = '9'
033800010601     C                   endif
033900010330     C*
034000010330     C                   if        %open(tivin00r)
034100010330     C                   close     tivin00r
034200010330     C                   endif
034300021025     C                   if        %open(fivabwwr)
034400021025     C                   close     fivabwwr
034500010330     C                   endif
034600040512     C                   if        %open(fivatwwr)
034700040512     C                   close     fivatwwr
034800040512     C                   endif
034900010601     C*
035000010601     C                   if        vlrpoi <> 999
035100010601     C                   eval      invfil = vlrpoi
035200010601     C                   endif
035300010330     C*
035400040119     C                   if        §ctrokvb > 0
035500010601     C                             and invfil > *zeros
035600010330     C                   exsr      invio
035700010330     C                   endif
035800010601     C*
035900010618     C                   if        flgGiro = '1'
036000010601     C                   exsr      endela
036100010618     C                   endif
036200010330     C*
036300010330     C                   ENDSR
036400010330     C***
036500040119
036600040119
036700040119
036800040119     C*----------------------------------------------------*
036900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037000040119     C*----------------------------------------------------*
037100040510     C     WRIVAB        BEGSR
037200040119     C*
037300060331     C* Gestisco l'eventuale rottura x numero spedizione
037400060331     C                   if        VABNSP <> wVABNSP
037500070301     C                   if        wVATNOT_E <> *blanks
037600070301     C                   eval      VABCTM = '7Q'
037700070301     C                   endif
037800040119     C                   write     fivab000                                     => scarico il VAB
037900060331     C                   eval      wVABNSP = VABNSP
038000060331     C                   endif
038100040119     C*
038200040119     C                   ENDSR
038300040119
038400040119
038500040119
038600040119     C*----------------------------------------------------*
038700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038800040119     C*----------------------------------------------------*
038900040510     C     WRIVAT        BEGSR
039000060512     C*
039100060512     C* Inizializzo i campi chiave
039200060512     C                   eval      VATFGS = VABFGS
039300060512     C                   eval      VATAAS = VABAAS
039400060512     C                   eval      VATCCM = VABCCM
039500060512     C                   eval      VATNSP = VABNSP
039600060512     C                   eval      VATLNP = VABLNP
039700040714     C*
039800040714     C* Solo se VATNOT valorizzato scarico il buffer
039900040714     C                   if        VATNOT <> *blanks
040000040119     C*
040100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040200040510     C                   if        VATTRC = *blanks
040300040510     C                   eval      VATTRC = 'E'
040400040510     C                   endif
040500040510     C                   write     FIVAT000
040600040119     C                   add       1             §CTROKVT
040700040714     C*
040800040714     C                   endif
040900060331     C*
041000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041100060331     C                   if        wVATNOT_A <> *blanks
041200060331     C                   eval      VATNOT = wVATNOT_A
041300060331     C                   eval      VATTRC = 'A'
041400060331     C                   write     FIVAT000
041500060331     C                   add       1             §CTROKVT
041600060331     C                   endif
041700060331     C*
041800060331     C                   if        wVATNOT_B <> *blanks
041900060331     C                   eval      VATNOT = wVATNOT_B
042000060331     C                   eval      VATTRC = 'B'
042100060331     C                   write     FIVAT000
042200060331     C                   add       1             §CTROKVT
042300060331     C                   endif
042400060331     C*
042500060331     C                   if        wVATNOT_E <> *blanks
042600060331     C                   eval      VATNOT = wVATNOT_E
042700060331     C                   eval      VATTRC = 'E'
042800060331     C                   write     FIVAT000
042900060331     C                   add       1             §CTROKVT
043000060331     C                   endif
043100040119     C*
043200040119     C                   ENDSR
043300040119
043400040119
043500040119
043600040119     C*----------------------------------------------------*
043700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043800040119     C*----------------------------------------------------*
043900040119     C     PREVAT        BEGSR
044000040119     C*
044100040119     C* Compongo il nome del membro da dare al FIVATWWR
044200040119     C                   eval      parmbr = vlrhdl
044300040119     C                   movel     'M'           parmbr
044400040510     C                   eval      parccm = vlrksc
044500040119     C                   eval      paropz = '1'
044600040119     C* Effettuo la chiamata al CLLE preposto
044700040119     C                   call(e)   'TITVVTC'
044800040119     C                   parm                    parccm
044900040119     C                   parm                    parmbr
045000040119     C                   parm                    paropz
045100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045200040119     C                   if        %error
045300040119     C                   movel     '1'           chkcall
045400040119     C                   else
045500040119     C                   movel     '0'           chkcall
045600040119     C                   endif
045700040119     C*
045800040119     C                   ENDSR
045900010601
046000010601
046100010601
046200010330     C*----------------------------------------------------*
046300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046400010330     C*----------------------------------------------------*
046500040119     C     INZVAR        BEGSR
046600010330     C*
046700030822     C* Inizializzo variabili di wrk
046800020204     C                   Z-ADD     *zeros        Num5_0            5 0
046900060331     C                   MOVEL     *blanks       wVATNOT_A
047000060331     C                   MOVEL     *blanks       wVATNOT_B
047100060331     C                   MOVEL     *blanks       wVATNOT_E
047200030822     C*
047300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047400030822     C                   CLEAR                   FIVAB000
047500040119     C                   CLEAR                   FIVAT000
047600030822     C                   CLEAR                   SkSplitCSV
047700030822     C                   EVAL      i = 1
047800030822     C                   EVAL      posDa = *zeros
047900030822     C                   EVAL      posA  = *zeros
048000030822     C*
048100030822     C* Reimposto i valori di default
048200030822     C                   EXSR      DEFCAM
048300030822     C*
048400010330     C                   ENDSR
048500010330     C*----------------------------------------------------*
048600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048700010330     C*----------------------------------------------------*
048800010330     C     DEFCAM        BEGSR
048900030822     C*
049000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049100030715     C* e delimitatore testo.
049200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049500030822     C*
049600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
049800030715     C                   SETON                                        50
049900030715     C                   ELSE
050000030715     C                   SETOFF                                       50
050100030715     C                   ENDIF
050200030822     C*
050300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050400030822     C                   EVAL      posDaDft = 1
050500030822     C                   EVAL      posADft  = 0
050600030822     C                   EVAL      wGiroDft = 0
050700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050800030822     C                             posDaDft > 0
050900030822     C*
051000030822     C* Gestisco il 1° giro
051100030822     C                   IF        wGiroDft = 0
051200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051400030822     C* Incremento il contatore dei "giri"
051500030822     C                   EVAL      wGiroDft = 1
051600030822     C                   ELSE
051700030822     C                   EVAL      posDaDft = posADft
051800030822     C                   ENDIF
051900030822     C* Eseguo lo scan x trovare la fine del campo corrente
052000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052100030822     C*
052200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052400030822     C                   IF        posDaDft > 0 AND
052500030822     C                             posADft  > 0
052600030822     C* NCL
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'NCL'
053100030822     C                   EVAL      PiStr=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   EXSR      CHKNUM
053500030822     C                   IF        PiInt=*on
053600030822     C                   Z-ADD     PiVal         VABNCL
053700030822     C                   ENDIF
053800030822     C                   ENDIF
053900030822     C* CCM
054000030822     C                   IF        %subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):1:3)
054300030822     C                             = 'CCM'
054400030822     C                   EVAL      PiStr=%trim(%subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700030822     C                   EXSR      CHKNUM
054800030822     C                   IF        PiInt=*on
054900030822     C                   Z-ADD     PiVal         VABCCM
055000030822     C                   ENDIF
055100030822     C                   ENDIF
055200030822     C* LNP
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'LNP'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiInt=*on
056200030822     C                   Z-ADD     PiVal         VABLNP
056300030822     C                   ENDIF
056400030822     C                   ENDIF
056500030822     C* NRS
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'NRS'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABNRS
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* CTR
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'CTR'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABCTR
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* PKB
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'PKB'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiNum=*on
060100030822     C                   Z-ADD     PiVal         VABPKB
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* VLB
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'VLB'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiNum=*on
061400030822     C                   Z-ADD     PiVal         VABVLB
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* QFT
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'QFT'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiNum=*on
062700030822     C                   Z-ADD     PiVal         VABQFT
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* CBO
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'CBO'
063500030822     C                   EVAL      VABCBO=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* TSP
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'TSP'
064400030822     C                   EVAL      VABTSP=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* VAS
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'VAS'
065300030822     C                   EVAL      VABVAS=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* VCA
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'VCA'
066200030822     C                   EVAL      VABVCA=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* GCA
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'GCA'
067100030822     C                   EVAL      VABGCA=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* CTM
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'CTM'
068000030822     C                   EVAL      VABCTM=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* FFD
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'FFD'
068900030822     C                   EVAL      VABFFD=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* VAD
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'VAD'
069800030822     C                   EVAL      VABVAD=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* GMA
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'GMA'
070700030822     C                   EVAL      VABGMA=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* GGA
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'GGA'
071600030822     C                   EVAL      VABGGA=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* GVA
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'GVA'
072500030822     C                   EVAL      VABGVA=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* TC1
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'TC1'
073400030822     C                   EVAL      VABTC1=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* TC2
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'TC2'
074300030822     C                   EVAL      VABTC2=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700040714     C* VATTRC
074800040714     C                   IF        %subst(
074900040714     C                             %subst(vlrppt:posDaDft+1:
075000040714     C                             posADft-posDaDft-1):1:3)
075100040714     C                             = 'TRC'
075200040714     C                   EVAL      VATTRC=%trim(%subst(
075300040714     C                             %subst(vlrppt:posDaDft+1:
075400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500040714     C                   ENDIF
075600030822     C* ...
075700030822     C                   ENDIF
075800030822     C                   ENDDO
075900020204     C*
076000010330     C                   ENDSR
076100010607     C*----------------------------------------------------*
076200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
076300010607     C*----------------------------------------------------*
076400010607     C     IMPVAB        BEGSR
076500010607     C*
076600010607     C                   SETOFF                                       3132
076700030515     C*
076800010607     C                   MOVEL     datcor        VABAAS
076900010607     C                   MOVE      datcor        VABMGS
077000010607     C*
077100030715     C**********
077200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077300030715     C**********
077400030715     C                   IF        wGiro = *zeros
077500040113     C*
077600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
077700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
077800040510     C                             %trim(intcol(3))<>*blanks
077900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078000040113     C                                      %trim(intcol(3))
078100040510     C                   ENDIF
078200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078300031201     C     minu:maiu     XLATE     vindta        vindta
078400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078500030715     C                   DOW       posDa <= %len(%trim(vindta))
078600030715     C*
078700030715     C* Gestisco il 1° campo
078800030715     C                   IF        i = 1
078900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079000030715     C                   EVAL      posDa = 1
079100030715     C* Eseguo lo scan x trovare la fine del primo campo
079200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079300030715     C* A questo punto "estraggo" il campo corrente
079400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079500030715     C                                                    (posA-posDa))
079600030715     C* X i campi successivi al 1°
079700030715     C                   ELSE
079800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079900030715     C                   EVAL      posDa = posA + 1
080000030715     C* Eseguo lo scan x trovare la fine del campo corrente
080100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080200030715     C* Gestisco l'ultimo campo
080300030715     C                   IF        posA = *zeros
080400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080500030715     C* A questo punto "estraggo" il campo corrente
080600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080700030715     C                                                    (posA-posDa))
080800030715     C                   LEAVE
080900030715     C                   ELSE
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C                   ENDIF
081400030715     C                   ENDIF
081500030715     C* Incremento il contatore d campo
081600030715     C                   EVAL      i = i +1
081700030715     C                   ENDDO
081800030715     C*
081900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082000030715     C* delimitazione testo
082100030715     C                   EVAL      i = 1
082200030715     C                   DOW       i < %elem(SkSplitFLD)
082300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082500030715     C                   EVAL      i = i + 1
082600030715     C                   ENDDO
082700030715     C                   EVAL      wGiro = 1
082800030715     C                   ELSE
082900030715     C                   EVAL      wGiro = 2
083000031204     C**********
083100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
083200031204     C* d separatore campo
083300031204     C**********
083400031204     C                   EVAL      vindta = %trim(vindta)
083500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083600031204     C                   EVAL      vindta = %subst(vindta:2)
083700031204     C                   ENDDO
083800031204     C*
083900031204     C                   Z-ADD     *zeros        lunghInput        4 0
084000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
084200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084400031204     C                   ENDDO
084500030715     C**********
084600030715     C* Effettuo lo split del campo dati d input
084700030715     C**********
084800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084900030715     C                   DOW       posDa <= %len(%trim(vindta))
085000030715     C*
085100030715     C* Gestisco il 1° campo
085200030715     C                   IF        i = 1
085300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085400030715     C                   EVAL      posDa = 1
085500030715     C* Eseguo lo scan x trovare la fine del primo campo
085600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C* X i campi successivi al 1°
086100030715     C                   ELSE
086200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086300030715     C                   EVAL      posDa = posA + 1
086400031204     C* Verifico che nn vi sia il campo nn valorizzato
086500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086600031204     C* Se campo nn valorizzato skippo al prossimo
086700031204     C                   EVAL      SkSplitCSV(i) = *blanks
086800031204     C                   EVAL      posA  = posA + 1
086900031204     C                   ELSE
087000030715     C* Eseguo lo scan x trovare la fine del campo corrente
087100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087200030715     C* Gestisco l'ultimo campo
087300030715     C                   IF        posA = *zeros
087400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C                   LEAVE
087900030715     C                   ELSE
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   ENDIF
088400031204     C                   ENDIF
088500030715     C                   ENDIF
088600030715     C* Incremento il contatore d campo
088700030715     C                   EVAL      i = i +1
088800030715     C                   ENDDO
088900030715     C*
089000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089100030715     C* delimitazione testo
089200030715     C                   EVAL      i = 1
089300030715     C                   DOW       i < %elem(SkSplitCSV)
089400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089600030715     C                   EVAL      i = i + 1
089700030715     C                   ENDDO
089800030715     C*
089900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090000030715     C                   EVAL      i = 1
090100030715     C                   DOW       i < %elem(SkSplitFLD)
090200030715     C*
090300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090500030715     C                             SkSplitCSV(i) <> *blanks
090600030721     C*** ATB
090700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090800030721     C                   EVAL      VABATB = SkSplitCSV(i)
090900030721     C                   ENDIF
091000030715     C*** RSD
091100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
091200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
091300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091500030715     C* ==
091600030715     C                   ENDIF
091700030721     C*** RD2
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030715     C*** IND
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
092300030715     C                   EVAL      VABIND = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030715     C*** LOD
092600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092800030715     C                   ENDIF
092900030715     C*** PRD
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
093100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030721     C*** NZD
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030715     C*** RMA
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030715     C*** NOT
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
094300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** NT2
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** VCA
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** VAS
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030715     C*** FFD
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030721     C*** GC1
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
096300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** GC2
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** TSP
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
097100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
097200030721     C                   ENDIF
097300040510     C*** CBO
097400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
097500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
097600040510     C                   ENDIF
097700030721     C*** NAS
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** TIC
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** GCA
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** XCO
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** CTM
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** TCR
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** CTS
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** FTM
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** VAD
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** GMA
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** GGA
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** GVA
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** TC1
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TC2
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** SCL
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** RMO
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** NMO
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*
104600030721     C* Reperisco quindi i campi numerici...
104700040510     C*** NSP
104800070301     C***                IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104900070301     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105000040510     C                   EVAL      PiStr=SkSplitCSV(i)
105100040510     C                   EXSR      CHKNUM
105200040510     C                   IF        PiInt=*on
105300040510     C                   Z-ADD     PiVal         VABNSP
105400070301     C                   Z-ADD     PiVal         VABRMN
105500040510     C                   ELSE
105600040510     C                   SETON                                        31
105700040510     C                   EVAL      VABNSP = *zeros
105800070301     C                   EVAL      VABRMN = *zeros
105900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
106000070301     C                             + ' ' + 'VABNSP VABRMN'
106100040510     C                   ENDIF
106200040510     C                   ENDIF
106300030721     C*** CAD
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106500030721     C                   EVAL      PiStr=SkSplitCSV(i)
106600030721     C                   EXSR      CHKNUM
106700030721     C                   IF        PiInt=*on
106800030721     C                   Z-ADD     PiVal         Num5_0
106900030721     C                   MOVEL(p)  Num5_0        VABCAD
107000030721     C                   ELSE
107100030721     C                   SETON                                        32
107200030721     C                   EVAL      VABCAD = *zeros
107300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107400030721     C                             + ' ' + 'VABCAD'
107500030721     C                   ENDIF
107600030721     C                   ENDIF
107700030721     C*** NCL
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107900030721     C                   EVAL      PiStr=SkSplitCSV(i)
108000030721     C                   EXSR      CHKNUM
108100030721     C                   IF        PiInt=*on
108200030721     C                   Z-ADD     PiVal         VABNCL
108300030721     C                   ELSE
108400030721     C                   SETON                                        32
108500030721     C                   EVAL      VABNCL = *zeros
108600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108700040122     C                             + ' ' + 'VABNCL'
108800030721     C                   ENDIF
108900030721     C                   ENDIF
109000030721     C*** PKB
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109200030721     C                   EVAL      PiStr=SkSplitCSV(i)
109300030721     C                   EXSR      CHKNUM
109400030721     C                   IF        PiNum=*on
109500030721     C                   Z-ADD     PiVal         VABPKB
109600030721     C                   ELSE
109700030721     C                   SETON                                        32
109800030721     C                   EVAL      VABPKB = *zeros
109900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110000030721     C                             + ' ' + 'VABPKB'
110100030721     C                   ENDIF
110200030721     C                   ENDIF
110300030721     C*** CAS
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiNum=*on
110800030721     C                   Z-ADD     PiVal         VABCAS
110900030721     C                   ELSE
111000030721     C                   SETON                                        32
111100030721     C                   EVAL      VABCAS = *zeros
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABCAS'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** CCM
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on      AND
112100030721     C                             PiVal<=9999999 AND
112200030721     C                             PiVal>*zeros
112300030721     C                   Z-ADD     PiVal         VABCCM
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      VABCCM = *zeros
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800040510     C                             + ' ' + 'VABCCM'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** LNP
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiInt=*on    AND
113600030721     C                             PiVal<=999   AND
113700030721     C                             PiVal>*zeros
113800030721     C                   Z-ADD     PiVal         VABLNP
113900030721     C                   ELSE
114000040715     C                   SETON                                        32
114100030721     C                   EVAL      VABLNP = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040510     C                             + ' ' + 'VABLNP'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** NRS
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on AND
115100030721     C                             PiVal<=99
115200030721     C                   Z-ADD     PiVal         VABNRS
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600030721     C                             + ' ' + 'VABNRS'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** LNA
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on    AND
116400030721     C                             PiVal<=999   AND
116500030721     C                             PiVal>*zeros
116600030721     C                   Z-ADD     PiVal         VABLNA
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      VABLNA = *zeros
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABLNA'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** CTR
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on  AND
117900030721     C                             PiVal<=999
118000030721     C                   Z-ADD     PiVal         VABCTR
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABCTR'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** IAS
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABIAS
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABIAS'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** VLB
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiNum=*on
120400030721     C                   Z-ADD     PiVal         VABVLB
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABVLB'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** QFT
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiNum=*on
121600030721     C                   Z-ADD     PiVal         VABQFT
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABQFT'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** NCD
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on      AND
122800030721     C                             PiVal<=9999999
122900030721     C                   Z-ADD     PiVal         VABNCD
123000030721     C                   ELSE
123100030721     C                   SETON                                        32
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300030721     C                             + ' ' + 'VABNCD'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** NCA
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiInt=*on      AND
124100030721     C                             PiVal<=9999999
124200030721     C                   Z-ADD     PiVal         VABNCA
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABNCA'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** ZNC
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on AND
125400030721     C                             PiVal<=99
125500030721     C                   Z-ADD     PiVal         VABZNC
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABZNC'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** DCR
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on       AND
126700030721     C                             PiVal<=99999999
126800030721     C                   Z-ADD     PiVal         VABDCR
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABDCR'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** HCR
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on   AND
128000030721     C                             PiVal<=9999
128100030721     C                   Z-ADD     PiVal         VABHCR
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABHCR'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** VMD
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiNum=*on
129300030721     C                   Z-ADD     PiVal         VABVMD
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABVMD'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** ANT
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on        AND
130500030721     C                             PiVal<=999999999
130600030721     C                   Z-ADD     PiVal         VABANT
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABANT'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** CMO
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiInt=*on
131800030721     C                   Z-ADD     PiVal         Num5_0
131900030721     C                   MOVEL(p)  Num5_0        VABCMO
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200030721     C                   EVAL      VABCMO = *zeros
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABCMO'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700040510     C*
132800040510     C* Reperisco i campi del FIVAT
132900040510     C*** VATNOT
133000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133200040510     C                   ENDIF
133300040510     C*** VATTRC
133400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133600040510     C                   ENDIF
133700060331     C*** VATNOT_A
133800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
133900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134000060331     C                   ENDIF
134100060331     C*** VATNOT_B
134200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134400060331     C                   ENDIF
134500060331     C*** VATNOT_E
134600070301     C***                IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
134700070301     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
134800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
134900060331     C                   ENDIF
135000040510     C*
135100030715     C                   ENDIF
135200030715     C                   EVAL      i = i + 1
135300030715     C                   ENDDO
135400040510     C*
135500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
135600040510     C                   IF        VABNSP = *zeros
135700040510     C                   SETON                                        31
135800040510     C                   ENDIF
135900040715     C*
136000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
136100040715     C                   IF        VABLNP = *zeros
136200040715     C                   SETON                                        31
136300040715     C                   ENDIF
136400040510     C*
136500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
136600040510     C                   IF        VABRMN = *zeros
136700040510     C                   Z-ADD     VABNSP        VABRMN
136800040510     C                   ENDIF
136900040510     C*
137000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
137100040510     C                   IF        VABRMA = *blanks
137200040714     C                   MOVEL     VABRMN        VABRMA
137300040510     C                   ENDIF
137400040128     C*
137500040510     C* Se provincia nn valorizzata la reperisco
137600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
137700040510     C                   IF        VABPRD = *blanks
137800040128     C                   CLEAR                   TISI95DS
137900040128     C                   EVAL      I95TCN = '3'
138000040128     C                   Z-ADD     datcor        I95DAT
138100040128     C                   EVAL      I95NAR = VABNZD
138200040128     C                   EVAL      I95CAP = VABCAD
138300040128     C                   EVAL      I95LOC = VABLOD
138400040128     C                   CALL      'TISI95R'
138500040128     C                   PARM                    TISI95DS
138600040128     C                   EVAL      VABPRD = O95PRV
138700040510     C                   ENDIF
138800040113     C*
138900040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
139000040113     C                   IF        VABCAS > *zeros
139100040119     C                   EVAL      VABVCA = 'EUR'
139200040119     C                   IF        VABCBO = '1'
139300040113     C                   EVAL      VABCBO = '4'
139400040119     C                   ENDIF
139500040119     C                   IF        VABCBO = '2'
139600040119     C                   EVAL      VABCBO = '6'
139700040119     C                   ENDIF
139800040113     C                   ENDIF
139900020204     C*
140000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
140100020204     C                   EXSR      CHKIMPDIV
140200030715     C                   ENDIF
140300020204     C*
140400010607     C                   ENDSR
140500010607     C*----------------------------------------------------*
140600010601
140700020204
140800020204     C*----------------------------------------------------*
140900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
141000020204     C*----------------------------------------------------*
141100020204     C     CHKIMPDIV     BEGSR
141200020204     C*
141300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
141400020204     C                   Z-ADD     *zeros        wrkDec            9 9
141500020204     C*
141600020204     C* Come prima cosa effettuo considerazioni sulla divisa
141700020204     C                   IF        vabIAS > *zeros
141800020204     C                   IF        vabVAS <> 'EUR'
141900020204     C                   EVAL      vabVAS =  'ITL'
142000020204     C                   ENDIF
142100020204     C                   ENDIF
142200020204     C*
142300020204     C                   IF        vabCAS > *zeros
142400020204     C                   IF        vabVCA <> 'EUR'
142500020204     C                   EVAL      vabVCA =  'ITL'
142600020204     C                   ENDIF
142700020204     C                   ENDIF
142800020204     C*
142900020204     C                   IF        vabVMD > *zeros
143000020204     C                   IF        vabVAD <> 'EUR'
143100020204     C                   EVAL      vabVAD =  'ITL'
143200020204     C                   ENDIF
143300020204     C                   ENDIF
143400020204     C*
143500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
143600020204     C                   Z-ADD     vabIAS        wrkDec
143700020204     C                   IF        wrkDec > *zeros
143800020204     C                   IF        vabVAS = 'ITL'
143900020204     C                   EVAL      vabIAS = *zeros
144000020204     C                   ENDIF
144100020204     C                   ENDIF
144200020204     C*
144300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
144400020204     C                   Z-ADD     vabCAS        wrkDec
144500020204     C                   IF        wrkDec > *zeros
144600020204     C                   IF        vabVCA = 'ITL'
144700020204     C                   EVAL      vabCAS = *zeros
144800020204     C                   ENDIF
144900020204     C                   ENDIF
145000020204     C*
145100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
145200020204     C                   Z-ADD     vabVMD        wrkDec
145300020204     C                   IF        wrkDec > *zeros
145400020204     C                   IF        vabVAD = 'ITL'
145500020204     C                   EVAL      vabVMD = *zeros
145600020204     C                   ENDIF
145700020204     C                   ENDIF
145800020204     C*
145900020204     C                   ENDSR
146000020204     C***
146100020204
146200010330
146300010330
146400010330     C*----------------------------------------------------*
146500010330     C*  CONTROLLO NUMERICITA' CAMPI
146600010330     C*----------------------------------------------------*
146700010330     C     CHKNUM        BEGSR
146800010330     C*
146900010606     C                   IF        PiDecChr = *blanks
147000030715     C                   EVAL      PiDecChr = CharNUM
147100010606     C                   ENDIF
147200010606     C*
147300010606     C                   CALL(e)   'ISNUMERIC'
147400010330     C                   PARM                    PiStr            30
147500010606     C                   PARM                    PiDecChr          1
147600010330     C                   PARM      *ZEROS        PiVal            30 9
147700010330     C                   PARM      '0'           PiInt             1
147800010330     C                   PARM      '0'           PiNum             1
147900010330     C                   IF        %error
148000010606     C                   EVAL      PiNum=*off
148100010330     C                   ENDIF
148200010330     C*
148300010330     C                   ENDSR
148400010330     C***
148500010330
148600010601
148700010601
148800010601
148900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149000010601     C     repfil        BEGSR
149100010601     C*
149200010601     C                   if        invfil = *zeros and
149300010601     C                             depfil > *zeros and
149400010629     C                             (vinflg = *blanks or
149500010629     C                              vinflg = *zeros)
149600010601     C
149700010601     C                   eval      invfil = depfil
149800010601     C                   endif
149900010601     C*
150000010601     C                   if        depfil <> invfil and
150100010601     C                             invfil > *zeros
150200010601     C                   eval      flgMulti = '1'
150300010601     C                   if        vinflg = *blanks
150400010601     C                   add       1             cntNonEl
150500010601     C                   endif
150600010601     C                   endif
150700010601     C*
150800010601     C                   if        vinflg = '2'
150900010601     C                   eval      flgStato = '2'
151000010601     C                   endif
151100010601     C*
151200010601     C                   ENDSR
151300010601     C***
151400010601
151500010601
151600010601
151700010330
151800040119      /TITLE Invio dei dati al punto operativo.
151900040119     C     invio         BEGSR
152000040119     C*
152100040119     C* 1° invio FIVAT
152200040119     C                   reset                   dscmz
152300040119     C                   move      vlrpoi        cmzdst
152400040119     C                   eval      cmzfld = 'FIVATWWR'
152500040119     C                   eval      cmzmbd = vlrhdl
152600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152700040119     C***                if        prmfir = *blanks
152800040119     C                   eval      cmzfla = 'FIVAT00F'
152900040119     C                   eval      cmzmba = 'FIVAT00F'
153000040119     C***                else
153100040119     C***                eval      cmzfla = prmfir
153200040119     C***                eval      cmzmba = prmfir
153300040119     C***                endif
153400040119     C                   eval      cmznrr = *zeros
153500040119     C                   move      §ctrokvt      cmznrr
153600040119     C                   eval      cmzlba = vlrfl1
153700040119     C                   call(e)   'TIS711C'
153800040119     C                   parm                    dscmz
153900040119     C                   parm      *blanks       esito
154000040119     C                   if        %error
154100040119     C                             or cmzerr = '1'
154200040119     C                             or esito  = '1'
154300040119     C                   eval      wrkesito = '3'
154400040119     C                   else
154500040119     C*
154600040119     C* 2° invio FIVAB
154700040119     C                   reset                   dscmz
154800040119     C                   move      vlrpoi        cmzdst
154900040119     C                   eval      cmzfld = vlrfou
155000040119     C                   eval      cmzmbd = vlrhdl
155100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155200040119     C***                if        prmfir = *blanks
155300040119     C                   eval      cmzfla = 'FIVAB00F'
155400040119     C                   eval      cmzmba = 'FIVAB00F'
155500040119     C***                else
155600040119     C***                eval      cmzfla = prmfir
155700040119     C***                eval      cmzmba = prmfir
155800040119     C***                endif
155900040119     C                   eval      cmznrr = *zeros
156000040119     C                   move      §ctrokvb      cmznrr
156100040119     C                   eval      cmzlba = vlrfl1
156200040119     C                   call(e)   'TIS711C'
156300040119     C                   parm                    dscmz
156400040119     C                   parm      *blanks       esito
156500040119     C                   if        %error
156600040119     C                             or cmzerr = '1'
156700040119     C                             or esito  = '1'
156800040119     C                   eval      wrkesito = '3'
156900040119     C                   endif
157000040119     C                   endif
157100040119     C*
157200040119     C                   ENDSR
157300040119     C***
157400010601
157500010601
157600010601
157700010601
157800010601      /TITLE Invio dei dati al punto operativo.
157900010601     C     opeini        BEGSR
158000010601     C*
158100010601     C* Inizializzo flag e contatori operativi
158200010601     C                   movel     '0'           flgGiro           1
158300010601     C                   movel     '0'           flgMulti          1
158400010601     C                   movel     '1'           flgStato          1
158500010615     C                   movel     '0'           flgOk             1
158600010601     C                   z-add     *zeros        cntNonEl         10 0
158700010601     C                   z-add     *zeros        depfil            3 0
158800010601     C                   z-add     *zeros        invfil            3 0
158900010601     C*
159000010601     C                   ENDSR
159100010601     C***
159200010601
159300010601
159400050421
159500070411
159600070411     C     *pssr         BEGSR
159700070411     C*
159800070411     C                   if        %open(tivin00r)
159900070411     C                   close     tivin00r
160000070411     C                   endif
160100070411     C                   if        %open(fivabwwr)
160200070411     C                   close     fivabwwr
160300070411     C                   endif
160400070411     C                   if        %open(fivatwwr)
160500070411     C                   close     fivatwwr
160600070411     C                   endif
160700070411     C*
160800070411     C* Effettuo la chiamata al CLLE preposto
160900070411     C                   call(e)   'TITVVTC'
161000070411     C                   parm                    parccm
161100070411     C                   parm                    parmbr
161200070411     C                   parm      '2'           paropz
161300070411     C*
161400070411     C                   eval      wrkesito = '2'
161500070411     C*
161600070411     C                   seton                                        LR
161700070411     C*
161800070411     C                   ENDSR     '*CANCL'
161900070411     C***
162000070411
162100050421
162200010601
162300010330
162400010330
162500000613     C     *inzsr        BEGSR
162600990910     C*
162700990910     C     *entry        plist
162800990920     C                   parm                    tivlrds
162900990921     C                   parm      wrkesito      esito
163000000724     C                   parm                    prmlit
163100000710     C                   parm                    prmfir
163200010330     C*
163300010330     C* CALCOLA LA DATA CORRENTE
163400010330     C                   time                    wn14             14 0
163500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
163600010330     C                   z-add     wn8           g08dat
163700010330     C                   z-add     *zeros        g08inv
163800010330     C                   movel     '0'           g08err
163900010330     C                   call      'XSRDA8'
164000010330     C                   parm                    wlbda8
164100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
164200000613     C*
164300000613     C                   ENDSR
164400000613     C***
164500990908
164600070301     Otitvy2p   E            riepilogo         2
164700990915     O                                              'Upload via Internet'
164800990915     O                                           +1 'Traduzione TIVIN00R -'
164900021025     O                                           +1 'FIVABWWR'
165000990915     O                                           +1 'Testate bolle'
165100990915     O          E            riepilogo   2
165200990915     O                       wrkdata
165300990915     O                       wrkora              +1
165400990915     O                       procname            +1
165500990915     O          E            riepilogo   2
165600990915     O                                              'Cliente..................:'
165700010605     O                       VABCCM        z     +1
165800990915     O          E            riepilogo   2
165900990920     O                                              'Riferimento Strategi.....:'
166000990920     O                       vlrhdl              +1
166100990915     O          E            riepilogo   2
166200990915     O                                              'Giusti...................:'
166300040119     O                       §CTROKVB      2   +  1
166400990915     O          E            riepilogo   2
166500990915     O                                              'Sbagliati e corretti.....:'
166600971022     O                       §CTRMO        2   +  1
166700990915     O          E            riepilogo   2
166800990915     O                                              'Sbagliati e scartati.....:'
166900971022     O                       §CTRNO        2   +  1
167000000613
167100070301     Otitvy2ps  E            testdett          2
167200000613     O                                              'Upload via Internet'
167300000613     O                                           +1 'Traduzione TIVIN00R -'
167400021025     O                                           +1 'FIVABWWR'
167500000613     O                                           +1 'Testate bolle'
167600000616     O          E            testdett    3
167700000613     O                                           +2 'N° rec'
167800000613     O                                           +3 'Anteprima contenuto'
167900000616     O          E            rigadett    2
168000000613     O                       rrnum               +2
168100000621     O                       recko               +3
168200000616     O          E            findett     2
168300000613     O                       wrkdata
168400000613     O                       wrkora              +1
168500000613     O                       procname            +1
168600000616     O          E            findett     2
168700000613     O                                              'Cliente..................:'
168800010605     O                       VABCCM        z     +1
168900000616     O          E            findett     2
169000000613     O                                              'Riferimento Strategi.....:'
169100000613     O                       vlrhdl              +1
169200000616     O          E            findett     2
169300000613     O                                              'Giusti...................:'
169400040119     O                       §CTROKVB      2   +  1
169500000616     O          E            findett     2
169600000613     O                                              'Sbagliati e corretti.....:'
169700000613     O                       §CTRMO        2   +  1
169800000616     O          E            findett     2
169900000613     O                                              'Sbagliati e scartati.....:'
170000000613     O                       §CTRNO        2   +  1
170100000613** CMD - COMANDI CL
170200070301OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
170300070301OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
170400070301DLTOVR FILE(TITVY2P TITVY2PS) LVL(*)
170500010606
170600000613
170700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170800040510
170900040122
171000040113
