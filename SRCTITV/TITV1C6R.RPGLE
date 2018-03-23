000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800071204     Ftitv1c6p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100071204     Ftitv1c6ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070329     F                                     infsr(*pssr)
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
004500071029     D wFlgCAS         s              1    INZ(*blanks)
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
005800070823     D wVATNOT_A       s                   LIKE(VATNOT)
005900060331     D wVATNOT_B       s                   LIKE(VATNOT)
006000060331     D wVATNOT_E       s                   LIKE(VATNOT)
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
013400071204     C                   if        not %open(titv1c6ps)
013500071204     C                   open      titv1c6ps
013600070329     C                   except    testdett
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
016600071204     C                   if        %open(titv1c6ps)
016700000616     C                   except    findett
016800071204     C                   close     titv1c6ps
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
018800071204     C                   if        not %open(titv1c6p)
018900071204     C   50              open      titv1c6p
019000990915     C                   endif
019100990915     C*
019200030715     C   50              except    riepilogo
019300990915     C*
019400071204     C                   if        %open(titv1c6p)
019500071204     C   50              close     titv1c6p
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
022800070823     C*
022900070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
023000070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
023100080108     C***                exsr      inzvar
023200080108     C***                exsr      impvab
023300010330     C*
023400030822     C                   do        *HIVAL
023500010330     C*
023600030822     C                   read      tivin00r                               70
023700010618     C*
023800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023900010618     C                   if        vinflg = '1'
024000010618     C                   eval      flgOk = '1'
024100010618     C                   endif
024200040510     C*
024300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024400040510     C                   z-add     *zeros        wLenVINDTA        4 0
024500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024900040510     C                   z-add     1             wFlgVINDTA
025000040510     C                   leave
025100040510     C                   endif
025200040510     C                   enddo
025300010618     C*
025400040510     C                   if        vindta > *blanks AND
025500040510     C                             wFlgVINDTA = 1
025600010330     C                   add       1             rrnum
025700010330     C*
025800010601     C                   if        *in70 = *off and
025900010330     C                             (vinflg = *blanks
026000010330     C                              or vinflg = '0'
026100010330     C                              or vinflg = '2')
026200010330     C*
026300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026400010711     C                   if        vinflg = *blanks or vinflg = '0'
026500010711     C                   clear                   vinmsg
026600010711     C                   endif
026700010601     C*
026800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
026900070925     C                   eval      vindta  = ' ' + %trim(vindta)
027000070925     C*
027100010330     C                   exsr      inzvar
027200010605     C                   exsr      impvab
027300010601     C*
027400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027500010605     C                   eval      depfil = VABLNP
027600010601     C                   exsr      repfil
027700010601     C                   if        depfil = invfil
027800021025     C                   if        vlrpoi = 999
027900030822     C                   move(P)   invfil        VABFGS
028000021025     C                   else
028100030822     C                   move(P)   vlrpoi        VABFGS
028200021025     C                   endif
028300010601     C*
028400010601     C                   exsr      PREELA
028500010601     C*
028600010604     C* Ebbene...
028700010604     C*
028800040119     C  N31              add       1             §CTROKVB
028900040119     C   32              add       1             §CTRMO
029000040119     C   31              add       1             §CTRNO
029100030822     C                   if        wGiro = 2
029200040510     C* Scrivo sempre il VAT
029300040510     C  N31              exsr      wrivat
029400040510     C* Scrivo il VAB solo a rottura d spedizione
029500040119     C                   if        depspe =  *zeros OR
029600040119     C                             depspe <> vabNSP
029700040119     C  N31              exsr      wrivab
029800040119     C  N31              eval      depspe = vabNSP
029900040119     C                   endif
030000040119     C                   endif
030100010604     C*
030200010604     C                   if        *in31 = *off and
030300010604     C                             *in32 = *off
030400010604     C                   eval      vinflg = '1'
030500010604     C                   else
030600010604     C                   eval      recko = vindta
030700010604     C                   except    rigadett
030800010604     C                   eval      vinflg = '2'
030900010604     C                   endif
031000010604     C                   endif
031100010601     C*
031200010604     C                   endif
031300010604     C*
031400010330     C                   else
031500010330     C                   eval      vinflg = '1'
031600010330     C                   endif
031700010601     C*
031800010601     C  N70              update    tivin000
031900010330     C*
032000030822     C  N70              enddo
032100040119     C*
032200040119     C                   endif                                                  (endif - chkcall)
032300010601     C*
032400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032500010601     C                   if        cntNonEl = *zeros or
032600010601     C                             flgMulti = '0'
032700010330     C* Se non ci sono record con errori ...
032800010601     C                   if        §ctrno = 0 and
032900010604     C                             §ctrmo = 0 and
033000010601     C                             flgStato <> '2'
033100010330     C* ... restituisco esito OK.
033200010330     C                   eval      wrkesito = '0'
033300010330     C                   else
033400040119     C                   if        §ctrokvb > 0
033500010330     C                   eval      wrkesito = '1'
033600010330     C                   else
033700010615     C                   if        flgOk = '0'
033800010615     C                   eval      wrkesito = '2'
033900010615     C                   else
034000010615     C                   eval      wrkesito = '6'
034100010615     C                   endif
034200010330     C                   endif
034300010330     C                   endif
034400010601     C                   else
034500010601     C                   eval      wrkesito = '9'
034600010601     C                   endif
034700010330     C*
034800010330     C                   if        %open(tivin00r)
034900010330     C                   close     tivin00r
035000010330     C                   endif
035100021025     C                   if        %open(fivabwwr)
035200021025     C                   close     fivabwwr
035300010330     C                   endif
035400040512     C                   if        %open(fivatwwr)
035500040512     C                   close     fivatwwr
035600040512     C                   endif
035700010601     C*
035800010601     C                   if        vlrpoi <> 999
035900010601     C                   eval      invfil = vlrpoi
036000010601     C                   endif
036100010330     C*
036200040119     C                   if        §ctrokvb > 0
036300010601     C                             and invfil > *zeros
036400010330     C                   exsr      invio
036500010330     C                   endif
036600010601     C*
036700010618     C                   if        flgGiro = '1'
036800010601     C                   exsr      endela
036900010618     C                   endif
037000010330     C*
037100010330     C                   ENDSR
037200010330     C***
037300040119
037400040119
037500040119
037600040119     C*----------------------------------------------------*
037700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037800040119     C*----------------------------------------------------*
037900040510     C     WRIVAB        BEGSR
038000040119     C*
038100060331     C* Gestisco l'eventuale rottura x numero spedizione
038200070823     C                   if        VABNSP <> depspe
038300070301     C                   if        wVATNOT_E <> *blanks
038400070301     C                   eval      VABCTM = '7Q'
038500070301     C                   endif
038600040119     C                   write     fivab000                                     => scarico il VAB
038700060331     C                   endif
038800040119     C*
038900040119     C                   ENDSR
039000040119
039100040119
039200040119
039300040119     C*----------------------------------------------------*
039400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039500040119     C*----------------------------------------------------*
039600040510     C     WRIVAT        BEGSR
039700060512     C*
039800060512     C* Inizializzo i campi chiave
039900060512     C                   eval      VATFGS = VABFGS
040000060512     C                   eval      VATAAS = VABAAS
040100060512     C                   eval      VATCCM = VABCCM
040200070823     C                   eval      VATNRS = VABNRS
040300060512     C                   eval      VATNSP = VABNSP
040400060512     C                   eval      VATLNP = VABLNP
040500040714     C*
040600040714     C* Solo se VATNOT valorizzato scarico il buffer
040700040714     C                   if        VATNOT <> *blanks
040800040119     C*
040900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
041000040510     C                   if        VATTRC = *blanks
041100040510     C                   eval      VATTRC = 'E'
041200040510     C                   endif
041300040510     C                   write     FIVAT000
041400040119     C                   add       1             §CTROKVT
041500040714     C*
041600040714     C                   endif
041700060331     C*
041800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041900060331     C                   if        wVATNOT_A <> *blanks
042000060331     C                   eval      VATNOT = wVATNOT_A
042100060331     C                   eval      VATTRC = 'A'
042200060331     C                   write     FIVAT000
042300060331     C                   add       1             §CTROKVT
042400060331     C                   endif
042500060331     C*
042600060331     C                   if        wVATNOT_B <> *blanks
042700060331     C                   eval      VATNOT = wVATNOT_B
042800060331     C                   eval      VATTRC = 'B'
042900060331     C                   write     FIVAT000
043000060331     C                   add       1             §CTROKVT
043100060331     C                   endif
043200060331     C*
043300060331     C                   if        wVATNOT_E <> *blanks
043400060331     C                   eval      VATNOT = wVATNOT_E
043500060331     C                   eval      VATTRC = 'E'
043600060331     C                   write     FIVAT000
043700060331     C                   add       1             §CTROKVT
043800060331     C                   endif
043900040119     C*
044000040119     C                   ENDSR
044100040119
044200040119
044300040119
044400040119     C*----------------------------------------------------*
044500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044600040119     C*----------------------------------------------------*
044700040119     C     PREVAT        BEGSR
044800040119     C*
044900040119     C* Compongo il nome del membro da dare al FIVATWWR
045000040119     C                   eval      parmbr = vlrhdl
045100040119     C                   movel     'M'           parmbr
045200040510     C                   eval      parccm = vlrksc
045300040119     C                   eval      paropz = '1'
045400040119     C* Effettuo la chiamata al CLLE preposto
045500040119     C                   call(e)   'TITVVTC'
045600040119     C                   parm                    parccm
045700040119     C                   parm                    parmbr
045800040119     C                   parm                    paropz
045900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046000040119     C                   if        %error
046100040119     C                   movel     '1'           chkcall
046200040119     C                   else
046300040119     C                   movel     '0'           chkcall
046400040119     C                   endif
046500040119     C*
046600040119     C                   ENDSR
046700010601
046800010601
046900010601
047000010330     C*----------------------------------------------------*
047100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047200010330     C*----------------------------------------------------*
047300040119     C     INZVAR        BEGSR
047400010330     C*
047500030822     C* Inizializzo variabili di wrk
047600020204     C                   Z-ADD     *zeros        Num5_0            5 0
047700060331     C                   MOVEL     *blanks       wVATNOT_A
047800060331     C                   MOVEL     *blanks       wVATNOT_B
047900060331     C                   MOVEL     *blanks       wVATNOT_E
048000030822     C*
048100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048200030822     C                   CLEAR                   FIVAB000
048300040119     C                   CLEAR                   FIVAT000
048400030822     C                   CLEAR                   SkSplitCSV
048500030822     C                   EVAL      i = 1
048600030822     C                   EVAL      posDa = *zeros
048700030822     C                   EVAL      posA  = *zeros
048800030822     C*
048900030822     C* Reimposto i valori di default
049000030822     C                   EXSR      DEFCAM
049100030822     C*
049200010330     C                   ENDSR
049300010330     C*----------------------------------------------------*
049400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049500010330     C*----------------------------------------------------*
049600010330     C     DEFCAM        BEGSR
049700030822     C*
049800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049900030715     C* e delimitatore testo.
050000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050300030822     C*
050400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
050600030715     C                   SETON                                        50
050700030715     C                   ELSE
050800030715     C                   SETOFF                                       50
050900030715     C                   ENDIF
051000030822     C*
051100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051200030822     C                   EVAL      posDaDft = 1
051300030822     C                   EVAL      posADft  = 0
051400030822     C                   EVAL      wGiroDft = 0
051500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051600030822     C                             posDaDft > 0
051700030822     C*
051800030822     C* Gestisco il 1° giro
051900030822     C                   IF        wGiroDft = 0
052000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052200030822     C* Incremento il contatore dei "giri"
052300030822     C                   EVAL      wGiroDft = 1
052400030822     C                   ELSE
052500030822     C                   EVAL      posDaDft = posADft
052600030822     C                   ENDIF
052700030822     C* Eseguo lo scan x trovare la fine del campo corrente
052800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052900030822     C*
053000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053200030822     C                   IF        posDaDft > 0 AND
053300030822     C                             posADft  > 0
053400030822     C* NCL
053500030822     C                   IF        %subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):1:3)
053800030822     C                             = 'NCL'
053900030822     C                   EVAL      PiStr=%trim(%subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200030822     C                   EXSR      CHKNUM
054300030822     C                   IF        PiInt=*on
054400030822     C                   Z-ADD     PiVal         VABNCL
054500030822     C                   ENDIF
054600030822     C                   ENDIF
054700030822     C* CCM
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'CCM'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABCCM
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* LNP
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'LNP'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABLNP
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* NRS
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'NRS'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABNRS
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* CTR
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'CTR'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiInt=*on
059600030822     C                   Z-ADD     PiVal         VABCTR
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* PKB
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'PKB'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiNum=*on
060900030822     C                   Z-ADD     PiVal         VABPKB
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* VLB
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'VLB'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiNum=*on
062200030822     C                   Z-ADD     PiVal         VABVLB
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* QFT
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'QFT'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiNum=*on
063500030822     C                   Z-ADD     PiVal         VABQFT
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* CBO
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'CBO'
064300030822     C                   EVAL      VABCBO=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   ENDIF
064700030822     C* TSP
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'TSP'
065200030822     C                   EVAL      VABTSP=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   ENDIF
065600030822     C* VAS
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'VAS'
066100030822     C                   EVAL      VABVAS=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* VCA
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'VCA'
067000030822     C                   EVAL      VABVCA=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400070925     C* TIC
067500070925     C                   IF        %subst(
067600070925     C                             %subst(vlrppt:posDaDft+1:
067700070925     C                             posADft-posDaDft-1):1:3)
067800070925     C                             = 'TIC'
067900070925     C                   EVAL      VABTIC=%trim(%subst(
068000070925     C                             %subst(vlrppt:posDaDft+1:
068100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200070925     C                   ENDIF
068300030822     C* GCA
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'GCA'
068800030822     C                   EVAL      VABGCA=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200030822     C* CTM
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'CTM'
069700030822     C                   EVAL      VABCTM=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100030822     C* FFD
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'FFD'
070600030822     C                   EVAL      VABFFD=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* VAD
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'VAD'
071500030822     C                   EVAL      VABVAD=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* GMA
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'GMA'
072400030822     C                   EVAL      VABGMA=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* GGA
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'GGA'
073300030822     C                   EVAL      VABGGA=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* GVA
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'GVA'
074200030822     C                   EVAL      VABGVA=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* TC1
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'TC1'
075100030822     C                   EVAL      VABTC1=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* TC2
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'TC2'
076000030822     C                   EVAL      VABTC2=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400040714     C* VATTRC
076500040714     C                   IF        %subst(
076600040714     C                             %subst(vlrppt:posDaDft+1:
076700040714     C                             posADft-posDaDft-1):1:3)
076800040714     C                             = 'TRC'
076900040714     C                   EVAL      VATTRC=%trim(%subst(
077000040714     C                             %subst(vlrppt:posDaDft+1:
077100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200040714     C                   ENDIF
077300030822     C* ...
077400030822     C                   ENDIF
077500030822     C                   ENDDO
077600020204     C*
077700010330     C                   ENDSR
077800010607     C*----------------------------------------------------*
077900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078000010607     C*----------------------------------------------------*
078100010607     C     IMPVAB        BEGSR
078200010607     C*
078300010607     C                   SETOFF                                       3132
078400030515     C*
078500010607     C                   MOVEL     datcor        VABAAS
078600010607     C                   MOVE      datcor        VABMGS
078700010607     C*
078800030715     C**********
078900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079000030715     C**********
079100030715     C                   IF        wGiro = *zeros
079200040113     C*
079300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079500040510     C                             %trim(intcol(3))<>*blanks
079600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079700040113     C                                      %trim(intcol(3))
079800070823     C     ';':CharCSV   XLATE     vindta        vindta
079900040510     C                   ENDIF
080000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080100031201     C     minu:maiu     XLATE     vindta        vindta
080200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080300030715     C                   DOW       posDa <= %len(%trim(vindta))
080400030715     C*
080500030715     C* Gestisco il 1° campo
080600030715     C                   IF        i = 1
080700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080800030715     C                   EVAL      posDa = 1
080900030715     C* Eseguo lo scan x trovare la fine del primo campo
081000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081100030715     C* A questo punto "estraggo" il campo corrente
081200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081300030715     C                                                    (posA-posDa))
081400030715     C* X i campi successivi al 1°
081500030715     C                   ELSE
081600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081700030715     C                   EVAL      posDa = posA + 1
081800030715     C* Eseguo lo scan x trovare la fine del campo corrente
081900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082000030715     C* Gestisco l'ultimo campo
082100030715     C                   IF        posA = *zeros
082200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082300030715     C* A questo punto "estraggo" il campo corrente
082400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082500030715     C                                                    (posA-posDa))
082600030715     C                   LEAVE
082700030715     C                   ELSE
082800030715     C* A questo punto "estraggo" il campo corrente
082900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083000030715     C                                                    (posA-posDa))
083100030715     C                   ENDIF
083200030715     C                   ENDIF
083300030715     C* Incremento il contatore d campo
083400030715     C                   EVAL      i = i +1
083500030715     C                   ENDDO
083600030715     C*
083700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083800030715     C* delimitazione testo
083900030715     C                   EVAL      i = 1
084000030715     C                   DOW       i < %elem(SkSplitFLD)
084100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084300030715     C                   EVAL      i = i + 1
084400030715     C                   ENDDO
084500030715     C                   EVAL      wGiro = 1
084600030715     C                   ELSE
084700030715     C                   EVAL      wGiro = 2
084800031204     C**********
084900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085000031204     C* d separatore campo
085100031204     C**********
085200031204     C                   EVAL      vindta = %trim(vindta)
085300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085400031204     C                   EVAL      vindta = %subst(vindta:2)
085500031204     C                   ENDDO
085600031204     C*
085700031204     C                   Z-ADD     *zeros        lunghInput        4 0
085800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086200031204     C                   ENDDO
086300030715     C**********
086400030715     C* Effettuo lo split del campo dati d input
086500030715     C**********
086600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086700030715     C                   DOW       posDa <= %len(%trim(vindta))
086800030715     C*
086900030715     C* Gestisco il 1° campo
087000030715     C                   IF        i = 1
087100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087200030715     C                   EVAL      posDa = 1
087300030715     C* Eseguo lo scan x trovare la fine del primo campo
087400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C* X i campi successivi al 1°
087900030715     C                   ELSE
088000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088100030715     C                   EVAL      posDa = posA + 1
088200031204     C* Verifico che nn vi sia il campo nn valorizzato
088300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088400031204     C* Se campo nn valorizzato skippo al prossimo
088500031204     C                   EVAL      SkSplitCSV(i) = *blanks
088600031204     C                   EVAL      posA  = posA + 1
088700031204     C                   ELSE
088800030715     C* Eseguo lo scan x trovare la fine del campo corrente
088900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089000030715     C* Gestisco l'ultimo campo
089100030715     C                   IF        posA = *zeros
089200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C                   LEAVE
089700030715     C                   ELSE
089800030715     C* A questo punto "estraggo" il campo corrente
089900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090000030715     C                                                    (posA-posDa))
090100030715     C                   ENDIF
090200031204     C                   ENDIF
090300030715     C                   ENDIF
090400030715     C* Incremento il contatore d campo
090500030715     C                   EVAL      i = i +1
090600030715     C                   ENDDO
090700030715     C*
090800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090900030715     C* delimitazione testo
091000030715     C                   EVAL      i = 1
091100030715     C                   DOW       i < %elem(SkSplitCSV)
091200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091400030715     C                   EVAL      i = i + 1
091500030715     C                   ENDDO
091600030715     C*
091700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
091800030715     C                   EVAL      i = 1
091900030715     C                   DOW       i < %elem(SkSplitFLD)
092000030715     C*
092100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092300030715     C                             SkSplitCSV(i) <> *blanks
092400030721     C*** ATB
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092600030721     C                   EVAL      VABATB = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030715     C*** RSD
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093300030715     C* ==
093400030715     C                   ENDIF
093500030721     C*** RD2
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
093700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030715     C*** IND
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094100030715     C                   EVAL      VABIND = SkSplitCSV(i)
094200030715     C                   ENDIF
094300030715     C*** LOD
094400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
094500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
094600030715     C                   ENDIF
094700030715     C*** PRD
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
094900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095000030715     C                   ENDIF
095100030721     C*** NZD
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
095300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030715     C*** RMA
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
095700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** NOT
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** NT2
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
096500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030715     C*** VCA
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
096900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** VAS
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
097300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** FFD
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
097700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030721     C*** GC1
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** GC2
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** TSP
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
098900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099000030721     C                   ENDIF
099100040510     C*** CBO
099200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099400040510     C                   ENDIF
099500030721     C*** NAS
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
099700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** TIC
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** GCA
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** XCO
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
100900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** CTM
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** TCR
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
101700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** CTS
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** FTM
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** VAD
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
102900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** GMA
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** GGA
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
103700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** GVA
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** TC1
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** TC2
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
104900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** SCL
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** RMO
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** NMO
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*
106400030721     C* Reperisco quindi i campi numerici...
106500040510     C*** NSP
106600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
106700040510     C                   EVAL      PiStr=SkSplitCSV(i)
106800040510     C                   EXSR      CHKNUM
106900040510     C                   IF        PiInt=*on
107000040510     C                   Z-ADD     PiVal         VABNSP
107100040510     C                   ELSE
107200040510     C                   SETON                                        31
107300040510     C                   EVAL      VABNSP = *zeros
107400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107500040510     C                             + ' ' + 'VABNSP'
107600040510     C                   ENDIF
107700040510     C                   ENDIF
107800071106     C*** RMN
107900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108000071106     C                   EVAL      PiStr=SkSplitCSV(i)
108100071106     C                   EXSR      CHKNUM
108200071106     C                   IF        PiInt=*on
108300071106     C                   Z-ADD     PiVal         VABRMN
108400071106     C                   ELSE
108500071106     C                   SETON                                        32
108600071106     C                   EVAL      VABRMN = *zeros
108700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
108800071106     C                             + ' ' + 'VABRMN'
108900071106     C                   ENDIF
109000071106     C                   ENDIF
109100030721     C*** CAD
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109300030721     C                   EVAL      PiStr=SkSplitCSV(i)
109400030721     C                   EXSR      CHKNUM
109500030721     C                   IF        PiInt=*on
109600030721     C                   Z-ADD     PiVal         Num5_0
109700030721     C                   MOVEL(p)  Num5_0        VABCAD
109800030721     C                   ELSE
109900030721     C                   SETON                                        32
110000030721     C                   EVAL      VABCAD = *zeros
110100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110200030721     C                             + ' ' + 'VABCAD'
110300030721     C                   ENDIF
110400030721     C                   ENDIF
110500030721     C*** NCL
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiInt=*on
111000030721     C                   Z-ADD     PiVal         VABNCL
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      VABNCL = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500040122     C                             + ' ' + 'VABNCL'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** PKB
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiNum=*on
112300030721     C                   Z-ADD     PiVal         VABPKB
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      VABPKB = *zeros
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800030721     C                             + ' ' + 'VABPKB'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** CAS
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiNum=*on
113600030721     C                   Z-ADD     PiVal         VABCAS
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      VABCAS = *zeros
114000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114100030721     C                             + ' ' + 'VABCAS'
114200030721     C                   ENDIF
114300030721     C                   ENDIF
114400030721     C*** CCM
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiInt=*on      AND
114900030721     C                             PiVal<=9999999 AND
115000030721     C                             PiVal>*zeros
115100030721     C                   Z-ADD     PiVal         VABCCM
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      VABCCM = *zeros
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600040510     C                             + ' ' + 'VABCCM'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** LNP
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on    AND
116400030721     C                             PiVal<=999   AND
116500030721     C                             PiVal>*zeros
116600030721     C                   Z-ADD     PiVal         VABLNP
116700030721     C                   ELSE
116800040715     C                   SETON                                        32
116900030721     C                   EVAL      VABLNP = *zeros
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100040510     C                             + ' ' + 'VABLNP'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** NRS
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on AND
117900030721     C                             PiVal<=99
118000030721     C                   Z-ADD     PiVal         VABNRS
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABNRS'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** LNA
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on    AND
119200030721     C                             PiVal<=999   AND
119300030721     C                             PiVal>*zeros
119400030721     C                   Z-ADD     PiVal         VABLNA
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      VABLNA = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABLNA'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** CTR
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on  AND
120700030721     C                             PiVal<=999
120800030721     C                   Z-ADD     PiVal         VABCTR
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABCTR'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** IAS
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiNum=*on
122000030721     C                   Z-ADD     PiVal         VABIAS
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABIAS'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** VLB
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiNum=*on
123200030721     C                   Z-ADD     PiVal         VABVLB
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABVLB'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** QFT
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiNum=*on
124400030721     C                   Z-ADD     PiVal         VABQFT
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABQFT'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** NCD
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on      AND
125600030721     C                             PiVal<=9999999
125700030721     C                   Z-ADD     PiVal         VABNCD
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABNCD'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** NCA
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on      AND
126900030721     C                             PiVal<=9999999
127000030721     C                   Z-ADD     PiVal         VABNCA
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABNCA'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** ZNC
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on AND
128200030721     C                             PiVal<=99
128300030721     C                   Z-ADD     PiVal         VABZNC
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABZNC'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** DCR
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiInt=*on       AND
129500030721     C                             PiVal<=99999999
129600030721     C                   Z-ADD     PiVal         VABDCR
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABDCR'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** HCR
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiInt=*on   AND
130800030721     C                             PiVal<=9999
130900030721     C                   Z-ADD     PiVal         VABHCR
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABHCR'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** VMD
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiNum=*on
132100030721     C                   Z-ADD     PiVal         VABVMD
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABVMD'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** ANT
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on        AND
133300030721     C                             PiVal<=999999999
133400030721     C                   Z-ADD     PiVal         VABANT
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABANT'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** CMO
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on
134600030721     C                   Z-ADD     PiVal         Num5_0
134700030721     C                   MOVEL(p)  Num5_0        VABCMO
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      VABCMO = *zeros
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABCMO'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500040510     C*
135600040510     C* Reperisco i campi del FIVAT
135700040510     C*** VATNOT
135800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
135900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
136000040510     C                   ENDIF
136100040510     C*** VATTRC
136200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
136300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
136400040510     C                   ENDIF
136500060331     C*** VATNOT_A
136600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
136700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
136800060331     C                   ENDIF
136900060331     C*** VATNOT_B
137000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
137100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
137200071106     C                   EVAL      VABNOT    = %trim(SkSplitCSV(i))
137300060331     C                   ENDIF
137400060331     C*** VATNOT_E
137500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
137600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
137700060331     C                   ENDIF
137800070823     C*
137900071029     C***  ===>  Gestione campi "particolari" dentro ciclo
138000071204     C*
138100071204     C*** NSP/RMN
138200071204     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
138300071204     C                   EVAL      PiStr=SkSplitCSV(i)
138400071204     C                   EXSR      CHKNUM
138500071204     C                   IF        PiInt=*on
138600071204     C                   Z-ADD     PiVal         VABNSP
138700071204     C                   Z-ADD     PiVal         VATNSP
138800071204     C                   Z-ADD     PiVal         VABRMN
138900071204     C                   ELSE
139000071204     C                   SETON                                        32
139100071204     C                   EVAL      VABNSP = *zeros
139200071204     C                   EVAL      VATNSP = *zeros
139300071204     C                   EVAL      VABRMN = *zeros
139400071204     C                   EVAL      vinmsg = %trimr(vinmsg)
139500071204     C                             + ' ' + 'VABNSP VATNSP VABRMN'
139600071204     C                   ENDIF
139700071204     C                   ENDIF
139800071204     C*
139900071204     C*** PKB
140000071204     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
140100071204     C                   EVAL      PiStr=SkSplitCSV(i)
140200071204     C                   EXSR      CHKNUM
140300071204     C                   IF        PiNum=*on
140400071204     C                   Z-ADD(H)  PiVal         VABPKB
140500071204     C                   ELSE
140600071204     C                   SETON                                        32
140700071204     C                   EVAL      VABPKB = *zeros
140800071204     C                   EVAL      vinmsg = %trimr(vinmsg)
140900071204     C                             + ' ' + 'VABPKB'
141000071204     C                   ENDIF
141100071204     C                   ENDIF
141200070925     C*
141300071029     C***  <===  -----------------------------------------
141400040510     C*
141500030715     C                   ENDIF
141600030715     C                   EVAL      i = i + 1
141700030715     C                   ENDDO
141800070925     C*
141900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142000070925     C*
142100071029     C***  <===  -----------------------------------------
142200070925     C*
142300070925     C* Considerazioni finali su CBO/CAS
142400070925     C                   IF        WFLGCAS = '1'
142500070925     C                   IF        VABCBO = '1'
142600070925     C                   EVAL      VABCBO = '4'
142700070925     C                   ENDIF
142800070925     C                   IF        VABCBO = '2'
142900070925     C                   EVAL      VABCBO = '6'
143000070925     C                   ENDIF
143100070925     C                   ENDIF
143200070925     C***  <===  ----------------------------
143300040510     C*
143400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
143500040510     C                   IF        VABNSP = *zeros
143600040510     C                   SETON                                        31
143700040510     C                   ENDIF
143800040715     C*
143900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
144000040715     C                   IF        VABLNP = *zeros
144100040715     C                   SETON                                        31
144200040715     C                   ENDIF
144300040510     C*
144400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
144500040510     C                   IF        VABRMN = *zeros
144600040510     C                   Z-ADD     VABNSP        VABRMN
144700040510     C                   ENDIF
144800040510     C*
144900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
145000040510     C                   IF        VABRMA = *blanks
145100040714     C                   MOVEL     VABRMN        VABRMA
145200040510     C                   ENDIF
145300040128     C*
145400040510     C* Se provincia nn valorizzata la reperisco
145500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
145600040510     C                   IF        VABPRD = *blanks
145700040128     C                   CLEAR                   TISI95DS
145800040128     C                   EVAL      I95TCN = '3'
145900040128     C                   Z-ADD     datcor        I95DAT
146000040128     C                   EVAL      I95NAR = VABNZD
146100040128     C                   EVAL      I95CAP = VABCAD
146200040128     C                   EVAL      I95LOC = VABLOD
146300040128     C                   CALL      'TISI95R'
146400040128     C                   PARM                    TISI95DS
146500040128     C                   EVAL      VABPRD = O95PRV
146600040510     C                   ENDIF
146700040113     C*
146800040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
146900040113     C                   IF        VABCAS > *zeros
147000040119     C                   EVAL      VABVCA = 'EUR'
147100040119     C                   IF        VABCBO = '1'
147200040113     C                   EVAL      VABCBO = '4'
147300040119     C                   ENDIF
147400040119     C                   IF        VABCBO = '2'
147500040119     C                   EVAL      VABCBO = '6'
147600040119     C                   ENDIF
147700040113     C                   ENDIF
147800020204     C*
147900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
148000020204     C                   EXSR      CHKIMPDIV
148100030715     C                   ENDIF
148200020204     C*
148300010607     C                   ENDSR
148400010607     C*----------------------------------------------------*
148500010601
148600020204
148700020204     C*----------------------------------------------------*
148800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
148900020204     C*----------------------------------------------------*
149000020204     C     CHKIMPDIV     BEGSR
149100020204     C*
149200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149300020204     C                   Z-ADD     *zeros        wrkDec            9 9
149400020204     C*
149500020204     C* Come prima cosa effettuo considerazioni sulla divisa
149600020204     C                   IF        vabIAS > *zeros
149700020204     C                   IF        vabVAS <> 'EUR'
149800020204     C                   EVAL      vabVAS =  'ITL'
149900020204     C                   ENDIF
150000020204     C                   ENDIF
150100020204     C*
150200020204     C                   IF        vabCAS > *zeros
150300020204     C                   IF        vabVCA <> 'EUR'
150400020204     C                   EVAL      vabVCA =  'ITL'
150500020204     C                   ENDIF
150600020204     C                   ENDIF
150700020204     C*
150800020204     C                   IF        vabVMD > *zeros
150900020204     C                   IF        vabVAD <> 'EUR'
151000020204     C                   EVAL      vabVAD =  'ITL'
151100020204     C                   ENDIF
151200020204     C                   ENDIF
151300020204     C*
151400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
151500020204     C                   Z-ADD     vabIAS        wrkDec
151600020204     C                   IF        wrkDec > *zeros
151700020204     C                   IF        vabVAS = 'ITL'
151800020204     C                   EVAL      vabIAS = *zeros
151900020204     C                   ENDIF
152000020204     C                   ENDIF
152100020204     C*
152200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152300020204     C                   Z-ADD     vabCAS        wrkDec
152400020204     C                   IF        wrkDec > *zeros
152500020204     C                   IF        vabVCA = 'ITL'
152600020204     C                   EVAL      vabCAS = *zeros
152700020204     C                   ENDIF
152800020204     C                   ENDIF
152900020204     C*
153000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153100020204     C                   Z-ADD     vabVMD        wrkDec
153200020204     C                   IF        wrkDec > *zeros
153300020204     C                   IF        vabVAD = 'ITL'
153400020204     C                   EVAL      vabVMD = *zeros
153500020204     C                   ENDIF
153600020204     C                   ENDIF
153700020204     C*
153800020204     C                   ENDSR
153900020204     C***
154000020204
154100010330
154200010330
154300010330     C*----------------------------------------------------*
154400010330     C*  CONTROLLO NUMERICITA' CAMPI
154500010330     C*----------------------------------------------------*
154600010330     C     CHKNUM        BEGSR
154700010330     C*
154800010606     C                   IF        PiDecChr = *blanks
154900030715     C                   EVAL      PiDecChr = CharNUM
155000010606     C                   ENDIF
155100010606     C*
155200010606     C                   CALL(e)   'ISNUMERIC'
155300010330     C                   PARM                    PiStr            30
155400010606     C                   PARM                    PiDecChr          1
155500010330     C                   PARM      *ZEROS        PiVal            30 9
155600010330     C                   PARM      '0'           PiInt             1
155700010330     C                   PARM      '0'           PiNum             1
155800010330     C                   IF        %error
155900010606     C                   EVAL      PiNum=*off
156000010330     C                   ENDIF
156100010330     C*
156200010330     C                   ENDSR
156300010330     C***
156400010330
156500010601
156600010601
156700010601
156800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156900010601     C     repfil        BEGSR
157000010601     C*
157100010601     C                   if        invfil = *zeros and
157200010601     C                             depfil > *zeros and
157300010629     C                             (vinflg = *blanks or
157400010629     C                              vinflg = *zeros)
157500010601     C
157600010601     C                   eval      invfil = depfil
157700010601     C                   endif
157800010601     C*
157900010601     C                   if        depfil <> invfil and
158000010601     C                             invfil > *zeros
158100010601     C                   eval      flgMulti = '1'
158200010601     C                   if        vinflg = *blanks
158300010601     C                   add       1             cntNonEl
158400010601     C                   endif
158500010601     C                   endif
158600010601     C*
158700010601     C                   if        vinflg = '2'
158800010601     C                   eval      flgStato = '2'
158900010601     C                   endif
159000010601     C*
159100010601     C                   ENDSR
159200010601     C***
159300010601
159400010601
159500010601
159600010330
159700040119      /TITLE Invio dei dati al punto operativo.
159800040119     C     invio         BEGSR
159900040119     C*
160000040119     C* 1° invio FIVAT
160100040119     C                   reset                   dscmz
160200040119     C                   move      vlrpoi        cmzdst
160300040119     C                   eval      cmzfld = 'FIVATWWR'
160400040119     C                   eval      cmzmbd = vlrhdl
160500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160600040119     C***                if        prmfir = *blanks
160700040119     C                   eval      cmzfla = 'FIVAT00F'
160800040119     C                   eval      cmzmba = 'FIVAT00F'
160900040119     C***                else
161000040119     C***                eval      cmzfla = prmfir
161100040119     C***                eval      cmzmba = prmfir
161200040119     C***                endif
161300040119     C                   eval      cmznrr = *zeros
161400040119     C                   move      §ctrokvt      cmznrr
161500040119     C                   eval      cmzlba = vlrfl1
161600040119     C                   call(e)   'TIS711C'
161700040119     C                   parm                    dscmz
161800040119     C                   parm      *blanks       esito
161900040119     C                   if        %error
162000040119     C                             or cmzerr = '1'
162100040119     C                             or esito  = '1'
162200040119     C                   eval      wrkesito = '3'
162300040119     C                   else
162400040119     C*
162500040119     C* 2° invio FIVAB
162600040119     C                   reset                   dscmz
162700040119     C                   move      vlrpoi        cmzdst
162800040119     C                   eval      cmzfld = vlrfou
162900040119     C                   eval      cmzmbd = vlrhdl
163000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163100040119     C***                if        prmfir = *blanks
163200040119     C                   eval      cmzfla = 'FIVAB00F'
163300040119     C                   eval      cmzmba = 'FIVAB00F'
163400040119     C***                else
163500040119     C***                eval      cmzfla = prmfir
163600040119     C***                eval      cmzmba = prmfir
163700040119     C***                endif
163800040119     C                   eval      cmznrr = *zeros
163900040119     C                   move      §ctrokvb      cmznrr
164000040119     C                   eval      cmzlba = vlrfl1
164100040119     C                   call(e)   'TIS711C'
164200040119     C                   parm                    dscmz
164300040119     C                   parm      *blanks       esito
164400040119     C                   if        %error
164500040119     C                             or cmzerr = '1'
164600040119     C                             or esito  = '1'
164700040119     C                   eval      wrkesito = '3'
164800040119     C                   endif
164900040119     C                   endif
165000040119     C*
165100040119     C                   ENDSR
165200040119     C***
165300010601
165400010601
165500010601
165600010601
165700010601      /TITLE Invio dei dati al punto operativo.
165800010601     C     opeini        BEGSR
165900010601     C*
166000010601     C* Inizializzo flag e contatori operativi
166100010601     C                   movel     '0'           flgGiro           1
166200010601     C                   movel     '0'           flgMulti          1
166300010601     C                   movel     '1'           flgStato          1
166400010615     C                   movel     '0'           flgOk             1
166500010601     C                   z-add     *zeros        cntNonEl         10 0
166600010601     C                   z-add     *zeros        depfil            3 0
166700010601     C                   z-add     *zeros        invfil            3 0
166800010601     C*
166900010601     C                   ENDSR
167000010601     C***
167100070326
167200070326
167300070326
167400070326
167500070326     C     *pssr         BEGSR
167600070329     C*
167700070329     C                   if        %open(tivin00r)
167800070329     C                   close     tivin00r
167900070329     C                   endif
168000070329     C                   if        %open(fivabwwr)
168100070329     C                   close     fivabwwr
168200070329     C                   endif
168300070329     C                   if        %open(fivatwwr)
168400070329     C                   close     fivatwwr
168500070329     C                   endif
168600070326     C*
168700070326     C* Effettuo la chiamata al CLLE preposto
168800070326     C                   call(e)   'TITVVTC'
168900070326     C                   parm                    parccm
169000070326     C                   parm                    parmbr
169100070326     C                   parm      '2'           paropz
169200070326     C*
169300070326     C                   eval      wrkesito = '2'
169400070404     C*
169500070404     C                   seton                                        LR
169600070326     C*
169700070326     C                   ENDSR     '*CANCL'
169800070326     C***
169900070326
170000070326
170100010330
170200010330
170300000613     C     *inzsr        BEGSR
170400990910     C*
170500990910     C     *entry        plist
170600990920     C                   parm                    tivlrds
170700990921     C                   parm      wrkesito      esito
170800000724     C                   parm                    prmlit
170900000710     C                   parm                    prmfir
171000010330     C*
171100010330     C* CALCOLA LA DATA CORRENTE
171200010330     C                   time                    wn14             14 0
171300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
171400010330     C                   z-add     wn8           g08dat
171500010330     C                   z-add     *zeros        g08inv
171600010330     C                   movel     '0'           g08err
171700010330     C                   call      'XSRDA8'
171800010330     C                   parm                    wlbda8
171900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
172000000613     C*
172100000613     C                   ENDSR
172200000613     C***
172300990908
172400071204     Otitv1c6p  E            riepilogo         2
172500990915     O                                              'Upload via Internet'
172600990915     O                                           +1 'Traduzione TIVIN00R -'
172700021025     O                                           +1 'FIVABWWR'
172800990915     O                                           +1 'Testate bolle'
172900990915     O          E            riepilogo   2
173000990915     O                       wrkdata
173100990915     O                       wrkora              +1
173200990915     O                       procname            +1
173300990915     O          E            riepilogo   2
173400990915     O                                              'Cliente..................:'
173500010605     O                       VABCCM        z     +1
173600990915     O          E            riepilogo   2
173700990920     O                                              'Riferimento Strategi.....:'
173800990920     O                       vlrhdl              +1
173900990915     O          E            riepilogo   2
174000990915     O                                              'Giusti...................:'
174100040119     O                       §CTROKVB      2   +  1
174200990915     O          E            riepilogo   2
174300990915     O                                              'Sbagliati e corretti.....:'
174400971022     O                       §CTRMO        2   +  1
174500990915     O          E            riepilogo   2
174600990915     O                                              'Sbagliati e scartati.....:'
174700971022     O                       §CTRNO        2   +  1
174800000613
174900071204     Otitv1c6ps E            testdett          2
175000000613     O                                              'Upload via Internet'
175100000613     O                                           +1 'Traduzione TIVIN00R -'
175200021025     O                                           +1 'FIVABWWR'
175300000613     O                                           +1 'Testate bolle'
175400000616     O          E            testdett    3
175500000613     O                                           +2 'N° rec'
175600000613     O                                           +3 'Anteprima contenuto'
175700000616     O          E            rigadett    2
175800000613     O                       rrnum               +2
175900000621     O                       recko               +3
176000000616     O          E            findett     2
176100000613     O                       wrkdata
176200000613     O                       wrkora              +1
176300000613     O                       procname            +1
176400000616     O          E            findett     2
176500000613     O                                              'Cliente..................:'
176600010605     O                       VABCCM        z     +1
176700000616     O          E            findett     2
176800000613     O                                              'Riferimento Strategi.....:'
176900000613     O                       vlrhdl              +1
177000000616     O          E            findett     2
177100000613     O                                              'Giusti...................:'
177200040119     O                       §CTROKVB      2   +  1
177300000616     O          E            findett     2
177400000613     O                                              'Sbagliati e corretti.....:'
177500000613     O                       §CTRMO        2   +  1
177600000616     O          E            findett     2
177700000613     O                                              'Sbagliati e scartati.....:'
177800000613     O                       §CTRNO        2   +  1
177900000613** CMD - COMANDI CL
178000071204OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1C6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
178100071204OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1C6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
178200071204DLTOVR FILE(TITV1C6P TITV1C6PS) LVL(*)
178300010606
178400000613
178500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178600071204FILL1;FILL2;FILL3;VABRMN;FILL4;FILL5;FILL6;VABRMA;VABRSD;FILL7;VABIND;
178700071204FILL8;FILL9;VABLOD;FILL10;VABCAD;VABRD2;FILL11;VATNOT_E;VABPKB;FILL12;
178800071204FILL13;FILL14;FILL15;VABNCL;FILL16;FILL17
