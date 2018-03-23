000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800090420     Ftitv1p0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100090420     Ftitv1p0ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300080916     F                                     infsr(*pssr)
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
003700100115     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040510     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500080125     D wFlgCAS         s              1    INZ(*blanks)
004600080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004700030715     D*------------------
004800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900030715     D*------------------
005000030715     D SkSplitFLD      S             10    DIM(100)
005100030715     D SkSplitCSV      S            256    DIM(100)
005200030715     D CharCSV         S              1
005300030715     D CharTXT         S              1
005400030715     D CharNUM         S              1
005500030715     D posDa           S              3  0 INZ(*zeros)
005600030715     D posA            S              3  0 INZ(*zeros)
005700030715     D i               s              3  0 INZ(1)
005800030715     D wGiro           s              1  0 INZ(*zeros)
005900080125     D wVATNOT_A       s                   LIKE(VATNOT)
006000080125     D wVATNOT_B       s                   LIKE(VATNOT)
006100080125     D wVATNOT_E       s                   LIKE(VATNOT)
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
008200080617     D*------------------
008300100115
008400100115     D*------------------
008500100115     D* DS REPERIMENTO NUMERATORE
008600100115     D*------------------
008700100115     D trul33ds      e ds                  inz
008800100115     D*------------------
008900100115     D* DS ARCHITETTURA
009000100115     D*------------------
009100100115     D kpjba         e ds                  inz
009200100115
009300080617
009400080617     D*------------------
009500080617     D* LINKING A DEFINIZIONI ESTERNE
009600080617     D*------------------
009700080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009800090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009900010330
010000010330
010100010330
010200990915     C                   time                    wrkdata
010300990915     C                   time                    wrkora
010400100115     C                   reset                   rrnum
010500990921     C                   reset                   esito
010600081014     C                   reset                   wrkesito
010700010601     C*
010800010601     C                   exsr      opeini
010900010605     C                   exsr      rwvab
011000081014     C*
011100081014     C                   exsr      endela
011200010601     C*
011300010601     C                   seton                                        lr
011400010601
011500010601
011600010601
011700010601     C*--------------------------------------------------------
011800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011900010601     C*--------------------------------------------------------
012000010601     C     PREELA        BEGSR
012100010601     C*
012200010601     C* SE OCCORRE SPEDIRE IN FILIALE
012300010601     C                   if        invfil <> *zeros and
012400010601     C                             flgGiro = '0'
012500010601     C*
012600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012700010601     C                   eval      flgGiro = '1'
012800000724     C*
012900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013100010601     C     invfil        chain     azorg01l
013200000724     C                   if        %found
013300000724     C                   movel(p)  CMD(1)        depcmd
013400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013500000724     C*
013600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013700011113     C                   Z-ADD     150           LENGH            15 5
013800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013900000724     C                   PARM                    depcmd
014000000724     C                   PARM                    LENGH
014100000724     C*
014200000724     C                   endif
014300000616     C*
014400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014500000616     C                   movel(p)  CMD(2)        depcmd
014600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014700000616     C*
014800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014900011113     C                   Z-ADD     150           LENGH            15 5
015000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015100000616     C                   PARM                    depcmd
015200000616     C                   PARM                    LENGH
015300000613     C*
015400090420     C                   if        not %open(titv1p0ps)
015500090420     C                   open      titv1p0ps
015600070329     C                   except    testdett
015700000613     C                   endif
015800010601     C*
015900010601     C                   endif
016000010601     C*
016100010601     C                   ENDSR
016200010601     C***
016300010601
016400010601
016500010601
016600010601
016700010601     C*--------------------------------------------------------
016800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016900010601     C*--------------------------------------------------------
017000010601     C     ENDELA        BEGSR
017100081014     C*
017200081014     C                   if        %open(tivin00r)
017300081014     C                   close     tivin00r
017400081014     C                   endif
017500081014     C                   if        %open(fivabwwr)
017600081014     C                   close     fivabwwr
017700081014     C                   endif
017800081014     C                   if        %open(fivatwwr)
017900081014     C                   close     fivatwwr
018000081014     C                   endif
018100040119     C*
018200040119     C* Effettuo la chiamata al CLLE preposto
018300040119     C                   call(e)   'TITVVTC'
018400040510     C                   parm                    parccm
018500040119     C                   parm                    parmbr
018600040119     C                   parm      '2'           paropz
018700050201     C*
018800050201     C* Effettuo lancio TISI95 solo x chiusura
018900050201     C                   CLEAR                   TISI95DS
019000050201     C                   EVAL      I95TLA = 'C'
019100050201     C                   CALL      'TISI95R'
019200050201     C                   PARM                    TISI95DS
019300000613     C*
019400090420     C                   if        %open(titv1p0ps)
019500000616     C                   except    findett
019600090420     C                   close     titv1p0ps
019700000613     C                   endif
019800000616     C*
019900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020000011113     C                   Z-ADD     150           LENGH            15 5
020100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020200000616     C                   PARM                    CMD(3)
020300000616     C                   PARM                    LENGH
020400000616     C*
020500010601     C                   ENDSR
020600010601     C***
020700010601
020800010601
020900010601
021000000613
021100000613     C*--------------------------------------------------------
021200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021300000613     C*--------------------------------------------------------
021400000621     C     STPR          BEGSR
021500000613     C*
021600090420     C                   if        not %open(titv1p0p)
021700090420     C   50              open      titv1p0p
021800990915     C                   endif
021900990915     C*
022000030715     C   50              except    riepilogo
022100990915     C*
022200090420     C                   if        %open(titv1p0p)
022300090420     C   50              close     titv1p0p
022400990914     C                   endif
022500000613     C*
022600000613     C                   ENDSR
022700000613     C***
022800000613
022900010601
023000010601
023100010330     C*--------------------------------------------------------
023200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023300010330     C*--------------------------------------------------------
023400010605     C     RWVAB         BEGSR
023500010330     C*
023600010330     C                   if        not %open(tivin00r)
023700010330     C                   open      tivin00r
023800010330     C                   endif
023900021025     C                   if        not %open(fivabwwr)
024000021025     C                   open      fivabwwr
024100010330     C                   endif
024200040119     C*
024300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024400040119     C                   exsr      prevat
024500040119     C*
024600040119     C                   if        chkcall = '0'
024700040119     C*
024800040119     C                   if        not %open(fivatwwr)
024900040119     C                   open      fivatwwr
025000040119     C                   endif
025100010330     C*
025200040119     C                   clear                   §CTROKVB          7 0
025300040119     C                   clear                   §CTROKVT          7 0
025400040119     C                   clear                   §CTRMO            7 0
025500040119     C                   clear                   §CTRNO            7 0
025600070823     C*
025700070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025800070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025900090420     C                   exsr      inzvar
026000090420     C                   exsr      impvab
026100010330     C*
026200030822     C                   do        *HIVAL
026300010330     C*
026400030822     C                   read      tivin00r                               70
026500010618     C*
026600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026700010618     C                   if        vinflg = '1'
026800010618     C                   eval      flgOk = '1'
026900010618     C                   endif
027000040510     C*
027100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027200040510     C                   z-add     *zeros        wLenVINDTA        4 0
027300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027700040510     C                   z-add     1             wFlgVINDTA
027800040510     C                   leave
027900040510     C                   endif
028000040510     C                   enddo
028100090420     C*
028200090420     C*****
028300090420     C* Escludo la prima riga
028400090420     C                   if        %subst(vindta:1:2) = 'R|'
028500090420     C                   z-add     *zeros        wFlgVINDTA
028600090420     C                   endif
028700090420     C*****
028800100115     C*
028900100115     C                   add       1             rrnum
029000010618     C*
029100040510     C                   if        vindta > *blanks AND
029200090420     C                             wFlgVINDTA = 1
029300010330     C*
029400010601     C                   if        *in70 = *off and
029500010330     C                             (vinflg = *blanks
029600010330     C                              or vinflg = '0'
029700010330     C                              or vinflg = '2')
029800010330     C*
029900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
030000010711     C                   if        vinflg = *blanks or vinflg = '0'
030100010711     C                   clear                   vinmsg
030200010711     C                   endif
030300010601     C*
030400090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
030500090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
030600090421     C                   eval      vindta  = '*' + %trim(vindta)
030700090421     C                   endif
030800070925     C*
030900010330     C                   exsr      inzvar
031000010605     C                   exsr      impvab
031100010601     C*
031200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
031300010605     C                   eval      depfil = VABLNP
031400010601     C                   exsr      repfil
031500010601     C                   if        depfil = invfil
031600021025     C                   if        vlrpoi = 999
031700030822     C                   move(P)   invfil        VABFGS
031800021025     C                   else
031900030822     C                   move(P)   vlrpoi        VABFGS
032000021025     C                   endif
032100010601     C*
032200010601     C                   exsr      PREELA
032300010601     C*
032400010604     C* Ebbene...
032500010604     C*
032600040119     C  N31              add       1             §CTROKVB
032700040119     C   32              add       1             §CTRMO
032800040119     C   31              add       1             §CTRNO
032900030822     C                   if        wGiro = 2
033000040510     C* Scrivo sempre il VAT
033100040510     C  N31              exsr      wrivat
033200040510     C* Scrivo il VAB solo a rottura d spedizione
033300040119     C                   if        depspe =  *zeros OR
033400040119     C                             depspe <> vabNSP
033500040119     C  N31              exsr      wrivab
033600040119     C  N31              eval      depspe = vabNSP
033700040119     C                   endif
033800040119     C                   endif
033900010604     C*
034000010604     C                   if        *in31 = *off and
034100010604     C                             *in32 = *off
034200010604     C                   eval      vinflg = '1'
034300010604     C                   else
034400010604     C                   eval      recko = vindta
034500010604     C                   except    rigadett
034600010604     C                   eval      vinflg = '2'
034700010604     C                   endif
034800010604     C                   endif
034900010601     C*
035000010604     C                   endif
035100010604     C*
035200010330     C                   else
035300010330     C                   eval      vinflg = '1'
035400010330     C                   endif
035500010601     C*
035600010601     C  N70              update    tivin000
035700010330     C*
035800030822     C  N70              enddo
035900040119     C*
036000040119     C                   endif                                                  (endif - chkcall)
036100010601     C*
036200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
036300010601     C                   if        cntNonEl = *zeros or
036400010601     C                             flgMulti = '0'
036500010330     C* Se non ci sono record con errori ...
036600010601     C                   if        §ctrno = 0 and
036700010604     C                             §ctrmo = 0 and
036800010601     C                             flgStato <> '2'
036900010330     C* ... restituisco esito OK.
037000010330     C                   eval      wrkesito = '0'
037100010330     C                   else
037200040119     C                   if        §ctrokvb > 0
037300010330     C                   eval      wrkesito = '1'
037400010330     C                   else
037500010615     C                   if        flgOk = '0'
037600010615     C                   eval      wrkesito = '2'
037700010615     C                   else
037800010615     C                   eval      wrkesito = '6'
037900010615     C                   endif
038000010330     C                   endif
038100010330     C                   endif
038200010601     C                   else
038300010601     C                   eval      wrkesito = '9'
038400010601     C                   endif
038500010330     C*
038600010330     C                   if        %open(tivin00r)
038700010330     C                   close     tivin00r
038800010330     C                   endif
038900021025     C                   if        %open(fivabwwr)
039000021025     C                   close     fivabwwr
039100010330     C                   endif
039200040512     C                   if        %open(fivatwwr)
039300040512     C                   close     fivatwwr
039400040512     C                   endif
039500010601     C*
039600010601     C                   if        vlrpoi <> 999
039700010601     C                   eval      invfil = vlrpoi
039800010601     C                   endif
039900010330     C*
040000040119     C                   if        §ctrokvb > 0
040100010601     C                             and invfil > *zeros
040200010330     C                   exsr      invio
040300010330     C                   endif
040400010601     C*
040500010618     C                   if        flgGiro = '1'
040600081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
040700010618     C                   endif
040800010330     C*
040900010330     C                   ENDSR
041000010330     C***
041100040119
041200040119
041300040119
041400040119     C*----------------------------------------------------*
041500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
041600040119     C*----------------------------------------------------*
041700040510     C     WRIVAB        BEGSR
041800040119     C*
041900060331     C* Gestisco l'eventuale rottura x numero spedizione
042000070823     C                   if        VABNSP <> depspe
042100070301     C                   if        wVATNOT_E <> *blanks
042200080125     C                   if        VABCTM = *blanks
042300080125     C                   eval      VABCTM = '7Q'
042400080125     C                   endif
042500070301     C                   endif
042600040119     C                   write     fivab000                                     => scarico il VAB
042700060331     C                   endif
042800040119     C*
042900040119     C                   ENDSR
043000040119
043100040119
043200040119
043300040119     C*----------------------------------------------------*
043400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
043500040119     C*----------------------------------------------------*
043600040510     C     WRIVAT        BEGSR
043700060512     C*
043800060512     C* Inizializzo i campi chiave
043900060512     C                   eval      VATFGS = VABFGS
044000060512     C                   eval      VATAAS = VABAAS
044100060512     C                   eval      VATCCM = VABCCM
044200070823     C                   eval      VATNRS = VABNRS
044300060512     C                   eval      VATNSP = VABNSP
044400060512     C                   eval      VATLNP = VABLNP
044500060331     C*
044600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
044700060331     C                   if        wVATNOT_A <> *blanks
044800060331     C                   eval      VATNOT = wVATNOT_A
044900060331     C                   eval      VATTRC = 'A'
045000060331     C                   write     FIVAT000
045100060331     C                   add       1             §CTROKVT
045200060331     C                   endif
045300060331     C*
045400060331     C                   if        wVATNOT_B <> *blanks
045500060331     C                   eval      VATNOT = wVATNOT_B
045600060331     C                   eval      VATTRC = 'B'
045700060331     C                   write     FIVAT000
045800060331     C                   add       1             §CTROKVT
045900060331     C                   endif
046000060331     C*
046100080625     C                   if        wVATNOT_E <> *blanks
046200060331     C                   eval      VATNOT = wVATNOT_E
046300060331     C                   eval      VATTRC = 'E'
046400080716     C                   write     FIVAT000
046500080716     C                   add       1             §CTROKVT
046600080625     C                   endif
046700040119     C*
046800040119     C                   ENDSR
046900040119
047000040119
047100040119
047200040119     C*----------------------------------------------------*
047300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047400040119     C*----------------------------------------------------*
047500040119     C     PREVAT        BEGSR
047600040119     C*
047700040119     C* Compongo il nome del membro da dare al FIVATWWR
047800040119     C                   eval      parmbr = vlrhdl
047900040119     C                   movel     'M'           parmbr
048000040510     C                   eval      parccm = vlrksc
048100040119     C                   eval      paropz = '1'
048200040119     C* Effettuo la chiamata al CLLE preposto
048300040119     C                   call(e)   'TITVVTC'
048400040119     C                   parm                    parccm
048500040119     C                   parm                    parmbr
048600040119     C                   parm                    paropz
048700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048800040119     C                   if        %error
048900040119     C                   movel     '1'           chkcall
049000040119     C                   else
049100040119     C                   movel     '0'           chkcall
049200040119     C                   endif
049300040119     C*
049400040119     C                   ENDSR
049500010601
049600010601
049700010601
049800010330     C*----------------------------------------------------*
049900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050000010330     C*----------------------------------------------------*
050100040119     C     INZVAR        BEGSR
050200010330     C*
050300030822     C* Inizializzo variabili di wrk
050400020204     C                   Z-ADD     *zeros        Num5_0            5 0
050500060331     C                   MOVEL     *blanks       wVATNOT_A
050600060331     C                   MOVEL     *blanks       wVATNOT_B
050700060331     C                   MOVEL     *blanks       wVATNOT_E
050800090220     C*
050900080617     C                   EVAL      wFlgCAS = *blanks
051000030822     C*
051100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051200030822     C                   CLEAR                   FIVAB000
051300040119     C                   CLEAR                   FIVAT000
051400030822     C                   CLEAR                   SkSplitCSV
051500030822     C                   EVAL      i = 1
051600030822     C                   EVAL      posDa = *zeros
051700030822     C                   EVAL      posA  = *zeros
051800030822     C*
051900030822     C* Reimposto i valori di default
052000030822     C                   EXSR      DEFCAM
052100030822     C*
052200010330     C                   ENDSR
052300010330     C*----------------------------------------------------*
052400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052500010330     C*----------------------------------------------------*
052600010330     C     DEFCAM        BEGSR
052700030822     C*
052800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052900030715     C* e delimitatore testo.
053000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053300030822     C*
053400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
053600030715     C                   SETON                                        50
053700030715     C                   ELSE
053800030715     C                   SETOFF                                       50
053900030715     C                   ENDIF
054000090421     C*
054100100115     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio
054200090421     C                   EVAL      VABCBO = '1'
054300090421     C                   EVAL      VABLNP = vlrpoi
054400100115     C                   EVAL      VABNSP = rrnum
054500100115     C*
054600100115     C* NSP => Stacco un numeratore da AZNUM
054700100115     C                   clear                   TRUL33DS
054800100115     C                   eval      I33OPE = *zeros
054900100115     C                   eval      I33CNU = 302
055000100115     C                   eval      I33NUM = 1
055100100115     C                   movel     TRUL33DS      KPJBU
055200100115     C                   call      'TRUL33R'
055300100115     C                   parm                    KPJBA
055400100115     C                   movel     KPJBU         TRUL33DS
055500100115     C                   if        O33ERR = *zeros
055600100115     C                   z-add     O33NRF        VABNSP
055700100115     C                   z-add     O33NRF        VATNSP
055800100115     C                   else
055900100115     C                   SETON                                        31
056000100115     C                   EVAL      vinmsg = %trimr(vinmsg)
056100100115     C                             + ' ' + 'VABNSP VATNSP'
056200100115     C                   endif
056300030822     C*
056400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056500030822     C                   EVAL      posDaDft = 1
056600030822     C                   EVAL      posADft  = 0
056700030822     C                   EVAL      wGiroDft = 0
056800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056900030822     C                             posDaDft > 0
057000030822     C*
057100030822     C* Gestisco il 1° giro
057200030822     C                   IF        wGiroDft = 0
057300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057500030822     C* Incremento il contatore dei "giri"
057600030822     C                   EVAL      wGiroDft = 1
057700030822     C                   ELSE
057800030822     C                   EVAL      posDaDft = posADft
057900030822     C                   ENDIF
058000030822     C* Eseguo lo scan x trovare la fine del campo corrente
058100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058200030822     C*
058300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058500030822     C                   IF        posDaDft > 0 AND
058600030822     C                             posADft  > 0
058700030822     C* NCL
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'NCL'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABNCL
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* CCM
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'CCM'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABCCM
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* LNP
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'LNP'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABLNP
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* NRS
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'NRS'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABNRS
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* CTR
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'CTR'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiInt=*on
064900030822     C                   Z-ADD     PiVal         VABCTR
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* PKB
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'PKB'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiNum=*on
066200030822     C                   Z-ADD     PiVal         VABPKB
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* VLB
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'VLB'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiNum=*on
067500030822     C                   Z-ADD     PiVal         VABVLB
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* QFT
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'QFT'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiNum=*on
068800030822     C                   Z-ADD     PiVal         VABQFT
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100030822     C* CBO
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'CBO'
069600030822     C                   EVAL      VABCBO=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* TSP
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'TSP'
070500030822     C                   EVAL      VABTSP=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* VAS
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'VAS'
071400030822     C                   EVAL      VABVAS=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* VCA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VCA'
072300030822     C                   EVAL      VABVCA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700070925     C* TIC
072800070925     C                   IF        %subst(
072900070925     C                             %subst(vlrppt:posDaDft+1:
073000070925     C                             posADft-posDaDft-1):1:3)
073100070925     C                             = 'TIC'
073200070925     C                   EVAL      VABTIC=%trim(%subst(
073300070925     C                             %subst(vlrppt:posDaDft+1:
073400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500070925     C                   ENDIF
073600030822     C* GCA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GCA'
074100030822     C                   EVAL      VABGCA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* CTM
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'CTM'
075000030822     C                   EVAL      VABCTM=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* FFD
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'FFD'
075900030822     C                   EVAL      VABFFD=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* VAD
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'VAD'
076800030822     C                   EVAL      VABVAD=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* GMA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GMA'
077700030822     C                   EVAL      VABGMA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* GGA
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'GGA'
078600030822     C                   EVAL      VABGGA=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* GVA
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'GVA'
079500030822     C                   EVAL      VABGVA=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* TC1
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'TC1'
080400030822     C                   EVAL      VABTC1=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* TC2
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'TC2'
081300030822     C                   EVAL      VABTC2=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700040714     C* VATTRC
081800040714     C                   IF        %subst(
081900040714     C                             %subst(vlrppt:posDaDft+1:
082000040714     C                             posADft-posDaDft-1):1:3)
082100040714     C                             = 'TRC'
082200040714     C                   EVAL      VATTRC=%trim(%subst(
082300040714     C                             %subst(vlrppt:posDaDft+1:
082400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500040714     C                   ENDIF
082600030822     C* ...
082700030822     C                   ENDIF
082800030822     C                   ENDDO
082900020204     C*
083000010330     C                   ENDSR
083100010607     C*----------------------------------------------------*
083200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083300010607     C*----------------------------------------------------*
083400010607     C     IMPVAB        BEGSR
083500010607     C*
083600010607     C                   SETOFF                                       3132
083700030515     C*
083800010607     C                   MOVEL     datcor        VABAAS
083900010607     C                   MOVE      datcor        VABMGS
084000010607     C*
084100030715     C**********
084200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084300030715     C**********
084400030715     C                   IF        wGiro = *zeros
084500080716     C*
084600080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
084700080716     C                   EVAL      depvinDTA = vinDTA
084800040113     C*
084900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085100040510     C                             %trim(intcol(3))<>*blanks
085200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085300040113     C                                      %trim(intcol(3))
085400070823     C     ';':CharCSV   XLATE     vindta        vindta
085500040510     C                   ENDIF
085600080626     C*
085700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085800031201     C     minu:maiu     XLATE     vindta        vindta
085900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086000030715     C                   DOW       posDa <= %len(%trim(vindta))
086100030715     C*
086200030715     C* Gestisco il 1° campo
086300030715     C                   IF        i = 1
086400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086500030715     C                   EVAL      posDa = 1
086600030715     C* Eseguo lo scan x trovare la fine del primo campo
086700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086800030715     C* A questo punto "estraggo" il campo corrente
086900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087000030715     C                                                    (posA-posDa))
087100030715     C* X i campi successivi al 1°
087200030715     C                   ELSE
087300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087400030715     C                   EVAL      posDa = posA + 1
087500030715     C* Eseguo lo scan x trovare la fine del campo corrente
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* Gestisco l'ultimo campo
087800030715     C                   IF        posA = *zeros
087900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   LEAVE
088400030715     C                   ELSE
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C                   ENDIF
088900030715     C                   ENDIF
089000030715     C* Incremento il contatore d campo
089100030715     C                   EVAL      i = i +1
089200030715     C                   ENDDO
089300030715     C*
089400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089500030715     C* delimitazione testo
089600030715     C                   EVAL      i = 1
089700030715     C                   DOW       i < %elem(SkSplitFLD)
089800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090000030715     C                   EVAL      i = i + 1
090100030715     C                   ENDDO
090200030715     C                   EVAL      wGiro = 1
090300080716     C* Ripristino la 1° riga originale
090400080716     C                   EVAL      vinDTA = depvinDTA
090500030715     C                   ELSE
090600030715     C                   EVAL      wGiro = 2
090700031204     C**********
090800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090900031204     C* d separatore campo
091000031204     C**********
091100031204     C                   EVAL      vindta = %trim(vindta)
091200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091300031204     C                   EVAL      vindta = %subst(vindta:2)
091400031204     C                   ENDDO
091500031204     C*
091600031204     C                   Z-ADD     *zeros        lunghInput        4 0
091700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092100031204     C                   ENDDO
092200030715     C**********
092300030715     C* Effettuo lo split del campo dati d input
092400030715     C**********
092500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092600030715     C                   DOW       posDa <= %len(%trim(vindta))
092700030715     C*
092800030715     C* Gestisco il 1° campo
092900030715     C                   IF        i = 1
093000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093100030715     C                   EVAL      posDa = 1
093200030715     C* Eseguo lo scan x trovare la fine del primo campo
093300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093400030715     C* A questo punto "estraggo" il campo corrente
093500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093600030715     C                                                    (posA-posDa))
093700030715     C* X i campi successivi al 1°
093800030715     C                   ELSE
093900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094000030715     C                   EVAL      posDa = posA + 1
094100031204     C* Verifico che nn vi sia il campo nn valorizzato
094200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094300031204     C* Se campo nn valorizzato skippo al prossimo
094400031204     C                   EVAL      SkSplitCSV(i) = *blanks
094500031204     C                   EVAL      posA  = posA + 1
094600031204     C                   ELSE
094700030715     C* Eseguo lo scan x trovare la fine del campo corrente
094800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094900030715     C* Gestisco l'ultimo campo
095000030715     C                   IF        posA = *zeros
095100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095200030715     C* A questo punto "estraggo" il campo corrente
095300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095400030715     C                                                    (posA-posDa))
095500030715     C                   LEAVE
095600030715     C                   ELSE
095700030715     C* A questo punto "estraggo" il campo corrente
095800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095900030715     C                                                    (posA-posDa))
096000030715     C                   ENDIF
096100031204     C                   ENDIF
096200030715     C                   ENDIF
096300030715     C* Incremento il contatore d campo
096400030715     C                   EVAL      i = i +1
096500030715     C                   ENDDO
096600030715     C*
096700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096800030715     C* delimitazione testo
096900030715     C                   EVAL      i = 1
097000030715     C                   DOW       i < %elem(SkSplitCSV)
097100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097300030715     C                   EVAL      i = i + 1
097400030715     C                   ENDDO
097500030715     C*
097600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097700030715     C                   EVAL      i = 1
097800030715     C                   DOW       i < %elem(SkSplitFLD)
097900030715     C*
098000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098200030715     C                             SkSplitCSV(i) <> *blanks
098300030721     C*** ATB
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098500030721     C                   EVAL      VABATB = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030715     C*** RSD
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099200030715     C* ==
099300030715     C                   ENDIF
099400030721     C*** RD2
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030715     C*** IND
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100000030715     C                   EVAL      VABIND = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** LOD
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** PRD
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030721     C*** NZD
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030715     C*** RMA
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101700030715     C                   ENDIF
101800030715     C*** NOT
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102100030715     C                   ENDIF
102200030715     C*** NT2
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102500030715     C                   ENDIF
102600030715     C*** VCA
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030715     C*** VAS
103100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103300030715     C                   ENDIF
103400030715     C*** FFD
103500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103700030715     C                   ENDIF
103800030721     C*** GC1
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GC2
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** TSP
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104900030721     C                   ENDIF
105000040510     C*** CBO
105100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105300040510     C                   ENDIF
105400030721     C*** NAS
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** TIC
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GCA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** XCO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** CTM
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** TCR
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** CTS
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** FTM
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** VAD
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** GMA
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** GGA
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** GVA
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** TC1
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** TC2
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** SCL
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** RMO
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** NMO
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*
112300030721     C* Reperisco quindi i campi numerici...
112400040510     C*** NSP
112500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112600040510     C                   EVAL      PiStr=SkSplitCSV(i)
112700040510     C                   EXSR      CHKNUM
112800040510     C                   IF        PiInt=*on
112900040510     C                   Z-ADD     PiVal         VABNSP
113000040510     C                   ELSE
113100040510     C                   SETON                                        31
113200040510     C                   EVAL      VABNSP = *zeros
113300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040510     C                             + ' ' + 'VABNSP'
113500040510     C                   ENDIF
113600040510     C                   ENDIF
113700071106     C*** RMN
113800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113900071106     C                   EVAL      PiStr=SkSplitCSV(i)
114000071106     C                   EXSR      CHKNUM
114100071106     C                   IF        PiInt=*on
114200071106     C                   Z-ADD     PiVal         VABRMN
114300071106     C                   ELSE
114400071106     C                   SETON                                        32
114500071106     C                   EVAL      VABRMN = *zeros
114600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114700071106     C                             + ' ' + 'VABRMN'
114800071106     C                   ENDIF
114900071106     C                   ENDIF
115000030721     C*** CAD
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on
115500030721     C                   Z-ADD     PiVal         Num5_0
115600030721     C                   MOVEL(p)  Num5_0        VABCAD
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABCAD = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABCAD'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** NCL
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on
116900030721     C                   Z-ADD     PiVal         VABNCL
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABNCL = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400040122     C                             + ' ' + 'VABNCL'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** PKB
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiNum=*on
118200030721     C                   Z-ADD     PiVal         VABPKB
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABPKB = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABPKB'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** CAS
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119200080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
119300080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
119400081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
119500081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
119600080125     C                   EVAL      wFlgCAS = '1'
119700080125     C                   ENDIF
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiNum=*on
120100030721     C                   Z-ADD     PiVal         VABCAS
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      VABCAS = *zeros
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABCAS'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** CCM
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on      AND
121400030721     C                             PiVal<=9999999 AND
121500030721     C                             PiVal>*zeros
121600030721     C                   Z-ADD     PiVal         VABCCM
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      VABCCM = *zeros
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100040510     C                             + ' ' + 'VABCCM'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** LNP
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on    AND
122900030721     C                             PiVal<=999   AND
123000030721     C                             PiVal>*zeros
123100030721     C                   Z-ADD     PiVal         VABLNP
123200030721     C                   ELSE
123300040715     C                   SETON                                        32
123400030721     C                   EVAL      VABLNP = *zeros
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600040510     C                             + ' ' + 'VABLNP'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** NRS
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on AND
124400030721     C                             PiVal<=99
124500030721     C                   Z-ADD     PiVal         VABNRS
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABNRS'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** LNA
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on    AND
125700030721     C                             PiVal<=999   AND
125800030721     C                             PiVal>*zeros
125900030721     C                   Z-ADD     PiVal         VABLNA
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      VABLNA = *zeros
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABLNA'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** CTR
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on  AND
127200030721     C                             PiVal<=999
127300030721     C                   Z-ADD     PiVal         VABCTR
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABCTR'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** IAS
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiNum=*on
128500030721     C                   Z-ADD     PiVal         VABIAS
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABIAS'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** VLB
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiNum=*on
129700030721     C                   Z-ADD     PiVal         VABVLB
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABVLB'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** QFT
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiNum=*on
130900030721     C                   Z-ADD     PiVal         VABQFT
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABQFT'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** NCD
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on      AND
132100030721     C                             PiVal<=9999999
132200030721     C                   Z-ADD     PiVal         VABNCD
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABNCD'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** NCA
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiInt=*on      AND
133400030721     C                             PiVal<=9999999
133500030721     C                   Z-ADD     PiVal         VABNCA
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABNCA'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** ZNC
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiInt=*on AND
134700030721     C                             PiVal<=99
134800030721     C                   Z-ADD     PiVal         VABZNC
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABZNC'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** DCR
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on       AND
136000030721     C                             PiVal<=99999999
136100030721     C                   Z-ADD     PiVal         VABDCR
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABDCR'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** HCR
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on   AND
137300030721     C                             PiVal<=9999
137400030721     C                   Z-ADD     PiVal         VABHCR
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABHCR'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** VMD
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiNum=*on
138600030721     C                   Z-ADD     PiVal         VABVMD
138700030721     C                   ELSE
138800030721     C                   SETON                                        32
138900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139000030721     C                             + ' ' + 'VABVMD'
139100030721     C                   ENDIF
139200030721     C                   ENDIF
139300030721     C*** ANT
139400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139500030721     C                   EVAL      PiStr=SkSplitCSV(i)
139600030721     C                   EXSR      CHKNUM
139700030721     C                   IF        PiInt=*on        AND
139800030721     C                             PiVal<=999999999
139900030721     C                   Z-ADD     PiVal         VABANT
140000030721     C                   ELSE
140100030721     C                   SETON                                        32
140200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140300030721     C                             + ' ' + 'VABANT'
140400030721     C                   ENDIF
140500030721     C                   ENDIF
140600030721     C*** CMO
140700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140800030721     C                   EVAL      PiStr=SkSplitCSV(i)
140900030721     C                   EXSR      CHKNUM
141000030721     C                   IF        PiInt=*on
141100030721     C                   Z-ADD     PiVal         Num5_0
141200030721     C                   MOVEL(p)  Num5_0        VABCMO
141300030721     C                   ELSE
141400030721     C                   SETON                                        32
141500030721     C                   EVAL      VABCMO = *zeros
141600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141700030721     C                             + ' ' + 'VABCMO'
141800030721     C                   ENDIF
141900030721     C                   ENDIF
142000040510     C*
142100040510     C* Reperisco i campi del FIVAT
142200040510     C*** VATNOT
142300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142500040510     C                   ENDIF
142600040510     C*** VATTRC
142700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142900040510     C                   ENDIF
143000060331     C*** VATNOT_A
143100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143300060331     C                   ENDIF
143400060331     C*** VATNOT_B
143500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143700060331     C                   ENDIF
143800060331     C*** VATNOT_E
143900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144100060331     C                   ENDIF
144200070823     C*
144300071029     C***  ===>  Gestione campi "particolari" dentro ciclo
144400090421     C*
144500090421     C*** W_VABLOD
144600090421     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD'
144700090421     C                   EVAL      VABLOD = %subst(%trim(SkSplitCSV(i)):1:
144800090421     C                                      %len(%trim(SkSplitCSV(i)))-2)
144900090421     C                   ENDIF
145000090521     C*** VABRMN / VATNOT-E
145100090521     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
145200090521     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145300090521     C                   EVAL      PiStr=SkSplitCSV(i)
145400090521     C                   EXSR      CHKNUM
145500090521     C                   IF        PiInt=*on
145600090521     C                   Z-ADD     PiVal         VABRMN
145700090521     C                   ELSE
145800090521     C                   SETON                                        32
145900090521     C                   EVAL      VABRMN = *zeros
146000090521     C                   EVAL      vinmsg = %trimr(vinmsg)
146100090521     C                             + ' ' + 'VABRMN'
146200090521     C                   ENDIF
146300090521     C                   ENDIF
146400090423     C*** VABNOT
146500090423     C                   EVAL      VABNOT='RICHIESTA VARIAZIONE INDIRIZZO E'
146600090423     C*** VABNT2
146700090423     C                   EVAL      VABNT2='FERMO DEP AUTORIZZATO - NO GIACENZA'
146800070925     C*
146900071029     C***  <===  -----------------------------------------
147000040510     C*
147100030715     C                   ENDIF
147200030715     C                   EVAL      i = i + 1
147300030715     C                   ENDDO
147400070925     C*
147500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147600080125     C*
147700070925     C*
147800071029     C***  <===  -----------------------------------------
147900070925     C*
148000070925     C* Considerazioni finali su CBO/CAS
148100080125     C                   IF        wFlgCAS = '1'
148200070925     C                   IF        VABCBO = '1'
148300070925     C                   EVAL      VABCBO = '4'
148400070925     C                   ENDIF
148500070925     C                   IF        VABCBO = '2'
148600070925     C                   EVAL      VABCBO = '6'
148700070925     C                   ENDIF
148800070925     C                   ENDIF
148900070925     C***  <===  ----------------------------
149000040510     C*
149100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149200040510     C                   IF        VABNSP = *zeros
149300040510     C                   SETON                                        31
149400040510     C                   ENDIF
149500040715     C*
149600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149700040715     C                   IF        VABLNP = *zeros
149800040715     C                   SETON                                        31
149900040715     C                   ENDIF
150000040510     C*
150100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150200040510     C                   IF        VABRMN = *zeros
150300040510     C                   Z-ADD     VABNSP        VABRMN
150400040510     C                   ENDIF
150500040510     C*
150600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150700040510     C                   IF        VABRMA = *blanks
150800040714     C                   MOVEL     VABRMN        VABRMA
150900040510     C                   ENDIF
151000040128     C*
151100040510     C* Se provincia nn valorizzata la reperisco
151200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151300040510     C                   IF        VABPRD = *blanks
151400040128     C                   CLEAR                   TISI95DS
151500040128     C                   EVAL      I95TCN = '3'
151600040128     C                   Z-ADD     datcor        I95DAT
151700040128     C                   EVAL      I95NAR = VABNZD
151800040128     C                   EVAL      I95CAP = VABCAD
151900040128     C                   EVAL      I95LOC = VABLOD
152000040128     C                   CALL      'TISI95R'
152100040128     C                   PARM                    TISI95DS
152200040128     C                   EVAL      VABPRD = O95PRV
152300040510     C                   ENDIF
152400020204     C*
152500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152600020204     C                   EXSR      CHKIMPDIV
152700030715     C                   ENDIF
152800020204     C*
152900010607     C                   ENDSR
153000010607     C*----------------------------------------------------*
153100010601
153200020204
153300020204     C*----------------------------------------------------*
153400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153500020204     C*----------------------------------------------------*
153600020204     C     CHKIMPDIV     BEGSR
153700020204     C*
153800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153900020204     C                   Z-ADD     *zeros        wrkDec            9 9
154000020204     C*
154100020204     C* Come prima cosa effettuo considerazioni sulla divisa
154200020204     C                   IF        vabIAS > *zeros
154300020204     C                   IF        vabVAS <> 'EUR'
154400020204     C                   EVAL      vabVAS =  'ITL'
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C                   IF        vabCAS > *zeros
154900020204     C                   IF        vabVCA <> 'EUR'
155000020204     C                   EVAL      vabVCA =  'ITL'
155100020204     C                   ENDIF
155200020204     C                   ENDIF
155300020204     C*
155400020204     C                   IF        vabVMD > *zeros
155500020204     C                   IF        vabVAD <> 'EUR'
155600020204     C                   EVAL      vabVAD =  'ITL'
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156100020204     C                   Z-ADD     vabIAS        wrkDec
156200020204     C                   IF        wrkDec > *zeros
156300020204     C                   IF        vabVAS = 'ITL'
156400020204     C                   EVAL      vabIAS = *zeros
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156900020204     C                   Z-ADD     vabCAS        wrkDec
157000020204     C                   IF        wrkDec > *zeros
157100020204     C                   IF        vabVCA = 'ITL'
157200020204     C                   EVAL      vabCAS = *zeros
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157700020204     C                   Z-ADD     vabVMD        wrkDec
157800020204     C                   IF        wrkDec > *zeros
157900020204     C                   IF        vabVAD = 'ITL'
158000020204     C                   EVAL      vabVMD = *zeros
158100020204     C                   ENDIF
158200020204     C                   ENDIF
158300020204     C*
158400020204     C                   ENDSR
158500020204     C***
158600080617
158700080617
158800080617
158900080617     C*----------------------------------------------------*
159000080617     C*  CONTROLLO NUMERICITA' CAMPI
159100080617     C*----------------------------------------------------*
159200080617     C     CHKNUM        BEGSR
159300080617     C*
159400080617     C                   IF        PiDecChr = *blanks
159500080617     C                   EVAL      PiDecChr = CharNUM
159600080617     C                   ENDIF
159700080617     C*
159800080617     C                   callp     UBISNUM_Check(PiStr
159900080617     C                                          :PiDecChr
160000080617     C                                          :PiVal
160100080617     C                                          :PiNum
160200080617     C                                          :PiInt)
160300080617     C*
160400080617     C                   ENDSR
160500080617     C***
160600010330
160700010601
160800010601
160900010601
161000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161100010601     C     repfil        BEGSR
161200010601     C*
161300010601     C                   if        invfil = *zeros and
161400010601     C                             depfil > *zeros and
161500010629     C                             (vinflg = *blanks or
161600010629     C                              vinflg = *zeros)
161700010601     C
161800010601     C                   eval      invfil = depfil
161900010601     C                   endif
162000010601     C*
162100010601     C                   if        depfil <> invfil and
162200010601     C                             invfil > *zeros
162300010601     C                   eval      flgMulti = '1'
162400010601     C                   if        vinflg = *blanks
162500010601     C                   add       1             cntNonEl
162600010601     C                   endif
162700010601     C                   endif
162800010601     C*
162900010601     C                   if        vinflg = '2'
163000010601     C                   eval      flgStato = '2'
163100010601     C                   endif
163200010601     C*
163300010601     C                   ENDSR
163400010601     C***
163500010601
163600010601
163700010601
163800010330
163900040119      /TITLE Invio dei dati al punto operativo.
164000040119     C     invio         BEGSR
164100040119     C*
164200040119     C* 1° invio FIVAT
164300040119     C                   reset                   dscmz
164400040119     C                   move      vlrpoi        cmzdst
164500040119     C                   eval      cmzfld = 'FIVATWWR'
164600040119     C                   eval      cmzmbd = vlrhdl
164700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164800040119     C***                if        prmfir = *blanks
164900040119     C                   eval      cmzfla = 'FIVAT00F'
165000040119     C                   eval      cmzmba = 'FIVAT00F'
165100040119     C***                else
165200040119     C***                eval      cmzfla = prmfir
165300040119     C***                eval      cmzmba = prmfir
165400040119     C***                endif
165500040119     C                   eval      cmznrr = *zeros
165600040119     C                   move      §ctrokvt      cmznrr
165700040119     C                   eval      cmzlba = vlrfl1
165800040119     C                   call(e)   'TIS711C'
165900040119     C                   parm                    dscmz
166000040119     C                   parm      *blanks       esito
166100040119     C                   if        %error
166200040119     C                             or cmzerr = '1'
166300040119     C                             or esito  = '1'
166400040119     C                   eval      wrkesito = '3'
166500040119     C                   else
166600040119     C*
166700040119     C* 2° invio FIVAB
166800040119     C                   reset                   dscmz
166900040119     C                   move      vlrpoi        cmzdst
167000040119     C                   eval      cmzfld = vlrfou
167100040119     C                   eval      cmzmbd = vlrhdl
167200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167300040119     C***                if        prmfir = *blanks
167400040119     C                   eval      cmzfla = 'FIVAB00F'
167500040119     C                   eval      cmzmba = 'FIVAB00F'
167600040119     C***                else
167700040119     C***                eval      cmzfla = prmfir
167800040119     C***                eval      cmzmba = prmfir
167900040119     C***                endif
168000040119     C                   eval      cmznrr = *zeros
168100040119     C                   move      §ctrokvb      cmznrr
168200040119     C                   eval      cmzlba = vlrfl1
168300040119     C                   call(e)   'TIS711C'
168400040119     C                   parm                    dscmz
168500040119     C                   parm      *blanks       esito
168600040119     C                   if        %error
168700040119     C                             or cmzerr = '1'
168800040119     C                             or esito  = '1'
168900040119     C                   eval      wrkesito = '3'
169000040119     C                   endif
169100040119     C                   endif
169200040119     C*
169300040119     C                   ENDSR
169400040119     C***
169500010601
169600010601
169700010601
169800010601
169900010601      /TITLE Invio dei dati al punto operativo.
170000010601     C     opeini        BEGSR
170100010601     C*
170200010601     C* Inizializzo flag e contatori operativi
170300010601     C                   movel     '0'           flgGiro           1
170400010601     C                   movel     '0'           flgMulti          1
170500010601     C                   movel     '1'           flgStato          1
170600010615     C                   movel     '0'           flgOk             1
170700010601     C                   z-add     *zeros        cntNonEl         10 0
170800010601     C                   z-add     *zeros        depfil            3 0
170900010601     C                   z-add     *zeros        invfil            3 0
171000010601     C*
171100010601     C                   ENDSR
171200010601     C***
171300070326
171400070326
171500070326
171600070326
171700080916     C     *pssr         BEGSR
171800070329     C*
171900080916     C                   if        %open(tivin00r)
172000080916     C                   close     tivin00r
172100080916     C                   endif
172200080916     C                   if        %open(fivabwwr)
172300080916     C                   close     fivabwwr
172400080916     C                   endif
172500080916     C                   if        %open(fivatwwr)
172600080916     C                   close     fivatwwr
172700080916     C                   endif
172800070326     C*
172900070326     C* Effettuo la chiamata al CLLE preposto
173000080916     C                   call(e)   'TITVVTC'
173100080916     C                   parm                    parccm
173200080916     C                   parm                    parmbr
173300080916     C                   parm      '2'           paropz
173400070326     C*
173500080916     C                   eval      wrkesito = '2'
173600070404     C*
173700080916     C                   seton                                        LR
173800070326     C*
173900080916     C                   ENDSR     '*CANCL'
174000070326     C***
174100070326
174200070326
174300010330
174400010330
174500000613     C     *inzsr        BEGSR
174600990910     C*
174700990910     C     *entry        plist
174800990920     C                   parm                    tivlrds
174900990921     C                   parm      wrkesito      esito
175000000724     C                   parm                    prmlit
175100000710     C                   parm                    prmfir
175200010330     C*
175300010330     C* CALCOLA LA DATA CORRENTE
175400010330     C                   time                    wn14             14 0
175500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
175600010330     C                   z-add     wn8           g08dat
175700010330     C                   z-add     *zeros        g08inv
175800010330     C                   movel     '0'           g08err
175900010330     C                   call      'XSRDA8'
176000010330     C                   parm                    wlbda8
176100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
176200000613     C*
176300000613     C                   ENDSR
176400000613     C***
176500990908
176600090420     Otitv1p0p  E            riepilogo         2
176700990915     O                                              'Upload via Internet'
176800990915     O                                           +1 'Traduzione TIVIN00R -'
176900021025     O                                           +1 'FIVABWWR'
177000990915     O                                           +1 'Testate bolle'
177100990915     O          E            riepilogo   2
177200990915     O                       wrkdata
177300990915     O                       wrkora              +1
177400990915     O                       procname            +1
177500990915     O          E            riepilogo   2
177600990915     O                                              'Cliente..................:'
177700010605     O                       VABCCM        z     +1
177800990915     O          E            riepilogo   2
177900990920     O                                              'Riferimento Strategi.....:'
178000990920     O                       vlrhdl              +1
178100990915     O          E            riepilogo   2
178200990915     O                                              'Giusti...................:'
178300040119     O                       §CTROKVB      2   +  1
178400990915     O          E            riepilogo   2
178500990915     O                                              'Sbagliati e corretti.....:'
178600971022     O                       §CTRMO        2   +  1
178700990915     O          E            riepilogo   2
178800990915     O                                              'Sbagliati e scartati.....:'
178900971022     O                       §CTRNO        2   +  1
179000000613
179100090420     Otitv1p0ps E            testdett          2
179200000613     O                                              'Upload via Internet'
179300000613     O                                           +1 'Traduzione TIVIN00R -'
179400021025     O                                           +1 'FIVABWWR'
179500000613     O                                           +1 'Testate bolle'
179600000616     O          E            testdett    3
179700000613     O                                           +2 'N° rec'
179800000613     O                                           +3 'Anteprima contenuto'
179900000616     O          E            rigadett    2
180000000613     O                       rrnum               +2
180100000621     O                       recko               +3
180200000616     O          E            findett     2
180300000613     O                       wrkdata
180400000613     O                       wrkora              +1
180500000613     O                       procname            +1
180600000616     O          E            findett     2
180700000613     O                                              'Cliente..................:'
180800010605     O                       VABCCM        z     +1
180900000616     O          E            findett     2
181000000613     O                                              'Riferimento Strategi.....:'
181100000613     O                       vlrhdl              +1
181200000616     O          E            findett     2
181300000613     O                                              'Giusti...................:'
181400040119     O                       §CTROKVB      2   +  1
181500000616     O          E            findett     2
181600000613     O                                              'Sbagliati e corretti.....:'
181700000613     O                       §CTRMO        2   +  1
181800000616     O          E            findett     2
181900000613     O                                              'Sbagliati e scartati.....:'
182000000613     O                       §CTRNO        2   +  1
182100000613** CMD - COMANDI CL
182200090420OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1P0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
182300090420OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1P0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
182400090420DLTOVR FILE(TITV1P0P TITV1P0PS) LVL(*)
182500010606
182600000613
182700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182800090521W_VABRMN;FILL01;W_VABLOD;VABRSD;FILL02;FILL03;FILL04;FILL05;FILL06;
182900090420FILL07;VABCAD;VABIND;FILL08;FILL09
183000080715
