000100100315      /TITLE Upload via Internet: traduzione in FIVABWR/FIVATWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100315     FFIVABwwr  O    E             DISK    usropn
000700100315     FFIVATwwr  O    E             DISK    usropn
000800100315     Ftitv1v9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100100315     Ftitv1v9ps O    f  198        PRINTER usropn
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
002200090716     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
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
005500100204     D CharSOS         S              1
005600030715     D posDa           S              3  0 INZ(*zeros)
005700030715     D posA            S              3  0 INZ(*zeros)
005800030715     D i               s              3  0 INZ(1)
005900030715     D wGiro           s              1  0 INZ(*zeros)
006000080125     D wVATNOT_A       s                   LIKE(VATNOT)
006100080125     D wVATNOT_B       s                   LIKE(VATNOT)
006200080125     D wVATNOT_E       s                   LIKE(VATNOT)
006300030822     D*------------------
006400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006500030822     D*------------------
006600030822     D posDaDft        S              3  0 INZ(*zeros)
006700030822     D posADft         S              3  0 INZ(*zeros)
006800030822     D j               s              3  0 INZ(1)
006900030822     D wGiroDft        s              1  0 INZ(*zeros)
007000031201     D*------------------
007100031201     D* Costanti
007200031201     D*------------------
007300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007500080617     D*------------------
007600080617
007700080617     D*------------------
007800080617     D* LINKING A DEFINIZIONI ESTERNE
007900080617     D*------------------
008000080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008100090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008200010330
008300010330
008400010330
008500990915     C                   time                    wrkdata
008600990915     C                   time                    wrkora
008700000913     C                   reset                   rrnum
008800990921     C                   reset                   esito
008900081014     C                   reset                   wrkesito
009000010601     C*
009100010601     C                   exsr      opeini
009200010605     C                   exsr      rwvab
009300081014     C*
009400081014     C                   exsr      endela
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
013700100315     C                   if        not %open(titv1v9ps)
013800100315     C                   open      titv1v9ps
013900070329     C                   except    testdett
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
015400081014     C*
015500081014     C                   if        %open(tivin00r)
015600081014     C                   close     tivin00r
015700081014     C                   endif
015800100315     C                   if        %open(fivabwwr)
015900100315     C                   close     fivabwwr
016000081014     C                   endif
016100100315     C                   if        %open(fivatwwr)
016200100315     C                   close     fivatwwr
016300081014     C                   endif
016400040119     C*
016500040119     C* Effettuo la chiamata al CLLE preposto
016600100315     C                   call(e)   'TITVVTC'
016700040510     C                   parm                    parccm
016800040119     C                   parm                    parmbr
016900040119     C                   parm      '2'           paropz
017000050201     C*
017100050201     C* Effettuo lancio TISI95 solo x chiusura
017200050201     C                   CLEAR                   TISI95DS
017300050201     C                   EVAL      I95TLA = 'C'
017400050201     C                   CALL      'TISI95R'
017500050201     C                   PARM                    TISI95DS
017600000613     C*
017700100315     C                   if        %open(titv1v9ps)
017800000616     C                   except    findett
017900100315     C                   close     titv1v9ps
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
019300000613
019400000613     C*--------------------------------------------------------
019500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019600000613     C*--------------------------------------------------------
019700000621     C     STPR          BEGSR
019800000613     C*
019900100315     C                   if        not %open(titv1v9p)
020000100315     C   50              open      titv1v9p
020100990915     C                   endif
020200990915     C*
020300030715     C   50              except    riepilogo
020400990915     C*
020500100315     C                   if        %open(titv1v9p)
020600100315     C   50              close     titv1v9p
020700990914     C                   endif
020800000613     C*
020900000613     C                   ENDSR
021000000613     C***
021100000613
021200010601
021300010601
021400010330     C*--------------------------------------------------------
021500100315     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021600010330     C*--------------------------------------------------------
021700010605     C     RWVAB         BEGSR
021800010330     C*
021900010330     C                   if        not %open(tivin00r)
022000010330     C                   open      tivin00r
022100010330     C                   endif
022200100315     C                   if        not %open(fivabwwr)
022300100315     C                   open      fivabwwr
022400010330     C                   endif
022500040119     C*
022600100315     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022700040119     C                   exsr      prevat
022800040119     C*
022900040119     C                   if        chkcall = '0'
023000040119     C*
023100100315     C                   if        not %open(fivatwwr)
023200100315     C                   open      fivatwwr
023300040119     C                   endif
023400010330     C*
023500040119     C                   clear                   §CTROKVB          7 0
023600040119     C                   clear                   §CTROKVT          7 0
023700040119     C                   clear                   §CTRMO            7 0
023800040119     C                   clear                   §CTRNO            7 0
023900070823     C*
024000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024200100302     C***                exsr      inzvar
024300100302     C***                exsr      impvab
024400010330     C*
024500030822     C                   do        *HIVAL
024600010330     C*
024700030822     C                   read      tivin00r                               70
024800010618     C*
024900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025000010618     C                   if        vinflg = '1'
025100010618     C                   eval      flgOk = '1'
025200010618     C                   endif
025300040510     C*
025400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025500040510     C                   z-add     *zeros        wLenVINDTA        4 0
025600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
025700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
025800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
025900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026000040510     C                   z-add     1             wFlgVINDTA
026100040510     C                   leave
026200040510     C                   endif
026300040510     C                   enddo
026400090420     C*
026500090420     C                   add       1             rrnum
026600010618     C*
026700040510     C                   if        vindta > *blanks AND
026800090420     C                             wFlgVINDTA = 1
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
027900100204     C*
028000100204     C                   exsr      inzvar
028100100204     C                   exsr      normCSV
028200010605     C                   exsr      impvab
028300010601     C*
028400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028500010605     C                   eval      depfil = VABLNP
028600010601     C                   exsr      repfil
028700010601     C                   if        depfil = invfil
028800021025     C                   if        vlrpoi = 999
028900030822     C                   move(P)   invfil        VABFGS
029000021025     C                   else
029100030822     C                   move(P)   vlrpoi        VABFGS
029200021025     C                   endif
029300010601     C*
029400010601     C                   exsr      PREELA
029500010601     C*
029600010604     C* Ebbene...
029700010604     C*
029800040119     C  N31              add       1             §CTROKVB
029900040119     C   32              add       1             §CTRMO
030000040119     C   31              add       1             §CTRNO
030100030822     C                   if        wGiro = 2
030200040510     C* Scrivo sempre il VAT
030300040510     C  N31              exsr      wrivat
030400040510     C* Scrivo il VAB solo a rottura d spedizione
030500040119     C                   if        depspe =  *zeros OR
030600040119     C                             depspe <> vabNSP
030700040119     C  N31              exsr      wrivab
030800040119     C  N31              eval      depspe = vabNSP
030900040119     C                   endif
031000040119     C                   endif
031100010604     C*
031200010604     C                   if        *in31 = *off and
031300010604     C                             *in32 = *off
031400010604     C                   eval      vinflg = '1'
031500010604     C                   else
031600010604     C                   eval      recko = vindta
031700010604     C                   except    rigadett
031800010604     C                   eval      vinflg = '2'
031900010604     C                   endif
032000010604     C                   endif
032100010601     C*
032200010604     C                   endif
032300010604     C*
032400010330     C                   else
032500010330     C                   eval      vinflg = '1'
032600010330     C                   endif
032700010601     C*
032800010601     C  N70              update    tivin000
032900010330     C*
033000030822     C  N70              enddo
033100040119     C*
033200040119     C                   endif                                                  (endif - chkcall)
033300010601     C*
033400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033500010601     C                   if        cntNonEl = *zeros or
033600010601     C                             flgMulti = '0'
033700010330     C* Se non ci sono record con errori ...
033800010601     C                   if        §ctrno = 0 and
033900010604     C                             §ctrmo = 0 and
034000010601     C                             flgStato <> '2'
034100010330     C* ... restituisco esito OK.
034200010330     C                   eval      wrkesito = '0'
034300010330     C                   else
034400040119     C                   if        §ctrokvb > 0
034500010330     C                   eval      wrkesito = '1'
034600010330     C                   else
034700010615     C                   if        flgOk = '0'
034800010615     C                   eval      wrkesito = '2'
034900010615     C                   else
035000010615     C                   eval      wrkesito = '6'
035100010615     C                   endif
035200010330     C                   endif
035300010330     C                   endif
035400010601     C                   else
035500010601     C                   eval      wrkesito = '9'
035600010601     C                   endif
035700010330     C*
035800010330     C                   if        %open(tivin00r)
035900010330     C                   close     tivin00r
036000010330     C                   endif
036100100315     C                   if        %open(fivabwwr)
036200100315     C                   close     fivabwwr
036300010330     C                   endif
036400100315     C                   if        %open(fivatwwr)
036500100315     C                   close     fivatwwr
036600040512     C                   endif
036700010601     C*
036800010601     C                   if        vlrpoi <> 999
036900010601     C                   eval      invfil = vlrpoi
037000010601     C                   endif
037100010330     C*
037200040119     C                   if        §ctrokvb > 0
037300010601     C                             and invfil > *zeros
037400010330     C                   exsr      invio
037500010330     C                   endif
037600010601     C*
037700010618     C                   if        flgGiro = '1'
037800081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
037900010618     C                   endif
038000010330     C*
038100010330     C                   ENDSR
038200010330     C***
038300040119
038400040119
038500040119
038600040119     C*----------------------------------------------------*
038700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038800040119     C*----------------------------------------------------*
038900040510     C     WRIVAB        BEGSR
039000040119     C*
039100060331     C* Gestisco l'eventuale rottura x numero spedizione
039200070823     C                   if        VABNSP <> depspe
039300070301     C                   if        wVATNOT_E <> *blanks
039400080125     C                   if        VABCTM = *blanks
039500080125     C                   eval      VABCTM = '7Q'
039600080125     C                   endif
039700070301     C                   endif
039800100302     C*
039900100315     C                   write     FIVAB000                                     => scarico il VAB
040000060331     C                   endif
040100040119     C*
040200040119     C                   ENDSR
040300040119
040400040119
040500040119
040600040119     C*----------------------------------------------------*
040700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040800040119     C*----------------------------------------------------*
040900040510     C     WRIVAT        BEGSR
041000060512     C*
041100060512     C* Inizializzo i campi chiave
041200060512     C                   eval      VATFGS = VABFGS
041300060512     C                   eval      VATAAS = VABAAS
041400060512     C                   eval      VATCCM = VABCCM
041500070823     C                   eval      VATNRS = VABNRS
041600060512     C                   eval      VATNSP = VABNSP
041700060512     C                   eval      VATLNP = VABLNP
041800060331     C*
041900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042000060331     C                   if        wVATNOT_A <> *blanks
042100060331     C                   eval      VATNOT = wVATNOT_A
042200060331     C                   eval      VATTRC = 'A'
042300100315     C                   write     FIVAT000
042400060331     C                   add       1             §CTROKVT
042500060331     C                   endif
042600060331     C*
042700060331     C                   if        wVATNOT_B <> *blanks
042800060331     C                   eval      VATNOT = wVATNOT_B
042900060331     C                   eval      VATTRC = 'B'
043000100315     C                   write     FIVAT000
043100060331     C                   add       1             §CTROKVT
043200060331     C                   endif
043300060331     C*
043400080625     C                   if        wVATNOT_E <> *blanks
043500060331     C                   eval      VATNOT = wVATNOT_E
043600060331     C                   eval      VATTRC = 'E'
043700100315     C                   write     FIVAT000
043800080716     C                   add       1             §CTROKVT
043900080625     C                   endif
044000040119     C*
044100040119     C                   ENDSR
044200040119
044300040119
044400040119
044500040119     C*----------------------------------------------------*
044600100315     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044700040119     C*----------------------------------------------------*
044800040119     C     PREVAT        BEGSR
044900040119     C*
045000100315     C* Compongo il nome del membro da dare al FIVATWWR
045100040119     C                   eval      parmbr = vlrhdl
045200040119     C                   movel     'M'           parmbr
045300040510     C                   eval      parccm = vlrksc
045400040119     C                   eval      paropz = '1'
045500040119     C* Effettuo la chiamata al CLLE preposto
045600100315     C                   call(e)   'TITVVTC'
045700040119     C                   parm                    parccm
045800040119     C                   parm                    parmbr
045900040119     C                   parm                    paropz
046000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046100040119     C                   if        %error
046200040119     C                   movel     '1'           chkcall
046300040119     C                   else
046400040119     C                   movel     '0'           chkcall
046500040119     C                   endif
046600040119     C*
046700040119     C                   ENDSR
046800010601
046900010601
047000010601
047100010330     C*----------------------------------------------------*
047200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047300010330     C*----------------------------------------------------*
047400040119     C     INZVAR        BEGSR
047500010330     C*
047600030822     C* Inizializzo variabili di wrk
047700020204     C                   Z-ADD     *zeros        Num5_0            5 0
047800060331     C                   MOVEL     *blanks       wVATNOT_A
047900060331     C                   MOVEL     *blanks       wVATNOT_B
048000060331     C                   MOVEL     *blanks       wVATNOT_E
048100090220     C*
048200080617     C                   EVAL      wFlgCAS = *blanks
048300030822     C*
048400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048500100315     C                   CLEAR                   FIVAB000
048600100315     C                   CLEAR                   FIVAT000
048700030822     C                   CLEAR                   SkSplitCSV
048800030822     C                   EVAL      i = 1
048900030822     C                   EVAL      posDa = *zeros
049000030822     C                   EVAL      posA  = *zeros
049100030822     C*
049200030822     C* Reimposto i valori di default
049300030822     C                   EXSR      DEFCAM
049400030822     C*
049500010330     C                   ENDSR
049600010330     C*----------------------------------------------------*
049700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049800010330     C*----------------------------------------------------*
049900010330     C     DEFCAM        BEGSR
050000030822     C*
050100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050200030715     C* e delimitatore testo.
050300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050600100204     C*
050700100204     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050800100204     C                   EVAL      CharSOS = CharNUM
050900030822     C*
051000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
051200030715     C                   SETON                                        50
051300030715     C                   ELSE
051400030715     C                   SETOFF                                       50
051500030715     C                   ENDIF
051600090421     C*
051700090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
051800090421     C                   EVAL      VABCBO = '1'
051900090421     C                   EVAL      VABLNP = vlrpoi
052000090421     C                   EVAL      VABNSP = rrnum
052100030822     C*
052200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052300030822     C                   EVAL      posDaDft = 1
052400030822     C                   EVAL      posADft  = 0
052500030822     C                   EVAL      wGiroDft = 0
052600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052700030822     C                             posDaDft > 0
052800030822     C*
052900030822     C* Gestisco il 1° giro
053000030822     C                   IF        wGiroDft = 0
053100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053300030822     C* Incremento il contatore dei "giri"
053400030822     C                   EVAL      wGiroDft = 1
053500030822     C                   ELSE
053600030822     C                   EVAL      posDaDft = posADft
053700030822     C                   ENDIF
053800030822     C* Eseguo lo scan x trovare la fine del campo corrente
053900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054000030822     C*
054100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054300030822     C                   IF        posDaDft > 0 AND
054400030822     C                             posADft  > 0
054500030822     C* NCL
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'NCL'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABNCL
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* CCM
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'CCM'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiInt=*on
056800030822     C                   Z-ADD     PiVal         VABCCM
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* LNP
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'LNP'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiInt=*on
058100030822     C                   Z-ADD     PiVal         VABLNP
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* NRS
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'NRS'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiInt=*on
059400030822     C                   Z-ADD     PiVal         VABNRS
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* CTR
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'CTR'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABCTR
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* PKB
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'PKB'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiNum=*on
062000030822     C                   Z-ADD     PiVal         VABPKB
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* VLB
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'VLB'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiNum=*on
063300030822     C                   Z-ADD     PiVal         VABVLB
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* QFT
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'QFT'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiNum=*on
064600030822     C                   Z-ADD     PiVal         VABQFT
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* CBO
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'CBO'
065400030822     C                   EVAL      VABCBO=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* TSP
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'TSP'
066300030822     C                   EVAL      VABTSP=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* VAS
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VAS'
067200030822     C                   EVAL      VABVAS=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* VCA
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VCA'
068100030822     C                   EVAL      VABVCA=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500070925     C* TIC
068600070925     C                   IF        %subst(
068700070925     C                             %subst(vlrppt:posDaDft+1:
068800070925     C                             posADft-posDaDft-1):1:3)
068900070925     C                             = 'TIC'
069000070925     C                   EVAL      VABTIC=%trim(%subst(
069100070925     C                             %subst(vlrppt:posDaDft+1:
069200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300070925     C                   ENDIF
069400030822     C* GCA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'GCA'
069900030822     C                   EVAL      VABGCA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* CTM
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'CTM'
070800030822     C                   EVAL      VABCTM=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* FFD
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'FFD'
071700030822     C                   EVAL      VABFFD=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* VAD
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'VAD'
072600030822     C                   EVAL      VABVAD=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* GMA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'GMA'
073500030822     C                   EVAL      VABGMA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GGA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GGA'
074400030822     C                   EVAL      VABGGA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* GVA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GVA'
075300030822     C                   EVAL      VABGVA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* TC1
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'TC1'
076200030822     C                   EVAL      VABTC1=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* TC2
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'TC2'
077100030822     C                   EVAL      VABTC2=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500040714     C* VATTRC
077600040714     C                   IF        %subst(
077700040714     C                             %subst(vlrppt:posDaDft+1:
077800040714     C                             posADft-posDaDft-1):1:3)
077900040714     C                             = 'TRC'
078000040714     C                   EVAL      VATTRC=%trim(%subst(
078100040714     C                             %subst(vlrppt:posDaDft+1:
078200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300040714     C                   ENDIF
078400030822     C* ...
078500030822     C                   ENDIF
078600030822     C                   ENDDO
078700020204     C*
078800010330     C                   ENDSR
078900010607     C*----------------------------------------------------*
079000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079100010607     C*----------------------------------------------------*
079200010607     C     IMPVAB        BEGSR
079300010607     C*
079400010607     C                   SETOFF                                       3132
079500030515     C*
079600010607     C                   MOVEL     datcor        VABAAS
079700010607     C                   MOVE      datcor        VABMGS
079800010607     C*
079900030715     C**********
080000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080100030715     C**********
080200030715     C                   IF        wGiro = *zeros
080300080716     C*
080400080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080500080716     C                   EVAL      depvinDTA = vinDTA
080600040113     C*
080700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080900090716     C                             %trim(intcol(3))+%trim(intcol(4))+
081000090716     C                             %trim(intcol(5))<>*blanks
081100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081200090716     C                                      %trim(intcol(3))+%trim(intcol(4))+
081300090716     C                                      %trim(intcol(5))
081400100204     C     ';':CharCSV   XLATE     vindta        vindta
081500040510     C                   ENDIF
081600080626     C*
081700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081800031201     C     minu:maiu     XLATE     vindta        vindta
081900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082000030715     C                   DOW       posDa <= %len(%trim(vindta))
082100030715     C*
082200030715     C* Gestisco il 1° campo
082300030715     C                   IF        i = 1
082400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082500030715     C                   EVAL      posDa = 1
082600030715     C* Eseguo lo scan x trovare la fine del primo campo
082700100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082800030715     C* A questo punto "estraggo" il campo corrente
082900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083000030715     C                                                    (posA-posDa))
083100030715     C* X i campi successivi al 1°
083200030715     C                   ELSE
083300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083400030715     C                   EVAL      posDa = posA + 1
083500030715     C* Eseguo lo scan x trovare la fine del campo corrente
083600100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083700030715     C* Gestisco l'ultimo campo
083800030715     C                   IF        posA = *zeros
083900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084000030715     C* A questo punto "estraggo" il campo corrente
084100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084200030715     C                                                    (posA-posDa))
084300030715     C                   LEAVE
084400030715     C                   ELSE
084500030715     C* A questo punto "estraggo" il campo corrente
084600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084700030715     C                                                    (posA-posDa))
084800030715     C                   ENDIF
084900030715     C                   ENDIF
085000030715     C* Incremento il contatore d campo
085100030715     C                   EVAL      i = i +1
085200030715     C                   ENDDO
085300030715     C*
085400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085500030715     C* delimitazione testo
085600030715     C                   EVAL      i = 1
085700030715     C                   DOW       i < %elem(SkSplitFLD)
085800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086000030715     C                   EVAL      i = i + 1
086100030715     C                   ENDDO
086200030715     C                   EVAL      wGiro = 1
086300080716     C* Ripristino la 1° riga originale
086400080716     C                   EVAL      vinDTA = depvinDTA
086500030715     C                   ELSE
086600030715     C                   EVAL      wGiro = 2
086700031204     C**********
086800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086900031204     C* d separatore campo
087000031204     C**********
087100031204     C                   EVAL      vindta = %trim(vindta)
087200100204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087300031204     C                   EVAL      vindta = %subst(vindta:2)
087400031204     C                   ENDDO
087500031204     C*
087600031204     C                   Z-ADD     *zeros        lunghInput        4 0
087700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087800100204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088100031204     C                   ENDDO
088200030715     C**********
088300030715     C* Effettuo lo split del campo dati d input
088400030715     C**********
088500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088600030715     C                   DOW       posDa <= %len(%trim(vindta))
088700030715     C*
088800030715     C* Gestisco il 1° campo
088900030715     C                   IF        i = 1
089000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089100030715     C                   EVAL      posDa = 1
089200030715     C* Eseguo lo scan x trovare la fine del primo campo
089300100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C* X i campi successivi al 1°
089800030715     C                   ELSE
089900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090000030715     C                   EVAL      posDa = posA + 1
090100031204     C* Verifico che nn vi sia il campo nn valorizzato
090200100204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090300031204     C* Se campo nn valorizzato skippo al prossimo
090400031204     C                   EVAL      SkSplitCSV(i) = *blanks
090500031204     C                   EVAL      posA  = posA + 1
090600031204     C                   ELSE
090700030715     C* Eseguo lo scan x trovare la fine del campo corrente
090800100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090900030715     C* Gestisco l'ultimo campo
091000030715     C                   IF        posA = *zeros
091100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C                   LEAVE
091600030715     C                   ELSE
091700030715     C* A questo punto "estraggo" il campo corrente
091800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091900030715     C                                                    (posA-posDa))
092000030715     C                   ENDIF
092100031204     C                   ENDIF
092200030715     C                   ENDIF
092300030715     C* Incremento il contatore d campo
092400030715     C                   EVAL      i = i +1
092500030715     C                   ENDDO
092600030715     C*
092700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092800030715     C* delimitazione testo
092900030715     C                   EVAL      i = 1
093000030715     C                   DOW       i < %elem(SkSplitCSV)
093100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093300030715     C                   EVAL      i = i + 1
093400030715     C                   ENDDO
093500030715     C*
093600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitFLD)
093900030715     C*
094000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094200030715     C                             SkSplitCSV(i) <> *blanks
094300030721     C*** ATB
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094500030721     C                   EVAL      VABATB = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030715     C*** RSD
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095200030715     C* ==
095300030715     C                   ENDIF
095400030721     C*** RD2
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** IND
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096000030715     C                   EVAL      VABIND = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** LOD
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** PRD
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030721     C*** NZD
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030715     C*** RMA
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** NOT
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** NT2
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** VCA
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** VAS
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** FFD
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030721     C*** GC1
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** GC2
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** TSP
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100900030721     C                   ENDIF
101000040510     C*** CBO
101100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101300040510     C                   ENDIF
101400030721     C*** NAS
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TIC
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GCA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** XCO
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** CTM
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TCR
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** CTS
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** FTM
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** VAD
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GMA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** GGA
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GVA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** TC1
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** TC2
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** SCL
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** RMO
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** NMO
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*
108300030721     C* Reperisco quindi i campi numerici...
108400040510     C*** NSP
108500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108600040510     C                   EVAL      PiStr=SkSplitCSV(i)
108700040510     C                   EXSR      CHKNUM
108800040510     C                   IF        PiInt=*on
108900040510     C                   Z-ADD     PiVal         VABNSP
109000040510     C                   ELSE
109100040510     C                   SETON                                        31
109200040510     C                   EVAL      VABNSP = *zeros
109300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109400040510     C                             + ' ' + 'VABNSP'
109500040510     C                   ENDIF
109600040510     C                   ENDIF
109700071106     C*** RMN
109800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109900071106     C                   EVAL      PiStr=SkSplitCSV(i)
110000071106     C                   EXSR      CHKNUM
110100071106     C                   IF        PiInt=*on
110200071106     C                   Z-ADD     PiVal         VABRMN
110300071106     C                   ELSE
110400071106     C                   SETON                                        32
110500071106     C                   EVAL      VABRMN = *zeros
110600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110700071106     C                             + ' ' + 'VABRMN'
110800071106     C                   ENDIF
110900071106     C                   ENDIF
111000030721     C*** CAD
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiInt=*on
111500030721     C                   Z-ADD     PiVal         Num5_0
111600030721     C                   MOVEL(p)  Num5_0        VABCAD
111700030721     C                   ELSE
111800090603     C                   EVAL      VABCAD = SkSplitCSV(i)
111900090603     C                   SETON                                        32
112000090603     C                   EVAL      VABCAD = *zeros
112100090603     C                   EVAL      vinmsg = %trimr(vinmsg)
112200090603     C                             + ' ' + 'VABCAD'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** NCL
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiInt=*on
113000030721     C                   Z-ADD     PiVal         VABNCL
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      VABNCL = *zeros
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500040122     C                             + ' ' + 'VABNCL'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** PKB
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiNum=*on
114300030721     C                   Z-ADD     PiVal         VABPKB
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      VABPKB = *zeros
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800030721     C                             + ' ' + 'VABPKB'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** CAS
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115300080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
115400080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
115500081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
115600090716     C                             %trim(SkSplitCSV(i)) <> '.0'     AND
115700090716     C                             %trim(SkSplitCSV(i)) <> '.00'    AND
115800090716     C                             %trim(SkSplitCSV(i)) <> ',0'     AND
115900090716     C                             %trim(SkSplitCSV(i)) <> ',00'    AND
116000090716     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
116100090716     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
116200090716     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
116300081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
116400080125     C                   EVAL      wFlgCAS = '1'
116500080125     C                   ENDIF
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiNum=*on
116900030721     C                   Z-ADD     PiVal         VABCAS
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABCAS = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABCAS'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** CCM
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on      AND
118200030721     C                             PiVal<=9999999 AND
118300030721     C                             PiVal>*zeros
118400030721     C                   Z-ADD     PiVal         VABCCM
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABCCM = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900040510     C                             + ' ' + 'VABCCM'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** LNP
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiInt=*on    AND
119700030721     C                             PiVal<=999   AND
119800030721     C                             PiVal>*zeros
119900030721     C                   Z-ADD     PiVal         VABLNP
120000030721     C                   ELSE
120100040715     C                   SETON                                        32
120200030721     C                   EVAL      VABLNP = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400040510     C                             + ' ' + 'VABLNP'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** NRS
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on AND
121200030721     C                             PiVal<=99
121300030721     C                   Z-ADD     PiVal         VABNRS
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABNRS'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** LNA
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on    AND
122500030721     C                             PiVal<=999   AND
122600030721     C                             PiVal>*zeros
122700030721     C                   Z-ADD     PiVal         VABLNA
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      VABLNA = *zeros
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABLNA'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** CTR
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on  AND
124000030721     C                             PiVal<=999
124100030721     C                   Z-ADD     PiVal         VABCTR
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABCTR'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** IAS
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiNum=*on
125300030721     C                   Z-ADD     PiVal         VABIAS
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABIAS'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** VLB
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiNum=*on
126500030721     C                   Z-ADD     PiVal         VABVLB
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABVLB'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** QFT
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABQFT
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABQFT'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** NCD
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on      AND
128900030721     C                             PiVal<=9999999
129000030721     C                   Z-ADD     PiVal         VABNCD
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABNCD'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** NCA
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on      AND
130200030721     C                             PiVal<=9999999
130300030721     C                   Z-ADD     PiVal         VABNCA
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABNCA'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** ZNC
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on AND
131500030721     C                             PiVal<=99
131600030721     C                   Z-ADD     PiVal         VABZNC
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABZNC'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** DCR
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on       AND
132800030721     C                             PiVal<=99999999
132900030721     C                   Z-ADD     PiVal         VABDCR
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABDCR'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** HCR
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on   AND
134100030721     C                             PiVal<=9999
134200030721     C                   Z-ADD     PiVal         VABHCR
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABHCR'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** VMD
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiNum=*on
135400030721     C                   Z-ADD     PiVal         VABVMD
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABVMD'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** ANT
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on        AND
136600030721     C                             PiVal<=999999999
136700030721     C                   Z-ADD     PiVal         VABANT
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABANT'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** CMO
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiInt=*on
137900030721     C                   Z-ADD     PiVal         Num5_0
138000030721     C                   MOVEL(p)  Num5_0        VABCMO
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      VABCMO = *zeros
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABCMO'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800040510     C*
138900040510     C* Reperisco i campi del FIVAT
139000040510     C*** VATNOT
139100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139300040510     C                   ENDIF
139400040510     C*** VATTRC
139500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139700040510     C                   ENDIF
139800060331     C*** VATNOT_A
139900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140100060331     C                   ENDIF
140200060331     C*** VATNOT_B
140300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140500060331     C                   ENDIF
140600060331     C*** VATNOT_E
140700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140900060331     C                   ENDIF
141000070823     C*
141100071029     C***  ===>  Gestione campi "particolari" dentro ciclo
141200090421     C*
141300100315     C*** W_VABNSP/VABRMN
141400090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
141500090622     C                   EVAL      VABRMA = SkSplitCSV(i)
141600090622     C                   EVAL      PiStr=SkSplitCSV(i)
141700090622     C                   EXSR      CHKNUM
141800090622     C                   IF        PiInt=*on
141900090622     C                   Z-ADD     PiVal         VABRMN
142000090622     C                   Z-ADD     PiVal         VABNSP
142100090622     C                   ELSE
142200090622     C                   SETON                                        32
142300090622     C                   EVAL      VABRMN = *zeros
142400090622     C                   EVAL      VABNSP = *zeros
142500090622     C                   EVAL      vinmsg = %trimr(vinmsg)
142600090622     C                             + ' ' + 'VABRMN VABNSP'
142700090622     C                   ENDIF
142800090603     C                   ENDIF
142900100315     C*** TIC
143000100315     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
143100100315     C                   EVAL      VABTIC = SkSplitCSV(i)
143200100315     C                   IF        VABTIC <> *blanks
143300100315     C                   EVAL      wFlgCAS = '1'
143400100315     C                   ENDIF
143500100315     C                   ENDIF
143600070925     C*
143700071029     C***  <===  -----------------------------------------
143800040510     C*
143900030715     C                   ENDIF
144000030715     C                   EVAL      i = i + 1
144100030715     C                   ENDDO
144200070925     C*
144300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144400080125     C*
144500070925     C*
144600071029     C***  <===  -----------------------------------------
144700070925     C*
144800070925     C* Considerazioni finali su CBO/CAS
144900080125     C                   IF        wFlgCAS = '1'
145000070925     C                   IF        VABCBO = '1'
145100070925     C                   EVAL      VABCBO = '4'
145200070925     C                   ENDIF
145300070925     C                   IF        VABCBO = '2'
145400070925     C                   EVAL      VABCBO = '6'
145500070925     C                   ENDIF
145600070925     C                   ENDIF
145700070925     C***  <===  ----------------------------
145800040510     C*
145900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146000040510     C                   IF        VABNSP = *zeros
146100040510     C                   SETON                                        31
146200040510     C                   ENDIF
146300040715     C*
146400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146500040715     C                   IF        VABLNP = *zeros
146600040715     C                   SETON                                        31
146700040715     C                   ENDIF
146800040510     C*
146900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147000040510     C                   IF        VABRMN = *zeros
147100040510     C                   Z-ADD     VABNSP        VABRMN
147200040510     C                   ENDIF
147300040510     C*
147400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147500040510     C                   IF        VABRMA = *blanks
147600040714     C                   MOVEL     VABRMN        VABRMA
147700040510     C                   ENDIF
147800040128     C*
147900040510     C* Se provincia nn valorizzata la reperisco
148000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148100040510     C                   IF        VABPRD = *blanks
148200040128     C                   CLEAR                   TISI95DS
148300040128     C                   EVAL      I95TCN = '3'
148400040128     C                   Z-ADD     datcor        I95DAT
148500040128     C                   EVAL      I95NAR = VABNZD
148600040128     C                   EVAL      I95CAP = VABCAD
148700040128     C                   EVAL      I95LOC = VABLOD
148800040128     C                   CALL      'TISI95R'
148900040128     C                   PARM                    TISI95DS
149000040128     C                   EVAL      VABPRD = O95PRV
149100040510     C                   ENDIF
149200020204     C*
149300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149400020204     C                   EXSR      CHKIMPDIV
149500030715     C                   ENDIF
149600020204     C*
149700010607     C                   ENDSR
149800010607     C*----------------------------------------------------*
149900010601
150000020204
150100020204     C*----------------------------------------------------*
150200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150300020204     C*----------------------------------------------------*
150400020204     C     CHKIMPDIV     BEGSR
150500020204     C*
150600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150700020204     C                   Z-ADD     *zeros        wrkDec            9 9
150800020204     C*
150900020204     C* Come prima cosa effettuo considerazioni sulla divisa
151000020204     C                   IF        vabIAS > *zeros
151100020204     C                   IF        vabVAS <> 'EUR'
151200020204     C                   EVAL      vabVAS =  'ITL'
151300020204     C                   ENDIF
151400020204     C                   ENDIF
151500020204     C*
151600020204     C                   IF        vabCAS > *zeros
151700020204     C                   IF        vabVCA <> 'EUR'
151800020204     C                   EVAL      vabVCA =  'ITL'
151900020204     C                   ENDIF
152000020204     C                   ENDIF
152100020204     C*
152200020204     C                   IF        vabVMD > *zeros
152300020204     C                   IF        vabVAD <> 'EUR'
152400020204     C                   EVAL      vabVAD =  'ITL'
152500020204     C                   ENDIF
152600020204     C                   ENDIF
152700020204     C*
152800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152900020204     C                   Z-ADD     vabIAS        wrkDec
153000020204     C                   IF        wrkDec > *zeros
153100020204     C                   IF        vabVAS = 'ITL'
153200020204     C                   EVAL      vabIAS = *zeros
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153700020204     C                   Z-ADD     vabCAS        wrkDec
153800020204     C                   IF        wrkDec > *zeros
153900020204     C                   IF        vabVCA = 'ITL'
154000020204     C                   EVAL      vabCAS = *zeros
154100020204     C                   ENDIF
154200020204     C                   ENDIF
154300020204     C*
154400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154500020204     C                   Z-ADD     vabVMD        wrkDec
154600020204     C                   IF        wrkDec > *zeros
154700020204     C                   IF        vabVAD = 'ITL'
154800020204     C                   EVAL      vabVMD = *zeros
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C                   ENDSR
155300020204     C***
155400080617
155500080617
155600080617
155700080617     C*----------------------------------------------------*
155800080617     C*  CONTROLLO NUMERICITA' CAMPI
155900080617     C*----------------------------------------------------*
156000080617     C     CHKNUM        BEGSR
156100080617     C*
156200080617     C                   IF        PiDecChr = *blanks
156300080617     C                   EVAL      PiDecChr = CharNUM
156400080617     C                   ENDIF
156500080617     C*
156600080617     C                   callp     UBISNUM_Check(PiStr
156700080617     C                                          :PiDecChr
156800080617     C                                          :PiVal
156900080617     C                                          :PiNum
157000080617     C                                          :PiInt)
157100080617     C*
157200080617     C                   ENDSR
157300080617     C***
157400010330
157500010601
157600010601
157700010601
157800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157900010601     C     repfil        BEGSR
158000010601     C*
158100010601     C                   if        invfil = *zeros and
158200010601     C                             depfil > *zeros and
158300010629     C                             (vinflg = *blanks or
158400010629     C                              vinflg = *zeros)
158500010601     C
158600010601     C                   eval      invfil = depfil
158700010601     C                   endif
158800010601     C*
158900010601     C                   if        depfil <> invfil and
159000010601     C                             invfil > *zeros
159100010601     C                   eval      flgMulti = '1'
159200010601     C                   if        vinflg = *blanks
159300010601     C                   add       1             cntNonEl
159400010601     C                   endif
159500010601     C                   endif
159600010601     C*
159700010601     C                   if        vinflg = '2'
159800010601     C                   eval      flgStato = '2'
159900010601     C                   endif
160000010601     C*
160100010601     C                   ENDSR
160200010601     C***
160300010601
160400010601
160500010601
160600010330
160700040119      /TITLE Invio dei dati al punto operativo.
160800040119     C     invio         BEGSR
160900040119     C*
161000040119     C* 1° invio FIVAT
161100040119     C                   reset                   dscmz
161200040119     C                   move      vlrpoi        cmzdst
161300100315     C                   eval      cmzfld = 'FIVATWWR'
161400040119     C                   eval      cmzmbd = vlrhdl
161500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161600040119     C***                if        prmfir = *blanks
161700100315     C                   eval      cmzfla = 'FIVAT00F'
161800100315     C                   eval      cmzmba = 'FIVAT00F'
161900040119     C***                else
162000040119     C***                eval      cmzfla = prmfir
162100040119     C***                eval      cmzmba = prmfir
162200040119     C***                endif
162300040119     C                   eval      cmznrr = *zeros
162400040119     C                   move      §ctrokvt      cmznrr
162500040119     C                   eval      cmzlba = vlrfl1
162600040119     C                   call(e)   'TIS711C'
162700040119     C                   parm                    dscmz
162800040119     C                   parm      *blanks       esito
162900040119     C                   if        %error
163000040119     C                             or cmzerr = '1'
163100040119     C                             or esito  = '1'
163200040119     C                   eval      wrkesito = '3'
163300040119     C                   else
163400040119     C*
163500040119     C* 2° invio FIVAB
163600040119     C                   reset                   dscmz
163700040119     C                   move      vlrpoi        cmzdst
163800040119     C                   eval      cmzfld = vlrfou
163900040119     C                   eval      cmzmbd = vlrhdl
164000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164100040119     C***                if        prmfir = *blanks
164200100315     C                   eval      cmzfla = 'FIVAB00F'
164300100315     C                   eval      cmzmba = 'FIVAB00F'
164400040119     C***                else
164500040119     C***                eval      cmzfla = prmfir
164600040119     C***                eval      cmzmba = prmfir
164700040119     C***                endif
164800040119     C                   eval      cmznrr = *zeros
164900040119     C                   move      §ctrokvb      cmznrr
165000040119     C                   eval      cmzlba = vlrfl1
165100040119     C                   call(e)   'TIS711C'
165200040119     C                   parm                    dscmz
165300040119     C                   parm      *blanks       esito
165400040119     C                   if        %error
165500040119     C                             or cmzerr = '1'
165600040119     C                             or esito  = '1'
165700040119     C                   eval      wrkesito = '3'
165800040119     C                   endif
165900040119     C                   endif
166000040119     C*
166100040119     C                   ENDSR
166200040119     C***
166300010601
166400010601
166500010601
166600010601
166700010601      /TITLE Invio dei dati al punto operativo.
166800010601     C     opeini        BEGSR
166900010601     C*
167000010601     C* Inizializzo flag e contatori operativi
167100010601     C                   movel     '0'           flgGiro           1
167200010601     C                   movel     '0'           flgMulti          1
167300010601     C                   movel     '1'           flgStato          1
167400010615     C                   movel     '0'           flgOk             1
167500010601     C                   z-add     *zeros        cntNonEl         10 0
167600010601     C                   z-add     *zeros        depfil            3 0
167700010601     C                   z-add     *zeros        invfil            3 0
167800010601     C*
167900010601     C                   ENDSR
168000010601     C***
168100070326
168200070326
168300070326
168400070326
168500080916     C     *pssr         BEGSR
168600070329     C*
168700080916     C                   if        %open(tivin00r)
168800080916     C                   close     tivin00r
168900080916     C                   endif
169000100315     C                   if        %open(fivabwwr)
169100100315     C                   close     fivabwwr
169200080916     C                   endif
169300100315     C                   if        %open(fivatwwr)
169400100315     C                   close     fivatwwr
169500080916     C                   endif
169600070326     C*
169700070326     C* Effettuo la chiamata al CLLE preposto
169800100315     C                   call(e)   'TITVVTC'
169900080916     C                   parm                    parccm
170000080916     C                   parm                    parmbr
170100080916     C                   parm      '2'           paropz
170200070326     C*
170300080916     C                   eval      wrkesito = '2'
170400070404     C*
170500080916     C                   seton                                        LR
170600070326     C*
170700080916     C                   ENDSR     '*CANCL'
170800070326     C***
170900070326
171000100204
171100100204
171200100204     C     normCSV       BEGSR
171300100204     C*
171400100204     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
171500100204     C* separatori campi
171600100204     C                   if        CharTXT <> *blanks
171700100204     C                   z-add     *zeros        wLenVINDTA        4 0
171800100204     C                   z-add     *zeros        wFlgVINDTA        1 0
171900100204     C                   setoff                                       80
172000100204     C                   dow       wLenVINDTA < %len(%trim(vindta))
172100100204     C                   eval      wLenVINDTA = wLenVINDTA + 1
172200100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
172300100204     C                   if        not *in80
172400100204     C                   seton                                        80
172500100204     C                   else
172600100204     C                   setoff                                       80
172700100204     C                   endif
172800100204     C                   endif
172900100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173000100204     C                             and *in80 = *on
173100100204     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
173200100204     C                   endif
173300100204     C                   enddo
173400100204     C                   endif
173500100204     C*
173600100204     C                   ENDSR
173700100204     C***
173800070326
173900010330
174000010330
174100000613     C     *inzsr        BEGSR
174200990910     C*
174300990910     C     *entry        plist
174400990920     C                   parm                    tivlrds
174500990921     C                   parm      wrkesito      esito
174600000724     C                   parm                    prmlit
174700000710     C                   parm                    prmfir
174800010330     C*
174900010330     C* CALCOLA LA DATA CORRENTE
175000100302     C                   time                    wn14             14 0
175100100302     C                   movel     wn14          oracor            6 0          *ORA
175200100302     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
175300100204     C                   eval      datcor = %dec(%date() : *ISO)
175400100302     C*
175500100302     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
175600100302     C                   MOVE(p)   *blanks       wNomeFile        30
175700100302     C                   MOVE(p)   vlrMSG        wNomeFile
175800100302     C     minu:maiu     XLATE     wNomeFile     wNomeFile
175900000613     C*
176000000613     C                   ENDSR
176100000613     C***
176200990908
176300100315     Otitv1v9p  E            riepilogo         2
176400990915     O                                              'Upload via Internet'
176500990915     O                                           +1 'Traduzione TIVIN00R -'
176600100315     O                                           +1 'FIVABWWR'
176700990915     O                                           +1 'Testate bolle'
176800990915     O          E            riepilogo   2
176900990915     O                       wrkdata
177000990915     O                       wrkora              +1
177100990915     O                       procname            +1
177200990915     O          E            riepilogo   2
177300990915     O                                              'Cliente..................:'
177400010605     O                       VABCCM        z     +1
177500990915     O          E            riepilogo   2
177600990920     O                                              'Riferimento Strategi.....:'
177700990920     O                       vlrhdl              +1
177800990915     O          E            riepilogo   2
177900990915     O                                              'Giusti...................:'
178000040119     O                       §CTROKVB      2   +  1
178100990915     O          E            riepilogo   2
178200990915     O                                              'Sbagliati e corretti.....:'
178300971022     O                       §CTRMO        2   +  1
178400990915     O          E            riepilogo   2
178500990915     O                                              'Sbagliati e scartati.....:'
178600971022     O                       §CTRNO        2   +  1
178700000613
178800100315     Otitv1v9ps E            testdett          2
178900000613     O                                              'Upload via Internet'
179000000613     O                                           +1 'Traduzione TIVIN00R -'
179100100315     O                                           +1 'FIVABWWR'
179200000613     O                                           +1 'Testate bolle'
179300000616     O          E            testdett    3
179400000613     O                                           +2 'N° rec'
179500000613     O                                           +3 'Anteprima contenuto'
179600000616     O          E            rigadett    2
179700000613     O                       rrnum               +2
179800000621     O                       recko               +3
179900000616     O          E            findett     2
180000000613     O                       wrkdata
180100000613     O                       wrkora              +1
180200000613     O                       procname            +1
180300000616     O          E            findett     2
180400000613     O                                              'Cliente..................:'
180500010605     O                       VABCCM        z     +1
180600000616     O          E            findett     2
180700000613     O                                              'Riferimento Strategi.....:'
180800000613     O                       vlrhdl              +1
180900000616     O          E            findett     2
181000000613     O                                              'Giusti...................:'
181100040119     O                       §CTROKVB      2   +  1
181200000616     O          E            findett     2
181300000613     O                                              'Sbagliati e corretti.....:'
181400000613     O                       §CTRMO        2   +  1
181500000616     O          E            findett     2
181600000613     O                                              'Sbagliati e scartati.....:'
181700000613     O                       §CTRNO        2   +  1
181800000613** CMD - COMANDI CL
181900100315OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1V9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
182000100315OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1V9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
182100100315DLTOVR FILE(TITV1V9P TITV1V9PS) LVL(*)
182200010606
182300000613
182400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182500100316X001;X002;VABRMA;W_VABNSP;VABNCL;VABPKB;VABQFT;W_VABTIC;X003;
182600100316VABCAS;X005;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_E;X006
182700100302
182800100302
182900100302
