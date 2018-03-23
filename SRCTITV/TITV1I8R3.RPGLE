000100081027      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081027     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600081027     FFIVABwwr  O    E             DISK    usropn
000700081027     FFIVATwwr  O    E             DISK    usropn
000800081027     FFIVAB02L  uf   e           k disk    usropn
000900081027     F                                     prefix(w_)
001000081027     F                                     rename(fivab000:fivab002)
001100081027     F                                     extfile(LibFileVB)
001200081027     FFIVAT01L  if   e           k disk    usropn
001300081027     F                                     prefix(w_)
001400081027     F                                     rename(fivat000:fivat001)
001500081027     F                                     extfile(LibFileVT)
001600080915     Ftitv1i8p  O    f  132        PRINTER usropn
001700000621     F                                     oflind(*inoa)
001800080915     F                                     infsr(*pssr)
001900080915     Ftitv1i8ps O    f  198        PRINTER usropn
002000000621     F                                     oflind(*inob)
002100080915     F                                     infsr(*pssr)
002200000313     D*
002300000613     D*------------
002400000613     D* COMANDI
002500000613     D*------------
002600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002700040113     D*------------
002800040113     D* SCHIEDE A PROGRAMMA
002900040113     D*------------
003000040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
003100010330     D*----------------------------------------------------
003200010330     D* DICHIARAZIOINE VARIABILI DI WRK
003300010330     D*----------------------------------------------------
003400010330     D dscmz         e ds                  inz
003500010330     D psds           sds
003600010330     D  procname         *PROC
003700010330     D tivlrds       e ds                  extname(tivlr00f)
003800040128     D tisi95ds      e ds
003900010330     D esito           s              1
004000010330     D prmlit          s             10
004100010330     D prmfir          s             10
004200010330     D wrkesito        s                   like(esito)
004300010330     D wrkdata         s               d
004400010330     D wrkora          s               t
004500010330     D rrnum           s              6  0 INZ(*zeros)
004600010330     D recko           s            150    INZ(*blanks)
004700011113     D depcmd          s            150    INZ(*blanks)
004800040510     D parccm          s              8    INZ(*blanks)
004900040119     D parmbr          s             10    INZ(*blanks)
005000040119     D paropz          s              1    INZ(*blanks)
005100040119     D chkcall         s              1    INZ(*blanks)
005200080125     D wFlgCAS         s              1    INZ(*blanks)
005300080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
005400081027     D LibFileVB       s             21A   inz
005500081027     D LibFileVT       s             21A   inz
005600081027     D currSysNeta     s              8A   inz('*NULL')
005700030715     D*------------------
005800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005900030715     D*------------------
006000030715     D SkSplitFLD      S             10    DIM(100)
006100030715     D SkSplitCSV      S            256    DIM(100)
006200030715     D CharCSV         S              1
006300030715     D CharTXT         S              1
006400030715     D CharNUM         S              1
006500030715     D posDa           S              3  0 INZ(*zeros)
006600030715     D posA            S              3  0 INZ(*zeros)
006700030715     D i               s              3  0 INZ(1)
006800030715     D wGiro           s              1  0 INZ(*zeros)
006900080125     D wVATNOT_A       s                   LIKE(VATNOT)
007000080125     D wVATNOT_B       s                   LIKE(VATNOT)
007100080125     D wVATNOT_E       s                   LIKE(VATNOT)
007200030822     D*------------------
007300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007400030822     D*------------------
007500030822     D posDaDft        S              3  0 INZ(*zeros)
007600030822     D posADft         S              3  0 INZ(*zeros)
007700030822     D j               s              3  0 INZ(1)
007800030822     D wGiroDft        s              1  0 INZ(*zeros)
007900010330     D*------------------
008000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
008100010330     D*------------------
008200010330     D WLBDA8          DS                  INZ
008300010330     D  G08DAT                 1      8  0
008400010330     D  G08INV                 9     16  0
008500010330     D  G08ERR                17     17
008600010330     D  G08TGI                18     22  0
008700031201     D*------------------
008800031201     D* Costanti
008900031201     D*------------------
009000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200080617     D*------------------
009300080617     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
009400080617     D*------------------
009500080617     D PiStr           S            100A   INZ
009600080617     D PiDecChr        S              1A   INZ
009700080617     D PiVal           S             63S30 INZ
009800080617     D PiInt           S               N   INZ
009900080617     D PiNum           S               N   INZ
010000081027
010100081027     D*------------------
010200081027     D* PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
010300081027     D*------------------
010400081027     D  PiOutSysNeta   S              8A
010500080617
010600081027
010700080617     D*------------------
010800080617     D* LINKING A DEFINIZIONI ESTERNE
010900080617     D*------------------
011000080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
011100081027     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
011200010330
011300010330
011400010330
011500081027
011600081027
011700081027     C*
011800081027     C* Reperisco il sistema AS/400 corrente
011900081027     C                   callp     UBRTVNETA_Rtv(currSysNeta)
012000081027     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
012100081027     C                   eval      LibFileVB = 'FILTRAGRU/FIVAB02L'
012200081027     C                   eval      LibFileVT = 'FILTRAGRU/FIVAT01L'
012300081027     C                   else
012400081027     C                   eval      LibFileVB = 'FILTRAGRPF/FIVAB02L'
012500081027     C                   eval      LibFileVT = 'FILTRAGRPF/FIVAT01L'
012600081027     C                   endif
012700081027     C*
012800081027     C                   if        not %open(FIVAB02L)
012900081027     C                   open      FIVAB02L
013000081027     C                   endif
013100081027     C                   if        not %open(FIVAT01L)
013200081027     C                   open      FIVAT01L
013300081027     C                   endif
013400081027     C*
013500990915     C                   time                    wrkdata
013600990915     C                   time                    wrkora
013700000913     C                   reset                   rrnum
013800990921     C                   reset                   esito
013900990921     C                   reset                   wrkesito
014000010601     C*
014100010601     C                   exsr      opeini
014200010605     C                   exsr      rwvab
014300081027     C*
014400081027     C                   if        %open(FIVAB02L)
014500081027     C                   close     FIVAB02L
014600081027     C                   endif
014700081027     C                   if        %open(FIVAT01L)
014800081027     C                   close     FIVAT01L
014900081027     C                   endif
015000081027     C*
015100081027     C                   if        %open(titv1i8ps)
015200081027     C                   except    findett
015300081027     C                   close     titv1i8ps
015400081027     C                   endif
015500010601     C*
015600010601     C                   seton                                        lr
015700010601
015800010601
015900010601
016000010601     C*--------------------------------------------------------
016100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
016200010601     C*--------------------------------------------------------
016300010601     C     PREELA        BEGSR
016400010601     C*
016500010601     C* SE OCCORRE SPEDIRE IN FILIALE
016600010601     C                   if        invfil <> *zeros and
016700010601     C                             flgGiro = '0'
016800010601     C*
016900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
017000010601     C                   eval      flgGiro = '1'
017100000724     C*
017200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
017300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
017400010601     C     invfil        chain     azorg01l
017500000724     C                   if        %found
017600000724     C                   movel(p)  CMD(1)        depcmd
017700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
017800000724     C*
017900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
018000011113     C                   Z-ADD     150           LENGH            15 5
018100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018200000724     C                   PARM                    depcmd
018300000724     C                   PARM                    LENGH
018400000724     C*
018500000724     C                   endif
018600000616     C*
018700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018800000616     C                   movel(p)  CMD(2)        depcmd
018900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
019000000616     C*
019100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
019200011113     C                   Z-ADD     150           LENGH            15 5
019300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019400000616     C                   PARM                    depcmd
019500000616     C                   PARM                    LENGH
019600000613     C*
019700080915     C                   if        not %open(titv1i8ps)
019800080915     C                   open      titv1i8ps
019900070329     C                   except    testdett
020000000613     C                   endif
020100010601     C*
020200010601     C                   endif
020300010601     C*
020400010601     C                   ENDSR
020500010601     C***
020600010601
020700010601
020800010601
020900010601
021000010601     C*--------------------------------------------------------
021100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
021200010601     C*--------------------------------------------------------
021300010601     C     ENDELA        BEGSR
021400990915     C*
021500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
021600040119     C*
021700040119     C* Effettuo la chiamata al CLLE preposto
021800081027     C                   call(e)   'TITVVTC'
021900040510     C                   parm                    parccm
022000040119     C                   parm                    parmbr
022100040119     C                   parm      '2'           paropz
022200050201     C*
022300050201     C* Effettuo lancio TISI95 solo x chiusura
022400050201     C                   CLEAR                   TISI95DS
022500050201     C                   EVAL      I95TLA = 'C'
022600050201     C                   CALL      'TISI95R'
022700050201     C                   PARM                    TISI95DS
022800000613     C*
022900080915     C                   if        %open(titv1i8ps)
023000000616     C                   except    findett
023100080915     C                   close     titv1i8ps
023200000613     C                   endif
023300000616     C*
023400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
023500011113     C                   Z-ADD     150           LENGH            15 5
023600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
023700000616     C                   PARM                    CMD(3)
023800000616     C                   PARM                    LENGH
023900000616     C*
024000010601     C                   ENDSR
024100010601     C***
024200010601
024300010601
024400010601
024500000613
024600000613     C*--------------------------------------------------------
024700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
024800000613     C*--------------------------------------------------------
024900000621     C     STPR          BEGSR
025000000613     C*
025100080915     C                   if        not %open(titv1i8p)
025200080915     C   50              open      titv1i8p
025300990915     C                   endif
025400990915     C*
025500030715     C   50              except    riepilogo
025600990915     C*
025700080915     C                   if        %open(titv1i8p)
025800080915     C   50              close     titv1i8p
025900990914     C                   endif
026000000613     C*
026100000613     C                   ENDSR
026200000613     C***
026300000613
026400010601
026500010601
026600010330     C*--------------------------------------------------------
026700081027     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
026800010330     C*--------------------------------------------------------
026900010605     C     RWVAB         BEGSR
027000010330     C*
027100010330     C                   if        not %open(tivin00r)
027200010330     C                   open      tivin00r
027300010330     C                   endif
027400081027     C                   if        not %open(fivabwwr)
027500081027     C                   open      fivabwwr
027600010330     C                   endif
027700040119     C*
027800081027     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
027900040119     C                   exsr      prevat
028000040119     C*
028100040119     C                   if        chkcall = '0'
028200040119     C*
028300081027     C                   if        not %open(fivatwwr)
028400081027     C                   open      fivatwwr
028500040119     C                   endif
028600010330     C*
028700040119     C                   clear                   §CTROKVB          7 0
028800040119     C                   clear                   §CTROKVT          7 0
028900040119     C                   clear                   §CTRMO            7 0
029000040119     C                   clear                   §CTRNO            7 0
029100070823     C*
029200070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
029300070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
029400080915     C                   exsr      inzvar
029500080915     C                   exsr      impvab
029600010330     C*
029700030822     C                   do        *HIVAL
029800010330     C*
029900030822     C                   read      tivin00r                               70
030000010618     C*
030100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
030200010618     C                   if        vinflg = '1'
030300010618     C                   eval      flgOk = '1'
030400010618     C                   endif
030500040510     C*
030600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
030700040510     C                   z-add     *zeros        wLenVINDTA        4 0
030800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
030900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
031000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
031100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
031200040510     C                   z-add     1             wFlgVINDTA
031300040510     C                   leave
031400040510     C                   endif
031500040510     C                   enddo
031600080915 xxx C*
031700080915     C* Aggiungo un byte a '*' all'inizio d ogni record
031800080915     C                   if        %subst(vindta:1:1) = CharCSV
031900080915     C                   eval      vindta  = '*' + %trim(vindta)
032000080915     C                   endif
032100081027     C*
032200081027     C* Forzatura controllo x scarto record nn "interessanti"
032300081027     C                   if        %subst(vindta:4:4) <> '2008'
032400081027     C                   z-add     *zeros        wFlgVINDTA
032500081027     C                   endif
032600010618     C*
032700040510     C                   if        vindta > *blanks AND
032800040510     C                             wFlgVINDTA = 1
032900010330     C                   add       1             rrnum
033000010330     C*
033100010601     C                   if        *in70 = *off and
033200010330     C                             (vinflg = *blanks
033300010330     C                              or vinflg = '0'
033400010330     C                              or vinflg = '2')
033500010330     C*
033600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
033700010711     C                   if        vinflg = *blanks or vinflg = '0'
033800010711     C                   clear                   vinmsg
033900010711     C                   endif
034000070925     C*
034100080915     C                   exsr      inzvar
034200010605     C                   exsr      impvab
034300010601     C*
034400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
034500010605     C                   eval      depfil = VABLNP
034600010601     C                   exsr      repfil
034700010601     C                   if        depfil = invfil
034800021025     C                   if        vlrpoi = 999
034900030822     C                   move(P)   invfil        VABFGS
035000021025     C                   else
035100030822     C                   move(P)   vlrpoi        VABFGS
035200021025     C                   endif
035300010601     C*
035400010601     C                   exsr      PREELA
035500010601     C*
035600010604     C* Ebbene...
035700010604     C*
035800040119     C  N31              add       1             §CTROKVB
035900040119     C   32              add       1             §CTRMO
036000040119     C   31              add       1             §CTRNO
036100030822     C                   if        wGiro = 2
036200040510     C* Scrivo sempre il VAT
036300040510     C  N31              exsr      wrivat
036400040510     C* Scrivo il VAB solo a rottura d spedizione
036500040119     C  N31              exsr      wrivab
036600040119     C                   endif
036700010604     C*
036800010604     C                   if        *in31 = *off and
036900010604     C                             *in32 = *off
037000010604     C                   eval      vinflg = '1'
037100010604     C                   else
037200010604     C                   eval      recko = vindta
037300010604     C                   except    rigadett
037400010604     C                   eval      vinflg = '2'
037500010604     C                   endif
037600010604     C                   endif
037700010601     C*
037800010604     C                   endif
037900010604     C*
038000010330     C                   else
038100010330     C                   eval      vinflg = '1'
038200010330     C                   endif
038300010601     C*
038400010601     C  N70              update    tivin000
038500010330     C*
038600030822     C  N70              enddo
038700040119     C*
038800040119     C                   endif                                                  (endif - chkcall)
038900010601     C*
039000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
039100010601     C                   if        cntNonEl = *zeros or
039200010601     C                             flgMulti = '0'
039300010330     C* Se non ci sono record con errori ...
039400010601     C                   if        §ctrno = 0 and
039500010604     C                             §ctrmo = 0 and
039600010601     C                             flgStato <> '2'
039700010330     C* ... restituisco esito OK.
039800010330     C                   eval      wrkesito = '0'
039900010330     C                   else
040000040119     C                   if        §ctrokvb > 0
040100010330     C                   eval      wrkesito = '1'
040200010330     C                   else
040300010615     C                   if        flgOk = '0'
040400010615     C                   eval      wrkesito = '2'
040500010615     C                   else
040600010615     C                   eval      wrkesito = '6'
040700010615     C                   endif
040800010330     C                   endif
040900010330     C                   endif
041000010601     C                   else
041100010601     C                   eval      wrkesito = '9'
041200010601     C                   endif
041300010330     C*
041400010330     C                   if        %open(tivin00r)
041500010330     C                   close     tivin00r
041600010330     C                   endif
041700081027     C                   if        %open(fivabwwr)
041800081027     C                   close     fivabwwr
041900010330     C                   endif
042000081027     C                   if        %open(fivatwwr)
042100081027     C                   close     fivatwwr
042200040512     C                   endif
042300010601     C*
042400010601     C                   if        vlrpoi <> 999
042500010601     C                   eval      invfil = vlrpoi
042600010601     C                   endif
042700010330     C*
042800040119     C                   if        §ctrokvb > 0
042900010601     C                             and invfil > *zeros
043000010330     C                   exsr      invio
043100010330     C                   endif
043200010601     C*
043300010618     C                   if        flgGiro = '1'
043400010601     C                   exsr      endela
043500010618     C                   endif
043600010330     C*
043700010330     C                   ENDSR
043800010330     C***
043900040119
044000040119
044100040119
044200040119     C*----------------------------------------------------*
044300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
044400040119     C*----------------------------------------------------*
044500040510     C     WRIVAB        BEGSR
044600081027     C*
044700081027     C* Forzatura x anomalia cliente => se peso a zero => ignorare completamente record
044800081027     C                   if        VABPKB > *zeros
044900081027     C*
045000081027     C* Verifico che stessa testata nn già presente
045100081027     C     KEYvab02_C    CHAIN     fivab02l
045200081027     C                   IF        %found(fivab02l)
045300081027     C                   IF        vlrpoi <> 789
045400081027     C                   EVAL      w_vabPKB = w_vabPKB + vabPKB
045500081027 xxx C***                EVAL      w_vabNCL = w_vabNCL + vabNCL
045600081027     C                   UPDATE    FIVAB002
045700081027     C                   ENDIF
045800081027     C                   ELSE
045900081027     C                   WRITE     FIVAB000
046000081027     C                   ENDIF
046100040119     C*
046200080917     C                   endif
046300080917     C*
046400040119     C                   ENDSR
046500040119
046600040119
046700040119
046800040119     C*----------------------------------------------------*
046900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
047000040119     C*----------------------------------------------------*
047100040510     C     WRIVAT        BEGSR
047200060512     C*
047300060512     C* Inizializzo i campi chiave
047400060512     C                   eval      VATFGS = VABFGS
047500060512     C                   eval      VATAAS = VABAAS
047600060512     C                   eval      VATCCM = VABCCM
047700070823     C                   eval      VATNRS = VABNRS
047800060512     C                   eval      VATNSP = VABNSP
047900060512     C                   eval      VATLNP = VABLNP
048000060331     C*
048100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
048200060331     C                   if        wVATNOT_A <> *blanks
048300060331     C                   eval      VATNOT = wVATNOT_A
048400060331     C                   eval      VATTRC = 'A'
048500081027     C                   exsr      updwriVAT
048600060331     C                   add       1             §CTROKVT
048700060331     C                   endif
048800060331     C*
048900060331     C                   if        wVATNOT_B <> *blanks
049000060331     C                   eval      VATNOT = wVATNOT_B
049100060331     C                   eval      VATTRC = 'B'
049200081027     C                   exsr      updwriVAT
049300060331     C                   add       1             §CTROKVT
049400060331     C                   endif
049500060331     C*
049600080625     C                   if        wVATNOT_E <> *blanks
049700060331     C                   eval      VATNOT = wVATNOT_E
049800060331     C                   eval      VATTRC = 'E'
049900081027     C                   exsr      updwriVAT
050000080716     C                   add       1             §CTROKVT
050100080625     C                   endif
050200040119     C*
050300040119     C                   ENDSR
050400081027
050500081027
050600081027
050700081027     C*----------------------------------------------------*
050800081027     C*  SCARICAMENTO BUFFER RECORDS VAT
050900081027     C*----------------------------------------------------*
051000081027     C     UPDWRIVAT     BEGSR
051100081027     C*
051200081027     C* Verifico che stessa testata nn già presente
051300081027     C     KEYvat01_C    SETLL     fivat01l
051400081027     C                   IF        %equal(fivat01l)
051500081027     C                   ELSE
051600081027     C                   WRITE     FIVAT000
051700081027     C                   ENDIF
051800081027     C*
051900081027     C                   ENDSR
052000040119
052100040119
052200040119
052300040119     C*----------------------------------------------------*
052400081027     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052500040119     C*----------------------------------------------------*
052600040119     C     PREVAT        BEGSR
052700040119     C*
052800081027     C* Compongo il nome del membro da dare al FIVATWWR
052900040119     C                   eval      parmbr = vlrhdl
053000040119     C                   movel     'M'           parmbr
053100040510     C                   eval      parccm = vlrksc
053200040119     C                   eval      paropz = '1'
053300040119     C* Effettuo la chiamata al CLLE preposto
053400081027     C                   call(e)   'TITVVTC'
053500040119     C                   parm                    parccm
053600040119     C                   parm                    parmbr
053700040119     C                   parm                    paropz
053800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053900040119     C                   if        %error
054000040119     C                   movel     '1'           chkcall
054100040119     C                   else
054200040119     C                   movel     '0'           chkcall
054300040119     C                   endif
054400040119     C*
054500040119     C                   ENDSR
054600010601
054700010601
054800010601
054900010330     C*----------------------------------------------------*
055000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
055100010330     C*----------------------------------------------------*
055200040119     C     INZVAR        BEGSR
055300010330     C*
055400030822     C* Inizializzo variabili di wrk
055500020204     C                   Z-ADD     *zeros        Num5_0            5 0
055600060331     C                   MOVEL     *blanks       wVATNOT_A
055700060331     C                   MOVEL     *blanks       wVATNOT_B
055800060331     C                   MOVEL     *blanks       wVATNOT_E
055900080617     C                   EVAL      wFlgCAS = *blanks
056000030822     C*
056100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056200081027     C                   CLEAR                   FIVAB000
056300081027     C                   CLEAR                   FIVAT000
056400030822     C                   CLEAR                   SkSplitCSV
056500030822     C                   EVAL      i = 1
056600030822     C                   EVAL      posDa = *zeros
056700030822     C                   EVAL      posA  = *zeros
056800030822     C*
056900030822     C* Reimposto i valori di default
057000030822     C                   EXSR      DEFCAM
057100030822     C*
057200010330     C                   ENDSR
057300010330     C*----------------------------------------------------*
057400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057500010330     C*----------------------------------------------------*
057600010330     C     DEFCAM        BEGSR
057700030822     C*
057800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057900030715     C* e delimitatore testo.
058000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058300030822     C*
058400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
058600030715     C                   SETON                                        50
058700030715     C                   ELSE
058800030715     C                   SETOFF                                       50
058900030715     C                   ENDIF
059000030822     C*
059100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059200030822     C                   EVAL      posDaDft = 1
059300030822     C                   EVAL      posADft  = 0
059400030822     C                   EVAL      wGiroDft = 0
059500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059600030822     C                             posDaDft > 0
059700030822     C*
059800030822     C* Gestisco il 1° giro
059900030822     C                   IF        wGiroDft = 0
060000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060200030822     C* Incremento il contatore dei "giri"
060300030822     C                   EVAL      wGiroDft = 1
060400030822     C                   ELSE
060500030822     C                   EVAL      posDaDft = posADft
060600030822     C                   ENDIF
060700030822     C* Eseguo lo scan x trovare la fine del campo corrente
060800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060900030822     C*
061000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061200030822     C                   IF        posDaDft > 0 AND
061300030822     C                             posADft  > 0
061400030822     C* NCL
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'NCL'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiInt=*on
062400030822     C                   Z-ADD     PiVal         VABNCL
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* CCM
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'CCM'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiInt=*on
063700030822     C                   Z-ADD     PiVal         VABCCM
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* LNP
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'LNP'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABLNP
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* NRS
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'NRS'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABNRS
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* CTR
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'CTR'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABCTR
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* PKB
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'PKB'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiNum=*on
068900030822     C                   Z-ADD     PiVal         VABPKB
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* VLB
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'VLB'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiNum=*on
070200030822     C                   Z-ADD     PiVal         VABVLB
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* QFT
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'QFT'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiNum=*on
071500030822     C                   Z-ADD     PiVal         VABQFT
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* CBO
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'CBO'
072300030822     C                   EVAL      VABCBO=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* TSP
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'TSP'
073200030822     C                   EVAL      VABTSP=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* VAS
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'VAS'
074100030822     C                   EVAL      VABVAS=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VCA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VCA'
075000030822     C                   EVAL      VABVCA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400070925     C* TIC
075500070925     C                   IF        %subst(
075600070925     C                             %subst(vlrppt:posDaDft+1:
075700070925     C                             posADft-posDaDft-1):1:3)
075800070925     C                             = 'TIC'
075900070925     C                   EVAL      VABTIC=%trim(%subst(
076000070925     C                             %subst(vlrppt:posDaDft+1:
076100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200070925     C                   ENDIF
076300030822     C* GCA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GCA'
076800030822     C                   EVAL      VABGCA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* CTM
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'CTM'
077700030822     C                   EVAL      VABCTM=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* FFD
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'FFD'
078600030822     C                   EVAL      VABFFD=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* VAD
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'VAD'
079500030822     C                   EVAL      VABVAD=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* GMA
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'GMA'
080400030822     C                   EVAL      VABGMA=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* GGA
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'GGA'
081300030822     C                   EVAL      VABGGA=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* GVA
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'GVA'
082200030822     C                   EVAL      VABGVA=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600030822     C* TC1
082700030822     C                   IF        %subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):1:3)
083000030822     C                             = 'TC1'
083100030822     C                   EVAL      VABTC1=%trim(%subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400030822     C                   ENDIF
083500030822     C* TC2
083600030822     C                   IF        %subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):1:3)
083900030822     C                             = 'TC2'
084000030822     C                   EVAL      VABTC2=%trim(%subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300030822     C                   ENDIF
084400040714     C* VATTRC
084500040714     C                   IF        %subst(
084600040714     C                             %subst(vlrppt:posDaDft+1:
084700040714     C                             posADft-posDaDft-1):1:3)
084800040714     C                             = 'TRC'
084900040714     C                   EVAL      VATTRC=%trim(%subst(
085000040714     C                             %subst(vlrppt:posDaDft+1:
085100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200040714     C                   ENDIF
085300030822     C* ...
085400030822     C                   ENDIF
085500030822     C                   ENDDO
085600020204     C*
085700010330     C                   ENDSR
085800010607     C*----------------------------------------------------*
085900081027     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086000010607     C*----------------------------------------------------*
086100010607     C     IMPVAB        BEGSR
086200010607     C*
086300010607     C                   SETOFF                                       3132
086400030515     C*
086500010607     C                   MOVEL     datcor        VABAAS
086600010607     C                   MOVE      datcor        VABMGS
086700010607     C*
086800030715     C**********
086900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087000030715     C**********
087100030715     C                   IF        wGiro = *zeros
087200080716     C*
087300080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
087400080716     C                   EVAL      depvinDTA = vinDTA
087500040113     C*
087600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087800040510     C                             %trim(intcol(3))<>*blanks
087900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088000040113     C                                      %trim(intcol(3))
088100070823     C     ';':CharCSV   XLATE     vindta        vindta
088200040510     C                   ENDIF
088300080626     C*
088400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088500031201     C     minu:maiu     XLATE     vindta        vindta
088600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088700030715     C                   DOW       posDa <= %len(%trim(vindta))
088800030715     C*
088900030715     C* Gestisco il 1° campo
089000030715     C                   IF        i = 1
089100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089200030715     C                   EVAL      posDa = 1
089300030715     C* Eseguo lo scan x trovare la fine del primo campo
089400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C* X i campi successivi al 1°
089900030715     C                   ELSE
090000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090100030715     C                   EVAL      posDa = posA + 1
090200030715     C* Eseguo lo scan x trovare la fine del campo corrente
090300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090400030715     C* Gestisco l'ultimo campo
090500030715     C                   IF        posA = *zeros
090600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C                   LEAVE
091100030715     C                   ELSE
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C                   ENDIF
091600030715     C                   ENDIF
091700030715     C* Incremento il contatore d campo
091800030715     C                   EVAL      i = i +1
091900030715     C                   ENDDO
092000030715     C*
092100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092200030715     C* delimitazione testo
092300030715     C                   EVAL      i = 1
092400030715     C                   DOW       i < %elem(SkSplitFLD)
092500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092700030715     C                   EVAL      i = i + 1
092800030715     C                   ENDDO
092900030715     C                   EVAL      wGiro = 1
093000080716     C* Ripristino la 1° riga originale
093100080716     C                   EVAL      vinDTA = depvinDTA
093200030715     C                   ELSE
093300030715     C                   EVAL      wGiro = 2
093400031204     C**********
093500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093600031204     C* d separatore campo
093700031204     C**********
093800031204     C                   EVAL      vindta = %trim(vindta)
093900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094000031204     C                   EVAL      vindta = %subst(vindta:2)
094100031204     C                   ENDDO
094200031204     C*
094300031204     C                   Z-ADD     *zeros        lunghInput        4 0
094400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094800031204     C                   ENDDO
094900030715     C**********
095000030715     C* Effettuo lo split del campo dati d input
095100030715     C**********
095200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095300030715     C                   DOW       posDa <= %len(%trim(vindta))
095400030715     C*
095500030715     C* Gestisco il 1° campo
095600030715     C                   IF        i = 1
095700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095800030715     C                   EVAL      posDa = 1
095900030715     C* Eseguo lo scan x trovare la fine del primo campo
096000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096100030715     C* A questo punto "estraggo" il campo corrente
096200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096300030715     C                                                    (posA-posDa))
096400030715     C* X i campi successivi al 1°
096500030715     C                   ELSE
096600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096700030715     C                   EVAL      posDa = posA + 1
096800031204     C* Verifico che nn vi sia il campo nn valorizzato
096900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097000031204     C* Se campo nn valorizzato skippo al prossimo
097100031204     C                   EVAL      SkSplitCSV(i) = *blanks
097200031204     C                   EVAL      posA  = posA + 1
097300031204     C                   ELSE
097400030715     C* Eseguo lo scan x trovare la fine del campo corrente
097500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097600030715     C* Gestisco l'ultimo campo
097700030715     C                   IF        posA = *zeros
097800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097900030715     C* A questo punto "estraggo" il campo corrente
098000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098100030715     C                                                    (posA-posDa))
098200030715     C                   LEAVE
098300030715     C                   ELSE
098400030715     C* A questo punto "estraggo" il campo corrente
098500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098600030715     C                                                    (posA-posDa))
098700030715     C                   ENDIF
098800031204     C                   ENDIF
098900030715     C                   ENDIF
099000030715     C* Incremento il contatore d campo
099100030715     C                   EVAL      i = i +1
099200030715     C                   ENDDO
099300030715     C*
099400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099500030715     C* delimitazione testo
099600030715     C                   EVAL      i = 1
099700030715     C                   DOW       i < %elem(SkSplitCSV)
099800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100000030715     C                   EVAL      i = i + 1
100100030715     C                   ENDDO
100200030715     C*
100300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100400030715     C                   EVAL      i = 1
100500030715     C                   DOW       i < %elem(SkSplitFLD)
100600030715     C*
100700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100900030715     C                             SkSplitCSV(i) <> *blanks
101000030721     C*** ATB
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101200030721     C                   EVAL      VABATB = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030715     C*** RSD
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101900030715     C* ==
102000030715     C                   ENDIF
102100030721     C*** RD2
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030715     C*** IND
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102700030715     C                   EVAL      VABIND = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** LOD
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** PRD
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030721     C*** NZD
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030715     C*** RMA
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** NOT
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** NT2
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** VCA
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** VAS
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** FFD
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030721     C*** GC1
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** GC2
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TSP
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107600030721     C                   ENDIF
107700040510     C*** CBO
107800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108000040510     C                   ENDIF
108100030721     C*** NAS
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TIC
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** GCA
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** XCO
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** CTM
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** TCR
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** CTS
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** FTM
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** VAD
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** GMA
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** GGA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** GVA
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** TC1
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** TC2
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** SCL
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** RMO
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** NMO
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*
115000030721     C* Reperisco quindi i campi numerici...
115100040510     C*** NSP
115200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115300040510     C                   EVAL      PiStr=SkSplitCSV(i)
115400040510     C                   EXSR      CHKNUM
115500040510     C                   IF        PiInt=*on
115600040510     C                   Z-ADD     PiVal         VABNSP
115700040510     C                   ELSE
115800040510     C                   SETON                                        31
115900040510     C                   EVAL      VABNSP = *zeros
116000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116100040510     C                             + ' ' + 'VABNSP'
116200040510     C                   ENDIF
116300040510     C                   ENDIF
116400071106     C*** RMN
116500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116600071106     C                   EVAL      PiStr=SkSplitCSV(i)
116700071106     C                   EXSR      CHKNUM
116800071106     C                   IF        PiInt=*on
116900071106     C                   Z-ADD     PiVal         VABRMN
117000071106     C                   ELSE
117100071106     C                   SETON                                        32
117200071106     C                   EVAL      VABRMN = *zeros
117300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117400071106     C                             + ' ' + 'VABRMN'
117500071106     C                   ENDIF
117600071106     C                   ENDIF
117700030721     C*** CAD
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on
118200030721     C                   Z-ADD     PiVal         Num5_0
118300030721     C                   MOVEL(p)  Num5_0        VABCAD
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABCAD = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABCAD'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** NCL
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on
119600030721     C                   Z-ADD     PiVal         VABNCL
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABNCL = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100040122     C                             + ' ' + 'VABNCL'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** PKB
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABPKB
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      VABPKB = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABPKB'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** CAS
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121900080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
122000080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
122100080617     C                             %trim(SkSplitCSV(i)) <> '0'
122200080125     C                   EVAL      wFlgCAS = '1'
122300080125     C                   ENDIF
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiNum=*on
122700030721     C                   Z-ADD     PiVal         VABCAS
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      VABCAS = *zeros
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABCAS'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** CCM
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on      AND
124000030721     C                             PiVal<=9999999 AND
124100030721     C                             PiVal>*zeros
124200030721     C                   Z-ADD     PiVal         VABCCM
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      VABCCM = *zeros
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700040510     C                             + ' ' + 'VABCCM'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** LNP
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on    AND
125500030721     C                             PiVal<=999   AND
125600030721     C                             PiVal>*zeros
125700030721     C                   Z-ADD     PiVal         VABLNP
125800030721     C                   ELSE
125900040715     C                   SETON                                        32
126000030721     C                   EVAL      VABLNP = *zeros
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200040510     C                             + ' ' + 'VABLNP'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** NRS
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiInt=*on AND
127000030721     C                             PiVal<=99
127100030721     C                   Z-ADD     PiVal         VABNRS
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABNRS'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** LNA
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on    AND
128300030721     C                             PiVal<=999   AND
128400030721     C                             PiVal>*zeros
128500030721     C                   Z-ADD     PiVal         VABLNA
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      VABLNA = *zeros
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABLNA'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** CTR
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on  AND
129800030721     C                             PiVal<=999
129900030721     C                   Z-ADD     PiVal         VABCTR
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABCTR'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** IAS
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiNum=*on
131100030721     C                   Z-ADD     PiVal         VABIAS
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABIAS'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** VLB
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiNum=*on
132300030721     C                   Z-ADD     PiVal         VABVLB
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABVLB'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** QFT
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiNum=*on
133500030721     C                   Z-ADD     PiVal         VABQFT
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABQFT'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** NCD
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiInt=*on      AND
134700030721     C                             PiVal<=9999999
134800030721     C                   Z-ADD     PiVal         VABNCD
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABNCD'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** NCA
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on      AND
136000030721     C                             PiVal<=9999999
136100030721     C                   Z-ADD     PiVal         VABNCA
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABNCA'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** ZNC
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on AND
137300030721     C                             PiVal<=99
137400030721     C                   Z-ADD     PiVal         VABZNC
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABZNC'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** DCR
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiInt=*on       AND
138600030721     C                             PiVal<=99999999
138700030721     C                   Z-ADD     PiVal         VABDCR
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABDCR'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400030721     C*** HCR
139500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139600030721     C                   EVAL      PiStr=SkSplitCSV(i)
139700030721     C                   EXSR      CHKNUM
139800030721     C                   IF        PiInt=*on   AND
139900030721     C                             PiVal<=9999
140000030721     C                   Z-ADD     PiVal         VABHCR
140100030721     C                   ELSE
140200030721     C                   SETON                                        32
140300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140400030721     C                             + ' ' + 'VABHCR'
140500030721     C                   ENDIF
140600030721     C                   ENDIF
140700030721     C*** VMD
140800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140900030721     C                   EVAL      PiStr=SkSplitCSV(i)
141000030721     C                   EXSR      CHKNUM
141100030721     C                   IF        PiNum=*on
141200030721     C                   Z-ADD     PiVal         VABVMD
141300030721     C                   ELSE
141400030721     C                   SETON                                        32
141500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141600030721     C                             + ' ' + 'VABVMD'
141700030721     C                   ENDIF
141800030721     C                   ENDIF
141900030721     C*** ANT
142000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142100030721     C                   EVAL      PiStr=SkSplitCSV(i)
142200030721     C                   EXSR      CHKNUM
142300030721     C                   IF        PiInt=*on        AND
142400030721     C                             PiVal<=999999999
142500030721     C                   Z-ADD     PiVal         VABANT
142600030721     C                   ELSE
142700030721     C                   SETON                                        32
142800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142900030721     C                             + ' ' + 'VABANT'
143000030721     C                   ENDIF
143100030721     C                   ENDIF
143200030721     C*** CMO
143300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143400030721     C                   EVAL      PiStr=SkSplitCSV(i)
143500030721     C                   EXSR      CHKNUM
143600030721     C                   IF        PiInt=*on
143700030721     C                   Z-ADD     PiVal         Num5_0
143800030721     C                   MOVEL(p)  Num5_0        VABCMO
143900030721     C                   ELSE
144000030721     C                   SETON                                        32
144100030721     C                   EVAL      VABCMO = *zeros
144200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144300030721     C                             + ' ' + 'VABCMO'
144400030721     C                   ENDIF
144500030721     C                   ENDIF
144600040510     C*
144700081027     C* Reperisco i campi del FIVAT
144800040510     C*** VATNOT
144900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
145000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
145100040510     C                   ENDIF
145200040510     C*** VATTRC
145300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145500040510     C                   ENDIF
145600060331     C*** VATNOT_A
145700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145900060331     C                   ENDIF
146000060331     C*** VATNOT_B
146100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
146200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
146300060331     C                   ENDIF
146400060331     C*** VATNOT_E
146500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146700060331     C                   ENDIF
146800070823     C*
146900071029     C***  ===>  Gestione campi "particolari" dentro ciclo
147000071204     C*
147100080915     C*** RMA/RMN/NSP
147200080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
147300080915     C                   EVAL      VABRMA=SkSplitCSV(i)
147400080716     C                   EVAL      PiStr=SkSplitCSV(i)
147500071204     C                   EXSR      CHKNUM
147600071204     C                   IF        PiInt=*on
147700081027     C                   Z-ADD     PiVal         VABNSP
147800080915     C                   Z-ADD     PiVal         VABRMN
147900071204     C                   ELSE
148000080915     C                   SETON                                        31
148100080915     C                   EVAL      VABRMN = 1
148200071204     C                   EVAL      vinmsg = %trimr(vinmsg)
148300081027     C                             + ' ' + 'VABNSP VATNSP VABRMN'
148400081027     C***                          + ' ' + 'VABRMN'
148500071204     C                   ENDIF
148600071204     C                   ENDIF
148700080915     C*** PKB
148800080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
148900080917     C                   IF        %trim(SkSplitCSV(i)) <> *zeros
149000080915     C                   EVAL      PiStr=SkSplitCSV(i)
149100080915     C                   EXSR      CHKNUM
149200080915     C                   IF        PiInt=*on
149300080915     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg.
149400080915     C                   Z-ADD     PiVal         VABPKB
149500080915     C                   ELSE
149600080915     C                   SETON                                        32
149700080915     C                   EVAL      vinmsg = %trimr(vinmsg)
149800080915     C                             + ' ' + 'VABPKB'
149900080917     C                   ENDIF
150000080915     C                   ENDIF
150100080915     C                   ENDIF
150200080915     C*** VLB
150300080915     C***                IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
150400080915     C***                EVAL      PiStr=SkSplitCSV(i)
150500080915     C***                EXSR      CHKNUM
150600080915     C***                IF        PiInt=*on
150700080915     C***                EVAL      PiVal = PiVal / 1000000000                   * da cm3 a m3
150800080915     C***                Z-ADD     PiVal         VABVLB
150900080915     C***                ELSE
151000080915     C***                SETON                                        32
151100080915     C***                EVAL      vinmsg = %trimr(vinmsg)
151200080915     C***                          + ' ' + 'VABVLB'
151300080915     C***                ENDIF
151400080915     C***                ENDIF
151500080915     C*** W_VATNOT_E
151600080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
151700080915     C                   EVAL      wVATNOT_E = %trim(%subst(SkSplitCSV(i):5:35))
151800080716     C                   ENDIF
151900070925     C*
152000071029     C***  <===  -----------------------------------------
152100040510     C*
152200030715     C                   ENDIF
152300030715     C                   EVAL      i = i + 1
152400030715     C                   ENDDO
152500070925     C*
152600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
152700080125     C*
152800070925     C*
152900071029     C***  <===  -----------------------------------------
153000070925     C*
153100070925     C* Considerazioni finali su CBO/CAS
153200080125     C                   IF        wFlgCAS = '1'
153300070925     C                   IF        VABCBO = '1'
153400070925     C                   EVAL      VABCBO = '4'
153500070925     C                   ENDIF
153600070925     C                   IF        VABCBO = '2'
153700070925     C                   EVAL      VABCBO = '6'
153800070925     C                   ENDIF
153900070925     C                   ENDIF
154000070925     C***  <===  ----------------------------
154100040510     C*
154200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154300040510     C                   IF        VABNSP = *zeros
154400040510     C                   SETON                                        31
154500040510     C                   ENDIF
154600040715     C*
154700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154800040715     C                   IF        VABLNP = *zeros
154900040715     C                   SETON                                        31
155000040715     C                   ENDIF
155100040510     C*
155200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155300040510     C                   IF        VABRMN = *zeros
155400040510     C                   Z-ADD     VABNSP        VABRMN
155500040510     C                   ENDIF
155600040510     C*
155700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155800040510     C                   IF        VABRMA = *blanks
155900040714     C                   MOVEL     VABRMN        VABRMA
156000040510     C                   ENDIF
156100040128     C*
156200040510     C* Se provincia nn valorizzata la reperisco
156300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156400040510     C                   IF        VABPRD = *blanks
156500040128     C                   CLEAR                   TISI95DS
156600040128     C                   EVAL      I95TCN = '3'
156700040128     C                   Z-ADD     datcor        I95DAT
156800040128     C                   EVAL      I95NAR = VABNZD
156900040128     C                   EVAL      I95CAP = VABCAD
157000040128     C                   EVAL      I95LOC = VABLOD
157100040128     C                   CALL      'TISI95R'
157200040128     C                   PARM                    TISI95DS
157300040128     C                   EVAL      VABPRD = O95PRV
157400040510     C                   ENDIF
157500020204     C*
157600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157700020204     C                   EXSR      CHKIMPDIV
157800030715     C                   ENDIF
157900020204     C*
158000010607     C                   ENDSR
158100010607     C*----------------------------------------------------*
158200010601
158300020204
158400020204     C*----------------------------------------------------*
158500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
158600020204     C*----------------------------------------------------*
158700020204     C     CHKIMPDIV     BEGSR
158800020204     C*
158900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159000020204     C                   Z-ADD     *zeros        wrkDec            9 9
159100020204     C*
159200020204     C* Come prima cosa effettuo considerazioni sulla divisa
159300020204     C                   IF        vabIAS > *zeros
159400020204     C                   IF        vabVAS <> 'EUR'
159500020204     C                   EVAL      vabVAS =  'ITL'
159600020204     C                   ENDIF
159700020204     C                   ENDIF
159800020204     C*
159900020204     C                   IF        vabCAS > *zeros
160000020204     C                   IF        vabVCA <> 'EUR'
160100020204     C                   EVAL      vabVCA =  'ITL'
160200020204     C                   ENDIF
160300020204     C                   ENDIF
160400020204     C*
160500020204     C                   IF        vabVMD > *zeros
160600020204     C                   IF        vabVAD <> 'EUR'
160700020204     C                   EVAL      vabVAD =  'ITL'
160800020204     C                   ENDIF
160900020204     C                   ENDIF
161000020204     C*
161100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161200020204     C                   Z-ADD     vabIAS        wrkDec
161300020204     C                   IF        wrkDec > *zeros
161400020204     C                   IF        vabVAS = 'ITL'
161500020204     C                   EVAL      vabIAS = *zeros
161600020204     C                   ENDIF
161700020204     C                   ENDIF
161800020204     C*
161900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162000020204     C                   Z-ADD     vabCAS        wrkDec
162100020204     C                   IF        wrkDec > *zeros
162200020204     C                   IF        vabVCA = 'ITL'
162300020204     C                   EVAL      vabCAS = *zeros
162400020204     C                   ENDIF
162500020204     C                   ENDIF
162600020204     C*
162700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162800020204     C                   Z-ADD     vabVMD        wrkDec
162900020204     C                   IF        wrkDec > *zeros
163000020204     C                   IF        vabVAD = 'ITL'
163100020204     C                   EVAL      vabVMD = *zeros
163200020204     C                   ENDIF
163300020204     C                   ENDIF
163400020204     C*
163500020204     C                   ENDSR
163600020204     C***
163700080617
163800080617
163900080617
164000080617     C*----------------------------------------------------*
164100080617     C*  CONTROLLO NUMERICITA' CAMPI
164200080617     C*----------------------------------------------------*
164300080617     C     CHKNUM        BEGSR
164400080617     C*
164500080617     C                   IF        PiDecChr = *blanks
164600080617     C                   EVAL      PiDecChr = CharNUM
164700080617     C                   ENDIF
164800080617     C*
164900080617     C                   callp     UBISNUM_Check(PiStr
165000080617     C                                          :PiDecChr
165100080617     C                                          :PiVal
165200080617     C                                          :PiNum
165300080617     C                                          :PiInt)
165400080617     C*
165500080617     C                   ENDSR
165600080617     C***
165700010330
165800010601
165900010601
166000010601
166100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166200010601     C     repfil        BEGSR
166300010601     C*
166400010601     C                   if        invfil = *zeros and
166500010601     C                             depfil > *zeros and
166600010629     C                             (vinflg = *blanks or
166700010629     C                              vinflg = *zeros)
166800010601     C
166900010601     C                   eval      invfil = depfil
167000010601     C                   endif
167100010601     C*
167200010601     C                   if        depfil <> invfil and
167300010601     C                             invfil > *zeros
167400010601     C                   eval      flgMulti = '1'
167500010601     C                   if        vinflg = *blanks
167600010601     C                   add       1             cntNonEl
167700010601     C                   endif
167800010601     C                   endif
167900010601     C*
168000010601     C                   if        vinflg = '2'
168100010601     C                   eval      flgStato = '2'
168200010601     C                   endif
168300010601     C*
168400010601     C                   ENDSR
168500010601     C***
168600010601
168700010601
168800010601
168900010330
169000040119      /TITLE Invio dei dati al punto operativo.
169100040119     C     invio         BEGSR
169200040119     C*
169300081027     C* 1° invio FIVAT
169400040119     C                   reset                   dscmz
169500040119     C                   move      vlrpoi        cmzdst
169600081027     C                   eval      cmzfld = 'FIVATWWR'
169700040119     C                   eval      cmzmbd = vlrhdl
169800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169900040119     C***                if        prmfir = *blanks
170000081027     C                   eval      cmzfla = 'FIVAT00F'
170100081027     C                   eval      cmzmba = 'FIVAT00F'
170200040119     C***                else
170300040119     C***                eval      cmzfla = prmfir
170400040119     C***                eval      cmzmba = prmfir
170500040119     C***                endif
170600040119     C                   eval      cmznrr = *zeros
170700040119     C                   move      §ctrokvt      cmznrr
170800040119     C                   eval      cmzlba = vlrfl1
170900040119     C                   call(e)   'TIS711C'
171000040119     C                   parm                    dscmz
171100040119     C                   parm      *blanks       esito
171200040119     C                   if        %error
171300040119     C                             or cmzerr = '1'
171400040119     C                             or esito  = '1'
171500040119     C                   eval      wrkesito = '3'
171600040119     C                   else
171700040119     C*
171800081027     C* 2° invio FIVAB
171900040119     C                   reset                   dscmz
172000040119     C                   move      vlrpoi        cmzdst
172100040119     C                   eval      cmzfld = vlrfou
172200040119     C                   eval      cmzmbd = vlrhdl
172300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172400040119     C***                if        prmfir = *blanks
172500081027     C                   eval      cmzfla = 'FIVAB00F'
172600081027     C                   eval      cmzmba = 'FIVAB00F'
172700040119     C***                else
172800040119     C***                eval      cmzfla = prmfir
172900040119     C***                eval      cmzmba = prmfir
173000040119     C***                endif
173100040119     C                   eval      cmznrr = *zeros
173200040119     C                   move      §ctrokvb      cmznrr
173300040119     C                   eval      cmzlba = vlrfl1
173400040119     C                   call(e)   'TIS711C'
173500040119     C                   parm                    dscmz
173600040119     C                   parm      *blanks       esito
173700040119     C                   if        %error
173800040119     C                             or cmzerr = '1'
173900040119     C                             or esito  = '1'
174000040119     C                   eval      wrkesito = '3'
174100040119     C                   endif
174200040119     C                   endif
174300040119     C*
174400040119     C                   ENDSR
174500040119     C***
174600010601
174700010601
174800010601
174900010601
175000010601      /TITLE Invio dei dati al punto operativo.
175100010601     C     opeini        BEGSR
175200010601     C*
175300010601     C* Inizializzo flag e contatori operativi
175400010601     C                   movel     '0'           flgGiro           1
175500010601     C                   movel     '0'           flgMulti          1
175600010601     C                   movel     '1'           flgStato          1
175700010615     C                   movel     '0'           flgOk             1
175800010601     C                   z-add     *zeros        cntNonEl         10 0
175900010601     C                   z-add     *zeros        depfil            3 0
176000010601     C                   z-add     *zeros        invfil            3 0
176100010601     C*
176200010601     C                   ENDSR
176300010601     C***
176400070326
176500070326
176600070326
176700070326
176800080915     C     *pssr         BEGSR
176900070329     C*
177000080915     C                   if        %open(tivin00r)
177100080915     C                   close     tivin00r
177200080915     C                   endif
177300081027     C                   if        %open(fivabwwr)
177400081027     C                   close     fivabwwr
177500080915     C                   endif
177600081027     C                   if        %open(fivatwwr)
177700081027     C                   close     fivatwwr
177800080915     C                   endif
177900081027     C                   if        %open(fivab02l)
178000081027     C                   close     fivab02l
178100081027     C                   endif
178200081027     C                   if        %open(fivat01l)
178300081027     C                   close     fivat01l
178400081027     C                   endif
178500070326     C*
178600070326     C* Effettuo la chiamata al CLLE preposto
178700081027     C                   call(e)   'TITVVTC'
178800080915     C                   parm                    parccm
178900080915     C                   parm                    parmbr
179000080915     C                   parm      '2'           paropz
179100070326     C*
179200080915     C                   eval      wrkesito = '2'
179300070404     C*
179400080915     C                   seton                                        LR
179500070326     C*
179600080915     C                   ENDSR     '*CANCL'
179700070326     C***
179800070326
179900070326
180000010330
180100010330
180200000613     C     *inzsr        BEGSR
180300990910     C*
180400990910     C     *entry        plist
180500990920     C                   parm                    tivlrds
180600990921     C                   parm      wrkesito      esito
180700000724     C                   parm                    prmlit
180800000710     C                   parm                    prmfir
180900010330     C*
181000010330     C* CALCOLA LA DATA CORRENTE
181100010330     C                   time                    wn14             14 0
181200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
181300080915     C                   movel     wn14          oracor            6 0          *ORA
181400010330     C                   z-add     wn8           g08dat
181500010330     C                   z-add     *zeros        g08inv
181600010330     C                   movel     '0'           g08err
181700010330     C                   call      'XSRDA8'
181800010330     C                   parm                    wlbda8
181900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
182000081027     C*
182100081027     C* FIVAB02L - completa
182200081027     C     KEYvab02_C    klist
182300081027     C                   kfld                    vabFGS
182400081027     C                   kfld                    vabCCM
182500081027     C                   kfld                    vabRMN
182600081027     C*
182700081027     C* FIVAT01L - completa
182800081027     C     KEYvat01_C    klist
182900081027     C                   kfld                    VATFGS
183000081027     C                   kfld                    VATCCM
183100081027     C                   kfld                    VATAAS
183200081027     C                   kfld                    VATLNP
183300081027     C                   kfld                    VATNRS
183400081027     C                   kfld                    VATNSP
183500081027     C                   kfld                    VATTRC
183600081027     C                   kfld                    VATNOT
183700000613     C*
183800000613     C                   ENDSR
183900000613     C***
184000990908
184100080915     Otitv1i8p  E            riepilogo         2
184200990915     O                                              'Upload via Internet'
184300990915     O                                           +1 'Traduzione TIVIN00R -'
184400081027     O                                           +1 'FIVABWWR'
184500990915     O                                           +1 'Testate bolle'
184600990915     O          E            riepilogo   2
184700990915     O                       wrkdata
184800990915     O                       wrkora              +1
184900990915     O                       procname            +1
185000990915     O          E            riepilogo   2
185100990915     O                                              'Cliente..................:'
185200010605     O                       VABCCM        z     +1
185300990915     O          E            riepilogo   2
185400990920     O                                              'Riferimento Strategi.....:'
185500990920     O                       vlrhdl              +1
185600990915     O          E            riepilogo   2
185700990915     O                                              'Giusti...................:'
185800040119     O                       §CTROKVB      2   +  1
185900990915     O          E            riepilogo   2
186000990915     O                                              'Sbagliati e corretti.....:'
186100971022     O                       §CTRMO        2   +  1
186200990915     O          E            riepilogo   2
186300990915     O                                              'Sbagliati e scartati.....:'
186400971022     O                       §CTRNO        2   +  1
186500000613
186600080915     Otitv1i8ps E            testdett          2
186700000613     O                                              'Upload via Internet'
186800000613     O                                           +1 'Traduzione TIVIN00R -'
186900081027     O                                           +1 'FIVABWWR'
187000000613     O                                           +1 'Testate bolle'
187100000616     O          E            testdett    3
187200000613     O                                           +2 'N° rec'
187300000613     O                                           +3 'Anteprima contenuto'
187400000616     O          E            rigadett    2
187500000613     O                       rrnum               +2
187600000621     O                       recko               +3
187700000616     O          E            findett     2
187800000613     O                       wrkdata
187900000613     O                       wrkora              +1
188000000613     O                       procname            +1
188100000616     O          E            findett     2
188200000613     O                                              'Cliente..................:'
188300010605     O                       VABCCM        z     +1
188400000616     O          E            findett     2
188500000613     O                                              'Riferimento Strategi.....:'
188600000613     O                       vlrhdl              +1
188700000616     O          E            findett     2
188800000613     O                                              'Giusti...................:'
188900040119     O                       §CTROKVB      2   +  1
189000000616     O          E            findett     2
189100000613     O                                              'Sbagliati e corretti.....:'
189200000613     O                       §CTRMO        2   +  1
189300000616     O          E            findett     2
189400000613     O                                              'Sbagliati e scartati.....:'
189500000613     O                       §CTRNO        2   +  1
189600000613** CMD - COMANDI CL
189700080915OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
189800080915OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
189900080915DLTOVR FILE(TITV1I8P TITV1I8PS) LVL(*)
190000010606
190100000613
190200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
190300081027VABNOT;W_VATNOT_E;W_VABRMA;FILL001;W_VABPKB;W_VABVLB;
190400081027FILL003;FILL004;VABRSD;VABLOD;VABIND;VABCAD;VABPRD
190500080715
