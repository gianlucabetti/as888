000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800081016     Ftitv1j5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100081016     Ftitv1j5ps O    f  198        PRINTER usropn
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
008300080617     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
008400080617     D*------------------
008500080617     D PiStr           S            100A   INZ
008600080617     D PiDecChr        S              1A   INZ
008700080617     D PiVal           S             63S30 INZ
008800080617     D PiInt           S               N   INZ
008900080617     D PiNum           S               N   INZ
009000080617
009100080617     D*------------------
009200080617     D* LINKING A DEFINIZIONI ESTERNE
009300080617     D*------------------
009400080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009500010330
009600010330
009700010330
009800990915     C                   time                    wrkdata
009900990915     C                   time                    wrkora
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200081014     C                   reset                   wrkesito
010300010601     C*
010400010601     C                   exsr      opeini
010500010605     C                   exsr      rwvab
010600081014     C*
010700081014     C                   exsr      endela
010800010601     C*
010900010601     C                   seton                                        lr
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011500010601     C*--------------------------------------------------------
011600010601     C     PREELA        BEGSR
011700010601     C*
011800010601     C* SE OCCORRE SPEDIRE IN FILIALE
011900010601     C                   if        invfil <> *zeros and
012000010601     C                             flgGiro = '0'
012100010601     C*
012200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012300010601     C                   eval      flgGiro = '1'
012400000724     C*
012500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012700010601     C     invfil        chain     azorg01l
012800000724     C                   if        %found
012900000724     C                   movel(p)  CMD(1)        depcmd
013000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013100000724     C*
013200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000724     C                   PARM                    depcmd
013600000724     C                   PARM                    LENGH
013700000724     C*
013800000724     C                   endif
013900000616     C*
014000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014100000616     C                   movel(p)  CMD(2)        depcmd
014200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014300000616     C*
014400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014500011113     C                   Z-ADD     150           LENGH            15 5
014600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700000616     C                   PARM                    depcmd
014800000616     C                   PARM                    LENGH
014900000613     C*
015000081016     C                   if        not %open(titv1j5ps)
015100081016     C                   open      titv1j5ps
015200070329     C                   except    testdett
015300000613     C                   endif
015400010601     C*
015500010601     C                   endif
015600010601     C*
015700010601     C                   ENDSR
015800010601     C***
015900010601
016000010601
016100010601
016200010601
016300010601     C*--------------------------------------------------------
016400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016500010601     C*--------------------------------------------------------
016600010601     C     ENDELA        BEGSR
016700081014     C*
016800081014     C                   if        %open(tivin00r)
016900081014     C                   close     tivin00r
017000081014     C                   endif
017100081014     C                   if        %open(fivabwwr)
017200081014     C                   close     fivabwwr
017300081014     C                   endif
017400081014     C                   if        %open(fivatwwr)
017500081014     C                   close     fivatwwr
017600081014     C                   endif
017700040119     C*
017800040119     C* Effettuo la chiamata al CLLE preposto
017900040119     C                   call(e)   'TITVVTC'
018000040510     C                   parm                    parccm
018100040119     C                   parm                    parmbr
018200040119     C                   parm      '2'           paropz
018300050201     C*
018400050201     C* Effettuo lancio TISI95 solo x chiusura
018500050201     C                   CLEAR                   TISI95DS
018600050201     C                   EVAL      I95TLA = 'C'
018700050201     C                   CALL      'TISI95R'
018800050201     C                   PARM                    TISI95DS
018900000613     C*
019000081016     C                   if        %open(titv1j5ps)
019100000616     C                   except    findett
019200081016     C                   close     titv1j5ps
019300000613     C                   endif
019400000616     C*
019500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019600011113     C                   Z-ADD     150           LENGH            15 5
019700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019800000616     C                   PARM                    CMD(3)
019900000616     C                   PARM                    LENGH
020000000616     C*
020100010601     C                   ENDSR
020200010601     C***
020300010601
020400010601
020500010601
020600000613
020700000613     C*--------------------------------------------------------
020800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020900000613     C*--------------------------------------------------------
021000000621     C     STPR          BEGSR
021100000613     C*
021200081016     C                   if        not %open(titv1j5p)
021300081016     C   50              open      titv1j5p
021400990915     C                   endif
021500990915     C*
021600030715     C   50              except    riepilogo
021700990915     C*
021800081016     C                   if        %open(titv1j5p)
021900081016     C   50              close     titv1j5p
022000990914     C                   endif
022100000613     C*
022200000613     C                   ENDSR
022300000613     C***
022400000613
022500010601
022600010601
022700010330     C*--------------------------------------------------------
022800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022900010330     C*--------------------------------------------------------
023000010605     C     RWVAB         BEGSR
023100010330     C*
023200010330     C                   if        not %open(tivin00r)
023300010330     C                   open      tivin00r
023400010330     C                   endif
023500021025     C                   if        not %open(fivabwwr)
023600021025     C                   open      fivabwwr
023700010330     C                   endif
023800040119     C*
023900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024000040119     C                   exsr      prevat
024100040119     C*
024200040119     C                   if        chkcall = '0'
024300040119     C*
024400040119     C                   if        not %open(fivatwwr)
024500040119     C                   open      fivatwwr
024600040119     C                   endif
024700010330     C*
024800040119     C                   clear                   §CTROKVB          7 0
024900040119     C                   clear                   §CTROKVT          7 0
025000040119     C                   clear                   §CTRMO            7 0
025100040119     C                   clear                   §CTRNO            7 0
025200070823     C*
025300070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025400070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025500081016     C***                exsr      inzvar
025600081016     C***                exsr      impvab
025700010330     C*
025800030822     C                   do        *HIVAL
025900010330     C*
026000030822     C                   read      tivin00r                               70
026100010618     C*
026200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026300010618     C                   if        vinflg = '1'
026400010618     C                   eval      flgOk = '1'
026500010618     C                   endif
026600040510     C*
026700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026800040510     C                   z-add     *zeros        wLenVINDTA        4 0
026900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027300040510     C                   z-add     1             wFlgVINDTA
027400040510     C                   leave
027500040510     C                   endif
027600040510     C                   enddo
027700010618     C*
027800040510     C                   if        vindta > *blanks AND
027900040510     C                             wFlgVINDTA = 1
028000010330     C                   add       1             rrnum
028100010330     C*
028200010601     C                   if        *in70 = *off and
028300010330     C                             (vinflg = *blanks
028400010330     C                              or vinflg = '0'
028500010330     C                              or vinflg = '2')
028600010330     C*
028700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028800010711     C                   if        vinflg = *blanks or vinflg = '0'
028900010711     C                   clear                   vinmsg
029000010711     C                   endif
029100010601     C*
029200070925     C* Aggiungo un byte a blanks all'inizio d ogni record
029300070925     C                   eval      vindta  = ' ' + %trim(vindta)
029400070925     C*
029500010330     C                   exsr      inzvar
029600010605     C                   exsr      impvab
029700010601     C*
029800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029900010605     C                   eval      depfil = VABLNP
030000010601     C                   exsr      repfil
030100010601     C                   if        depfil = invfil
030200021025     C                   if        vlrpoi = 999
030300030822     C                   move(P)   invfil        VABFGS
030400021025     C                   else
030500030822     C                   move(P)   vlrpoi        VABFGS
030600021025     C                   endif
030700010601     C*
030800010601     C                   exsr      PREELA
030900010601     C*
031000010604     C* Ebbene...
031100010604     C*
031200040119     C  N31              add       1             §CTROKVB
031300040119     C   32              add       1             §CTRMO
031400040119     C   31              add       1             §CTRNO
031500030822     C                   if        wGiro = 2
031600040510     C* Scrivo sempre il VAT
031700040510     C  N31              exsr      wrivat
031800040510     C* Scrivo il VAB solo a rottura d spedizione
031900040119     C                   if        depspe =  *zeros OR
032000040119     C                             depspe <> vabNSP
032100040119     C  N31              exsr      wrivab
032200040119     C  N31              eval      depspe = vabNSP
032300040119     C                   endif
032400040119     C                   endif
032500010604     C*
032600010604     C                   if        *in31 = *off and
032700010604     C                             *in32 = *off
032800010604     C                   eval      vinflg = '1'
032900010604     C                   else
033000010604     C                   eval      recko = vindta
033100010604     C                   except    rigadett
033200010604     C                   eval      vinflg = '2'
033300010604     C                   endif
033400010604     C                   endif
033500010601     C*
033600010604     C                   endif
033700010604     C*
033800010330     C                   else
033900010330     C                   eval      vinflg = '1'
034000010330     C                   endif
034100010601     C*
034200010601     C  N70              update    tivin000
034300010330     C*
034400030822     C  N70              enddo
034500040119     C*
034600040119     C                   endif                                                  (endif - chkcall)
034700010601     C*
034800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034900010601     C                   if        cntNonEl = *zeros or
035000010601     C                             flgMulti = '0'
035100010330     C* Se non ci sono record con errori ...
035200010601     C                   if        §ctrno = 0 and
035300010604     C                             §ctrmo = 0 and
035400010601     C                             flgStato <> '2'
035500010330     C* ... restituisco esito OK.
035600010330     C                   eval      wrkesito = '0'
035700010330     C                   else
035800040119     C                   if        §ctrokvb > 0
035900010330     C                   eval      wrkesito = '1'
036000010330     C                   else
036100010615     C                   if        flgOk = '0'
036200010615     C                   eval      wrkesito = '2'
036300010615     C                   else
036400010615     C                   eval      wrkesito = '6'
036500010615     C                   endif
036600010330     C                   endif
036700010330     C                   endif
036800010601     C                   else
036900010601     C                   eval      wrkesito = '9'
037000010601     C                   endif
037100010330     C*
037200010330     C                   if        %open(tivin00r)
037300010330     C                   close     tivin00r
037400010330     C                   endif
037500021025     C                   if        %open(fivabwwr)
037600021025     C                   close     fivabwwr
037700010330     C                   endif
037800040512     C                   if        %open(fivatwwr)
037900040512     C                   close     fivatwwr
038000040512     C                   endif
038100010601     C*
038200010601     C                   if        vlrpoi <> 999
038300010601     C                   eval      invfil = vlrpoi
038400010601     C                   endif
038500010330     C*
038600040119     C                   if        §ctrokvb > 0
038700010601     C                             and invfil > *zeros
038800010330     C                   exsr      invio
038900010330     C                   endif
039000010601     C*
039100010618     C                   if        flgGiro = '1'
039200081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039300010618     C                   endif
039400010330     C*
039500010330     C                   ENDSR
039600010330     C***
039700040119
039800040119
039900040119
040000040119     C*----------------------------------------------------*
040100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040200040119     C*----------------------------------------------------*
040300040510     C     WRIVAB        BEGSR
040400040119     C*
040500060331     C* Gestisco l'eventuale rottura x numero spedizione
040600070823     C                   if        VABNSP <> depspe
040700070301     C                   if        wVATNOT_E <> *blanks
040800080125     C                   if        VABCTM = *blanks
040900080125     C                   eval      VABCTM = '7Q'
041000080125     C                   endif
041100070301     C                   endif
041200040119     C                   write     fivab000                                     => scarico il VAB
041300060331     C                   endif
041400040119     C*
041500040119     C                   ENDSR
041600040119
041700040119
041800040119
041900040119     C*----------------------------------------------------*
042000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042100040119     C*----------------------------------------------------*
042200040510     C     WRIVAT        BEGSR
042300060512     C*
042400060512     C* Inizializzo i campi chiave
042500060512     C                   eval      VATFGS = VABFGS
042600060512     C                   eval      VATAAS = VABAAS
042700060512     C                   eval      VATCCM = VABCCM
042800070823     C                   eval      VATNRS = VABNRS
042900060512     C                   eval      VATNSP = VABNSP
043000060512     C                   eval      VATLNP = VABLNP
043100060331     C*
043200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043300060331     C                   if        wVATNOT_A <> *blanks
043400060331     C                   eval      VATNOT = wVATNOT_A
043500060331     C                   eval      VATTRC = 'A'
043600060331     C                   write     FIVAT000
043700060331     C                   add       1             §CTROKVT
043800060331     C                   endif
043900060331     C*
044000060331     C                   if        wVATNOT_B <> *blanks
044100060331     C                   eval      VATNOT = wVATNOT_B
044200060331     C                   eval      VATTRC = 'B'
044300060331     C                   write     FIVAT000
044400060331     C                   add       1             §CTROKVT
044500060331     C                   endif
044600060331     C*
044700080625     C                   if        wVATNOT_E <> *blanks
044800060331     C                   eval      VATNOT = wVATNOT_E
044900060331     C                   eval      VATTRC = 'E'
045000080716     C                   write     FIVAT000
045100080716     C                   add       1             §CTROKVT
045200080625     C                   endif
045300040119     C*
045400040119     C                   ENDSR
045500040119
045600040119
045700040119
045800040119     C*----------------------------------------------------*
045900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046000040119     C*----------------------------------------------------*
046100040119     C     PREVAT        BEGSR
046200040119     C*
046300040119     C* Compongo il nome del membro da dare al FIVATWWR
046400040119     C                   eval      parmbr = vlrhdl
046500040119     C                   movel     'M'           parmbr
046600040510     C                   eval      parccm = vlrksc
046700040119     C                   eval      paropz = '1'
046800040119     C* Effettuo la chiamata al CLLE preposto
046900040119     C                   call(e)   'TITVVTC'
047000040119     C                   parm                    parccm
047100040119     C                   parm                    parmbr
047200040119     C                   parm                    paropz
047300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047400040119     C                   if        %error
047500040119     C                   movel     '1'           chkcall
047600040119     C                   else
047700040119     C                   movel     '0'           chkcall
047800040119     C                   endif
047900040119     C*
048000040119     C                   ENDSR
048100010601
048200010601
048300010601
048400010330     C*----------------------------------------------------*
048500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048600010330     C*----------------------------------------------------*
048700040119     C     INZVAR        BEGSR
048800010330     C*
048900030822     C* Inizializzo variabili di wrk
049000020204     C                   Z-ADD     *zeros        Num5_0            5 0
049100060331     C                   MOVEL     *blanks       wVATNOT_A
049200060331     C                   MOVEL     *blanks       wVATNOT_B
049300060331     C                   MOVEL     *blanks       wVATNOT_E
049400080617     C                   EVAL      wFlgCAS = *blanks
049500030822     C*
049600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049700030822     C                   CLEAR                   FIVAB000
049800040119     C                   CLEAR                   FIVAT000
049900030822     C                   CLEAR                   SkSplitCSV
050000030822     C                   EVAL      i = 1
050100030822     C                   EVAL      posDa = *zeros
050200030822     C                   EVAL      posA  = *zeros
050300030822     C*
050400030822     C* Reimposto i valori di default
050500030822     C                   EXSR      DEFCAM
050600030822     C*
050700010330     C                   ENDSR
050800010330     C*----------------------------------------------------*
050900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051000010330     C*----------------------------------------------------*
051100010330     C     DEFCAM        BEGSR
051200030822     C*
051300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051400030715     C* e delimitatore testo.
051500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051800030822     C*
051900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
052100030715     C                   SETON                                        50
052200030715     C                   ELSE
052300030715     C                   SETOFF                                       50
052400030715     C                   ENDIF
052500030822     C*
052600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052700030822     C                   EVAL      posDaDft = 1
052800030822     C                   EVAL      posADft  = 0
052900030822     C                   EVAL      wGiroDft = 0
053000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053100030822     C                             posDaDft > 0
053200030822     C*
053300030822     C* Gestisco il 1° giro
053400030822     C                   IF        wGiroDft = 0
053500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053700030822     C* Incremento il contatore dei "giri"
053800030822     C                   EVAL      wGiroDft = 1
053900030822     C                   ELSE
054000030822     C                   EVAL      posDaDft = posADft
054100030822     C                   ENDIF
054200030822     C* Eseguo lo scan x trovare la fine del campo corrente
054300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054400030822     C*
054500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054700030822     C                   IF        posDaDft > 0 AND
054800030822     C                             posADft  > 0
054900030822     C* NCL
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'NCL'
055400030822     C                   EVAL      PiStr=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   EXSR      CHKNUM
055800030822     C                   IF        PiInt=*on
055900030822     C                   Z-ADD     PiVal         VABNCL
056000030822     C                   ENDIF
056100030822     C                   ENDIF
056200030822     C* CCM
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'CCM'
056700030822     C                   EVAL      PiStr=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   EXSR      CHKNUM
057100030822     C                   IF        PiInt=*on
057200030822     C                   Z-ADD     PiVal         VABCCM
057300030822     C                   ENDIF
057400030822     C                   ENDIF
057500030822     C* LNP
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'LNP'
058000030822     C                   EVAL      PiStr=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   EXSR      CHKNUM
058400030822     C                   IF        PiInt=*on
058500030822     C                   Z-ADD     PiVal         VABLNP
058600030822     C                   ENDIF
058700030822     C                   ENDIF
058800030822     C* NRS
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'NRS'
059300030822     C                   EVAL      PiStr=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   EXSR      CHKNUM
059700030822     C                   IF        PiInt=*on
059800030822     C                   Z-ADD     PiVal         VABNRS
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* CTR
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'CTR'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiInt=*on
061100030822     C                   Z-ADD     PiVal         VABCTR
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* PKB
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'PKB'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiNum=*on
062400030822     C                   Z-ADD     PiVal         VABPKB
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* VLB
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'VLB'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiNum=*on
063700030822     C                   Z-ADD     PiVal         VABVLB
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* QFT
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'QFT'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiNum=*on
065000030822     C                   Z-ADD     PiVal         VABQFT
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* CBO
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'CBO'
065800030822     C                   EVAL      VABCBO=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* TSP
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'TSP'
066700030822     C                   EVAL      VABTSP=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100030822     C* VAS
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'VAS'
067600030822     C                   EVAL      VABVAS=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* VCA
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'VCA'
068500030822     C                   EVAL      VABVCA=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900070925     C* TIC
069000070925     C                   IF        %subst(
069100070925     C                             %subst(vlrppt:posDaDft+1:
069200070925     C                             posADft-posDaDft-1):1:3)
069300070925     C                             = 'TIC'
069400070925     C                   EVAL      VABTIC=%trim(%subst(
069500070925     C                             %subst(vlrppt:posDaDft+1:
069600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700070925     C                   ENDIF
069800030822     C* GCA
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'GCA'
070300030822     C                   EVAL      VABGCA=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* CTM
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'CTM'
071200030822     C                   EVAL      VABCTM=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* FFD
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'FFD'
072100030822     C                   EVAL      VABFFD=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* VAD
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'VAD'
073000030822     C                   EVAL      VABVAD=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* GMA
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'GMA'
073900030822     C                   EVAL      VABGMA=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* GGA
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'GGA'
074800030822     C                   EVAL      VABGGA=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* GVA
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'GVA'
075700030822     C                   EVAL      VABGVA=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* TC1
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'TC1'
076600030822     C                   EVAL      VABTC1=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* TC2
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'TC2'
077500030822     C                   EVAL      VABTC2=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900040714     C* VATTRC
078000040714     C                   IF        %subst(
078100040714     C                             %subst(vlrppt:posDaDft+1:
078200040714     C                             posADft-posDaDft-1):1:3)
078300040714     C                             = 'TRC'
078400040714     C                   EVAL      VATTRC=%trim(%subst(
078500040714     C                             %subst(vlrppt:posDaDft+1:
078600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700040714     C                   ENDIF
078800030822     C* ...
078900030822     C                   ENDIF
079000030822     C                   ENDDO
079100020204     C*
079200010330     C                   ENDSR
079300010607     C*----------------------------------------------------*
079400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079500010607     C*----------------------------------------------------*
079600010607     C     IMPVAB        BEGSR
079700010607     C*
079800010607     C                   SETOFF                                       3132
079900030515     C*
080000010607     C                   MOVEL     datcor        VABAAS
080100010607     C                   MOVE      datcor        VABMGS
080200010607     C*
080300030715     C**********
080400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080500030715     C**********
080600030715     C                   IF        wGiro = *zeros
080700080716     C*
080800080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080900080716     C                   EVAL      depvinDTA = vinDTA
081000040113     C*
081100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081300040510     C                             %trim(intcol(3))<>*blanks
081400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081500040113     C                                      %trim(intcol(3))
081600070823     C     ';':CharCSV   XLATE     vindta        vindta
081700040510     C                   ENDIF
081800080626     C*
081900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082000031201     C     minu:maiu     XLATE     vindta        vindta
082100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082200030715     C                   DOW       posDa <= %len(%trim(vindta))
082300030715     C*
082400030715     C* Gestisco il 1° campo
082500030715     C                   IF        i = 1
082600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082700030715     C                   EVAL      posDa = 1
082800030715     C* Eseguo lo scan x trovare la fine del primo campo
082900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083000030715     C* A questo punto "estraggo" il campo corrente
083100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083200030715     C                                                    (posA-posDa))
083300030715     C* X i campi successivi al 1°
083400030715     C                   ELSE
083500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083600030715     C                   EVAL      posDa = posA + 1
083700030715     C* Eseguo lo scan x trovare la fine del campo corrente
083800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083900030715     C* Gestisco l'ultimo campo
084000030715     C                   IF        posA = *zeros
084100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   LEAVE
084600030715     C                   ELSE
084700030715     C* A questo punto "estraggo" il campo corrente
084800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084900030715     C                                                    (posA-posDa))
085000030715     C                   ENDIF
085100030715     C                   ENDIF
085200030715     C* Incremento il contatore d campo
085300030715     C                   EVAL      i = i +1
085400030715     C                   ENDDO
085500030715     C*
085600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085700030715     C* delimitazione testo
085800030715     C                   EVAL      i = 1
085900030715     C                   DOW       i < %elem(SkSplitFLD)
086000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086200030715     C                   EVAL      i = i + 1
086300030715     C                   ENDDO
086400030715     C                   EVAL      wGiro = 1
086500080716     C* Ripristino la 1° riga originale
086600080716     C                   EVAL      vinDTA = depvinDTA
086700030715     C                   ELSE
086800030715     C                   EVAL      wGiro = 2
086900031204     C**********
087000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087100031204     C* d separatore campo
087200031204     C**********
087300031204     C                   EVAL      vindta = %trim(vindta)
087400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087500031204     C                   EVAL      vindta = %subst(vindta:2)
087600031204     C                   ENDDO
087700031204     C*
087800031204     C                   Z-ADD     *zeros        lunghInput        4 0
087900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088300031204     C                   ENDDO
088400030715     C**********
088500030715     C* Effettuo lo split del campo dati d input
088600030715     C**********
088700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088800030715     C                   DOW       posDa <= %len(%trim(vindta))
088900030715     C*
089000030715     C* Gestisco il 1° campo
089100030715     C                   IF        i = 1
089200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089300030715     C                   EVAL      posDa = 1
089400030715     C* Eseguo lo scan x trovare la fine del primo campo
089500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C* X i campi successivi al 1°
090000030715     C                   ELSE
090100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090200030715     C                   EVAL      posDa = posA + 1
090300031204     C* Verifico che nn vi sia il campo nn valorizzato
090400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090500031204     C* Se campo nn valorizzato skippo al prossimo
090600031204     C                   EVAL      SkSplitCSV(i) = *blanks
090700031204     C                   EVAL      posA  = posA + 1
090800031204     C                   ELSE
090900030715     C* Eseguo lo scan x trovare la fine del campo corrente
091000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091100030715     C* Gestisco l'ultimo campo
091200030715     C                   IF        posA = *zeros
091300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   LEAVE
091800030715     C                   ELSE
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C                   ENDIF
092300031204     C                   ENDIF
092400030715     C                   ENDIF
092500030715     C* Incremento il contatore d campo
092600030715     C                   EVAL      i = i +1
092700030715     C                   ENDDO
092800030715     C*
092900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093000030715     C* delimitazione testo
093100030715     C                   EVAL      i = 1
093200030715     C                   DOW       i < %elem(SkSplitCSV)
093300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093500030715     C                   EVAL      i = i + 1
093600030715     C                   ENDDO
093700030715     C*
093800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093900030715     C                   EVAL      i = 1
094000030715     C                   DOW       i < %elem(SkSplitFLD)
094100030715     C*
094200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094400030715     C                             SkSplitCSV(i) <> *blanks
094500030721     C*** ATB
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094700030721     C                   EVAL      VABATB = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030715     C*** RSD
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095400030715     C* ==
095500030715     C                   ENDIF
095600030721     C*** RD2
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030715     C*** IND
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096200030715     C                   EVAL      VABIND = SkSplitCSV(i)
096300030715     C                   ENDIF
096400030715     C*** LOD
096500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096700030715     C                   ENDIF
096800030715     C*** PRD
096900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097100030715     C                   ENDIF
097200030721     C*** NZD
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030715     C*** RMA
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** NOT
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** NT2
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** VCA
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** VAS
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** FFD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030721     C*** GC1
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GC2
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** TSP
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101100030721     C                   ENDIF
101200040510     C*** CBO
101300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101500040510     C                   ENDIF
101600030721     C*** NAS
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TIC
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** GCA
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** XCO
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** CTM
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** TCR
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** CTS
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** FTM
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** VAD
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** GMA
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** GGA
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** GVA
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TC1
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** TC2
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** SCL
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** RMO
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** NMO
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*
108500030721     C* Reperisco quindi i campi numerici...
108600040510     C*** NSP
108700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108800040510     C                   EVAL      PiStr=SkSplitCSV(i)
108900040510     C                   EXSR      CHKNUM
109000040510     C                   IF        PiInt=*on
109100040510     C                   Z-ADD     PiVal         VABNSP
109200040510     C                   ELSE
109300040510     C                   SETON                                        31
109400040510     C                   EVAL      VABNSP = *zeros
109500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109600040510     C                             + ' ' + 'VABNSP'
109700040510     C                   ENDIF
109800040510     C                   ENDIF
109900071106     C*** RMN
110000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110100071106     C                   EVAL      PiStr=SkSplitCSV(i)
110200071106     C                   EXSR      CHKNUM
110300071106     C                   IF        PiInt=*on
110400071106     C                   Z-ADD     PiVal         VABRMN
110500071106     C                   ELSE
110600071106     C                   SETON                                        32
110700071106     C                   EVAL      VABRMN = *zeros
110800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110900071106     C                             + ' ' + 'VABRMN'
111000071106     C                   ENDIF
111100071106     C                   ENDIF
111200030721     C*** CAD
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on
111700030721     C                   Z-ADD     PiVal         Num5_0
111800030721     C                   MOVEL(p)  Num5_0        VABCAD
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100030721     C                   EVAL      VABCAD = *zeros
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300030721     C                             + ' ' + 'VABCAD'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600030721     C*** NCL
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on
113100030721     C                   Z-ADD     PiVal         VABNCL
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      VABNCL = *zeros
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600040122     C                             + ' ' + 'VABNCL'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** PKB
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiNum=*on
114400030721     C                   Z-ADD     PiVal         VABPKB
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      VABPKB = *zeros
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABPKB'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** CAS
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115400080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
115500080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
115600081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
115700081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
115800080125     C                   EVAL      wFlgCAS = '1'
115900080125     C                   ENDIF
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiNum=*on
116300030721     C                   Z-ADD     PiVal         VABCAS
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      VABCAS = *zeros
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABCAS'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** CCM
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on      AND
117600030721     C                             PiVal<=9999999 AND
117700030721     C                             PiVal>*zeros
117800030721     C                   Z-ADD     PiVal         VABCCM
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABCCM = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040510     C                             + ' ' + 'VABCCM'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** LNP
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on    AND
119100030721     C                             PiVal<=999   AND
119200030721     C                             PiVal>*zeros
119300030721     C                   Z-ADD     PiVal         VABLNP
119400030721     C                   ELSE
119500040715     C                   SETON                                        32
119600030721     C                   EVAL      VABLNP = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800040510     C                             + ' ' + 'VABLNP'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** NRS
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on AND
120600030721     C                             PiVal<=99
120700030721     C                   Z-ADD     PiVal         VABNRS
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABNRS'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** LNA
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on    AND
121900030721     C                             PiVal<=999   AND
122000030721     C                             PiVal>*zeros
122100030721     C                   Z-ADD     PiVal         VABLNA
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      VABLNA = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABLNA'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** CTR
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on  AND
123400030721     C                             PiVal<=999
123500030721     C                   Z-ADD     PiVal         VABCTR
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABCTR'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** IAS
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiNum=*on
124700030721     C                   Z-ADD     PiVal         VABIAS
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABIAS'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** VLB
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiNum=*on
125900030721     C                   Z-ADD     PiVal         VABVLB
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABVLB'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** QFT
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiNum=*on
127100030721     C                   Z-ADD     PiVal         VABQFT
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABQFT'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** NCD
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on      AND
128300030721     C                             PiVal<=9999999
128400030721     C                   Z-ADD     PiVal         VABNCD
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABNCD'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** NCA
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiInt=*on      AND
129600030721     C                             PiVal<=9999999
129700030721     C                   Z-ADD     PiVal         VABNCA
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABNCA'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** ZNC
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on AND
130900030721     C                             PiVal<=99
131000030721     C                   Z-ADD     PiVal         VABZNC
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABZNC'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** DCR
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on       AND
132200030721     C                             PiVal<=99999999
132300030721     C                   Z-ADD     PiVal         VABDCR
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABDCR'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** HCR
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on   AND
133500030721     C                             PiVal<=9999
133600030721     C                   Z-ADD     PiVal         VABHCR
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABHCR'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** VMD
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiNum=*on
134800030721     C                   Z-ADD     PiVal         VABVMD
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABVMD'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** ANT
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on        AND
136000030721     C                             PiVal<=999999999
136100030721     C                   Z-ADD     PiVal         VABANT
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABANT'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** CMO
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on
137300030721     C                   Z-ADD     PiVal         Num5_0
137400030721     C                   MOVEL(p)  Num5_0        VABCMO
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      VABCMO = *zeros
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABCMO'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200040510     C*
138300040510     C* Reperisco i campi del FIVAT
138400040510     C*** VATNOT
138500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138700040510     C                   ENDIF
138800040510     C*** VATTRC
138900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139100040510     C                   ENDIF
139200060331     C*** VATNOT_A
139300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139500060331     C                   ENDIF
139600060331     C*** VATNOT_B
139700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139900060331     C                   ENDIF
140000060331     C*** VATNOT_E
140100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140300060331     C                   ENDIF
140400070823     C*
140500071029     C***  ===>  Gestione campi "particolari" dentro ciclo
140600071204     C*
140700070925     C*
140800071029     C***  <===  -----------------------------------------
140900040510     C*
141000030715     C                   ENDIF
141100030715     C                   EVAL      i = i + 1
141200030715     C                   ENDDO
141300070925     C*
141400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
141500080125     C*
141600081016     C                   EVAL      VABNSP = depspe + 1
141700070925     C*
141800071029     C***  <===  -----------------------------------------
141900070925     C*
142000070925     C* Considerazioni finali su CBO/CAS
142100080125     C                   IF        wFlgCAS = '1'
142200070925     C                   IF        VABCBO = '1'
142300070925     C                   EVAL      VABCBO = '4'
142400070925     C                   ENDIF
142500070925     C                   IF        VABCBO = '2'
142600070925     C                   EVAL      VABCBO = '6'
142700070925     C                   ENDIF
142800070925     C                   ENDIF
142900070925     C***  <===  ----------------------------
143000040510     C*
143100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
143200040510     C                   IF        VABNSP = *zeros
143300040510     C                   SETON                                        31
143400040510     C                   ENDIF
143500040715     C*
143600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
143700040715     C                   IF        VABLNP = *zeros
143800040715     C                   SETON                                        31
143900040715     C                   ENDIF
144000040510     C*
144100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
144200040510     C                   IF        VABRMN = *zeros
144300040510     C                   Z-ADD     VABNSP        VABRMN
144400040510     C                   ENDIF
144500040510     C*
144600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
144700040510     C                   IF        VABRMA = *blanks
144800040714     C                   MOVEL     VABRMN        VABRMA
144900040510     C                   ENDIF
145000040128     C*
145100040510     C* Se provincia nn valorizzata la reperisco
145200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
145300040510     C                   IF        VABPRD = *blanks
145400040128     C                   CLEAR                   TISI95DS
145500040128     C                   EVAL      I95TCN = '3'
145600040128     C                   Z-ADD     datcor        I95DAT
145700040128     C                   EVAL      I95NAR = VABNZD
145800040128     C                   EVAL      I95CAP = VABCAD
145900040128     C                   EVAL      I95LOC = VABLOD
146000040128     C                   CALL      'TISI95R'
146100040128     C                   PARM                    TISI95DS
146200040128     C                   EVAL      VABPRD = O95PRV
146300040510     C                   ENDIF
146400020204     C*
146500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
146600020204     C                   EXSR      CHKIMPDIV
146700030715     C                   ENDIF
146800020204     C*
146900010607     C                   ENDSR
147000010607     C*----------------------------------------------------*
147100010601
147200020204
147300020204     C*----------------------------------------------------*
147400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
147500020204     C*----------------------------------------------------*
147600020204     C     CHKIMPDIV     BEGSR
147700020204     C*
147800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
147900020204     C                   Z-ADD     *zeros        wrkDec            9 9
148000020204     C*
148100020204     C* Come prima cosa effettuo considerazioni sulla divisa
148200020204     C                   IF        vabIAS > *zeros
148300020204     C                   IF        vabVAS <> 'EUR'
148400020204     C                   EVAL      vabVAS =  'ITL'
148500020204     C                   ENDIF
148600020204     C                   ENDIF
148700020204     C*
148800020204     C                   IF        vabCAS > *zeros
148900020204     C                   IF        vabVCA <> 'EUR'
149000020204     C                   EVAL      vabVCA =  'ITL'
149100020204     C                   ENDIF
149200020204     C                   ENDIF
149300020204     C*
149400020204     C                   IF        vabVMD > *zeros
149500020204     C                   IF        vabVAD <> 'EUR'
149600020204     C                   EVAL      vabVAD =  'ITL'
149700020204     C                   ENDIF
149800020204     C                   ENDIF
149900020204     C*
150000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
150100020204     C                   Z-ADD     vabIAS        wrkDec
150200020204     C                   IF        wrkDec > *zeros
150300020204     C                   IF        vabVAS = 'ITL'
150400020204     C                   EVAL      vabIAS = *zeros
150500020204     C                   ENDIF
150600020204     C                   ENDIF
150700020204     C*
150800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
150900020204     C                   Z-ADD     vabCAS        wrkDec
151000020204     C                   IF        wrkDec > *zeros
151100020204     C                   IF        vabVCA = 'ITL'
151200020204     C                   EVAL      vabCAS = *zeros
151300020204     C                   ENDIF
151400020204     C                   ENDIF
151500020204     C*
151600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
151700020204     C                   Z-ADD     vabVMD        wrkDec
151800020204     C                   IF        wrkDec > *zeros
151900020204     C                   IF        vabVAD = 'ITL'
152000020204     C                   EVAL      vabVMD = *zeros
152100020204     C                   ENDIF
152200020204     C                   ENDIF
152300020204     C*
152400020204     C                   ENDSR
152500020204     C***
152600080617
152700080617
152800080617
152900080617     C*----------------------------------------------------*
153000080617     C*  CONTROLLO NUMERICITA' CAMPI
153100080617     C*----------------------------------------------------*
153200080617     C     CHKNUM        BEGSR
153300080617     C*
153400080617     C                   IF        PiDecChr = *blanks
153500080617     C                   EVAL      PiDecChr = CharNUM
153600080617     C                   ENDIF
153700080617     C*
153800080617     C                   callp     UBISNUM_Check(PiStr
153900080617     C                                          :PiDecChr
154000080617     C                                          :PiVal
154100080617     C                                          :PiNum
154200080617     C                                          :PiInt)
154300080617     C*
154400080617     C                   ENDSR
154500080617     C***
154600010330
154700010601
154800010601
154900010601
155000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
155100010601     C     repfil        BEGSR
155200010601     C*
155300010601     C                   if        invfil = *zeros and
155400010601     C                             depfil > *zeros and
155500010629     C                             (vinflg = *blanks or
155600010629     C                              vinflg = *zeros)
155700010601     C
155800010601     C                   eval      invfil = depfil
155900010601     C                   endif
156000010601     C*
156100010601     C                   if        depfil <> invfil and
156200010601     C                             invfil > *zeros
156300010601     C                   eval      flgMulti = '1'
156400010601     C                   if        vinflg = *blanks
156500010601     C                   add       1             cntNonEl
156600010601     C                   endif
156700010601     C                   endif
156800010601     C*
156900010601     C                   if        vinflg = '2'
157000010601     C                   eval      flgStato = '2'
157100010601     C                   endif
157200010601     C*
157300010601     C                   ENDSR
157400010601     C***
157500010601
157600010601
157700010601
157800010330
157900040119      /TITLE Invio dei dati al punto operativo.
158000040119     C     invio         BEGSR
158100040119     C*
158200040119     C* 1° invio FIVAT
158300040119     C                   reset                   dscmz
158400040119     C                   move      vlrpoi        cmzdst
158500040119     C                   eval      cmzfld = 'FIVATWWR'
158600040119     C                   eval      cmzmbd = vlrhdl
158700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
158800040119     C***                if        prmfir = *blanks
158900040119     C                   eval      cmzfla = 'FIVAT00F'
159000040119     C                   eval      cmzmba = 'FIVAT00F'
159100040119     C***                else
159200040119     C***                eval      cmzfla = prmfir
159300040119     C***                eval      cmzmba = prmfir
159400040119     C***                endif
159500040119     C                   eval      cmznrr = *zeros
159600040119     C                   move      §ctrokvt      cmznrr
159700040119     C                   eval      cmzlba = vlrfl1
159800040119     C                   call(e)   'TIS711C'
159900040119     C                   parm                    dscmz
160000040119     C                   parm      *blanks       esito
160100040119     C                   if        %error
160200040119     C                             or cmzerr = '1'
160300040119     C                             or esito  = '1'
160400040119     C                   eval      wrkesito = '3'
160500040119     C                   else
160600040119     C*
160700040119     C* 2° invio FIVAB
160800040119     C                   reset                   dscmz
160900040119     C                   move      vlrpoi        cmzdst
161000040119     C                   eval      cmzfld = vlrfou
161100040119     C                   eval      cmzmbd = vlrhdl
161200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161300040119     C***                if        prmfir = *blanks
161400040119     C                   eval      cmzfla = 'FIVAB00F'
161500040119     C                   eval      cmzmba = 'FIVAB00F'
161600040119     C***                else
161700040119     C***                eval      cmzfla = prmfir
161800040119     C***                eval      cmzmba = prmfir
161900040119     C***                endif
162000040119     C                   eval      cmznrr = *zeros
162100040119     C                   move      §ctrokvb      cmznrr
162200040119     C                   eval      cmzlba = vlrfl1
162300040119     C                   call(e)   'TIS711C'
162400040119     C                   parm                    dscmz
162500040119     C                   parm      *blanks       esito
162600040119     C                   if        %error
162700040119     C                             or cmzerr = '1'
162800040119     C                             or esito  = '1'
162900040119     C                   eval      wrkesito = '3'
163000040119     C                   endif
163100040119     C                   endif
163200040119     C*
163300040119     C                   ENDSR
163400040119     C***
163500010601
163600010601
163700010601
163800010601
163900010601      /TITLE Invio dei dati al punto operativo.
164000010601     C     opeini        BEGSR
164100010601     C*
164200010601     C* Inizializzo flag e contatori operativi
164300010601     C                   movel     '0'           flgGiro           1
164400010601     C                   movel     '0'           flgMulti          1
164500010601     C                   movel     '1'           flgStato          1
164600010615     C                   movel     '0'           flgOk             1
164700010601     C                   z-add     *zeros        cntNonEl         10 0
164800010601     C                   z-add     *zeros        depfil            3 0
164900010601     C                   z-add     *zeros        invfil            3 0
165000010601     C*
165100010601     C                   ENDSR
165200010601     C***
165300070326
165400070326
165500070326
165600070326
165700080916     C     *pssr         BEGSR
165800070329     C*
165900080916     C                   if        %open(tivin00r)
166000080916     C                   close     tivin00r
166100080916     C                   endif
166200080916     C                   if        %open(fivabwwr)
166300080916     C                   close     fivabwwr
166400080916     C                   endif
166500080916     C                   if        %open(fivatwwr)
166600080916     C                   close     fivatwwr
166700080916     C                   endif
166800070326     C*
166900070326     C* Effettuo la chiamata al CLLE preposto
167000080916     C                   call(e)   'TITVVTC'
167100080916     C                   parm                    parccm
167200080916     C                   parm                    parmbr
167300080916     C                   parm      '2'           paropz
167400070326     C*
167500080916     C                   eval      wrkesito = '2'
167600070404     C*
167700080916     C                   seton                                        LR
167800070326     C*
167900080916     C                   ENDSR     '*CANCL'
168000070326     C***
168100070326
168200070326
168300010330
168400010330
168500000613     C     *inzsr        BEGSR
168600990910     C*
168700990910     C     *entry        plist
168800990920     C                   parm                    tivlrds
168900990921     C                   parm      wrkesito      esito
169000000724     C                   parm                    prmlit
169100000710     C                   parm                    prmfir
169200010330     C*
169300010330     C* CALCOLA LA DATA CORRENTE
169400010330     C                   time                    wn14             14 0
169500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
169600010330     C                   z-add     wn8           g08dat
169700010330     C                   z-add     *zeros        g08inv
169800010330     C                   movel     '0'           g08err
169900010330     C                   call      'XSRDA8'
170000010330     C                   parm                    wlbda8
170100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
170200000613     C*
170300000613     C                   ENDSR
170400000613     C***
170500990908
170600081016     Otitv1j5p  E            riepilogo         2
170700990915     O                                              'Upload via Internet'
170800990915     O                                           +1 'Traduzione TIVIN00R -'
170900021025     O                                           +1 'FIVABWWR'
171000990915     O                                           +1 'Testate bolle'
171100990915     O          E            riepilogo   2
171200990915     O                       wrkdata
171300990915     O                       wrkora              +1
171400990915     O                       procname            +1
171500990915     O          E            riepilogo   2
171600990915     O                                              'Cliente..................:'
171700010605     O                       VABCCM        z     +1
171800990915     O          E            riepilogo   2
171900990920     O                                              'Riferimento Strategi.....:'
172000990920     O                       vlrhdl              +1
172100990915     O          E            riepilogo   2
172200990915     O                                              'Giusti...................:'
172300040119     O                       §CTROKVB      2   +  1
172400990915     O          E            riepilogo   2
172500990915     O                                              'Sbagliati e corretti.....:'
172600971022     O                       §CTRMO        2   +  1
172700990915     O          E            riepilogo   2
172800990915     O                                              'Sbagliati e scartati.....:'
172900971022     O                       §CTRNO        2   +  1
173000000613
173100081016     Otitv1j5ps E            testdett          2
173200000613     O                                              'Upload via Internet'
173300000613     O                                           +1 'Traduzione TIVIN00R -'
173400021025     O                                           +1 'FIVABWWR'
173500000613     O                                           +1 'Testate bolle'
173600000616     O          E            testdett    3
173700000613     O                                           +2 'N° rec'
173800000613     O                                           +3 'Anteprima contenuto'
173900000616     O          E            rigadett    2
174000000613     O                       rrnum               +2
174100000621     O                       recko               +3
174200000616     O          E            findett     2
174300000613     O                       wrkdata
174400000613     O                       wrkora              +1
174500000613     O                       procname            +1
174600000616     O          E            findett     2
174700000613     O                                              'Cliente..................:'
174800010605     O                       VABCCM        z     +1
174900000616     O          E            findett     2
175000000613     O                                              'Riferimento Strategi.....:'
175100000613     O                       vlrhdl              +1
175200000616     O          E            findett     2
175300000613     O                                              'Giusti...................:'
175400040119     O                       §CTROKVB      2   +  1
175500000616     O          E            findett     2
175600000613     O                                              'Sbagliati e corretti.....:'
175700000613     O                       §CTRMO        2   +  1
175800000616     O          E            findett     2
175900000613     O                                              'Sbagliati e scartati.....:'
176000000613     O                       §CTRNO        2   +  1
176100000613** CMD - COMANDI CL
176200081016OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1J5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
176300081016OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1J5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
176400081016DLTOVR FILE(TITV1J5P TITV1J5PS) LVL(*)
176500010606
176600000613
176700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
176800081024FILLE01;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;FILLER02;
176900110915VABNOT;VABNAS;VATNOT_A;VATNOT_B;
177000080715
