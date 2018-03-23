000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800081124     Ftitv1k6p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100081124     Ftitv1k6ps O    f  198        PRINTER usropn
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
015000081124     C                   if        not %open(titv1k6ps)
015100081124     C                   open      titv1k6ps
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
019000081124     C                   if        %open(titv1k6ps)
019100000616     C                   except    findett
019200081124     C                   close     titv1k6ps
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
021200081124     C                   if        not %open(titv1k6p)
021300081124     C   50              open      titv1k6p
021400990915     C                   endif
021500990915     C*
021600030715     C   50              except    riepilogo
021700990915     C*
021800081124     C                   if        %open(titv1k6p)
021900081124     C   50              close     titv1k6p
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
025500080916     C                   exsr      inzvar
025600080916     C                   exsr      impvab
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
044600081124     C*
044700080625     C                   if        wVATNOT_E <> *blanks
044800081124     C                   exsr      exeVATE
044900080625     C                   endif
045000040119     C*
045100040119     C                   ENDSR
045200081124
045300081124
045400081124
045500081124     C*----------------------------------------------------*
045600081124     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045700081124     C*----------------------------------------------------*
045800081124     C     EXEVATE       BEGSR
045900081124     C*
046000081124     C                   Z-ADD     1             wCollo            2 0
046100081124     C                   MOVEL     *zeros        wColloAlfa1      12
046200081124     C                   MOVEL     *zeros        wColloAlfa2       2
046300081124     C*
046400081124     C                   EVAL      VATTRC='E'
046500081124     C                   EVAL      wColloAlfa1=%trim(wVATNOT_E)
046600081124     C*
046700081124     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
046800081124     C                   dow       wCollo <= VABNCL and wCollo < 99
046900081124     C                   move(p)   wCollo        wColloAlfa2
047000081124     C                   eval      VATNOT = %trim(wColloAlfa1) + wColloAlfa2
047100081124     C* Eseguo calcolo check-digit
047200081124     C                   CALL      'TRUL35R1'
047300081124     C                   PARM      VATNOT        InBarcode        35
047400081124     C                   PARM      'EANP'        InTip             4
047500081124     C                   PARM      *blanks       InDsply           1
047600081124     C                   PARM      *blanks       OutBarcode       36
047700081124     C*
047800081124     C                   EVAL      VATNOT = %trim(OutBarcode)
047900081124     C*
048000081124     C                   write     fivat000                                     => scarico VAT
048100081124     C                   add       1             §CTROKVT
048200081124     C                   add       1             wCollo
048300081124     C                   enddo
048400081124     C*
048500081124     C                   ENDSR
048600040119
048700040119
048800040119
048900040119     C*----------------------------------------------------*
049000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049100040119     C*----------------------------------------------------*
049200040119     C     PREVAT        BEGSR
049300040119     C*
049400040119     C* Compongo il nome del membro da dare al FIVATWWR
049500040119     C                   eval      parmbr = vlrhdl
049600040119     C                   movel     'M'           parmbr
049700040510     C                   eval      parccm = vlrksc
049800040119     C                   eval      paropz = '1'
049900040119     C* Effettuo la chiamata al CLLE preposto
050000040119     C                   call(e)   'TITVVTC'
050100040119     C                   parm                    parccm
050200040119     C                   parm                    parmbr
050300040119     C                   parm                    paropz
050400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050500040119     C                   if        %error
050600040119     C                   movel     '1'           chkcall
050700040119     C                   else
050800040119     C                   movel     '0'           chkcall
050900040119     C                   endif
051000040119     C*
051100040119     C                   ENDSR
051200010601
051300010601
051400010601
051500010330     C*----------------------------------------------------*
051600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051700010330     C*----------------------------------------------------*
051800040119     C     INZVAR        BEGSR
051900010330     C*
052000030822     C* Inizializzo variabili di wrk
052100020204     C                   Z-ADD     *zeros        Num5_0            5 0
052200060331     C                   MOVEL     *blanks       wVATNOT_A
052300060331     C                   MOVEL     *blanks       wVATNOT_B
052400060331     C                   MOVEL     *blanks       wVATNOT_E
052500080617     C                   EVAL      wFlgCAS = *blanks
052600030822     C*
052700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052800030822     C                   CLEAR                   FIVAB000
052900040119     C                   CLEAR                   FIVAT000
053000030822     C                   CLEAR                   SkSplitCSV
053100030822     C                   EVAL      i = 1
053200030822     C                   EVAL      posDa = *zeros
053300030822     C                   EVAL      posA  = *zeros
053400030822     C*
053500030822     C* Reimposto i valori di default
053600030822     C                   EXSR      DEFCAM
053700030822     C*
053800010330     C                   ENDSR
053900010330     C*----------------------------------------------------*
054000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054100010330     C*----------------------------------------------------*
054200010330     C     DEFCAM        BEGSR
054300030822     C*
054400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054500030715     C* e delimitatore testo.
054600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054900030822     C*
055000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
055200030715     C                   SETON                                        50
055300030715     C                   ELSE
055400030715     C                   SETOFF                                       50
055500030715     C                   ENDIF
055600030822     C*
055700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055800030822     C                   EVAL      posDaDft = 1
055900030822     C                   EVAL      posADft  = 0
056000030822     C                   EVAL      wGiroDft = 0
056100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056200030822     C                             posDaDft > 0
056300030822     C*
056400030822     C* Gestisco il 1° giro
056500030822     C                   IF        wGiroDft = 0
056600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056800030822     C* Incremento il contatore dei "giri"
056900030822     C                   EVAL      wGiroDft = 1
057000030822     C                   ELSE
057100030822     C                   EVAL      posDaDft = posADft
057200030822     C                   ENDIF
057300030822     C* Eseguo lo scan x trovare la fine del campo corrente
057400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057500030822     C*
057600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057800030822     C                   IF        posDaDft > 0 AND
057900030822     C                             posADft  > 0
058000030822     C* NCL
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'NCL'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000030822     C                   Z-ADD     PiVal         VABNCL
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* CCM
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'CCM'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABCCM
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* LNP
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'LNP'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABLNP
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* NRS
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'NRS'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiInt=*on
062900030822     C                   Z-ADD     PiVal         VABNRS
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* CTR
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'CTR'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiInt=*on
064200030822     C                   Z-ADD     PiVal         VABCTR
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* PKB
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'PKB'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiNum=*on
065500030822     C                   Z-ADD     PiVal         VABPKB
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* VLB
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'VLB'
066300030822     C                   EVAL      PiStr=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   EXSR      CHKNUM
066700030822     C                   IF        PiNum=*on
066800030822     C                   Z-ADD     PiVal         VABVLB
066900030822     C                   ENDIF
067000030822     C                   ENDIF
067100030822     C* QFT
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'QFT'
067600030822     C                   EVAL      PiStr=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   EXSR      CHKNUM
068000030822     C                   IF        PiNum=*on
068100030822     C                   Z-ADD     PiVal         VABQFT
068200030822     C                   ENDIF
068300030822     C                   ENDIF
068400030822     C* CBO
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'CBO'
068900030822     C                   EVAL      VABCBO=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* TSP
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'TSP'
069800030822     C                   EVAL      VABTSP=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* VAS
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'VAS'
070700030822     C                   EVAL      VABVAS=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* VCA
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'VCA'
071600030822     C                   EVAL      VABVCA=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000070925     C* TIC
072100070925     C                   IF        %subst(
072200070925     C                             %subst(vlrppt:posDaDft+1:
072300070925     C                             posADft-posDaDft-1):1:3)
072400070925     C                             = 'TIC'
072500070925     C                   EVAL      VABTIC=%trim(%subst(
072600070925     C                             %subst(vlrppt:posDaDft+1:
072700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800070925     C                   ENDIF
072900030822     C* GCA
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'GCA'
073400030822     C                   EVAL      VABGCA=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* CTM
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'CTM'
074300030822     C                   EVAL      VABCTM=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* FFD
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'FFD'
075200030822     C                   EVAL      VABFFD=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* VAD
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'VAD'
076100030822     C                   EVAL      VABVAD=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* GMA
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'GMA'
077000030822     C                   EVAL      VABGMA=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* GGA
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'GGA'
077900030822     C                   EVAL      VABGGA=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* GVA
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'GVA'
078800030822     C                   EVAL      VABGVA=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* TC1
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'TC1'
079700030822     C                   EVAL      VABTC1=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* TC2
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'TC2'
080600030822     C                   EVAL      VABTC2=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000040714     C* VATTRC
081100040714     C                   IF        %subst(
081200040714     C                             %subst(vlrppt:posDaDft+1:
081300040714     C                             posADft-posDaDft-1):1:3)
081400040714     C                             = 'TRC'
081500040714     C                   EVAL      VATTRC=%trim(%subst(
081600040714     C                             %subst(vlrppt:posDaDft+1:
081700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800040714     C                   ENDIF
081900030822     C* ...
082000030822     C                   ENDIF
082100030822     C                   ENDDO
082200020204     C*
082300010330     C                   ENDSR
082400010607     C*----------------------------------------------------*
082500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082600010607     C*----------------------------------------------------*
082700010607     C     IMPVAB        BEGSR
082800010607     C*
082900010607     C                   SETOFF                                       3132
083000030515     C*
083100010607     C                   MOVEL     datcor        VABAAS
083200010607     C                   MOVE      datcor        VABMGS
083300010607     C*
083400030715     C**********
083500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083600030715     C**********
083700030715     C                   IF        wGiro = *zeros
083800080716     C*
083900080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
084000080716     C                   EVAL      depvinDTA = vinDTA
084100040113     C*
084200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084400040510     C                             %trim(intcol(3))<>*blanks
084500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084600040113     C                                      %trim(intcol(3))
084700070823     C     ';':CharCSV   XLATE     vindta        vindta
084800040510     C                   ENDIF
084900080626     C*
085000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085100031201     C     minu:maiu     XLATE     vindta        vindta
085200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085300030715     C                   DOW       posDa <= %len(%trim(vindta))
085400030715     C*
085500030715     C* Gestisco il 1° campo
085600030715     C                   IF        i = 1
085700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085800030715     C                   EVAL      posDa = 1
085900030715     C* Eseguo lo scan x trovare la fine del primo campo
086000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086100030715     C* A questo punto "estraggo" il campo corrente
086200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086300030715     C                                                    (posA-posDa))
086400030715     C* X i campi successivi al 1°
086500030715     C                   ELSE
086600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086700030715     C                   EVAL      posDa = posA + 1
086800030715     C* Eseguo lo scan x trovare la fine del campo corrente
086900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087000030715     C* Gestisco l'ultimo campo
087100030715     C                   IF        posA = *zeros
087200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087300030715     C* A questo punto "estraggo" il campo corrente
087400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087500030715     C                                                    (posA-posDa))
087600030715     C                   LEAVE
087700030715     C                   ELSE
087800030715     C* A questo punto "estraggo" il campo corrente
087900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088000030715     C                                                    (posA-posDa))
088100030715     C                   ENDIF
088200030715     C                   ENDIF
088300030715     C* Incremento il contatore d campo
088400030715     C                   EVAL      i = i +1
088500030715     C                   ENDDO
088600030715     C*
088700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088800030715     C* delimitazione testo
088900030715     C                   EVAL      i = 1
089000030715     C                   DOW       i < %elem(SkSplitFLD)
089100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089300030715     C                   EVAL      i = i + 1
089400030715     C                   ENDDO
089500030715     C                   EVAL      wGiro = 1
089600080716     C* Ripristino la 1° riga originale
089700080716     C                   EVAL      vinDTA = depvinDTA
089800030715     C                   ELSE
089900030715     C                   EVAL      wGiro = 2
090000031204     C**********
090100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090200031204     C* d separatore campo
090300031204     C**********
090400031204     C                   EVAL      vindta = %trim(vindta)
090500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090600031204     C                   EVAL      vindta = %subst(vindta:2)
090700031204     C                   ENDDO
090800031204     C*
090900031204     C                   Z-ADD     *zeros        lunghInput        4 0
091000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091400031204     C                   ENDDO
091500030715     C**********
091600030715     C* Effettuo lo split del campo dati d input
091700030715     C**********
091800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091900030715     C                   DOW       posDa <= %len(%trim(vindta))
092000030715     C*
092100030715     C* Gestisco il 1° campo
092200030715     C                   IF        i = 1
092300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092400030715     C                   EVAL      posDa = 1
092500030715     C* Eseguo lo scan x trovare la fine del primo campo
092600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092700030715     C* A questo punto "estraggo" il campo corrente
092800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092900030715     C                                                    (posA-posDa))
093000030715     C* X i campi successivi al 1°
093100030715     C                   ELSE
093200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093300030715     C                   EVAL      posDa = posA + 1
093400031204     C* Verifico che nn vi sia il campo nn valorizzato
093500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093600031204     C* Se campo nn valorizzato skippo al prossimo
093700031204     C                   EVAL      SkSplitCSV(i) = *blanks
093800031204     C                   EVAL      posA  = posA + 1
093900031204     C                   ELSE
094000030715     C* Eseguo lo scan x trovare la fine del campo corrente
094100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094200030715     C* Gestisco l'ultimo campo
094300030715     C                   IF        posA = *zeros
094400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094500030715     C* A questo punto "estraggo" il campo corrente
094600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094700030715     C                                                    (posA-posDa))
094800030715     C                   LEAVE
094900030715     C                   ELSE
095000030715     C* A questo punto "estraggo" il campo corrente
095100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095200030715     C                                                    (posA-posDa))
095300030715     C                   ENDIF
095400031204     C                   ENDIF
095500030715     C                   ENDIF
095600030715     C* Incremento il contatore d campo
095700030715     C                   EVAL      i = i +1
095800030715     C                   ENDDO
095900030715     C*
096000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096100030715     C* delimitazione testo
096200030715     C                   EVAL      i = 1
096300030715     C                   DOW       i < %elem(SkSplitCSV)
096400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096600030715     C                   EVAL      i = i + 1
096700030715     C                   ENDDO
096800030715     C*
096900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097000030715     C                   EVAL      i = 1
097100030715     C                   DOW       i < %elem(SkSplitFLD)
097200030715     C*
097300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097500030715     C                             SkSplitCSV(i) <> *blanks
097600030721     C*** ATB
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097800030721     C                   EVAL      VABATB = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030715     C*** RSD
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098500030715     C* ==
098600030715     C                   ENDIF
098700030721     C*** RD2
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030715     C*** IND
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099300030715     C                   EVAL      VABIND = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** LOD
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** PRD
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030721     C*** NZD
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030715     C*** RMA
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** NOT
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** NT2
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030715     C*** VCA
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030715     C*** VAS
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** FFD
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030721     C*** GC1
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** GC2
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TSP
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104200030721     C                   ENDIF
104300040510     C*** CBO
104400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104600040510     C                   ENDIF
104700030721     C*** NAS
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** TIC
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GCA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** XCO
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** CTM
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TCR
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** CTS
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** FTM
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** VAD
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** GMA
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** GGA
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** GVA
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** TC1
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** TC2
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** SCL
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** RMO
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** NMO
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*
111600030721     C* Reperisco quindi i campi numerici...
111700040510     C*** NSP
111800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111900040510     C                   EVAL      PiStr=SkSplitCSV(i)
112000040510     C                   EXSR      CHKNUM
112100040510     C                   IF        PiInt=*on
112200040510     C                   Z-ADD     PiVal         VABNSP
112300040510     C                   ELSE
112400040510     C                   SETON                                        31
112500040510     C                   EVAL      VABNSP = *zeros
112600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112700040510     C                             + ' ' + 'VABNSP'
112800040510     C                   ENDIF
112900040510     C                   ENDIF
113000071106     C*** RMN
113100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113200071106     C                   EVAL      PiStr=SkSplitCSV(i)
113300071106     C                   EXSR      CHKNUM
113400071106     C                   IF        PiInt=*on
113500071106     C                   Z-ADD     PiVal         VABRMN
113600071106     C                   ELSE
113700071106     C                   SETON                                        32
113800071106     C                   EVAL      VABRMN = *zeros
113900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114000071106     C                             + ' ' + 'VABRMN'
114100071106     C                   ENDIF
114200071106     C                   ENDIF
114300030721     C*** CAD
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiInt=*on
114800030721     C                   Z-ADD     PiVal         Num5_0
114900030721     C                   MOVEL(p)  Num5_0        VABCAD
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      VABCAD = *zeros
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400030721     C                             + ' ' + 'VABCAD'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** NCL
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiInt=*on
116200030721     C                   Z-ADD     PiVal         VABNCL
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABNCL = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700040122     C                             + ' ' + 'VABNCL'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** PKB
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiNum=*on
117500030721     C                   Z-ADD     PiVal         VABPKB
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABPKB = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABPKB'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** CAS
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118500080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
118600080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
118700081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
118800081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
118900080125     C                   EVAL      wFlgCAS = '1'
119000080125     C                   ENDIF
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiNum=*on
119400030721     C                   Z-ADD     PiVal         VABCAS
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      VABCAS = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABCAS'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** CCM
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on      AND
120700030721     C                             PiVal<=9999999 AND
120800030721     C                             PiVal>*zeros
120900030721     C                   Z-ADD     PiVal         VABCCM
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      VABCCM = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400040510     C                             + ' ' + 'VABCCM'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** LNP
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiInt=*on    AND
122200030721     C                             PiVal<=999   AND
122300030721     C                             PiVal>*zeros
122400030721     C                   Z-ADD     PiVal         VABLNP
122500030721     C                   ELSE
122600040715     C                   SETON                                        32
122700030721     C                   EVAL      VABLNP = *zeros
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900040510     C                             + ' ' + 'VABLNP'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** NRS
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on AND
123700030721     C                             PiVal<=99
123800030721     C                   Z-ADD     PiVal         VABNRS
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABNRS'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** LNA
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on    AND
125000030721     C                             PiVal<=999   AND
125100030721     C                             PiVal>*zeros
125200030721     C                   Z-ADD     PiVal         VABLNA
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      VABLNA = *zeros
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABLNA'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** CTR
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiInt=*on  AND
126500030721     C                             PiVal<=999
126600030721     C                   Z-ADD     PiVal         VABCTR
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABCTR'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** IAS
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiNum=*on
127800030721     C                   Z-ADD     PiVal         VABIAS
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABIAS'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** VLB
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiNum=*on
129000030721     C                   Z-ADD     PiVal         VABVLB
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABVLB'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** QFT
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiNum=*on
130200030721     C                   Z-ADD     PiVal         VABQFT
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABQFT'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** NCD
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on      AND
131400030721     C                             PiVal<=9999999
131500030721     C                   Z-ADD     PiVal         VABNCD
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABNCD'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** NCA
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on      AND
132700030721     C                             PiVal<=9999999
132800030721     C                   Z-ADD     PiVal         VABNCA
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABNCA'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** ZNC
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on AND
134000030721     C                             PiVal<=99
134100030721     C                   Z-ADD     PiVal         VABZNC
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABZNC'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** DCR
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on       AND
135300030721     C                             PiVal<=99999999
135400030721     C                   Z-ADD     PiVal         VABDCR
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABDCR'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** HCR
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on   AND
136600030721     C                             PiVal<=9999
136700030721     C                   Z-ADD     PiVal         VABHCR
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABHCR'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** VMD
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiNum=*on
137900030721     C                   Z-ADD     PiVal         VABVMD
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300030721     C                             + ' ' + 'VABVMD'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600030721     C*** ANT
138700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138800030721     C                   EVAL      PiStr=SkSplitCSV(i)
138900030721     C                   EXSR      CHKNUM
139000030721     C                   IF        PiInt=*on        AND
139100030721     C                             PiVal<=999999999
139200030721     C                   Z-ADD     PiVal         VABANT
139300030721     C                   ELSE
139400030721     C                   SETON                                        32
139500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139600030721     C                             + ' ' + 'VABANT'
139700030721     C                   ENDIF
139800030721     C                   ENDIF
139900030721     C*** CMO
140000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140100030721     C                   EVAL      PiStr=SkSplitCSV(i)
140200030721     C                   EXSR      CHKNUM
140300030721     C                   IF        PiInt=*on
140400030721     C                   Z-ADD     PiVal         Num5_0
140500030721     C                   MOVEL(p)  Num5_0        VABCMO
140600030721     C                   ELSE
140700030721     C                   SETON                                        32
140800030721     C                   EVAL      VABCMO = *zeros
140900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141000030721     C                             + ' ' + 'VABCMO'
141100030721     C                   ENDIF
141200030721     C                   ENDIF
141300040510     C*
141400040510     C* Reperisco i campi del FIVAT
141500040510     C*** VATNOT
141600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141800040510     C                   ENDIF
141900040510     C*** VATTRC
142000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142200040510     C                   ENDIF
142300060331     C*** VATNOT_A
142400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142600060331     C                   ENDIF
142700060331     C*** VATNOT_B
142800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143000060331     C                   ENDIF
143100060331     C*** VATNOT_E
143200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143400060331     C                   ENDIF
143500070823     C*
143600071029     C***  ===>  Gestione campi "particolari" dentro ciclo
143700071204     C*
143800080916     C*** NSP/RMN/RMA
143900080617     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
144000080916     C                   EVAL      VABRMA=SkSplitCSV(i)
144100080716     C                   EVAL      PiStr=SkSplitCSV(i)
144200071204     C                   EXSR      CHKNUM
144300071204     C                   IF        PiInt=*on
144400071204     C                   Z-ADD     PiVal         VABNSP
144500071204     C                   Z-ADD     PiVal         VATNSP
144600071204     C                   Z-ADD     PiVal         VABRMN
144700071204     C                   ELSE
144800080916     C                   SETON                                        31
144900071204     C                   EVAL      VABNSP = *zeros
145000071204     C                   EVAL      VATNSP = *zeros
145100071204     C                   EVAL      VABRMN = *zeros
145200071204     C                   EVAL      vinmsg = %trimr(vinmsg)
145300081014     C                             + ' ' + 'VABNSP VATNSP VABRMN VABRMA'
145400071204     C                   ENDIF
145500071204     C                   ENDIF
145600070925     C*
145700071029     C***  <===  -----------------------------------------
145800040510     C*
145900030715     C                   ENDIF
146000030715     C                   EVAL      i = i + 1
146100030715     C                   ENDDO
146200070925     C*
146300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146400080125     C*
146500070925     C*
146600071029     C***  <===  -----------------------------------------
146700070925     C*
146800070925     C* Considerazioni finali su CBO/CAS
146900080125     C                   IF        wFlgCAS = '1'
147000070925     C                   IF        VABCBO = '1'
147100070925     C                   EVAL      VABCBO = '4'
147200070925     C                   ENDIF
147300070925     C                   IF        VABCBO = '2'
147400070925     C                   EVAL      VABCBO = '6'
147500070925     C                   ENDIF
147600070925     C                   ENDIF
147700070925     C***  <===  ----------------------------
147800040510     C*
147900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148000040510     C                   IF        VABNSP = *zeros
148100040510     C                   SETON                                        31
148200040510     C                   ENDIF
148300040715     C*
148400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148500040715     C                   IF        VABLNP = *zeros
148600040715     C                   SETON                                        31
148700040715     C                   ENDIF
148800040510     C*
148900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149000040510     C                   IF        VABRMN = *zeros
149100040510     C                   Z-ADD     VABNSP        VABRMN
149200040510     C                   ENDIF
149300040510     C*
149400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149500040510     C                   IF        VABRMA = *blanks
149600040714     C                   MOVEL     VABRMN        VABRMA
149700040510     C                   ENDIF
149800040128     C*
149900040510     C* Se provincia nn valorizzata la reperisco
150000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150100040510     C                   IF        VABPRD = *blanks
150200040128     C                   CLEAR                   TISI95DS
150300040128     C                   EVAL      I95TCN = '3'
150400040128     C                   Z-ADD     datcor        I95DAT
150500040128     C                   EVAL      I95NAR = VABNZD
150600040128     C                   EVAL      I95CAP = VABCAD
150700040128     C                   EVAL      I95LOC = VABLOD
150800040128     C                   CALL      'TISI95R'
150900040128     C                   PARM                    TISI95DS
151000040128     C                   EVAL      VABPRD = O95PRV
151100040510     C                   ENDIF
151200020204     C*
151300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151400020204     C                   EXSR      CHKIMPDIV
151500030715     C                   ENDIF
151600020204     C*
151700010607     C                   ENDSR
151800010607     C*----------------------------------------------------*
151900010601
152000020204
152100020204     C*----------------------------------------------------*
152200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152300020204     C*----------------------------------------------------*
152400020204     C     CHKIMPDIV     BEGSR
152500020204     C*
152600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152700020204     C                   Z-ADD     *zeros        wrkDec            9 9
152800020204     C*
152900020204     C* Come prima cosa effettuo considerazioni sulla divisa
153000020204     C                   IF        vabIAS > *zeros
153100020204     C                   IF        vabVAS <> 'EUR'
153200020204     C                   EVAL      vabVAS =  'ITL'
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C                   IF        vabCAS > *zeros
153700020204     C                   IF        vabVCA <> 'EUR'
153800020204     C                   EVAL      vabVCA =  'ITL'
153900020204     C                   ENDIF
154000020204     C                   ENDIF
154100020204     C*
154200020204     C                   IF        vabVMD > *zeros
154300020204     C                   IF        vabVAD <> 'EUR'
154400020204     C                   EVAL      vabVAD =  'ITL'
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154900020204     C                   Z-ADD     vabIAS        wrkDec
155000020204     C                   IF        wrkDec > *zeros
155100020204     C                   IF        vabVAS = 'ITL'
155200020204     C                   EVAL      vabIAS = *zeros
155300020204     C                   ENDIF
155400020204     C                   ENDIF
155500020204     C*
155600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155700020204     C                   Z-ADD     vabCAS        wrkDec
155800020204     C                   IF        wrkDec > *zeros
155900020204     C                   IF        vabVCA = 'ITL'
156000020204     C                   EVAL      vabCAS = *zeros
156100020204     C                   ENDIF
156200020204     C                   ENDIF
156300020204     C*
156400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156500020204     C                   Z-ADD     vabVMD        wrkDec
156600020204     C                   IF        wrkDec > *zeros
156700020204     C                   IF        vabVAD = 'ITL'
156800020204     C                   EVAL      vabVMD = *zeros
156900020204     C                   ENDIF
157000020204     C                   ENDIF
157100020204     C*
157200020204     C                   ENDSR
157300020204     C***
157400080617
157500080617
157600080617
157700080617     C*----------------------------------------------------*
157800080617     C*  CONTROLLO NUMERICITA' CAMPI
157900080617     C*----------------------------------------------------*
158000080617     C     CHKNUM        BEGSR
158100080617     C*
158200080617     C                   IF        PiDecChr = *blanks
158300080617     C                   EVAL      PiDecChr = CharNUM
158400080617     C                   ENDIF
158500080617     C*
158600080617     C                   callp     UBISNUM_Check(PiStr
158700080617     C                                          :PiDecChr
158800080617     C                                          :PiVal
158900080617     C                                          :PiNum
159000080617     C                                          :PiInt)
159100080617     C*
159200080617     C                   ENDSR
159300080617     C***
159400010330
159500010601
159600010601
159700010601
159800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159900010601     C     repfil        BEGSR
160000010601     C*
160100010601     C                   if        invfil = *zeros and
160200010601     C                             depfil > *zeros and
160300010629     C                             (vinflg = *blanks or
160400010629     C                              vinflg = *zeros)
160500010601     C
160600010601     C                   eval      invfil = depfil
160700010601     C                   endif
160800010601     C*
160900010601     C                   if        depfil <> invfil and
161000010601     C                             invfil > *zeros
161100010601     C                   eval      flgMulti = '1'
161200010601     C                   if        vinflg = *blanks
161300010601     C                   add       1             cntNonEl
161400010601     C                   endif
161500010601     C                   endif
161600010601     C*
161700010601     C                   if        vinflg = '2'
161800010601     C                   eval      flgStato = '2'
161900010601     C                   endif
162000010601     C*
162100010601     C                   ENDSR
162200010601     C***
162300010601
162400010601
162500010601
162600010330
162700040119      /TITLE Invio dei dati al punto operativo.
162800040119     C     invio         BEGSR
162900040119     C*
163000040119     C* 1° invio FIVAT
163100040119     C                   reset                   dscmz
163200040119     C                   move      vlrpoi        cmzdst
163300040119     C                   eval      cmzfld = 'FIVATWWR'
163400040119     C                   eval      cmzmbd = vlrhdl
163500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163600040119     C***                if        prmfir = *blanks
163700040119     C                   eval      cmzfla = 'FIVAT00F'
163800040119     C                   eval      cmzmba = 'FIVAT00F'
163900040119     C***                else
164000040119     C***                eval      cmzfla = prmfir
164100040119     C***                eval      cmzmba = prmfir
164200040119     C***                endif
164300040119     C                   eval      cmznrr = *zeros
164400040119     C                   move      §ctrokvt      cmznrr
164500040119     C                   eval      cmzlba = vlrfl1
164600040119     C                   call(e)   'TIS711C'
164700040119     C                   parm                    dscmz
164800040119     C                   parm      *blanks       esito
164900040119     C                   if        %error
165000040119     C                             or cmzerr = '1'
165100040119     C                             or esito  = '1'
165200040119     C                   eval      wrkesito = '3'
165300040119     C                   else
165400040119     C*
165500040119     C* 2° invio FIVAB
165600040119     C                   reset                   dscmz
165700040119     C                   move      vlrpoi        cmzdst
165800040119     C                   eval      cmzfld = vlrfou
165900040119     C                   eval      cmzmbd = vlrhdl
166000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166100040119     C***                if        prmfir = *blanks
166200040119     C                   eval      cmzfla = 'FIVAB00F'
166300040119     C                   eval      cmzmba = 'FIVAB00F'
166400040119     C***                else
166500040119     C***                eval      cmzfla = prmfir
166600040119     C***                eval      cmzmba = prmfir
166700040119     C***                endif
166800040119     C                   eval      cmznrr = *zeros
166900040119     C                   move      §ctrokvb      cmznrr
167000040119     C                   eval      cmzlba = vlrfl1
167100040119     C                   call(e)   'TIS711C'
167200040119     C                   parm                    dscmz
167300040119     C                   parm      *blanks       esito
167400040119     C                   if        %error
167500040119     C                             or cmzerr = '1'
167600040119     C                             or esito  = '1'
167700040119     C                   eval      wrkesito = '3'
167800040119     C                   endif
167900040119     C                   endif
168000040119     C*
168100040119     C                   ENDSR
168200040119     C***
168300010601
168400010601
168500010601
168600010601
168700010601      /TITLE Invio dei dati al punto operativo.
168800010601     C     opeini        BEGSR
168900010601     C*
169000010601     C* Inizializzo flag e contatori operativi
169100010601     C                   movel     '0'           flgGiro           1
169200010601     C                   movel     '0'           flgMulti          1
169300010601     C                   movel     '1'           flgStato          1
169400010615     C                   movel     '0'           flgOk             1
169500010601     C                   z-add     *zeros        cntNonEl         10 0
169600010601     C                   z-add     *zeros        depfil            3 0
169700010601     C                   z-add     *zeros        invfil            3 0
169800010601     C*
169900010601     C                   ENDSR
170000010601     C***
170100070326
170200070326
170300070326
170400070326
170500080916     C     *pssr         BEGSR
170600070329     C*
170700080916     C                   if        %open(tivin00r)
170800080916     C                   close     tivin00r
170900080916     C                   endif
171000080916     C                   if        %open(fivabwwr)
171100080916     C                   close     fivabwwr
171200080916     C                   endif
171300080916     C                   if        %open(fivatwwr)
171400080916     C                   close     fivatwwr
171500080916     C                   endif
171600070326     C*
171700070326     C* Effettuo la chiamata al CLLE preposto
171800080916     C                   call(e)   'TITVVTC'
171900080916     C                   parm                    parccm
172000080916     C                   parm                    parmbr
172100080916     C                   parm      '2'           paropz
172200070326     C*
172300080916     C                   eval      wrkesito = '2'
172400070404     C*
172500080916     C                   seton                                        LR
172600070326     C*
172700080916     C                   ENDSR     '*CANCL'
172800070326     C***
172900070326
173000070326
173100010330
173200010330
173300000613     C     *inzsr        BEGSR
173400990910     C*
173500990910     C     *entry        plist
173600990920     C                   parm                    tivlrds
173700990921     C                   parm      wrkesito      esito
173800000724     C                   parm                    prmlit
173900000710     C                   parm                    prmfir
174000010330     C*
174100010330     C* CALCOLA LA DATA CORRENTE
174200010330     C                   time                    wn14             14 0
174300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
174400010330     C                   z-add     wn8           g08dat
174500010330     C                   z-add     *zeros        g08inv
174600010330     C                   movel     '0'           g08err
174700010330     C                   call      'XSRDA8'
174800010330     C                   parm                    wlbda8
174900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
175000000613     C*
175100000613     C                   ENDSR
175200000613     C***
175300990908
175400081124     Otitv1k6p  E            riepilogo         2
175500990915     O                                              'Upload via Internet'
175600990915     O                                           +1 'Traduzione TIVIN00R -'
175700021025     O                                           +1 'FIVABWWR'
175800990915     O                                           +1 'Testate bolle'
175900990915     O          E            riepilogo   2
176000990915     O                       wrkdata
176100990915     O                       wrkora              +1
176200990915     O                       procname            +1
176300990915     O          E            riepilogo   2
176400990915     O                                              'Cliente..................:'
176500010605     O                       VABCCM        z     +1
176600990915     O          E            riepilogo   2
176700990920     O                                              'Riferimento Strategi.....:'
176800990920     O                       vlrhdl              +1
176900990915     O          E            riepilogo   2
177000990915     O                                              'Giusti...................:'
177100040119     O                       §CTROKVB      2   +  1
177200990915     O          E            riepilogo   2
177300990915     O                                              'Sbagliati e corretti.....:'
177400971022     O                       §CTRMO        2   +  1
177500990915     O          E            riepilogo   2
177600990915     O                                              'Sbagliati e scartati.....:'
177700971022     O                       §CTRNO        2   +  1
177800000613
177900081124     Otitv1k6ps E            testdett          2
178000000613     O                                              'Upload via Internet'
178100000613     O                                           +1 'Traduzione TIVIN00R -'
178200021025     O                                           +1 'FIVABWWR'
178300000613     O                                           +1 'Testate bolle'
178400000616     O          E            testdett    3
178500000613     O                                           +2 'N° rec'
178600000613     O                                           +3 'Anteprima contenuto'
178700000616     O          E            rigadett    2
178800000613     O                       rrnum               +2
178900000621     O                       recko               +3
179000000616     O          E            findett     2
179100000613     O                       wrkdata
179200000613     O                       wrkora              +1
179300000613     O                       procname            +1
179400000616     O          E            findett     2
179500000613     O                                              'Cliente..................:'
179600010605     O                       VABCCM        z     +1
179700000616     O          E            findett     2
179800000613     O                                              'Riferimento Strategi.....:'
179900000613     O                       vlrhdl              +1
180000000616     O          E            findett     2
180100000613     O                                              'Giusti...................:'
180200040119     O                       §CTROKVB      2   +  1
180300000616     O          E            findett     2
180400000613     O                                              'Sbagliati e corretti.....:'
180500000613     O                       §CTRMO        2   +  1
180600000616     O          E            findett     2
180700000613     O                                              'Sbagliati e scartati.....:'
180800000613     O                       §CTRNO        2   +  1
180900000613** CMD - COMANDI CL
181000081124OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1K6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
181100081124OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1K6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
181200081124DLTOVR FILE(TITV1K6P TITV1K6PS) LVL(*)
181300010606
181400000613
181500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181600081124W_VABRMN;VABRSD;VABIND;VABLOD;VABCAD;VABPRD;VABNCL;VABPKB;VATNOT_E;
181700081124FILL001;VABCAS;VABVCA
181800080715
