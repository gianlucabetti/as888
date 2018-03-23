000100080915      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080915     FEDIVABwr  O    E             DISK    usropn
000700080915     FEDIVATwr  O    E             DISK    usropn
000800080915     Ftitv1i8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080915     F                                     infsr(*pssr)
001100080915     Ftitv1i8ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300080915     F                                     infsr(*pssr)
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
004000040510     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400080125     D wFlgCAS         s              1    INZ(*blanks)
004500080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
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
005800080125     D wVATNOT_A       s                   LIKE(VATNOT)
005900080125     D wVATNOT_B       s                   LIKE(VATNOT)
006000080125     D wVATNOT_E       s                   LIKE(VATNOT)
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
008100080617     D*------------------
008200080617     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
008300080617     D*------------------
008400080617     D PiStr           S            100A   INZ
008500080617     D PiDecChr        S              1A   INZ
008600080617     D PiVal           S             63S30 INZ
008700080617     D PiInt           S               N   INZ
008800080617     D PiNum           S               N   INZ
008900080617
009000080617     D*------------------
009100080617     D* LINKING A DEFINIZIONI ESTERNE
009200080617     D*------------------
009300080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009400010330
009500010330
009600010330
009700990915     C                   time                    wrkdata
009800990915     C                   time                    wrkora
009900000913     C                   reset                   rrnum
010000990921     C                   reset                   esito
010100990921     C                   reset                   wrkesito
010200010601     C*
010300010601     C                   exsr      opeini
010400010605     C                   exsr      rwvab
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011200010601     C*--------------------------------------------------------
011300010601     C     PREELA        BEGSR
011400010601     C*
011500010601     C* SE OCCORRE SPEDIRE IN FILIALE
011600010601     C                   if        invfil <> *zeros and
011700010601     C                             flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100000724     C*
012200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012400010601     C     invfil        chain     azorg01l
012500000724     C                   if        %found
012600000724     C                   movel(p)  CMD(1)        depcmd
012700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012800000724     C*
012900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013000011113     C                   Z-ADD     150           LENGH            15 5
013100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200000724     C                   PARM                    depcmd
013300000724     C                   PARM                    LENGH
013400000724     C*
013500000724     C                   endif
013600000616     C*
013700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013800000616     C                   movel(p)  CMD(2)        depcmd
013900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014000000616     C*
014100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014200011113     C                   Z-ADD     150           LENGH            15 5
014300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014400000616     C                   PARM                    depcmd
014500000616     C                   PARM                    LENGH
014600000613     C*
014700080915     C                   if        not %open(titv1i8ps)
014800080915     C                   open      titv1i8ps
014900070329     C                   except    testdett
015000000613     C                   endif
015100010601     C*
015200010601     C                   endif
015300010601     C*
015400010601     C                   ENDSR
015500010601     C***
015600010601
015700010601
015800010601
015900010601
016000010601     C*--------------------------------------------------------
016100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016200010601     C*--------------------------------------------------------
016300010601     C     ENDELA        BEGSR
016400990915     C*
016500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016600040119     C*
016700040119     C* Effettuo la chiamata al CLLE preposto
016800080915     C                   call(e)   'TITVEVTC'
016900040510     C                   parm                    parccm
017000040119     C                   parm                    parmbr
017100040119     C                   parm      '2'           paropz
017200050201     C*
017300050201     C* Effettuo lancio TISI95 solo x chiusura
017400050201     C                   CLEAR                   TISI95DS
017500050201     C                   EVAL      I95TLA = 'C'
017600050201     C                   CALL      'TISI95R'
017700050201     C                   PARM                    TISI95DS
017800000613     C*
017900080915     C                   if        %open(titv1i8ps)
018000000616     C                   except    findett
018100080915     C                   close     titv1i8ps
018200000613     C                   endif
018300000616     C*
018400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018500011113     C                   Z-ADD     150           LENGH            15 5
018600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018700000616     C                   PARM                    CMD(3)
018800000616     C                   PARM                    LENGH
018900000616     C*
019000010601     C                   ENDSR
019100010601     C***
019200010601
019300010601
019400010601
019500000613
019600000613     C*--------------------------------------------------------
019700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019800000613     C*--------------------------------------------------------
019900000621     C     STPR          BEGSR
020000000613     C*
020100080915     C                   if        not %open(titv1i8p)
020200080915     C   50              open      titv1i8p
020300990915     C                   endif
020400990915     C*
020500030715     C   50              except    riepilogo
020600990915     C*
020700080915     C                   if        %open(titv1i8p)
020800080915     C   50              close     titv1i8p
020900990914     C                   endif
021000000613     C*
021100000613     C                   ENDSR
021200000613     C***
021300000613
021400010601
021500010601
021600010330     C*--------------------------------------------------------
021700080915     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
021800010330     C*--------------------------------------------------------
021900010605     C     RWVAB         BEGSR
022000010330     C*
022100010330     C                   if        not %open(tivin00r)
022200010330     C                   open      tivin00r
022300010330     C                   endif
022400080915     C                   if        not %open(edivabwr)
022500080915     C                   open      edivabwr
022600010330     C                   endif
022700040119     C*
022800080915     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
022900040119     C                   exsr      prevat
023000040119     C*
023100040119     C                   if        chkcall = '0'
023200040119     C*
023300080915     C                   if        not %open(edivatwr)
023400080915     C                   open      edivatwr
023500040119     C                   endif
023600010330     C*
023700040119     C                   clear                   §CTROKVB          7 0
023800040119     C                   clear                   §CTROKVT          7 0
023900040119     C                   clear                   §CTRMO            7 0
024000040119     C                   clear                   §CTRNO            7 0
024100070823     C*
024200070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024300070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024400080915     C                   exsr      inzvar
024500080915     C                   exsr      impvab
024600010330     C*
024700030822     C                   do        *HIVAL
024800010330     C*
024900030822     C                   read      tivin00r                               70
025000010618     C*
025100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025200010618     C                   if        vinflg = '1'
025300010618     C                   eval      flgOk = '1'
025400010618     C                   endif
025500040510     C*
025600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025700040510     C                   z-add     *zeros        wLenVINDTA        4 0
025800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
025900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026200040510     C                   z-add     1             wFlgVINDTA
026300040510     C                   leave
026400040510     C                   endif
026500040510     C                   enddo
026600080923     C*
026700080915     C* Aggiungo un byte a '*' all'inizio d ogni record
026800080915     C                   if        %subst(vindta:1:1) = CharCSV
026900080915     C                   eval      vindta  = '*' + %trim(vindta)
027000080915     C                   endif
027100080915     C*
027200080923     C* Forzatura controllo x scarto record nn "interessanti"
027300080915     C                   if        %subst(vindta:4:4) <> '2008'
027400080915     C                   z-add     *zeros        wFlgVINDTA
027500080915     C                   endif
027600010618     C*
027700040510     C                   if        vindta > *blanks AND
027800040510     C                             wFlgVINDTA = 1
027900010330     C                   add       1             rrnum
028000010330     C*
028100010601     C                   if        *in70 = *off and
028200010330     C                             (vinflg = *blanks
028300010330     C                              or vinflg = '0'
028400010330     C                              or vinflg = '2')
028500010330     C*
028600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028700010711     C                   if        vinflg = *blanks or vinflg = '0'
028800010711     C                   clear                   vinmsg
028900010711     C                   endif
029000070925     C*
029100080915     C                   exsr      inzvar
029200010605     C                   exsr      impvab
029300010601     C*
029400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029500010605     C                   eval      depfil = VABLNP
029600010601     C                   exsr      repfil
029700010601     C                   if        depfil = invfil
029800021025     C                   if        vlrpoi = 999
029900030822     C                   move(P)   invfil        VABFGS
030000021025     C                   else
030100030822     C                   move(P)   vlrpoi        VABFGS
030200021025     C                   endif
030300010601     C*
030400010601     C                   exsr      PREELA
030500010601     C*
030600010604     C* Ebbene...
030700010604     C*
030800040119     C  N31              add       1             §CTROKVB
030900040119     C   32              add       1             §CTRMO
031000040119     C   31              add       1             §CTRNO
031100030822     C                   if        wGiro = 2
031200040510     C* Scrivo sempre il VAT
031300040510     C  N31              exsr      wrivat
031400040510     C* Scrivo il VAB solo a rottura d spedizione
031500040119     C  N31              exsr      wrivab
031600040119     C                   endif
031700010604     C*
031800010604     C                   if        *in31 = *off and
031900010604     C                             *in32 = *off
032000010604     C                   eval      vinflg = '1'
032100010604     C                   else
032200010604     C                   eval      recko = vindta
032300010604     C                   except    rigadett
032400010604     C                   eval      vinflg = '2'
032500010604     C                   endif
032600010604     C                   endif
032700010601     C*
032800010604     C                   endif
032900010604     C*
033000010330     C                   else
033100010330     C                   eval      vinflg = '1'
033200010330     C                   endif
033300010601     C*
033400010601     C  N70              update    tivin000
033500010330     C*
033600030822     C  N70              enddo
033700040119     C*
033800040119     C                   endif                                                  (endif - chkcall)
033900010601     C*
034000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034100010601     C                   if        cntNonEl = *zeros or
034200010601     C                             flgMulti = '0'
034300010330     C* Se non ci sono record con errori ...
034400010601     C                   if        §ctrno = 0 and
034500010604     C                             §ctrmo = 0 and
034600010601     C                             flgStato <> '2'
034700010330     C* ... restituisco esito OK.
034800010330     C                   eval      wrkesito = '0'
034900010330     C                   else
035000040119     C                   if        §ctrokvb > 0
035100010330     C                   eval      wrkesito = '1'
035200010330     C                   else
035300010615     C                   if        flgOk = '0'
035400010615     C                   eval      wrkesito = '2'
035500010615     C                   else
035600010615     C                   eval      wrkesito = '6'
035700010615     C                   endif
035800010330     C                   endif
035900010330     C                   endif
036000010601     C                   else
036100010601     C                   eval      wrkesito = '9'
036200010601     C                   endif
036300010330     C*
036400010330     C                   if        %open(tivin00r)
036500010330     C                   close     tivin00r
036600010330     C                   endif
036700080915     C                   if        %open(edivabwr)
036800080915     C                   close     edivabwr
036900010330     C                   endif
037000080915     C                   if        %open(edivatwr)
037100080915     C                   close     edivatwr
037200040512     C                   endif
037300010601     C*
037400010601     C                   if        vlrpoi <> 999
037500010601     C                   eval      invfil = vlrpoi
037600010601     C                   endif
037700010330     C*
037800040119     C                   if        §ctrokvb > 0
037900010601     C                             and invfil > *zeros
038000010330     C                   exsr      invio
038100010330     C                   endif
038200010601     C*
038300010618     C                   if        flgGiro = '1'
038400010601     C                   exsr      endela
038500010618     C                   endif
038600010330     C*
038700010330     C                   ENDSR
038800010330     C***
038900040119
039000040119
039100040119
039200040119     C*----------------------------------------------------*
039300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039400040119     C*----------------------------------------------------*
039500040510     C     WRIVAB        BEGSR
039600080917     C*
039700080917     C* Forzatura x anomalia cliente => se peso a zero => ignorare completamente record
039800080917     C                   if        VABPKB > *zeros
039900080923 xxx C                   eval      VABPKB = 7,3
040000080923 xxx C                   endif
040100080915     C*
040200080915     C                   eval      VABCMR = 'THUN "NEW"'
040300080915     C                   eval      VABCNT = 1
040400080915     C                   eval      VABDCM = DATCOR
040500080915     C                   eval      VABDTS = DATCOR
040600080915     C                   eval      VABHMS = ORACOR
040700040119     C*
040800060331     C* Gestisco l'eventuale rottura x numero spedizione
040900070301     C                   if        wVATNOT_E <> *blanks
041000080125     C                   if        VABCTM = *blanks
041100080125     C                   eval      VABCTM = '7Q'
041200080125     C                   endif
041300070301     C                   endif
041400080915     C                   write     edivab00                                     => scarico il VAB
041500040119     C*
041600080923 xxx C***                endif
041700080917     C*
041800040119     C                   ENDSR
041900040119
042000040119
042100040119
042200040119     C*----------------------------------------------------*
042300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042400040119     C*----------------------------------------------------*
042500040510     C     WRIVAT        BEGSR
042600060512     C*
042700060512     C* Inizializzo i campi chiave
042800060512     C                   eval      VATFGS = VABFGS
042900060512     C                   eval      VATAAS = VABAAS
043000060512     C                   eval      VATCCM = VABCCM
043100070823     C                   eval      VATNRS = VABNRS
043200060512     C                   eval      VATNSP = VABNSP
043300060512     C                   eval      VATLNP = VABLNP
043400080915     C*
043500080915     C                   eval      VATCMR = 'THUN "NEW"'
043600080915     C                   eval      VATCNT = 1
043700060331     C*
043800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043900060331     C                   if        wVATNOT_A <> *blanks
044000060331     C                   eval      VATNOT = wVATNOT_A
044100060331     C                   eval      VATTRC = 'A'
044200080915     C                   write     EDIVAT00
044300060331     C                   add       1             §CTROKVT
044400060331     C                   endif
044500060331     C*
044600060331     C                   if        wVATNOT_B <> *blanks
044700060331     C                   eval      VATNOT = wVATNOT_B
044800060331     C                   eval      VATTRC = 'B'
044900080915     C                   write     EDIVAT00
045000060331     C                   add       1             §CTROKVT
045100060331     C                   endif
045200060331     C*
045300080625     C                   if        wVATNOT_E <> *blanks
045400060331     C                   eval      VATNOT = wVATNOT_E
045500060331     C                   eval      VATTRC = 'E'
045600080915     C                   write     EDIVAT00
045700080716     C                   add       1             §CTROKVT
045800080625     C                   endif
045900040119     C*
046000040119     C                   ENDSR
046100040119
046200040119
046300040119
046400040119     C*----------------------------------------------------*
046500080915     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
046600040119     C*----------------------------------------------------*
046700040119     C     PREVAT        BEGSR
046800040119     C*
046900080915     C* Compongo il nome del membro da dare al EDIVATWR
047000040119     C                   eval      parmbr = vlrhdl
047100040119     C                   movel     'M'           parmbr
047200040510     C                   eval      parccm = vlrksc
047300040119     C                   eval      paropz = '1'
047400040119     C* Effettuo la chiamata al CLLE preposto
047500080915     C                   call(e)   'TITVEVTC'
047600040119     C                   parm                    parccm
047700040119     C                   parm                    parmbr
047800040119     C                   parm                    paropz
047900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048000040119     C                   if        %error
048100040119     C                   movel     '1'           chkcall
048200040119     C                   else
048300040119     C                   movel     '0'           chkcall
048400040119     C                   endif
048500040119     C*
048600040119     C                   ENDSR
048700010601
048800010601
048900010601
049000010330     C*----------------------------------------------------*
049100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049200010330     C*----------------------------------------------------*
049300040119     C     INZVAR        BEGSR
049400010330     C*
049500030822     C* Inizializzo variabili di wrk
049600020204     C                   Z-ADD     *zeros        Num5_0            5 0
049700060331     C                   MOVEL     *blanks       wVATNOT_A
049800060331     C                   MOVEL     *blanks       wVATNOT_B
049900060331     C                   MOVEL     *blanks       wVATNOT_E
050000080617     C                   EVAL      wFlgCAS = *blanks
050100030822     C*
050200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050300080915     C                   CLEAR                   EDIVAB00
050400080915     C                   CLEAR                   EDIVAT00
050500030822     C                   CLEAR                   SkSplitCSV
050600030822     C                   EVAL      i = 1
050700030822     C                   EVAL      posDa = *zeros
050800030822     C                   EVAL      posA  = *zeros
050900030822     C*
051000030822     C* Reimposto i valori di default
051100030822     C                   EXSR      DEFCAM
051200030822     C*
051300010330     C                   ENDSR
051400010330     C*----------------------------------------------------*
051500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051600010330     C*----------------------------------------------------*
051700010330     C     DEFCAM        BEGSR
051800030822     C*
051900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052000030715     C* e delimitatore testo.
052100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052400030822     C*
052500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
052700030715     C                   SETON                                        50
052800030715     C                   ELSE
052900030715     C                   SETOFF                                       50
053000030715     C                   ENDIF
053100030822     C*
053200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053300030822     C                   EVAL      posDaDft = 1
053400030822     C                   EVAL      posADft  = 0
053500030822     C                   EVAL      wGiroDft = 0
053600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053700030822     C                             posDaDft > 0
053800030822     C*
053900030822     C* Gestisco il 1° giro
054000030822     C                   IF        wGiroDft = 0
054100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054300030822     C* Incremento il contatore dei "giri"
054400030822     C                   EVAL      wGiroDft = 1
054500030822     C                   ELSE
054600030822     C                   EVAL      posDaDft = posADft
054700030822     C                   ENDIF
054800030822     C* Eseguo lo scan x trovare la fine del campo corrente
054900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055000030822     C*
055100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055300030822     C                   IF        posDaDft > 0 AND
055400030822     C                             posADft  > 0
055500030822     C* NCL
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'NCL'
056000030822     C                   EVAL      PiStr=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   EXSR      CHKNUM
056400030822     C                   IF        PiInt=*on
056500030822     C                   Z-ADD     PiVal         VABNCL
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* CCM
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'CCM'
057300030822     C                   EVAL      PiStr=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   EXSR      CHKNUM
057700030822     C                   IF        PiInt=*on
057800030822     C                   Z-ADD     PiVal         VABCCM
057900030822     C                   ENDIF
058000030822     C                   ENDIF
058100030822     C* LNP
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'LNP'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiInt=*on
059100030822     C                   Z-ADD     PiVal         VABLNP
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* NRS
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'NRS'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiInt=*on
060400030822     C                   Z-ADD     PiVal         VABNRS
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* CTR
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'CTR'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiInt=*on
061700030822     C                   Z-ADD     PiVal         VABCTR
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* PKB
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'PKB'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiNum=*on
063000030822     C                   Z-ADD     PiVal         VABPKB
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* VLB
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'VLB'
063800030822     C                   EVAL      PiStr=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   EXSR      CHKNUM
064200030822     C                   IF        PiNum=*on
064300030822     C                   Z-ADD     PiVal         VABVLB
064400030822     C                   ENDIF
064500030822     C                   ENDIF
064600030822     C* QFT
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'QFT'
065100030822     C                   EVAL      PiStr=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   EXSR      CHKNUM
065500030822     C                   IF        PiNum=*on
065600030822     C                   Z-ADD     PiVal         VABQFT
065700030822     C                   ENDIF
065800030822     C                   ENDIF
065900030822     C* CBO
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'CBO'
066400030822     C                   EVAL      VABCBO=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* TSP
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'TSP'
067300030822     C                   EVAL      VABTSP=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* VAS
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'VAS'
068200030822     C                   EVAL      VABVAS=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600030822     C* VCA
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'VCA'
069100030822     C                   EVAL      VABVCA=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500070925     C* TIC
069600070925     C                   IF        %subst(
069700070925     C                             %subst(vlrppt:posDaDft+1:
069800070925     C                             posADft-posDaDft-1):1:3)
069900070925     C                             = 'TIC'
070000070925     C                   EVAL      VABTIC=%trim(%subst(
070100070925     C                             %subst(vlrppt:posDaDft+1:
070200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300070925     C                   ENDIF
070400030822     C* GCA
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'GCA'
070900030822     C                   EVAL      VABGCA=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* CTM
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'CTM'
071800030822     C                   EVAL      VABCTM=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* FFD
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'FFD'
072700030822     C                   EVAL      VABFFD=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* VAD
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'VAD'
073600030822     C                   EVAL      VABVAD=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* GMA
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'GMA'
074500030822     C                   EVAL      VABGMA=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* GGA
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'GGA'
075400030822     C                   EVAL      VABGGA=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* GVA
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'GVA'
076300030822     C                   EVAL      VABGVA=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* TC1
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'TC1'
077200030822     C                   EVAL      VABTC1=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600030822     C* TC2
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'TC2'
078100030822     C                   EVAL      VABTC2=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500040714     C* VATTRC
078600040714     C                   IF        %subst(
078700040714     C                             %subst(vlrppt:posDaDft+1:
078800040714     C                             posADft-posDaDft-1):1:3)
078900040714     C                             = 'TRC'
079000040714     C                   EVAL      VATTRC=%trim(%subst(
079100040714     C                             %subst(vlrppt:posDaDft+1:
079200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300040714     C                   ENDIF
079400030822     C* ...
079500030822     C                   ENDIF
079600030822     C                   ENDDO
079700020204     C*
079800010330     C                   ENDSR
079900010607     C*----------------------------------------------------*
080000080915     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
080100010607     C*----------------------------------------------------*
080200010607     C     IMPVAB        BEGSR
080300010607     C*
080400010607     C                   SETOFF                                       3132
080500030515     C*
080600010607     C                   MOVEL     datcor        VABAAS
080700010607     C                   MOVE      datcor        VABMGS
080800010607     C*
080900030715     C**********
081000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081100030715     C**********
081200030715     C                   IF        wGiro = *zeros
081300080716     C*
081400080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
081500080716     C                   EVAL      depvinDTA = vinDTA
081600040113     C*
081700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081900040510     C                             %trim(intcol(3))<>*blanks
082000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082100040113     C                                      %trim(intcol(3))
082200070823     C     ';':CharCSV   XLATE     vindta        vindta
082300040510     C                   ENDIF
082400080626     C*
082500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082600031201     C     minu:maiu     XLATE     vindta        vindta
082700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082800030715     C                   DOW       posDa <= %len(%trim(vindta))
082900030715     C*
083000030715     C* Gestisco il 1° campo
083100030715     C                   IF        i = 1
083200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083300030715     C                   EVAL      posDa = 1
083400030715     C* Eseguo lo scan x trovare la fine del primo campo
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C* X i campi successivi al 1°
084000030715     C                   ELSE
084100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084200030715     C                   EVAL      posDa = posA + 1
084300030715     C* Eseguo lo scan x trovare la fine del campo corrente
084400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084500030715     C* Gestisco l'ultimo campo
084600030715     C                   IF        posA = *zeros
084700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C                   LEAVE
085200030715     C                   ELSE
085300030715     C* A questo punto "estraggo" il campo corrente
085400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085500030715     C                                                    (posA-posDa))
085600030715     C                   ENDIF
085700030715     C                   ENDIF
085800030715     C* Incremento il contatore d campo
085900030715     C                   EVAL      i = i +1
086000030715     C                   ENDDO
086100030715     C*
086200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086300030715     C* delimitazione testo
086400030715     C                   EVAL      i = 1
086500030715     C                   DOW       i < %elem(SkSplitFLD)
086600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086800030715     C                   EVAL      i = i + 1
086900030715     C                   ENDDO
087000030715     C                   EVAL      wGiro = 1
087100080716     C* Ripristino la 1° riga originale
087200080716     C                   EVAL      vinDTA = depvinDTA
087300030715     C                   ELSE
087400030715     C                   EVAL      wGiro = 2
087500031204     C**********
087600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087700031204     C* d separatore campo
087800031204     C**********
087900031204     C                   EVAL      vindta = %trim(vindta)
088000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088100031204     C                   EVAL      vindta = %subst(vindta:2)
088200031204     C                   ENDDO
088300031204     C*
088400031204     C                   Z-ADD     *zeros        lunghInput        4 0
088500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088900031204     C                   ENDDO
089000030715     C**********
089100030715     C* Effettuo lo split del campo dati d input
089200030715     C**********
089300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089400030715     C                   DOW       posDa <= %len(%trim(vindta))
089500030715     C*
089600030715     C* Gestisco il 1° campo
089700030715     C                   IF        i = 1
089800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089900030715     C                   EVAL      posDa = 1
090000030715     C* Eseguo lo scan x trovare la fine del primo campo
090100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C* X i campi successivi al 1°
090600030715     C                   ELSE
090700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090800030715     C                   EVAL      posDa = posA + 1
090900031204     C* Verifico che nn vi sia il campo nn valorizzato
091000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091100031204     C* Se campo nn valorizzato skippo al prossimo
091200031204     C                   EVAL      SkSplitCSV(i) = *blanks
091300031204     C                   EVAL      posA  = posA + 1
091400031204     C                   ELSE
091500030715     C* Eseguo lo scan x trovare la fine del campo corrente
091600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091700030715     C* Gestisco l'ultimo campo
091800030715     C                   IF        posA = *zeros
091900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092000030715     C* A questo punto "estraggo" il campo corrente
092100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092200030715     C                                                    (posA-posDa))
092300030715     C                   LEAVE
092400030715     C                   ELSE
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C                   ENDIF
092900031204     C                   ENDIF
093000030715     C                   ENDIF
093100030715     C* Incremento il contatore d campo
093200030715     C                   EVAL      i = i +1
093300030715     C                   ENDDO
093400030715     C*
093500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093600030715     C* delimitazione testo
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitCSV)
093900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094100030715     C                   EVAL      i = i + 1
094200030715     C                   ENDDO
094300030715     C*
094400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094500030715     C                   EVAL      i = 1
094600030715     C                   DOW       i < %elem(SkSplitFLD)
094700030715     C*
094800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095000030715     C                             SkSplitCSV(i) <> *blanks
095100030721     C*** ATB
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095300030721     C                   EVAL      VABATB = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030715     C*** RSD
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096000030715     C* ==
096100030715     C                   ENDIF
096200030721     C*** RD2
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030715     C*** IND
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096800030715     C                   EVAL      VABIND = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** LOD
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** PRD
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030721     C*** NZD
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030715     C*** RMA
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** NOT
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** NT2
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** VCA
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** VAS
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** FFD
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030721     C*** GC1
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** GC2
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** TSP
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101700030721     C                   ENDIF
101800040510     C*** CBO
101900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102100040510     C                   ENDIF
102200030721     C*** NAS
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** TIC
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** GCA
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** XCO
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** CTM
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** TCR
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** CTS
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** FTM
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** VAD
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GMA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GGA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** GVA
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** TC1
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** TC2
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** SCL
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** RMO
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** NMO
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*
109100030721     C* Reperisco quindi i campi numerici...
109200040510     C*** NSP
109300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109400040510     C                   EVAL      PiStr=SkSplitCSV(i)
109500040510     C                   EXSR      CHKNUM
109600040510     C                   IF        PiInt=*on
109700040510     C                   Z-ADD     PiVal         VABNSP
109800040510     C                   ELSE
109900040510     C                   SETON                                        31
110000040510     C                   EVAL      VABNSP = *zeros
110100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110200040510     C                             + ' ' + 'VABNSP'
110300040510     C                   ENDIF
110400040510     C                   ENDIF
110500071106     C*** RMN
110600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110700071106     C                   EVAL      PiStr=SkSplitCSV(i)
110800071106     C                   EXSR      CHKNUM
110900071106     C                   IF        PiInt=*on
111000071106     C                   Z-ADD     PiVal         VABRMN
111100071106     C                   ELSE
111200071106     C                   SETON                                        32
111300071106     C                   EVAL      VABRMN = *zeros
111400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111500071106     C                             + ' ' + 'VABRMN'
111600071106     C                   ENDIF
111700071106     C                   ENDIF
111800030721     C*** CAD
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on
112300030721     C                   Z-ADD     PiVal         Num5_0
112400030721     C                   MOVEL(p)  Num5_0        VABCAD
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700030721     C                   EVAL      VABCAD = *zeros
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900030721     C                             + ' ' + 'VABCAD'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** NCL
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on
113700030721     C                   Z-ADD     PiVal         VABNCL
113800030721     C                   ELSE
113900030721     C                   SETON                                        32
114000030721     C                   EVAL      VABNCL = *zeros
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200040122     C                             + ' ' + 'VABNCL'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** PKB
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiNum=*on
115000030721     C                   Z-ADD     PiVal         VABPKB
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABPKB = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABPKB'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** CAS
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116000080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
116100080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
116200080617     C                             %trim(SkSplitCSV(i)) <> '0'
116300080125     C                   EVAL      wFlgCAS = '1'
116400080125     C                   ENDIF
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiNum=*on
116800030721     C                   Z-ADD     PiVal         VABCAS
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      VABCAS = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABCAS'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** CCM
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on      AND
118100030721     C                             PiVal<=9999999 AND
118200030721     C                             PiVal>*zeros
118300030721     C                   Z-ADD     PiVal         VABCCM
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABCCM = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800040510     C                             + ' ' + 'VABCCM'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** LNP
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on    AND
119600030721     C                             PiVal<=999   AND
119700030721     C                             PiVal>*zeros
119800030721     C                   Z-ADD     PiVal         VABLNP
119900030721     C                   ELSE
120000040715     C                   SETON                                        32
120100030721     C                   EVAL      VABLNP = *zeros
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300040510     C                             + ' ' + 'VABLNP'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** NRS
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on AND
121100030721     C                             PiVal<=99
121200030721     C                   Z-ADD     PiVal         VABNRS
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABNRS'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** LNA
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on    AND
122400030721     C                             PiVal<=999   AND
122500030721     C                             PiVal>*zeros
122600030721     C                   Z-ADD     PiVal         VABLNA
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      VABLNA = *zeros
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABLNA'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** CTR
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on  AND
123900030721     C                             PiVal<=999
124000030721     C                   Z-ADD     PiVal         VABCTR
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABCTR'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** IAS
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiNum=*on
125200030721     C                   Z-ADD     PiVal         VABIAS
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABIAS'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** VLB
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiNum=*on
126400030721     C                   Z-ADD     PiVal         VABVLB
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABVLB'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** QFT
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiNum=*on
127600030721     C                   Z-ADD     PiVal         VABQFT
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABQFT'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** NCD
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on      AND
128800030721     C                             PiVal<=9999999
128900030721     C                   Z-ADD     PiVal         VABNCD
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABNCD'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** NCA
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on      AND
130100030721     C                             PiVal<=9999999
130200030721     C                   Z-ADD     PiVal         VABNCA
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABNCA'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** ZNC
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on AND
131400030721     C                             PiVal<=99
131500030721     C                   Z-ADD     PiVal         VABZNC
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABZNC'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** DCR
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on       AND
132700030721     C                             PiVal<=99999999
132800030721     C                   Z-ADD     PiVal         VABDCR
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABDCR'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** HCR
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on   AND
134000030721     C                             PiVal<=9999
134100030721     C                   Z-ADD     PiVal         VABHCR
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABHCR'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** VMD
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiNum=*on
135300030721     C                   Z-ADD     PiVal         VABVMD
135400030721     C                   ELSE
135500030721     C                   SETON                                        32
135600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135700030721     C                             + ' ' + 'VABVMD'
135800030721     C                   ENDIF
135900030721     C                   ENDIF
136000030721     C*** ANT
136100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136200030721     C                   EVAL      PiStr=SkSplitCSV(i)
136300030721     C                   EXSR      CHKNUM
136400030721     C                   IF        PiInt=*on        AND
136500030721     C                             PiVal<=999999999
136600030721     C                   Z-ADD     PiVal         VABANT
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABANT'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** CMO
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiInt=*on
137800030721     C                   Z-ADD     PiVal         Num5_0
137900030721     C                   MOVEL(p)  Num5_0        VABCMO
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      VABCMO = *zeros
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABCMO'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700040510     C*
138800080915     C* Reperisco i campi del EDIVAT
138900040510     C*** VATNOT
139000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139200040510     C                   ENDIF
139300040510     C*** VATTRC
139400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139600040510     C                   ENDIF
139700060331     C*** VATNOT_A
139800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140000060331     C                   ENDIF
140100060331     C*** VATNOT_B
140200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140400060331     C                   ENDIF
140500060331     C*** VATNOT_E
140600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140800060331     C                   ENDIF
140900070823     C*
141000071029     C***  ===>  Gestione campi "particolari" dentro ciclo
141100071204     C*
141200080915     C*** RMA/RMN/NSP
141300080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
141400080915     C                   EVAL      VABRMA=SkSplitCSV(i)
141500080716     C                   EVAL      PiStr=SkSplitCSV(i)
141600071204     C                   EXSR      CHKNUM
141700071204     C                   IF        PiInt=*on
141800080915     C***                Z-ADD     PiVal         VABNSP
141900080915     C                   Z-ADD     PiVal         VABRMN
142000071204     C                   ELSE
142100080915     C                   SETON                                        31
142200080915     C                   EVAL      VABRMN = 1
142300071204     C                   EVAL      vinmsg = %trimr(vinmsg)
142400080915     C***                          + ' ' + 'VABNSP VATNSP VABRMN'
142500080915     C                             + ' ' + 'VABRMN'
142600071204     C                   ENDIF
142700071204     C                   ENDIF
142800080915     C*** PKB
142900080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
143000080917     C                   IF        %trim(SkSplitCSV(i)) <> *zeros
143100080915     C                   EVAL      PiStr=SkSplitCSV(i)
143200080915     C                   EXSR      CHKNUM
143300080915     C                   IF        PiInt=*on
143400080915     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg.
143500080915     C                   Z-ADD     PiVal         VABPKB
143600080915     C                   ELSE
143700080915     C                   SETON                                        32
143800080915     C                   EVAL      vinmsg = %trimr(vinmsg)
143900080915     C                             + ' ' + 'VABPKB'
144000080917     C                   ENDIF
144100080915     C                   ENDIF
144200080915     C                   ENDIF
144300080915     C*** VLB
144400080915     C***                IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
144500080915     C***                EVAL      PiStr=SkSplitCSV(i)
144600080915     C***                EXSR      CHKNUM
144700080915     C***                IF        PiInt=*on
144800080915     C***                EVAL      PiVal = PiVal / 1000000000                   * da cm3 a m3
144900080915     C***                Z-ADD     PiVal         VABVLB
145000080915     C***                ELSE
145100080915     C***                SETON                                        32
145200080915     C***                EVAL      vinmsg = %trimr(vinmsg)
145300080915     C***                          + ' ' + 'VABVLB'
145400080915     C***                ENDIF
145500080915     C***                ENDIF
145600080915     C*** W_VATNOT_E
145700080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
145800080915     C                   EVAL      wVATNOT_E = %trim(%subst(SkSplitCSV(i):5:35))
145900080716     C                   ENDIF
146000070925     C*
146100071029     C***  <===  -----------------------------------------
146200040510     C*
146300030715     C                   ENDIF
146400030715     C                   EVAL      i = i + 1
146500030715     C                   ENDDO
146600070925     C*
146700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146800080125     C*
146900070925     C*
147000071029     C***  <===  -----------------------------------------
147100070925     C*
147200070925     C* Considerazioni finali su CBO/CAS
147300080125     C                   IF        wFlgCAS = '1'
147400070925     C                   IF        VABCBO = '1'
147500070925     C                   EVAL      VABCBO = '4'
147600070925     C                   ENDIF
147700070925     C                   IF        VABCBO = '2'
147800070925     C                   EVAL      VABCBO = '6'
147900070925     C                   ENDIF
148000070925     C                   ENDIF
148100070925     C***  <===  ----------------------------
148200040510     C*
148300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148400040510     C                   IF        VABNSP = *zeros
148500040510     C                   SETON                                        31
148600040510     C                   ENDIF
148700040715     C*
148800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148900040715     C                   IF        VABLNP = *zeros
149000040715     C                   SETON                                        31
149100040715     C                   ENDIF
149200040510     C*
149300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149400040510     C                   IF        VABRMN = *zeros
149500040510     C                   Z-ADD     VABNSP        VABRMN
149600040510     C                   ENDIF
149700040510     C*
149800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149900040510     C                   IF        VABRMA = *blanks
150000040714     C                   MOVEL     VABRMN        VABRMA
150100040510     C                   ENDIF
150200040128     C*
150300040510     C* Se provincia nn valorizzata la reperisco
150400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150500040510     C                   IF        VABPRD = *blanks
150600040128     C                   CLEAR                   TISI95DS
150700040128     C                   EVAL      I95TCN = '3'
150800040128     C                   Z-ADD     datcor        I95DAT
150900040128     C                   EVAL      I95NAR = VABNZD
151000040128     C                   EVAL      I95CAP = VABCAD
151100040128     C                   EVAL      I95LOC = VABLOD
151200040128     C                   CALL      'TISI95R'
151300040128     C                   PARM                    TISI95DS
151400040128     C                   EVAL      VABPRD = O95PRV
151500040510     C                   ENDIF
151600020204     C*
151700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151800020204     C                   EXSR      CHKIMPDIV
151900030715     C                   ENDIF
152000020204     C*
152100010607     C                   ENDSR
152200010607     C*----------------------------------------------------*
152300010601
152400020204
152500020204     C*----------------------------------------------------*
152600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152700020204     C*----------------------------------------------------*
152800020204     C     CHKIMPDIV     BEGSR
152900020204     C*
153000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153100020204     C                   Z-ADD     *zeros        wrkDec            9 9
153200020204     C*
153300020204     C* Come prima cosa effettuo considerazioni sulla divisa
153400020204     C                   IF        vabIAS > *zeros
153500020204     C                   IF        vabVAS <> 'EUR'
153600020204     C                   EVAL      vabVAS =  'ITL'
153700020204     C                   ENDIF
153800020204     C                   ENDIF
153900020204     C*
154000020204     C                   IF        vabCAS > *zeros
154100020204     C                   IF        vabVCA <> 'EUR'
154200020204     C                   EVAL      vabVCA =  'ITL'
154300020204     C                   ENDIF
154400020204     C                   ENDIF
154500020204     C*
154600020204     C                   IF        vabVMD > *zeros
154700020204     C                   IF        vabVAD <> 'EUR'
154800020204     C                   EVAL      vabVAD =  'ITL'
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155300020204     C                   Z-ADD     vabIAS        wrkDec
155400020204     C                   IF        wrkDec > *zeros
155500020204     C                   IF        vabVAS = 'ITL'
155600020204     C                   EVAL      vabIAS = *zeros
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156100020204     C                   Z-ADD     vabCAS        wrkDec
156200020204     C                   IF        wrkDec > *zeros
156300020204     C                   IF        vabVCA = 'ITL'
156400020204     C                   EVAL      vabCAS = *zeros
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156900020204     C                   Z-ADD     vabVMD        wrkDec
157000020204     C                   IF        wrkDec > *zeros
157100020204     C                   IF        vabVAD = 'ITL'
157200020204     C                   EVAL      vabVMD = *zeros
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C                   ENDSR
157700020204     C***
157800080617
157900080617
158000080617
158100080617     C*----------------------------------------------------*
158200080617     C*  CONTROLLO NUMERICITA' CAMPI
158300080617     C*----------------------------------------------------*
158400080617     C     CHKNUM        BEGSR
158500080617     C*
158600080617     C                   IF        PiDecChr = *blanks
158700080617     C                   EVAL      PiDecChr = CharNUM
158800080617     C                   ENDIF
158900080617     C*
159000080617     C                   callp     UBISNUM_Check(PiStr
159100080617     C                                          :PiDecChr
159200080617     C                                          :PiVal
159300080617     C                                          :PiNum
159400080617     C                                          :PiInt)
159500080617     C*
159600080617     C                   ENDSR
159700080617     C***
159800010330
159900010601
160000010601
160100010601
160200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160300010601     C     repfil        BEGSR
160400010601     C*
160500010601     C                   if        invfil = *zeros and
160600010601     C                             depfil > *zeros and
160700010629     C                             (vinflg = *blanks or
160800010629     C                              vinflg = *zeros)
160900010601     C
161000010601     C                   eval      invfil = depfil
161100010601     C                   endif
161200010601     C*
161300010601     C                   if        depfil <> invfil and
161400010601     C                             invfil > *zeros
161500010601     C                   eval      flgMulti = '1'
161600010601     C                   if        vinflg = *blanks
161700010601     C                   add       1             cntNonEl
161800010601     C                   endif
161900010601     C                   endif
162000010601     C*
162100010601     C                   if        vinflg = '2'
162200010601     C                   eval      flgStato = '2'
162300010601     C                   endif
162400010601     C*
162500010601     C                   ENDSR
162600010601     C***
162700010601
162800010601
162900010601
163000010330
163100040119      /TITLE Invio dei dati al punto operativo.
163200040119     C     invio         BEGSR
163300040119     C*
163400080915     C* 1° invio EDIVAT
163500040119     C                   reset                   dscmz
163600040119     C                   move      vlrpoi        cmzdst
163700080915     C                   eval      cmzfld = 'EDIVATWR'
163800040119     C                   eval      cmzmbd = vlrhdl
163900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164000040119     C***                if        prmfir = *blanks
164100080915     C                   eval      cmzfla = 'EDIVAT0F'
164200080915     C                   eval      cmzmba = 'EDIVAT0F'
164300040119     C***                else
164400040119     C***                eval      cmzfla = prmfir
164500040119     C***                eval      cmzmba = prmfir
164600040119     C***                endif
164700040119     C                   eval      cmznrr = *zeros
164800040119     C                   move      §ctrokvt      cmznrr
164900040119     C                   eval      cmzlba = vlrfl1
165000040119     C                   call(e)   'TIS711C'
165100040119     C                   parm                    dscmz
165200040119     C                   parm      *blanks       esito
165300040119     C                   if        %error
165400040119     C                             or cmzerr = '1'
165500040119     C                             or esito  = '1'
165600040119     C                   eval      wrkesito = '3'
165700040119     C                   else
165800040119     C*
165900080915     C* 2° invio EDIVAB
166000040119     C                   reset                   dscmz
166100040119     C                   move      vlrpoi        cmzdst
166200040119     C                   eval      cmzfld = vlrfou
166300040119     C                   eval      cmzmbd = vlrhdl
166400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166500040119     C***                if        prmfir = *blanks
166600080915     C                   eval      cmzfla = 'EDIVAB0F'
166700080915     C                   eval      cmzmba = 'EDIVAB0F'
166800040119     C***                else
166900040119     C***                eval      cmzfla = prmfir
167000040119     C***                eval      cmzmba = prmfir
167100040119     C***                endif
167200040119     C                   eval      cmznrr = *zeros
167300040119     C                   move      §ctrokvb      cmznrr
167400040119     C                   eval      cmzlba = vlrfl1
167500040119     C                   call(e)   'TIS711C'
167600040119     C                   parm                    dscmz
167700040119     C                   parm      *blanks       esito
167800040119     C                   if        %error
167900040119     C                             or cmzerr = '1'
168000040119     C                             or esito  = '1'
168100040119     C                   eval      wrkesito = '3'
168200040119     C                   endif
168300040119     C                   endif
168400040119     C*
168500040119     C                   ENDSR
168600040119     C***
168700010601
168800010601
168900010601
169000010601
169100010601      /TITLE Invio dei dati al punto operativo.
169200010601     C     opeini        BEGSR
169300010601     C*
169400010601     C* Inizializzo flag e contatori operativi
169500010601     C                   movel     '0'           flgGiro           1
169600010601     C                   movel     '0'           flgMulti          1
169700010601     C                   movel     '1'           flgStato          1
169800010615     C                   movel     '0'           flgOk             1
169900010601     C                   z-add     *zeros        cntNonEl         10 0
170000010601     C                   z-add     *zeros        depfil            3 0
170100010601     C                   z-add     *zeros        invfil            3 0
170200010601     C*
170300010601     C                   ENDSR
170400010601     C***
170500070326
170600070326
170700070326
170800070326
170900080915     C     *pssr         BEGSR
171000070329     C*
171100080915     C                   if        %open(tivin00r)
171200080915     C                   close     tivin00r
171300080915     C                   endif
171400080915     C                   if        %open(edivabwr)
171500080915     C                   close     edivabwr
171600080915     C                   endif
171700080915     C                   if        %open(edivatwr)
171800080915     C                   close     edivatwr
171900080915     C                   endif
172000070326     C*
172100070326     C* Effettuo la chiamata al CLLE preposto
172200080915     C                   call(e)   'TITVEVTC'
172300080915     C                   parm                    parccm
172400080915     C                   parm                    parmbr
172500080915     C                   parm      '2'           paropz
172600070326     C*
172700080915     C                   eval      wrkesito = '2'
172800070404     C*
172900080915     C                   seton                                        LR
173000070326     C*
173100080915     C                   ENDSR     '*CANCL'
173200070326     C***
173300070326
173400070326
173500010330
173600010330
173700000613     C     *inzsr        BEGSR
173800990910     C*
173900990910     C     *entry        plist
174000990920     C                   parm                    tivlrds
174100990921     C                   parm      wrkesito      esito
174200000724     C                   parm                    prmlit
174300000710     C                   parm                    prmfir
174400010330     C*
174500010330     C* CALCOLA LA DATA CORRENTE
174600010330     C                   time                    wn14             14 0
174700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
174800080915     C                   movel     wn14          oracor            6 0          *ORA
174900010330     C                   z-add     wn8           g08dat
175000010330     C                   z-add     *zeros        g08inv
175100010330     C                   movel     '0'           g08err
175200010330     C                   call      'XSRDA8'
175300010330     C                   parm                    wlbda8
175400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
175500000613     C*
175600000613     C                   ENDSR
175700000613     C***
175800990908
175900080915     Otitv1i8p  E            riepilogo         2
176000990915     O                                              'Upload via Internet'
176100990915     O                                           +1 'Traduzione TIVIN00R -'
176200080915     O                                           +1 'EDIVABWR'
176300990915     O                                           +1 'Testate bolle'
176400990915     O          E            riepilogo   2
176500990915     O                       wrkdata
176600990915     O                       wrkora              +1
176700990915     O                       procname            +1
176800990915     O          E            riepilogo   2
176900990915     O                                              'Cliente..................:'
177000010605     O                       VABCCM        z     +1
177100990915     O          E            riepilogo   2
177200990920     O                                              'Riferimento Strategi.....:'
177300990920     O                       vlrhdl              +1
177400990915     O          E            riepilogo   2
177500990915     O                                              'Giusti...................:'
177600040119     O                       §CTROKVB      2   +  1
177700990915     O          E            riepilogo   2
177800990915     O                                              'Sbagliati e corretti.....:'
177900971022     O                       §CTRMO        2   +  1
178000990915     O          E            riepilogo   2
178100990915     O                                              'Sbagliati e scartati.....:'
178200971022     O                       §CTRNO        2   +  1
178300000613
178400080915     Otitv1i8ps E            testdett          2
178500000613     O                                              'Upload via Internet'
178600000613     O                                           +1 'Traduzione TIVIN00R -'
178700080915     O                                           +1 'EDIVABWR'
178800000613     O                                           +1 'Testate bolle'
178900000616     O          E            testdett    3
179000000613     O                                           +2 'N° rec'
179100000613     O                                           +3 'Anteprima contenuto'
179200000616     O          E            rigadett    2
179300000613     O                       rrnum               +2
179400000621     O                       recko               +3
179500000616     O          E            findett     2
179600000613     O                       wrkdata
179700000613     O                       wrkora              +1
179800000613     O                       procname            +1
179900000616     O          E            findett     2
180000000613     O                                              'Cliente..................:'
180100010605     O                       VABCCM        z     +1
180200000616     O          E            findett     2
180300000613     O                                              'Riferimento Strategi.....:'
180400000613     O                       vlrhdl              +1
180500000616     O          E            findett     2
180600000613     O                                              'Giusti...................:'
180700040119     O                       §CTROKVB      2   +  1
180800000616     O          E            findett     2
180900000613     O                                              'Sbagliati e corretti.....:'
181000000613     O                       §CTRMO        2   +  1
181100000616     O          E            findett     2
181200000613     O                                              'Sbagliati e scartati.....:'
181300000613     O                       §CTRNO        2   +  1
181400000613** CMD - COMANDI CL
181500080915OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
181600080915OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
181700080915DLTOVR FILE(TITV1I8P TITV1I8PS) LVL(*)
181800010606
181900000613
182000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182100080915FILL001;W_VATNOT_E;W_VABRMA;VABNSP;W_VABPKB;W_VABVLB;
182200080915FILL003;FILL004;VABRSD;VABLOD;VABIND;VABCAD;VABPRD
182300080715
