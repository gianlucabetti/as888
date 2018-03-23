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
026600080915 xxx C*
026700080915     C* Aggiungo un byte a '*' all'inizio d ogni record
026800080915     C                   if        %subst(vindta:1:1) = CharCSV
026900080915     C                   eval      vindta  = '*' + %trim(vindta)
027000080915     C                   endif
027100010618     C*
027200040510     C                   if        vindta > *blanks AND
027300040510     C                             wFlgVINDTA = 1
027400010330     C                   add       1             rrnum
027500010330     C*
027600010601     C                   if        *in70 = *off and
027700010330     C                             (vinflg = *blanks
027800010330     C                              or vinflg = '0'
027900010330     C                              or vinflg = '2')
028000010330     C*
028100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028200010711     C                   if        vinflg = *blanks or vinflg = '0'
028300010711     C                   clear                   vinmsg
028400010711     C                   endif
028500070925     C*
028600080915     C                   exsr      inzvar
028700010605     C                   exsr      impvab
028800010601     C*
028900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029000010605     C                   eval      depfil = VABLNP
029100010601     C                   exsr      repfil
029200010601     C                   if        depfil = invfil
029300021025     C                   if        vlrpoi = 999
029400030822     C                   move(P)   invfil        VABFGS
029500021025     C                   else
029600030822     C                   move(P)   vlrpoi        VABFGS
029700021025     C                   endif
029800010601     C*
029900010601     C                   exsr      PREELA
030000010601     C*
030100010604     C* Ebbene...
030200010604     C*
030300040119     C  N31              add       1             §CTROKVB
030400040119     C   32              add       1             §CTRMO
030500040119     C   31              add       1             §CTRNO
030600030822     C                   if        wGiro = 2
030700040510     C* Scrivo sempre il VAT
030800040510     C  N31              exsr      wrivat
030900040510     C* Scrivo il VAB solo a rottura d spedizione
031000040119     C  N31              exsr      wrivab
031100040119     C                   endif
031200010604     C*
031300010604     C                   if        *in31 = *off and
031400010604     C                             *in32 = *off
031500010604     C                   eval      vinflg = '1'
031600010604     C                   else
031700010604     C                   eval      recko = vindta
031800010604     C                   except    rigadett
031900010604     C                   eval      vinflg = '2'
032000010604     C                   endif
032100010604     C                   endif
032200010601     C*
032300010604     C                   endif
032400010604     C*
032500010330     C                   else
032600010330     C                   eval      vinflg = '1'
032700010330     C                   endif
032800010601     C*
032900010601     C  N70              update    tivin000
033000010330     C*
033100030822     C  N70              enddo
033200040119     C*
033300040119     C                   endif                                                  (endif - chkcall)
033400010601     C*
033500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033600010601     C                   if        cntNonEl = *zeros or
033700010601     C                             flgMulti = '0'
033800010330     C* Se non ci sono record con errori ...
033900010601     C                   if        §ctrno = 0 and
034000010604     C                             §ctrmo = 0 and
034100010601     C                             flgStato <> '2'
034200010330     C* ... restituisco esito OK.
034300010330     C                   eval      wrkesito = '0'
034400010330     C                   else
034500040119     C                   if        §ctrokvb > 0
034600010330     C                   eval      wrkesito = '1'
034700010330     C                   else
034800010615     C                   if        flgOk = '0'
034900010615     C                   eval      wrkesito = '2'
035000010615     C                   else
035100010615     C                   eval      wrkesito = '6'
035200010615     C                   endif
035300010330     C                   endif
035400010330     C                   endif
035500010601     C                   else
035600010601     C                   eval      wrkesito = '9'
035700010601     C                   endif
035800010330     C*
035900010330     C                   if        %open(tivin00r)
036000010330     C                   close     tivin00r
036100010330     C                   endif
036200080915     C                   if        %open(edivabwr)
036300080915     C                   close     edivabwr
036400010330     C                   endif
036500080915     C                   if        %open(edivatwr)
036600080915     C                   close     edivatwr
036700040512     C                   endif
036800010601     C*
036900010601     C                   if        vlrpoi <> 999
037000010601     C                   eval      invfil = vlrpoi
037100010601     C                   endif
037200010330     C*
037300040119     C                   if        §ctrokvb > 0
037400010601     C                             and invfil > *zeros
037500010330     C                   exsr      invio
037600010330     C                   endif
037700010601     C*
037800010618     C                   if        flgGiro = '1'
037900010601     C                   exsr      endela
038000010618     C                   endif
038100010330     C*
038200010330     C                   ENDSR
038300010330     C***
038400040119
038500040119
038600040119
038700040119     C*----------------------------------------------------*
038800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038900040119     C*----------------------------------------------------*
039000040510     C     WRIVAB        BEGSR
039100080917     C*
039200080917     C* Forzatura x anomalia cliente => se peso a zero => ignorare completamente record
039300080917     C                   if        VABPKB > *zeros
039400080915     C*
039500080915     C                   eval      VABCMR = 'THUN "NEW"'
039600080915     C                   eval      VABCNT = 1
039700080915     C                   eval      VABDCM = DATCOR
039800080915     C                   eval      VABDTS = DATCOR
039900080915     C                   eval      VABHMS = ORACOR
040000040119     C*
040100060331     C* Gestisco l'eventuale rottura x numero spedizione
040200070301     C                   if        wVATNOT_E <> *blanks
040300080125     C                   if        VABCTM = *blanks
040400080125     C                   eval      VABCTM = '7Q'
040500080125     C                   endif
040600070301     C                   endif
040700080915     C                   write     edivab00                                     => scarico il VAB
040800040119     C*
040900080917     C                   endif
041000080917     C*
041100040119     C                   ENDSR
041200040119
041300040119
041400040119
041500040119     C*----------------------------------------------------*
041600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041700040119     C*----------------------------------------------------*
041800040510     C     WRIVAT        BEGSR
041900060512     C*
042000060512     C* Inizializzo i campi chiave
042100060512     C                   eval      VATFGS = VABFGS
042200060512     C                   eval      VATAAS = VABAAS
042300060512     C                   eval      VATCCM = VABCCM
042400070823     C                   eval      VATNRS = VABNRS
042500060512     C                   eval      VATNSP = VABNSP
042600060512     C                   eval      VATLNP = VABLNP
042700080915     C*
042800080915     C                   eval      VATCMR = 'THUN "NEW"'
042900080915     C                   eval      VATCNT = 1
043000060331     C*
043100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043200060331     C                   if        wVATNOT_A <> *blanks
043300060331     C                   eval      VATNOT = wVATNOT_A
043400060331     C                   eval      VATTRC = 'A'
043500080915     C                   write     EDIVAT00
043600060331     C                   add       1             §CTROKVT
043700060331     C                   endif
043800060331     C*
043900060331     C                   if        wVATNOT_B <> *blanks
044000060331     C                   eval      VATNOT = wVATNOT_B
044100060331     C                   eval      VATTRC = 'B'
044200080915     C                   write     EDIVAT00
044300060331     C                   add       1             §CTROKVT
044400060331     C                   endif
044500060331     C*
044600080625     C                   if        wVATNOT_E <> *blanks
044700060331     C                   eval      VATNOT = wVATNOT_E
044800060331     C                   eval      VATTRC = 'E'
044900080915     C                   write     EDIVAT00
045000080716     C                   add       1             §CTROKVT
045100080625     C                   endif
045200040119     C*
045300040119     C                   ENDSR
045400040119
045500040119
045600040119
045700040119     C*----------------------------------------------------*
045800080915     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
045900040119     C*----------------------------------------------------*
046000040119     C     PREVAT        BEGSR
046100040119     C*
046200080915     C* Compongo il nome del membro da dare al EDIVATWR
046300040119     C                   eval      parmbr = vlrhdl
046400040119     C                   movel     'M'           parmbr
046500040510     C                   eval      parccm = vlrksc
046600040119     C                   eval      paropz = '1'
046700040119     C* Effettuo la chiamata al CLLE preposto
046800080915     C                   call(e)   'TITVEVTC'
046900040119     C                   parm                    parccm
047000040119     C                   parm                    parmbr
047100040119     C                   parm                    paropz
047200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047300040119     C                   if        %error
047400040119     C                   movel     '1'           chkcall
047500040119     C                   else
047600040119     C                   movel     '0'           chkcall
047700040119     C                   endif
047800040119     C*
047900040119     C                   ENDSR
048000010601
048100010601
048200010601
048300010330     C*----------------------------------------------------*
048400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048500010330     C*----------------------------------------------------*
048600040119     C     INZVAR        BEGSR
048700010330     C*
048800030822     C* Inizializzo variabili di wrk
048900020204     C                   Z-ADD     *zeros        Num5_0            5 0
049000060331     C                   MOVEL     *blanks       wVATNOT_A
049100060331     C                   MOVEL     *blanks       wVATNOT_B
049200060331     C                   MOVEL     *blanks       wVATNOT_E
049300080617     C                   EVAL      wFlgCAS = *blanks
049400030822     C*
049500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049600080915     C                   CLEAR                   EDIVAB00
049700080915     C                   CLEAR                   EDIVAT00
049800030822     C                   CLEAR                   SkSplitCSV
049900030822     C                   EVAL      i = 1
050000030822     C                   EVAL      posDa = *zeros
050100030822     C                   EVAL      posA  = *zeros
050200030822     C*
050300030822     C* Reimposto i valori di default
050400030822     C                   EXSR      DEFCAM
050500030822     C*
050600010330     C                   ENDSR
050700010330     C*----------------------------------------------------*
050800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050900010330     C*----------------------------------------------------*
051000010330     C     DEFCAM        BEGSR
051100030822     C*
051200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051300030715     C* e delimitatore testo.
051400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051700030822     C*
051800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
052000030715     C                   SETON                                        50
052100030715     C                   ELSE
052200030715     C                   SETOFF                                       50
052300030715     C                   ENDIF
052400030822     C*
052500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052600030822     C                   EVAL      posDaDft = 1
052700030822     C                   EVAL      posADft  = 0
052800030822     C                   EVAL      wGiroDft = 0
052900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053000030822     C                             posDaDft > 0
053100030822     C*
053200030822     C* Gestisco il 1° giro
053300030822     C                   IF        wGiroDft = 0
053400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053600030822     C* Incremento il contatore dei "giri"
053700030822     C                   EVAL      wGiroDft = 1
053800030822     C                   ELSE
053900030822     C                   EVAL      posDaDft = posADft
054000030822     C                   ENDIF
054100030822     C* Eseguo lo scan x trovare la fine del campo corrente
054200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054300030822     C*
054400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054600030822     C                   IF        posDaDft > 0 AND
054700030822     C                             posADft  > 0
054800030822     C* NCL
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'NCL'
055300030822     C                   EVAL      PiStr=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   EXSR      CHKNUM
055700030822     C                   IF        PiInt=*on
055800030822     C                   Z-ADD     PiVal         VABNCL
055900030822     C                   ENDIF
056000030822     C                   ENDIF
056100030822     C* CCM
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'CCM'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiInt=*on
057100030822     C                   Z-ADD     PiVal         VABCCM
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400030822     C* LNP
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'LNP'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiInt=*on
058400030822     C                   Z-ADD     PiVal         VABLNP
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* NRS
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'NRS'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABNRS
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* CTR
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'CTR'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABCTR
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* PKB
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'PKB'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiNum=*on
062300030822     C                   Z-ADD     PiVal         VABPKB
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* VLB
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'VLB'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiNum=*on
063600030822     C                   Z-ADD     PiVal         VABVLB
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* QFT
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'QFT'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiNum=*on
064900030822     C                   Z-ADD     PiVal         VABQFT
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* CBO
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'CBO'
065700030822     C                   EVAL      VABCBO=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* TSP
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'TSP'
066600030822     C                   EVAL      VABTSP=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* VAS
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'VAS'
067500030822     C                   EVAL      VABVAS=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* VCA
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'VCA'
068400030822     C                   EVAL      VABVCA=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800070925     C* TIC
068900070925     C                   IF        %subst(
069000070925     C                             %subst(vlrppt:posDaDft+1:
069100070925     C                             posADft-posDaDft-1):1:3)
069200070925     C                             = 'TIC'
069300070925     C                   EVAL      VABTIC=%trim(%subst(
069400070925     C                             %subst(vlrppt:posDaDft+1:
069500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600070925     C                   ENDIF
069700030822     C* GCA
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'GCA'
070200030822     C                   EVAL      VABGCA=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* CTM
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'CTM'
071100030822     C                   EVAL      VABCTM=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* FFD
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'FFD'
072000030822     C                   EVAL      VABFFD=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* VAD
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'VAD'
072900030822     C                   EVAL      VABVAD=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* GMA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'GMA'
073800030822     C                   EVAL      VABGMA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* GGA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GGA'
074700030822     C                   EVAL      VABGGA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* GVA
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'GVA'
075600030822     C                   EVAL      VABGVA=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* TC1
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'TC1'
076500030822     C                   EVAL      VABTC1=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* TC2
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'TC2'
077400030822     C                   EVAL      VABTC2=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800040714     C* VATTRC
077900040714     C                   IF        %subst(
078000040714     C                             %subst(vlrppt:posDaDft+1:
078100040714     C                             posADft-posDaDft-1):1:3)
078200040714     C                             = 'TRC'
078300040714     C                   EVAL      VATTRC=%trim(%subst(
078400040714     C                             %subst(vlrppt:posDaDft+1:
078500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600040714     C                   ENDIF
078700030822     C* ...
078800030822     C                   ENDIF
078900030822     C                   ENDDO
079000020204     C*
079100010330     C                   ENDSR
079200010607     C*----------------------------------------------------*
079300080915     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
079400010607     C*----------------------------------------------------*
079500010607     C     IMPVAB        BEGSR
079600010607     C*
079700010607     C                   SETOFF                                       3132
079800030515     C*
079900010607     C                   MOVEL     datcor        VABAAS
080000010607     C                   MOVE      datcor        VABMGS
080100010607     C*
080200030715     C**********
080300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080400030715     C**********
080500030715     C                   IF        wGiro = *zeros
080600080716     C*
080700080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080800080716     C                   EVAL      depvinDTA = vinDTA
080900040113     C*
081000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081200040510     C                             %trim(intcol(3))<>*blanks
081300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081400040113     C                                      %trim(intcol(3))
081500070823     C     ';':CharCSV   XLATE     vindta        vindta
081600040510     C                   ENDIF
081700080626     C*
081800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081900031201     C     minu:maiu     XLATE     vindta        vindta
082000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082100030715     C                   DOW       posDa <= %len(%trim(vindta))
082200030715     C*
082300030715     C* Gestisco il 1° campo
082400030715     C                   IF        i = 1
082500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082600030715     C                   EVAL      posDa = 1
082700030715     C* Eseguo lo scan x trovare la fine del primo campo
082800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082900030715     C* A questo punto "estraggo" il campo corrente
083000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083100030715     C                                                    (posA-posDa))
083200030715     C* X i campi successivi al 1°
083300030715     C                   ELSE
083400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083500030715     C                   EVAL      posDa = posA + 1
083600030715     C* Eseguo lo scan x trovare la fine del campo corrente
083700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083800030715     C* Gestisco l'ultimo campo
083900030715     C                   IF        posA = *zeros
084000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   LEAVE
084500030715     C                   ELSE
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C                   ENDIF
085000030715     C                   ENDIF
085100030715     C* Incremento il contatore d campo
085200030715     C                   EVAL      i = i +1
085300030715     C                   ENDDO
085400030715     C*
085500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085600030715     C* delimitazione testo
085700030715     C                   EVAL      i = 1
085800030715     C                   DOW       i < %elem(SkSplitFLD)
085900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086100030715     C                   EVAL      i = i + 1
086200030715     C                   ENDDO
086300030715     C                   EVAL      wGiro = 1
086400080716     C* Ripristino la 1° riga originale
086500080716     C                   EVAL      vinDTA = depvinDTA
086600030715     C                   ELSE
086700030715     C                   EVAL      wGiro = 2
086800031204     C**********
086900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087000031204     C* d separatore campo
087100031204     C**********
087200031204     C                   EVAL      vindta = %trim(vindta)
087300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087400031204     C                   EVAL      vindta = %subst(vindta:2)
087500031204     C                   ENDDO
087600031204     C*
087700031204     C                   Z-ADD     *zeros        lunghInput        4 0
087800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088200031204     C                   ENDDO
088300030715     C**********
088400030715     C* Effettuo lo split del campo dati d input
088500030715     C**********
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
089600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C* X i campi successivi al 1°
089900030715     C                   ELSE
090000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090100030715     C                   EVAL      posDa = posA + 1
090200031204     C* Verifico che nn vi sia il campo nn valorizzato
090300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090400031204     C* Se campo nn valorizzato skippo al prossimo
090500031204     C                   EVAL      SkSplitCSV(i) = *blanks
090600031204     C                   EVAL      posA  = posA + 1
090700031204     C                   ELSE
090800030715     C* Eseguo lo scan x trovare la fine del campo corrente
090900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091000030715     C* Gestisco l'ultimo campo
091100030715     C                   IF        posA = *zeros
091200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   LEAVE
091700030715     C                   ELSE
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C                   ENDIF
092200031204     C                   ENDIF
092300030715     C                   ENDIF
092400030715     C* Incremento il contatore d campo
092500030715     C                   EVAL      i = i +1
092600030715     C                   ENDDO
092700030715     C*
092800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092900030715     C* delimitazione testo
093000030715     C                   EVAL      i = 1
093100030715     C                   DOW       i < %elem(SkSplitCSV)
093200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093400030715     C                   EVAL      i = i + 1
093500030715     C                   ENDDO
093600030715     C*
093700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093800030715     C                   EVAL      i = 1
093900030715     C                   DOW       i < %elem(SkSplitFLD)
094000030715     C*
094100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094300030715     C                             SkSplitCSV(i) <> *blanks
094400030721     C*** ATB
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094600030721     C                   EVAL      VABATB = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030715     C*** RSD
094900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095300030715     C* ==
095400030715     C                   ENDIF
095500030721     C*** RD2
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030715     C*** IND
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096100030715     C                   EVAL      VABIND = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** LOD
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030715     C*** PRD
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030721     C*** NZD
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030715     C*** RMA
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** NOT
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** NT2
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** VCA
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** VAS
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** FFD
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030721     C*** GC1
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** GC2
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** TSP
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101000030721     C                   ENDIF
101100040510     C*** CBO
101200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101400040510     C                   ENDIF
101500030721     C*** NAS
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** TIC
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** GCA
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** XCO
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** CTM
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** TCR
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** CTS
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** FTM
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** VAD
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GMA
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GGA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** GVA
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** TC1
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TC2
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** SCL
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** RMO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** NMO
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*
108400030721     C* Reperisco quindi i campi numerici...
108500040510     C*** NSP
108600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108700040510     C                   EVAL      PiStr=SkSplitCSV(i)
108800040510     C                   EXSR      CHKNUM
108900040510     C                   IF        PiInt=*on
109000040510     C                   Z-ADD     PiVal         VABNSP
109100040510     C                   ELSE
109200040510     C                   SETON                                        31
109300040510     C                   EVAL      VABNSP = *zeros
109400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109500040510     C                             + ' ' + 'VABNSP'
109600040510     C                   ENDIF
109700040510     C                   ENDIF
109800071106     C*** RMN
109900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110000071106     C                   EVAL      PiStr=SkSplitCSV(i)
110100071106     C                   EXSR      CHKNUM
110200071106     C                   IF        PiInt=*on
110300071106     C                   Z-ADD     PiVal         VABRMN
110400071106     C                   ELSE
110500071106     C                   SETON                                        32
110600071106     C                   EVAL      VABRMN = *zeros
110700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110800071106     C                             + ' ' + 'VABRMN'
110900071106     C                   ENDIF
111000071106     C                   ENDIF
111100030721     C*** CAD
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111300030721     C                   EVAL      PiStr=SkSplitCSV(i)
111400030721     C                   EXSR      CHKNUM
111500030721     C                   IF        PiInt=*on
111600030721     C                   Z-ADD     PiVal         Num5_0
111700030721     C                   MOVEL(p)  Num5_0        VABCAD
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      VABCAD = *zeros
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABCAD'
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
115500080617     C                             %trim(SkSplitCSV(i)) <> '0'
115600080125     C                   EVAL      wFlgCAS = '1'
115700080125     C                   ENDIF
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiNum=*on
116100030721     C                   Z-ADD     PiVal         VABCAS
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABCAS = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABCAS'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** CCM
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on      AND
117400030721     C                             PiVal<=9999999 AND
117500030721     C                             PiVal>*zeros
117600030721     C                   Z-ADD     PiVal         VABCCM
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABCCM = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100040510     C                             + ' ' + 'VABCCM'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** LNP
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on    AND
118900030721     C                             PiVal<=999   AND
119000030721     C                             PiVal>*zeros
119100030721     C                   Z-ADD     PiVal         VABLNP
119200030721     C                   ELSE
119300040715     C                   SETON                                        32
119400030721     C                   EVAL      VABLNP = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600040510     C                             + ' ' + 'VABLNP'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** NRS
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on AND
120400030721     C                             PiVal<=99
120500030721     C                   Z-ADD     PiVal         VABNRS
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABNRS'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** LNA
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on    AND
121700030721     C                             PiVal<=999   AND
121800030721     C                             PiVal>*zeros
121900030721     C                   Z-ADD     PiVal         VABLNA
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      VABLNA = *zeros
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABLNA'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** CTR
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on  AND
123200030721     C                             PiVal<=999
123300030721     C                   Z-ADD     PiVal         VABCTR
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABCTR'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** IAS
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiNum=*on
124500030721     C                   Z-ADD     PiVal         VABIAS
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABIAS'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** VLB
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABVLB
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABVLB'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** QFT
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABQFT
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABQFT'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** NCD
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on      AND
128100030721     C                             PiVal<=9999999
128200030721     C                   Z-ADD     PiVal         VABNCD
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABNCD'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** NCA
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on      AND
129400030721     C                             PiVal<=9999999
129500030721     C                   Z-ADD     PiVal         VABNCA
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABNCA'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** ZNC
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on AND
130700030721     C                             PiVal<=99
130800030721     C                   Z-ADD     PiVal         VABZNC
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABZNC'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** DCR
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on       AND
132000030721     C                             PiVal<=99999999
132100030721     C                   Z-ADD     PiVal         VABDCR
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABDCR'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** HCR
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on   AND
133300030721     C                             PiVal<=9999
133400030721     C                   Z-ADD     PiVal         VABHCR
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABHCR'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** VMD
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiNum=*on
134600030721     C                   Z-ADD     PiVal         VABVMD
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABVMD'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** ANT
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on        AND
135800030721     C                             PiVal<=999999999
135900030721     C                   Z-ADD     PiVal         VABANT
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABANT'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** CMO
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on
137100030721     C                   Z-ADD     PiVal         Num5_0
137200030721     C                   MOVEL(p)  Num5_0        VABCMO
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      VABCMO = *zeros
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABCMO'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000040510     C*
138100080915     C* Reperisco i campi del EDIVAT
138200040510     C*** VATNOT
138300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138500040510     C                   ENDIF
138600040510     C*** VATTRC
138700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138900040510     C                   ENDIF
139000060331     C*** VATNOT_A
139100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139300060331     C                   ENDIF
139400060331     C*** VATNOT_B
139500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139700060331     C                   ENDIF
139800060331     C*** VATNOT_E
139900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140100060331     C                   ENDIF
140200070823     C*
140300071029     C***  ===>  Gestione campi "particolari" dentro ciclo
140400071204     C*
140500080915     C*** RMA/RMN/NSP
140600080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
140700080915     C                   EVAL      VABRMA=SkSplitCSV(i)
140800080716     C                   EVAL      PiStr=SkSplitCSV(i)
140900071204     C                   EXSR      CHKNUM
141000071204     C                   IF        PiInt=*on
141100080915     C***                Z-ADD     PiVal         VABNSP
141200080915     C                   Z-ADD     PiVal         VABRMN
141300071204     C                   ELSE
141400080915     C                   SETON                                        31
141500080915     C                   EVAL      VABRMN = 1
141600071204     C                   EVAL      vinmsg = %trimr(vinmsg)
141700080915     C***                          + ' ' + 'VABNSP VATNSP VABRMN'
141800080915     C                             + ' ' + 'VABRMN'
141900071204     C                   ENDIF
142000071204     C                   ENDIF
142100080915     C*** PKB
142200080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
142300080917     C                   IF        %trim(SkSplitCSV(i)) <> *zeros
142400080915     C                   EVAL      PiStr=SkSplitCSV(i)
142500080915     C                   EXSR      CHKNUM
142600080915     C                   IF        PiInt=*on
142700080915     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg.
142800080915     C                   Z-ADD     PiVal         VABPKB
142900080915     C                   ELSE
143000080915     C                   SETON                                        32
143100080915     C                   EVAL      vinmsg = %trimr(vinmsg)
143200080915     C                             + ' ' + 'VABPKB'
143300080917     C                   ENDIF
143400080915     C                   ENDIF
143500080915     C                   ENDIF
143600080915     C*** VLB
143700080915     C***                IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
143800080915     C***                EVAL      PiStr=SkSplitCSV(i)
143900080915     C***                EXSR      CHKNUM
144000080915     C***                IF        PiInt=*on
144100080915     C***                EVAL      PiVal = PiVal / 1000000000                   * da cm3 a m3
144200080915     C***                Z-ADD     PiVal         VABVLB
144300080915     C***                ELSE
144400080915     C***                SETON                                        32
144500080915     C***                EVAL      vinmsg = %trimr(vinmsg)
144600080915     C***                          + ' ' + 'VABVLB'
144700080915     C***                ENDIF
144800080915     C***                ENDIF
144900080915     C*** W_VATNOT_E
145000080915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
145100080915     C                   EVAL      wVATNOT_E = %trim(%subst(SkSplitCSV(i):5:35))
145200080716     C                   ENDIF
145300070925     C*
145400071029     C***  <===  -----------------------------------------
145500040510     C*
145600030715     C                   ENDIF
145700030715     C                   EVAL      i = i + 1
145800030715     C                   ENDDO
145900070925     C*
146000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146100080125     C*
146200070925     C*
146300071029     C***  <===  -----------------------------------------
146400070925     C*
146500070925     C* Considerazioni finali su CBO/CAS
146600080125     C                   IF        wFlgCAS = '1'
146700070925     C                   IF        VABCBO = '1'
146800070925     C                   EVAL      VABCBO = '4'
146900070925     C                   ENDIF
147000070925     C                   IF        VABCBO = '2'
147100070925     C                   EVAL      VABCBO = '6'
147200070925     C                   ENDIF
147300070925     C                   ENDIF
147400070925     C***  <===  ----------------------------
147500040510     C*
147600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147700040510     C                   IF        VABNSP = *zeros
147800040510     C                   SETON                                        31
147900040510     C                   ENDIF
148000040715     C*
148100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148200040715     C                   IF        VABLNP = *zeros
148300040715     C                   SETON                                        31
148400040715     C                   ENDIF
148500040510     C*
148600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148700040510     C                   IF        VABRMN = *zeros
148800040510     C                   Z-ADD     VABNSP        VABRMN
148900040510     C                   ENDIF
149000040510     C*
149100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149200040510     C                   IF        VABRMA = *blanks
149300040714     C                   MOVEL     VABRMN        VABRMA
149400040510     C                   ENDIF
149500040128     C*
149600040510     C* Se provincia nn valorizzata la reperisco
149700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149800040510     C                   IF        VABPRD = *blanks
149900040128     C                   CLEAR                   TISI95DS
150000040128     C                   EVAL      I95TCN = '3'
150100040128     C                   Z-ADD     datcor        I95DAT
150200040128     C                   EVAL      I95NAR = VABNZD
150300040128     C                   EVAL      I95CAP = VABCAD
150400040128     C                   EVAL      I95LOC = VABLOD
150500040128     C                   CALL      'TISI95R'
150600040128     C                   PARM                    TISI95DS
150700040128     C                   EVAL      VABPRD = O95PRV
150800040510     C                   ENDIF
150900020204     C*
151000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151100020204     C                   EXSR      CHKIMPDIV
151200030715     C                   ENDIF
151300020204     C*
151400010607     C                   ENDSR
151500010607     C*----------------------------------------------------*
151600010601
151700020204
151800020204     C*----------------------------------------------------*
151900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152000020204     C*----------------------------------------------------*
152100020204     C     CHKIMPDIV     BEGSR
152200020204     C*
152300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152400020204     C                   Z-ADD     *zeros        wrkDec            9 9
152500020204     C*
152600020204     C* Come prima cosa effettuo considerazioni sulla divisa
152700020204     C                   IF        vabIAS > *zeros
152800020204     C                   IF        vabVAS <> 'EUR'
152900020204     C                   EVAL      vabVAS =  'ITL'
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C                   IF        vabCAS > *zeros
153400020204     C                   IF        vabVCA <> 'EUR'
153500020204     C                   EVAL      vabVCA =  'ITL'
153600020204     C                   ENDIF
153700020204     C                   ENDIF
153800020204     C*
153900020204     C                   IF        vabVMD > *zeros
154000020204     C                   IF        vabVAD <> 'EUR'
154100020204     C                   EVAL      vabVAD =  'ITL'
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154600020204     C                   Z-ADD     vabIAS        wrkDec
154700020204     C                   IF        wrkDec > *zeros
154800020204     C                   IF        vabVAS = 'ITL'
154900020204     C                   EVAL      vabIAS = *zeros
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155400020204     C                   Z-ADD     vabCAS        wrkDec
155500020204     C                   IF        wrkDec > *zeros
155600020204     C                   IF        vabVCA = 'ITL'
155700020204     C                   EVAL      vabCAS = *zeros
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156200020204     C                   Z-ADD     vabVMD        wrkDec
156300020204     C                   IF        wrkDec > *zeros
156400020204     C                   IF        vabVAD = 'ITL'
156500020204     C                   EVAL      vabVMD = *zeros
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C                   ENDSR
157000020204     C***
157100080617
157200080617
157300080617
157400080617     C*----------------------------------------------------*
157500080617     C*  CONTROLLO NUMERICITA' CAMPI
157600080617     C*----------------------------------------------------*
157700080617     C     CHKNUM        BEGSR
157800080617     C*
157900080617     C                   IF        PiDecChr = *blanks
158000080617     C                   EVAL      PiDecChr = CharNUM
158100080617     C                   ENDIF
158200080617     C*
158300080617     C                   callp     UBISNUM_Check(PiStr
158400080617     C                                          :PiDecChr
158500080617     C                                          :PiVal
158600080617     C                                          :PiNum
158700080617     C                                          :PiInt)
158800080617     C*
158900080617     C                   ENDSR
159000080617     C***
159100010330
159200010601
159300010601
159400010601
159500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159600010601     C     repfil        BEGSR
159700010601     C*
159800010601     C                   if        invfil = *zeros and
159900010601     C                             depfil > *zeros and
160000010629     C                             (vinflg = *blanks or
160100010629     C                              vinflg = *zeros)
160200010601     C
160300010601     C                   eval      invfil = depfil
160400010601     C                   endif
160500010601     C*
160600010601     C                   if        depfil <> invfil and
160700010601     C                             invfil > *zeros
160800010601     C                   eval      flgMulti = '1'
160900010601     C                   if        vinflg = *blanks
161000010601     C                   add       1             cntNonEl
161100010601     C                   endif
161200010601     C                   endif
161300010601     C*
161400010601     C                   if        vinflg = '2'
161500010601     C                   eval      flgStato = '2'
161600010601     C                   endif
161700010601     C*
161800010601     C                   ENDSR
161900010601     C***
162000010601
162100010601
162200010601
162300010330
162400040119      /TITLE Invio dei dati al punto operativo.
162500040119     C     invio         BEGSR
162600040119     C*
162700080915     C* 1° invio EDIVAT
162800040119     C                   reset                   dscmz
162900040119     C                   move      vlrpoi        cmzdst
163000080915     C                   eval      cmzfld = 'EDIVATWR'
163100040119     C                   eval      cmzmbd = vlrhdl
163200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163300040119     C***                if        prmfir = *blanks
163400080915     C                   eval      cmzfla = 'EDIVAT0F'
163500080915     C                   eval      cmzmba = 'EDIVAT0F'
163600040119     C***                else
163700040119     C***                eval      cmzfla = prmfir
163800040119     C***                eval      cmzmba = prmfir
163900040119     C***                endif
164000040119     C                   eval      cmznrr = *zeros
164100040119     C                   move      §ctrokvt      cmznrr
164200040119     C                   eval      cmzlba = vlrfl1
164300040119     C                   call(e)   'TIS711C'
164400040119     C                   parm                    dscmz
164500040119     C                   parm      *blanks       esito
164600040119     C                   if        %error
164700040119     C                             or cmzerr = '1'
164800040119     C                             or esito  = '1'
164900040119     C                   eval      wrkesito = '3'
165000040119     C                   else
165100040119     C*
165200080915     C* 2° invio EDIVAB
165300040119     C                   reset                   dscmz
165400040119     C                   move      vlrpoi        cmzdst
165500040119     C                   eval      cmzfld = vlrfou
165600040119     C                   eval      cmzmbd = vlrhdl
165700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165800040119     C***                if        prmfir = *blanks
165900080915     C                   eval      cmzfla = 'EDIVAB0F'
166000080915     C                   eval      cmzmba = 'EDIVAB0F'
166100040119     C***                else
166200040119     C***                eval      cmzfla = prmfir
166300040119     C***                eval      cmzmba = prmfir
166400040119     C***                endif
166500040119     C                   eval      cmznrr = *zeros
166600040119     C                   move      §ctrokvb      cmznrr
166700040119     C                   eval      cmzlba = vlrfl1
166800040119     C                   call(e)   'TIS711C'
166900040119     C                   parm                    dscmz
167000040119     C                   parm      *blanks       esito
167100040119     C                   if        %error
167200040119     C                             or cmzerr = '1'
167300040119     C                             or esito  = '1'
167400040119     C                   eval      wrkesito = '3'
167500040119     C                   endif
167600040119     C                   endif
167700040119     C*
167800040119     C                   ENDSR
167900040119     C***
168000010601
168100010601
168200010601
168300010601
168400010601      /TITLE Invio dei dati al punto operativo.
168500010601     C     opeini        BEGSR
168600010601     C*
168700010601     C* Inizializzo flag e contatori operativi
168800010601     C                   movel     '0'           flgGiro           1
168900010601     C                   movel     '0'           flgMulti          1
169000010601     C                   movel     '1'           flgStato          1
169100010615     C                   movel     '0'           flgOk             1
169200010601     C                   z-add     *zeros        cntNonEl         10 0
169300010601     C                   z-add     *zeros        depfil            3 0
169400010601     C                   z-add     *zeros        invfil            3 0
169500010601     C*
169600010601     C                   ENDSR
169700010601     C***
169800070326
169900070326
170000070326
170100070326
170200080915     C     *pssr         BEGSR
170300070329     C*
170400080915     C                   if        %open(tivin00r)
170500080915     C                   close     tivin00r
170600080915     C                   endif
170700080915     C                   if        %open(edivabwr)
170800080915     C                   close     edivabwr
170900080915     C                   endif
171000080915     C                   if        %open(edivatwr)
171100080915     C                   close     edivatwr
171200080915     C                   endif
171300070326     C*
171400070326     C* Effettuo la chiamata al CLLE preposto
171500080915     C                   call(e)   'TITVEVTC'
171600080915     C                   parm                    parccm
171700080915     C                   parm                    parmbr
171800080915     C                   parm      '2'           paropz
171900070326     C*
172000080915     C                   eval      wrkesito = '2'
172100070404     C*
172200080915     C                   seton                                        LR
172300070326     C*
172400080915     C                   ENDSR     '*CANCL'
172500070326     C***
172600070326
172700070326
172800010330
172900010330
173000000613     C     *inzsr        BEGSR
173100990910     C*
173200990910     C     *entry        plist
173300990920     C                   parm                    tivlrds
173400990921     C                   parm      wrkesito      esito
173500000724     C                   parm                    prmlit
173600000710     C                   parm                    prmfir
173700010330     C*
173800010330     C* CALCOLA LA DATA CORRENTE
173900010330     C                   time                    wn14             14 0
174000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
174100080915     C                   movel     wn14          oracor            6 0          *ORA
174200010330     C                   z-add     wn8           g08dat
174300010330     C                   z-add     *zeros        g08inv
174400010330     C                   movel     '0'           g08err
174500010330     C                   call      'XSRDA8'
174600010330     C                   parm                    wlbda8
174700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
174800000613     C*
174900000613     C                   ENDSR
175000000613     C***
175100990908
175200080915     Otitv1i8p  E            riepilogo         2
175300990915     O                                              'Upload via Internet'
175400990915     O                                           +1 'Traduzione TIVIN00R -'
175500080915     O                                           +1 'EDIVABWR'
175600990915     O                                           +1 'Testate bolle'
175700990915     O          E            riepilogo   2
175800990915     O                       wrkdata
175900990915     O                       wrkora              +1
176000990915     O                       procname            +1
176100990915     O          E            riepilogo   2
176200990915     O                                              'Cliente..................:'
176300010605     O                       VABCCM        z     +1
176400990915     O          E            riepilogo   2
176500990920     O                                              'Riferimento Strategi.....:'
176600990920     O                       vlrhdl              +1
176700990915     O          E            riepilogo   2
176800990915     O                                              'Giusti...................:'
176900040119     O                       §CTROKVB      2   +  1
177000990915     O          E            riepilogo   2
177100990915     O                                              'Sbagliati e corretti.....:'
177200971022     O                       §CTRMO        2   +  1
177300990915     O          E            riepilogo   2
177400990915     O                                              'Sbagliati e scartati.....:'
177500971022     O                       §CTRNO        2   +  1
177600000613
177700080915     Otitv1i8ps E            testdett          2
177800000613     O                                              'Upload via Internet'
177900000613     O                                           +1 'Traduzione TIVIN00R -'
178000080915     O                                           +1 'EDIVABWR'
178100000613     O                                           +1 'Testate bolle'
178200000616     O          E            testdett    3
178300000613     O                                           +2 'N° rec'
178400000613     O                                           +3 'Anteprima contenuto'
178500000616     O          E            rigadett    2
178600000613     O                       rrnum               +2
178700000621     O                       recko               +3
178800000616     O          E            findett     2
178900000613     O                       wrkdata
179000000613     O                       wrkora              +1
179100000613     O                       procname            +1
179200000616     O          E            findett     2
179300000613     O                                              'Cliente..................:'
179400010605     O                       VABCCM        z     +1
179500000616     O          E            findett     2
179600000613     O                                              'Riferimento Strategi.....:'
179700000613     O                       vlrhdl              +1
179800000616     O          E            findett     2
179900000613     O                                              'Giusti...................:'
180000040119     O                       §CTROKVB      2   +  1
180100000616     O          E            findett     2
180200000613     O                                              'Sbagliati e corretti.....:'
180300000613     O                       §CTRMO        2   +  1
180400000616     O          E            findett     2
180500000613     O                                              'Sbagliati e scartati.....:'
180600000613     O                       §CTRNO        2   +  1
180700000613** CMD - COMANDI CL
180800080915OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
180900080915OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
181000080915DLTOVR FILE(TITV1I8P TITV1I8PS) LVL(*)
181100010606
181200000613
181300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181400081021VABNOT;W_VATNOT_E;W_VABRMA;VABNSP;W_VABPKB;W_VABVLB;
181500081021FILL003;VABRSD;VABLOD;VABIND;VABCAD;VABPRD
181600080715
