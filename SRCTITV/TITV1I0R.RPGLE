000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800080715     Ftitv1i0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080716 xxx F***                                  infsr(*pssr)
001100080715     Ftitv1i0ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300080716 xxx F***                                  infsr(*pssr)
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
010200990921     C                   reset                   wrkesito
010300010601     C*
010400010601     C                   exsr      opeini
010500010605     C                   exsr      rwvab
010600010601     C*
010700010601     C                   seton                                        lr
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010601     C*--------------------------------------------------------
011400010601     C     PREELA        BEGSR
011500010601     C*
011600010601     C* SE OCCORRE SPEDIRE IN FILIALE
011700010601     C                   if        invfil <> *zeros and
011800010601     C                             flgGiro = '0'
011900010601     C*
012000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012100010601     C                   eval      flgGiro = '1'
012200000724     C*
012300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500010601     C     invfil        chain     azorg01l
012600000724     C                   if        %found
012700000724     C                   movel(p)  CMD(1)        depcmd
012800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012900000724     C*
013000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000724     C                   PARM                    depcmd
013400000724     C                   PARM                    LENGH
013500000724     C*
013600000724     C                   endif
013700000616     C*
013800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013900000616     C                   movel(p)  CMD(2)        depcmd
014000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014100000616     C*
014200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014300011113     C                   Z-ADD     150           LENGH            15 5
014400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014500000616     C                   PARM                    depcmd
014600000616     C                   PARM                    LENGH
014700000613     C*
014800080715     C                   if        not %open(titv1i0ps)
014900080715     C                   open      titv1i0ps
015000070329     C                   except    testdett
015100000613     C                   endif
015200010601     C*
015300010601     C                   endif
015400010601     C*
015500010601     C                   ENDSR
015600010601     C***
015700010601
015800010601
015900010601
016000010601
016100010601     C*--------------------------------------------------------
016200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016300010601     C*--------------------------------------------------------
016400010601     C     ENDELA        BEGSR
016500990915     C*
016600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016700040119     C*
016800040119     C* Effettuo la chiamata al CLLE preposto
016900040119     C                   call(e)   'TITVVTC'
017000040510     C                   parm                    parccm
017100040119     C                   parm                    parmbr
017200040119     C                   parm      '2'           paropz
017300050201     C*
017400050201     C* Effettuo lancio TISI95 solo x chiusura
017500050201     C                   CLEAR                   TISI95DS
017600050201     C                   EVAL      I95TLA = 'C'
017700050201     C                   CALL      'TISI95R'
017800050201     C                   PARM                    TISI95DS
017900000613     C*
018000080715     C                   if        %open(titv1i0ps)
018100000616     C                   except    findett
018200080715     C                   close     titv1i0ps
018300000613     C                   endif
018400000616     C*
018500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018600011113     C                   Z-ADD     150           LENGH            15 5
018700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018800000616     C                   PARM                    CMD(3)
018900000616     C                   PARM                    LENGH
019000000616     C*
019100010601     C                   ENDSR
019200010601     C***
019300010601
019400010601
019500010601
019600000613
019700000613     C*--------------------------------------------------------
019800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019900000613     C*--------------------------------------------------------
020000000621     C     STPR          BEGSR
020100000613     C*
020200080715     C                   if        not %open(titv1i0p)
020300080715     C   50              open      titv1i0p
020400990915     C                   endif
020500990915     C*
020600030715     C   50              except    riepilogo
020700990915     C*
020800080715     C                   if        %open(titv1i0p)
020900080715     C   50              close     titv1i0p
021000990914     C                   endif
021100000613     C*
021200000613     C                   ENDSR
021300000613     C***
021400000613
021500010601
021600010601
021700010330     C*--------------------------------------------------------
021800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021900010330     C*--------------------------------------------------------
022000010605     C     RWVAB         BEGSR
022100010330     C*
022200010330     C                   if        not %open(tivin00r)
022300010330     C                   open      tivin00r
022400010330     C                   endif
022500021025     C                   if        not %open(fivabwwr)
022600021025     C                   open      fivabwwr
022700010330     C                   endif
022800040119     C*
022900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023000040119     C                   exsr      prevat
023100040119     C*
023200040119     C                   if        chkcall = '0'
023300040119     C*
023400040119     C                   if        not %open(fivatwwr)
023500040119     C                   open      fivatwwr
023600040119     C                   endif
023700010330     C*
023800040119     C                   clear                   §CTROKVB          7 0
023900040119     C                   clear                   §CTROKVT          7 0
024000040119     C                   clear                   §CTRMO            7 0
024100040119     C                   clear                   §CTRNO            7 0
024200070823     C*
024300070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024400070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024500080716     C***                exsr      inzvar
024600080716     C***                exsr      impvab
024700010330     C*
024800030822     C                   do        *HIVAL
024900010330     C*
025000030822     C                   read      tivin00r                               70
025100010618     C*
025200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025300010618     C                   if        vinflg = '1'
025400010618     C                   eval      flgOk = '1'
025500010618     C                   endif
025600040510     C*
025700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025800040510     C                   z-add     *zeros        wLenVINDTA        4 0
025900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026300040510     C                   z-add     1             wFlgVINDTA
026400040510     C                   leave
026500040510     C                   endif
026600040510     C                   enddo
026700010618     C*
026800040510     C                   if        vindta > *blanks AND
026900040510     C                             wFlgVINDTA = 1
027000010330     C                   add       1             rrnum
027100010330     C*
027200010601     C                   if        *in70 = *off and
027300010330     C                             (vinflg = *blanks
027400010330     C                              or vinflg = '0'
027500010330     C                              or vinflg = '2')
027600010330     C*
027700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027800010711     C                   if        vinflg = *blanks or vinflg = '0'
027900010711     C                   clear                   vinmsg
028000010711     C                   endif
028100010601     C*
028200070925     C* Aggiungo un byte a blanks all'inizio d ogni record
028300070925     C                   eval      vindta  = ' ' + %trim(vindta)
028400070925     C*
028500010330     C                   exsr      inzvar
028600010605     C                   exsr      impvab
028700010601     C*
028800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028900010605     C                   eval      depfil = VABLNP
029000010601     C                   exsr      repfil
029100010601     C                   if        depfil = invfil
029200021025     C                   if        vlrpoi = 999
029300030822     C                   move(P)   invfil        VABFGS
029400021025     C                   else
029500030822     C                   move(P)   vlrpoi        VABFGS
029600021025     C                   endif
029700010601     C*
029800010601     C                   exsr      PREELA
029900010601     C*
030000010604     C* Ebbene...
030100010604     C*
030200040119     C  N31              add       1             §CTROKVB
030300040119     C   32              add       1             §CTRMO
030400040119     C   31              add       1             §CTRNO
030500030822     C                   if        wGiro = 2
030600040510     C* Scrivo sempre il VAT
030700040510     C  N31              exsr      wrivat
030800040510     C* Scrivo il VAB solo a rottura d spedizione
030900040119     C                   if        depspe =  *zeros OR
031000040119     C                             depspe <> vabNSP
031100040119     C  N31              exsr      wrivab
031200040119     C  N31              eval      depspe = vabNSP
031300040119     C                   endif
031400040119     C                   endif
031500010604     C*
031600010604     C                   if        *in31 = *off and
031700010604     C                             *in32 = *off
031800010604     C                   eval      vinflg = '1'
031900010604     C                   else
032000010604     C                   eval      recko = vindta
032100010604     C                   except    rigadett
032200010604     C                   eval      vinflg = '2'
032300010604     C                   endif
032400010604     C                   endif
032500010601     C*
032600010604     C                   endif
032700010604     C*
032800010330     C                   else
032900010330     C                   eval      vinflg = '1'
033000010330     C                   endif
033100010601     C*
033200010601     C  N70              update    tivin000
033300010330     C*
033400030822     C  N70              enddo
033500040119     C*
033600040119     C                   endif                                                  (endif - chkcall)
033700010601     C*
033800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033900010601     C                   if        cntNonEl = *zeros or
034000010601     C                             flgMulti = '0'
034100010330     C* Se non ci sono record con errori ...
034200010601     C                   if        §ctrno = 0 and
034300010604     C                             §ctrmo = 0 and
034400010601     C                             flgStato <> '2'
034500010330     C* ... restituisco esito OK.
034600010330     C                   eval      wrkesito = '0'
034700010330     C                   else
034800040119     C                   if        §ctrokvb > 0
034900010330     C                   eval      wrkesito = '1'
035000010330     C                   else
035100010615     C                   if        flgOk = '0'
035200010615     C                   eval      wrkesito = '2'
035300010615     C                   else
035400010615     C                   eval      wrkesito = '6'
035500010615     C                   endif
035600010330     C                   endif
035700010330     C                   endif
035800010601     C                   else
035900010601     C                   eval      wrkesito = '9'
036000010601     C                   endif
036100010330     C*
036200010330     C                   if        %open(tivin00r)
036300010330     C                   close     tivin00r
036400010330     C                   endif
036500021025     C                   if        %open(fivabwwr)
036600021025     C                   close     fivabwwr
036700010330     C                   endif
036800040512     C                   if        %open(fivatwwr)
036900040512     C                   close     fivatwwr
037000040512     C                   endif
037100010601     C*
037200010601     C                   if        vlrpoi <> 999
037300010601     C                   eval      invfil = vlrpoi
037400010601     C                   endif
037500010330     C*
037600040119     C                   if        §ctrokvb > 0
037700010601     C                             and invfil > *zeros
037800010330     C                   exsr      invio
037900010330     C                   endif
038000010601     C*
038100010618     C                   if        flgGiro = '1'
038200010601     C                   exsr      endela
038300010618     C                   endif
038400010330     C*
038500010330     C                   ENDSR
038600010330     C***
038700040119
038800040119
038900040119
039000040119     C*----------------------------------------------------*
039100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039200040119     C*----------------------------------------------------*
039300040510     C     WRIVAB        BEGSR
039400040119     C*
039500060331     C* Gestisco l'eventuale rottura x numero spedizione
039600070823     C                   if        VABNSP <> depspe
039700070301     C                   if        wVATNOT_E <> *blanks
039800080125     C                   if        VABCTM = *blanks
039900080125     C                   eval      VABCTM = '7Q'
040000080125     C                   endif
040100070301     C                   endif
040200040119     C                   write     fivab000                                     => scarico il VAB
040300060331     C                   endif
040400040119     C*
040500040119     C                   ENDSR
040600040119
040700040119
040800040119
040900040119     C*----------------------------------------------------*
041000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041100040119     C*----------------------------------------------------*
041200040510     C     WRIVAT        BEGSR
041300060512     C*
041400060512     C* Inizializzo i campi chiave
041500060512     C                   eval      VATFGS = VABFGS
041600060512     C                   eval      VATAAS = VABAAS
041700060512     C                   eval      VATCCM = VABCCM
041800070823     C                   eval      VATNRS = VABNRS
041900060512     C                   eval      VATNSP = VABNSP
042000060512     C                   eval      VATLNP = VABLNP
042100060331     C*
042200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042300060331     C                   if        wVATNOT_A <> *blanks
042400060331     C                   eval      VATNOT = wVATNOT_A
042500060331     C                   eval      VATTRC = 'A'
042600060331     C                   write     FIVAT000
042700060331     C                   add       1             §CTROKVT
042800060331     C                   endif
042900060331     C*
043000060331     C                   if        wVATNOT_B <> *blanks
043100060331     C                   eval      VATNOT = wVATNOT_B
043200060331     C                   eval      VATTRC = 'B'
043300060331     C                   write     FIVAT000
043400060331     C                   add       1             §CTROKVT
043500060331     C                   endif
043600060331     C*
043700080625     C                   if        wVATNOT_E <> *blanks
043800060331     C                   eval      VATNOT = wVATNOT_E
043900060331     C                   eval      VATTRC = 'E'
044000080716     C                   write     FIVAT000
044100080716     C                   add       1             §CTROKVT
044200080625     C                   endif
044300040119     C*
044400040119     C                   ENDSR
044500040119
044600040119
044700040119
044800040119     C*----------------------------------------------------*
044900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045000040119     C*----------------------------------------------------*
045100040119     C     PREVAT        BEGSR
045200040119     C*
045300040119     C* Compongo il nome del membro da dare al FIVATWWR
045400040119     C                   eval      parmbr = vlrhdl
045500040119     C                   movel     'M'           parmbr
045600040510     C                   eval      parccm = vlrksc
045700040119     C                   eval      paropz = '1'
045800040119     C* Effettuo la chiamata al CLLE preposto
045900040119     C                   call(e)   'TITVVTC'
046000040119     C                   parm                    parccm
046100040119     C                   parm                    parmbr
046200040119     C                   parm                    paropz
046300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046400040119     C                   if        %error
046500040119     C                   movel     '1'           chkcall
046600040119     C                   else
046700040119     C                   movel     '0'           chkcall
046800040119     C                   endif
046900040119     C*
047000040119     C                   ENDSR
047100010601
047200010601
047300010601
047400010330     C*----------------------------------------------------*
047500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047600010330     C*----------------------------------------------------*
047700040119     C     INZVAR        BEGSR
047800010330     C*
047900030822     C* Inizializzo variabili di wrk
048000020204     C                   Z-ADD     *zeros        Num5_0            5 0
048100060331     C                   MOVEL     *blanks       wVATNOT_A
048200060331     C                   MOVEL     *blanks       wVATNOT_B
048300060331     C                   MOVEL     *blanks       wVATNOT_E
048400080617     C                   EVAL      wFlgCAS = *blanks
048500030822     C*
048600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048700030822     C                   CLEAR                   FIVAB000
048800040119     C                   CLEAR                   FIVAT000
048900030822     C                   CLEAR                   SkSplitCSV
049000030822     C                   EVAL      i = 1
049100030822     C                   EVAL      posDa = *zeros
049200030822     C                   EVAL      posA  = *zeros
049300030822     C*
049400030822     C* Reimposto i valori di default
049500030822     C                   EXSR      DEFCAM
049600030822     C*
049700010330     C                   ENDSR
049800010330     C*----------------------------------------------------*
049900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050000010330     C*----------------------------------------------------*
050100010330     C     DEFCAM        BEGSR
050200030822     C*
050300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050400030715     C* e delimitatore testo.
050500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050800030822     C*
050900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
051100030715     C                   SETON                                        50
051200030715     C                   ELSE
051300030715     C                   SETOFF                                       50
051400030715     C                   ENDIF
051500030822     C*
051600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051700030822     C                   EVAL      posDaDft = 1
051800030822     C                   EVAL      posADft  = 0
051900030822     C                   EVAL      wGiroDft = 0
052000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052100030822     C                             posDaDft > 0
052200030822     C*
052300030822     C* Gestisco il 1° giro
052400030822     C                   IF        wGiroDft = 0
052500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052700030822     C* Incremento il contatore dei "giri"
052800030822     C                   EVAL      wGiroDft = 1
052900030822     C                   ELSE
053000030822     C                   EVAL      posDaDft = posADft
053100030822     C                   ENDIF
053200030822     C* Eseguo lo scan x trovare la fine del campo corrente
053300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053400030822     C*
053500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053700030822     C                   IF        posDaDft > 0 AND
053800030822     C                             posADft  > 0
053900030822     C* NCL
054000030822     C                   IF        %subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):1:3)
054300030822     C                             = 'NCL'
054400030822     C                   EVAL      PiStr=%trim(%subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700030822     C                   EXSR      CHKNUM
054800030822     C                   IF        PiInt=*on
054900030822     C                   Z-ADD     PiVal         VABNCL
055000030822     C                   ENDIF
055100030822     C                   ENDIF
055200030822     C* CCM
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'CCM'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiInt=*on
056200030822     C                   Z-ADD     PiVal         VABCCM
056300030822     C                   ENDIF
056400030822     C                   ENDIF
056500030822     C* LNP
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'LNP'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABLNP
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* NRS
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'NRS'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABNRS
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* CTR
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'CTR'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABCTR
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* PKB
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'PKB'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiNum=*on
061400030822     C                   Z-ADD     PiVal         VABPKB
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* VLB
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'VLB'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiNum=*on
062700030822     C                   Z-ADD     PiVal         VABVLB
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* QFT
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'QFT'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000030822     C                   Z-ADD     PiVal         VABQFT
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* CBO
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'CBO'
064800030822     C                   EVAL      VABCBO=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200030822     C* TSP
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'TSP'
065700030822     C                   EVAL      VABTSP=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* VAS
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'VAS'
066600030822     C                   EVAL      VABVAS=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* VCA
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'VCA'
067500030822     C                   EVAL      VABVCA=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900070925     C* TIC
068000070925     C                   IF        %subst(
068100070925     C                             %subst(vlrppt:posDaDft+1:
068200070925     C                             posADft-posDaDft-1):1:3)
068300070925     C                             = 'TIC'
068400070925     C                   EVAL      VABTIC=%trim(%subst(
068500070925     C                             %subst(vlrppt:posDaDft+1:
068600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700070925     C                   ENDIF
068800030822     C* GCA
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'GCA'
069300030822     C                   EVAL      VABGCA=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* CTM
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'CTM'
070200030822     C                   EVAL      VABCTM=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* FFD
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'FFD'
071100030822     C                   EVAL      VABFFD=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* VAD
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'VAD'
072000030822     C                   EVAL      VABVAD=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* GMA
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'GMA'
072900030822     C                   EVAL      VABGMA=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* GGA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'GGA'
073800030822     C                   EVAL      VABGGA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* GVA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GVA'
074700030822     C                   EVAL      VABGVA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* TC1
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'TC1'
075600030822     C                   EVAL      VABTC1=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* TC2
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'TC2'
076500030822     C                   EVAL      VABTC2=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900040714     C* VATTRC
077000040714     C                   IF        %subst(
077100040714     C                             %subst(vlrppt:posDaDft+1:
077200040714     C                             posADft-posDaDft-1):1:3)
077300040714     C                             = 'TRC'
077400040714     C                   EVAL      VATTRC=%trim(%subst(
077500040714     C                             %subst(vlrppt:posDaDft+1:
077600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700040714     C                   ENDIF
077800030822     C* ...
077900030822     C                   ENDIF
078000030822     C                   ENDDO
078100020204     C*
078200010330     C                   ENDSR
078300010607     C*----------------------------------------------------*
078400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078500010607     C*----------------------------------------------------*
078600010607     C     IMPVAB        BEGSR
078700010607     C*
078800010607     C                   SETOFF                                       3132
078900030515     C*
079000010607     C                   MOVEL     datcor        VABAAS
079100010607     C                   MOVE      datcor        VABMGS
079200010607     C*
079300030715     C**********
079400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079500030715     C**********
079600030715     C                   IF        wGiro = *zeros
079700080716     C*
079800080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
079900080716     C                   EVAL      depvinDTA = vinDTA
080000040113     C*
080100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080300040510     C                             %trim(intcol(3))<>*blanks
080400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080500040113     C                                      %trim(intcol(3))
080600070823     C     ';':CharCSV   XLATE     vindta        vindta
080700040510     C                   ENDIF
080800080626     C*
080900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081000031201     C     minu:maiu     XLATE     vindta        vindta
081100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081200030715     C                   DOW       posDa <= %len(%trim(vindta))
081300030715     C*
081400030715     C* Gestisco il 1° campo
081500030715     C                   IF        i = 1
081600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081700030715     C                   EVAL      posDa = 1
081800030715     C* Eseguo lo scan x trovare la fine del primo campo
081900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082000030715     C* A questo punto "estraggo" il campo corrente
082100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082200030715     C                                                    (posA-posDa))
082300030715     C* X i campi successivi al 1°
082400030715     C                   ELSE
082500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082600030715     C                   EVAL      posDa = posA + 1
082700030715     C* Eseguo lo scan x trovare la fine del campo corrente
082800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082900030715     C* Gestisco l'ultimo campo
083000030715     C                   IF        posA = *zeros
083100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C                   LEAVE
083600030715     C                   ELSE
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   ENDIF
084100030715     C                   ENDIF
084200030715     C* Incremento il contatore d campo
084300030715     C                   EVAL      i = i +1
084400030715     C                   ENDDO
084500030715     C*
084600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084700030715     C* delimitazione testo
084800030715     C                   EVAL      i = 1
084900030715     C                   DOW       i < %elem(SkSplitFLD)
085000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085200030715     C                   EVAL      i = i + 1
085300030715     C                   ENDDO
085400030715     C                   EVAL      wGiro = 1
085500080716     C* Ripristino la 1° riga originale
085600080716     C                   EVAL      vinDTA = depvinDTA
085700030715     C                   ELSE
085800030715     C                   EVAL      wGiro = 2
085900031204     C**********
086000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086100031204     C* d separatore campo
086200031204     C**********
086300031204     C                   EVAL      vindta = %trim(vindta)
086400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086500031204     C                   EVAL      vindta = %subst(vindta:2)
086600031204     C                   ENDDO
086700031204     C*
086800031204     C                   Z-ADD     *zeros        lunghInput        4 0
086900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087300031204     C                   ENDDO
087400030715     C**********
087500030715     C* Effettuo lo split del campo dati d input
087600030715     C**********
087700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087800030715     C                   DOW       posDa <= %len(%trim(vindta))
087900030715     C*
088000030715     C* Gestisco il 1° campo
088100030715     C                   IF        i = 1
088200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088300030715     C                   EVAL      posDa = 1
088400030715     C* Eseguo lo scan x trovare la fine del primo campo
088500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088600030715     C* A questo punto "estraggo" il campo corrente
088700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088800030715     C                                                    (posA-posDa))
088900030715     C* X i campi successivi al 1°
089000030715     C                   ELSE
089100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089200030715     C                   EVAL      posDa = posA + 1
089300031204     C* Verifico che nn vi sia il campo nn valorizzato
089400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089500031204     C* Se campo nn valorizzato skippo al prossimo
089600031204     C                   EVAL      SkSplitCSV(i) = *blanks
089700031204     C                   EVAL      posA  = posA + 1
089800031204     C                   ELSE
089900030715     C* Eseguo lo scan x trovare la fine del campo corrente
090000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090100030715     C* Gestisco l'ultimo campo
090200030715     C                   IF        posA = *zeros
090300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090400030715     C* A questo punto "estraggo" il campo corrente
090500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090600030715     C                                                    (posA-posDa))
090700030715     C                   LEAVE
090800030715     C                   ELSE
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   ENDIF
091300031204     C                   ENDIF
091400030715     C                   ENDIF
091500030715     C* Incremento il contatore d campo
091600030715     C                   EVAL      i = i +1
091700030715     C                   ENDDO
091800030715     C*
091900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092000030715     C* delimitazione testo
092100030715     C                   EVAL      i = 1
092200030715     C                   DOW       i < %elem(SkSplitCSV)
092300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092500030715     C                   EVAL      i = i + 1
092600030715     C                   ENDDO
092700030715     C*
092800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092900030715     C                   EVAL      i = 1
093000030715     C                   DOW       i < %elem(SkSplitFLD)
093100030715     C*
093200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093400030715     C                             SkSplitCSV(i) <> *blanks
093500030721     C*** ATB
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093700030721     C                   EVAL      VABATB = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030715     C*** RSD
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094400030715     C* ==
094500030715     C                   ENDIF
094600030721     C*** RD2
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030715     C*** IND
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095200030715     C                   EVAL      VABIND = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** LOD
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** PRD
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030721     C*** NZD
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030715     C*** RMA
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** NOT
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** NT2
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** VCA
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** VAS
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** FFD
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030721     C*** GC1
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** GC2
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** TSP
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100100030721     C                   ENDIF
100200040510     C*** CBO
100300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100500040510     C                   ENDIF
100600030721     C*** NAS
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** TIC
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** GCA
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** XCO
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** CTM
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** TCR
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** CTS
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** FTM
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** VAD
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GMA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** GGA
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GVA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** TC1
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** TC2
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** SCL
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** RMO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** NMO
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*
107500030721     C* Reperisco quindi i campi numerici...
107600040510     C*** NSP
107700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107800040510     C                   EVAL      PiStr=SkSplitCSV(i)
107900040510     C                   EXSR      CHKNUM
108000040510     C                   IF        PiInt=*on
108100040510     C                   Z-ADD     PiVal         VABNSP
108200040510     C                   ELSE
108300040510     C                   SETON                                        31
108400040510     C                   EVAL      VABNSP = *zeros
108500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108600040510     C                             + ' ' + 'VABNSP'
108700040510     C                   ENDIF
108800040510     C                   ENDIF
108900071106     C*** RMN
109000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109100071106     C                   EVAL      PiStr=SkSplitCSV(i)
109200071106     C                   EXSR      CHKNUM
109300071106     C                   IF        PiInt=*on
109400071106     C                   Z-ADD     PiVal         VABRMN
109500071106     C                   ELSE
109600071106     C                   SETON                                        32
109700071106     C                   EVAL      VABRMN = *zeros
109800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109900071106     C                             + ' ' + 'VABRMN'
110000071106     C                   ENDIF
110100071106     C                   ENDIF
110200030721     C*** CAD
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiInt=*on
110700030721     C                   Z-ADD     PiVal         Num5_0
110800030721     C                   MOVEL(p)  Num5_0        VABCAD
110900030721     C                   ELSE
111000030721     C                   SETON                                        32
111100030721     C                   EVAL      VABCAD = *zeros
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABCAD'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** NCL
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on
112100030721     C                   Z-ADD     PiVal         VABNCL
112200030721     C                   ELSE
112300030721     C                   SETON                                        32
112400030721     C                   EVAL      VABNCL = *zeros
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600040122     C                             + ' ' + 'VABNCL'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** PKB
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiNum=*on
113400030721     C                   Z-ADD     PiVal         VABPKB
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      VABPKB = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABPKB'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** CAS
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114400080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
114500080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
114600080617     C                             %trim(SkSplitCSV(i)) <> '0'
114700080125     C                   EVAL      wFlgCAS = '1'
114800080125     C                   ENDIF
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiNum=*on
115200030721     C                   Z-ADD     PiVal         VABCAS
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABCAS = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABCAS'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** CCM
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on      AND
116500030721     C                             PiVal<=9999999 AND
116600030721     C                             PiVal>*zeros
116700030721     C                   Z-ADD     PiVal         VABCCM
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABCCM = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200040510     C                             + ' ' + 'VABCCM'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** LNP
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiInt=*on    AND
118000030721     C                             PiVal<=999   AND
118100030721     C                             PiVal>*zeros
118200030721     C                   Z-ADD     PiVal         VABLNP
118300030721     C                   ELSE
118400040715     C                   SETON                                        32
118500030721     C                   EVAL      VABLNP = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700040510     C                             + ' ' + 'VABLNP'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** NRS
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on AND
119500030721     C                             PiVal<=99
119600030721     C                   Z-ADD     PiVal         VABNRS
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABNRS'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** LNA
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiInt=*on    AND
120800030721     C                             PiVal<=999   AND
120900030721     C                             PiVal>*zeros
121000030721     C                   Z-ADD     PiVal         VABLNA
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      VABLNA = *zeros
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABLNA'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** CTR
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on  AND
122300030721     C                             PiVal<=999
122400030721     C                   Z-ADD     PiVal         VABCTR
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABCTR'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** IAS
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiNum=*on
123600030721     C                   Z-ADD     PiVal         VABIAS
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABIAS'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** VLB
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiNum=*on
124800030721     C                   Z-ADD     PiVal         VABVLB
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABVLB'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** QFT
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiNum=*on
126000030721     C                   Z-ADD     PiVal         VABQFT
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABQFT'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** NCD
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on      AND
127200030721     C                             PiVal<=9999999
127300030721     C                   Z-ADD     PiVal         VABNCD
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABNCD'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** NCA
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on      AND
128500030721     C                             PiVal<=9999999
128600030721     C                   Z-ADD     PiVal         VABNCA
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABNCA'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** ZNC
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on AND
129800030721     C                             PiVal<=99
129900030721     C                   Z-ADD     PiVal         VABZNC
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABZNC'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** DCR
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on       AND
131100030721     C                             PiVal<=99999999
131200030721     C                   Z-ADD     PiVal         VABDCR
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABDCR'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** HCR
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on   AND
132400030721     C                             PiVal<=9999
132500030721     C                   Z-ADD     PiVal         VABHCR
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABHCR'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** VMD
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiNum=*on
133700030721     C                   Z-ADD     PiVal         VABVMD
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABVMD'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** ANT
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiInt=*on        AND
134900030721     C                             PiVal<=999999999
135000030721     C                   Z-ADD     PiVal         VABANT
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABANT'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700030721     C*** CMO
135800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on
136200030721     C                   Z-ADD     PiVal         Num5_0
136300030721     C                   MOVEL(p)  Num5_0        VABCMO
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      VABCMO = *zeros
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABCMO'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100040510     C*
137200040510     C* Reperisco i campi del FIVAT
137300040510     C*** VATNOT
137400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137600040510     C                   ENDIF
137700040510     C*** VATTRC
137800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138000040510     C                   ENDIF
138100060331     C*** VATNOT_A
138200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138400060331     C                   ENDIF
138500060331     C*** VATNOT_B
138600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138800060331     C                   ENDIF
138900060331     C*** VATNOT_E
139000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139200060331     C                   ENDIF
139300070823     C*
139400071029     C***  ===>  Gestione campi "particolari" dentro ciclo
139500071204     C*
139600080716     C*** NSP/RMN
139700080617     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
139800080716     C                   EVAL      PiStr=SkSplitCSV(i)
139900071204     C                   EXSR      CHKNUM
140000071204     C                   IF        PiInt=*on
140100071204     C                   Z-ADD     PiVal         VABNSP
140200071204     C                   Z-ADD     PiVal         VATNSP
140300071204     C                   Z-ADD     PiVal         VABRMN
140400071204     C                   ELSE
140500071204     C                   SETON                                        32
140600071204     C                   EVAL      VABNSP = *zeros
140700071204     C                   EVAL      VATNSP = *zeros
140800071204     C                   EVAL      VABRMN = *zeros
140900071204     C                   EVAL      vinmsg = %trimr(vinmsg)
141000071204     C                             + ' ' + 'VABNSP VATNSP VABRMN'
141100071204     C                   ENDIF
141200071204     C                   ENDIF
141300080716     C*** W_VABRSD
141400080716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141500080716     C                   IF        %len(%trim(SkSplitCSV(i))) > 35
141600080716     C                   EVAL      VABRSD = %subst(%trim(SkSplitCSV(i)):1:35)
141700080716     C                   EVAL      VABRD2 = %subst(%trim(SkSplitCSV(i)):36)
141800080716     C                   ELSE
141900080716     C                   EVAL      VABRSD = %trim(SkSplitCSV(i))
142000080716     C                   ENDIF
142100080716     C     '@':'A'       XLATE     VABRSD        VABRSD
142200080716     C                   ENDIF
142300080716     C*** W_VABNOT
142400080716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
142500080716     C                   IF        %len(%trim(SkSplitCSV(i))) > 35
142600080716     C                   EVAL      VABNOT = %subst(%trim(SkSplitCSV(i)):1:35)
142700080716     C                   EVAL      VABNT2 = %subst(%trim(SkSplitCSV(i)):36)
142800080716     C                   ELSE
142900080716     C                   EVAL      VABNOT = %trim(SkSplitCSV(i))
143000080716     C                   ENDIF
143100080716     C                   ENDIF
143200070925     C*
143300071029     C***  <===  -----------------------------------------
143400040510     C*
143500030715     C                   ENDIF
143600030715     C                   EVAL      i = i + 1
143700030715     C                   ENDDO
143800070925     C*
143900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144000080125     C*
144100070925     C*
144200071029     C***  <===  -----------------------------------------
144300070925     C*
144400070925     C* Considerazioni finali su CBO/CAS
144500080125     C                   IF        wFlgCAS = '1'
144600070925     C                   IF        VABCBO = '1'
144700070925     C                   EVAL      VABCBO = '4'
144800070925     C                   ENDIF
144900070925     C                   IF        VABCBO = '2'
145000070925     C                   EVAL      VABCBO = '6'
145100070925     C                   ENDIF
145200070925     C                   ENDIF
145300070925     C***  <===  ----------------------------
145400040510     C*
145500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145600040510     C                   IF        VABNSP = *zeros
145700040510     C                   SETON                                        31
145800040510     C                   ENDIF
145900040715     C*
146000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146100040715     C                   IF        VABLNP = *zeros
146200040715     C                   SETON                                        31
146300040715     C                   ENDIF
146400040510     C*
146500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146600040510     C                   IF        VABRMN = *zeros
146700040510     C                   Z-ADD     VABNSP        VABRMN
146800040510     C                   ENDIF
146900040510     C*
147000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147100040510     C                   IF        VABRMA = *blanks
147200040714     C                   MOVEL     VABRMN        VABRMA
147300040510     C                   ENDIF
147400040128     C*
147500040510     C* Se provincia nn valorizzata la reperisco
147600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
147700040510     C                   IF        VABPRD = *blanks
147800040128     C                   CLEAR                   TISI95DS
147900040128     C                   EVAL      I95TCN = '3'
148000040128     C                   Z-ADD     datcor        I95DAT
148100040128     C                   EVAL      I95NAR = VABNZD
148200040128     C                   EVAL      I95CAP = VABCAD
148300040128     C                   EVAL      I95LOC = VABLOD
148400040128     C                   CALL      'TISI95R'
148500040128     C                   PARM                    TISI95DS
148600040128     C                   EVAL      VABPRD = O95PRV
148700040510     C                   ENDIF
148800020204     C*
148900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149000020204     C                   EXSR      CHKIMPDIV
149100030715     C                   ENDIF
149200020204     C*
149300010607     C                   ENDSR
149400010607     C*----------------------------------------------------*
149500010601
149600020204
149700020204     C*----------------------------------------------------*
149800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149900020204     C*----------------------------------------------------*
150000020204     C     CHKIMPDIV     BEGSR
150100020204     C*
150200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150300020204     C                   Z-ADD     *zeros        wrkDec            9 9
150400020204     C*
150500020204     C* Come prima cosa effettuo considerazioni sulla divisa
150600020204     C                   IF        vabIAS > *zeros
150700020204     C                   IF        vabVAS <> 'EUR'
150800020204     C                   EVAL      vabVAS =  'ITL'
150900020204     C                   ENDIF
151000020204     C                   ENDIF
151100020204     C*
151200020204     C                   IF        vabCAS > *zeros
151300020204     C                   IF        vabVCA <> 'EUR'
151400020204     C                   EVAL      vabVCA =  'ITL'
151500020204     C                   ENDIF
151600020204     C                   ENDIF
151700020204     C*
151800020204     C                   IF        vabVMD > *zeros
151900020204     C                   IF        vabVAD <> 'EUR'
152000020204     C                   EVAL      vabVAD =  'ITL'
152100020204     C                   ENDIF
152200020204     C                   ENDIF
152300020204     C*
152400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152500020204     C                   Z-ADD     vabIAS        wrkDec
152600020204     C                   IF        wrkDec > *zeros
152700020204     C                   IF        vabVAS = 'ITL'
152800020204     C                   EVAL      vabIAS = *zeros
152900020204     C                   ENDIF
153000020204     C                   ENDIF
153100020204     C*
153200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153300020204     C                   Z-ADD     vabCAS        wrkDec
153400020204     C                   IF        wrkDec > *zeros
153500020204     C                   IF        vabVCA = 'ITL'
153600020204     C                   EVAL      vabCAS = *zeros
153700020204     C                   ENDIF
153800020204     C                   ENDIF
153900020204     C*
154000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154100020204     C                   Z-ADD     vabVMD        wrkDec
154200020204     C                   IF        wrkDec > *zeros
154300020204     C                   IF        vabVAD = 'ITL'
154400020204     C                   EVAL      vabVMD = *zeros
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C                   ENDSR
154900020204     C***
155000080617
155100080617
155200080617
155300080617     C*----------------------------------------------------*
155400080617     C*  CONTROLLO NUMERICITA' CAMPI
155500080617     C*----------------------------------------------------*
155600080617     C     CHKNUM        BEGSR
155700080617     C*
155800080617     C                   IF        PiDecChr = *blanks
155900080617     C                   EVAL      PiDecChr = CharNUM
156000080617     C                   ENDIF
156100080617     C*
156200080617     C                   callp     UBISNUM_Check(PiStr
156300080617     C                                          :PiDecChr
156400080617     C                                          :PiVal
156500080617     C                                          :PiNum
156600080617     C                                          :PiInt)
156700080617     C*
156800080617     C                   ENDSR
156900080617     C***
157000010330
157100010601
157200010601
157300010601
157400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157500010601     C     repfil        BEGSR
157600010601     C*
157700010601     C                   if        invfil = *zeros and
157800010601     C                             depfil > *zeros and
157900010629     C                             (vinflg = *blanks or
158000010629     C                              vinflg = *zeros)
158100010601     C
158200010601     C                   eval      invfil = depfil
158300010601     C                   endif
158400010601     C*
158500010601     C                   if        depfil <> invfil and
158600010601     C                             invfil > *zeros
158700010601     C                   eval      flgMulti = '1'
158800010601     C                   if        vinflg = *blanks
158900010601     C                   add       1             cntNonEl
159000010601     C                   endif
159100010601     C                   endif
159200010601     C*
159300010601     C                   if        vinflg = '2'
159400010601     C                   eval      flgStato = '2'
159500010601     C                   endif
159600010601     C*
159700010601     C                   ENDSR
159800010601     C***
159900010601
160000010601
160100010601
160200010330
160300040119      /TITLE Invio dei dati al punto operativo.
160400040119     C     invio         BEGSR
160500040119     C*
160600040119     C* 1° invio FIVAT
160700040119     C                   reset                   dscmz
160800040119     C                   move      vlrpoi        cmzdst
160900040119     C                   eval      cmzfld = 'FIVATWWR'
161000040119     C                   eval      cmzmbd = vlrhdl
161100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161200040119     C***                if        prmfir = *blanks
161300040119     C                   eval      cmzfla = 'FIVAT00F'
161400040119     C                   eval      cmzmba = 'FIVAT00F'
161500040119     C***                else
161600040119     C***                eval      cmzfla = prmfir
161700040119     C***                eval      cmzmba = prmfir
161800040119     C***                endif
161900040119     C                   eval      cmznrr = *zeros
162000040119     C                   move      §ctrokvt      cmznrr
162100040119     C                   eval      cmzlba = vlrfl1
162200040119     C                   call(e)   'TIS711C'
162300040119     C                   parm                    dscmz
162400040119     C                   parm      *blanks       esito
162500040119     C                   if        %error
162600040119     C                             or cmzerr = '1'
162700040119     C                             or esito  = '1'
162800040119     C                   eval      wrkesito = '3'
162900040119     C                   else
163000040119     C*
163100040119     C* 2° invio FIVAB
163200040119     C                   reset                   dscmz
163300040119     C                   move      vlrpoi        cmzdst
163400040119     C                   eval      cmzfld = vlrfou
163500040119     C                   eval      cmzmbd = vlrhdl
163600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163700040119     C***                if        prmfir = *blanks
163800040119     C                   eval      cmzfla = 'FIVAB00F'
163900040119     C                   eval      cmzmba = 'FIVAB00F'
164000040119     C***                else
164100040119     C***                eval      cmzfla = prmfir
164200040119     C***                eval      cmzmba = prmfir
164300040119     C***                endif
164400040119     C                   eval      cmznrr = *zeros
164500040119     C                   move      §ctrokvb      cmznrr
164600040119     C                   eval      cmzlba = vlrfl1
164700040119     C                   call(e)   'TIS711C'
164800040119     C                   parm                    dscmz
164900040119     C                   parm      *blanks       esito
165000040119     C                   if        %error
165100040119     C                             or cmzerr = '1'
165200040119     C                             or esito  = '1'
165300040119     C                   eval      wrkesito = '3'
165400040119     C                   endif
165500040119     C                   endif
165600040119     C*
165700040119     C                   ENDSR
165800040119     C***
165900010601
166000010601
166100010601
166200010601
166300010601      /TITLE Invio dei dati al punto operativo.
166400010601     C     opeini        BEGSR
166500010601     C*
166600010601     C* Inizializzo flag e contatori operativi
166700010601     C                   movel     '0'           flgGiro           1
166800010601     C                   movel     '0'           flgMulti          1
166900010601     C                   movel     '1'           flgStato          1
167000010615     C                   movel     '0'           flgOk             1
167100010601     C                   z-add     *zeros        cntNonEl         10 0
167200010601     C                   z-add     *zeros        depfil            3 0
167300010601     C                   z-add     *zeros        invfil            3 0
167400010601     C*
167500010601     C                   ENDSR
167600010601     C***
167700070326
167800070326
167900070326
168000070326
168100080716     C***  *pssr         BEGSR
168200070329     C*
168300080716     C***                if        %open(tivin00r)
168400080716     C***                close     tivin00r
168500080716     C***                endif
168600080716     C***                if        %open(fivabwwr)
168700080716     C***                close     fivabwwr
168800080716     C***                endif
168900080716     C***                if        %open(fivatwwr)
169000080716     C***                close     fivatwwr
169100080716     C***                endif
169200070326     C*
169300070326     C* Effettuo la chiamata al CLLE preposto
169400080716     C***                call(e)   'TITVVTC'
169500080716     C***                parm                    parccm
169600080716     C***                parm                    parmbr
169700080716     C***                parm      '2'           paropz
169800070326     C*
169900080716     C***                eval      wrkesito = '2'
170000070404     C*
170100080716     C***                seton                                        LR
170200070326     C*
170300080716     C***                ENDSR     '*CANCL'
170400070326     C***
170500070326
170600070326
170700010330
170800010330
170900000613     C     *inzsr        BEGSR
171000990910     C*
171100990910     C     *entry        plist
171200990920     C                   parm                    tivlrds
171300990921     C                   parm      wrkesito      esito
171400000724     C                   parm                    prmlit
171500000710     C                   parm                    prmfir
171600010330     C*
171700010330     C* CALCOLA LA DATA CORRENTE
171800010330     C                   time                    wn14             14 0
171900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
172000010330     C                   z-add     wn8           g08dat
172100010330     C                   z-add     *zeros        g08inv
172200010330     C                   movel     '0'           g08err
172300010330     C                   call      'XSRDA8'
172400010330     C                   parm                    wlbda8
172500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
172600000613     C*
172700000613     C                   ENDSR
172800000613     C***
172900990908
173000080715     Otitv1i0p  E            riepilogo         2
173100990915     O                                              'Upload via Internet'
173200990915     O                                           +1 'Traduzione TIVIN00R -'
173300021025     O                                           +1 'FIVABWWR'
173400990915     O                                           +1 'Testate bolle'
173500990915     O          E            riepilogo   2
173600990915     O                       wrkdata
173700990915     O                       wrkora              +1
173800990915     O                       procname            +1
173900990915     O          E            riepilogo   2
174000990915     O                                              'Cliente..................:'
174100010605     O                       VABCCM        z     +1
174200990915     O          E            riepilogo   2
174300990920     O                                              'Riferimento Strategi.....:'
174400990920     O                       vlrhdl              +1
174500990915     O          E            riepilogo   2
174600990915     O                                              'Giusti...................:'
174700040119     O                       §CTROKVB      2   +  1
174800990915     O          E            riepilogo   2
174900990915     O                                              'Sbagliati e corretti.....:'
175000971022     O                       §CTRMO        2   +  1
175100990915     O          E            riepilogo   2
175200990915     O                                              'Sbagliati e scartati.....:'
175300971022     O                       §CTRNO        2   +  1
175400000613
175500080715     Otitv1i0ps E            testdett          2
175600000613     O                                              'Upload via Internet'
175700000613     O                                           +1 'Traduzione TIVIN00R -'
175800021025     O                                           +1 'FIVABWWR'
175900000613     O                                           +1 'Testate bolle'
176000000616     O          E            testdett    3
176100000613     O                                           +2 'N° rec'
176200000613     O                                           +3 'Anteprima contenuto'
176300000616     O          E            rigadett    2
176400000613     O                       rrnum               +2
176500000621     O                       recko               +3
176600000616     O          E            findett     2
176700000613     O                       wrkdata
176800000613     O                       wrkora              +1
176900000613     O                       procname            +1
177000000616     O          E            findett     2
177100000613     O                                              'Cliente..................:'
177200010605     O                       VABCCM        z     +1
177300000616     O          E            findett     2
177400000613     O                                              'Riferimento Strategi.....:'
177500000613     O                       vlrhdl              +1
177600000616     O          E            findett     2
177700000613     O                                              'Giusti...................:'
177800040119     O                       §CTROKVB      2   +  1
177900000616     O          E            findett     2
178000000613     O                                              'Sbagliati e corretti.....:'
178100000613     O                       §CTRMO        2   +  1
178200000616     O          E            findett     2
178300000613     O                                              'Sbagliati e scartati.....:'
178400000613     O                       §CTRNO        2   +  1
178500000613** CMD - COMANDI CL
178600080715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
178700080715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
178800080715DLTOVR FILE(TITV1I0P TITV1I0PS) LVL(*)
178900010606
179000000613
179100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179200080716VABNZD;VABCAD;VABLOD;W_VABRMN;VABRMA;W_VABRSD;VABIND;VABVLB;
179300080716VABPKB;VABNCL;FILL02;FILL03;FILL04;FILL05;FILL06;W_VABNOT;FILL07
179400080715
