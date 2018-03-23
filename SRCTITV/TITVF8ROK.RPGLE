000100041108      /TITLE Upload via Internet: traduzione in FIVABWWR/FIBATWWR. (Cliente ARTE' - Cod. 0012232)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700041108     FFIVATwwr  O    E             DISK    usropn
000800050504     Ftitvf8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000050504     Ftitvf8ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700040726     D*------------
001800040726     D* SCHIEDE A PROGRAMMA
001900040726     D*------------
002000040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800050504     D tisi95ds      e ds
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D wrkdata         s               d
003400010330     D wrkora          s               t
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600010330     D recko           s            150    INZ(*blanks)
003700011113     D depcmd          s            150    INZ(*blanks)
003800040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003900041108     d wbarcode        s                   like(vatnot) inz(*blanks)
004000041108     d wtel            s                   like(vatnot) inz(*blanks)
004100041108     d wref            s                   like(vatnot) inz(*blanks)
004200050504     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004300041108     D parccm          s              8    INZ(*blanks)
004400041108     D parmbr          s             10    INZ(*blanks)
004500041108     D paropz          s              1    INZ(*blanks)
004600041108     D chkcall         s              1    INZ(*blanks)
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
005900041122     d savrmn          s                   like(vabrmn)
006000030822     D*------------------
006100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006200030822     D*------------------
006300030822     D posDaDft        S              3  0 INZ(*zeros)
006400030822     D posADft         S              3  0 INZ(*zeros)
006500030822     D j               s              3  0 INZ(1)
006600030822     D wGiroDft        s              1  0 INZ(*zeros)
006700010330     D*------------------
006800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006900010330     D*------------------
007000010330     D WLBDA8          DS                  INZ
007100010330     D  G08DAT                 1      8  0
007200010330     D  G08INV                 9     16  0
007300010330     D  G08ERR                17     17
007400010330     D  G08TGI                18     22  0
007500031201     D*------------------
007600031201     D* Costanti
007700031201     D*------------------
007800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008000010330
008100041122     D*------------------
008200041122     D* DS REPERIMENTO NUMERATORE
008300041122     D*------------------
008400041122     D trul33ds      e ds                  inz
008500041122     D*------------------
008600041122     D* DS ARCHITETTURA
008700041122     D*------------------
008800041122     D kpjba         e ds                  inz
008900041122     D*------------------
009000010330
009100010330
009200990915     C                   time                    wrkdata
009300990915     C                   time                    wrkora
009400000913     C                   reset                   rrnum
009500990921     C                   reset                   esito
009600990921     C                   reset                   wrkesito
009700010601     C*
009800010601     C                   exsr      opeini
009900010605     C                   exsr      rwvab
010000010601     C*
010100010601     C                   seton                                        lr
010200010601
010300010601
010400010601
010500010601
010600010601     C*--------------------------------------------------------
010700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010800010601     C*--------------------------------------------------------
010900010601     C     PREELA        BEGSR
011000010601     C*
011100010601     C* SE OCCORRE SPEDIRE IN FILIALE
011200010601     C                   if        invfil <> *zeros and
011300010601     C                             flgGiro = '0'
011400010601     C*
011500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011600010601     C                   eval      flgGiro = '1'
011700000724     C*
011800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012000010601     C     invfil        chain     azorg01l
012100000724     C                   if        %found
012200000724     C                   movel(p)  CMD(1)        depcmd
012300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012400000724     C*
012500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000724     C                   PARM                    depcmd
012900000724     C                   PARM                    LENGH
013000000724     C*
013100000724     C                   endif
013200000616     C*
013300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013400000616     C                   movel(p)  CMD(2)        depcmd
013500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013600000616     C*
013700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013800011113     C                   Z-ADD     150           LENGH            15 5
013900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014000000616     C                   PARM                    depcmd
014100000616     C                   PARM                    LENGH
014200000613     C*
014300050504     C                   if        not %open(titvf8ps)
014400050504     C                   open      titvf8ps
014500030715     C                   except    testdett
014600000613     C                   endif
014700010601     C*
014800010601     C                   endif
014900010601     C*
015000010601     C                   ENDSR
015100010601     C***
015200010601
015300010601
015400010601
015500010601
015600010601     C*--------------------------------------------------------
015700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015800010601     C*--------------------------------------------------------
015900010601     C     ENDELA        BEGSR
016000990915     C*
016100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016200041108     C*
016300041108     C* Effettuo la chiamata al CLLE preposto
016400041108     C                   call(e)   'TITVVTC'
016500041108     C                   parm                    parccm
016600041108     C                   parm                    parmbr
016700041108     C                   parm      '2'           paropz
016800050504     C*
016900050504     C* Effettuo lancio TISI95 solo x chiusura
017000050504     C                   CLEAR                   TISI95DS
017100050504     C                   EVAL      I95TLA = 'C'
017200050504     C                   CALL      'TISI95R'
017300050504     C                   PARM                    TISI95DS
017400000613     C*
017500050504     C                   if        %open(titvf8ps)
017600000616     C                   except    findett
017700050504     C                   close     titvf8ps
017800000613     C                   endif
017900000616     C*
018000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018100011113     C                   Z-ADD     150           LENGH            15 5
018200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018300000616     C                   PARM                    CMD(3)
018400000616     C                   PARM                    LENGH
018500000616     C*
018600010601     C                   ENDSR
018700010601     C***
018800010601
018900010601
019000010601
019100010601
019200000613
019300000613     C*--------------------------------------------------------
019400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019500000613     C*--------------------------------------------------------
019600000621     C     STPR          BEGSR
019700000613     C*
019800050504     C                   if        not %open(titvf8p)
019900050504     C   50              open      titvf8p
020000990915     C                   endif
020100990915     C*
020200030715     C   50              except    riepilogo
020300990915     C*
020400050504     C                   if        %open(titvf8p)
020500050504     C   50              close     titvf8p
020600990914     C                   endif
020700000613     C*
020800000613     C                   ENDSR
020900000613     C***
021000000613
021100010330
021200010601
021300010601
021400010601
021500010330     C*--------------------------------------------------------
021600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021700010330     C*--------------------------------------------------------
021800010605     C     RWVAB         BEGSR
021900010330     C*
022000010330     C                   if        not %open(tivin00r)
022100010330     C                   open      tivin00r
022200010330     C                   endif
022300021025     C                   if        not %open(fivabwwr)
022400021025     C                   open      fivabwwr
022500010330     C                   endif
022600010330     C*
022700041108     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022800041108     C                   exsr      prevat
022900041108     C*
023000041108     C                   if        chkcall = '0'
023100041108     C*
023200041108     C                   if        not %open(fivatwwr)
023300041108     C                   open      fivatwwr
023400041108     C                   endif
023500041108     C*
023600041108     C                   clear                   §CTROKVB
023700041109     C                   clear                   §CTROKVT          7 0
023800010604     C                   clear                   §CTRMO
023900010604     C                   clear                   §CTRNO
024000041109
024100050504     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024200050504     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024300050504     C                   exsr      inzvar
024400050504     C                   exsr      impvab
024500010330     C*
024600030822     C                   do        *HIVAL
024700010330     C*
024800030822     C                   read      tivin00r                               70
024900010618     C*
025000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025100010618     C                   if        vinflg = '1'
025200010618     C                   eval      flgOk = '1'
025300010618     C                   endif
025400010618     C*
025500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025600040510     C                   z-add     *zeros        wLenVINDTA        4 0
025700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
025800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
025900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026100040510     C                   z-add     1             wFlgVINDTA
026200040510     C                   leave
026300040510     C                   endif
026400040510     C                   enddo
026500040510     C*
026600040218     C                   if        vindta > *blanks AND
026700040510     C                             wFlgVINDTA = 1
026800010330     C                   add       1             rrnum
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
027900010601     C*
028000010330     C                   exsr      inzvar
028100010605     C                   exsr      impvab
028200010601     C*
028300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028400010605     C                   eval      depfil = VABLNP
028500010601     C                   exsr      repfil
028600010601     C                   if        depfil = invfil
028700021025     C                   if        vlrpoi = 999
028800030822     C                   move(P)   invfil        VABFGS
028900041108     C                   move(P)   invfil        VATFGS
029000021025     C                   else
029100030822     C                   move(P)   vlrpoi        VABFGS
029200041108     C                   move(P)   vlrpoi        VATFGS
029300021025     C                   endif
029400010601     C*
029500010601     C                   exsr      PREELA
029600010601     C*
029700010604     C* Ebbene...
029800010604     C*
029900041108     C  N31              add       1             §CTROKVB          7 0
030000030822     C   32              add       1             §CTRMO            7 0
030100030822     C   31              add       1             §CTRNO            7 0
030200050504     C                   if        wGiro = 2
030300050504     C                   if        depspe =  *zeros OR
030400050504     C                             depspe <> vabNSP
030500050504     C  N31              exsr      wrivab
030600050504     C  N31              eval      depspe = vabNSP
030700050504     C                   endif
030800041025     C                   endif
030900010604     C*
031000010604     C                   if        *in31 = *off and
031100010604     C                             *in32 = *off
031200010604     C                   eval      vinflg = '1'
031300010604     C                   else
031400010604     C                   eval      recko = vindta
031500010604     C                   except    rigadett
031600010604     C                   eval      vinflg = '2'
031700010604     C                   endif
031800010604     C                   endif
031900010601     C*
032000010604     C                   endif
032100010604     C*
032200010330     C                   else
032300010330     C                   eval      vinflg = '1'
032400010330     C                   endif
032500010601     C*
032600010601     C  N70              update    tivin000
032700010330     C*
032800030822     C  N70              enddo
032900041025     C*
033000041109     C                   endif
033100010601     C*
033200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033300010601     C                   if        cntNonEl = *zeros or
033400010601     C                             flgMulti = '0'
033500010330     C* Se non ci sono record con errori ...
033600010601     C                   if        §ctrno = 0 and
033700010604     C                             §ctrmo = 0 and
033800010601     C                             flgStato <> '2'
033900010330     C* ... restituisco esito OK.
034000010330     C                   eval      wrkesito = '0'
034100010330     C                   else
034200041108     C                   if        §ctrokvb > 0
034300010330     C                   eval      wrkesito = '1'
034400010330     C                   else
034500010615     C                   if        flgOk = '0'
034600010615     C                   eval      wrkesito = '2'
034700010615     C                   else
034800010615     C                   eval      wrkesito = '6'
034900010615     C                   endif
035000010330     C                   endif
035100010330     C                   endif
035200010601     C                   else
035300010601     C                   eval      wrkesito = '9'
035400010601     C                   endif
035500010330     C*
035600010330     C                   if        %open(tivin00r)
035700010330     C                   close     tivin00r
035800010330     C                   endif
035900021025     C                   if        %open(fivabwwr)
036000021025     C                   close     fivabwwr
036100010330     C                   endif
036200041108     C                   if        %open(fivatwwr)
036300041108     C                   close     fivatwwr
036400041108     C                   endif
036500010601     C*
036600010601     C                   if        vlrpoi <> 999
036700010601     C                   eval      invfil = vlrpoi
036800010601     C                   endif
036900010330     C*
037000041108     C                   if        §ctrokvb > 0
037100010601     C                             and invfil > *zeros
037200010330     C                   exsr      invio
037300010330     C                   endif
037400010601     C*
037500010618     C                   if        flgGiro = '1'
037600010601     C                   exsr      endela
037700010618     C                   endif
037800010330     C*
037900010330     C                   ENDSR
038000010330     C***
038100041109
038200041109
038300041109
038400041109
038500041109
038600050504     C*----------------------------------------------------*
038700050504     C*  SCARICAMENTO BUFFER RECORDS VAB
038800050504     C*----------------------------------------------------*
038900050504     C     WRIVAB        BEGSR
039000050504     C*
039100050504     C                   write     fivab000                                     => scarico il VAB
039200050504     C*
039300050504     C                   ENDSR
039400050504
039500050504
039600050504
039700041108     C*----------------------------------------------------*
039800041108     C*  SCARICAMENTO BUFFER RECORDS VAT
039900041108     C*----------------------------------------------------*
040000041108     C     WRIVAT        BEGSR
040100041108     C*
040200041108     C* Attribuisco lo stesso numero numero spedizione del file testate
040300041108     C                   EVAL      VATNSP = VABNSP
040400041109      * Barcode
040500041108     C                   EVAL      VATTRC = 'E'
040600041108     C                   EVAL      VATNOT = wbarcode
040700041108     C                   WRITE     FIVAT000
040800041109     C                   ADD       1             §CTROKVT
040900041109      * Telefono
041000041109     c                   If        wtel <> *Blanks
041100041109     C                   EVAL      VATTRC = 'B'
041200041109     C                   EVAL      VATNOT = wtel
041300041109     C                   WRITE     FIVAT000
041400041109     C                   ADD       1             §CTROKVT
041500041109     c                   EndIf
041600041109      * Referente
041700041109     c                   If        wref <> *Blanks
041800041109     C                   EVAL      VATTRC = 'A'
041900041109     C                   EVAL      VATNOT = wref
042000041109     C                   WRITE     FIVAT000
042100041109     C                   ADD       1             §CTROKVT
042200041109     c                   EndIf
042300041108     C*
042400041108     C                   ENDSR
042500010601
042600010601
042700010601
042800041108     C*----------------------------------------------------*
042900041108     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043000041108     C*----------------------------------------------------*
043100041108     C     PREVAT        BEGSR
043200041108     C*
043300041108     C* Compongo il nome del membro da dare al FIVATWWR
043400041108     C                   eval      parmbr = vlrhdl
043500041108     C                   movel     'M'           parmbr
043600041108     C                   eval      parccm = vlrksc
043700041108     C                   eval      paropz = '1'
043800041108     C* Effettuo la chiamata al CLLE preposto
043900041108     C                   call(e)   'TITVVTC'
044000041108     C                   parm                    parccm
044100041108     C                   parm                    parmbr
044200041108     C                   parm                    paropz
044300041108     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044400041108     C                   if        %error
044500041108     C                   movel     '1'           chkcall
044600041108     C                   else
044700041108     C                   movel     '0'           chkcall
044800041108     C                   endif
044900041108     C*
045000041108     C                   ENDSR
045100041108
045200041108
045300041108
045400010330     C*----------------------------------------------------*
045500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045600010330     C*----------------------------------------------------*
045700010330     C     INZVAR        BEGSR
045800010330     C*
045900030822     C* Inizializzo variabili di wrk
046000020204     C                   Z-ADD     *zeros        Num5_0            5 0
046100041108     C                   clear                   wbarcode
046200041108     C                   clear                   wtel
046300041108     C                   clear                   wref
046400030822     C*
046500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046600030822     C                   CLEAR                   SkSplitCSV
046700041108     c                   CLEAR                   FIVAT000
046800050503     C                   CLEAR                   FIVAB000
046900030822     C                   EVAL      i = 1
047000030822     C                   EVAL      posDa = *zeros
047100030822     C                   EVAL      posA  = *zeros
047200030822     C*
047300030822     C* Reimposto i valori di default
047400030822     C                   EXSR      DEFCAM
047500030822     C*
047600010330     C                   ENDSR
047700010330     C*----------------------------------------------------*
047800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047900010330     C*----------------------------------------------------*
048000010330     C     DEFCAM        BEGSR
048100030822     C*
048200050504     C* Imposto i valori d default a programma dei campi fondamentali
048300050504     C                   EVAL      VABCCM = 0012232
048400050504     C                   EVAL      VATCCM = 0012232
048500050504     C                   EVAL      VABLNP = 001
048600050504     C                   EVAL      VATLNP = 001
048700050504     C                   EVAL      VABCTR = 001
048800050504     C                   EVAL      VABCTM = '7Q'
048900050504     C*
049000050504     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049100050504     C* e delimitatore testo.
049200050504     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049300050504     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049400050504     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
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
053600041025     C                   Z-ADD     PiVal         VABNCL
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
055000041108     C                   Z-ADD     PiVal         VATCCM
055100030822     C                   ENDIF
055200030822     C                   ENDIF
055300030822     C* LNP
055400030822     C                   IF        %subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):1:3)
055700030822     C                             = 'LNP'
055800030822     C                   EVAL      PiStr=%trim(%subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100030822     C                   EXSR      CHKNUM
056200030822     C                   IF        PiInt=*on
056300030822     C                   Z-ADD     PiVal         VABLNP
056400041108     C                   Z-ADD     PiVal         VATLNP
056500030822     C                   ENDIF
056600030822     C                   ENDIF
056700030822     C* NRS
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'NRS'
057200030822     C                   EVAL      PiStr=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   EXSR      CHKNUM
057600030822     C                   IF        PiInt=*on
057700030822     C                   Z-ADD     PiVal         VABNRS
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* CTR
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'CTR'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000030822     C                   Z-ADD     PiVal         VABCTR
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* PKB
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'PKB'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiNum=*on
060300030822     C                   Z-ADD     PiVal         VABPKB
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* VLB
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'VLB'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiNum=*on
061600030822     C                   Z-ADD     PiVal         VABVLB
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* QFT
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'QFT'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiNum=*on
062900030822     C                   Z-ADD     PiVal         VABQFT
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* CBO
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'CBO'
063700030822     C                   EVAL      VABCBO=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* TSP
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'TSP'
064600030822     C                   EVAL      VABTSP=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* VAS
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'VAS'
065500030822     C                   EVAL      VABVAS=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* VCA
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'VCA'
066400030822     C                   EVAL      VABVCA=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* GCA
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'GCA'
067300030822     C                   EVAL      VABGCA=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* CTM
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'CTM'
068200030822     C                   EVAL      VABCTM=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600030822     C* FFD
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'FFD'
069100030822     C                   EVAL      VABFFD=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500030822     C* VAD
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'VAD'
070000030822     C                   EVAL      VABVAD=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* GMA
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'GMA'
070900030822     C                   EVAL      VABGMA=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* GGA
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'GGA'
071800030822     C                   EVAL      VABGGA=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* GVA
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'GVA'
072700030822     C                   EVAL      VABGVA=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* TC1
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'TC1'
073600030822     C                   EVAL      VABTC1=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* TC2
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'TC2'
074500030822     C                   EVAL      VABTC2=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* ...
075000030822     C                   ENDIF
075100030822     C                   ENDDO
075200020204     C*
075300010330     C                   ENDSR
075400010607     C*----------------------------------------------------*
075500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075600010607     C*----------------------------------------------------*
075700010607     C     IMPVAB        BEGSR
075800010607     C*
075900010607     C                   SETOFF                                       3132
076000030515     C*
076100010607     C                   MOVEL     datcor        VABAAS
076200041108     C                   MOVEL     datcor        VATAAS
076300010607     C                   MOVE      datcor        VABMGS
076400050504     C**********
076500050504     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076600050504     C**********
076700050504     C                   IF        wGiro = *zeros
076800050504     C*
076900050504     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
077000050504     C                   EVAL      depvinDTA = vinDTA
077100050504     C*
077200050504     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
077300050504     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077400050504     C                                      %trim(intcol(3))
077500050504     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077600050504     C     minu:maiu     XLATE     vindta        vindta
077700050504     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077800050504     C                   DOW       posDa <= %len(%trim(vindta))
077900050504     C*
078000050504     C* Gestisco il 1° campo
078100050504     C                   IF        i = 1
078200050504     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078300050504     C                   EVAL      posDa = 1
078400050504     C* Eseguo lo scan x trovare la fine del primo campo
078500050504     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078600050504     C* A questo punto "estraggo" il campo corrente
078700050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078800050504     C                                                    (posA-posDa))
078900050504     C* X i campi successivi al 1°
079000050504     C                   ELSE
079100050504     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079200050504     C                   EVAL      posDa = posA + 1
079300050504     C* Eseguo lo scan x trovare la fine del campo corrente
079400050504     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079500050504     C* Gestisco l'ultimo campo
079600050504     C                   IF        posA = *zeros
079700050504     C                   EVAL      posA = %len(%trim(vindta)) + 1
079800050504     C* A questo punto "estraggo" il campo corrente
079900050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080000050504     C                                                    (posA-posDa))
080100050504     C                   LEAVE
080200050504     C                   ELSE
080300050504     C* A questo punto "estraggo" il campo corrente
080400050504     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080500050504     C                                                    (posA-posDa))
080600050504     C                   ENDIF
080700050504     C                   ENDIF
080800050504     C* Incremento il contatore d campo
080900050504     C                   EVAL      i = i +1
081000050504     C                   ENDDO
081100050504     C*
081200050504     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081300050504     C* delimitazione testo
081400050504     C                   EVAL      i = 1
081500050504     C                   DOW       i < %elem(SkSplitFLD)
081600050504     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081700050504     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081800050504     C                   EVAL      i = i + 1
081900050504     C                   ENDDO
082000050504     C                   EVAL      wGiro = 1
082100050504     C* Ripristino la 1° riga originale
082200050504     C                   EVAL      vinDTA = depvinDTA
082300050504     C                   ELSE
082400050504     C                   EVAL      wGiro = 2
082500010607     C*
082600031204     C**********
082700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082800031204     C* d separatore campo
082900031204     C**********
083000040726     C                   EVAL      vindta = %trim(vindta)
083100040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083200040726     C                   EVAL      vindta = %subst(vindta:2)
083300040726     C                   ENDDO
083400031204     C*
083500031204     C                   Z-ADD     *zeros        lunghInput        4 0
083600040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
083700040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083800040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083900040726     C                   EVAL      lunghInput = %len(%trim(vindta))
084000040726     C                   ENDDO
084100030715     C**********
084200030715     C* Effettuo lo split del campo dati d input
084300030715     C**********
084400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084500040324     C                   DOW       posDa <= %len(%trimr(vindta))
084600030715     C*
084700030715     C* Gestisco il 1° campo
084800030715     C                   IF        i = 1
084900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085000030715     C                   EVAL      posDa = 1
085100030715     C* Eseguo lo scan x trovare la fine del primo campo
085200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085300030715     C* A questo punto "estraggo" il campo corrente
085400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085500030715     C                                                    (posA-posDa))
085600030715     C* X i campi successivi al 1°
085700030715     C                   ELSE
085800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085900030715     C                   EVAL      posDa = posA + 1
086000031204     C* Verifico che nn vi sia il campo nn valorizzato
086100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086200031204     C* Se campo nn valorizzato skippo al prossimo
086300031204     C                   EVAL      SkSplitCSV(i) = *blanks
086400031204     C                   EVAL      posA  = posA + 1
086500031204     C                   ELSE
086600030715     C* Eseguo lo scan x trovare la fine del campo corrente
086700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086800030715     C* Gestisco l'ultimo campo
086900030715     C                   IF        posA = *zeros
087000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
087100030715     C* A questo punto "estraggo" il campo corrente
087200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087300030715     C                                                    (posA-posDa))
087400030715     C                   LEAVE
087500030715     C                   ELSE
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C                   ENDIF
088000031204     C                   ENDIF
088100030715     C                   ENDIF
088200030715     C* Incremento il contatore d campo
088300030715     C                   EVAL      i = i +1
088400030715     C                   ENDDO
088500030715     C*
088600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088700030715     C* delimitazione testo
088800030715     C                   EVAL      i = 1
088900030715     C                   DOW       i < %elem(SkSplitCSV)
089000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089200030715     C                   EVAL      i = i + 1
089300030715     C                   ENDDO
089400030715     C*
089500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089600030715     C                   EVAL      i = 1
089700030715     C                   DOW       i < %elem(SkSplitFLD)
089800030715     C*
089900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090100030715     C                             SkSplitCSV(i) <> *blanks
090200030721     C*** ATB
090300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090400030721     C                   EVAL      VABATB = SkSplitCSV(i)
090500030721     C                   ENDIF
090600030715     C*** RSD
090700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091100030715     C* ==
091200030715     C                   ENDIF
091300030721     C*** RD2
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030715     C*** IND
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091900030715     C                   EVAL      VABIND = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030715     C*** LOD
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030715     C*** PRD
092600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092800030715     C                   ENDIF
092900030721     C*** NZD
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030715     C*** RMA
093400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093600030715     C                   ENDIF
093700030715     C*** NOT
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030715     C*** NT2
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** VCA
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** VAS
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** FFD
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030715     C*** CBO
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
095900030715     C                   EVAL      VABCBO = SkSplitCSV(i)
096000041108     c                   if        vabcbo = 'F'
096100041108     c                   eval      vabcbo = '1'
096200041108     c                   endif
096300041108     c                   if        vabcbo = 'A'
096400041108     c                   eval      vabcbo = '2'
096500041108     c                   endif
096600030715     C                   ENDIF
096700030715     C
096800030721     C*** GC1
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** GC2
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** TSP
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
097800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** NAS
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** TIC
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** GCA
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** XCO
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** CTM
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** TCR
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** CTS
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** FTM
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** VAD
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** GMA
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** GGA
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** GVA
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** TC1
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103000050503     c                   select
103100050503     c                   when      sksplitcsv(i) <> *Blanks and vabtc1 = *Blanks
103200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103300050503     c                   when      sksplitcsv(i) <> *Blanks and vabtc2 = *Blanks
103400050503     C                   EVAL      VABTC2 = SkSplitCSV(i)
103500050503     c                   Endsl
103600030721     C                   ENDIF
103700030721     C*** TC2
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103900050503     c                   select
104000050503     c                   when      sksplitcsv(i) <> *Blanks and vabtc1 = *Blanks
104100050503     C                   EVAL      VABTC1 = SkSplitCSV(i)
104200050503     c                   when      sksplitcsv(i) <> *Blanks and vabtc2 = *Blanks
104300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
104400050503     c                   Endsl
104500030721     C                   ENDIF
104600030721     C*** SCL
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
104800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** RMO
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** NMO
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
105600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*
105900030721     C* Reperisco quindi i campi numerici...
106000030721     C*** CAD
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106200030721     C                   EVAL      PiStr=SkSplitCSV(i)
106300030721     C                   EXSR      CHKNUM
106400030721     C                   IF        PiInt=*on
106500030721     C                   Z-ADD     PiVal         Num5_0
106600030721     C                   MOVEL(p)  Num5_0        VABCAD
106700030721     C                   ELSE
106800030721     C                   SETON                                        32
106900030721     C                   EVAL      VABCAD = *zeros
107000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107100030721     C                             + ' ' + 'VABCAD'
107200030721     C                   ENDIF
107300030721     C                   ENDIF
107400041122     C*** RMN
107500041122     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
107600041122     C                   EVAL      PiStr=SkSplitCSV(i)
107700041122     C                   EXSR      CHKNUM
107800041122     C                   IF        PiInt=*on
107900041122     C                   Z-ADD     PiVal         VABRMN
108000041122     C                   ELSE
108100041122     C                   SETON                                        32
108200041122     C                   EVAL      VABRMN = *zeros
108300041122     C                   EVAL      vinmsg = %trimr(vinmsg)
108400041122     C                             + ' ' + 'VABRMN'
108500041122     C                   ENDIF
108600041122     C                   ENDIF
108700041122     C* NSP => Stacco un numeratore da AZNUM
108800041122      * a cambio di RMN
108900041122     c                   If        VabRmn <> savrmn
109000041122     C                   clear                   TRUL33DS
109100041122     C                   eval      I33OPE = *zeros
109200041122     C                   eval      I33CNU = 302
109300041122     C                   eval      I33NUM = 1
109400041122     C                   movel     TRUL33DS      KPJBU
109500041122     C                   call      'TRUL33R'
109600041122     C                   parm                    KPJBA
109700041122     C                   movel     KPJBU         TRUL33DS
109800041122     C                   if        O33ERR = *zeros
109900041122     C                   z-add     O33NRF        VABNSP
110000041122     C                   else
110100041122     C                   seton                                        32
110200041122     C                   EVAL      vinmsg = %trimr(vinmsg)
110300041122     C                             + ' ' + 'VABNSP'
110400041122     C                   endif
110500041122     c                   Eval      savrmn = vabrmn
110600041122     C                   endif
110700030721     C*** NCL
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on
111200041108     C                   Z-ADD     PiVal         VABNCL
111300030721     C                   ELSE
111400030721     C                   SETON                                        32
111500030721     C                   EVAL      VABNCL = *zeros
111600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111700030721     C                             + ' ' + 'VABNCL'
111800030721     C                   ENDIF
111900030721     C                   ENDIF
112000030721     C*** PKB
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112200030721     C                   EVAL      PiStr=SkSplitCSV(i)
112300030721     C                   EXSR      CHKNUM
112400030721     C                   IF        PiNum=*on
112500041108     C                   Z-ADD     PiVal         VABPKB
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      VABPKB = *zeros
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000030721     C                             + ' ' + 'VABPKB'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** CAS
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiNum=*on
113800030721     C                   Z-ADD     PiVal         VABCAS
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      VABCAS = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300030721     C                             + ' ' + 'VABCAS'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** CCM
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on      AND
115100030721     C                             PiVal<=9999999 AND
115200030721     C                             PiVal>*zeros
115300030721     C                   Z-ADD     PiVal         VABCCM
115400041108     C                   Z-ADD     PiVal         VATCCM
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      VABCCM = *zeros
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900041108     C                             + ' ' + 'VABCCM VATCCM'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** LNP
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiInt=*on    AND
116700030721     C                             PiVal<=999   AND
116800030721     C                             PiVal>*zeros
116900030721     C                   Z-ADD     PiVal         VABLNP
117000041108     C                   Z-ADD     PiVal         VATLNP
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      VABLNP = *zeros
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500041108     C                             + ' ' + 'VABLNP VATLNP'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** NRS
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiInt=*on AND
118300030721     C                             PiVal<=99
118400030721     C                   Z-ADD     PiVal         VABNRS
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABNRS'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** LNA
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on    AND
119600030721     C                             PiVal<=999   AND
119700030721     C                             PiVal>*zeros
119800030721     C                   Z-ADD     PiVal         VABLNA
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      VABLNA = *zeros
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300030721     C                             + ' ' + 'VABLNA'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** CTR
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on  AND
121100030721     C                             PiVal<=999
121200030721     C                   Z-ADD     PiVal         VABCTR
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABCTR'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** IAS
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiNum=*on
122400030721     C                   Z-ADD     PiVal         VABIAS
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABIAS'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** VLB
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiNum=*on
123600030721     C                   Z-ADD     PiVal         VABVLB
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABVLB'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** QFT
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiNum=*on
124800030721     C                   Z-ADD     PiVal         VABQFT
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABQFT'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** NCD
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on      AND
126000030721     C                             PiVal<=9999999
126100030721     C                   Z-ADD     PiVal         VABNCD
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABNCD'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** NCA
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiInt=*on      AND
127300030721     C                             PiVal<=9999999
127400030721     C                   Z-ADD     PiVal         VABNCA
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABNCA'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** ZNC
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on AND
128600030721     C                             PiVal<=99
128700030721     C                   Z-ADD     PiVal         VABZNC
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABZNC'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** DCR
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on       AND
129900030721     C                             PiVal<=99999999
130000030721     C                   Z-ADD     PiVal         VABDCR
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABDCR'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** HCR
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on   AND
131200030721     C                             PiVal<=9999
131300030721     C                   Z-ADD     PiVal         VABHCR
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABHCR'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** VMD
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiNum=*on
132500030721     C                   Z-ADD     PiVal         VABVMD
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABVMD'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** ANT
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on        AND
133700030721     C                             PiVal<=999999999
133800030721     C                   Z-ADD     PiVal         VABANT
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABANT'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** CMO
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on
135000030721     C                   Z-ADD     PiVal         Num5_0
135100030721     C                   MOVEL(p)  Num5_0        VABCMO
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      VABCMO = *zeros
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABCMO'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900041108
136000041108     c* Gestico i campi "personalizzati"
136100041108     c* BARCODE
136200041108     C                   IF        %trim(SkSplitFLD(i)) = 'BARCODE'
136300041108     C                   EVAL      wbarcode = SkSplitCSV(i)
136400041108     C                   ENDIF
136500041108     c* TEL
136600041108     C                   IF        %trim(SkSplitFLD(i)) = 'TEL'
136700041108     C                   EVAL      wtel = SkSplitCSV(i)
136800041108     C                   ENDIF
136900041108     c* REF
137000041108     C                   IF        %trim(SkSplitFLD(i)) = 'REF'
137100041108     C                   EVAL      wref = SkSplitCSV(i)
137200041108     C                   ENDIF
137300030715     C*
137400030715     C                   ENDIF
137500030715     C                   EVAL      i = i + 1
137600030715     C                   ENDDO
137700050504     C*
137800050504     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
137900050504     C* Reperisco la provincia dal CAP e dalla località
138000050504     C                   IF        VABCAD <> *blanks
138100050504     C                   CLEAR                   TISI95DS
138200050504     C                   EVAL      I95TCN = '3'
138300050504     C                   Z-ADD     datcor        I95DAT
138400050504     C                   EVAL      I95CAP = VABCAD
138500050504     C                   EVAL      I95LOC = VABLOD
138600050504     C                   CALL      'TISI95R'
138700050504     C                   PARM                    TISI95DS
138800050504     C                   EVAL      VABPRD = O95PRV
138900050504     C                   ENDIF
139000040726     C*
139100040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
139200040726     C                   IF        VABLNP = *zeros
139300040726     C                   SETON                                        31
139400050504     C                   EVAL      vinmsg = %trimr(vinmsg)
139500050504     C                             + ' ' + 'VABLNP'
139600040726     C                   ENDIF
139700050504     C*
139800050504     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
139900050504     C  n31              EXSR      wrivat
140000040726     C*
140100040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
140200040726     C* d valorizzarli sempre entrambi
140300040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
140400040726     C                   Z-ADD     VABNSP        VABRMN
140500040726     C                   ENDIF
140600040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
140700040726     C                   MOVEL     VABRMN        VABRMA
140800040726     C                   ENDIF
140900040726     C*
141000040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
141100040726     C                   IF        VABCAS > *zeros
141200040726     C                   IF        VABVCA = *blanks
141300040726     C                   EVAL      VABVCA = 'EUR'
141400040726     C                   ENDIF
141500040726     C                   IF        VABCBO = '1'
141600040726     C                   EVAL      VABCBO = '4'
141700040726     C                   ENDIF
141800040726     C                   IF        VABCBO = '2'
141900040726     C                   EVAL      VABCBO = '6'
142000040726     C                   ENDIF
142100040726     C                   ENDIF
142200020204     C*
142300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
142400020204     C                   EXSR      CHKIMPDIV
142500050504     C                   ENDIF
142600020204     C*
142700010607     C                   ENDSR
142800010607     C*----------------------------------------------------*
142900010601
143000041109
143100041109
143200020204
143300020204     C*----------------------------------------------------*
143400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
143500020204     C*----------------------------------------------------*
143600020204     C     CHKIMPDIV     BEGSR
143700020204     C*
143800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
143900020204     C                   Z-ADD     *zeros        wrkDec            9 9
144000020204     C*
144100020204     C* Come prima cosa effettuo considerazioni sulla divisa
144200020204     C                   IF        vabIAS > *zeros
144300020204     C                   IF        vabVAS <> 'EUR'
144400020204     C                   EVAL      vabVAS =  'ITL'
144500020204     C                   ENDIF
144600020204     C                   ENDIF
144700020204     C*
144800020204     C                   IF        vabCAS > *zeros
144900020204     C                   IF        vabVCA <> 'EUR'
145000020204     C                   EVAL      vabVCA =  'ITL'
145100020204     C                   ENDIF
145200020204     C                   ENDIF
145300020204     C*
145400020204     C                   IF        vabVMD > *zeros
145500020204     C                   IF        vabVAD <> 'EUR'
145600020204     C                   EVAL      vabVAD =  'ITL'
145700020204     C                   ENDIF
145800020204     C                   ENDIF
145900020204     C*
146000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
146100020204     C                   Z-ADD     vabIAS        wrkDec
146200020204     C                   IF        wrkDec > *zeros
146300020204     C                   IF        vabVAS = 'ITL'
146400020204     C                   EVAL      vabIAS = *zeros
146500020204     C                   ENDIF
146600020204     C                   ENDIF
146700020204     C*
146800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
146900020204     C                   Z-ADD     vabCAS        wrkDec
147000020204     C                   IF        wrkDec > *zeros
147100020204     C                   IF        vabVCA = 'ITL'
147200020204     C                   EVAL      vabCAS = *zeros
147300020204     C                   ENDIF
147400020204     C                   ENDIF
147500020204     C*
147600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
147700020204     C                   Z-ADD     vabVMD        wrkDec
147800020204     C                   IF        wrkDec > *zeros
147900020204     C                   IF        vabVAD = 'ITL'
148000020204     C                   EVAL      vabVMD = *zeros
148100020204     C                   ENDIF
148200020204     C                   ENDIF
148300020204     C*
148400020204     C                   ENDSR
148500020204     C***
148600020204
148700010330
148800010330
148900010330     C*----------------------------------------------------*
149000010330     C*  CONTROLLO NUMERICITA' CAMPI
149100010330     C*----------------------------------------------------*
149200010330     C     CHKNUM        BEGSR
149300010330     C*
149400010606     C                   IF        PiDecChr = *blanks
149500030715     C                   EVAL      PiDecChr = CharNUM
149600010606     C                   ENDIF
149700010606     C*
149800010606     C                   CALL(e)   'ISNUMERIC'
149900010330     C                   PARM                    PiStr            30
150000010606     C                   PARM                    PiDecChr          1
150100010330     C                   PARM      *ZEROS        PiVal            30 9
150200010330     C                   PARM      '0'           PiInt             1
150300010330     C                   PARM      '0'           PiNum             1
150400010330     C                   IF        %error
150500010606     C                   EVAL      PiNum=*off
150600010330     C                   ENDIF
150700010330     C*
150800010330     C                   ENDSR
150900010330     C***
151000010330
151100010601
151200010601
151300010601
151400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
151500010601     C     repfil        BEGSR
151600010601     C*
151700010601     C                   if        invfil = *zeros and
151800010601     C                             depfil > *zeros and
151900010629     C                             (vinflg = *blanks or
152000010629     C                              vinflg = *zeros)
152100010601     C
152200010601     C                   eval      invfil = depfil
152300010601     C                   endif
152400010601     C*
152500010601     C                   if        depfil <> invfil and
152600010601     C                             invfil > *zeros
152700010601     C                   eval      flgMulti = '1'
152800010601     C                   if        vinflg = *blanks
152900010601     C                   add       1             cntNonEl
153000010601     C                   endif
153100010601     C                   endif
153200010601     C*
153300010601     C                   if        vinflg = '2'
153400010601     C                   eval      flgStato = '2'
153500010601     C                   endif
153600010601     C*
153700010601     C                   ENDSR
153800010601     C***
153900010601
154000010601
154100010601
154200010330
154300010330
154400010330
154500990920      /TITLE Invio dei dati al punto operativo.
154600000613     C     invio         BEGSR
154700041108
154800041108     C* 1° invio FIVAT
154900041108     C                   reset                   dscmz
155000041108     C                   move      invfil        cmzdst
155100041108     C                   eval      cmzfld = 'FIVATWWR'
155200041108     C                   eval      cmzmbd = vlrhdl
155300041108     C                   eval      %subst(cmzmbd:1:1) = 'M'
155400041108     C                   eval      cmzfla = 'FIVAT00F'
155500041108     C                   eval      cmzmba = 'FIVAT00F'
155600041108     C                   eval      cmznrr = *zeros
155700041108     C                   move      §ctrokvt      cmznrr
155800041108     C                   eval      cmzlba = vlrfl1
155900041108     C                   call(e)   'TIS711C'
156000041108     C                   parm                    dscmz
156100041108     C                   parm      *blanks       esito
156200041108     C                   if        %error
156300041108     C                             or cmzerr = '1'
156400041108     C                             or esito  = '1'
156500041108     C                   eval      wrkesito = '3'
156600041108     C                   else
156700990920     C*
156800041108     C* 2° invio FIVAB
156900990920     C                   reset                   dscmz
157000010601     C                   move      invfil        cmzdst
157100990920     C                   eval      cmzfld = vlrfou
157200990920     C                   eval      cmzmbd = vlrhdl
157300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
157400021025     C                   eval      cmzfla = 'FIVAB00F'
157500021025     C                   eval      cmzmba = 'FIVAB00F'
157600990920     C                   eval      cmznrr = *zeros
157700041108     C                   move      §ctrokvb      cmznrr
157800021018     C                   eval      cmzlba = vlrfl1
157900990920     C                   call(e)   'TIS711C'
158000990920     C                   parm                    dscmz
158100990921     C                   parm      *blanks       esito
158200990920     C                   if        %error
158300990920     C                             or cmzerr = '1'
158400990921     C                             or esito  = '1'
158500000710     C                   eval      wrkesito = '3'
158600990920     C                   endif
158700041108     C                   endif
158800990920     C*
158900000613     C                   ENDSR
159000990910
159100010601
159200010601
159300010601
159400010601
159500010601      /TITLE Invio dei dati al punto operativo.
159600010601     C     opeini        BEGSR
159700010601     C*
159800010601     C* Inizializzo flag e contatori operativi
159900010601     C                   movel     '0'           flgGiro           1
160000010601     C                   movel     '0'           flgMulti          1
160100010601     C                   movel     '1'           flgStato          1
160200010615     C                   movel     '0'           flgOk             1
160300010601     C                   z-add     *zeros        cntNonEl         10 0
160400010601     C                   z-add     *zeros        depfil            3 0
160500010601     C                   z-add     *zeros        invfil            3 0
160600010601     C*
160700010601     C                   ENDSR
160800010601     C***
160900010601
161000010601
161100050421
161200050421
161300050421     C     *pssr         BEGSR
161400050421     C*
161500050421     C                   eval      wrkesito = '2'
161600050421     C*
161700050421     C                   ENDSR     '*CANCL'
161800050421     C***
161900050421
162000010601
162100010330
162200010330
162300000613     C     *inzsr        BEGSR
162400990910     C*
162500990910     C     *entry        plist
162600990920     C                   parm                    tivlrds
162700990921     C                   parm      wrkesito      esito
162800000724     C                   parm                    prmlit
162900000710     C                   parm                    prmfir
163000010330     C*
163100010330     C* CALCOLA LA DATA CORRENTE
163200010330     C                   time                    wn14             14 0
163300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
163400010330     C                   z-add     wn8           g08dat
163500010330     C                   z-add     *zeros        g08inv
163600010330     C                   movel     '0'           g08err
163700010330     C                   call      'XSRDA8'
163800010330     C                   parm                    wlbda8
163900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
164000000613     C*
164100000613     C                   ENDSR
164200000613     C***
164300990908
164400050504     Otitvf8p   E            riepilogo         2
164500990915     O                                              'Upload via Internet'
164600990915     O                                           +1 'Traduzione TIVIN00R -'
164700021025     O                                           +1 'FIVABWWR'
164800990915     O                                           +1 'Testate bolle'
164900990915     O          E            riepilogo   2
165000990915     O                       wrkdata
165100990915     O                       wrkora              +1
165200990915     O                       procname            +1
165300990915     O          E            riepilogo   2
165400990915     O                                              'Cliente..................:'
165500010605     O                       VABCCM        z     +1
165600990915     O          E            riepilogo   2
165700990920     O                                              'Riferimento Strategi.....:'
165800990920     O                       vlrhdl              +1
165900990915     O          E            riepilogo   2
166000990915     O                                              'Giusti...................:'
166100041108     O                       §CTROKVB      2   +  1
166200990915     O          E            riepilogo   2
166300990915     O                                              'Sbagliati e corretti.....:'
166400971022     O                       §CTRMO        2   +  1
166500990915     O          E            riepilogo   2
166600990915     O                                              'Sbagliati e scartati.....:'
166700971022     O                       §CTRNO        2   +  1
166800000613
166900050504     Otitvf8ps  E            testdett          2
167000000613     O                                              'Upload via Internet'
167100000613     O                                           +1 'Traduzione TIVIN00R -'
167200021025     O                                           +1 'FIVABWWR'
167300000613     O                                           +1 'Testate bolle'
167400000616     O          E            testdett    3
167500000613     O                                           +2 'N° rec'
167600000613     O                                           +3 'Anteprima contenuto'
167700000616     O          E            rigadett    2
167800000613     O                       rrnum               +2
167900000621     O                       recko               +3
168000000616     O          E            findett     2
168100000613     O                       wrkdata
168200000613     O                       wrkora              +1
168300000613     O                       procname            +1
168400000616     O          E            findett     2
168500000613     O                                              'Cliente..................:'
168600010605     O                       VABCCM        z     +1
168700000616     O          E            findett     2
168800000613     O                                              'Riferimento Strategi.....:'
168900000613     O                       vlrhdl              +1
169000000616     O          E            findett     2
169100000613     O                                              'Giusti...................:'
169200041108     O                       §CTROKVB      2   +  1
169300000616     O          E            findett     2
169400000613     O                                              'Sbagliati e corretti.....:'
169500000613     O                       §CTRMO        2   +  1
169600000616     O          E            findett     2
169700000613     O                                              'Sbagliati e scartati.....:'
169800000613     O                       §CTRNO        2   +  1
169900000613** CMD - COMANDI CL
170000050504OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
170100050504OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
170200050504DLTOVR FILE(TITVF8P TITVF8PS) LVL(*)
170300010606
170400000613
170500040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170600041109Filler0|Filler1|Filler2|VABCBO|VABRSD|VABRD2|VABIND|VABLOD|VABCAD|VABPRD|
170700050503VABPKB|VABCAS|VABVCA|VABRMA|BARCODE|VABNCL|TEL|REF|VABRMN|VABTSP|VABTC1|VABTC2
170800040726
