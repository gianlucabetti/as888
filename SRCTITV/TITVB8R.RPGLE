000100040510      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040510     FEDIVABwr  O    E             DISK    usropn
000700040510     FEDIVATwr  O    E             DISK    usropn
000800040510     Ftitvb8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040510     Ftitvb8ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
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
004500040510     D wNomeFile       s             30    INZ(*blanks)
004600040510     D wCMR            s             35    INZ(*blanks)
004700040510     D wPosDaA         s              2    INZ(*blanks)
004800040510     D wPosDa          s              2  0 INZ(*zeros)
004900040510     D wLungA          s              2    INZ(*blanks)
005000040510     D wLung           s              2  0 INZ(*zeros)
005100030715     D*------------------
005200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005300030715     D*------------------
005400030715     D SkSplitFLD      S             10    DIM(100)
005500030715     D SkSplitCSV      S            256    DIM(100)
005600030715     D CharCSV         S              1
005700030715     D CharTXT         S              1
005800030715     D CharNUM         S              1
005900030715     D posDa           S              3  0 INZ(*zeros)
006000030715     D posA            S              3  0 INZ(*zeros)
006100030715     D i               s              3  0 INZ(1)
006200030715     D wGiro           s              1  0 INZ(*zeros)
006300030822     D*------------------
006400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006500030822     D*------------------
006600030822     D posDaDft        S              3  0 INZ(*zeros)
006700030822     D posADft         S              3  0 INZ(*zeros)
006800030822     D j               s              3  0 INZ(1)
006900030822     D wGiroDft        s              1  0 INZ(*zeros)
007000010330     D*------------------
007100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007200010330     D*------------------
007300010330     D WLBDA8          DS                  INZ
007400010330     D  G08DAT                 1      8  0
007500010330     D  G08INV                 9     16  0
007600010330     D  G08ERR                17     17
007700010330     D  G08TGI                18     22  0
007800031201     D*------------------
007900031201     D* Costanti
008000031201     D*------------------
008100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008300010330
008400010330
008500010330
008600990915     C                   time                    wrkdata
008700990915     C                   time                    wrkora
008800000913     C                   reset                   rrnum
008900990921     C                   reset                   esito
009000990921     C                   reset                   wrkesito
009100010601     C*
009200010601     C                   exsr      opeini
009300010605     C                   exsr      rwvab
009400010601     C*
009500010601     C                   seton                                        lr
009600010601
009700010601
009800010601
009900010601     C*--------------------------------------------------------
010000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010100010601     C*--------------------------------------------------------
010200010601     C     PREELA        BEGSR
010300010601     C*
010400010601     C* SE OCCORRE SPEDIRE IN FILIALE
010500010601     C                   if        invfil <> *zeros and
010600010601     C                             flgGiro = '0'
010700010601     C*
010800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010900010601     C                   eval      flgGiro = '1'
011000000724     C*
011100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011300010601     C     invfil        chain     azorg01l
011400000724     C                   if        %found
011500000724     C                   movel(p)  CMD(1)        depcmd
011600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011700000724     C*
011800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000724     C                   PARM                    depcmd
012200000724     C                   PARM                    LENGH
012300000724     C*
012400000724     C                   endif
012500000616     C*
012600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012700000616     C                   movel(p)  CMD(2)        depcmd
012800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012900000616     C*
013000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    depcmd
013400000616     C                   PARM                    LENGH
013500000613     C*
013600040510     C                   if        not %open(titvb8ps)
013700040510     C                   open      titvb8ps
013800030715     C                   except    testdett
013900000613     C                   endif
014000010601     C*
014100010601     C                   endif
014200010601     C*
014300010601     C                   ENDSR
014400010601     C***
014500010601
014600010601
014700010601
014800010601
014900010601     C*--------------------------------------------------------
015000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015100010601     C*--------------------------------------------------------
015200010601     C     ENDELA        BEGSR
015300990915     C*
015400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015500040119     C*
015600040119     C* Effettuo la chiamata al CLLE preposto
015700040510     C                   call(e)   'TITVEVTC'
015800040510     C                   parm                    parccm
015900040119     C                   parm                    parmbr
016000040119     C                   parm      '2'           paropz
016100050201     C*
016200050201     C* Effettuo lancio TISI95 solo x chiusura
016300050201     C                   CLEAR                   TISI95DS
016400050201     C                   EVAL      I95TLA = 'C'
016500050201     C                   CALL      'TISI95R'
016600050201     C                   PARM                    TISI95DS
016700000613     C*
016800040510     C                   if        %open(titvb8ps)
016900000616     C                   except    findett
017000040510     C                   close     titvb8ps
017100000613     C                   endif
017200000616     C*
017300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017400011113     C                   Z-ADD     150           LENGH            15 5
017500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017600000616     C                   PARM                    CMD(3)
017700000616     C                   PARM                    LENGH
017800000616     C*
017900010601     C                   ENDSR
018000010601     C***
018100010601
018200010601
018300010601
018400000613
018500000613     C*--------------------------------------------------------
018600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018700000613     C*--------------------------------------------------------
018800000621     C     STPR          BEGSR
018900000613     C*
019000040510     C                   if        not %open(titvb8p)
019100040510     C   50              open      titvb8p
019200990915     C                   endif
019300990915     C*
019400030715     C   50              except    riepilogo
019500990915     C*
019600040510     C                   if        %open(titvb8p)
019700040510     C   50              close     titvb8p
019800990914     C                   endif
019900000613     C*
020000000613     C                   ENDSR
020100000613     C***
020200000613
020300010601
020400010601
020500010330     C*--------------------------------------------------------
020600040510     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
020700010330     C*--------------------------------------------------------
020800010605     C     RWVAB         BEGSR
020900040510     C*
021000040510     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
021100040510     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
021200040510     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
021300040510     C                   if        *in60 = *on
021400010330     C*
021500010330     C                   if        not %open(tivin00r)
021600010330     C                   open      tivin00r
021700010330     C                   endif
021800040510     C                   if        not %open(edivabwr)
021900040510     C                   open      edivabwr
022000010330     C                   endif
022100040119     C*
022200040510     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
022300040119     C                   exsr      prevat
022400040119     C*
022500040119     C                   if        chkcall = '0'
022600040119     C*
022700040510     C                   if        not %open(edivatwr)
022800040510     C                   open      edivatwr
022900040119     C                   endif
023000010330     C*
023100040119     C                   clear                   §CTROKVB          7 0
023200040119     C                   clear                   §CTROKVT          7 0
023300040119     C                   clear                   §CTRMO            7 0
023400040119     C                   clear                   §CTRNO            7 0
023500010330     C*
023600030822     C                   do        *HIVAL
023700010330     C*
023800030822     C                   read      tivin00r                               70
023900010618     C*
024000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024100010618     C                   if        vinflg = '1'
024200010618     C                   eval      flgOk = '1'
024300010618     C                   endif
024400040510     C*
024500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024600040510     C                   z-add     *zeros        wLenVINDTA        4 0
024700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
025000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
025100040510     C                   z-add     1             wFlgVINDTA
025200040510     C                   leave
025300040510     C                   endif
025400040510     C                   enddo
025500010618     C*
025600040510     C                   if        vindta > *blanks AND
025700040510     C                             wFlgVINDTA = 1
025800010330     C                   add       1             rrnum
025900010330     C*
026000010601     C                   if        *in70 = *off and
026100010330     C                             (vinflg = *blanks
026200010330     C                              or vinflg = '0'
026300010330     C                              or vinflg = '2')
026400010330     C*
026500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026600010711     C                   if        vinflg = *blanks or vinflg = '0'
026700010711     C                   clear                   vinmsg
026800010711     C                   endif
026900010601     C*
027000010330     C                   exsr      inzvar
027100010605     C                   exsr      impvab
027200010601     C*
027300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027400010605     C                   eval      depfil = VABLNP
027500010601     C                   exsr      repfil
027600010601     C                   if        depfil = invfil
027700021025     C                   if        vlrpoi = 999
027800030822     C                   move(P)   invfil        VABFGS
027900021025     C                   else
028000030822     C                   move(P)   vlrpoi        VABFGS
028100021025     C                   endif
028200010601     C*
028300010601     C                   exsr      PREELA
028400040510     C*
028500040510     C* VALORIZZO CAMPI RELATIVI AL "CMR"
028600040510     C                   eval      VABCMR = wCMR
028700040510     C                   eval      VABDCM = DATCOR
028800040510     C                   eval      VABDTS = DATCOR
028900040510     C                   eval      VABHMS = ORACOR
029000040510     C                   eval      VABCNT = 1
029100010601     C*
029200010604     C* Ebbene...
029300010604     C*
029400040119     C  N31              add       1             §CTROKVB
029500040119     C   32              add       1             §CTRMO
029600040119     C   31              add       1             §CTRNO
029700030822     C                   if        wGiro = 2
029800040510     C* Scrivo sempre il VAT
029900040510     C  N31              exsr      wrivat
030000040510     C* Scrivo il VAB solo a rottura d spedizione
030100040119     C                   if        depspe =  *zeros OR
030200040119     C                             depspe <> vabNSP
030300040119     C  N31              exsr      wrivab
030400040119     C  N31              eval      depspe = vabNSP
030500040119     C                   endif
030600040119     C                   endif
030700010604     C*
030800010604     C                   if        *in31 = *off and
030900010604     C                             *in32 = *off
031000010604     C                   eval      vinflg = '1'
031100010604     C                   else
031200010604     C                   eval      recko = vindta
031300010604     C                   except    rigadett
031400010604     C                   eval      vinflg = '2'
031500010604     C                   endif
031600010604     C                   endif
031700010601     C*
031800010604     C                   endif
031900010604     C*
032000010330     C                   else
032100010330     C                   eval      vinflg = '1'
032200010330     C                   endif
032300010601     C*
032400010601     C  N70              update    tivin000
032500010330     C*
032600030822     C  N70              enddo
032700040119     C*
032800040119     C                   endif                                                  (endif - chkcall)
032900010601     C*
033000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033100010601     C                   if        cntNonEl = *zeros or
033200010601     C                             flgMulti = '0'
033300010330     C* Se non ci sono record con errori ...
033400010601     C                   if        §ctrno = 0 and
033500010604     C                             §ctrmo = 0 and
033600010601     C                             flgStato <> '2'
033700010330     C* ... restituisco esito OK.
033800010330     C                   eval      wrkesito = '0'
033900010330     C                   else
034000040119     C                   if        §ctrokvb > 0
034100010330     C                   eval      wrkesito = '1'
034200010330     C                   else
034300010615     C                   if        flgOk = '0'
034400010615     C                   eval      wrkesito = '2'
034500010615     C                   else
034600010615     C                   eval      wrkesito = '6'
034700010615     C                   endif
034800010330     C                   endif
034900010330     C                   endif
035000010601     C                   else
035100010601     C                   eval      wrkesito = '9'
035200010601     C                   endif
035300010330     C*
035400010330     C                   if        %open(tivin00r)
035500010330     C                   close     tivin00r
035600010330     C                   endif
035700040510     C                   if        %open(edivabwr)
035800040510     C                   close     edivabwr
035900010330     C                   endif
036000040512     C                   if        %open(edivatwr)
036100040512     C                   close     edivatwr
036200040512     C                   endif
036300010601     C*
036400010601     C                   if        vlrpoi <> 999
036500010601     C                   eval      invfil = vlrpoi
036600010601     C                   endif
036700010330     C*
036800040119     C                   if        §ctrokvb > 0
036900010601     C                             and invfil > *zeros
037000010330     C                   exsr      invio
037100010330     C                   endif
037200010601     C*
037300010618     C                   if        flgGiro = '1'
037400010601     C                   exsr      endela
037500010618     C                   endif
037600040510     C                   endif
037700010330     C*
037800010330     C                   ENDSR
037900010330     C***
038000040119
038100040119
038200040119
038300040119     C*----------------------------------------------------*
038400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038500040119     C*----------------------------------------------------*
038600040510     C     WRIVAB        BEGSR
038700040119     C*
038800040510     C                   write     edivab00                                     => scarico il VAB
038900040119     C*
039000040119     C                   ENDSR
039100040119
039200040119
039300040119
039400040119     C*----------------------------------------------------*
039500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039600040119     C*----------------------------------------------------*
039700040510     C     WRIVAT        BEGSR
039800040119     C*
039900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040000040510     C                   if        VATTRC = *blanks
040100040510     C                   eval      VATTRC = 'B'
040200040510     C                   endif
040300040510     C                   eval      VATFGS = VABFGS
040400040510     C                   eval      VATAAS = VABAAS
040500040510     C                   eval      VATCCM = VABCCM
040600040510     C                   eval      VATNSP = VABNSP
040700040510     C                   eval      VATLNP = VABLNP
040800040510     C* Valorizzo i campi relativi al CMR
040900040510     C                   eval      VATCMR = VABCMR
041000040510     C                   eval      VATCNT = VABCNT
041100040510     C*
041200040510     C                   write     edivat00
041300040119     C                   add       1             §CTROKVT
041400040119     C*
041500040119     C                   ENDSR
041600040510
041700040510
041800040510
041900040510
042000040119
042100040119
042200040119
042300040119     C*----------------------------------------------------*
042400040510     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
042500040119     C*----------------------------------------------------*
042600040119     C     PREVAT        BEGSR
042700040119     C*
042800040510     C* Compongo il nome del membro da dare al EDIVATWR
042900040119     C                   eval      parmbr = vlrhdl
043000040119     C                   movel     'M'           parmbr
043100040510     C                   eval      parccm = vlrksc
043200040119     C                   eval      paropz = '1'
043300040119     C* Effettuo la chiamata al CLLE preposto
043400040510     C                   call(e)   'TITVEVTC'
043500040119     C                   parm                    parccm
043600040119     C                   parm                    parmbr
043700040119     C                   parm                    paropz
043800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043900040119     C                   if        %error
044000040119     C                   movel     '1'           chkcall
044100040119     C                   else
044200040119     C                   movel     '0'           chkcall
044300040119     C                   endif
044400040119     C*
044500040119     C                   ENDSR
044600010601
044700010601
044800010601
044900010330     C*----------------------------------------------------*
045000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045100010330     C*----------------------------------------------------*
045200040119     C     INZVAR        BEGSR
045300010330     C*
045400030822     C* Inizializzo variabili di wrk
045500020204     C                   Z-ADD     *zeros        Num5_0            5 0
045600030822     C*
045700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
045800040510     C                   CLEAR                   EDIVAB00
045900040510     C                   CLEAR                   EDIVAT00
046000030822     C                   CLEAR                   SkSplitCSV
046100030822     C                   EVAL      i = 1
046200030822     C                   EVAL      posDa = *zeros
046300030822     C                   EVAL      posA  = *zeros
046400030822     C*
046500030822     C* Reimposto i valori di default
046600030822     C                   EXSR      DEFCAM
046700030822     C*
046800010330     C                   ENDSR
046900010330     C*----------------------------------------------------*
047000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047100010330     C*----------------------------------------------------*
047200010330     C     DEFCAM        BEGSR
047300030822     C*
047400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047500030715     C* e delimitatore testo.
047600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
047700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
047800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
047900030822     C*
048000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
048100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
048200030715     C                   SETON                                        50
048300030715     C                   ELSE
048400030715     C                   SETOFF                                       50
048500030715     C                   ENDIF
048600030822     C*
048700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
048800030822     C                   EVAL      posDaDft = 1
048900030822     C                   EVAL      posADft  = 0
049000030822     C                   EVAL      wGiroDft = 0
049100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
049200030822     C                             posDaDft > 0
049300030822     C*
049400030822     C* Gestisco il 1° giro
049500030822     C                   IF        wGiroDft = 0
049600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
049700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
049800030822     C* Incremento il contatore dei "giri"
049900030822     C                   EVAL      wGiroDft = 1
050000030822     C                   ELSE
050100030822     C                   EVAL      posDaDft = posADft
050200030822     C                   ENDIF
050300030822     C* Eseguo lo scan x trovare la fine del campo corrente
050400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
050500030822     C*
050600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
050700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
050800030822     C                   IF        posDaDft > 0 AND
050900030822     C                             posADft  > 0
051000030822     C* NCL
051100030822     C                   IF        %subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):1:3)
051400030822     C                             = 'NCL'
051500030822     C                   EVAL      PiStr=%trim(%subst(
051600030822     C                             %subst(vlrppt:posDaDft+1:
051700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051800030822     C                   EXSR      CHKNUM
051900030822     C                   IF        PiInt=*on
052000030822     C                   Z-ADD     PiVal         VABNCL
052100030822     C                   ENDIF
052200030822     C                   ENDIF
052300030822     C* CCM
052400030822     C                   IF        %subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):1:3)
052700030822     C                             = 'CCM'
052800030822     C                   EVAL      PiStr=%trim(%subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053100030822     C                   EXSR      CHKNUM
053200030822     C                   IF        PiInt=*on
053300030822     C                   Z-ADD     PiVal         VABCCM
053400030822     C                   ENDIF
053500030822     C                   ENDIF
053600030822     C* LNP
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'LNP'
054100030822     C                   EVAL      PiStr=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   EXSR      CHKNUM
054500030822     C                   IF        PiInt=*on
054600030822     C                   Z-ADD     PiVal         VABLNP
054700030822     C                   ENDIF
054800030822     C                   ENDIF
054900030822     C* NRS
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'NRS'
055400030822     C                   EVAL      PiStr=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   EXSR      CHKNUM
055800030822     C                   IF        PiInt=*on
055900030822     C                   Z-ADD     PiVal         VABNRS
056000030822     C                   ENDIF
056100030822     C                   ENDIF
056200030822     C* CTR
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'CTR'
056700030822     C                   EVAL      PiStr=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   EXSR      CHKNUM
057100030822     C                   IF        PiInt=*on
057200030822     C                   Z-ADD     PiVal         VABCTR
057300030822     C                   ENDIF
057400030822     C                   ENDIF
057500030822     C* PKB
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'PKB'
058000030822     C                   EVAL      PiStr=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   EXSR      CHKNUM
058400030822     C                   IF        PiNum=*on
058500030822     C                   Z-ADD     PiVal         VABPKB
058600030822     C                   ENDIF
058700030822     C                   ENDIF
058800030822     C* VLB
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'VLB'
059300030822     C                   EVAL      PiStr=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   EXSR      CHKNUM
059700030822     C                   IF        PiNum=*on
059800030822     C                   Z-ADD     PiVal         VABVLB
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* QFT
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'QFT'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiNum=*on
061100030822     C                   Z-ADD     PiVal         VABQFT
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* CBO
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'CBO'
061900030822     C                   EVAL      VABCBO=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* TSP
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'TSP'
062800030822     C                   EVAL      VABTSP=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* VAS
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'VAS'
063700030822     C                   EVAL      VABVAS=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* VCA
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'VCA'
064600030822     C                   EVAL      VABVCA=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* GCA
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'GCA'
065500030822     C                   EVAL      VABGCA=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* CTM
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'CTM'
066400030822     C                   EVAL      VABCTM=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* FFD
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'FFD'
067300030822     C                   EVAL      VABFFD=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* VAD
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'VAD'
068200030822     C                   EVAL      VABVAD=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600030822     C* GMA
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'GMA'
069100030822     C                   EVAL      VABGMA=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500030822     C* GGA
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'GGA'
070000030822     C                   EVAL      VABGGA=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* GVA
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'GVA'
070900030822     C                   EVAL      VABGVA=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* TC1
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'TC1'
071800030822     C                   EVAL      VABTC1=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* TC2
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'TC2'
072700030822     C                   EVAL      VABTC2=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* ...
073200030822     C                   ENDIF
073300030822     C                   ENDDO
073400020204     C*
073500010330     C                   ENDSR
073600010607     C*----------------------------------------------------*
073700040510     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
073800010607     C*----------------------------------------------------*
073900010607     C     IMPVAB        BEGSR
074000010607     C*
074100010607     C                   SETOFF                                       3132
074200030515     C*
074300010607     C                   MOVEL     datcor        VABAAS
074400010607     C                   MOVE      datcor        VABMGS
074500010607     C*
074600030715     C**********
074700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
074800030715     C**********
074900030715     C                   IF        wGiro = *zeros
075000040113     C*
075100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
075200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
075300040510     C                             %trim(intcol(3))<>*blanks
075400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
075500040113     C                                      %trim(intcol(3))
075600040510     C                   ENDIF
075700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
075800031201     C     minu:maiu     XLATE     vindta        vindta
075900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076000030715     C                   DOW       posDa <= %len(%trim(vindta))
076100030715     C*
076200030715     C* Gestisco il 1° campo
076300030715     C                   IF        i = 1
076400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
076500030715     C                   EVAL      posDa = 1
076600030715     C* Eseguo lo scan x trovare la fine del primo campo
076700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076800030715     C* A questo punto "estraggo" il campo corrente
076900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077000030715     C                                                    (posA-posDa))
077100030715     C* X i campi successivi al 1°
077200030715     C                   ELSE
077300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
077400030715     C                   EVAL      posDa = posA + 1
077500030715     C* Eseguo lo scan x trovare la fine del campo corrente
077600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077700030715     C* Gestisco l'ultimo campo
077800030715     C                   IF        posA = *zeros
077900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
078000030715     C* A questo punto "estraggo" il campo corrente
078100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078200030715     C                                                    (posA-posDa))
078300030715     C                   LEAVE
078400030715     C                   ELSE
078500030715     C* A questo punto "estraggo" il campo corrente
078600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078700030715     C                                                    (posA-posDa))
078800030715     C                   ENDIF
078900030715     C                   ENDIF
079000030715     C* Incremento il contatore d campo
079100030715     C                   EVAL      i = i +1
079200030715     C                   ENDDO
079300030715     C*
079400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079500030715     C* delimitazione testo
079600030715     C                   EVAL      i = 1
079700030715     C                   DOW       i < %elem(SkSplitFLD)
079800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
079900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
080000030715     C                   EVAL      i = i + 1
080100030715     C                   ENDDO
080200030715     C                   EVAL      wGiro = 1
080300030715     C                   ELSE
080400030715     C                   EVAL      wGiro = 2
080500031204     C**********
080600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
080700031204     C* d separatore campo
080800031204     C**********
080900031204     C                   EVAL      vindta = %trim(vindta)
081000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081100031204     C                   EVAL      vindta = %subst(vindta:2)
081200031204     C                   ENDDO
081300031204     C*
081400031204     C                   Z-ADD     *zeros        lunghInput        4 0
081500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
081700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
081800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081900031204     C                   ENDDO
082000030715     C**********
082100030715     C* Effettuo lo split del campo dati d input
082200030715     C**********
082300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082400030715     C                   DOW       posDa <= %len(%trim(vindta))
082500030715     C*
082600030715     C* Gestisco il 1° campo
082700030715     C                   IF        i = 1
082800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082900030715     C                   EVAL      posDa = 1
083000030715     C* Eseguo lo scan x trovare la fine del primo campo
083100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C* X i campi successivi al 1°
083600030715     C                   ELSE
083700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083800030715     C                   EVAL      posDa = posA + 1
083900031204     C* Verifico che nn vi sia il campo nn valorizzato
084000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084100031204     C* Se campo nn valorizzato skippo al prossimo
084200031204     C                   EVAL      SkSplitCSV(i) = *blanks
084300031204     C                   EVAL      posA  = posA + 1
084400031204     C                   ELSE
084500030715     C* Eseguo lo scan x trovare la fine del campo corrente
084600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084700030715     C* Gestisco l'ultimo campo
084800030715     C                   IF        posA = *zeros
084900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085000030715     C* A questo punto "estraggo" il campo corrente
085100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085200030715     C                                                    (posA-posDa))
085300030715     C                   LEAVE
085400030715     C                   ELSE
085500030715     C* A questo punto "estraggo" il campo corrente
085600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085700030715     C                                                    (posA-posDa))
085800030715     C                   ENDIF
085900031204     C                   ENDIF
086000030715     C                   ENDIF
086100030715     C* Incremento il contatore d campo
086200030715     C                   EVAL      i = i +1
086300030715     C                   ENDDO
086400030715     C*
086500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086600030715     C* delimitazione testo
086700030715     C                   EVAL      i = 1
086800030715     C                   DOW       i < %elem(SkSplitCSV)
086900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
087000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087100030715     C                   EVAL      i = i + 1
087200030715     C                   ENDDO
087300030715     C*
087400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
087500030715     C                   EVAL      i = 1
087600030715     C                   DOW       i < %elem(SkSplitFLD)
087700030715     C*
087800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
087900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
088000030715     C                             SkSplitCSV(i) <> *blanks
088100030721     C*** ATB
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
088300030721     C                   EVAL      VABATB = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030715     C*** RSD
088600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
088700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
088800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
088900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
089000030715     C* ==
089100030715     C                   ENDIF
089200030721     C*** RD2
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
089400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
089500030721     C                   ENDIF
089600030715     C*** IND
089700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
089800030715     C                   EVAL      VABIND = SkSplitCSV(i)
089900030715     C                   ENDIF
090000030715     C*** LOD
090100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
090200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
090300030715     C                   ENDIF
090400030715     C*** PRD
090500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
090600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
090700030715     C                   ENDIF
090800030721     C*** NZD
090900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
091000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
091100030721     C                   ENDIF
091200030715     C*** RMA
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
091400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030715     C*** NOT
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
091800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** NT2
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
092200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030715     C*** VCA
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
092600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030715     C*** VAS
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
093000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
093100030715     C                   ENDIF
093200030715     C*** FFD
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
093400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030721     C*** GC1
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
093800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** GC2
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
094200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*** TSP
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
094600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
094700030721     C                   ENDIF
094800040510     C*** CBO
094900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
095000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
095100040510     C                   ENDIF
095200030721     C*** NAS
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
095400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** TIC
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
095800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** GCA
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
096200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** XCO
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
096600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** CTM
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
097000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** TCR
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
097400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** CTS
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
097800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** FTM
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
098200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** VAD
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
098600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** GMA
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
099000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** GGA
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
099400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** GVA
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
099800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** TC1
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
100200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** TC2
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
100600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** SCL
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
101000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** RMO
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
101400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** NMO
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
101800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*
102100030721     C* Reperisco quindi i campi numerici...
102200040510     C*** NSP
102300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
102400040510     C                   EVAL      PiStr=SkSplitCSV(i)
102500040510     C                   EXSR      CHKNUM
102600040510     C                   IF        PiInt=*on
102700040510     C                   Z-ADD     PiVal         VABNSP
102800040510     C                   ELSE
102900040510     C                   SETON                                        31
103000040510     C                   EVAL      VABNSP = *zeros
103100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
103200040510     C                             + ' ' + 'VABNSP'
103300040510     C                   ENDIF
103400040510     C                   ENDIF
103500030721     C*** CAD
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
103700030721     C                   EVAL      PiStr=SkSplitCSV(i)
103800030721     C                   EXSR      CHKNUM
103900030721     C                   IF        PiInt=*on
104000030721     C                   Z-ADD     PiVal         Num5_0
104100030721     C                   MOVEL(p)  Num5_0        VABCAD
104200030721     C                   ELSE
104300030721     C                   SETON                                        32
104400030721     C                   EVAL      VABCAD = *zeros
104500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104600030721     C                             + ' ' + 'VABCAD'
104700030721     C                   ENDIF
104800030721     C                   ENDIF
104900030721     C*** NCL
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105100030721     C                   EVAL      PiStr=SkSplitCSV(i)
105200030721     C                   EXSR      CHKNUM
105300030721     C                   IF        PiInt=*on
105400030721     C                   Z-ADD     PiVal         VABNCL
105500030721     C                   ELSE
105600030721     C                   SETON                                        32
105700030721     C                   EVAL      VABNCL = *zeros
105800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105900040122     C                             + ' ' + 'VABNCL'
106000030721     C                   ENDIF
106100030721     C                   ENDIF
106200030721     C*** PKB
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106400030721     C                   EVAL      PiStr=SkSplitCSV(i)
106500030721     C                   EXSR      CHKNUM
106600030721     C                   IF        PiNum=*on
106700030721     C                   Z-ADD     PiVal         VABPKB
106800030721     C                   ELSE
106900030721     C                   SETON                                        32
107000030721     C                   EVAL      VABPKB = *zeros
107100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107200030721     C                             + ' ' + 'VABPKB'
107300030721     C                   ENDIF
107400030721     C                   ENDIF
107500030721     C*** CAS
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
107700030721     C                   EVAL      PiStr=SkSplitCSV(i)
107800030721     C                   EXSR      CHKNUM
107900030721     C                   IF        PiNum=*on
108000030721     C                   Z-ADD     PiVal         VABCAS
108100030721     C                   ELSE
108200030721     C                   SETON                                        32
108300030721     C                   EVAL      VABCAS = *zeros
108400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108500030721     C                             + ' ' + 'VABCAS'
108600030721     C                   ENDIF
108700030721     C                   ENDIF
108800030721     C*** CCM
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
109000030721     C                   EVAL      PiStr=SkSplitCSV(i)
109100030721     C                   EXSR      CHKNUM
109200030721     C                   IF        PiInt=*on      AND
109300030721     C                             PiVal<=9999999 AND
109400030721     C                             PiVal>*zeros
109500030721     C                   Z-ADD     PiVal         VABCCM
109600030721     C                   ELSE
109700030721     C                   SETON                                        32
109800030721     C                   EVAL      VABCCM = *zeros
109900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110000040510     C                             + ' ' + 'VABCCM'
110100030721     C                   ENDIF
110200030721     C                   ENDIF
110300030721     C*** LNP
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiInt=*on    AND
110800030721     C                             PiVal<=999   AND
110900030721     C                             PiVal>*zeros
111000030721     C                   Z-ADD     PiVal         VABLNP
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      VABLNP = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500040510     C                             + ' ' + 'VABLNP'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** NRS
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on AND
112300030721     C                             PiVal<=99
112400030721     C                   Z-ADD     PiVal         VABNRS
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800030721     C                             + ' ' + 'VABNRS'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** LNA
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiInt=*on    AND
113600030721     C                             PiVal<=999   AND
113700030721     C                             PiVal>*zeros
113800030721     C                   Z-ADD     PiVal         VABLNA
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      VABLNA = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300030721     C                             + ' ' + 'VABLNA'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** CTR
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on  AND
115100030721     C                             PiVal<=999
115200030721     C                   Z-ADD     PiVal         VABCTR
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600030721     C                             + ' ' + 'VABCTR'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** IAS
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiNum=*on
116400030721     C                   Z-ADD     PiVal         VABIAS
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABIAS'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** VLB
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiNum=*on
117600030721     C                   Z-ADD     PiVal         VABVLB
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABVLB'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** QFT
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiNum=*on
118800030721     C                   Z-ADD     PiVal         VABQFT
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABQFT'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** NCD
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiInt=*on      AND
120000030721     C                             PiVal<=9999999
120100030721     C                   Z-ADD     PiVal         VABNCD
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABNCD'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** NCA
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiInt=*on      AND
121300030721     C                             PiVal<=9999999
121400030721     C                   Z-ADD     PiVal         VABNCA
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABNCA'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** ZNC
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiInt=*on AND
122600030721     C                             PiVal<=99
122700030721     C                   Z-ADD     PiVal         VABZNC
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABZNC'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** DCR
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on       AND
123900030721     C                             PiVal<=99999999
124000030721     C                   Z-ADD     PiVal         VABDCR
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABDCR'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** HCR
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on   AND
125200030721     C                             PiVal<=9999
125300030721     C                   Z-ADD     PiVal         VABHCR
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABHCR'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** VMD
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiNum=*on
126500030721     C                   Z-ADD     PiVal         VABVMD
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABVMD'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** ANT
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on        AND
127700030721     C                             PiVal<=999999999
127800030721     C                   Z-ADD     PiVal         VABANT
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABANT'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** CMO
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on
129000030721     C                   Z-ADD     PiVal         Num5_0
129100030721     C                   MOVEL(p)  Num5_0        VABCMO
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      VABCMO = *zeros
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABCMO'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900040510     C*
130000040510     C* Considerazioni su campi "personalizzati"
130100040510     C*** VABGC1/VABGC2
130200040511     C                   IF        %trim(SkSplitFLD(i)) = 'RIPOSO'
130300040511     C     minu:maiu     XLATE     SkSplitCSV(i) SkSplitCSV(i)
130400040511     C                   IF        %trim(SkSplitCSV(i)) = 'LUN'
130500040510     C                   EVAL      VABGC1 = '1'
130600040510     C                   ENDIF
130700040511     C                   IF        %trim(SkSplitCSV(i)) = 'MAR'
130800040510     C                   EVAL      VABGC1 = '2'
130900040510     C                   ENDIF
131000040510     C                   IF        %trim(SkSplitCSV(i)) = 'MER'
131100040510     C                   EVAL      VABGC1 = '3'
131200040510     C                   ENDIF
131300040510     C                   IF        %trim(SkSplitCSV(i)) = 'GIO'
131400040510     C                   EVAL      VABGC1 = '4'
131500040510     C                   ENDIF
131600040510     C                   IF        %trim(SkSplitCSV(i)) = 'VEN'
131700040510     C                   EVAL      VABGC1 = '5'
131800040510     C                   ENDIF
131900040510     C                   IF        %trim(SkSplitCSV(i)) = 'SAB'
132000040510     C                   EVAL      VABGC1 = '6'
132100040510     C                   ENDIF
132200040510     C                   ENDIF
132300040510     C*
132400040510     C* Reperisco i campi del EDIVAT
132500040510     C*** VATNOT
132600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
132700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
132800040510     C                   ENDIF
132900040510     C*** VATTRC
133000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133200040510     C                   ENDIF
133300040510     C*
133400030715     C                   ENDIF
133500030715     C                   EVAL      i = i + 1
133600030715     C                   ENDDO
133700040510     C*
133800040510     C* Se NSP nn valorizzato considero errore in traduzione
133900040510     C                   IF        VABNSP = *zeros
134000040510     C                   SETON                                        31
134100040510     C                   ENDIF
134200040510     C*
134300040510     C* Se RMN nn valorizzato lo forzo a 1
134400040510     C                   IF        VABRMN = *zeros
134500040510     C                   Z-ADD     VABNSP        VABRMN
134600040510     C                   ENDIF
134700040510     C*
134800040510     C* Se RMA nn valorizzato lo forzo uguale a VABNSP
134900040510     C                   IF        VABRMA = *blanks
135000040510     C                   MOVEL     VABNSP        VABRMA
135100040510     C                   ENDIF
135200040128     C*
135300040510     C* Se provincia nn valorizzata la reperisco
135400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
135500040510     C                   IF        VABPRD = *blanks
135600040128     C                   CLEAR                   TISI95DS
135700040128     C                   EVAL      I95TCN = '3'
135800040128     C                   Z-ADD     datcor        I95DAT
135900040128     C                   EVAL      I95NAR = VABNZD
136000040128     C                   EVAL      I95CAP = VABCAD
136100040128     C                   EVAL      I95LOC = VABLOD
136200040128     C                   CALL      'TISI95R'
136300040128     C                   PARM                    TISI95DS
136400040128     C                   EVAL      VABPRD = O95PRV
136500040510     C                   ENDIF
136600040113     C*
136700040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
136800040113     C                   IF        VABCAS > *zeros
136900040119     C                   EVAL      VABVCA = 'EUR'
137000040119     C                   IF        VABCBO = '1'
137100040113     C                   EVAL      VABCBO = '4'
137200040119     C                   ENDIF
137300040119     C                   IF        VABCBO = '2'
137400040119     C                   EVAL      VABCBO = '6'
137500040119     C                   ENDIF
137600040113     C                   ENDIF
137700020204     C*
137800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
137900020204     C                   EXSR      CHKIMPDIV
138000030715     C                   ENDIF
138100020204     C*
138200010607     C                   ENDSR
138300010607     C*----------------------------------------------------*
138400010601
138500020204
138600020204     C*----------------------------------------------------*
138700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
138800020204     C*----------------------------------------------------*
138900020204     C     CHKIMPDIV     BEGSR
139000020204     C*
139100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
139200020204     C                   Z-ADD     *zeros        wrkDec            9 9
139300020204     C*
139400020204     C* Come prima cosa effettuo considerazioni sulla divisa
139500020204     C                   IF        vabIAS > *zeros
139600020204     C                   IF        vabVAS <> 'EUR'
139700020204     C                   EVAL      vabVAS =  'ITL'
139800020204     C                   ENDIF
139900020204     C                   ENDIF
140000020204     C*
140100020204     C                   IF        vabCAS > *zeros
140200020204     C                   IF        vabVCA <> 'EUR'
140300020204     C                   EVAL      vabVCA =  'ITL'
140400020204     C                   ENDIF
140500020204     C                   ENDIF
140600020204     C*
140700020204     C                   IF        vabVMD > *zeros
140800020204     C                   IF        vabVAD <> 'EUR'
140900020204     C                   EVAL      vabVAD =  'ITL'
141000020204     C                   ENDIF
141100020204     C                   ENDIF
141200020204     C*
141300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
141400020204     C                   Z-ADD     vabIAS        wrkDec
141500020204     C                   IF        wrkDec > *zeros
141600020204     C                   IF        vabVAS = 'ITL'
141700020204     C                   EVAL      vabIAS = *zeros
141800020204     C                   ENDIF
141900020204     C                   ENDIF
142000020204     C*
142100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
142200020204     C                   Z-ADD     vabCAS        wrkDec
142300020204     C                   IF        wrkDec > *zeros
142400020204     C                   IF        vabVCA = 'ITL'
142500020204     C                   EVAL      vabCAS = *zeros
142600020204     C                   ENDIF
142700020204     C                   ENDIF
142800020204     C*
142900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
143000020204     C                   Z-ADD     vabVMD        wrkDec
143100020204     C                   IF        wrkDec > *zeros
143200020204     C                   IF        vabVAD = 'ITL'
143300020204     C                   EVAL      vabVMD = *zeros
143400020204     C                   ENDIF
143500020204     C                   ENDIF
143600020204     C*
143700020204     C                   ENDSR
143800020204     C***
143900020204
144000010330
144100010330
144200010330     C*----------------------------------------------------*
144300010330     C*  CONTROLLO NUMERICITA' CAMPI
144400010330     C*----------------------------------------------------*
144500010330     C     CHKNUM        BEGSR
144600010330     C*
144700010606     C                   IF        PiDecChr = *blanks
144800030715     C                   EVAL      PiDecChr = CharNUM
144900010606     C                   ENDIF
145000010606     C*
145100010606     C                   CALL(e)   'ISNUMERIC'
145200010330     C                   PARM                    PiStr            30
145300010606     C                   PARM                    PiDecChr          1
145400010330     C                   PARM      *ZEROS        PiVal            30 9
145500010330     C                   PARM      '0'           PiInt             1
145600010330     C                   PARM      '0'           PiNum             1
145700010330     C                   IF        %error
145800010606     C                   EVAL      PiNum=*off
145900010330     C                   ENDIF
146000010330     C*
146100010330     C                   ENDSR
146200010330     C***
146300010330
146400010601
146500010601
146600010601
146700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
146800010601     C     repfil        BEGSR
146900010601     C*
147000010601     C                   if        invfil = *zeros and
147100010601     C                             depfil > *zeros and
147200010629     C                             (vinflg = *blanks or
147300010629     C                              vinflg = *zeros)
147400010601     C
147500010601     C                   eval      invfil = depfil
147600010601     C                   endif
147700010601     C*
147800010601     C                   if        depfil <> invfil and
147900010601     C                             invfil > *zeros
148000010601     C                   eval      flgMulti = '1'
148100010601     C                   if        vinflg = *blanks
148200010601     C                   add       1             cntNonEl
148300010601     C                   endif
148400010601     C                   endif
148500010601     C*
148600010601     C                   if        vinflg = '2'
148700010601     C                   eval      flgStato = '2'
148800010601     C                   endif
148900010601     C*
149000010601     C                   ENDSR
149100010601     C***
149200010601
149300010601
149400010601
149500010330
149600040119      /TITLE Invio dei dati al punto operativo.
149700040119     C     invio         BEGSR
149800040119     C*
149900040510     C* 1° invio EDIVAT
150000040119     C                   reset                   dscmz
150100040119     C                   move      vlrpoi        cmzdst
150200040510     C                   eval      cmzfld = 'EDIVATWR'
150300040119     C                   eval      cmzmbd = vlrhdl
150400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
150500040119     C***                if        prmfir = *blanks
150600040510     C                   eval      cmzfla = 'EDIVAT0F'
150700040510     C                   eval      cmzmba = 'EDIVAT0F'
150800040119     C***                else
150900040119     C***                eval      cmzfla = prmfir
151000040119     C***                eval      cmzmba = prmfir
151100040119     C***                endif
151200040119     C                   eval      cmznrr = *zeros
151300040119     C                   move      §ctrokvt      cmznrr
151400040119     C                   eval      cmzlba = vlrfl1
151500040119     C                   call(e)   'TIS711C'
151600040119     C                   parm                    dscmz
151700040119     C                   parm      *blanks       esito
151800040119     C                   if        %error
151900040119     C                             or cmzerr = '1'
152000040119     C                             or esito  = '1'
152100040119     C                   eval      wrkesito = '3'
152200040119     C                   else
152300040119     C*
152400040510     C* 2° invio EDIVAB
152500040119     C                   reset                   dscmz
152600040119     C                   move      vlrpoi        cmzdst
152700040119     C                   eval      cmzfld = vlrfou
152800040119     C                   eval      cmzmbd = vlrhdl
152900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153000040119     C***                if        prmfir = *blanks
153100040510     C                   eval      cmzfla = 'EDIVAB0F'
153200040510     C                   eval      cmzmba = 'EDIVAB0F'
153300040119     C***                else
153400040119     C***                eval      cmzfla = prmfir
153500040119     C***                eval      cmzmba = prmfir
153600040119     C***                endif
153700040119     C                   eval      cmznrr = *zeros
153800040119     C                   move      §ctrokvb      cmznrr
153900040119     C                   eval      cmzlba = vlrfl1
154000040119     C                   call(e)   'TIS711C'
154100040119     C                   parm                    dscmz
154200040119     C                   parm      *blanks       esito
154300040119     C                   if        %error
154400040119     C                             or cmzerr = '1'
154500040119     C                             or esito  = '1'
154600040119     C                   eval      wrkesito = '3'
154700040119     C                   endif
154800040119     C                   endif
154900040119     C*
155000040119     C                   ENDSR
155100040119     C***
155200010601
155300010601
155400010601
155500010601
155600010601      /TITLE Invio dei dati al punto operativo.
155700010601     C     opeini        BEGSR
155800010601     C*
155900010601     C* Inizializzo flag e contatori operativi
156000010601     C                   movel     '0'           flgGiro           1
156100010601     C                   movel     '0'           flgMulti          1
156200010601     C                   movel     '1'           flgStato          1
156300010615     C                   movel     '0'           flgOk             1
156400010601     C                   z-add     *zeros        cntNonEl         10 0
156500010601     C                   z-add     *zeros        depfil            3 0
156600010601     C                   z-add     *zeros        invfil            3 0
156700010601     C*
156800010601     C                   ENDSR
156900010601     C***
157000010601
157100010601
157200050421
157300070411     C     *pssr         BEGSR
157400070411     C*
157500070411     C                   if        %open(tivin00r)
157600070411     C                   close     tivin00r
157700070411     C                   endif
157800070502     C                   if        %open(edivabwr)
157900070502     C                   close     edivabwr
158000070411     C                   endif
158100070502     C                   if        %open(edivatwr)
158200070502     C                   close     edivatwr
158300070411     C                   endif
158400070411     C*
158500070411     C* Effettuo la chiamata al CLLE preposto
158600070502     C                   call(e)   'TITVEVTC'
158700070411     C                   parm                    parccm
158800070411     C                   parm                    parmbr
158900070411     C                   parm      '2'           paropz
159000070411     C*
159100070411     C                   eval      wrkesito = '2'
159200070411     C*
159300070411     C                   seton                                        LR
159400070411     C*
159500070411     C                   ENDSR     '*CANCL'
159600070411     C***
159700050421
159800010601
159900010330
160000010330
160100000613     C     *inzsr        BEGSR
160200990910     C*
160300990910     C     *entry        plist
160400990920     C                   parm                    tivlrds
160500990921     C                   parm      wrkesito      esito
160600000724     C                   parm                    prmlit
160700000710     C                   parm                    prmfir
160800040510     C*
160900040510     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
161000040510     C                   move(p)   vlrMSG        wNomeFile
161100040510     C*
161200040510     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
161300040510     C* del codice CMR dal nome del file importato.
161400040510     C                   if        wNomeFile <> *blanks AND
161500040510     C                             vlrppt<>*blanks
161600040510     C                   eval      wPosDaA = %subst(vlrppt:5:2)
161700040510     C                   move(p)   wPosDaA       wPosDa
161800040510     C                   eval      wLungA  = %subst(vlrppt:7:2)
161900040510     C                   move(p)   wLungA        wLung
162000040510     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
162100040510     C                   seton                                        60
162200040510     C                   else
162300040510     C                   setoff                                       60
162400040510     C                   endif
162500010330     C*
162600010330     C* CALCOLA LA DATA CORRENTE
162700010330     C                   time                    wn14             14 0
162800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
162900040510     C                   movel     wn14          oracor            6 0          *ORA
163000010330     C                   z-add     wn8           g08dat
163100010330     C                   z-add     *zeros        g08inv
163200010330     C                   movel     '0'           g08err
163300010330     C                   call      'XSRDA8'
163400010330     C                   parm                    wlbda8
163500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
163600000613     C*
163700000613     C                   ENDSR
163800000613     C***
163900990908
164000040510     Otitvb8p   E            riepilogo         2
164100990915     O                                              'Upload via Internet'
164200990915     O                                           +1 'Traduzione TIVIN00R -'
164300040510     O                                           +1 'EDIVABWR'
164400990915     O                                           +1 'Testate bolle'
164500990915     O          E            riepilogo   2
164600990915     O                       wrkdata
164700990915     O                       wrkora              +1
164800990915     O                       procname            +1
164900990915     O          E            riepilogo   2
165000990915     O                                              'Cliente..................:'
165100010605     O                       VABCCM        z     +1
165200990915     O          E            riepilogo   2
165300990920     O                                              'Riferimento Strategi.....:'
165400990920     O                       vlrhdl              +1
165500990915     O          E            riepilogo   2
165600990915     O                                              'Giusti...................:'
165700040119     O                       §CTROKVB      2   +  1
165800990915     O          E            riepilogo   2
165900990915     O                                              'Sbagliati e corretti.....:'
166000971022     O                       §CTRMO        2   +  1
166100990915     O          E            riepilogo   2
166200990915     O                                              'Sbagliati e scartati.....:'
166300971022     O                       §CTRNO        2   +  1
166400000613
166500040510     Otitvb8ps  E            testdett          2
166600000613     O                                              'Upload via Internet'
166700000613     O                                           +1 'Traduzione TIVIN00R -'
166800040510     O                                           +1 'EDIVABWR'
166900000613     O                                           +1 'Testate bolle'
167000000616     O          E            testdett    3
167100000613     O                                           +2 'N° rec'
167200000613     O                                           +3 'Anteprima contenuto'
167300000616     O          E            rigadett    2
167400000613     O                       rrnum               +2
167500000621     O                       recko               +3
167600000616     O          E            findett     2
167700000613     O                       wrkdata
167800000613     O                       wrkora              +1
167900000613     O                       procname            +1
168000000616     O          E            findett     2
168100000613     O                                              'Cliente..................:'
168200010605     O                       VABCCM        z     +1
168300000616     O          E            findett     2
168400000613     O                                              'Riferimento Strategi.....:'
168500000613     O                       vlrhdl              +1
168600000616     O          E            findett     2
168700000613     O                                              'Giusti...................:'
168800040119     O                       §CTROKVB      2   +  1
168900000616     O          E            findett     2
169000000613     O                                              'Sbagliati e corretti.....:'
169100000613     O                       §CTRMO        2   +  1
169200000616     O          E            findett     2
169300000613     O                                              'Sbagliati e scartati.....:'
169400000613     O                       §CTRNO        2   +  1
169500000613** CMD - COMANDI CL
169600040510OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
169700040510OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
169800040510DLTOVR FILE(TITVB8P TITVB8PS) LVL(*)
169900010606
170000000613
170100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170200040510VABNSP;VABRMA;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VATNOT;BRC;RIPOSO
170300040510
170400040113
