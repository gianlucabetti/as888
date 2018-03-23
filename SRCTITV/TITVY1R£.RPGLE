000100070301      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300070208     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600070301     FEDIVABwr  O    E             DISK    usropn
000700070301     FEDIVATwr  O    E             DISK    usropn
000800070216     Ftitvy1p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070216     Ftitvy1ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700040113     D*------------
001800040113     D* SCHIEDE A PROGRAMMA
001900040113     D*------------
002000040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800040706     D tisi95ds      e ds
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D wrkdata         s               d
003400010330     D wrkora          s               t
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600010330     D recko           s            150    INZ(*blanks)
003700011113     D depcmd          s            150    INZ(*blanks)
003800040119     D parccm          s              8    INZ(*blanks)
003900040119     D parmbr          s             10    INZ(*blanks)
004000040119     D paropz          s              1    INZ(*blanks)
004100040119     D chkcall         s              1    INZ(*blanks)
004200040706     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004300070306     D wNomeFile       s             30    INZ(*blanks)
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600030822     D*------------------
005700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005800030822     D*------------------
005900030822     D posDaDft        S              3  0 INZ(*zeros)
006000030822     D posADft         S              3  0 INZ(*zeros)
006100030822     D j               s              3  0 INZ(1)
006200030822     D wGiroDft        s              1  0 INZ(*zeros)
006300010330     D*------------------
006400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006500010330     D*------------------
006600010330     D WLBDA8          DS                  INZ
006700010330     D  G08DAT                 1      8  0
006800010330     D  G08INV                 9     16  0
006900010330     D  G08ERR                17     17
007000010330     D  G08TGI                18     22  0
007100060327     D*------------------
007200060327     D* DS REPERIMENTO NUMERATORE
007300060327     D*------------------
007400060327     D trul33ds      e ds                  inz
007500060327     D*------------------
007600060327     D* DS ARCHITETTURA
007700060327     D*------------------
007800060327     D kpjba         e ds                  inz
007900031201     D*------------------
008000031201     D* Costanti
008100031201     D*------------------
008200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400010330
008500010330
008600010330
008700990915     C                   time                    wrkdata
008800990915     C                   time                    wrkora
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200010601     C*
009300010601     C                   exsr      opeini
009400010605     C                   exsr      rwvab
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
013700070216     C                   if        not %open(titvy1ps)
013800070216     C                   open      titvy1ps
013900030715     C                   except    testdett
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
015400990915     C*
015500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015600040119     C*
015700040119     C* Effettuo la chiamata al CLLE preposto
015800070208     C                   if        *in60 = *on
015900070301     C                   call(e)   'TITVEVTC'
016000040119     C                   parm                    parccm
016100040119     C                   parm                    parmbr
016200040119     C                   parm      '2'           paropz
016300070208     C                   endif
016400050201     C*
016500050201     C* Effettuo lancio TISI95 solo x chiusura
016600050201     C                   CLEAR                   TISI95DS
016700050201     C                   EVAL      I95TLA = 'C'
016800050201     C                   CALL      'TISI95R'
016900050201     C                   PARM                    TISI95DS
017000060327     C*
017100060327     C* Effettuo lancio TRUL33 solo x chiusura
017200060327     C                   clear                   TRUL33DS
017300060327     C                   eval      I33TLA = 'C'
017400060327     C                   movel     TRUL33DS      KPJBU
017500060327     C                   call      'TRUL33R'
017600060327     C                   parm                    KPJBA
017700000613     C*
017800070216     C                   if        %open(titvy1ps)
017900000616     C                   except    findett
018000070216     C                   close     titvy1ps
018100000613     C                   endif
018200000616     C*
018300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018400011113     C                   Z-ADD     150           LENGH            15 5
018500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018600000616     C                   PARM                    CMD(3)
018700000616     C                   PARM                    LENGH
018800000616     C*
018900010601     C                   ENDSR
019000010601     C***
019100010601
019200010601
019300010601
019400000613
019500000613     C*--------------------------------------------------------
019600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019700000613     C*--------------------------------------------------------
019800000621     C     STPR          BEGSR
019900000613     C*
020000070216     C                   if        not %open(titvy1p)
020100070216     C   50              open      titvy1p
020200990915     C                   endif
020300990915     C*
020400030715     C   50              except    riepilogo
020500990915     C*
020600070216     C                   if        %open(titvy1p)
020700070216     C   50              close     titvy1p
020800990914     C                   endif
020900000613     C*
021000000613     C                   ENDSR
021100000613     C***
021200000613
021300010601
021400010601
021500010330     C*--------------------------------------------------------
021600070301     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
021700010330     C*--------------------------------------------------------
021800010605     C     RWVAB         BEGSR
021900010330     C*
022000010330     C                   if        not %open(tivin00r)
022100010330     C                   open      tivin00r
022200010330     C                   endif
022300070301     C                   if        not %open(edivabwr)
022400070301     C                   open      edivabwr
022500010330     C                   endif
022600040119     C*
022700070301     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
022800040119     C                   exsr      prevat
022900040119     C*
023000040119     C                   if        chkcall = '0'
023100040119     C*
023200070208     C                   if        *in60 = *on
023300070301     C                   if        not %open(edivatwr)
023400070301     C                   open      edivatwr
023500070208     C                   endif
023600040119     C                   endif
023700010330     C*
023800040119     C                   clear                   §CTROKVB          7 0
023900040119     C                   clear                   §CTROKVT          7 0
024000040119     C                   clear                   §CTRMO            7 0
024100040119     C                   clear                   §CTRNO            7 0
024200050330     C*
024300050330     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024400050330     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024500070208     C                   exsr      inzvar
024600070208     C                   exsr      impvab
024700050330     C*
024800050330     C                   do        *HIVAL
024900010330     C*
025000040706     C                   read      tivin00r                               70
025100010618     C*
025200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025300010618     C                   if        vinflg = '1'
025400010618     C                   eval      flgOk = '1'
025500010618     C                   endif
025600050330     C*
025700050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025800050330     C                   z-add     *zeros        wLenVINDTA        4 0
025900050330     C                   z-add     *zeros        wFlgVINDTA        1 0
026000050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
026100050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
026200050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026300050330     C                   z-add     1             wFlgVINDTA
026400050330     C                   leave
026500050330     C                   endif
026600050330     C                   enddo
026700010618     C*
026800050330     C                   if        vindta > *blanks AND
026900050330     C                             wFlgVINDTA = 1
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
028200010330     C                   exsr      inzvar
028300010605     C                   exsr      impvab
028400010601     C*
028500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028600010605     C                   eval      depfil = VABLNP
028700010601     C                   exsr      repfil
028800010601     C                   if        depfil = invfil
028900021025     C                   if        vlrpoi = 999
029000030822     C                   move(P)   invfil        VABFGS
029100070208     C   60              move(P)   invfil        VATFGS
029200021025     C                   else
029300030822     C                   move(P)   vlrpoi        VABFGS
029400070208     C   60              move(P)   vlrpoi        VATFGS
029500021025     C                   endif
029600010601     C*
029700010601     C                   exsr      PREELA
029800070301     C*
029900070301     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
030000070306     C                   EVAL      VABCMR = wNomeFile
030100070301     C                   EVAL      VABDCM = DATCOR
030200070301     C                   EVAL      VABDTS = DATCOR
030300070301     C                   EVAL      VABHMS = ORACOR
030400070301     C                   EVAL      VABCNT = 1
030500070301     C*
030600070301     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
030700070306     C                   EVAL      VATCMR = wNomeFile
030800070301     C                   EVAL      VATCNT = 1
030900010601     C*
031000010604     C* Ebbene...
031100010604     C*
031200040119     C  N31              add       1             §CTROKVB
031300040119     C   32              add       1             §CTRMO
031400040119     C   31              add       1             §CTRNO
031500030822     C                   if        wGiro = 2
031600040119     C  N31              exsr      wrivab
031700040119     C                   endif
031800010604     C*
031900010604     C                   if        *in31 = *off and
032000010604     C                             *in32 = *off
032100010604     C                   eval      vinflg = '1'
032200010604     C                   else
032300010604     C                   eval      recko = vindta
032400010604     C                   except    rigadett
032500010604     C                   eval      vinflg = '2'
032600010604     C                   endif
032700010604     C                   endif
032800010601     C*
032900010604     C                   endif
033000010604     C*
033100010330     C                   else
033200010330     C                   eval      vinflg = '1'
033300010330     C                   endif
033400010601     C*
033500010601     C  N70              update    tivin000
033600010330     C*
033700030822     C  N70              enddo
033800040119     C*
033900040119     C                   endif                                                  (endif - chkcall)
034000010601     C*
034100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034200010601     C                   if        cntNonEl = *zeros or
034300010601     C                             flgMulti = '0'
034400010330     C* Se non ci sono record con errori ...
034500010601     C                   if        §ctrno = 0 and
034600010604     C                             §ctrmo = 0 and
034700010601     C                             flgStato <> '2'
034800010330     C* ... restituisco esito OK.
034900010330     C                   eval      wrkesito = '0'
035000010330     C                   else
035100040119     C                   if        §ctrokvb > 0
035200010330     C                   eval      wrkesito = '1'
035300010330     C                   else
035400010615     C                   if        flgOk = '0'
035500010615     C                   eval      wrkesito = '2'
035600010615     C                   else
035700010615     C                   eval      wrkesito = '6'
035800010615     C                   endif
035900010330     C                   endif
036000010330     C                   endif
036100010601     C                   else
036200010601     C                   eval      wrkesito = '9'
036300010601     C                   endif
036400010330     C*
036500010330     C                   if        %open(tivin00r)
036600010330     C                   close     tivin00r
036700010330     C                   endif
036800070301     C                   if        %open(edivabwr)
036900070301     C                   close     edivabwr
037000010330     C                   endif
037100070208     C                   if        *in60 = *on
037200070301     C                   if        %open(edivatwr)
037300070301     C                   close     edivatwr
037400070208     C                   endif
037500040512     C                   endif
037600010601     C*
037700010601     C                   if        vlrpoi <> 999
037800010601     C                   eval      invfil = vlrpoi
037900010601     C                   endif
038000010330     C*
038100040119     C                   if        §ctrokvb > 0
038200010601     C                             and invfil > *zeros
038300010330     C                   exsr      invio
038400010330     C                   endif
038500010601     C*
038600010618     C                   if        flgGiro = '1'
038700010601     C                   exsr      endela
038800010618     C                   endif
038900010330     C*
039000010330     C                   ENDSR
039100010330     C***
039200040119
039300040119
039400040119
039500040119     C*----------------------------------------------------*
039600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039700040119     C*----------------------------------------------------*
039800040119     C     WRIVAB        BEGSR
039900040119     C*
040000070301     C                   write     edivab00                                     => scarico il VAB
040100040119     C*
040200040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
040300070216     C   60              exsr      wrivat
040400040119     C*
040500040119     C                   ENDSR
040600040119
040700040119
040800040119
040900060327     C*----------------------------------------------------*
041000060327     C*  SCARICAMENTO BUFFER RECORDS VAT
041100060327     C*----------------------------------------------------*
041200060327     C     WRIVAT        BEGSR
041300060327     C*
041400060327     C* Attribuisco la stessa chiave della testata bolle
041500060327     C                   EVAL      VATAAS = VABAAS
041600070216     C                   EVAL      VATLNP = VABLNP
041700070216     C                   EVAL      VATNRS = VABNRS
041800070216     C                   EVAL      VATNSP = VABNSP
041900070216     C                   EVAL      VATTRC = 'E'
042000070220     C*
042100070220     C* Il barcode d ogni collo (tutte e sempre spedizioni mono-collo) coincide con il rif.mitt.alfa
042200070220     C                   EVAL      VATNOT = %trim(VABRMA)
042300060327     C*
042400060327     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
042500060327     C                   IF        VATNOT <> *blanks
042600070301     C                   WRITE     EDIVAT00
042700060327     C                   ADD       1             §CTROKVT
042800060327     C                   ENDIF
042900060327     C*
043000060327     C                   ENDSR
043100040119
043200040119
043300040119
043400040119     C*----------------------------------------------------*
043500070301     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
043600040119     C*----------------------------------------------------*
043700040119     C     PREVAT        BEGSR
043800070208     C*
043900070208     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
044000070208     C                   SETOFF                                       50
044100070208     C                   IF        %subst(vlrppt:1:1) = 'C'
044200070208     C                   SETON                                        60
044300070208     C                   ELSE
044400070208     C                   SETOFF                                       60
044500070208     C                   ENDIF
044600070208     C*
044700070208     C* Se richiesta elaborazione anxhe FNVAT => proseguo
044800070208     C                   if        *in60 = *on
044900040119     C*
045000070301     C* Compongo il nome del membro da dare al EDIVATWR
045100040119     C                   eval      parmbr = vlrhdl
045200040119     C                   movel     'M'           parmbr
045300050330     C                   eval      parccm = %subst(vlrKSC:2:7)
045400040119     C                   eval      paropz = '1'
045500040119     C* Effettuo la chiamata al CLLE preposto
045600070301     C                   call(e)   'TITVEVTC'
045700040119     C                   parm                    parccm
045800040119     C                   parm                    parmbr
045900040119     C                   parm                    paropz
046000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046100040119     C                   if        %error
046200040119     C                   movel     '1'           chkcall
046300040119     C                   else
046400040119     C                   movel     '0'           chkcall
046500040119     C                   endif
046600070208     C*
046700070208     C                   else
046800070208     C                   movel     '0'           chkcall
046900070208     C                   endif
047000040119     C*
047100040119     C                   ENDSR
047200010601
047300010601
047400010601
047500010330     C*----------------------------------------------------*
047600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047700010330     C*----------------------------------------------------*
047800040119     C     INZVAR        BEGSR
047900010330     C*
048000030822     C* Inizializzo variabili di wrk
048100020204     C                   Z-ADD     *zeros        Num5_0            5 0
048200030822     C*
048300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048400070301     C                   CLEAR                   EDIVAB00
048500070301     C   60              CLEAR                   EDIVAT00
048600030822     C                   CLEAR                   SkSplitCSV
048700030822     C                   EVAL      i = 1
048800030822     C                   EVAL      posDa = *zeros
048900030822     C                   EVAL      posA  = *zeros
049000030822     C*
049100030822     C* Reimposto i valori di default
049200030822     C                   EXSR      DEFCAM
049300030822     C*
049400010330     C                   ENDSR
049500010330     C*----------------------------------------------------*
049600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049700010330     C*----------------------------------------------------*
049800010330     C     DEFCAM        BEGSR
049900040119     C*
050000040119     C* Imposto i valori d default a programma dei campi fondamentali
050100110516     C                   EVAL      VABCCM = 2494540
050200110516     C   60              EVAL      VATCCM = 2494540
050300070216     C                   EVAL      VABLNP = 249
050400070216     C   60              EVAL      VATLNP = 249
050500070216     C                   EVAL      VABCTR = 000
050600070220     C                   EVAL      VABCTM = '7Q'
050700040119     C                   EVAL      VABCBO = '1'
050800070216     C                   EVAL      VABNCL = 1
050900030822     C*
051000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051100030715     C* e delimitatore testo.
051200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
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
056300070208     C   60              Z-ADD     PiVal         VATCCM
056400030822     C                   ENDIF
056500030822     C                   ENDIF
056600030822     C* LNP
056700030822     C                   IF        %subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):1:3)
057000030822     C                             = 'LNP'
057100030822     C                   EVAL      PiStr=%trim(%subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400030822     C                   EXSR      CHKNUM
057500030822     C                   IF        PiInt=*on
057600030822     C                   Z-ADD     PiVal         VABLNP
057700070208     C   60              Z-ADD     PiVal         VATLNP
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* NRS
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'NRS'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000030822     C                   Z-ADD     PiVal         VABNRS
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* CTR
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'CTR'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABCTR
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* PKB
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'PKB'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiNum=*on
061600050330     C                   Z-ADD     PiVal         VABPKB
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* VLB
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'VLB'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiNum=*on
062900030822     C                   Z-ADD     PiVal         VABVLB
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* QFT
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'QFT'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiNum=*on
064200030822     C                   Z-ADD     PiVal         VABQFT
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* CBO
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'CBO'
065000030822     C                   EVAL      VABCBO=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   ENDIF
065400030822     C* TSP
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'TSP'
065900030822     C                   EVAL      VABTSP=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   ENDIF
066300030822     C* VAS
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'VAS'
066800030822     C                   EVAL      VABVAS=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   ENDIF
067200030822     C* VCA
067300030822     C                   IF        %subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):1:3)
067600030822     C                             = 'VCA'
067700030822     C                   EVAL      VABVCA=%trim(%subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000030822     C                   ENDIF
068100030822     C* GCA
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'GCA'
068600030822     C                   EVAL      VABGCA=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   ENDIF
069000030822     C* CTM
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'CTM'
069500030822     C                   EVAL      VABCTM=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   ENDIF
069900030822     C* FFD
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'FFD'
070400030822     C                   EVAL      VABFFD=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   ENDIF
070800030822     C* VAD
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'VAD'
071300030822     C                   EVAL      VABVAD=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   ENDIF
071700030822     C* GMA
071800030822     C                   IF        %subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):1:3)
072100030822     C                             = 'GMA'
072200030822     C                   EVAL      VABGMA=%trim(%subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500030822     C                   ENDIF
072600030822     C* GGA
072700030822     C                   IF        %subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):1:3)
073000030822     C                             = 'GGA'
073100030822     C                   EVAL      VABGGA=%trim(%subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073400030822     C                   ENDIF
073500030822     C* GVA
073600030822     C                   IF        %subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):1:3)
073900030822     C                             = 'GVA'
074000030822     C                   EVAL      VABGVA=%trim(%subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300030822     C                   ENDIF
074400030822     C* TC1
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'TC1'
074900030822     C                   EVAL      VABTC1=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300030822     C* TC2
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'TC2'
075800030822     C                   EVAL      VABTC2=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200030822     C* ...
076300030822     C                   ENDIF
076400030822     C                   ENDDO
076500020204     C*
076600010330     C                   ENDSR
076700010607     C*----------------------------------------------------*
076800070301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
076900010607     C*----------------------------------------------------*
077000010607     C     IMPVAB        BEGSR
077100010607     C*
077200010607     C                   SETOFF                                       3132
077300030515     C*
077400010607     C                   MOVEL     datcor        VABAAS
077500070208     C   60              MOVEL     datcor        VATAAS
077600010607     C                   MOVE      datcor        VABMGS
077700010607     C*
077800030715     C**********
077900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078000030715     C**********
078100030715     C                   IF        wGiro = *zeros
078200040706     C*
078300040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
078400040706     C                   EVAL      depvinDTA = vinDTA
078500040113     C*
078600040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
078700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078800040113     C                                      %trim(intcol(3))
078900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079000031201     C     minu:maiu     XLATE     vindta        vindta
079100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079200030715     C                   DOW       posDa <= %len(%trim(vindta))
079300030715     C*
079400030715     C* Gestisco il 1° campo
079500030715     C                   IF        i = 1
079600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079700030715     C                   EVAL      posDa = 1
079800030715     C* Eseguo lo scan x trovare la fine del primo campo
079900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C* X i campi successivi al 1°
080400030715     C                   ELSE
080500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080600030715     C                   EVAL      posDa = posA + 1
080700030715     C* Eseguo lo scan x trovare la fine del campo corrente
080800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080900030715     C* Gestisco l'ultimo campo
081000030715     C                   IF        posA = *zeros
081100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081200030715     C* A questo punto "estraggo" il campo corrente
081300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081400030715     C                                                    (posA-posDa))
081500030715     C                   LEAVE
081600030715     C                   ELSE
081700030715     C* A questo punto "estraggo" il campo corrente
081800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081900030715     C                                                    (posA-posDa))
082000030715     C                   ENDIF
082100030715     C                   ENDIF
082200030715     C* Incremento il contatore d campo
082300030715     C                   EVAL      i = i +1
082400030715     C                   ENDDO
082500030715     C*
082600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082700030715     C* delimitazione testo
082800030715     C                   EVAL      i = 1
082900030715     C                   DOW       i < %elem(SkSplitFLD)
083000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
083100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
083200030715     C                   EVAL      i = i + 1
083300030715     C                   ENDDO
083400030715     C                   EVAL      wGiro = 1
083500040706     C* Ripristino la 1° riga originale
083600040706     C                   EVAL      vinDTA = depvinDTA
083700030715     C                   ELSE
083800030715     C                   EVAL      wGiro = 2
083900031204     C**********
084000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
084100031204     C* d separatore campo
084200031204     C**********
084300031204     C                   EVAL      vindta = %trim(vindta)
084400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084500031204     C                   EVAL      vindta = %subst(vindta:2)
084600031204     C                   ENDDO
084700031204     C*
084800031204     C                   Z-ADD     *zeros        lunghInput        4 0
084900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
085100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
085200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085300031204     C                   ENDDO
085400030715     C**********
085500030715     C* Effettuo lo split del campo dati d input
085600030715     C**********
085700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085800030715     C                   DOW       posDa <= %len(%trim(vindta))
085900030715     C*
086000030715     C* Gestisco il 1° campo
086100030715     C                   IF        i = 1
086200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086300030715     C                   EVAL      posDa = 1
086400030715     C* Eseguo lo scan x trovare la fine del primo campo
086500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086600030715     C* A questo punto "estraggo" il campo corrente
086700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086800030715     C                                                    (posA-posDa))
086900030715     C* X i campi successivi al 1°
087000030715     C                   ELSE
087100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087200030715     C                   EVAL      posDa = posA + 1
087300031204     C* Verifico che nn vi sia il campo nn valorizzato
087400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087500031204     C* Se campo nn valorizzato skippo al prossimo
087600031204     C                   EVAL      SkSplitCSV(i) = *blanks
087700031204     C                   EVAL      posA  = posA + 1
087800031204     C                   ELSE
087900030715     C* Eseguo lo scan x trovare la fine del campo corrente
088000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088100030715     C* Gestisco l'ultimo campo
088200030715     C                   IF        posA = *zeros
088300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088400030715     C* A questo punto "estraggo" il campo corrente
088500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088600030715     C                                                    (posA-posDa))
088700030715     C                   LEAVE
088800030715     C                   ELSE
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C                   ENDIF
089300031204     C                   ENDIF
089400030715     C                   ENDIF
089500030715     C* Incremento il contatore d campo
089600030715     C                   EVAL      i = i +1
089700030715     C                   ENDDO
089800030715     C*
089900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090000030715     C* delimitazione testo
090100030715     C                   EVAL      i = 1
090200030715     C                   DOW       i < %elem(SkSplitCSV)
090300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090500030715     C                   EVAL      i = i + 1
090600030715     C                   ENDDO
090700030715     C*
090800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090900030715     C                   EVAL      i = 1
091000030715     C                   DOW       i < %elem(SkSplitFLD)
091100030715     C*
091200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
091300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091400030715     C                             SkSplitCSV(i) <> *blanks
091500030721     C*** ATB
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
091700030721     C                   EVAL      VABATB = SkSplitCSV(i)
091800030721     C                   ENDIF
091900060327     C*** RSD
092000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
092100060327     C                   EVAL      VABRSD = SkSplitCSV(i)
092200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
092400030715     C* ==
092500030715     C                   ENDIF
092600030721     C*** RD2
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
092800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030715     C*** IND
093100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
093200030715     C                   EVAL      VABIND = SkSplitCSV(i)
093300030715     C                   ENDIF
093400030715     C*** LOD
093500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
093600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
093700030715     C                   ENDIF
093800030715     C*** PRD
093900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
094000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
094100030715     C                   ENDIF
094200030721     C*** NZD
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
094400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030715     C*** RMA
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
094800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** NOT
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
095200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** NT2
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
095600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** VCA
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
096000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** VAS
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** FFD
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030721     C*** GC1
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** GC2
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** TSP
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
098000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** NAS
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** TIC
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** GCA
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** XCO
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** CTM
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
100000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** TCR
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** CTS
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** FTM
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** VAD
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** GMA
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
102000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GGA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** GVA
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** TC1
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TC2
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** SCL
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
104000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** RMO
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
104400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** NMO
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104900030721     C                   ENDIF
105000040119     C*** CBO
105100040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105200040706     C                   EVAL      VABCBO = SkSplitCSV(i)
105300040119     C                   ENDIF
105400030721     C*
105500040706     C* Reperisco quindi i campi numerici...
105600040706     C*** NSP
105700040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105800040706     C                   EVAL      PiStr=SkSplitCSV(i)
105900040706     C                   EXSR      CHKNUM
106000040706     C                   IF        PiInt=*on
106100040706     C                   Z-ADD     PiVal         VABNSP
106200040706     C                   ELSE
106300040706     C                   SETON                                        31
106400040706     C                   EVAL      VABNSP = *zeros
106500040706     C                   EVAL      vinmsg = %trimr(vinmsg)
106600040706     C                             + ' ' + 'VABNSP'
106700040706     C                   ENDIF
106800040706     C                   ENDIF
106900070208     C*** RMN
107000040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
107100060530     C                   EVAL      PiStr=%subst(SkSplitCSV(i):2)
107200040706     C                   EXSR      CHKNUM
107300040706     C                   IF        PiInt=*on
107400040706     C                   Z-ADD     PiVal         VABRMN
107500040706     C                   ELSE
107600040706     C                   SETON                                        32
107700040706     C                   EVAL      VABRMN = 1
107800040706     C                   EVAL      vinmsg = %trimr(vinmsg)
107900060530     C                             + ' ' + 'VABRMN'
108000040706     C                   ENDIF
108100040706     C                   ENDIF
108200030721     C*** CAD
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
108400030721     C                   EVAL      PiStr=SkSplitCSV(i)
108500030721     C                   EXSR      CHKNUM
108600030721     C                   IF        PiInt=*on
108700030721     C                   Z-ADD     PiVal         Num5_0
108800030721     C                   MOVEL(p)  Num5_0        VABCAD
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      VABCAD = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABCAD'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** NCL
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiInt=*on
110100030721     C                   Z-ADD     PiVal         VABNCL
110200030721     C                   ELSE
110300030721     C                   SETON                                        32
110400030721     C                   EVAL      VABNCL = *zeros
110500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110600040706     C                             + ' ' + 'VABNCL'
110700030721     C                   ENDIF
110800030721     C                   ENDIF
110900030721     C*** PKB
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
111100030721     C                   EVAL      PiStr=SkSplitCSV(i)
111200030721     C                   EXSR      CHKNUM
111300030721     C                   IF        PiNum=*on
111400050330     C                   Z-ADD(h)  PiVal         VABPKB
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      VABPKB = *zeros
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABPKB'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** CAS
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiNum=*on
112700030721     C                   Z-ADD     PiVal         VABCAS
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000030721     C                   EVAL      VABCAS = *zeros
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABCAS'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** CCM
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on      AND
114000030721     C                             PiVal<=9999999 AND
114100030721     C                             PiVal>*zeros
114200030721     C                   Z-ADD     PiVal         VABCCM
114300070208     C   60              Z-ADD     PiVal         VATCCM
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      VABCCM = *zeros
114700070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
114800070208     C                             + ' ' + 'VABCCM'
114900070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
115000070208     C                             + ' ' + 'VABCCM VATCCM'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** LNP
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on    AND
115800030721     C                             PiVal<=999   AND
115900030721     C                             PiVal>*zeros
116000030721     C                   Z-ADD     PiVal         VABLNP
116100070208     C   60              Z-ADD     PiVal         VATLNP
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABLNP = *zeros
116500070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
116600070208     C                             + ' ' + 'VABLNP'
116700070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
116800070208     C                             + ' ' + 'VABLNP VATLNP'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** NRS
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on AND
117600030721     C                             PiVal<=99
117700030721     C                   Z-ADD     PiVal         VABNRS
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABNRS'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** LNA
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on    AND
118900030721     C                             PiVal<=999   AND
119000030721     C                             PiVal>*zeros
119100030721     C                   Z-ADD     PiVal         VABLNA
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABLNA = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABLNA'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** CTR
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on  AND
120400030721     C                             PiVal<=999
120500030721     C                   Z-ADD     PiVal         VABCTR
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABCTR'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** IAS
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiNum=*on
121700030721     C                   Z-ADD     PiVal         VABIAS
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABIAS'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** VLB
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiNum=*on
122900030721     C                   Z-ADD     PiVal         VABVLB
123000030721     C                   ELSE
123100030721     C                   SETON                                        32
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300030721     C                             + ' ' + 'VABVLB'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** QFT
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiNum=*on
124100030721     C                   Z-ADD     PiVal         VABQFT
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABQFT'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** NCD
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on      AND
125300030721     C                             PiVal<=9999999
125400030721     C                   Z-ADD     PiVal         VABNCD
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABNCD'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** NCA
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on      AND
126600030721     C                             PiVal<=9999999
126700030721     C                   Z-ADD     PiVal         VABNCA
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABNCA'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** ZNC
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on AND
127900030721     C                             PiVal<=99
128000030721     C                   Z-ADD     PiVal         VABZNC
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABZNC'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** DCR
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiInt=*on       AND
129200030721     C                             PiVal<=99999999
129300030721     C                   Z-ADD     PiVal         VABDCR
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABDCR'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** HCR
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on   AND
130500030721     C                             PiVal<=9999
130600030721     C                   Z-ADD     PiVal         VABHCR
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABHCR'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** VMD
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiNum=*on
131800030721     C                   Z-ADD     PiVal         VABVMD
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABVMD'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** ANT
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on        AND
133000030721     C                             PiVal<=999999999
133100030721     C                   Z-ADD     PiVal         VABANT
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABANT'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** CMO
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on
134300030721     C                   Z-ADD     PiVal         Num5_0
134400030721     C                   MOVEL(p)  Num5_0        VABCMO
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      VABCMO = *zeros
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABCMO'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200040706     C*
135300040706     C* Gestisco i campi "personalizzati"
135400030715     C*
135500030715     C                   ENDIF
135600030715     C                   EVAL      i = i + 1
135700030715     C                   ENDDO
135800040706     C*
135900040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
136000040706     C* Reperisco la provincia dal CAP e dalla località
136100040706     C                   IF        VABCAD <> *blanks
136200040706     C                   CLEAR                   TISI95DS
136300040706     C                   EVAL      I95TCN = '3'
136400040706     C                   Z-ADD     datcor        I95DAT
136500040706     C                   EVAL      I95CAP = VABCAD
136600040706     C                   EVAL      I95LOC = VABLOD
136700040706     C                   CALL      'TISI95R'
136800040706     C                   PARM                    TISI95DS
136900040706     C                   EVAL      VABPRD = O95PRV
137000040706     C                   ENDIF
137100070208     C*
137200060530     C* NSP => Stacco un numeratore da AZNUM se NSP a 0
137300070208     C                   IF        VABNSP = 0
137400060530     C                   clear                   TRUL33DS
137500060530     C                   eval      I33OPE = *zeros
137600060530     C                   eval      I33CNU = 302
137700060530     C                   eval      I33NUM = 1
137800060530     C                   movel     TRUL33DS      KPJBU
137900060530     C                   call      'TRUL33R'
138000060530     C                   parm                    KPJBA
138100060530     C                   movel     KPJBU         TRUL33DS
138200060530     C                   if        O33ERR = *zeros
138300060530     C                   z-add     O33NRF        VABNSP
138400070208     C   60              z-add     O33NRF        VATNSP
138500060530     C                   else
138600070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
138700070208     C                             + ' ' + 'VABNSP'
138800070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
138900070208     C                             + ' ' + 'VABNSP VATNSP'
139000060530     C                   endif
139100060530     C                   ENDIF
139200040706     C*
139300040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
139400040706     C* d valorizzarli sempre entrambi
139500040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
139600040706     C                   Z-ADD     VABNSP        VABRMN
139700040706     C                   ENDIF
139800040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
139900040706     C                   MOVEL     VABRMN        VABRMA
140000040706     C                   ENDIF
140100040113     C*
140200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
140300040113     C                   IF        VABCAS > *zeros
140400060327     C                   EVAL      VABVCA = 'EUR'
140500040119     C                   IF        VABCBO = '1'
140600040113     C                   EVAL      VABCBO = '4'
140700040119     C                   ENDIF
140800040119     C                   IF        VABCBO = '2'
140900040119     C                   EVAL      VABCBO = '6'
141000040119     C                   ENDIF
141100040113     C                   ENDIF
141200020204     C*
141300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
141400020204     C                   EXSR      CHKIMPDIV
141500030715     C                   ENDIF
141600020204     C*
141700010607     C                   ENDSR
141800010607     C*----------------------------------------------------*
141900010601
142000020204
142100020204     C*----------------------------------------------------*
142200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
142300020204     C*----------------------------------------------------*
142400020204     C     CHKIMPDIV     BEGSR
142500020204     C*
142600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
142700020204     C                   Z-ADD     *zeros        wrkDec            9 9
142800020204     C*
142900020204     C* Come prima cosa effettuo considerazioni sulla divisa
143000020204     C                   IF        vabIAS > *zeros
143100020204     C                   IF        vabVAS <> 'EUR'
143200020204     C                   EVAL      vabVAS =  'ITL'
143300020204     C                   ENDIF
143400020204     C                   ENDIF
143500020204     C*
143600020204     C                   IF        vabCAS > *zeros
143700020204     C                   IF        vabVCA <> 'EUR'
143800020204     C                   EVAL      vabVCA =  'ITL'
143900020204     C                   ENDIF
144000020204     C                   ENDIF
144100020204     C*
144200020204     C                   IF        vabVMD > *zeros
144300020204     C                   IF        vabVAD <> 'EUR'
144400020204     C                   EVAL      vabVAD =  'ITL'
144500020204     C                   ENDIF
144600020204     C                   ENDIF
144700020204     C*
144800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
144900020204     C                   Z-ADD     vabIAS        wrkDec
145000020204     C                   IF        wrkDec > *zeros
145100020204     C                   IF        vabVAS = 'ITL'
145200020204     C                   EVAL      vabIAS = *zeros
145300020204     C                   ENDIF
145400020204     C                   ENDIF
145500020204     C*
145600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
145700020204     C                   Z-ADD     vabCAS        wrkDec
145800020204     C                   IF        wrkDec > *zeros
145900020204     C                   IF        vabVCA = 'ITL'
146000020204     C                   EVAL      vabCAS = *zeros
146100020204     C                   ENDIF
146200020204     C                   ENDIF
146300020204     C*
146400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
146500020204     C                   Z-ADD     vabVMD        wrkDec
146600020204     C                   IF        wrkDec > *zeros
146700020204     C                   IF        vabVAD = 'ITL'
146800020204     C                   EVAL      vabVMD = *zeros
146900020204     C                   ENDIF
147000020204     C                   ENDIF
147100020204     C*
147200020204     C                   ENDSR
147300020204     C***
147400020204
147500010330
147600010330
147700010330     C*----------------------------------------------------*
147800010330     C*  CONTROLLO NUMERICITA' CAMPI
147900010330     C*----------------------------------------------------*
148000010330     C     CHKNUM        BEGSR
148100010330     C*
148200010606     C                   IF        PiDecChr = *blanks
148300030715     C                   EVAL      PiDecChr = CharNUM
148400010606     C                   ENDIF
148500010606     C*
148600010606     C                   CALL(e)   'ISNUMERIC'
148700010330     C                   PARM                    PiStr            30
148800010606     C                   PARM                    PiDecChr          1
148900010330     C                   PARM      *ZEROS        PiVal            30 9
149000010330     C                   PARM      '0'           PiInt             1
149100010330     C                   PARM      '0'           PiNum             1
149200010330     C                   IF        %error
149300010606     C                   EVAL      PiNum=*off
149400010330     C                   ENDIF
149500010330     C*
149600010330     C                   ENDSR
149700010330     C***
149800010330
149900010601
150000010601
150100010601
150200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
150300010601     C     repfil        BEGSR
150400010601     C*
150500010601     C                   if        invfil = *zeros and
150600010601     C                             depfil > *zeros and
150700010629     C                             (vinflg = *blanks or
150800010629     C                              vinflg = *zeros)
150900010601     C
151000010601     C                   eval      invfil = depfil
151100010601     C                   endif
151200010601     C*
151300010601     C                   if        depfil <> invfil and
151400010601     C                             invfil > *zeros
151500010601     C                   eval      flgMulti = '1'
151600010601     C                   if        vinflg = *blanks
151700010601     C                   add       1             cntNonEl
151800010601     C                   endif
151900010601     C                   endif
152000010601     C*
152100010601     C                   if        vinflg = '2'
152200010601     C                   eval      flgStato = '2'
152300010601     C                   endif
152400010601     C*
152500010601     C                   ENDSR
152600010601     C***
152700010601
152800010601
152900010601
153000010330
153100040119      /TITLE Invio dei dati al punto operativo.
153200040119     C     invio         BEGSR
153300040119     C*
153400070301     C* 1° invio EDIVAT
153500070208     C                   if        *in60 = *on
153600040119     C                   reset                   dscmz
153700040119     C                   move      vlrpoi        cmzdst
153800070301     C                   eval      cmzfld = 'EDIVATWR'
153900040119     C                   eval      cmzmbd = vlrhdl
154000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
154100040119     C***                if        prmfir = *blanks
154200070301     C                   eval      cmzfla = 'EDIVAT0F'
154300070301     C                   eval      cmzmba = 'EDIVAT0F'
154400040119     C***                else
154500040119     C***                eval      cmzfla = prmfir
154600040119     C***                eval      cmzmba = prmfir
154700040119     C***                endif
154800040119     C                   eval      cmznrr = *zeros
154900040119     C                   move      §ctrokvt      cmznrr
155000040119     C                   eval      cmzlba = vlrfl1
155100040119     C                   call(e)   'TIS711C'
155200040119     C                   parm                    dscmz
155300040119     C                   parm      *blanks       esito
155400070208     C                   endif
155500040119     C                   if        %error
155600040119     C                             or cmzerr = '1'
155700040119     C                             or esito  = '1'
155800040119     C                   eval      wrkesito = '3'
155900040119     C                   else
156000040119     C*
156100070301     C* 2° invio EDIVAB
156200040119     C                   reset                   dscmz
156300040119     C                   move      vlrpoi        cmzdst
156400040119     C                   eval      cmzfld = vlrfou
156500040119     C                   eval      cmzmbd = vlrhdl
156600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
156700040119     C***                if        prmfir = *blanks
156800070301     C                   eval      cmzfla = 'EDIVAB0F'
156900070301     C                   eval      cmzmba = 'EDIVAB0F'
157000040119     C***                else
157100040119     C***                eval      cmzfla = prmfir
157200040119     C***                eval      cmzmba = prmfir
157300040119     C***                endif
157400040119     C                   eval      cmznrr = *zeros
157500040119     C                   move      §ctrokvb      cmznrr
157600040119     C                   eval      cmzlba = vlrfl1
157700040119     C                   call(e)   'TIS711C'
157800040119     C                   parm                    dscmz
157900040119     C                   parm      *blanks       esito
158000040119     C                   if        %error
158100040119     C                             or cmzerr = '1'
158200040119     C                             or esito  = '1'
158300040119     C                   eval      wrkesito = '3'
158400040119     C                   endif
158500040119     C                   endif
158600040119     C*
158700040119     C                   ENDSR
158800040119     C***
158900010601
159000010601
159100010601
159200010601
159300010601      /TITLE Invio dei dati al punto operativo.
159400010601     C     opeini        BEGSR
159500010601     C*
159600010601     C* Inizializzo flag e contatori operativi
159700010601     C                   movel     '0'           flgGiro           1
159800010601     C                   movel     '0'           flgMulti          1
159900010601     C                   movel     '1'           flgStato          1
160000010615     C                   movel     '0'           flgOk             1
160100010601     C                   z-add     *zeros        cntNonEl         10 0
160200010601     C                   z-add     *zeros        depfil            3 0
160300010601     C                   z-add     *zeros        invfil            3 0
160400010601     C*
160500010601     C                   ENDSR
160600010601     C***
160700010601
160800010601
160900050421
161000050421
161100050421     C     *pssr         BEGSR
161200050421     C*
161300050421     C                   eval      wrkesito = '2'
161400050421     C*
161500050421     C                   ENDSR     '*CANCL'
161600050421     C***
161700050421
161800010601
161900010330
162000010330
162100000613     C     *inzsr        BEGSR
162200990910     C*
162300990910     C     *entry        plist
162400990920     C                   parm                    tivlrds
162500990921     C                   parm      wrkesito      esito
162600000724     C                   parm                    prmlit
162700000710     C                   parm                    prmfir
162800070306     C*
162900070306     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
163000070306     C                   move(p)   vlrMSG        wNomeFile
163100070306     C                   if        wNomeFile <> *blanks
163200070306     C                   eval      wNomeFile = %subst(wNomeFile:15:6)
163300070306     C                   else
163400070306     C                   eval      wNomeFile = 'CECCATO'
163500070306     C                   endif
163600010330     C*
163700010330     C* CALCOLA LA DATA CORRENTE
163800010330     C                   time                    wn14             14 0
163900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
164000070301     C                   movel     wn14          oracor            6 0          *ORA
164100010330     C                   z-add     wn8           g08dat
164200010330     C                   z-add     *zeros        g08inv
164300010330     C                   movel     '0'           g08err
164400010330     C                   call      'XSRDA8'
164500010330     C                   parm                    wlbda8
164600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
164700000613     C*
164800000613     C                   ENDSR
164900000613     C***
165000990908
165100070216     Otitvy1p   E            riepilogo         2
165200990915     O                                              'Upload via Internet'
165300990915     O                                           +1 'Traduzione TIVIN00R -'
165400070301     O                                           +1 'EDIVABWR'
165500990915     O                                           +1 'Testate bolle'
165600990915     O          E            riepilogo   2
165700990915     O                       wrkdata
165800990915     O                       wrkora              +1
165900990915     O                       procname            +1
166000990915     O          E            riepilogo   2
166100990915     O                                              'Cliente..................:'
166200010605     O                       VABCCM        z     +1
166300990915     O          E            riepilogo   2
166400990920     O                                              'Riferimento Strategi.....:'
166500990920     O                       vlrhdl              +1
166600990915     O          E            riepilogo   2
166700990915     O                                              'Giusti...................:'
166800040119     O                       §CTROKVB      2   +  1
166900990915     O          E            riepilogo   2
167000990915     O                                              'Sbagliati e corretti.....:'
167100971022     O                       §CTRMO        2   +  1
167200990915     O          E            riepilogo   2
167300990915     O                                              'Sbagliati e scartati.....:'
167400971022     O                       §CTRNO        2   +  1
167500000613
167600070216     Otitvy1ps  E            testdett          2
167700000613     O                                              'Upload via Internet'
167800000613     O                                           +1 'Traduzione TIVIN00R -'
167900070301     O                                           +1 'EDIVABWR'
168000000613     O                                           +1 'Testate bolle'
168100000616     O          E            testdett    3
168200000613     O                                           +2 'N° rec'
168300000613     O                                           +3 'Anteprima contenuto'
168400000616     O          E            rigadett    2
168500000613     O                       rrnum               +2
168600000621     O                       recko               +3
168700000616     O          E            findett     2
168800000613     O                       wrkdata
168900000613     O                       wrkora              +1
169000000613     O                       procname            +1
169100000616     O          E            findett     2
169200000613     O                                              'Cliente..................:'
169300010605     O                       VABCCM        z     +1
169400000616     O          E            findett     2
169500000613     O                                              'Riferimento Strategi.....:'
169600000613     O                       vlrhdl              +1
169700000616     O          E            findett     2
169800000613     O                                              'Giusti...................:'
169900040119     O                       §CTROKVB      2   +  1
170000000616     O          E            findett     2
170100000613     O                                              'Sbagliati e corretti.....:'
170200000613     O                       §CTRMO        2   +  1
170300000616     O          E            findett     2
170400000613     O                                              'Sbagliati e scartati.....:'
170500000613     O                       §CTRNO        2   +  1
170600000613** CMD - COMANDI CL
170700070216OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
170800070216OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
170900070216DLTOVR FILE(TITVY1P TITVY1PS) LVL(*)
171000010606
171100000613
171200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
171300070216VABRMN~X02~X03~X04~X05~X06~X07~X08~VABRMA~VABNAS~VABPKB~X12~
171400070216X13~X14~X15~X16~X17~X18~X19~X20~VABRSD~VABRD2~VABIND~VABLOD~
171500070216X25~X26~VABCAD~X28~X29~X30~X31~X32~X33~X34~X35
