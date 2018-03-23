000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800060530     Ftitvt3p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060530     Ftitvt3ps  O    f  198        PRINTER usropn
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
003000040706     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400040706     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000030715     D CharCSV         S              1
005100030715     D CharTXT         S              1
005200030715     D CharNUM         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700030822     D*------------------
005800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005900030822     D*------------------
006000030822     D posDaDft        S              3  0 INZ(*zeros)
006100030822     D posADft         S              3  0 INZ(*zeros)
006200030822     D j               s              3  0 INZ(1)
006300030822     D wGiroDft        s              1  0 INZ(*zeros)
006400010330     D*------------------
006500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600010330     D*------------------
006700010330     D WLBDA8          DS                  INZ
006800010330     D  G08DAT                 1      8  0
006900010330     D  G08INV                 9     16  0
007000010330     D  G08ERR                17     17
007100010330     D  G08TGI                18     22  0
007200060327     D*------------------
007300060327     D* DS REPERIMENTO NUMERATORE
007400060327     D*------------------
007500060327     D trul33ds      e ds                  inz
007600060327     D*------------------
007700060327     D* DS ARCHITETTURA
007800060327     D*------------------
007900060327     D kpjba         e ds                  inz
008000031201     D*------------------
008100031201     D* Costanti
008200031201     D*------------------
008300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500010330
008600010330
008700010330
008800990915     C                   time                    wrkdata
008900990915     C                   time                    wrkora
009000000913     C                   reset                   rrnum
009100990921     C                   reset                   esito
009200990921     C                   reset                   wrkesito
009300010601     C*
009400010601     C                   exsr      opeini
009500010605     C                   exsr      rwvab
009600010601     C*
009700010601     C                   seton                                        lr
009800010601
009900010601
010000010601
010100010601     C*--------------------------------------------------------
010200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010300010601     C*--------------------------------------------------------
010400010601     C     PREELA        BEGSR
010500010601     C*
010600010601     C* SE OCCORRE SPEDIRE IN FILIALE
010700010601     C                   if        invfil <> *zeros and
010800010601     C                             flgGiro = '0'
010900010601     C*
011000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011100010601     C                   eval      flgGiro = '1'
011200000724     C*
011300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500010601     C     invfil        chain     azorg01l
011600000724     C                   if        %found
011700000724     C                   movel(p)  CMD(1)        depcmd
011800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011900000724     C*
012000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000724     C                   PARM                    depcmd
012400000724     C                   PARM                    LENGH
012500000724     C*
012600000724     C                   endif
012700000616     C*
012800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012900000616     C                   movel(p)  CMD(2)        depcmd
013000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013100000616     C*
013200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    depcmd
013600000616     C                   PARM                    LENGH
013700000613     C*
013800060530     C                   if        not %open(titvt3ps)
013900060530     C                   open      titvt3ps
014000030715     C                   except    testdett
014100000613     C                   endif
014200010601     C*
014300010601     C                   endif
014400010601     C*
014500010601     C                   ENDSR
014600010601     C***
014700010601
014800010601
014900010601
015000010601
015100010601     C*--------------------------------------------------------
015200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015300010601     C*--------------------------------------------------------
015400010601     C     ENDELA        BEGSR
015500990915     C*
015600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015700040119     C*
015800040119     C* Effettuo la chiamata al CLLE preposto
015900040119     C                   call(e)   'TITVVTC'
016000040119     C                   parm                    parccm
016100040119     C                   parm                    parmbr
016200040119     C                   parm      '2'           paropz
016300050201     C*
016400050201     C* Effettuo lancio TISI95 solo x chiusura
016500050201     C                   CLEAR                   TISI95DS
016600050201     C                   EVAL      I95TLA = 'C'
016700050201     C                   CALL      'TISI95R'
016800050201     C                   PARM                    TISI95DS
016900060327     C*
017000060327     C* Effettuo lancio TRUL33 solo x chiusura
017100060327     C                   clear                   TRUL33DS
017200060327     C                   eval      I33TLA = 'C'
017300060327     C                   movel     TRUL33DS      KPJBU
017400060327     C                   call      'TRUL33R'
017500060327     C                   parm                    KPJBA
017600000613     C*
017700060530     C                   if        %open(titvt3ps)
017800000616     C                   except    findett
017900060530     C                   close     titvt3ps
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
019900060530     C                   if        not %open(titvt3p)
020000060530     C   50              open      titvt3p
020100990915     C                   endif
020200990915     C*
020300030715     C   50              except    riepilogo
020400990915     C*
020500060530     C                   if        %open(titvt3p)
020600060530     C   50              close     titvt3p
020700990914     C                   endif
020800000613     C*
020900000613     C                   ENDSR
021000000613     C***
021100000613
021200010601
021300010601
021400010330     C*--------------------------------------------------------
021500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021600010330     C*--------------------------------------------------------
021700010605     C     RWVAB         BEGSR
021800010330     C*
021900010330     C                   if        not %open(tivin00r)
022000010330     C                   open      tivin00r
022100010330     C                   endif
022200021025     C                   if        not %open(fivabwwr)
022300021025     C                   open      fivabwwr
022400010330     C                   endif
022500040119     C*
022600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022700040119     C                   exsr      prevat
022800040119     C*
022900040119     C                   if        chkcall = '0'
023000040119     C*
023100040119     C                   if        not %open(fivatwwr)
023200040119     C                   open      fivatwwr
023300040119     C                   endif
023400010330     C*
023500040119     C                   clear                   §CTROKVB          7 0
023600040119     C                   clear                   §CTROKVT          7 0
023700040119     C                   clear                   §CTRMO            7 0
023800040119     C                   clear                   §CTRNO            7 0
023900050330     C*
024000050330     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024100050330     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024200060531     C***                exsr      inzvar
024300060531     C***                exsr      impvab
024400050330     C*
024500050330     C                   do        *HIVAL
024600010330     C*
024700040706     C                   read      tivin00r                               70
024800010618     C*
024900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025000010618     C                   if        vinflg = '1'
025100010618     C                   eval      flgOk = '1'
025200010618     C                   endif
025300050330     C*
025400050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025500050330     C                   z-add     *zeros        wLenVINDTA        4 0
025600050330     C                   z-add     *zeros        wFlgVINDTA        1 0
025700050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
025800050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
025900050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026000050330     C                   z-add     1             wFlgVINDTA
026100050330     C                   leave
026200050330     C                   endif
026300050330     C                   enddo
026400010618     C*
026500050330     C                   if        vindta > *blanks AND
026600050330     C                             wFlgVINDTA = 1
026700010330     C                   add       1             rrnum
026800010330     C*
026900010601     C                   if        *in70 = *off and
027000010330     C                             (vinflg = *blanks
027100010330     C                              or vinflg = '0'
027200010330     C                              or vinflg = '2')
027300010330     C*
027400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027500010711     C                   if        vinflg = *blanks or vinflg = '0'
027600010711     C                   clear                   vinmsg
027700010711     C                   endif
027800010601     C*
027900010330     C                   exsr      inzvar
028000010605     C                   exsr      impvab
028100010601     C*
028200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028300010605     C                   eval      depfil = VABLNP
028400010601     C                   exsr      repfil
028500010601     C                   if        depfil = invfil
028600021025     C                   if        vlrpoi = 999
028700030822     C                   move(P)   invfil        VABFGS
028800040119     C                   move(P)   invfil        VATFGS
028900021025     C                   else
029000030822     C                   move(P)   vlrpoi        VABFGS
029100040119     C                   move(P)   vlrpoi        VATFGS
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
030200040119     C  N31              exsr      wrivab
030300040119     C                   endif
030400010604     C*
030500010604     C                   if        *in31 = *off and
030600010604     C                             *in32 = *off
030700010604     C                   eval      vinflg = '1'
030800010604     C                   else
030900010604     C                   eval      recko = vindta
031000010604     C                   except    rigadett
031100010604     C                   eval      vinflg = '2'
031200010604     C                   endif
031300010604     C                   endif
031400010601     C*
031500010604     C                   endif
031600010604     C*
031700010330     C                   else
031800010330     C                   eval      vinflg = '1'
031900010330     C                   endif
032000010601     C*
032100010601     C  N70              update    tivin000
032200010330     C*
032300030822     C  N70              enddo
032400040119     C*
032500040119     C                   endif                                                  (endif - chkcall)
032600010601     C*
032700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032800010601     C                   if        cntNonEl = *zeros or
032900010601     C                             flgMulti = '0'
033000010330     C* Se non ci sono record con errori ...
033100010601     C                   if        §ctrno = 0 and
033200010604     C                             §ctrmo = 0 and
033300010601     C                             flgStato <> '2'
033400010330     C* ... restituisco esito OK.
033500010330     C                   eval      wrkesito = '0'
033600010330     C                   else
033700040119     C                   if        §ctrokvb > 0
033800010330     C                   eval      wrkesito = '1'
033900010330     C                   else
034000010615     C                   if        flgOk = '0'
034100010615     C                   eval      wrkesito = '2'
034200010615     C                   else
034300010615     C                   eval      wrkesito = '6'
034400010615     C                   endif
034500010330     C                   endif
034600010330     C                   endif
034700010601     C                   else
034800010601     C                   eval      wrkesito = '9'
034900010601     C                   endif
035000010330     C*
035100010330     C                   if        %open(tivin00r)
035200010330     C                   close     tivin00r
035300010330     C                   endif
035400021025     C                   if        %open(fivabwwr)
035500021025     C                   close     fivabwwr
035600010330     C                   endif
035700040512     C                   if        %open(fivatwwr)
035800040512     C                   close     fivatwwr
035900040512     C                   endif
036000010601     C*
036100010601     C                   if        vlrpoi <> 999
036200010601     C                   eval      invfil = vlrpoi
036300010601     C                   endif
036400010330     C*
036500040119     C                   if        §ctrokvb > 0
036600010601     C                             and invfil > *zeros
036700010330     C                   exsr      invio
036800010330     C                   endif
036900010601     C*
037000010618     C                   if        flgGiro = '1'
037100010601     C                   exsr      endela
037200010618     C                   endif
037300010330     C*
037400010330     C                   ENDSR
037500010330     C***
037600040119
037700040119
037800040119
037900040119     C*----------------------------------------------------*
038000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038100040119     C*----------------------------------------------------*
038200040119     C     WRIVAB        BEGSR
038300040119     C*
038400040119     C                   write     fivab000                                     => scarico il VAB
038500040119     C*
038600040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
038700050330     C                   exsr      wrivat
038800040119     C*
038900040119     C                   ENDSR
039000040119
039100040119
039200040119
039300060327     C*----------------------------------------------------*
039400060327     C*  SCARICAMENTO BUFFER RECORDS VAT
039500060327     C*----------------------------------------------------*
039600060327     C     WRIVAT        BEGSR
039700060327     C*
039800060327     C* Attribuisco la stessa chiave della testata bolle
039900060327     C                   EVAL      VATAAS = VABAAS
040000060327     C                   EVAL      VATLNP = VABLNP
040100060327     C                   EVAL      VATNSP = VABNSP
040200060327     C*
040300060327     C* Imposto il tipo record sul file VAT a 'A' x indicare il referente destinatario
040400060327     C                   EVAL      VATTRC = 'A'
040500060327     C                   EVAL      VATNOT = %trim(wREF)
040600060327     C*
040700060327     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
040800060327     C                   IF        VATNOT <> *blanks
040900060327     C                   WRITE     FIVAT000
041000060327     C                   ADD       1             §CTROKVT
041100060327     C                   ENDIF
041200060327     C*
041300060327     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
041400060327     C                   EVAL      VATTRC = 'B'
041500060327     C                   EVAL      VATNOT = %trim(wTEL)
041600060327     C*
041700060327     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
041800060327     C                   IF        VATNOT <> *blanks
041900060327     C                   WRITE     FIVAT000
042000060327     C                   ADD       1             §CTROKVT
042100060327     C                   ENDIF
042200060327     C*
042300060327     C                   ENDSR
042400040119
042500040119
042600040119
042700040119     C*----------------------------------------------------*
042800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042900040119     C*----------------------------------------------------*
043000040119     C     PREVAT        BEGSR
043100040119     C*
043200040119     C* Compongo il nome del membro da dare al FIVATWWR
043300040119     C                   eval      parmbr = vlrhdl
043400040119     C                   movel     'M'           parmbr
043500050330     C                   eval      parccm = %subst(vlrKSC:2:7)
043600040119     C                   eval      paropz = '1'
043700040119     C* Effettuo la chiamata al CLLE preposto
043800040119     C                   call(e)   'TITVVTC'
043900040119     C                   parm                    parccm
044000040119     C                   parm                    parmbr
044100040119     C                   parm                    paropz
044200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044300040119     C                   if        %error
044400040119     C                   movel     '1'           chkcall
044500040119     C                   else
044600040119     C                   movel     '0'           chkcall
044700040119     C                   endif
044800040119     C*
044900040119     C                   ENDSR
045000010601
045100010601
045200010601
045300010330     C*----------------------------------------------------*
045400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045500010330     C*----------------------------------------------------*
045600040119     C     INZVAR        BEGSR
045700010330     C*
045800030822     C* Inizializzo variabili di wrk
045900020204     C                   Z-ADD     *zeros        Num5_0            5 0
046000060327     C                   MOVEL     *blanks       wTEL             35
046100060327     C                   MOVEL     *blanks       wREF             35
046200060327     C                   MOVEL     *blanks       wRMN             15
046300030822     C*
046400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046500030822     C                   CLEAR                   FIVAB000
046600040119     C                   CLEAR                   FIVAT000
046700030822     C                   CLEAR                   SkSplitCSV
046800030822     C                   EVAL      i = 1
046900030822     C                   EVAL      posDa = *zeros
047000030822     C                   EVAL      posA  = *zeros
047100030822     C*
047200030822     C* Reimposto i valori di default
047300030822     C                   EXSR      DEFCAM
047400030822     C*
047500010330     C                   ENDSR
047600010330     C*----------------------------------------------------*
047700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047800010330     C*----------------------------------------------------*
047900010330     C     DEFCAM        BEGSR
048000040119     C*
048100040119     C* Imposto i valori d default a programma dei campi fondamentali
048200060530     C                   EVAL      VABCCM = 1110567
048300060530     C                   EVAL      VATCCM = 1110567
048400060530     C                   EVAL      VABLNP = 111
048500060530     C                   EVAL      VATLNP = 111
048600060327     C                   EVAL      VABCTR = 300
048700040119     C                   EVAL      VABCBO = '1'
048800060530     C                   EVAL      VABPKB = 1
048900060530     C                   EVAL      VABTIC = ' '
049000030822     C*
049100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049200030715     C* e delimitatore testo.
049300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049600050330     C*
049700050330     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049800050330     C                   IF        %subst(vlrppt:1:1) = 'S'
049900050330     C                   SETON                                        50
050000050330     C                   ELSE
050100050330     C                   SETOFF                                       50
050200050330     C                   ENDIF
050300030822     C*
050400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050500030822     C                   EVAL      posDaDft = 1
050600030822     C                   EVAL      posADft  = 0
050700030822     C                   EVAL      wGiroDft = 0
050800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050900030822     C                             posDaDft > 0
051000030822     C*
051100030822     C* Gestisco il 1° giro
051200030822     C                   IF        wGiroDft = 0
051300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051500030822     C* Incremento il contatore dei "giri"
051600030822     C                   EVAL      wGiroDft = 1
051700030822     C                   ELSE
051800030822     C                   EVAL      posDaDft = posADft
051900030822     C                   ENDIF
052000030822     C* Eseguo lo scan x trovare la fine del campo corrente
052100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052200030822     C*
052300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052500030822     C                   IF        posDaDft > 0 AND
052600030822     C                             posADft  > 0
052700030822     C* NCL
052800030822     C                   IF        %subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):1:3)
053100030822     C                             = 'NCL'
053200030822     C                   EVAL      PiStr=%trim(%subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500030822     C                   EXSR      CHKNUM
053600030822     C                   IF        PiInt=*on
053700030822     C                   Z-ADD     PiVal         VABNCL
053800030822     C                   ENDIF
053900030822     C                   ENDIF
054000030822     C* CCM
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'CCM'
054500030822     C                   EVAL      PiStr=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   EXSR      CHKNUM
054900030822     C                   IF        PiInt=*on
055000030822     C                   Z-ADD     PiVal         VABCCM
055100040119     C                   Z-ADD     PiVal         VATCCM
055200030822     C                   ENDIF
055300030822     C                   ENDIF
055400030822     C* LNP
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'LNP'
055900030822     C                   EVAL      PiStr=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   EXSR      CHKNUM
056300030822     C                   IF        PiInt=*on
056400030822     C                   Z-ADD     PiVal         VABLNP
056500040119     C                   Z-ADD     PiVal         VATLNP
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* NRS
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'NRS'
057300030822     C                   EVAL      PiStr=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   EXSR      CHKNUM
057700030822     C                   IF        PiInt=*on
057800030822     C                   Z-ADD     PiVal         VABNRS
057900030822     C                   ENDIF
058000030822     C                   ENDIF
058100030822     C* CTR
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'CTR'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiInt=*on
059100030822     C                   Z-ADD     PiVal         VABCTR
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* PKB
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'PKB'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiNum=*on
060400050330     C                   Z-ADD     PiVal         VABPKB
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* VLB
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'VLB'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiNum=*on
061700030822     C                   Z-ADD     PiVal         VABVLB
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* QFT
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'QFT'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiNum=*on
063000030822     C                   Z-ADD     PiVal         VABQFT
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* CBO
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'CBO'
063800030822     C                   EVAL      VABCBO=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200030822     C* TSP
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'TSP'
064700030822     C                   EVAL      VABTSP=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* VAS
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'VAS'
065600030822     C                   EVAL      VABVAS=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* VCA
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'VCA'
066500030822     C                   EVAL      VABVCA=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* GCA
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'GCA'
067400030822     C                   EVAL      VABGCA=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* CTM
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'CTM'
068300030822     C                   EVAL      VABCTM=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* FFD
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'FFD'
069200030822     C                   EVAL      VABFFD=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* VAD
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VAD'
070100030822     C                   EVAL      VABVAD=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* GMA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'GMA'
071000030822     C                   EVAL      VABGMA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* GGA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GGA'
071900030822     C                   EVAL      VABGGA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* GVA
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'GVA'
072800030822     C                   EVAL      VABGVA=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* TC1
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'TC1'
073700030822     C                   EVAL      VABTC1=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* TC2
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'TC2'
074600030822     C                   EVAL      VABTC2=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* ...
075100030822     C                   ENDIF
075200030822     C                   ENDDO
075300020204     C*
075400010330     C                   ENDSR
075500010607     C*----------------------------------------------------*
075600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075700010607     C*----------------------------------------------------*
075800010607     C     IMPVAB        BEGSR
075900010607     C*
076000010607     C                   SETOFF                                       3132
076100030515     C*
076200010607     C                   MOVEL     datcor        VABAAS
076300040119     C                   MOVEL     datcor        VATAAS
076400010607     C                   MOVE      datcor        VABMGS
076500010607     C*
076600030715     C**********
076700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076800030715     C**********
076900030715     C                   IF        wGiro = *zeros
077000040706     C*
077100040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
077200040706     C                   EVAL      depvinDTA = vinDTA
077300040113     C*
077400040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
077500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077600040113     C                                      %trim(intcol(3))
077700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077800031201     C     minu:maiu     XLATE     vindta        vindta
077900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078000030715     C                   DOW       posDa <= %len(%trim(vindta))
078100030715     C*
078200030715     C* Gestisco il 1° campo
078300030715     C                   IF        i = 1
078400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078500030715     C                   EVAL      posDa = 1
078600030715     C* Eseguo lo scan x trovare la fine del primo campo
078700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078800030715     C* A questo punto "estraggo" il campo corrente
078900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079000030715     C                                                    (posA-posDa))
079100030715     C* X i campi successivi al 1°
079200030715     C                   ELSE
079300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079400030715     C                   EVAL      posDa = posA + 1
079500030715     C* Eseguo lo scan x trovare la fine del campo corrente
079600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079700030715     C* Gestisco l'ultimo campo
079800030715     C                   IF        posA = *zeros
079900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C                   LEAVE
080400030715     C                   ELSE
080500030715     C* A questo punto "estraggo" il campo corrente
080600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080700030715     C                                                    (posA-posDa))
080800030715     C                   ENDIF
080900030715     C                   ENDIF
081000030715     C* Incremento il contatore d campo
081100030715     C                   EVAL      i = i +1
081200030715     C                   ENDDO
081300030715     C*
081400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081500030715     C* delimitazione testo
081600030715     C                   EVAL      i = 1
081700030715     C                   DOW       i < %elem(SkSplitFLD)
081800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082000030715     C                   EVAL      i = i + 1
082100030715     C                   ENDDO
082200030715     C                   EVAL      wGiro = 1
082300040706     C* Ripristino la 1° riga originale
082400040706     C                   EVAL      vinDTA = depvinDTA
082500030715     C                   ELSE
082600030715     C                   EVAL      wGiro = 2
082700031204     C**********
082800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082900031204     C* d separatore campo
083000031204     C**********
083100031204     C                   EVAL      vindta = %trim(vindta)
083200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083300031204     C                   EVAL      vindta = %subst(vindta:2)
083400031204     C                   ENDDO
083500031204     C*
083600031204     C                   Z-ADD     *zeros        lunghInput        4 0
083700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084100031204     C                   ENDDO
084200030715     C**********
084300030715     C* Effettuo lo split del campo dati d input
084400030715     C**********
084500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084600030715     C                   DOW       posDa <= %len(%trim(vindta))
084700030715     C*
084800030715     C* Gestisco il 1° campo
084900030715     C                   IF        i = 1
085000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085100030715     C                   EVAL      posDa = 1
085200030715     C* Eseguo lo scan x trovare la fine del primo campo
085300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085400030715     C* A questo punto "estraggo" il campo corrente
085500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085600030715     C                                                    (posA-posDa))
085700030715     C* X i campi successivi al 1°
085800030715     C                   ELSE
085900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086000030715     C                   EVAL      posDa = posA + 1
086100031204     C* Verifico che nn vi sia il campo nn valorizzato
086200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086300031204     C* Se campo nn valorizzato skippo al prossimo
086400031204     C                   EVAL      SkSplitCSV(i) = *blanks
086500031204     C                   EVAL      posA  = posA + 1
086600031204     C                   ELSE
086700030715     C* Eseguo lo scan x trovare la fine del campo corrente
086800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086900030715     C* Gestisco l'ultimo campo
087000030715     C                   IF        posA = *zeros
087100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087200030715     C* A questo punto "estraggo" il campo corrente
087300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087400030715     C                                                    (posA-posDa))
087500030715     C                   LEAVE
087600030715     C                   ELSE
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C                   ENDIF
088100031204     C                   ENDIF
088200030715     C                   ENDIF
088300030715     C* Incremento il contatore d campo
088400030715     C                   EVAL      i = i +1
088500030715     C                   ENDDO
088600030715     C*
088700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088800030715     C* delimitazione testo
088900030715     C                   EVAL      i = 1
089000030715     C                   DOW       i < %elem(SkSplitCSV)
089100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089300030715     C                   EVAL      i = i + 1
089400030715     C                   ENDDO
089500030715     C*
089600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089700030715     C                   EVAL      i = 1
089800030715     C                   DOW       i < %elem(SkSplitFLD)
089900030715     C*
090000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090200030715     C                             SkSplitCSV(i) <> *blanks
090300030721     C*** ATB
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090500030721     C                   EVAL      VABATB = SkSplitCSV(i)
090600030721     C                   ENDIF
090700060327     C*** RSD
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090900060327     C                   EVAL      VABRSD = SkSplitCSV(i)
091000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091200030715     C* ==
091300030715     C                   ENDIF
091400030721     C*** RD2
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091700030721     C                   ENDIF
091800030715     C*** IND
091900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
092000030715     C                   EVAL      VABIND = SkSplitCSV(i)
092100030715     C                   ENDIF
092200030715     C*** LOD
092300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092500030715     C                   ENDIF
092600030715     C*** PRD
092700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092900030715     C                   ENDIF
093000030721     C*** NZD
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093300030721     C                   ENDIF
093400030715     C*** RMA
093500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093700030715     C                   ENDIF
093800030715     C*** NOT
093900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
094000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094100030715     C                   ENDIF
094200030715     C*** NT2
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094500030715     C                   ENDIF
094600030715     C*** VCA
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** VAS
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** FFD
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030721     C*** GC1
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
096000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030721     C*** GC2
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030721     C*** TSP
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*** NAS
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** TIC
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** GCA
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** XCO
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** CTM
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
098800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** TCR
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** CTS
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** FTM
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** VAD
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** GMA
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
100800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** GGA
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** GVA
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TC1
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** TC2
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** SCL
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
102800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** RMO
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** NMO
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103700030721     C                   ENDIF
103800040119     C*** CBO
103900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104000040706     C                   EVAL      VABCBO = SkSplitCSV(i)
104100040119     C                   ENDIF
104200030721     C*
104300040706     C* Reperisco quindi i campi numerici...
104400040706     C*** NSP
104500040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104600040706     C                   EVAL      PiStr=SkSplitCSV(i)
104700040706     C                   EXSR      CHKNUM
104800040706     C                   IF        PiInt=*on
104900040706     C                   Z-ADD     PiVal         VABNSP
105000040706     C                   ELSE
105100040706     C                   SETON                                        31
105200040706     C                   EVAL      VABNSP = *zeros
105300040706     C                   EVAL      vinmsg = %trimr(vinmsg)
105400040706     C                             + ' ' + 'VABNSP'
105500040706     C                   ENDIF
105600040706     C                   ENDIF
105700060530     C*** RMN/NSP
105800040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105900060530     C                   EVAL      PiStr=%subst(SkSplitCSV(i):2)
106000040706     C                   EXSR      CHKNUM
106100040706     C                   IF        PiInt=*on
106200040706     C                   Z-ADD     PiVal         VABRMN
106300060530     C                   Z-ADD     PiVal         VABNSP
106400040706     C                   ELSE
106500040706     C                   SETON                                        32
106600040706     C                   EVAL      VABRMN = 1
106700040706     C                   EVAL      vinmsg = %trimr(vinmsg)
106800060530     C                             + ' ' + 'VABRMN'
106900040706     C                   ENDIF
107000040706     C                   ENDIF
107100030721     C*** CAD
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
107300030721     C                   EVAL      PiStr=SkSplitCSV(i)
107400030721     C                   EXSR      CHKNUM
107500030721     C                   IF        PiInt=*on
107600030721     C                   Z-ADD     PiVal         Num5_0
107700030721     C                   MOVEL(p)  Num5_0        VABCAD
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      VABCAD = *zeros
108100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108200030721     C                             + ' ' + 'VABCAD'
108300030721     C                   ENDIF
108400030721     C                   ENDIF
108500030721     C*** NCL
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108700030721     C                   EVAL      PiStr=SkSplitCSV(i)
108800030721     C                   EXSR      CHKNUM
108900030721     C                   IF        PiInt=*on
109000030721     C                   Z-ADD     PiVal         VABNCL
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      VABNCL = *zeros
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500040706     C                             + ' ' + 'VABNCL'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** PKB
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300050330     C                   Z-ADD(h)  PiVal         VABPKB
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      VABPKB = *zeros
110700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110800030721     C                             + ' ' + 'VABPKB'
110900030721     C                   ENDIF
111000030721     C                   ENDIF
111100030721     C*** CAS
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
111300030721     C                   EVAL      PiStr=SkSplitCSV(i)
111400030721     C                   EXSR      CHKNUM
111500030721     C                   IF        PiNum=*on
111600030721     C                   Z-ADD     PiVal         VABCAS
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABCAS = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCAS'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** CCM
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on      AND
112900030721     C                             PiVal<=9999999 AND
113000030721     C                             PiVal>*zeros
113100030721     C                   Z-ADD     PiVal         VABCCM
113200040119     C                   Z-ADD     PiVal         VATCCM
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      VABCCM = *zeros
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700040119     C                             + ' ' + 'VABCCM VATCCM'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** LNP
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on    AND
114500030721     C                             PiVal<=999   AND
114600030721     C                             PiVal>*zeros
114700030721     C                   Z-ADD     PiVal         VABLNP
114800040119     C                   Z-ADD     PiVal         VATLNP
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      VABLNP = *zeros
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300040119     C                             + ' ' + 'VABLNP VATLNP'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** NRS
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on AND
116100030721     C                             PiVal<=99
116200030721     C                   Z-ADD     PiVal         VABNRS
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABNRS'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** LNA
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on    AND
117400030721     C                             PiVal<=999   AND
117500030721     C                             PiVal>*zeros
117600030721     C                   Z-ADD     PiVal         VABLNA
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABLNA = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABLNA'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** CTR
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on  AND
118900030721     C                             PiVal<=999
119000030721     C                   Z-ADD     PiVal         VABCTR
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABCTR'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** IAS
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiNum=*on
120200030721     C                   Z-ADD     PiVal         VABIAS
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABIAS'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** VLB
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiNum=*on
121400030721     C                   Z-ADD     PiVal         VABVLB
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABVLB'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** QFT
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiNum=*on
122600030721     C                   Z-ADD     PiVal         VABQFT
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABQFT'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** NCD
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on      AND
123800030721     C                             PiVal<=9999999
123900030721     C                   Z-ADD     PiVal         VABNCD
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABNCD'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** NCA
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiInt=*on      AND
125100030721     C                             PiVal<=9999999
125200030721     C                   Z-ADD     PiVal         VABNCA
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABNCA'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** ZNC
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on AND
126400030721     C                             PiVal<=99
126500030721     C                   Z-ADD     PiVal         VABZNC
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABZNC'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** DCR
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on       AND
127700030721     C                             PiVal<=99999999
127800030721     C                   Z-ADD     PiVal         VABDCR
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABDCR'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** HCR
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on   AND
129000030721     C                             PiVal<=9999
129100030721     C                   Z-ADD     PiVal         VABHCR
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABHCR'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** VMD
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiNum=*on
130300030721     C                   Z-ADD     PiVal         VABVMD
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABVMD'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** ANT
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on        AND
131500030721     C                             PiVal<=999999999
131600030721     C                   Z-ADD     PiVal         VABANT
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABANT'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** CMO
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on
132800030721     C                   Z-ADD     PiVal         Num5_0
132900030721     C                   MOVEL(p)  Num5_0        VABCMO
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      VABCMO = *zeros
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABCMO'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700040706     C*
133800040706     C* Gestisco i campi "personalizzati"
133900060327     C*** TELEFONO
134000060327     C                   IF        %trim(SkSplitFLD(i))='TELEFONO'
134100060327     C                   EVAL      wTEL = SkSplitCSV(i)
134200040706     C                   ENDIF
134300060530     C*** REFERENTE
134400060530     C                   IF        %trim(SkSplitFLD(i))='REFERENTE'
134500060530     C                   EVAL      wREF = SkSplitCSV(i)
134600060530     C                   ENDIF
134700030715     C*
134800030715     C                   ENDIF
134900030715     C                   EVAL      i = i + 1
135000030715     C                   ENDDO
135100040706     C*
135200040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
135300040706     C* Reperisco la provincia dal CAP e dalla località
135400040706     C                   IF        VABCAD <> *blanks
135500040706     C                   CLEAR                   TISI95DS
135600040706     C                   EVAL      I95TCN = '3'
135700040706     C                   Z-ADD     datcor        I95DAT
135800040706     C                   EVAL      I95CAP = VABCAD
135900040706     C                   EVAL      I95LOC = VABLOD
136000040706     C                   CALL      'TISI95R'
136100040706     C                   PARM                    TISI95DS
136200040706     C                   EVAL      VABPRD = O95PRV
136300040706     C                   ENDIF
136400060530
136500060530     C* NSP => Stacco un numeratore da AZNUM se NSP a 0
136600060530     c                   IF        VABNSP = 0
136700060530     C                   clear                   TRUL33DS
136800060530     C                   eval      I33OPE = *zeros
136900060530     C                   eval      I33CNU = 302
137000060530     C                   eval      I33NUM = 1
137100060530     C                   movel     TRUL33DS      KPJBU
137200060530     C                   call      'TRUL33R'
137300060530     C                   parm                    KPJBA
137400060530     C                   movel     KPJBU         TRUL33DS
137500060530     C                   if        O33ERR = *zeros
137600060530     C                   z-add     O33NRF        VABNSP
137700060530     C                   z-add     O33NRF        VATNSP
137800060530     C                   else
137900060530     C                   EVAL      vinmsg = %trimr(vinmsg)
138000060530     C                             + ' ' + 'VABNSP VATNSP'
138100060530     C                   endif
138200060530     C                   ENDIF
138300040706     C*
138400040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
138500040706     C* d valorizzarli sempre entrambi
138600040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
138700040706     C                   Z-ADD     VABNSP        VABRMN
138800040706     C                   ENDIF
138900040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
139000040706     C                   MOVEL     VABRMN        VABRMA
139100040706     C                   ENDIF
139200040113     C*
139300040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
139400040113     C                   IF        VABCAS > *zeros
139500060327     C                   EVAL      VABVCA = 'EUR'
139600040119     C                   IF        VABCBO = '1'
139700040113     C                   EVAL      VABCBO = '4'
139800040119     C                   ENDIF
139900040119     C                   IF        VABCBO = '2'
140000040119     C                   EVAL      VABCBO = '6'
140100040119     C                   ENDIF
140200040113     C                   ENDIF
140300020204     C*
140400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
140500020204     C                   EXSR      CHKIMPDIV
140600030715     C                   ENDIF
140700020204     C*
140800010607     C                   ENDSR
140900010607     C*----------------------------------------------------*
141000010601
141100020204
141200020204     C*----------------------------------------------------*
141300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
141400020204     C*----------------------------------------------------*
141500020204     C     CHKIMPDIV     BEGSR
141600020204     C*
141700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
141800020204     C                   Z-ADD     *zeros        wrkDec            9 9
141900020204     C*
142000020204     C* Come prima cosa effettuo considerazioni sulla divisa
142100020204     C                   IF        vabIAS > *zeros
142200020204     C                   IF        vabVAS <> 'EUR'
142300020204     C                   EVAL      vabVAS =  'ITL'
142400020204     C                   ENDIF
142500020204     C                   ENDIF
142600020204     C*
142700020204     C                   IF        vabCAS > *zeros
142800020204     C                   IF        vabVCA <> 'EUR'
142900020204     C                   EVAL      vabVCA =  'ITL'
143000020204     C                   ENDIF
143100020204     C                   ENDIF
143200020204     C*
143300020204     C                   IF        vabVMD > *zeros
143400020204     C                   IF        vabVAD <> 'EUR'
143500020204     C                   EVAL      vabVAD =  'ITL'
143600020204     C                   ENDIF
143700020204     C                   ENDIF
143800020204     C*
143900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
144000020204     C                   Z-ADD     vabIAS        wrkDec
144100020204     C                   IF        wrkDec > *zeros
144200020204     C                   IF        vabVAS = 'ITL'
144300020204     C                   EVAL      vabIAS = *zeros
144400020204     C                   ENDIF
144500020204     C                   ENDIF
144600020204     C*
144700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
144800020204     C                   Z-ADD     vabCAS        wrkDec
144900020204     C                   IF        wrkDec > *zeros
145000020204     C                   IF        vabVCA = 'ITL'
145100020204     C                   EVAL      vabCAS = *zeros
145200020204     C                   ENDIF
145300020204     C                   ENDIF
145400020204     C*
145500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
145600020204     C                   Z-ADD     vabVMD        wrkDec
145700020204     C                   IF        wrkDec > *zeros
145800020204     C                   IF        vabVAD = 'ITL'
145900020204     C                   EVAL      vabVMD = *zeros
146000020204     C                   ENDIF
146100020204     C                   ENDIF
146200020204     C*
146300020204     C                   ENDSR
146400020204     C***
146500020204
146600010330
146700010330
146800010330     C*----------------------------------------------------*
146900010330     C*  CONTROLLO NUMERICITA' CAMPI
147000010330     C*----------------------------------------------------*
147100010330     C     CHKNUM        BEGSR
147200010330     C*
147300010606     C                   IF        PiDecChr = *blanks
147400030715     C                   EVAL      PiDecChr = CharNUM
147500010606     C                   ENDIF
147600010606     C*
147700010606     C                   CALL(e)   'ISNUMERIC'
147800010330     C                   PARM                    PiStr            30
147900010606     C                   PARM                    PiDecChr          1
148000010330     C                   PARM      *ZEROS        PiVal            30 9
148100010330     C                   PARM      '0'           PiInt             1
148200010330     C                   PARM      '0'           PiNum             1
148300010330     C                   IF        %error
148400010606     C                   EVAL      PiNum=*off
148500010330     C                   ENDIF
148600010330     C*
148700010330     C                   ENDSR
148800010330     C***
148900010330
149000010601
149100010601
149200010601
149300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149400010601     C     repfil        BEGSR
149500010601     C*
149600010601     C                   if        invfil = *zeros and
149700010601     C                             depfil > *zeros and
149800010629     C                             (vinflg = *blanks or
149900010629     C                              vinflg = *zeros)
150000010601     C
150100010601     C                   eval      invfil = depfil
150200010601     C                   endif
150300010601     C*
150400010601     C                   if        depfil <> invfil and
150500010601     C                             invfil > *zeros
150600010601     C                   eval      flgMulti = '1'
150700010601     C                   if        vinflg = *blanks
150800010601     C                   add       1             cntNonEl
150900010601     C                   endif
151000010601     C                   endif
151100010601     C*
151200010601     C                   if        vinflg = '2'
151300010601     C                   eval      flgStato = '2'
151400010601     C                   endif
151500010601     C*
151600010601     C                   ENDSR
151700010601     C***
151800010601
151900010601
152000010601
152100010330
152200040119      /TITLE Invio dei dati al punto operativo.
152300040119     C     invio         BEGSR
152400040119     C*
152500040119     C* 1° invio FIVAT
152600040119     C                   reset                   dscmz
152700040119     C                   move      vlrpoi        cmzdst
152800040119     C                   eval      cmzfld = 'FIVATWWR'
152900040119     C                   eval      cmzmbd = vlrhdl
153000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153100040119     C***                if        prmfir = *blanks
153200040119     C                   eval      cmzfla = 'FIVAT00F'
153300040119     C                   eval      cmzmba = 'FIVAT00F'
153400040119     C***                else
153500040119     C***                eval      cmzfla = prmfir
153600040119     C***                eval      cmzmba = prmfir
153700040119     C***                endif
153800040119     C                   eval      cmznrr = *zeros
153900040119     C                   move      §ctrokvt      cmznrr
154000040119     C                   eval      cmzlba = vlrfl1
154100040119     C                   call(e)   'TIS711C'
154200040119     C                   parm                    dscmz
154300040119     C                   parm      *blanks       esito
154400040119     C                   if        %error
154500040119     C                             or cmzerr = '1'
154600040119     C                             or esito  = '1'
154700040119     C                   eval      wrkesito = '3'
154800040119     C                   else
154900040119     C*
155000040119     C* 2° invio FIVAB
155100040119     C                   reset                   dscmz
155200040119     C                   move      vlrpoi        cmzdst
155300040119     C                   eval      cmzfld = vlrfou
155400040119     C                   eval      cmzmbd = vlrhdl
155500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155600040119     C***                if        prmfir = *blanks
155700040119     C                   eval      cmzfla = 'FIVAB00F'
155800040119     C                   eval      cmzmba = 'FIVAB00F'
155900040119     C***                else
156000040119     C***                eval      cmzfla = prmfir
156100040119     C***                eval      cmzmba = prmfir
156200040119     C***                endif
156300040119     C                   eval      cmznrr = *zeros
156400040119     C                   move      §ctrokvb      cmznrr
156500040119     C                   eval      cmzlba = vlrfl1
156600040119     C                   call(e)   'TIS711C'
156700040119     C                   parm                    dscmz
156800040119     C                   parm      *blanks       esito
156900040119     C                   if        %error
157000040119     C                             or cmzerr = '1'
157100040119     C                             or esito  = '1'
157200040119     C                   eval      wrkesito = '3'
157300040119     C                   endif
157400040119     C                   endif
157500040119     C*
157600040119     C                   ENDSR
157700040119     C***
157800010601
157900010601
158000010601
158100010601
158200010601      /TITLE Invio dei dati al punto operativo.
158300010601     C     opeini        BEGSR
158400010601     C*
158500010601     C* Inizializzo flag e contatori operativi
158600010601     C                   movel     '0'           flgGiro           1
158700010601     C                   movel     '0'           flgMulti          1
158800010601     C                   movel     '1'           flgStato          1
158900010615     C                   movel     '0'           flgOk             1
159000010601     C                   z-add     *zeros        cntNonEl         10 0
159100010601     C                   z-add     *zeros        depfil            3 0
159200010601     C                   z-add     *zeros        invfil            3 0
159300010601     C*
159400010601     C                   ENDSR
159500010601     C***
159600010601
159700010601
159800050421
159900050421
160000070411
160100070411     C     *pssr         BEGSR
160200070411     C*
160300070411     C                   if        %open(tivin00r)
160400070411     C                   close     tivin00r
160500070411     C                   endif
160600070411     C                   if        %open(fivabwwr)
160700070411     C                   close     fivabwwr
160800070411     C                   endif
160900070411     C                   if        %open(fivatwwr)
161000070411     C                   close     fivatwwr
161100070411     C                   endif
161200070411     C*
161300070411     C* Effettuo la chiamata al CLLE preposto
161400070411     C                   call(e)   'TITVVTC'
161500070411     C                   parm                    parccm
161600070411     C                   parm                    parmbr
161700070411     C                   parm      '2'           paropz
161800070411     C*
161900070411     C                   eval      wrkesito = '2'
162000070411     C*
162100070411     C                   seton                                        LR
162200070411     C*
162300070411     C                   ENDSR     '*CANCL'
162400070411     C***
162500070411
162600050421
162700010601
162800010330
162900010330
163000000613     C     *inzsr        BEGSR
163100990910     C*
163200990910     C     *entry        plist
163300990920     C                   parm                    tivlrds
163400990921     C                   parm      wrkesito      esito
163500000724     C                   parm                    prmlit
163600000710     C                   parm                    prmfir
163700010330     C*
163800010330     C* CALCOLA LA DATA CORRENTE
163900010330     C                   time                    wn14             14 0
164000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
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
165100060530     Otitvt3p   E            riepilogo         2
165200990915     O                                              'Upload via Internet'
165300990915     O                                           +1 'Traduzione TIVIN00R -'
165400021025     O                                           +1 'FIVABWWR'
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
167600060530     Otitvt3ps  E            testdett          2
167700000613     O                                              'Upload via Internet'
167800000613     O                                           +1 'Traduzione TIVIN00R -'
167900021025     O                                           +1 'FIVABWWR'
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
170700060530OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
170800060530OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
170900060530DLTOVR FILE(TITVT3P TITVT3PS) LVL(*)
171000010606
171100000613
171200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
171300060530VABRMN;VABRSD;REFERENTE;VABIND;VABNOT;VABRD2;VABLOD;
171400060530VABPRD;VABCAD;X;TELEFONO;X;VABRMA;X;VABNCL;VABCAS;
171500040113
