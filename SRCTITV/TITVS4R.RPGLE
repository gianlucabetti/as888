000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800060327     Ftitvs4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060327     Ftitvs4ps  O    f  198        PRINTER usropn
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
013800060327     C                   if        not %open(titvs4ps)
013900060327     C                   open      titvs4ps
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
017700060327     C                   if        %open(titvs4ps)
017800000616     C                   except    findett
017900060327     C                   close     titvs4ps
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
019900060327     C                   if        not %open(titvs4p)
020000060327     C   50              open      titvs4p
020100990915     C                   endif
020200990915     C*
020300030715     C   50              except    riepilogo
020400990915     C*
020500060327     C                   if        %open(titvs4p)
020600060327     C   50              close     titvs4p
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
024200050330     C                   exsr      inzvar
024300050330     C                   exsr      impvab
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
042300060327     C* Imposto il tipo record sul file VAT a 'E' x indicare il "chi sono" della spedizione
042400060327     C                   EVAL      VATTRC = 'E'
042500060327     C                   EVAL      VATNOT = %trim(VABRMA)
042600060327     C*
042700060327     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
042800060327     C                   IF        VATNOT <> *blanks
042900060327     C                   WRITE     FIVAT000
043000060327     C                   ADD       1             §CTROKVT
043100060327     C                   ENDIF
043200060327     C*
043300060327     C                   ENDSR
043400040119
043500040119
043600040119
043700040119     C*----------------------------------------------------*
043800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043900040119     C*----------------------------------------------------*
044000040119     C     PREVAT        BEGSR
044100040119     C*
044200040119     C* Compongo il nome del membro da dare al FIVATWWR
044300040119     C                   eval      parmbr = vlrhdl
044400040119     C                   movel     'M'           parmbr
044500050330     C                   eval      parccm = %subst(vlrKSC:2:7)
044600040119     C                   eval      paropz = '1'
044700040119     C* Effettuo la chiamata al CLLE preposto
044800040119     C                   call(e)   'TITVVTC'
044900040119     C                   parm                    parccm
045000040119     C                   parm                    parmbr
045100040119     C                   parm                    paropz
045200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045300040119     C                   if        %error
045400040119     C                   movel     '1'           chkcall
045500040119     C                   else
045600040119     C                   movel     '0'           chkcall
045700040119     C                   endif
045800040119     C*
045900040119     C                   ENDSR
046000010601
046100010601
046200010601
046300010330     C*----------------------------------------------------*
046400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046500010330     C*----------------------------------------------------*
046600040119     C     INZVAR        BEGSR
046700010330     C*
046800030822     C* Inizializzo variabili di wrk
046900020204     C                   Z-ADD     *zeros        Num5_0            5 0
047000060327     C                   MOVEL     *blanks       wTEL             35
047100060327     C                   MOVEL     *blanks       wREF             35
047200060327     C                   MOVEL     *blanks       wRMN             15
047300030822     C*
047400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047500030822     C                   CLEAR                   FIVAB000
047600040119     C                   CLEAR                   FIVAT000
047700030822     C                   CLEAR                   SkSplitCSV
047800030822     C                   EVAL      i = 1
047900030822     C                   EVAL      posDa = *zeros
048000030822     C                   EVAL      posA  = *zeros
048100030822     C*
048200030822     C* Reimposto i valori di default
048300030822     C                   EXSR      DEFCAM
048400030822     C*
048500010330     C                   ENDSR
048600010330     C*----------------------------------------------------*
048700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048800010330     C*----------------------------------------------------*
048900010330     C     DEFCAM        BEGSR
049000040119     C*
049100040119     C* Imposto i valori d default a programma dei campi fondamentali
049200060327     C                   EVAL      VABCCM = 0893556
049300060327     C                   EVAL      VATCCM = 0893556
049400060327     C                   EVAL      VABLNP = 089
049500060327     C                   EVAL      VATLNP = 089
049600060327     C                   EVAL      VABCTR = 300
049700040119     C                   EVAL      VABCTM = '7Q'
049800040119     C                   EVAL      VABCBO = '1'
049900060327     C                   EVAL      VABTSP = 'C'
050000060327     C                   EVAL      VABNAS = 'DISCHI E STRUMENTI MUSICALI'
050100030822     C*
050200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050300030715     C* e delimitatore testo.
050400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050700050330     C*
050800050330     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050900050330     C                   IF        %subst(vlrppt:1:1) = 'S'
051000050330     C                   SETON                                        50
051100050330     C                   ELSE
051200050330     C                   SETOFF                                       50
051300050330     C                   ENDIF
051400030822     C*
051500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051600030822     C                   EVAL      posDaDft = 1
051700030822     C                   EVAL      posADft  = 0
051800030822     C                   EVAL      wGiroDft = 0
051900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052000030822     C                             posDaDft > 0
052100030822     C*
052200030822     C* Gestisco il 1° giro
052300030822     C                   IF        wGiroDft = 0
052400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052600030822     C* Incremento il contatore dei "giri"
052700030822     C                   EVAL      wGiroDft = 1
052800030822     C                   ELSE
052900030822     C                   EVAL      posDaDft = posADft
053000030822     C                   ENDIF
053100030822     C* Eseguo lo scan x trovare la fine del campo corrente
053200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053300030822     C*
053400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053600030822     C                   IF        posDaDft > 0 AND
053700030822     C                             posADft  > 0
053800030822     C* NCL
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'NCL'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiInt=*on
054800030822     C                   Z-ADD     PiVal         VABNCL
054900030822     C                   ENDIF
055000030822     C                   ENDIF
055100030822     C* CCM
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'CCM'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiInt=*on
056100030822     C                   Z-ADD     PiVal         VABCCM
056200040119     C                   Z-ADD     PiVal         VATCCM
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
057600040119     C                   Z-ADD     PiVal         VATLNP
057700030822     C                   ENDIF
057800030822     C                   ENDIF
057900030822     C* NRS
058000030822     C                   IF        %subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):1:3)
058300030822     C                             = 'NRS'
058400030822     C                   EVAL      PiStr=%trim(%subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700030822     C                   EXSR      CHKNUM
058800030822     C                   IF        PiInt=*on
058900030822     C                   Z-ADD     PiVal         VABNRS
059000030822     C                   ENDIF
059100030822     C                   ENDIF
059200030822     C* CTR
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'CTR'
059700030822     C                   EVAL      PiStr=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   EXSR      CHKNUM
060100030822     C                   IF        PiInt=*on
060200030822     C                   Z-ADD     PiVal         VABCTR
060300030822     C                   ENDIF
060400030822     C                   ENDIF
060500030822     C* PKB
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'PKB'
061000030822     C                   EVAL      PiStr=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   EXSR      CHKNUM
061400030822     C                   IF        PiNum=*on
061500050330     C                   Z-ADD     PiVal         VABPKB
061600030822     C                   ENDIF
061700030822     C                   ENDIF
061800030822     C* VLB
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'VLB'
062300030822     C                   EVAL      PiStr=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   EXSR      CHKNUM
062700030822     C                   IF        PiNum=*on
062800030822     C                   Z-ADD     PiVal         VABVLB
062900030822     C                   ENDIF
063000030822     C                   ENDIF
063100030822     C* QFT
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'QFT'
063600030822     C                   EVAL      PiStr=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   EXSR      CHKNUM
064000030822     C                   IF        PiNum=*on
064100030822     C                   Z-ADD     PiVal         VABQFT
064200030822     C                   ENDIF
064300030822     C                   ENDIF
064400030822     C* CBO
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'CBO'
064900030822     C                   EVAL      VABCBO=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   ENDIF
065300030822     C* TSP
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'TSP'
065800030822     C                   EVAL      VABTSP=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* VAS
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'VAS'
066700030822     C                   EVAL      VABVAS=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100030822     C* VCA
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'VCA'
067600030822     C                   EVAL      VABVCA=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* GCA
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'GCA'
068500030822     C                   EVAL      VABGCA=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* CTM
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'CTM'
069400030822     C                   EVAL      VABCTM=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* FFD
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'FFD'
070300030822     C                   EVAL      VABFFD=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* VAD
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'VAD'
071200030822     C                   EVAL      VABVAD=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* GMA
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'GMA'
072100030822     C                   EVAL      VABGMA=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* GGA
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'GGA'
073000030822     C                   EVAL      VABGGA=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* GVA
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'GVA'
073900030822     C                   EVAL      VABGVA=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* TC1
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'TC1'
074800030822     C                   EVAL      VABTC1=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* TC2
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'TC2'
075700030822     C                   EVAL      VABTC2=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* ...
076200030822     C                   ENDIF
076300030822     C                   ENDDO
076400020204     C*
076500010330     C                   ENDSR
076600010607     C*----------------------------------------------------*
076700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
076800010607     C*----------------------------------------------------*
076900010607     C     IMPVAB        BEGSR
077000010607     C*
077100010607     C                   SETOFF                                       3132
077200030515     C*
077300010607     C                   MOVEL     datcor        VABAAS
077400040119     C                   MOVEL     datcor        VATAAS
077500010607     C                   MOVE      datcor        VABMGS
077600010607     C*
077700030715     C**********
077800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077900030715     C**********
078000030715     C                   IF        wGiro = *zeros
078100040706     C*
078200040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
078300040706     C                   EVAL      depvinDTA = vinDTA
078400040113     C*
078500040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
078600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078700040113     C                                      %trim(intcol(3))
078800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078900031201     C     minu:maiu     XLATE     vindta        vindta
079000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079100030715     C                   DOW       posDa <= %len(%trim(vindta))
079200030715     C*
079300030715     C* Gestisco il 1° campo
079400030715     C                   IF        i = 1
079500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079600030715     C                   EVAL      posDa = 1
079700030715     C* Eseguo lo scan x trovare la fine del primo campo
079800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079900030715     C* A questo punto "estraggo" il campo corrente
080000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080100030715     C                                                    (posA-posDa))
080200030715     C* X i campi successivi al 1°
080300030715     C                   ELSE
080400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080500030715     C                   EVAL      posDa = posA + 1
080600030715     C* Eseguo lo scan x trovare la fine del campo corrente
080700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080800030715     C* Gestisco l'ultimo campo
080900030715     C                   IF        posA = *zeros
081000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081100030715     C* A questo punto "estraggo" il campo corrente
081200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081300030715     C                                                    (posA-posDa))
081400030715     C                   LEAVE
081500030715     C                   ELSE
081600030715     C* A questo punto "estraggo" il campo corrente
081700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081800030715     C                                                    (posA-posDa))
081900030715     C                   ENDIF
082000030715     C                   ENDIF
082100030715     C* Incremento il contatore d campo
082200030715     C                   EVAL      i = i +1
082300030715     C                   ENDDO
082400030715     C*
082500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082600030715     C* delimitazione testo
082700030715     C                   EVAL      i = 1
082800030715     C                   DOW       i < %elem(SkSplitFLD)
082900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
083000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
083100030715     C                   EVAL      i = i + 1
083200030715     C                   ENDDO
083300030715     C                   EVAL      wGiro = 1
083400040706     C* Ripristino la 1° riga originale
083500040706     C                   EVAL      vinDTA = depvinDTA
083600030715     C                   ELSE
083700030715     C                   EVAL      wGiro = 2
083800031204     C**********
083900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
084000031204     C* d separatore campo
084100031204     C**********
084200031204     C                   EVAL      vindta = %trim(vindta)
084300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084400031204     C                   EVAL      vindta = %subst(vindta:2)
084500031204     C                   ENDDO
084600031204     C*
084700031204     C                   Z-ADD     *zeros        lunghInput        4 0
084800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
085000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
085100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085200031204     C                   ENDDO
085300030715     C**********
085400030715     C* Effettuo lo split del campo dati d input
085500030715     C**********
085600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085700030715     C                   DOW       posDa <= %len(%trim(vindta))
085800030715     C*
085900030715     C* Gestisco il 1° campo
086000030715     C                   IF        i = 1
086100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086200030715     C                   EVAL      posDa = 1
086300030715     C* Eseguo lo scan x trovare la fine del primo campo
086400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C* X i campi successivi al 1°
086900030715     C                   ELSE
087000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087100030715     C                   EVAL      posDa = posA + 1
087200031204     C* Verifico che nn vi sia il campo nn valorizzato
087300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087400031204     C* Se campo nn valorizzato skippo al prossimo
087500031204     C                   EVAL      SkSplitCSV(i) = *blanks
087600031204     C                   EVAL      posA  = posA + 1
087700031204     C                   ELSE
087800030715     C* Eseguo lo scan x trovare la fine del campo corrente
087900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088000030715     C* Gestisco l'ultimo campo
088100030715     C                   IF        posA = *zeros
088200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088300030715     C* A questo punto "estraggo" il campo corrente
088400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C                   LEAVE
088700030715     C                   ELSE
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   ENDIF
089200031204     C                   ENDIF
089300030715     C                   ENDIF
089400030715     C* Incremento il contatore d campo
089500030715     C                   EVAL      i = i +1
089600030715     C                   ENDDO
089700030715     C*
089800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089900030715     C* delimitazione testo
090000030715     C                   EVAL      i = 1
090100030715     C                   DOW       i < %elem(SkSplitCSV)
090200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090400030715     C                   EVAL      i = i + 1
090500030715     C                   ENDDO
090600030715     C*
090700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090800030715     C                   EVAL      i = 1
090900030715     C                   DOW       i < %elem(SkSplitFLD)
091000030715     C*
091100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
091200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091300030715     C                             SkSplitCSV(i) <> *blanks
091400030721     C*** ATB
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
091600030721     C                   EVAL      VABATB = SkSplitCSV(i)
091700030721     C                   ENDIF
091800060327     C*** RSD
091900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
092000060327     C                   EVAL      VABRSD = SkSplitCSV(i)
092100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
092300030715     C* ==
092400030715     C                   ENDIF
092500030721     C*** RD2
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
092700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030715     C*** IND
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
093100030715     C                   EVAL      VABIND = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030715     C*** LOD
093400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
093500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
093600030715     C                   ENDIF
093700030715     C*** PRD
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
093900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030721     C*** NZD
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
094300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030715     C*** RMA
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
094700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** NOT
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
095100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** NT2
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
095500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030715     C*** VCA
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030715     C*** VAS
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030715     C*** FFD
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030721     C*** GC1
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** GC2
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** TSP
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
097900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** NAS
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** TIC
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** GCA
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** XCO
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** CTM
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** TCR
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** CTS
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** FTM
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** VAD
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** GMA
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** GGA
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** GVA
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TC1
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** TC2
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** SCL
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** RMO
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
104300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** NMO
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104800030721     C                   ENDIF
104900040119     C*** CBO
105000040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105100040706     C                   EVAL      VABCBO = SkSplitCSV(i)
105200040119     C                   ENDIF
105300030721     C*
105400040706     C* Reperisco quindi i campi numerici...
105500040706     C*** NSP
105600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105700040706     C                   EVAL      PiStr=SkSplitCSV(i)
105800040706     C                   EXSR      CHKNUM
105900040706     C                   IF        PiInt=*on
106000040706     C                   Z-ADD     PiVal         VABNSP
106100040706     C                   ELSE
106200040706     C                   SETON                                        31
106300040706     C                   EVAL      VABNSP = *zeros
106400040706     C                   EVAL      vinmsg = %trimr(vinmsg)
106500040706     C                             + ' ' + 'VABNSP'
106600040706     C                   ENDIF
106700040706     C                   ENDIF
106800060327     C*** RMN
106900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
107000050330     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
107100040706     C                   EVAL      PiStr=SkSplitCSV(i)
107200040706     C                   EXSR      CHKNUM
107300040706     C                   IF        PiInt=*on
107400040706     C                   Z-ADD     PiVal         VABRMN
107500040706     C                   ELSE
107600040706     C                   SETON                                        32
107700040706     C                   EVAL      VABRMN = 1
107800040706     C                   EVAL      vinmsg = %trimr(vinmsg)
107900040706     C                             + ' ' + 'VABRMN'
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
114300040119     C                   Z-ADD     PiVal         VATCCM
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      VABCCM = *zeros
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800040119     C                             + ' ' + 'VABCCM VATCCM'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** LNP
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on    AND
115600030721     C                             PiVal<=999   AND
115700030721     C                             PiVal>*zeros
115800030721     C                   Z-ADD     PiVal         VABLNP
115900040119     C                   Z-ADD     PiVal         VATLNP
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      VABLNP = *zeros
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400040119     C                             + ' ' + 'VABLNP VATLNP'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** NRS
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on AND
117200030721     C                             PiVal<=99
117300030721     C                   Z-ADD     PiVal         VABNRS
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117700030721     C                             + ' ' + 'VABNRS'
117800030721     C                   ENDIF
117900030721     C                   ENDIF
118000030721     C*** LNA
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiInt=*on    AND
118500030721     C                             PiVal<=999   AND
118600030721     C                             PiVal>*zeros
118700030721     C                   Z-ADD     PiVal         VABLNA
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      VABLNA = *zeros
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABLNA'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** CTR
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiInt=*on  AND
120000030721     C                             PiVal<=999
120100030721     C                   Z-ADD     PiVal         VABCTR
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABCTR'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** IAS
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiNum=*on
121300030721     C                   Z-ADD     PiVal         VABIAS
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABIAS'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** VLB
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiNum=*on
122500030721     C                   Z-ADD     PiVal         VABVLB
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABVLB'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** QFT
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiNum=*on
123700030721     C                   Z-ADD     PiVal         VABQFT
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABQFT'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** NCD
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on      AND
124900030721     C                             PiVal<=9999999
125000030721     C                   Z-ADD     PiVal         VABNCD
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABNCD'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** NCA
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on      AND
126200030721     C                             PiVal<=9999999
126300030721     C                   Z-ADD     PiVal         VABNCA
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABNCA'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** ZNC
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on AND
127500030721     C                             PiVal<=99
127600030721     C                   Z-ADD     PiVal         VABZNC
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABZNC'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** DCR
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on       AND
128800030721     C                             PiVal<=99999999
128900030721     C                   Z-ADD     PiVal         VABDCR
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABDCR'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** HCR
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on   AND
130100030721     C                             PiVal<=9999
130200030721     C                   Z-ADD     PiVal         VABHCR
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABHCR'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** VMD
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiNum=*on
131400030721     C                   Z-ADD     PiVal         VABVMD
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABVMD'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** ANT
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on        AND
132600030721     C                             PiVal<=999999999
132700030721     C                   Z-ADD     PiVal         VABANT
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABANT'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** CMO
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on
133900030721     C                   Z-ADD     PiVal         Num5_0
134000030721     C                   MOVEL(p)  Num5_0        VABCMO
134100030721     C                   ELSE
134200030721     C                   SETON                                        32
134300030721     C                   EVAL      VABCMO = *zeros
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABCMO'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800040706     C*
134900040706     C* Gestisco i campi "personalizzati"
135000060327     C*** TELEFONO
135100060327     C                   IF        %trim(SkSplitFLD(i))='TELEFONO'
135200060327     C                   EVAL      wTEL = SkSplitCSV(i)
135300040706     C                   ENDIF
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
137100060327     C*
137200060327     C* NSP => Stacco un numeratore da AZNUM
137300060327     C                   clear                   TRUL33DS
137400060327     C                   eval      I33OPE = *zeros
137500060327     C                   eval      I33CNU = 302
137600060327     C                   eval      I33NUM = 1
137700060327     C                   movel     TRUL33DS      KPJBU
137800060327     C                   call      'TRUL33R'
137900060327     C                   parm                    KPJBA
138000060327     C                   movel     KPJBU         TRUL33DS
138100060327     C                   if        O33ERR = *zeros
138200060327     C                   z-add     O33NRF        VABNSP
138300060327     C                   z-add     O33NRF        VATNSP
138400060327     C                   else
138500060327     C                   EVAL      vinmsg = %trimr(vinmsg)
138600060327     C                             + ' ' + 'VABNSP VATNSP'
138700060327     C                   endif
138800060327     C*
138900060327     C* Dal RMA reperisco anche l'RMN
139000060327     C                   EVALR     wRMN = %trim(%subst(VABRMA:4))
139100060327     C                   MOVE(P)   wRMN          VABRMN
139200060327     C*
139300060327     C* Dal RSD reperisco anche il referente "privato"
139400060327     C                   EVAL      wREF = VABRSD
139500040706     C*
139600040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
139700040706     C* d valorizzarli sempre entrambi
139800040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
139900040706     C                   Z-ADD     VABNSP        VABRMN
140000040706     C                   ENDIF
140100040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
140200040706     C                   MOVEL     VABRMN        VABRMA
140300040706     C                   ENDIF
140400040113     C*
140500040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
140600040113     C                   IF        VABCAS > *zeros
140700060327     C                   EVAL      VABVCA = 'EUR'
140800040119     C                   IF        VABCBO = '1'
140900040113     C                   EVAL      VABCBO = '4'
141000040119     C                   ENDIF
141100040119     C                   IF        VABCBO = '2'
141200040119     C                   EVAL      VABCBO = '6'
141300040119     C                   ENDIF
141400040113     C                   ENDIF
141500020204     C*
141600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
141700020204     C                   EXSR      CHKIMPDIV
141800030715     C                   ENDIF
141900020204     C*
142000010607     C                   ENDSR
142100010607     C*----------------------------------------------------*
142200010601
142300020204
142400020204     C*----------------------------------------------------*
142500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
142600020204     C*----------------------------------------------------*
142700020204     C     CHKIMPDIV     BEGSR
142800020204     C*
142900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
143000020204     C                   Z-ADD     *zeros        wrkDec            9 9
143100020204     C*
143200020204     C* Come prima cosa effettuo considerazioni sulla divisa
143300020204     C                   IF        vabIAS > *zeros
143400020204     C                   IF        vabVAS <> 'EUR'
143500020204     C                   EVAL      vabVAS =  'ITL'
143600020204     C                   ENDIF
143700020204     C                   ENDIF
143800020204     C*
143900020204     C                   IF        vabCAS > *zeros
144000020204     C                   IF        vabVCA <> 'EUR'
144100020204     C                   EVAL      vabVCA =  'ITL'
144200020204     C                   ENDIF
144300020204     C                   ENDIF
144400020204     C*
144500020204     C                   IF        vabVMD > *zeros
144600020204     C                   IF        vabVAD <> 'EUR'
144700020204     C                   EVAL      vabVAD =  'ITL'
144800020204     C                   ENDIF
144900020204     C                   ENDIF
145000020204     C*
145100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
145200020204     C                   Z-ADD     vabIAS        wrkDec
145300020204     C                   IF        wrkDec > *zeros
145400020204     C                   IF        vabVAS = 'ITL'
145500020204     C                   EVAL      vabIAS = *zeros
145600020204     C                   ENDIF
145700020204     C                   ENDIF
145800020204     C*
145900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
146000020204     C                   Z-ADD     vabCAS        wrkDec
146100020204     C                   IF        wrkDec > *zeros
146200020204     C                   IF        vabVCA = 'ITL'
146300020204     C                   EVAL      vabCAS = *zeros
146400020204     C                   ENDIF
146500020204     C                   ENDIF
146600020204     C*
146700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
146800020204     C                   Z-ADD     vabVMD        wrkDec
146900020204     C                   IF        wrkDec > *zeros
147000020204     C                   IF        vabVAD = 'ITL'
147100020204     C                   EVAL      vabVMD = *zeros
147200020204     C                   ENDIF
147300020204     C                   ENDIF
147400020204     C*
147500020204     C                   ENDSR
147600020204     C***
147700020204
147800010330
147900010330
148000010330     C*----------------------------------------------------*
148100010330     C*  CONTROLLO NUMERICITA' CAMPI
148200010330     C*----------------------------------------------------*
148300010330     C     CHKNUM        BEGSR
148400010330     C*
148500010606     C                   IF        PiDecChr = *blanks
148600030715     C                   EVAL      PiDecChr = CharNUM
148700010606     C                   ENDIF
148800010606     C*
148900010606     C                   CALL(e)   'ISNUMERIC'
149000010330     C                   PARM                    PiStr            30
149100010606     C                   PARM                    PiDecChr          1
149200010330     C                   PARM      *ZEROS        PiVal            30 9
149300010330     C                   PARM      '0'           PiInt             1
149400010330     C                   PARM      '0'           PiNum             1
149500010330     C                   IF        %error
149600010606     C                   EVAL      PiNum=*off
149700010330     C                   ENDIF
149800010330     C*
149900010330     C                   ENDSR
150000010330     C***
150100010330
150200010601
150300010601
150400010601
150500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
150600010601     C     repfil        BEGSR
150700010601     C*
150800010601     C                   if        invfil = *zeros and
150900010601     C                             depfil > *zeros and
151000010629     C                             (vinflg = *blanks or
151100010629     C                              vinflg = *zeros)
151200010601     C
151300010601     C                   eval      invfil = depfil
151400010601     C                   endif
151500010601     C*
151600010601     C                   if        depfil <> invfil and
151700010601     C                             invfil > *zeros
151800010601     C                   eval      flgMulti = '1'
151900010601     C                   if        vinflg = *blanks
152000010601     C                   add       1             cntNonEl
152100010601     C                   endif
152200010601     C                   endif
152300010601     C*
152400010601     C                   if        vinflg = '2'
152500010601     C                   eval      flgStato = '2'
152600010601     C                   endif
152700010601     C*
152800010601     C                   ENDSR
152900010601     C***
153000010601
153100010601
153200010601
153300010330
153400040119      /TITLE Invio dei dati al punto operativo.
153500040119     C     invio         BEGSR
153600040119     C*
153700040119     C* 1° invio FIVAT
153800040119     C                   reset                   dscmz
153900040119     C                   move      vlrpoi        cmzdst
154000040119     C                   eval      cmzfld = 'FIVATWWR'
154100040119     C                   eval      cmzmbd = vlrhdl
154200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
154300040119     C***                if        prmfir = *blanks
154400040119     C                   eval      cmzfla = 'FIVAT00F'
154500040119     C                   eval      cmzmba = 'FIVAT00F'
154600040119     C***                else
154700040119     C***                eval      cmzfla = prmfir
154800040119     C***                eval      cmzmba = prmfir
154900040119     C***                endif
155000040119     C                   eval      cmznrr = *zeros
155100040119     C                   move      §ctrokvt      cmznrr
155200040119     C                   eval      cmzlba = vlrfl1
155300040119     C                   call(e)   'TIS711C'
155400040119     C                   parm                    dscmz
155500040119     C                   parm      *blanks       esito
155600040119     C                   if        %error
155700040119     C                             or cmzerr = '1'
155800040119     C                             or esito  = '1'
155900040119     C                   eval      wrkesito = '3'
156000040119     C                   else
156100040119     C*
156200040119     C* 2° invio FIVAB
156300040119     C                   reset                   dscmz
156400040119     C                   move      vlrpoi        cmzdst
156500040119     C                   eval      cmzfld = vlrfou
156600040119     C                   eval      cmzmbd = vlrhdl
156700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
156800040119     C***                if        prmfir = *blanks
156900040119     C                   eval      cmzfla = 'FIVAB00F'
157000040119     C                   eval      cmzmba = 'FIVAB00F'
157100040119     C***                else
157200040119     C***                eval      cmzfla = prmfir
157300040119     C***                eval      cmzmba = prmfir
157400040119     C***                endif
157500040119     C                   eval      cmznrr = *zeros
157600040119     C                   move      §ctrokvb      cmznrr
157700040119     C                   eval      cmzlba = vlrfl1
157800040119     C                   call(e)   'TIS711C'
157900040119     C                   parm                    dscmz
158000040119     C                   parm      *blanks       esito
158100040119     C                   if        %error
158200040119     C                             or cmzerr = '1'
158300040119     C                             or esito  = '1'
158400040119     C                   eval      wrkesito = '3'
158500040119     C                   endif
158600040119     C                   endif
158700040119     C*
158800040119     C                   ENDSR
158900040119     C***
159000010601
159100010601
159200010601
159300010601
159400010601      /TITLE Invio dei dati al punto operativo.
159500010601     C     opeini        BEGSR
159600010601     C*
159700010601     C* Inizializzo flag e contatori operativi
159800010601     C                   movel     '0'           flgGiro           1
159900010601     C                   movel     '0'           flgMulti          1
160000010601     C                   movel     '1'           flgStato          1
160100010615     C                   movel     '0'           flgOk             1
160200010601     C                   z-add     *zeros        cntNonEl         10 0
160300010601     C                   z-add     *zeros        depfil            3 0
160400010601     C                   z-add     *zeros        invfil            3 0
160500010601     C*
160600010601     C                   ENDSR
160700010601     C***
160800010601
160900010601
161000050421
161100050421
161200070411
161300070411     C     *pssr         BEGSR
161400070411     C*
161500070411     C                   if        %open(tivin00r)
161600070411     C                   close     tivin00r
161700070411     C                   endif
161800070411     C                   if        %open(fivabwwr)
161900070411     C                   close     fivabwwr
162000070411     C                   endif
162100070411     C                   if        %open(fivatwwr)
162200070411     C                   close     fivatwwr
162300070411     C                   endif
162400070411     C*
162500070411     C* Effettuo la chiamata al CLLE preposto
162600070411     C                   call(e)   'TITVVTC'
162700070411     C                   parm                    parccm
162800070411     C                   parm                    parmbr
162900070411     C                   parm      '2'           paropz
163000070411     C*
163100070411     C                   eval      wrkesito = '2'
163200070411     C*
163300070411     C                   seton                                        LR
163400070411     C*
163500070411     C                   ENDSR     '*CANCL'
163600070411     C***
163700070411
163800050421
163900010601
164000010330
164100010330
164200000613     C     *inzsr        BEGSR
164300990910     C*
164400990910     C     *entry        plist
164500990920     C                   parm                    tivlrds
164600990921     C                   parm      wrkesito      esito
164700000724     C                   parm                    prmlit
164800000710     C                   parm                    prmfir
164900010330     C*
165000010330     C* CALCOLA LA DATA CORRENTE
165100010330     C                   time                    wn14             14 0
165200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
165300010330     C                   z-add     wn8           g08dat
165400010330     C                   z-add     *zeros        g08inv
165500010330     C                   movel     '0'           g08err
165600010330     C                   call      'XSRDA8'
165700010330     C                   parm                    wlbda8
165800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
165900000613     C*
166000000613     C                   ENDSR
166100000613     C***
166200990908
166300060327     Otitvs4p   E            riepilogo         2
166400990915     O                                              'Upload via Internet'
166500990915     O                                           +1 'Traduzione TIVIN00R -'
166600021025     O                                           +1 'FIVABWWR'
166700990915     O                                           +1 'Testate bolle'
166800990915     O          E            riepilogo   2
166900990915     O                       wrkdata
167000990915     O                       wrkora              +1
167100990915     O                       procname            +1
167200990915     O          E            riepilogo   2
167300990915     O                                              'Cliente..................:'
167400010605     O                       VABCCM        z     +1
167500990915     O          E            riepilogo   2
167600990920     O                                              'Riferimento Strategi.....:'
167700990920     O                       vlrhdl              +1
167800990915     O          E            riepilogo   2
167900990915     O                                              'Giusti...................:'
168000040119     O                       §CTROKVB      2   +  1
168100990915     O          E            riepilogo   2
168200990915     O                                              'Sbagliati e corretti.....:'
168300971022     O                       §CTRMO        2   +  1
168400990915     O          E            riepilogo   2
168500990915     O                                              'Sbagliati e scartati.....:'
168600971022     O                       §CTRNO        2   +  1
168700000613
168800060327     Otitvs4ps  E            testdett          2
168900000613     O                                              'Upload via Internet'
169000000613     O                                           +1 'Traduzione TIVIN00R -'
169100021025     O                                           +1 'FIVABWWR'
169200000613     O                                           +1 'Testate bolle'
169300000616     O          E            testdett    3
169400000613     O                                           +2 'N° rec'
169500000613     O                                           +3 'Anteprima contenuto'
169600000616     O          E            rigadett    2
169700000613     O                       rrnum               +2
169800000621     O                       recko               +3
169900000616     O          E            findett     2
170000000613     O                       wrkdata
170100000613     O                       wrkora              +1
170200000613     O                       procname            +1
170300000616     O          E            findett     2
170400000613     O                                              'Cliente..................:'
170500010605     O                       VABCCM        z     +1
170600000616     O          E            findett     2
170700000613     O                                              'Riferimento Strategi.....:'
170800000613     O                       vlrhdl              +1
170900000616     O          E            findett     2
171000000613     O                                              'Giusti...................:'
171100040119     O                       §CTROKVB      2   +  1
171200000616     O          E            findett     2
171300000613     O                                              'Sbagliati e corretti.....:'
171400000613     O                       §CTRMO        2   +  1
171500000616     O          E            findett     2
171600000613     O                                              'Sbagliati e scartati.....:'
171700000613     O                       §CTRNO        2   +  1
171800000613** CMD - COMANDI CL
171900060327OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVS4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
172000060327OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVS4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
172100060327DLTOVR FILE(TITVS4P TITVS4PS) LVL(*)
172200010606
172300000613
172400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
172500060327VABRMA;FILLER1;VABRSD;VABIND;VABRD2;VABCAD;
172600060327VABLOD;TELEFONO;FILLER2;VABPKB;VABCAS;
172700040113
